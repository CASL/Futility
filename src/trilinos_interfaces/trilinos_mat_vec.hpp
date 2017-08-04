/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
/                          Futility Development Group                          !
/                             All rights reserved.                             !
/                                                                              !
/ Futility is a jointly-maintained, open-source project between the University !
/ of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
/ can be found in LICENSE.txt in the head directory of this repository.        !
/+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/
#pragma once

#include <MatrixMarket_Tpetra.hpp>
#include <Teuchos_DefaultMpiComm.hpp>
#include <Tpetra_CombineMode.hpp>
#include <Tpetra_CrsMatrix.hpp>
#include <Tpetra_Import.hpp>
#include <Tpetra_Map.hpp>
#include <Tpetra_Vector.hpp>
#include <cassert>
#include <iostream>
#include <map>
#include "Teuchos_Comm.hpp"
#include "Teuchos_RCP.hpp"

#ifdef HAVE_MPI
#include <mpi.h>
typedef Teuchos::MpiComm<int> SelectedComm;
#else
typedef int MPI_Comm;
typedef Teuchos::SerialComm<int> SelectedComm;
#endif

// We only have C++11 support, so implement "std::make_unique" by hand.
template <typename T, typename... Args>
std::unique_ptr<T> make_unique(Args &&... args)
{
    return std::unique_ptr<T>(new T(std::forward<Args>(args)...));
}

class ForPETRA_SelectedTypes {
public:
    typedef double SC;
    typedef int LO;
    typedef int GO;
    typedef Kokkos::Compat::KokkosDeviceWrapperNode<Kokkos::OpenMP> NO;
    typedef Tpetra::Map<LO, GO, NO> Map;
    typedef Tpetra::Vector<SC, LO, GO, NO> Vector;
    typedef Tpetra::MultiVector<SC, LO, GO, NO> MultiVector;
    typedef Tpetra::Import<> Import;
    typedef Tpetra::CrsMatrix<SC, LO, GO, NO> CrsMatrix;
    typedef Tpetra::Operator<SC, LO, GO, NO> Operator;
};

class TpetraVecCnt : public ForPETRA_SelectedTypes {
public:
    Teuchos::RCP<const Teuchos::Comm<int>> comm;
    Teuchos::RCP<Map> map;
    Teuchos::RCP<Map> distMap;
    Teuchos::RCP<Vector> vec;
    Teuchos::RCP<Import> importer;
    Teuchos::RCP<Vector> distvec;

    // Maintain host views of the data for doing element-wise access. Keep in
    // mind that there will be some extra difficulties if/when we want to move
    // to heterogeneous architectures.
    decltype(Kokkos::subview(vec->getLocalView<Kokkos::HostSpace>(),
                             Kokkos::ALL, 0)) localView;
    decltype(Kokkos::subview(vec->getLocalView<Kokkos::HostSpace>(),
                             Kokkos::ALL, 0)) distLocalView;

    bool havedistMap;

    TpetraVecCnt(int n, int nloc, MPI_Comm rawComm)
        : comm(new SelectedComm(rawComm)),
          map(new Map(n, nloc, 1, comm)),
          vec(new Vector(map)),
          havedistMap(false)
    {
        // This is unneccesary, since this Tpetra::Vector<> constructor
        // initializes to zero
        vec->putScalar(0.0);
    }

    void defineMapData(const int id, const int nloc, const int *gid)
    {
        Kokkos::View<const Vector::global_ordinal_type *, Kokkos::HostSpace>
            gid_view(gid, nloc);
        distMap     = Teuchos::rcp(new Map(-1, gid_view, 1, comm));
        importer    = Teuchos::rcp(new Import(distMap, map));
        distvec     = Teuchos::rcp(new Vector(distMap));
        havedistMap = true;

        return;
    }

    double &operator[](int i)
    {
        return localView(i);
    }
};

class TpetraVecStore : public ForPETRA_SelectedTypes {

public:
    TpetraVecStore() : cid(0)
    {
    }

    int new_data(const int n, const int nloc, const MPI_Comm rawComm)
    {
        vec_map.emplace(cid, make_unique<TpetraVecCnt>(n, nloc, rawComm));
        cid++;
        return cid - 1;
    }

    int define_map_data(const int id, const int nloc, const int *gid)
    {
        vec_map[id]->defineMapData(id, nloc, gid);
        return 0;
    }

    int delete_data(const int id)
    {
        vec_map.erase(id);
        return 0;
    }

    // TODO: if this is setting a scalar (it is), then there is no reason to use
    // pointers, other than if the Fortran by-referenceness is better. Could
    // change to pass-by-value to make the interface more clear
    void set_data(const int id, const int *i, const double *val)
    {
        vec_map[id]->vec->replaceGlobalValue(*i, *val);
        return;
    }

    void setall_data(const int id, const double val)
    {
        vec_map[id]->vec->putScalar(val);
        return;
    }

    void transfer_data(const int id)
    {
        vec_map[id]->distvec->doImport(
            *(vec_map[id]->vec), *(vec_map[id]->importer), Tpetra::INSERT);
        return;
    }

    int get_data(const int id, const int i, double &val)
    {
        if (vec_map[id]->havedistMap) {
            int lid = vec_map[id]->distMap->getLocalElement(i);
            if (lid >= 0) {
                val = vec_map[id]->distLocalView(lid);
                return 0;
            } else {
                return lid;
            }
        } else {
            int lid = vec_map[id]->map->getLocalElement(i);
            if (lid >= 0) {
                val = vec_map[id]->localView(lid);
                return 0;
            } else {
                return lid;
            }
        }
    }

    int copy_data(const int id, const int idfrom)
    {
        *(vec_map[id]->vec) = *(vec_map[idfrom]->vec);
        return 0;
    }

    int axpy_data(const int id, const int idx, const double a, const double b)
    {
        vec_map[id]->vec->update(a, *(vec_map[idx]->vec), b);
        return 0;
    }

    int norm1_data(const int id, double val[])
    {
        auto temp = vec_map[id]->vec->norm1();
        *val      = temp;
        return 0;
    }

    int norm2_data(const int id, double val[])
    {
        auto temp = vec_map[id]->vec->norm2();
        *val      = temp;
        return 0;
    }

    int max_data(const int id, double val[])
    {
        auto temp = vec_map[id]->vec->normInf();
        *val      = temp;
        return 0;
    }

    int scale_data(const int id, double val)
    {
        vec_map[id]->vec->scale(val);
        return 0;
    }

    // TODO: eventually send a string in
    int edit_data(const int id, const char name[])
    {
        Tpetra::MatrixMarket::Writer<Vector>::writeDenseFile(name,
                                                             vec_map[id]->vec);
        return 0;
    }

    Teuchos::RCP<Vector> get_vec(const int id)
    {
        return vec_map[id]->vec;
    }

private:
    int cid;
    std::map<int, std::unique_ptr<TpetraVecCnt>> vec_map;
};

class TpetraMatCnt : public ForPETRA_SelectedTypes {
public:
    typedef Tpetra::Map<> Map;

    Teuchos::RCP<const Teuchos::Comm<int>> comm;
    Teuchos::RCP<Map> map;
    Teuchos::RCP<CrsMatrix> mat;
    bool isAssembled = false;
    int rnnz;

    TpetraMatCnt(int n, int nloc, int row_nnz, MPI_Comm rawComm)
        : comm(new SelectedComm(rawComm)),
          map(new Map(n, nloc, 1, comm)),
          mat(new CrsMatrix(map, rnnz)),
          rnnz(row_nnz)
    {
        return;
    }

    void reset()
    {
        mat = Teuchos::rcp(new CrsMatrix(map, rnnz));
        return;
    }
};

class TpetraMatStore : public ForPETRA_SelectedTypes {
    typedef TpetraMatCnt::Map Map;
    typedef TpetraVecCnt::Vector Vector;
    typedef TpetraMatCnt::CrsMatrix CrsMatrix;
    typedef Teuchos::ArrayView<const CrsMatrix::global_ordinal_type>
        RowIndexView;
    typedef Teuchos::ArrayView<const CrsMatrix::scalar_type> RowValueView;

public:
    TpetraMatStore() : cid(0)
    {
    }

    int new_data(const int n, const int nloc, const int rnnz,
                 const MPI_Comm rawComm)
    {
        mat_map.emplace(cid, make_unique<TpetraMatCnt>(n, nloc, rnnz, rawComm));
        cid++;
        return cid - 1;
    }

    int delete_data(const int id)
    {
        mat_map.erase(id);
        return 0;
    }

    int reset_data(const int id)
    {
        mat_map[id]->reset();
        return 0;
    }

    int set_data(const int id, const int i, const int nnz, const int j[],
                 const double val[])
    {
        mat_map[id]->mat->insertGlobalValues(i, nnz, val, j);
        return 0;
    }

    int assemble_data(const int id)
    {
        mat_map[id]->mat->fillComplete();
        mat_map[id]->isAssembled = true;
        return 0;
    }

    // defering this for a while
    int get_data(const int id, const int i, const int j, double &val)
    {
        val = 0.0;

        RowValueView values;
        RowIndexView indices;
        CrsMatrix::global_ordinal_type row = i;
        mat_map[id]->mat->getGlobalRowView(row, indices, values);

        for (int k = 0; k < indices.size(); ++k) {
            if (indices[k] == j - 1) {
                val = values[k];
                break;
            }
        }
        return 0;
    }

    int matvec_data(const int id, const bool trans, Teuchos::RCP<Vector> x,
                    Teuchos::RCP<Vector> y)
    {
        auto mode = trans ? Teuchos::TRANS : Teuchos::NO_TRANS;
        mat_map[id]->mat->apply(*x, *y, mode);
        return 0;
    }

    int edit_data(const int id, const char name[])
    {
        Tpetra::MatrixMarket::Writer<CrsMatrix>::writeSparseFile(
            name, mat_map[id]->mat);
        return 0;
    }

    int normF_data(const int id, double &x)
    {
        x = mat_map[id]->mat->getFrobeniusNorm();
        return 0;
    }

    Teuchos::RCP<CrsMatrix> get_mat(const int id)
    {
        return mat_map[id]->mat;
    }

private:
    int cid;
    std::map<int, std::unique_ptr<TpetraMatCnt>> mat_map;
};
