#pragma once

#include <iostream>
#include <map>
#include "Teuchos_RCP.hpp"
#ifdef HAVE_MPI
#include "Epetra_MpiComm.h"
#include <mpi.h>
#else
#include "Epetra_SerialComm.h"
#endif
#include "Epetra_Map.h"
#include "Epetra_Vector.h"
#include "Epetra_CrsMatrix.h"
#include "EpetraExt_VectorOut.h"
#include "EpetraExt_RowMatrixOut.h"
#include <cassert>

using std::map;

bool verbose=true;

class EpetraVecCnt{
public:
#ifdef HAVE_MPI
    Epetra_MpiComm Comm;
#else
    Epetra_SerialComm Comm;
#endif
    Epetra_Map emap;
    Teuchos::RCP<Epetra_Vector> evec;

    EpetraVecCnt(int n, int nloc, int rawComm) :
#ifdef HAVE_MPI
        Comm(MPI_COMM_WORLD),
#else
        //Comm(),
#endif
        emap(n,nloc,1,Comm),
        evec(new Epetra_Vector(emap))
    {
        evec->PutScalar(0.);
    }

    double &operator[](int i){ return (*evec)[i];}
};

class EpetraVecStore {
public:
    EpetraVecStore():
        cid(0)
    {
        if(verbose) std::cout << "Constructing new vector store" << std::endl;
        return;
    }

    int new_data(const int n, const int nloc, const int rawComm) {
        things_[cid]=new EpetraVecCnt(n,nloc,rawComm);
        cid++;
        return cid-1;
    }

    int set_data(const int id, const int *i, const double *val) {
        return things_[id]->evec->ReplaceGlobalValues(1,val,i);
    }

    int get_data(const int id, const int i, double &val) {
        int lid=things_[id]->emap.LID(i);
        if(lid>=0){
            val = (*things_[id])[lid];
            return 0;
        }
        else return lid;
    }

    //TODO: eventually send a string in
    int edit_data(const int id) {
        return EpetraExt::VectorToMatlabFile("myvector.m",*(things_[id]->evec));
    }

    Teuchos::RCP<Epetra_Vector> get_vec(const int id){
        return things_[id]->evec;
    }

private:
        int cid;
        map<int, EpetraVecCnt*> things_;
};


class EpetraMatCnt{
public:
#ifdef HAVE_MPI
    Epetra_MpiComm Comm;
#else
    Epetra_SerialComm Comm;
#endif
    Epetra_Map emap;
    Teuchos::RCP<Epetra_CrsMatrix> emat;

    EpetraMatCnt(int n, int nloc, int rnnz, int rawComm) :
#ifdef HAVE_MPI
        Comm(rawComm),
#else
        //Comm(),
#endif
        emap(n,nloc,1,Comm),
        emat(new Epetra_CrsMatrix(Copy,emap,rnnz))
    {}

    ~EpetraMatCnt(){
        //delete Comm;
        //delete emap;
        //delete emat;
    }
};

class EpetraMatStore {
public:
    EpetraMatStore():
        cid(0)
    {
        if(verbose) std::cout << "Constructing new matrix store" << std::endl;
        return;
    }

    int new_data(const int n, const int nloc, const int rnnz, const int rawComm) {
        things_[cid]=new EpetraMatCnt(n,nloc,rnnz,rawComm);
        cid++;
        return cid-1;
    }

    int set_data(const int id, const int i, const int nnz, const int j[], const double val[]) {
        return things_[id]->emat->InsertGlobalValues(i,nnz,val,j);
    }

    int assemble_data(const int id){
        return things_[id]->emat->FillComplete();
    }

    //defering this for a while
    //int get_data(const int id, const int i, double &val) {
    //    val = things_[id]->emat[i-1];   Need to overload this like the vector
    //    return 0;
    //}

    int edit_data(const int id) {
        return EpetraExt::RowMatrixToMatlabFile("mymatrix.m",*(things_[id]->emat));
    }

    int normF_data(const int id, double &x) {
        x=things_[id]->emat->NormFrobenius();
        return 0;
    }

    Teuchos::RCP<Epetra_CrsMatrix> get_mat(const int id){
        return things_[id]->emat;
    }

private:
        int cid;
        map<int, EpetraMatCnt*> things_;
};
