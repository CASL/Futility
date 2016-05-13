#pragma once

#include <iostream>
#include <map>
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
    Epetra_Vector evec;

    EpetraVecCnt(int n, int nloc, int rawComm) :
#ifdef HAVE_MPI
        Comm(MPI_COMM_WORLD),
#else
        //Comm(),
#endif
        emap(n,nloc,1,Comm),
        evec(emap)
    {
        evec.PutScalar(0.);
    }
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
        std::cout << n << " " << nloc << " " << rawComm << std::endl;
        things_[cid]=new EpetraVecCnt(n,nloc,rawComm);
        if(verbose) std::cout << things_[cid]->evec.MyLength() <<std::endl;
        cid++;
        return cid-1;
    }

    int set_data(const int id, const int *i, const double *val) {
        if(verbose) std::cout << "replacing location " << i[0] << " with value " << val[0] <<std::endl;
        return things_[id]->evec.ReplaceGlobalValues(1,val,i);
    }

    int get_data(const int id, const int i, double &val) {
        val = things_[id]->evec[i-1];
        return 0;
    }

    int edit_data(const int id) {
        return EpetraExt::VectorToMatlabFile("myvector.m",(things_[id]->evec));
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
    Epetra_CrsMatrix emat;

    EpetraMatCnt(int n, int nloc, int rnnz, int rawComm) :
#ifdef HAVE_MPI
        Comm(rawComm),
#else
        //Comm(),
#endif
        emap(n,nloc,1,Comm),
        emat(Copy,emap,rnnz)
    {}
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

    int set_data(const int id, const int i, const int nnz, const int *j, const double *val) {
        return things_[id]->emat.ReplaceGlobalValues(i,nnz,val,j);
    }

    //defering this for a while
    //int get_data(const int id, const int i, double &val) {
    //    val = things_[id]->emat[i-1];
    //    return 0;
    //}

    int edit_data(const int id) {
        return EpetraExt::RowMatrixToMatlabFile("mymatrix.m",(things_[id]->emat));
    }

private:
        int cid;
        map<int, EpetraMatCnt*> things_;
};
