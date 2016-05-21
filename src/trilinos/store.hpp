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

bool verbose=false;

class EpetraVecCnt{
public:
#ifdef HAVE_MPI
    Epetra_MpiComm Comm;
#else
    Epetra_SerialComm Comm;
#endif
    Epetra_Map emap;
    Teuchos::RCP<Epetra_Vector> evec;

    EpetraVecCnt(int n, int nloc, MPI_Comm rawComm) :
#ifdef HAVE_MPI
        Comm(MPI_COMM_WORLD),
#else
        //Comm(),
#endif
        emap(n,nloc,1,Comm),
        evec(new Epetra_Vector(emap))
    {
        evec->PutScalar(1.);
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

    int new_data(const int n, const int nloc, const MPI_Comm rawComm) {
        things_[cid]=new EpetraVecCnt(n,nloc,rawComm);
        cid++;
        return cid-1;
    }

    int set_data(const int id, const int *i, const double *val) {
        return things_[id]->evec->ReplaceGlobalValues(1,val,i);
    }

    int setall_data(const int id, const double val) {
        return things_[id]->evec->PutScalar(val);
    }

    int get_data(const int id, const int i, double &val) {
        int lid=things_[id]->emap.LID(i);
        if(lid>=0){
            val = (*things_[id])[lid];
            return 0;
        }
        else return lid;
    }

    int copy_data(const int id, const int idfrom) {
        *(things_[id]->evec)= *(things_[idfrom]->evec);
        return 0;
    }

    int axpy_data(const int id, const int idx, const double a, const double b) {
        return things_[id]->evec->Update(a,*(things_[idx]->evec),b);
    }

    int norm1_data(const int id, double val[]) {
        int ierr = things_[id]->evec->MeanValue(val);
        val[0]*=double(things_[id]->evec->GlobalLength());
        return ierr;
    }

    int max_data(const int id, double val[]) {
        return things_[id]->evec->MaxValue(val);
    }

    int scale_data(const int id, double val) {
        return things_[id]->evec->Scale(val);
    }

    //TODO: eventually send a string in
    int edit_data(const int id, const char name[]) {
        return EpetraExt::VectorToMatlabFile(name,*(things_[id]->evec));
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
    bool b_asy=false;

    EpetraMatCnt(int n, int nloc, int rnnz, MPI_Comm rawComm) :
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

    int new_data(const int n, const int nloc, const int rnnz, const MPI_Comm rawComm) {
        things_[cid]=new EpetraMatCnt(n,nloc,rnnz,rawComm);
        cid++;
        return cid-1;
    }

    int set_data(const int id, const int i, const int nnz, const int j[], const double val[]) {
        //std::cout << id << " - " << i << " - " << nnz << " - " << things_[id]->b_asy << std::endl;
        //for (int it = 0; it < nnz; it++) { std::cout << j[it] << " ";}
        //std::cout << std::endl;
        //for (int it = 0; it < nnz; it++) { std::cout << val[it]<< " ";}
        //std::cout << std::endl;
        int ierr = things_[id]->emat->InsertGlobalValues(i,nnz,val,j);
        if(ierr!=0) ierr = things_[id]->emat->ReplaceGlobalValues(i,nnz,val,j);
        //std::cout << ierr << std::endl;
        return ierr;
    }

    int assemble_data(const int id){
        things_[id]->b_asy=true;
        return things_[id]->emat->FillComplete();
    }

    //defering this for a while
    //int get_data(const int id, const int i, double &val) {
    //    val = things_[id]->emat[i-1];   Need to overload this like the vector
    //    return 0;
    //}

    int matvec_data(const int id, const bool trans, Teuchos::RCP<Epetra_Vector> x, Teuchos::RCP<Epetra_Vector> y){
        return things_[id]->emat->Multiply(trans,*x,*y);
    }

    int edit_data(const int id,const char name[]) {
        return EpetraExt::RowMatrixToMatlabFile(name,*(things_[id]->emat));
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
