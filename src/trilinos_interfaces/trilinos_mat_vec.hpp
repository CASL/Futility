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
#include "Epetra_Import.h"
#include "Epetra_DistObject.h"
#include "Epetra_Vector.h"
#include "Epetra_CrsMatrix.h"
#include "EpetraExt_VectorOut.h"
#include "EpetraExt_RowMatrixOut.h"
#include <cassert>

using std::map;

class EpetraVecCnt{
public:
#ifdef HAVE_MPI
    Epetra_MpiComm Comm;
#else
    Epetra_SerialComm Comm;
#endif
    Epetra_Map emap;
    Teuchos::RCP<Epetra_Map> distMap;
    Teuchos::RCP<Epetra_Vector> evec;
    Teuchos::RCP<Epetra_Import> importer;
    Teuchos::RCP<Epetra_Vector> distvec;
    bool havedistMap;

#ifdef HAVE_MPI
    EpetraVecCnt(int n, int nloc, MPI_Comm rawComm) :
        Comm(rawComm),
#else
    EpetraVecCnt(int n, int nloc, int rawComm) :
        Comm(),
#endif
        emap(n,nloc,1,Comm),
        evec(new Epetra_Vector(emap))
    {
        evec->PutScalar(0.);
        havedistMap=false;
    }

    double &operator[](int i){ return (*evec)[i];}

    ~EpetraVecCnt(){
        evec = Teuchos::null;
        distMap = Teuchos::null;
    }
};

class EpetraVecStore {
public:
    EpetraVecStore():
        cid(0)
    {}

#ifdef HAVE_MPI
    int new_data(const int n, const int nloc, const MPI_Comm rawComm) {
#else
    int new_data(const int n, const int nloc, const int rawComm) {
#endif
        vec_map[cid]=new EpetraVecCnt(n,nloc,rawComm);
        //vec_map[cid]->Comm.PrintInfo(std::cout);
        cid++;
        return cid-1;
    }

    int define_map_data(const int id, const int nloc, const int *gid){
        vec_map[id]->distMap=Teuchos::rcp(new Epetra_Map(-1,nloc,gid,1,vec_map[id]->Comm));
        vec_map[id]->importer=Teuchos::rcp(new Epetra_Import(*(vec_map[id]->distMap),vec_map[id]->emap));
        vec_map[id]->distvec=Teuchos::rcp(new Epetra_Vector(*(vec_map[id]->distMap)));
        vec_map[id]->havedistMap=true;
        return 0;
    }

    int delete_data(const int id){
        delete vec_map[id];
        vec_map.erase(id);
        return 0;
    }

    int set_data(const int id, const int *i, const double *val) {
        return vec_map[id]->evec->ReplaceGlobalValues(1,val,i);
    }

    int setall_data(const int id, const double val) {
        return vec_map[id]->evec->PutScalar(val);
    }

    int transfer_data(const int id) {
        return vec_map[id]->distvec->Import(*(vec_map[id]->evec),*(vec_map[id]->importer),Insert);
    }

    int get_data(const int id, const int i, double &val) {
        if(vec_map[id]->havedistMap){
            int lid=vec_map[id]->distMap->LID(i);
            if(lid>=0){
                val = (*(vec_map[id]->distvec))[lid];
                return 0;
            }
            else return lid;
        }
        else{
            int lid=vec_map[id]->emap.LID(i);
            if(lid>=0){
                val = (*vec_map[id])[lid];
                return 0;
            }
            else return lid;
        }
    }

    int copy_data(const int id, const int idfrom) {
        *(vec_map[id]->evec)= *(vec_map[idfrom]->evec);
        return 0;
    }

    int axpy_data(const int id, const int idx, const double a, const double b) {
        return vec_map[id]->evec->Update(a,*(vec_map[idx]->evec),b);
    }

    int norm1_data(const int id, double val[]) {
        return vec_map[id]->evec->Norm1(val);
    }

    int norm2_data(const int id, double val[]) {
        return vec_map[id]->evec->Norm2(val);
    }

    int max_data(const int id, double val[]) {
        return vec_map[id]->evec->MaxValue(val);
    }

    int scale_data(const int id, double val) {
        return vec_map[id]->evec->Scale(val);
    }

    //TODO: eventually send a string in
    int edit_data(const int id, const char name[]) {
        return EpetraExt::VectorToMatlabFile(name,*(vec_map[id]->evec));
    }

    Teuchos::RCP<Epetra_Vector> get_vec(const int id){
        return vec_map[id]->evec;
    }

private:
        int cid;
        map<int, EpetraVecCnt*> vec_map;
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
    int m_rnnz;

#ifdef HAVE_MPI
    EpetraMatCnt(int n, int nloc, int rnnz, MPI_Comm rawComm) :
        Comm(rawComm),
#else
    EpetraMatCnt(int n, int nloc, int rnnz, int rawComm) :
        Comm(),
#endif
        emap(n,nloc,1,Comm),
        emat(new Epetra_CrsMatrix(Copy,emap,rnnz))
    {
        m_rnnz=rnnz;
    }
};

class EpetraMatStore {
public:
    EpetraMatStore():
        cid(0)
    {}

#ifdef HAVE_MPI
    int new_data(const int n, const int nloc, const int rnnz, const MPI_Comm rawComm) {
#else
    int new_data(const int n, const int nloc, const int rnnz, const int rawComm) {
#endif
        mat_map[cid]=new EpetraMatCnt(n,nloc,rnnz,rawComm);
        cid++;
        return cid-1;
    }

    int delete_data(const int id){
        delete mat_map[id];
        mat_map.erase(id);
        return 0;
    }


    int reset_data(const int id){
        mat_map[id]->emat=Teuchos::RCP<Epetra_CrsMatrix>(new Epetra_CrsMatrix(Copy,mat_map[id]->emap,mat_map[id]->m_rnnz));
        return 0;
    }

    int set_data(const int id, const int i, const int nnz, const int j[], const double val[]) {
        //std::cout << id << " - " << i << " - " << nnz << " - " << mat_map[id]->b_asy << std::endl;
        //for (int it = 0; it < nnz; it++) { std::cout << j[it] << " ";}
        //std::cout << std::endl;
        //for (int it = 0; it < nnz; it++) { std::cout << val[it]<< " ";}
        //std::cout << std::endl;
        int ierr = mat_map[id]->emat->InsertGlobalValues(i,nnz,val,j);
        if(ierr!=0) ierr = mat_map[id]->emat->ReplaceGlobalValues(i,nnz,val,j);
        if(ierr!=0) std::cout << id << " - " << ierr << " - " << i << std::endl;
        //std::cout << ierr << std::endl;
        return ierr;
    }

    int assemble_data(const int id){
        mat_map[id]->b_asy=true;
        return mat_map[id]->emat->FillComplete();
    }

    //defering this for a while
    int get_data(const int id, const int i, const int j, double &val) {
        val=0.0;
        int N=0;
        int* ind = NULL;
        double* row = NULL;
        int lid=mat_map[id]->emat->LCID(i);
        int ierr=mat_map[id]->emat->ExtractMyRowView(lid, N, row, ind);
        for(int k=0; k<N; k++){
            if(ind[k]==j-1){
                val = row[k];
                break;
            }
        }
        return 0;
    }

    int matvec_data(const int id, const bool trans, Teuchos::RCP<Epetra_Vector> x, Teuchos::RCP<Epetra_Vector> y){
        return mat_map[id]->emat->Multiply(trans,*x,*y);
    }

    int edit_data(const int id,const char name[]) {
        return EpetraExt::RowMatrixToMatlabFile(name,*(mat_map[id]->emat));
    }

    int normF_data(const int id, double &x) {
        x=mat_map[id]->emat->NormFrobenius();
        return 0;
    }

    Teuchos::RCP<Epetra_CrsMatrix> get_mat(const int id){
        return mat_map[id]->emat;
    }

private:
        int cid;
        map<int, EpetraMatCnt*> mat_map;
};
