#include "store.hpp"

EpetraVecStore *evec = nullptr;
EpetraMatStore *emat = nullptr;

extern "C" void MPACT_Trilinos_Init() {
    assert(!evec);
    assert(!emat);
    if(verbose) std::cout << "Initializing data store" << std::endl;
    evec = new EpetraVecStore();
    emat = new EpetraMatStore();
}

extern "C" void MPACT_Trilinos_Finalize() {
    if(verbose) std::cout << "Deleting data store" << std::endl;
    delete evec;
    delete emat;
}

extern "C" void ForPETRA_VecInit( int &id, const int n, const int nlocal, const int Comm ) {
    id = evec->new_data(n,nlocal,Comm);
}

extern "C" void ForPETRA_VecSet(const int id, const int i, const double val) {
    if(verbose) std::cout << "Adding location " << i << " with value " << val <<std::endl;
    int ierr = evec->set_data(id,&i,&val);
    std::cout << ierr << std::endl;
    assert(ierr==0);
}

extern "C" void ForPETRA_VecGet(const int id, const int i, double &val) {
    int ierr = evec->get_data(id,i,val);
}

extern "C" void ForPETRA_VecEdit(const int id) {
    int ierr = evec->edit_data(id);
    assert(ierr=0);
}

//Matrix
extern "C" void ForPETRA_MatInit( int &id, const int n, const int nlocal, const int rnnz, const int Comm ) {
    id = emat->new_data(n,nlocal,rnnz,Comm);
}

extern "C" void ForPETRA_MatSet(const int id, const int i, const int nnz, const int *j, const double *val) {
    int ierr = emat->set_data(id,i,nnz,j,val);
    assert(ierr=0);
}

//defer this for a while...
//extern "C" void ForPETRA_MatGet(const int id, const int i, const int j, double val) {
//    ierr = emat->get_data(id,i,j,val);
//    assert(ierr=0);
//}

extern "C" void ForPETRA_MatEdit(const int id) {
    int ierr = emat->edit_data(id);
    assert(ierr=0);
}
