#include "store.hpp"
#include "store_solvers.hpp"

EpetraVecStore *evec = nullptr;
EpetraMatStore *emat = nullptr;
AnasaziStore   *aeig = nullptr;

extern "C" void MPACT_Trilinos_Init() {
    assert(!evec);
    assert(!emat);
    if(verbose) std::cout << "Initializing data store" << std::endl;
    evec = new EpetraVecStore();
    emat = new EpetraMatStore();
    aeig = new AnasaziStore();
}

extern "C" void MPACT_Trilinos_Finalize() {
    if(verbose) std::cout << "Deleting data store" << std::endl;
    delete evec;
    delete emat;
    delete aeig;
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
    assert(ierr==0);
}

extern "C" void ForPETRA_VecEdit(const int id) {
    int ierr = evec->edit_data(id);
    assert(ierr==0);
}

//Matrix
extern "C" void ForPETRA_MatInit( int &id, const int n, const int nlocal, const int rnnz, const int Comm ) {
    id = emat->new_data(n,nlocal,rnnz,Comm);
}

extern "C" void ForPETRA_MatSet(const int id, const int i, const int nnz, const int j[], const double val[]) {
    int ierr = emat->set_data(id,i,nnz,j,val);
    std::cout << id << " " << i << " " << nnz << " " << j[0] << " " << val[0] << " " << ierr << std::endl;
    assert(ierr==0);
}

extern "C" void ForPETRA_MatAssemble(const int id) {
    int ierr = emat->assemble_data(id);
    assert(ierr==0);
}

//defer this for a while...
//extern "C" void ForPETRA_MatGet(const int id, const int i, const int j, double val) {
//    ierr = emat->get_data(id,i,j,val);
//    assert(ierr==0);
//}

extern "C" void ForPETRA_MatEdit(const int id) {
    int ierr = emat->edit_data(id);
    assert(ierr==0);
}

extern "C" void ForPETRA_MatNormF(const int id, double &val) {
    int ierr = emat->normF_data(id,val);
    assert(ierr==0);
}

extern "C" void test_solve(const int idLHS, const int idRHS, const int idX) {
    int id=aeig->new_data();
    std::cout << id << std::endl;

    aeig->setMat_data(id,emat->get_mat(idLHS),emat->get_mat(idRHS));
    aeig->setX0_data(id,evec->get_vec(idX));
    aeig->solve(id);

}