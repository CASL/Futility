#include "store.hpp"
#include "store_solvers.hpp"
#include "store_pc.hpp"

EpetraVecStore *evec = nullptr;
EpetraMatStore *emat = nullptr;
AnasaziStore   *aeig = nullptr;
BelosStore     *bels = nullptr;
PCStore        *pcst = nullptr;

extern "C" void MPACT_Trilinos_Init() {
    assert(!evec);
    assert(!emat);
    assert(!aeig);
    assert(!bels);
    assert(!pcst);
    if(verbose) std::cout << "Initializing data store" << std::endl;
    evec = new EpetraVecStore();
    emat = new EpetraMatStore();
    aeig = new AnasaziStore();
    bels = new BelosStore();
    pcst = new PCStore();
}

extern "C" void MPACT_Trilinos_Finalize() {
    if(verbose) std::cout << "Deleting data store" << std::endl;
    delete aeig;
    delete bels;
    delete pcst;
    delete evec;
    delete emat;
}

//------------------------------------------------------------------------------
//Vector
//------------------------------------------------------------------------------
extern "C" void ForPETRA_VecInit( int &id, const int n, const int nlocal, const int Comm ) {
    id = evec->new_data(n,nlocal,Comm);
}

extern "C" void ForPETRA_VecSet(const int id, const int i, const double val) {
    int ierr = evec->set_data(id,&i,&val);
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

//------------------------------------------------------------------------------
//Matrix
//------------------------------------------------------------------------------
extern "C" void ForPETRA_MatInit( int &id, const int n, const int nlocal, const int rnnz, const int Comm ) {
    id = emat->new_data(n,nlocal,rnnz,Comm);
}

extern "C" void ForPETRA_MatSet(const int id, const int i, const int nnz, const int j[], const double val[]) {
    int ierr = emat->set_data(id,i,nnz,j,val);
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

//------------------------------------------------------------------------------
//Anasazi
//------------------------------------------------------------------------------
extern "C" void Anasazi_Init( int &id) {
    id = aeig->new_data();
}

extern "C" void Anasazi_SetMat( const int id, const int idLHS, const int idRHS) {
    aeig->setMat_data(id,emat->get_mat(idLHS),emat->get_mat(idRHS));
}

extern "C" void Anasazi_SetPC( const int id, const int idpc) {
    aeig->setPC_data(id,pcst->get_pc(idpc));
}

extern "C" void Anasazi_SetX( const int id, const int idX) {
    aeig->setX0_data(id,evec->get_vec(idX));
}

extern "C" void Anasazi_Solve( int id) {
    aeig->solve(id);
}

//------------------------------------------------------------------------------
//Belos
//------------------------------------------------------------------------------
extern "C" void Belos_Init( int &id) {
    id = bels->new_data();
}

//------------------------------------------------------------------------------
//Preconditioner
//------------------------------------------------------------------------------
extern "C" void Preconditioner_Init( int &id) {
    id = pcst->new_data();
}

extern "C" void Preconditioner_Setup( const int id, const int idM ) {
    pcst->setupPC_data(id,emat->get_mat(idM));
    //std::cout << (pcst->get_pc(pid))->Label() << std::endl;
}
