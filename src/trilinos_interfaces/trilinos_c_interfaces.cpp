//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
//                          Futility Development Group                          !
//                             All rights reserved.                             !
//                                                                              !
// Futility is a jointly-maintained, open-source project between the University !
// of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
// can be found in LICENSE.txt in the head directory of this repository.        !
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
#ifdef HAVE_MPI
#include "mpi.h"
#endif
#ifdef MPACT_HAVE_Trilinos
#include "trilinos_anderson.hpp"
#include "trilinos_mat_vec.hpp"
#include "trilinos_pc.hpp"
#include "trilinos_solvers.hpp"
#include <omp.h>
#include "CTeuchos_ParameterList.h"
#include "CTeuchos_ParameterList_Cpp.hpp"

bool mpact_trilinos_isinit = false;
Teuchos::RCP< EpetraVecStore > evec(new EpetraVecStore);
Teuchos::RCP< EpetraMatStore > emat(new EpetraMatStore);
Teuchos::RCP< AnasaziStore   > aeig(new AnasaziStore);
Teuchos::RCP< BelosStore     > bels(new BelosStore);
Teuchos::RCP< AndersonStore  > andr(new AndersonStore);
Teuchos::RCP< JFNKStore      > jfnk(new JFNKStore);
Teuchos::RCP< PCStore        > pcst(new PCStore);

//------------------------------------------------------------------------------
// Vector
//------------------------------------------------------------------------------
extern "C" void ForPETRA_VecInit(int &id, const int n, const int nlocal,
                                 const int Comm) {
#ifdef HAVE_MPI
    id = evec->new_data(n, nlocal, MPI_Comm_f2c(Comm));
#else
    id = evec->new_data(n, nlocal, Comm);
#endif
}

extern "C" void ForPETRA_VecSetImportMap(const int id, const int n,
                                         const int *gids) {
    int ierr = evec->define_map_data(id, n, gids);
    assert(ierr == 0);
}

extern "C" void ForPETRA_VecDestroy(const int id) {
    evec->delete_data(id);
}

extern "C" void ForPETRA_VecSet(const int id, const int i, const double val) {
    int ierr = evec->set_data(id, &i, &val);
    assert(ierr == 0);
}

extern "C" void ForPETRA_VecSetAll(const int id, const double val) {
    int ierr = evec->setall_data(id, val);
    assert(ierr == 0);
}

extern "C" void ForPETRA_VecTransfer(const int id) {
    evec->transfer_data(id);
}

extern "C" void ForPETRA_VecGet(const int id, const int i, double &val) {
    int ierr = evec->get_data(id, i, val);
    assert(ierr == 0);
}

extern "C" void ForPETRA_VecCopy(const int id, const int idfrom) {
    int ierr = evec->copy_data(id, idfrom);
    assert(ierr == 0);
}

extern "C" void ForPETRA_VecAXPY(const int id, const int idx, const double a,
                                 const double b) {
    int ierr = evec->axpy_data(id, idx, a, b);
    assert(ierr == 0);
}

extern "C" void ForPETRA_VecSum(const int id, double &val) {
    double vals[1];
    int ierr = evec->norm1_data(id, vals);
    val      = vals[0];
    assert(ierr == 0);
}

extern "C" void ForPETRA_VecNorm2(const int id, double &val) {
    double vals[1];
    int ierr = evec->norm2_data(id, vals);
    val      = vals[0];
    assert(ierr == 0);
}

extern "C" void ForPETRA_VecMax(const int id, double &val) {
    double vals[1];
    int ierr = evec->max_data(id, vals);
    val      = vals[0];
    assert(ierr == 0);
}

extern "C" void ForPETRA_VecScale(const int id, const double val) {
    int ierr = evec->scale_data(id, val);
    assert(ierr == 0);
}

extern "C" void ForPETRA_VecEdit(const int id, const char name[]) {
    int ierr = evec->edit_data(id, name);
    assert(ierr == 0);
}

//------------------------------------------------------------------------------
// Matrix
//------------------------------------------------------------------------------
extern "C" void ForPETRA_MatInit(int &id, const int n, const int nlocal,
                                 const int rnnz, const int Comm) {
#ifdef HAVE_MPI
    id = emat->new_data(n, nlocal, rnnz, MPI_Comm_f2c(Comm));
#else
    id = emat->new_data(n, nlocal, rnnz, Comm);
#endif
}

extern "C" void ForPETRA_MatDestroy(const int id) {
    emat->delete_data(id);
}

extern "C" void ForPETRA_MatReset(const int id) {
    emat->reset_data(id);
}

extern "C" void ForPETRA_MatSet(const int id, const int i, const int nnz,
                                const int j[], const double val[]) {
    int ierr = emat->set_data(id, i, nnz, j, val);
    assert(ierr == 0);
}

extern "C" void ForPETRA_MatAssemble(const int id) {
    int ierr = emat->assemble_data(id);
    assert(ierr == 0);
}

extern "C" void ForPETRA_MatGet(const int id, const int i, const int j,
                                double &val) {
    int ierr = emat->get_data(id, i, j, val);
    assert(ierr == 0);
}

extern "C" void ForPETRA_MatMult(const int idA, const bool trans, const int idX,
                                 const int idY) {
    int ierr =
        emat->matvec_data(idA, trans, evec->get_vec(idX), evec->get_vec(idY));
    assert(ierr == 0);
}

extern "C" void ForPETRA_MatEdit(const int id, const char name[]) {
    int ierr = emat->edit_data(id, name);
    assert(ierr == 0);
}

extern "C" void ForPETRA_MatNormF(const int id, double &val) {
    int ierr = emat->normF_data(id, val);
    assert(ierr == 0);
}

//------------------------------------------------------------------------------
// Anasazi
//------------------------------------------------------------------------------
extern "C" void Anasazi_Init(int &id) {
    Teuchos::ParameterList params;
    id = aeig->new_data(params);
}

extern "C" void Anasazi_Init_Params(int &id, CTeuchos_ParameterList_ID &plist) {
    auto plistDB = CTeuchos::getNonconstParameterListDB();
    Teuchos::Ptr<Teuchos::ParameterList> params =
        plistDB->getNonconstObjPtr(plist.id);
    id = aeig->new_data(*params);
}

extern "C" void Anasazi_Destroy(const int id) {
    aeig->delete_data(id);
}

extern "C" void Anasazi_SetMat(const int id, const int idLHS, const int idRHS) {
    aeig->setMat_data(id, emat->get_mat(idLHS), emat->get_mat(idRHS));
}

extern "C" void Anasazi_SetPC(const int id, const int idpc) {
    aeig->setPC_data(id, idpc, pcst->get_pc(idpc));
}

extern "C" void Anasazi_SetX(const int id, const int idX) {
    aeig->setX0_data(id, evec->get_vec(idX));
}

extern "C" void Anasazi_SetConvCrit(const int id, const double tol,
                                    const int maxit) {
    aeig->setConvCrit_data(id, tol, maxit);
}

extern "C" void Anasazi_Solve(int id) {
    int ierr = aeig->solve(id);
    // if(ierr>0){
    //    //update pc
    //    int pcid;
    //    Teuchos::RCP<Epetra_CrsMatrix> M;
    //    aeig->getPCid_data(id,pcid,M);
    //    pcst->resetPC_data(pcid,M);
    //    //resolve
    //    int ierr = aeig->solve(id);
    //}
    // assert(ierr==0);
}

extern "C" void Anasazi_GetEigenvalue(const int id, double &k) {
    aeig->getEigenvalue_data(id, k);
}

extern "C" void Anasazi_GetResid(const int id, double &resid) {
    aeig->getResidual(id, resid);
}

extern "C" void Anasazi_GetIterationCount(const int id, int &niter) {
    aeig->getIterations_data(id, niter);
}

//------------------------------------------------------------------------------
// Belos
//------------------------------------------------------------------------------
extern "C" void Belos_Init(int &id) {
    Teuchos::ParameterList params;
    id = bels->new_data(params);
}

extern "C" void Belos_Init_Params(int &id, CTeuchos_ParameterList_ID &plist) {
    auto plistDB = CTeuchos::getNonconstParameterListDB();
    Teuchos::Ptr<Teuchos::ParameterList> params =
        plistDB->getNonconstObjPtr(plist.id);
    id = bels->new_data(*params);
}

extern "C" void Belos_Destroy(const int id) {
    bels->delete_data(id);
}

extern "C" void Belos_SetMat(const int id, const int idA) {
    bels->setMat_data(id, emat->get_mat(idA));
}

extern "C" void Belos_SetPC(const int id, const int idpc) {
    bels->setPC_data(id, pcst->get_pc(idpc));
}

extern "C" void Belos_SetX(const int id, const int idX) {
    bels->setX0_data(id, evec->get_vec(idX));
}

extern "C" void Belos_Setb(const int id, const int idb) {
    bels->setb_data(id, evec->get_vec(idb));
}

extern "C" void Belos_SetConvCrit(const int id, const double tol,
                                  const int maxit) {
    bels->setConvCrit_data(id, tol, maxit);
}

extern "C" void Belos_Solve(int id) {
    bels->solve(id);
}

extern "C" void Belos_GetResid(const int id, double &resid) {
    bels->getResidual(id, resid);
}

extern "C" void Belos_GetIterationCount(const int id, int &niter) {
    bels->getIterations_data(id, niter);
}

//------------------------------------------------------------------------------
// Preconditioner
//--------------------------------------------------------------------
extern "C" void Preconditioner_Init(int &id, int opt) {
    auto plistDB = CTeuchos::getNonconstParameterListDB();
    Teuchos::ParameterList params;
    params.set("pc_option", opt);
    id = pcst->new_data(params);
}

extern "C" void Preconditioner_InitParams(int &id,
                                          CTeuchos_ParameterList_ID &plist) {
    auto plistDB = CTeuchos::getNonconstParameterListDB();
    Teuchos::Ptr<Teuchos::ParameterList> params =
        plistDB->getNonconstObjPtr(plist.id);
    id = pcst->new_data(*params);
}

extern "C" void Preconditioner_Destroy(const int id) {
    pcst->delete_data(id);
}

extern "C" void Preconditioner_Setup(const int id, const int idM) {
    pcst->setupPC_data(id, emat->get_mat(idM));
    // std::cout << (pcst->get_pc(pid))->Label() << std::endl;
}

extern "C" void Preconditioner_Reset(const int id, const int idM) {
    pcst->resetPC_data(id, emat->get_mat(idM));
    // std::cout << (pcst->get_pc(pid))->Label() << std::endl;
}

//------------------------------------------------------------------------------
// Anderson
//------------------------------------------------------------------------------
extern "C" void Anderson_Init(int &id, const int depth, const double beta,
                              const int start, const int idv) {
    id = andr->new_data(depth, beta, start, evec->get_vec(idv));
}

extern "C" void Anderson_Destroy(const int id) {
    andr->delete_data(id);
}

extern "C" void Anderson_Update(const int id) {
    andr->step(id);
}

extern "C" void Anderson_Reset(const int id) {
    andr->reset_data(id);
}

//------------------------------------------------------------------------------
// JFNK NOX
//------------------------------------------------------------------------------
extern "C" void JFNK_Init(int &id, void (*funptr)(), const int idx,
                          const int idF) {
    id = jfnk->new_data(funptr, evec->get_vec(idx), evec->get_vec(idF));
}

extern "C" void JFNK_Destroy(const int id) {
    jfnk->delete_data(id);
}

extern "C" void Anderson_Solve(const int id) {
    jfnk->solve(id);
}
#endif
