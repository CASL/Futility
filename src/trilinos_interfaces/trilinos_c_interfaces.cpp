/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
/                          Futility Development Group                          !
/                             All rights reserved.                             !
/                                                                              !
/ Futility is a jointly-maintained, open-source project between the University !
/ of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
/ can be found in LICENSE.txt in the head directory of this repository.        !
/+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/
#ifdef HAVE_MPI
#include "mpi.h"
#endif
#ifdef FUTILITY_HAVE_Trilinos
#include "trilinos_mat_vec.hpp"
//#include "trilinos_anderson.hpp"
#include <omp.h>
#include "CTeuchos_ParameterList.h"
#include "CTeuchos_ParameterList_Cpp.hpp"
#include "trilinos_pc.hpp"
#include "trilinos_solvers.hpp"
#endif
#ifdef FUTILITY_HAVE_Kokkos
#include <Kokkos_Core.hpp>
#endif 
#include <iostream>
// #include "trilinos_ts.hpp"

#ifdef FUTILITY_HAVE_Trilinos
bool futility_trilinos_isinit = false;
Teuchos::RCP<TpetraVecStore> tvec(new TpetraVecStore);
Teuchos::RCP<TpetraMatStore> tmat(new TpetraMatStore);
Teuchos::RCP<PCStore> pcst(new PCStore);
Teuchos::RCP<AnasaziStore> aeig(new AnasaziStore);
Teuchos::RCP<BelosStore> bels(new BelosStore);
#endif
//Teuchos::RCP< AndersonStore  > andr(new AndersonStore);
//Teuchos::RCP<TSStore> tsst(new TSStore);
//------------------------------------------------------------------------------
// Kokkos
//------------------------------------------------------------------------------
#ifdef FUTILITY_HAVE_Kokkos
extern "C" void InitializeKokkos(int num_threads, int device_id)
{
    Kokkos::InitArguments args;
    // Necessary to recase these as const.
    const int threads = num_threads;
    const int device = device_id;
    args.num_threads = threads;
    args.device_id = device;
    Kokkos::initialize(args);
    std::cout << "Kokkos init done" << std::endl;
}

extern "C" void FinalizeKokkos()
{
    Kokkos::finalize();
}
#endif

//------------------------------------------------------------------------------
// Vector
//------------------------------------------------------------------------------
#ifdef FUTILITY_HAVE_Trilinos 
extern "C" void ForPETRA_VecInit(int &id, const int n, const int nlocal,
                                 const int Comm)
{
#ifdef HAVE_MPI
    id = tvec->new_data(n, nlocal, MPI_Comm_f2c(Comm));
#else
    id = tvec->new_data(n, nlocal, Comm);
#endif
}

extern "C" void ForPETRA_VecSetImportMap(const int id, const int n,
                                         const int *gids)
{
    int ierr = tvec->define_map_data(id, n, gids);
    assert(ierr == 0);
}

extern "C" void ForPETRA_VecDestroy(const int id)
{
    tvec->delete_data(id);
}

extern "C" void ForPETRA_VecSet(const int id, const int i, const double val)
{
    tvec->set_data(id, &i, &val);
}

extern "C" void ForPETRA_VecSetAll(const int id, const double val)
{
    tvec->setall_data(id, val);
}

extern "C" void ForPETRA_VecTransfer(const int id)
{
    tvec->transfer_data(id);
}

extern "C" void ForPETRA_VecGet(const int id, const int i, double &val)
{
    int ierr = tvec->get_data(id, i, val);
    assert(ierr == 0);
}

extern "C" void ForPETRA_VecCopy(const int id, const int idfrom)
{
    int ierr = tvec->copy_data(id, idfrom);
    assert(ierr == 0);
}

extern "C" void ForPETRA_VecAXPY(const int id, const int idx, const double a,
                                 const double b)
{
    int ierr = tvec->axpy_data(id, idx, a, b);
    assert(ierr == 0);
}

extern "C" void ForPETRA_VecSum(const int id, double &val)
{
    double vals[1];
    int ierr = tvec->norm1_data(id, vals);
    val      = vals[0];
    assert(ierr == 0);
}

extern "C" void ForPETRA_VecNorm2(const int id, double &val)
{
    double vals[1];
    int ierr = tvec->norm2_data(id, vals);
    val      = vals[0];
    assert(ierr == 0);
}

extern "C" void ForPETRA_VecMax(const int id, double &val)
{
    double vals[1];
    int ierr = tvec->max_data(id, vals);
    val      = vals[0];
    assert(ierr == 0);
}

extern "C" void ForPETRA_VecScale(const int id, const double val)
{
    int ierr = tvec->scale_data(id, val);
    assert(ierr == 0);
}

extern "C" void ForPETRA_VecEdit(const int id, const char name[])
{
    int ierr = tvec->edit_data(id, name);
    assert(ierr == 0);
}

//------------------------------------------------------------------------------
// Matrix
//------------------------------------------------------------------------------
extern "C" void ForPETRA_MatInit(int &id, const int n, const int nlocal,
                                 const int rnnz, const int Comm)
{
#ifdef HAVE_MPI
    id = tmat->new_data(n, nlocal, rnnz, MPI_Comm_f2c(Comm));
#else
    id = tmat->new_data(n, nlocal, rnnz, Comm);
#endif
}

extern "C" void ForPETRA_MatDestroy(const int id)
{
    tmat->delete_data(id);
}

extern "C" void ForPETRA_MatReset(const int id)
{
    tmat->reset_data(id);
}

extern "C" void ForPETRA_MatSet(const int id, const int i, const int nnz,
                                const int j[], const double val[])
{
    int ierr = tmat->set_data(id, i, nnz, j, val);
    assert(ierr == 0);
}

extern "C" void ForPETRA_MatAssemble(const int id)
{
    int ierr = tmat->assemble_data(id);
    assert(ierr == 0);
}

extern "C" void ForPETRA_MatGet(const int id, const int i, const int j,
                                double &val)
{
    int ierr = tmat->get_data(id, i, j, val);
    assert(ierr == 0);
}

extern "C" void ForPETRA_MatMult(const int idA, const bool trans, const int idX,
                                 const int idY)
{
    int ierr =
        tmat->matvec_data(idA, trans, tvec->get_vec(idX), tvec->get_vec(idY));
    assert(ierr == 0);
}

extern "C" void ForPETRA_MatEdit(const int id, const char name[])
{
    int ierr = tmat->edit_data(id, name);
    assert(ierr == 0);
}

extern "C" void ForPETRA_MatNormF(const int id, double &val)
{
    int ierr = tmat->normF_data(id, val);
    assert(ierr == 0);
}

//------------------------------------------------------------------------------
// Anasazi
//------------------------------------------------------------------------------
extern "C" void Anasazi_Init(int &id)
{
    Teuchos::ParameterList params;
    id = aeig->new_data(params);
}

extern "C" void Anasazi_Init_Params(int &id, CTeuchos_ParameterList_ID &plist)
{
    auto plistDB = CTeuchos::getNonconstParameterListDB();
    Teuchos::Ptr<Teuchos::ParameterList> params =
        plistDB->getNonconstObjPtr(plist.id);
    id = aeig->new_data(*params);
}

extern "C" void Anasazi_Destroy(const int id)
{
    aeig->delete_data(id);
}

extern "C" void Anasazi_SetMat(const int id, const int idLHS, const int idRHS)
{
    aeig->setMat_data(id, tmat->get_mat(idLHS), tmat->get_mat(idRHS));
}

extern "C" void Anasazi_SetPC(const int id, const int idpc)
{
    aeig->setPC_data(id, idpc, pcst->get_pc(idpc));
}

extern "C" void Anasazi_SetX(const int id, const int idX)
{
    aeig->setX0_data(id, tvec->get_vec(idX));
}

extern "C" void Anasazi_SetConvCrit(const int id, const double tol,
                                    const int maxit)
{
    aeig->setConvCrit_data(id, tol, maxit);
}

extern "C" void Anasazi_Solve(int id)
{
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

extern "C" void Anasazi_GetEigenvalue(const int id, double &k)
{
    aeig->getEigenvalue_data(id, k);
}

extern "C" void Anasazi_GetResid(const int id, double &resid)
{
    aeig->getResidual(id, resid);
}

extern "C" void Anasazi_GetIterationCount(const int id, int &niter)
{
    aeig->getIterations_data(id, niter);
}

//------------------------------------------------------------------------------
// Belos
//------------------------------------------------------------------------------
extern "C" void Belos_Init(int &id)
{
    Teuchos::ParameterList params;
    id = bels->new_data(params);
}

extern "C" void Belos_Init_Params(int &id, CTeuchos_ParameterList_ID &plist)
{
    auto plistDB = CTeuchos::getNonconstParameterListDB();
    Teuchos::Ptr<Teuchos::ParameterList> params =
        plistDB->getNonconstObjPtr(plist.id);
    id = bels->new_data(*params);
}

extern "C" void Belos_Destroy(const int id)
{
    bels->delete_data(id);
}

extern "C" void Belos_SetMat(const int id, const int idA)
{
    bels->setMat_data(id, tmat->get_mat(idA));
}

extern "C" void Belos_SetPC(const int id, const int idpc)
{
    bels->setPC_data(id, pcst->get_pc(idpc));
}

extern "C" void Belos_SetX(const int id, const int idX)
{
    bels->setX0_data(id, tvec->get_vec(idX));
}

extern "C" void Belos_Setb(const int id, const int idb)
{
    bels->setb_data(id, tvec->get_vec(idb));
}

extern "C" void Belos_SetConvCrit(const int id, const double tol,
                                  const int maxit)
{
    bels->setConvCrit_data(id, tol, maxit);
}

extern "C" void Belos_Solve(int id)
{
    bels->solve(id);
}

extern "C" void Belos_GetResid(const int id, double &resid)
{
    bels->getResidual(id, resid);
}

extern "C" void Belos_GetIterationCount(const int id, int &niter)
{
    bels->getIterations_data(id, niter);
}

//------------------------------------------------------------------------------
// Preconditioner
//--------------------------------------------------------------------
extern "C" void Preconditioner_Init(int &id, int opt)
{
    auto plistDB = CTeuchos::getNonconstParameterListDB();
    Teuchos::ParameterList params;
    params.set("pc_option", opt);
    id = pcst->new_data(params);
}

extern "C" void Preconditioner_InitParams(int &id,
                                          CTeuchos_ParameterList_ID &plist)
{
    auto plistDB = CTeuchos::getNonconstParameterListDB();
    Teuchos::Ptr<Teuchos::ParameterList> params =
        plistDB->getNonconstObjPtr(plist.id);
    id = pcst->new_data(*params);
}

extern "C" void Preconditioner_Destroy(const int id)
{
    pcst->delete_data(id);
}

extern "C" void Preconditioner_Setup(const int id, const int idM)
{
    pcst->setupPC_data(id, tmat->get_mat(idM));
    // std::cout << (pcst->get_pc(pid))->Label() << std::endl;
}

extern "C" void Preconditioner_Reset(const int id, const int idM)
{
    pcst->resetPC_data(id, tmat->get_mat(idM));
    // std::cout << (pcst->get_pc(pid))->Label() << std::endl;
}

//------------------------------------------------------------------------------
// Anderson
//------------------------------------------------------------------------------
/*   Disabled to cut down build time and currently unused

extern "C" void Anderson_Init(int &id, const int depth, const double beta,
                              const int start, const int idv)
{
    id = andr->new_data(depth, beta, start, tvec->get_vec(idv));
}

extern "C" void Anderson_Destroy(const int id)
{
    andr->delete_data(id);
}

extern "C" void Anderson_Update(const int id)
{
    andr->step(id);
}

extern "C" void Anderson_Reset(const int id)
{
    andr->reset_data(id);
}

//------------------------------------------------------------------------------
// Rythmos Time stepper
//------------------------------------------------------------------------------
extern "C" void TS_Init(int &id, TSStore::FunctionPointer fptr, int n,
                        double tol)
{
    Teuchos::ParameterList params;
    id = tsst->new_data(fptr, n, tol, params);
}

extern "C" void TS_Init_Params(int &id, TSStore::FunctionPointer fptr, int n,
                               double tol, CTeuchos_ParameterList_ID &plist)
{
    auto plistDB = CTeuchos::getNonconstParameterListDB();
    Teuchos::Ptr<Teuchos::ParameterList> params =
        plistDB->getNonconstObjPtr(plist.id);
    id = tsst->new_data(fptr, n, tol, *params);
}

extern "C" void TS_Destroy(int id)
{
    tsst->delete_data(id);
}

extern "C" void TS_Step(int id, double tstart, double tend, double *xstart,
                        double *xend)
{
    // convert to vector in here?
    int idS = -1;
    int idE = -1;
    (*tsst)[id].step(tstart, tend, xstart, xend);
}
*/
#endif
