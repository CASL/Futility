#include <petsc.h>
#include <petscsys.h>
#include <petscfix.h>
#include <petscksp.h>
#if ((PETSC_VERSION_MAJOR<=3) && (PETSC_VERSION_MINOR<=5))
#include <petsc-private/fortranimpl.h>
#include <petsc-private/pcimpl.h>
#else
#include <petsc/private/fortranimpl.h>
#include <petsc/private/pcmgimpl.h>
#endif

#ifdef PETSC_USE_POINTER_CONVERSION
extern void *PetscToPointer(void*);
extern int PetscFromPointer(void *); 
extern void PetscRmPointer(void*);
#else
#define PetscToPointer(a) (*(PetscFortranAddr *)(a))
#define PetscFromPointer(a) (PetscFortranAddr)(a)
#define PetscRmPointer(a)
#endif

#if ((PETSC_VERSION_MAJOR<=3) && (PETSC_VERSION_MINOR<=5))
//This is taken from petsc-3.5/src/ksp/pc/impls/mg/mgimpl.h
//There is no version of petsc/private/pcmgimpl.h that is includable in older
//  versions of PETSc, so we just have to manually redefine the typdefs defined
//  in mgimpl.h here.
typedef struct {
  PetscInt cycles;                             /* Type of cycle to run: 1 V 2 W */
  PetscInt level;                              /* level = 0 coarsest level */
  PetscInt levels;                             /* number of active levels used */
  Vec      b;                                  /* Right hand side */
  Vec      x;                                  /* Solution */
  Vec      r;                                  /* Residual */

  PetscErrorCode (*residual)(Mat,Vec,Vec,Vec);

  Mat           A;                             /* matrix used in forming residual*/
  KSP           smoothd;                       /* pre smoother */
  KSP           smoothu;                       /* post smoother */
  Mat           interpolate;
  Mat           restrct;                       /* restrict is a reserved word in C99 and on Cray */
  Vec           rscale;                        /* scaling of restriction matrix */
  PetscLogEvent eventsmoothsetup;              /* if logging times for each level */
  PetscLogEvent eventsmoothsolve;
  PetscLogEvent eventresidual;
  PetscLogEvent eventinterprestrict;
} PC_MG_Levels;
typedef struct {
  PCMGType  am;                               /* Multiplicative, additive or full */
  PetscInt  cyclesperpcapply;                 /* Number of cycles to use in each PCApply(), multiplicative only*/
  PetscInt  maxlevels;                        /* total number of levels allocated */
  PetscInt  galerkin;                         /* use Galerkin process to compute coarser matrices, 0=no, 1=yes, 2=yes but computed externally */
  PetscBool usedmfornumberoflevels;           /* sets the number of levels by getting this information out of the DM */

  PetscInt     nlevels;
  PC_MG_Levels **levels;
  PetscInt     default_smoothu;               /* number of smooths per level if not over-ridden */
  PetscInt     default_smoothd;               /*  with calls to KSPSetTolerances() */
  PetscReal    rtol,abstol,dtol,ttol;         /* tolerances for when running with PCApplyRichardson_MG */

  void          *innerctx;                   /* optional data for preconditioner, like PCEXOTIC that inherits off of PCMG */
  PetscLogStage stageApply;
} PC_MG;
#endif

#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcmgsoftreset_ PCMGSOFTRESET
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcmgsoftreset_ pcmgsoftreset
#endif

#undef __FUNCT__
#define __FUNCT__ "PCMGSoftReset"

PetscErrorCode PCMGSoftReset(PC pc)
{
  PC_MG          *mg        = (PC_MG*)pc->data;
  PC_MG_Levels   **mglevels = mg->levels;
  PetscErrorCode ierr;
  PetscInt       i,n;

  PetscFunctionBegin;
  if (mglevels) {
    n = mglevels[0]->levels;
    for (i=0; i<n; i++) {
      ierr = MatDestroy(&mglevels[i]->A);CHKERRQ(ierr);
      if (mglevels[i]->smoothd != mglevels[i]->smoothu) {
        ierr = KSPReset(mglevels[i]->smoothd);CHKERRQ(ierr);
      }
      ierr = KSPReset(mglevels[i]->smoothu);CHKERRQ(ierr);
    }
  }
  //This may not work if DM's are used to define the MG structure.
  pc->setupcalled = PETSC_FALSE;
  PetscFunctionReturn(0);
}

PETSC_EXTERN void PETSC_STDCALL  pcmgsoftreset_(PC pc, int *__ierr ){
*__ierr = PCMGSoftReset(
  (PC)PetscToPointer((pc) ));
}
