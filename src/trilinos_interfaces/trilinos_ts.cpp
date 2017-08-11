/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
/                          Futility Development Group                          !
/                             All rights reserved.                             !
/                                                                              !
/ Futility is a jointly-maintained, open-source project between the University !
/ of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
/ can be found in LICENSE.txt in the head directory of this repository.        !
/+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/
#include "trilinos_ts.hpp"

#include <Thyra_DefaultSpmdVectorSpace.hpp>
#include <Rythmos_TimeStepNonlinearSolver.hpp>
#include <Rythmos_ImplicitBDFStepper.hpp>

using Teuchos::RCP;
using Teuchos::rcp;
using Teuchos::ParameterList;

////////////////////////////////////////////////////////////////////////////////
// TSModelEvaluator
////////////////////////////////////////////////////////////////////////////////
TSModelEvaluator::TSModelEvaluator(int n) : n_(n)
{

    x_space_ = Thyra::defaultSpmdVectorSpace<Scalar>(n_);
    f_space_ = Thyra::defaultSpmdVectorSpace<Scalar>(n_);

    {
        Thyra::ModelEvaluatorBase::InArgsSetup<Scalar> inArgs;

        inArgs.setSupports(ModelEvaluatorBase::IN_ARG_t);
        inArgs.setSupports(ModelEvaluatorBase::IN_ARG_x);

        inArgs_ = inArgs;
    }

    {
        Thyra::ModelEvaluatorBase::OutArgsSetup<Scalar> outArgs;

        // how to get derivative out? What is W?
        outArgs.setSupports(ModelEvaluatorBase::OUT_ARG_f);
        outArgs.setSupports(ModelEvaluatorBase::OUT_ARG_W);

        outArgs_ = outArgs;
    }

    return;
}

auto TSModelEvaluator::createInArgs() const -> InArgs
{
    return inArgs_;
}

auto TSModelEvaluator::createOutArgsImpl() const -> OutArgs
{
    return outArgs_;
}

void TSModelEvaluator::evalModelImpl(const InArgs &inArgs,
                                     const OutArgs &outArgs) const
{
    const Scalar t = inArgs.get_t();
    return;
}

////////////////////////////////////////////////////////////////////////////////
// TSCnt
////////////////////////////////////////////////////////////////////////////////
TSCnt::TSCnt(FunctionPointer fptr, int n, double tol,
             Teuchos::ParameterList &params)
    : fptr(fptr), n(n), tol(tol)
{
    params.remove("ts_option");

    // setup parameterlist with defaults
    // eventually read this from somewhere
    ts_db = params;

    model_ = rcp(new TSModelEvaluator(n));

    // Setup non-linear solver for the stepper
    // Still need to configure parameters
    RCP<ParameterList> solverParams = Teuchos::parameterList();
    solver_ = rcp(new Rythmos::TimeStepNonlinearSolver<Scalar>());
    solver_->setParameterList(solverParams);

    // Setup the stepper
    // Still need to configure parameters
    RCP<ParameterList> BDFParams = Teuchos::parameterList();
    stepper_                     = rcp(
        new Rythmos::ImplicitBDFStepper<Scalar>(model_, solver_, BDFParams));

    return;
}

int TSCnt::step(double tstart, double tend, const double *xstart, double *xend)
{
    return 0;
}
