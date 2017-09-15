/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
/                          Futility Development Group                          !
/                             All rights reserved.                             !
/                                                                              !
/ Futility is a jointly-maintained, open-source project between the University !
/ of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
/ can be found in LICENSE.txt in the head directory of this repository.        !
/+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/
#pragma once

#include <cassert>
#include <iostream>
#include <map>

#include <Teuchos_RCP.hpp>

#include <Thyra_ModelEvaluator.hpp>
#include <Thyra_StateFuncModelEvaluatorBase.hpp>
#include <Thyra_NonlinearSolverBase.hpp>

#include <Rythmos_IntegratorBase.hpp>
#include <Rythmos_StepperBase.hpp>

#include "trilinos_mat_vec.hpp"

class TSModelEvaluator : public Thyra::StateFuncModelEvaluatorBase<double> {
public:
    typedef double Scalar;
    typedef Thyra::ModelEvaluatorBase::InArgs<Scalar> InArgs;
    typedef Thyra::ModelEvaluatorBase::OutArgs<Scalar> OutArgs;

    TSModelEvaluator(int n);

    // Implement pure virtuals left over from ModelEvaluator
    Teuchos::RCP<const Thyra::VectorSpaceBase<Scalar>>
    get_x_space() const override
    {
        return x_space_;
    }

    Teuchos::RCP<const Thyra::VectorSpaceBase<Scalar>>
    get_f_space() const override
    {
        return f_space_;
    }

    InArgs createInArgs() const override;

    OutArgs createOutArgsImpl() const override;

    void evalModelImpl(const InArgs &inArgs,
                       const OutArgs &outArgs) const override;

private:
    // Number of coupled equations
    const int n_;
    Teuchos::RCP<const Thyra::VectorSpaceBase<Scalar>> x_space_;
    Teuchos::RCP<const Thyra::VectorSpaceBase<Scalar>> f_space_;

    InArgs inArgs_;
    OutArgs outArgs_;
};

class TSCnt {
public:
    typedef TSModelEvaluator::Scalar Scalar;
    typedef void (*FunctionPointer)();

    TSCnt()
    {
    }

    TSCnt(FunctionPointer fptr, int n, double tol,
          Teuchos::ParameterList &params);

    int step(double tstart, double tend, const double *xtart, double *xend);

    // Data
private:
    FunctionPointer fptr = nullptr;
    int n;
    double tol;
    Teuchos::ParameterList ts_db;
    // To drive Rythmos, we need to create:
    //  - a ModelEvaluator (which should just wrap the call to the Fortran model
    //  and marshal the results appropriately)
    //  - a time Stepper, which knows how to use the ModelEvaluator to perform a
    //  single time step at a time. This is where we can select a specific
    //  integration method, such as BDF, RK, or various Euler methods.
    //  - a TimeIntegrator, which uses the ModelEvaluator and the time Stepper
    //  to advance the simulation forward to a requested time point
    //  - a non-linear solver for solving each time step taken by the Stepper

    Teuchos::RCP<Thyra::ModelEvaluator<Scalar>> model_;
    Teuchos::RCP<Thyra::NonlinearSolverBase<Scalar>> solver_;
    Teuchos::RCP<Rythmos::StepperBase<Scalar>> stepper_;
    Teuchos::RCP<Rythmos::IntegratorBase<Scalar>> integrator_;
};

class TSStore : ForPETRA_SelectedTypes {
public:
    typedef TSCnt::FunctionPointer FunctionPointer;
    TSStore() : cid(0)
    {
    }

    int new_data(FunctionPointer fptr, int n, double tol,
                 Teuchos::ParameterList &params)
    {
        ts_map[cid] = TSCnt(fptr, n, tol, params);
        cid++;
        return cid - 1;
    }

    int delete_data(const int id)
    {
        ts_map.erase(id);
    }

    const TSCnt &operator[](int id) const
    {
        return ts_map.at(id);
    }

    TSCnt &operator[](int id)
    {
        return ts_map.at(id);
    }

private:
    int cid;
    std::map<int, TSCnt> ts_map;
};
