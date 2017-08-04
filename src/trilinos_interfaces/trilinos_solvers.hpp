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
#include "Teuchos_RCP.hpp"

#ifdef HAVE_MPI
#include <mpi.h>
#endif

#include "AnasaziBasicEigenproblem.hpp"
#include "AnasaziTpetraAdapter.hpp"
#include "AnasaziGeneralizedDavidsonSolMgr.hpp"
#include "AnasaziTypes.hpp"
#include "BelosTpetraAdapter.hpp"
#include "BelosSolverFactory.hpp"
#include "BelosSolverManager.hpp"
#include "Teuchos_ParameterList.hpp"

class AnasaziCnt : public ForPETRA_SelectedTypes {
public:
    /*
    Notes:
      solver manager needs a problem
      problem needs matrices
      the two above statements mean we can't constrcut them until solve time
    (consistent with Steven)
    */

    // We're solving   LHS*x = k*RHS*x
    Teuchos::RCP<CrsMatrix> LHS;
    Teuchos::RCP<CrsMatrix> RHS;
    Teuchos::RCP<Operator> pc;
    bool haspc = false;
    double keff;
    int niters;
    int pc_id;
    Teuchos::RCP<Vector> x;
    Teuchos::ParameterList anasazi_db;
    // maybe some other things about a specific solver
};

class AnasaziStore {
    typedef AnasaziCnt::Vector Vector;
    typedef AnasaziCnt::CrsMatrix CrsMatrix;
    typedef TpetraMatCnt::Operator Operator;
    typedef Tpetra::MultiVector<> MultiVector;

public:
    AnasaziStore() : cid(0)
    {
    }

    int new_data(Teuchos::ParameterList &params)
    {
        // setup parameterlist with defaults. The get() method will add the
        // passed settings to the parameter list if they do not already exist,
        // otherwise there is no effect. As such, only options missing from the
        // passed parameter list will be set to default values.
        params.set("Which", std::string("LM"));
        params.get("Convergence Tolerance", 1e-7);
        params.get("Maximum Subspace Dimension", 25);
        params.get("Restart Dimension", 5);
        params.get("Maximum Restarts", 20);
        params.get("Initial Guess", std::string("User"));
        params.get("Verbosity", Anasazi::Errors + Anasazi::Warnings);
        // + Anasazi::FinalSummary
        // + Anasazi::TimingDetails
        // + Anasazi::IterationDetails);

        anasazi_map[cid]            = AnasaziCnt();
        anasazi_map[cid].anasazi_db = params;

        cid++;
        return cid - 1;
    }

    int delete_data(const int id)
    {
        anasazi_map.erase(id);
        return 0;
    }

    int setMat_data(const int id, Teuchos::RCP<CrsMatrix> LHS,
                    Teuchos::RCP<CrsMatrix> RHS)
    {
        anasazi_map[id].LHS = LHS;
        anasazi_map[id].RHS = RHS;
        return 0;
    }

    int setConvCrit_data(const int id, const double tol, const int maxit)
    {
        anasazi_map[id].anasazi_db.set("Convergence Tolerance", tol);
        anasazi_map[id].anasazi_db.set("Maximum Restarts", maxit);
        return 0;
    }

    int setX0_data(const int id, Teuchos::RCP<Vector> x0)
    {
        anasazi_map[id].x = x0;
        return 0;
    }

    int setPC_data(const int id, const int pc_id, Teuchos::RCP<Operator> pc)
    {
        // if(anasazi_map[id].LHS->Comm().MyPID()==0) std::cout << pc->Label()
        // << std::endl;
        anasazi_map[id].pc_id = pc_id;
        anasazi_map[id].pc    = pc;
        anasazi_map[id].haspc = true;
        return 0;
    }

    int solve(const int id)
    {
        // get a reference to the indexed Anasazi container
        auto &anasazi = anasazi_map[id];

        Teuchos::RCP<Anasazi::BasicEigenproblem<double, MultiVector, Operator>>
        problem(
            new Anasazi::BasicEigenproblem<double, MultiVector, Operator>());
        problem->setA(anasazi.LHS);
        problem->setM(anasazi.RHS);

        if (anasazi.haspc)
            problem->setPrec(anasazi.pc);
        problem->setInitVec(anasazi.x);
        problem->setNEV(1);
        bool problem_set = problem->setProblem();
        assert(problem_set);

        Anasazi::GeneralizedDavidsonSolMgr<double, MultiVector, Operator>
            solver(problem, anasazi.anasazi_db);

        Anasazi::ReturnType returnval = solver.solve();

        if (returnval == Anasazi::Converged) {
            anasazi.niters = solver.getNumIters();

            // Extract solution
            Anasazi::Eigensolution<double, MultiVector> solution =
                solver.getProblem().getSolution();
            auto eval    = (solution.Evals)[0];
            anasazi.keff = eval.realpart;
            auto evec    = solution.Evecs->getVector(0);
            double val   = evec->meanValue();
            anasazi.x->update(1.0 / val, *(solution.Evecs), 0.0);
            return 0;
        } else {
            // If Anasazi doesn't return, approximate k as x^T Fx/x^M Fx
            anasazi.niters = -1;
            // Another place where a copy is overkill. should make new vector
            // with same map.
            Vector tmp = Vector(*anasazi.x, Teuchos::Copy);
            anasazi.RHS->apply(*anasazi.x, tmp);
            double rhs = tmp.dot(*anasazi.x);
            anasazi.LHS->apply(*(anasazi.x), tmp);
            double lhs   = tmp.dot(*anasazi.x);
            anasazi.keff = lhs / rhs;
            return 1;
        }
    }

    int getEigenvalue_data(const int id, double &keff)
    {
        keff = anasazi_map[id].keff;
        return 0;
    }

    int getIterations_data(const int id, int &niters)
    {
        niters = anasazi_map[id].niters;
        return 0;
    }

    int getResidual(const int id, double &resid)
    {
        const auto &solver = anasazi_map[id];

        // This is a little wasteful, we dont actually use the copied data;
        // could just use the map to make brand new temp vectors
        Vector ltmp(*solver.x, Teuchos::Copy);
        Vector rtmp(*solver.x, Teuchos::Copy);

        solver.LHS->apply(*solver.x, ltmp);
        solver.RHS->apply(*solver.x, rtmp);

        double denom = solver.x->norm2();
        rtmp.update(-1., ltmp, solver.keff);
        double resids = rtmp.norm2();
        resid         = resids / denom;
        return 0;
    }

    int getPCid_data(const int id, int &pcid, Teuchos::RCP<CrsMatrix> &M)
    {
        pcid = anasazi_map[id].pc_id;
        M    = anasazi_map[id].LHS;
        return 0;
    }

private:
    int cid;
    std::map<int, AnasaziCnt> anasazi_map;
};

class BelosCnt : public ForPETRA_SelectedTypes {
public:
    /*
    Notes:
      solver manager needs a problem
      problem needs matrices
      the two above statements mean we can't constrcut them until solve time
    (consistent with Steven)
    */
    Teuchos::RCP<CrsMatrix> A;
    Teuchos::RCP<Operator> pc;
    bool haspc = false;
    int num_iters;
    Teuchos::RCP<Vector> x;
    Teuchos::RCP<Vector> b;
    Teuchos::ParameterList belos_db;
    // maybe some other things about a specific solver

    ~BelosCnt()
    {
    }
};

class BelosStore : public ForPETRA_SelectedTypes {
public:
    BelosStore() : cid(0)
    {
    }

    int new_data(Teuchos::ParameterList &params)
    {
        // Teuchos::ParameterList db
        belos_map[cid] = BelosCnt();
        // setup parameterlist with defaults

        // belos_map[cid].anasazi_db = Teuchos::sublist(db, "Anasazi");
        // belos_map[cid].belos_db.get("belos_type","Pseudo Block GMRES");
        params.get("Convergence Tolerance", 1e-6);
        params.get("Maximum Iterations", 250);
        params.get("Verbosity", 0);
        // Belos::Warnings+Belos::FinalSummary+Belos::StatusTestDetails
        params.get("Output Frequency", 1);
        params.get("Implicit Residual Scaling", "Norm of RHS");
        params.get("Explicit Residual Scaling", "Norm of RHS");
        belos_map[cid].belos_db = params;

        cid++;
        return cid - 1;
    }

    int delete_data(const int id)
    {
        belos_map.erase(id);
        return 0;
    }

    int setMat_data(const int id, Teuchos::RCP<CrsMatrix> A)
    {
        belos_map[id].A = A;
        return 0;
    }

    int setConvCrit_data(const int id, const double tol, const int maxit)
    {
        belos_map[id].belos_db.set("Convergence Tolerance", tol);
        belos_map[id].belos_db.set("Maximum Iterations", maxit);
        return 0;
    }

    int setX0_data(const int id, Teuchos::RCP<Vector> x0)
    {
        belos_map[id].x = x0;
        return 0;
    }

    int setb_data(const int id, Teuchos::RCP<Vector> b)
    {
        belos_map[id].b = b;
        return 0;
    }

    int setPC_data(const int id, Teuchos::RCP<Operator> pc)
    {
        belos_map[id].pc    = pc;
        belos_map[id].haspc = true;
        return 0;
    }

    int solve(const int id)
    {
        Belos::SolverFactory<SC, MultiVector, Operator> factory;

        std::string type = "Pseudo Block GMRES";
        Teuchos::RCP<Belos::SolverManager<SC, MultiVector, Operator>>
            solver = factory.create(
                type, Teuchos::rcpFromRef(belos_map[id].belos_db));

        // Create linear problem
        Teuchos::RCP<Belos::LinearProblem<SC, MultiVector, Operator>>
        problem(new Belos::LinearProblem<SC, MultiVector, Operator>());
        problem->setOperator(belos_map[id].A);
        problem->setLHS(belos_map[id].x);
        problem->setRHS(belos_map[id].b);
        if (belos_map[id].haspc)
            problem->setRightPrec(belos_map[id].pc);
        problem->setProblem();

        solver->setParameters(Teuchos::rcpFromRef(belos_map[id].belos_db));
        solver->setProblem(problem);

        Belos::ReturnType result = solver->solve();
        belos_map[id].num_iters  = solver->getNumIters();

        return 0;
    }

    int getIterations_data(const int id, int &niter)
    {
        niter = belos_map[id].num_iters;
        return 0;
    }

    int getResidual(const int id, double &resid)
    {
        Teuchos::RCP<Vector> rtmp(new Vector(*belos_map[id].x, Teuchos::Copy));
        belos_map[id].A->apply(*(belos_map[id].x), *rtmp);
        rtmp->update(-1.0, *(belos_map[id].b), 1.0);
        resid = rtmp->norm2();
        return 0;
    }

private:
    int cid;
    std::map<int, BelosCnt> belos_map;
};
