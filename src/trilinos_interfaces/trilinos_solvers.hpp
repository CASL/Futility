/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
/                          Futility Development Group                          !
/                             All rights reserved.                             !
/                                                                              !
/ Futility is a jointly-maintained, open-source project between the University !
/ of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
/ can be found in LICENSE.txt in the head directory of this repository.        !
/+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/
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
#include "Teuchos_ParameterList.hpp"
#include "AnasaziTypes.hpp"
#include "AnasaziBasicEigenproblem.hpp"
#include "AnasaziGeneralizedDavidsonSolMgr.hpp"
#include "AnasaziEpetraAdapter.hpp"
#include "BelosSolverFactory.hpp"
#include "BelosSolverManager.hpp"
#include "BelosEpetraAdapter.hpp"
#include "Epetra_MultiVector.h"
#include "Epetra_Operator.h"
#include <cassert>

class AnasaziCnt{
public:
    /*
    Notes:
      solver manager needs a problem
      problem needs matrices
      the two above statements mean we can't constrcut them until solve time (consistent with Steven)
    */

    //We're solving   LHS*x = k*RHS*x
    Teuchos::RCP<Epetra_CrsMatrix> LHS;
    Teuchos::RCP<Epetra_CrsMatrix> RHS;
    Teuchos::RCP<Epetra_Operator>  pc;
    bool haspc=false;
    double keff;
    int niters;
    int pc_id;
    Teuchos::RCP<Epetra_Vector> x;
    Teuchos::ParameterList anasazi_db;
    //maybe some other things about a specific solver
};

class AnasaziStore {
public:
    AnasaziStore():
        cid(0)
    {}

    int new_data(Teuchos::ParameterList &params) {
        //setup parameterlist with defaults. The get() method will add the
        //passed settings to the parameter list if they do not already exist,
        //otherwise there is no effect. As such, only options missing from the
        //passed parameter list will be set to default values.
        params.set("Which", std::string("LM"));
        params.get("Convergence Tolerance",1e-7);
        params.get("Maximum Subspace Dimension",25);
        params.get("Restart Dimension",5);
        params.get("Maximum Restarts",20);
        params.get("Initial Guess",std::string("User"));
        params.get("Verbosity",Anasazi::Errors + Anasazi::Warnings);
        // + Anasazi::FinalSummary
        // + Anasazi::TimingDetails
        // + Anasazi::IterationDetails);

        anasazi_map[cid] = AnasaziCnt();
        anasazi_map[cid].anasazi_db = params;

        cid++;
        return cid-1;
    }

    int delete_data(const int id){
        anasazi_map.erase(id);
        return 0;
    }

    int setMat_data(const int id, Teuchos::RCP<Epetra_CrsMatrix> LHS, Teuchos::RCP<Epetra_CrsMatrix> RHS) {
        anasazi_map[id].LHS=LHS;
        anasazi_map[id].RHS=RHS;
        return 0;
    }

    int setConvCrit_data(const int id, const double tol, const int maxit) {
        anasazi_map[id].anasazi_db.set("Convergence Tolerance", tol);
        anasazi_map[id].anasazi_db.set("Maximum Restarts", maxit);
        return 0;
    }

    int setX0_data(const int id, Teuchos::RCP<Epetra_Vector> x0) {
        anasazi_map[id].x=x0;
        return 0;
    }

    int setPC_data(const int id, const int pc_id, Teuchos::RCP<Epetra_Operator> pc) {
        //if(anasazi_map[id].LHS->Comm().MyPID()==0) std::cout << pc->Label() << std::endl;
        anasazi_map[id].pc_id=pc_id;
        anasazi_map[id].pc=pc;
        anasazi_map[id].haspc=true;
        return 0;
    }

    int solve(const int id) {
        // get a reference to the indexed Anasazi container
        auto &anasazi = anasazi_map[id];

        Teuchos::RCP<Anasazi::BasicEigenproblem<double,Epetra_MultiVector,Epetra_Operator>> problem(
            new Anasazi::BasicEigenproblem<double,Epetra_MultiVector,Epetra_Operator>());
        problem->setA(anasazi.LHS);
        problem->setM(anasazi.RHS);

        if(anasazi.haspc) problem->setPrec(anasazi.pc);
        problem->setInitVec(anasazi.x);
        problem->setNEV(1);
        bool problem_set = problem->setProblem();
        assert(problem_set);

        Anasazi::GeneralizedDavidsonSolMgr<double,Epetra_MultiVector,Epetra_Operator> solver(
            problem, anasazi.anasazi_db);

        Anasazi::ReturnType returnval = solver.solve();

        if(returnval==Anasazi::Converged){
            anasazi.niters=solver.getNumIters();

            // Extract solution
            Anasazi::Eigensolution<double,Epetra_MultiVector> solution =
                solver.getProblem().getSolution();
            Anasazi::Value<double> eval = (solution.Evals)[0];
            anasazi.keff = eval.realpart;
            double val;
            solution.Evecs->MeanValue(&val);
            anasazi.x->Update(1.0/val,*(solution.Evecs),0.0);
            return 0;
        }
        else{
            //If Anasazi doesn't return, approximate k as x^T Fx/x^M Fx
            anasazi.niters=-1;
            double rhs=1.0;
            double lhs=1.0;
            Epetra_Vector tmp=Epetra_Vector(*(anasazi.x));
            anasazi.RHS->Multiply(false,*(anasazi.x),tmp);
            tmp.Dot(*(anasazi.x),&rhs);
            anasazi.LHS->Multiply(false,*(anasazi.x),tmp);
            tmp.Dot(*(anasazi.x),&lhs);
            //LHS*phi = k * RHS * phi
            anasazi.keff=lhs/rhs;
            return 1;
        }
    }

    int getEigenvalue_data(const int id,double &keff) {
        keff=anasazi_map[id].keff;
        return 0;
    }

    int getIterations_data(const int id,int &niters) {
        niters=anasazi_map[id].niters;
        return 0;
    }

    int getResidual(const int id,double &resid) {
        Teuchos::RCP<Epetra_Vector> ltmp(new Epetra_Vector(*anasazi_map[id].x));
        Teuchos::RCP<Epetra_Vector> rtmp(new Epetra_Vector(*anasazi_map[id].x));
        anasazi_map[id].LHS->Multiply(false,*(anasazi_map[id].x),*ltmp);
        anasazi_map[id].RHS->Multiply(false,*(anasazi_map[id].x),*rtmp);
        double denom[1];
        (anasazi_map[id].x)->Norm2(denom);
        rtmp->Update(-1.,*(ltmp),anasazi_map[id].keff);
        double resids[1];
        rtmp->Norm2(resids);
        resid=resids[0]/denom[0];
        return 0;
    }

    int getPCid_data(const int id,int &pcid, Teuchos::RCP<Epetra_CrsMatrix> &M) {
        pcid=anasazi_map[id].pc_id;
        M=anasazi_map[id].LHS;
        return 0;
    }

private:
        int cid;
        std::map<int, AnasaziCnt> anasazi_map;
};

class BelosCnt{
public:
    /*
    Notes:
      solver manager needs a problem
      problem needs matrices
      the two above statements mean we can't constrcut them until solve time (consistent with Steven)
    */
    Teuchos::RCP<Epetra_CrsMatrix> A;
    Teuchos::RCP<Epetra_Operator>  pc;
    bool haspc=false;
    int num_iters;
    Teuchos::RCP<Epetra_Vector> x;
    Teuchos::RCP<Epetra_Vector> b;
    Teuchos::ParameterList belos_db;
    //maybe some other things about a specific solver


    ~BelosCnt(){
        A  = Teuchos::null;
        pc = Teuchos::null;
        x  = Teuchos::null;
        b  = Teuchos::null;
    }
};

class BelosStore {
public:
    BelosStore():
        cid(0)
    {}

    int new_data(Teuchos::ParameterList &params) {
        //Teuchos::ParameterList db
        belos_map[cid]=BelosCnt();
        //setup parameterlist with defaults

        //belos_map[cid].anasazi_db = Teuchos::sublist(db, "Anasazi");
        //belos_map[cid].belos_db.get("belos_type","Pseudo Block GMRES");
        params.get("Convergence Tolerance",1e-6);
        params.get("Maximum Iterations",250);
        params.get("Verbosity",0);  //Belos::Warnings+Belos::FinalSummary+Belos::StatusTestDetails
        params.get("Output Frequency",1);
        params.get("Implicit Residual Scaling","Norm of RHS");
        params.get("Explicit Residual Scaling","Norm of RHS");
        belos_map[cid].belos_db = params;

        cid++;
        return cid-1;
    }

    int delete_data(const int id) {
        belos_map.erase(id);
        return 0;
    }

    int setMat_data(const int id, Teuchos::RCP<Epetra_CrsMatrix> A) {
        belos_map[id].A=A;
        return 0;
    }

    int setConvCrit_data(const int id, const double tol, const int maxit) {
        belos_map[id].belos_db.set("Convergence Tolerance", tol);
        belos_map[id].belos_db.set("Maximum Iterations", maxit);
        return 0;
    }

    int setX0_data(const int id, Teuchos::RCP<Epetra_Vector> x0) {
        belos_map[id].x=x0;
        return 0;
    }

    int setb_data(const int id, Teuchos::RCP<Epetra_Vector> b) {
        belos_map[id].b=b;
        return 0;
    }

    int setPC_data(const int id, Teuchos::RCP<Epetra_Operator> pc) {
        belos_map[id].pc=pc;
        belos_map[id].haspc=true;
        return 0;
    }

    int solve(const int id) {
        Belos::SolverFactory<double,Epetra_MultiVector,Epetra_Operator> factory;

        std::string type ="Pseudo Block GMRES";
        Teuchos::RCP<Belos::SolverManager<double,Epetra_MultiVector,Epetra_Operator> > solver =
            factory.create(type,Teuchos::rcpFromRef(belos_map[id].belos_db));

        // Create linear problem
        Teuchos::RCP<Belos::LinearProblem<double,Epetra_MultiVector,Epetra_Operator> > problem(
                new Belos::LinearProblem<double,Epetra_MultiVector,Epetra_Operator>() );
        problem->setOperator(belos_map[id].A);
        problem->setLHS(belos_map[id].x);
        problem->setRHS(belos_map[id].b);
        if(belos_map[id].haspc) problem->setRightPrec(belos_map[id].pc);
        problem->setProblem();

        solver->setParameters(Teuchos::rcpFromRef(belos_map[id].belos_db));
        solver->setProblem(problem);

        Belos::ReturnType result = solver->solve();
        belos_map[id].num_iters = solver->getNumIters();

        return 0;
    }

    int getIterations_data(const int id,int &niter) {
        niter=belos_map[id].num_iters;
        return 0;
    }

    int getResidual(const int id,double &resid) {
        Teuchos::RCP<Epetra_Vector> rtmp(new Epetra_Vector(*belos_map[id].x));
        belos_map[id].A->Multiply(false,*(belos_map[id].x),*rtmp);
        rtmp->Update(-1.,*(belos_map[id].b),1.);
        double resids[1];
        rtmp->Norm2(resids);
        resid=resids[0];
        return 0;
    }

private:
        int cid;
        std::map<int, BelosCnt> belos_map;
};
