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

using std::map;

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

    int new_data() {
        //Teuchos::ParameterList db
        anasazi_map[cid]=AnasaziCnt();
        //setup parameterlist with defaults
        //anasazi_map[cid].anasazi_db = Teuchos::sublist(db, "Anasazi");
        anasazi_map[cid].anasazi_db.set("Which", std::string("LM"));
        anasazi_map[cid].anasazi_db.get("Convergence Tolerance",1e-7);
        anasazi_map[cid].anasazi_db.get("Maximum Subspace Dimension",25);
        anasazi_map[cid].anasazi_db.get("Restart Dimension",5);
        anasazi_map[cid].anasazi_db.get("Maximum Restarts",20);
        anasazi_map[cid].anasazi_db.get("Initial Guess",std::string("User"));
        anasazi_map[cid].anasazi_db.get("Verbosity",Anasazi::Errors + Anasazi::Warnings);  // + Anasazi::FinalSummary + Anasazi::TimingDetails + Anasazi::IterationDetails);

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
        Teuchos::RCP<Anasazi::BasicEigenproblem<double,Epetra_MultiVector,Epetra_Operator>> problem(
            new Anasazi::BasicEigenproblem<double,Epetra_MultiVector,Epetra_Operator>());
        problem->setA(anasazi_map[id].LHS);
        problem->setM(anasazi_map[id].RHS);
        if(anasazi_map[id].haspc) problem->setPrec(anasazi_map[id].pc);
        problem->setInitVec(anasazi_map[id].x);
        problem->setNEV(1);
        bool problem_set = problem->setProblem();
        assert(problem_set);

        Anasazi::GeneralizedDavidsonSolMgr<double,Epetra_MultiVector,Epetra_Operator> solver(
            problem, anasazi_map[id].anasazi_db);

        Anasazi::ReturnType returnval = solver.solve();

        if(returnval==Anasazi::Converged){
            anasazi_map[id].niters=solver.getNumIters();

            // Extract solution
            Anasazi::Eigensolution<double,Epetra_MultiVector> solution =
                solver.getProblem().getSolution();
            Anasazi::Value<double> eval = (solution.Evals)[0];
            anasazi_map[id].keff = eval.realpart;
            double val[0];
            solution.Evecs->MeanValue(val);
            anasazi_map[id].x->Update(1.0/val[0],*(solution.Evecs),0.0);
            return 0;
        } else{ return 1;}
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
        map<int, AnasaziCnt> anasazi_map;
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

    int new_data() {
        //Teuchos::ParameterList db
        belos_map[cid]=BelosCnt();
        //setup parameterlist with defaults

        //belos_map[cid].anasazi_db = Teuchos::sublist(db, "Anasazi");
        //belos_map[cid].belos_db.get("belos_type","Pseudo Block GMRES");
        belos_map[cid].belos_db.get("Convergence Tolerance",1e-6);
        belos_map[cid].belos_db.get("Maximum Iterations",250);
        belos_map[cid].belos_db.get("Verbosity",0);  //Belos::Warnings+Belos::FinalSummary+Belos::StatusTestDetails
        belos_map[cid].belos_db.get("Output Frequency",1);
        belos_map[cid].belos_db.get("Implicit Residual Scaling","Norm of RHS");
        belos_map[cid].belos_db.get("Explicit Residual Scaling","Norm of RHS");

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
        map<int, BelosCnt> belos_map;
};
