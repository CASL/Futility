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
    Teuchos::RCP<Epetra_CrsMatrix> LHS;
    Teuchos::RCP<Epetra_CrsMatrix> RHS;
    double keff;
    Teuchos::RCP<Epetra_Vector> x;
    Teuchos::ParameterList anasazi_db;
    //maybe some other things about a specific solver
};

class AnasaziStore {
public:
    AnasaziStore():
        cid(0)
    {
        if(verbose) std::cout << "Constructing new Anasazi store" << std::endl;
        return;
    }

    int new_data() {
        //Teuchos::ParameterList db
        things_[cid]=AnasaziCnt();
        //setup parameterlist with defaults
        //things_[cid].anasazi_db = Teuchos::sublist(db, "Anasazi");
        things_[cid].anasazi_db.set("Which", std::string("LM"));
        things_[cid].anasazi_db.get("Convergence Tolerance",1e-6);
        things_[cid].anasazi_db.get("Maximum Subspace Dimension",10);
        things_[cid].anasazi_db.get("Restart Dimension",5);
        things_[cid].anasazi_db.get("Maximum Restarts",100);
        things_[cid].anasazi_db.get("Initial Guess",std::string("User"));
        things_[cid].anasazi_db.get("Verbosity",Anasazi::Errors + Anasazi::Warnings + Anasazi::FinalSummary + Anasazi::TimingDetails);

        cid++;
        return cid-1;
    }

    int setMat_data(const int id, Teuchos::RCP<Epetra_CrsMatrix> LHS, Teuchos::RCP<Epetra_CrsMatrix> RHS) {
        things_[id].LHS=LHS;
        things_[id].RHS=RHS;
        return 0;
    }

    int setConvCrit_data(const int id, const double tol, const int maxit) {
        things_[id].anasazi_db.set("Convergence Tolerance", tol);
        // check if this is what I think it is...
        //things_[id].anasazi_db->set("Maximum Restarts", maxit);
        return 0;
    }

    int setX0_data(const int id, Teuchos::RCP<Epetra_Vector> x0) {
        things_[id].x=x0;
        return 0;
    }

    int solve(const int id) {
        Teuchos::RCP<Anasazi::BasicEigenproblem<double,Epetra_MultiVector,Epetra_Operator>> problem(
            new Anasazi::BasicEigenproblem<double,Epetra_MultiVector,Epetra_Operator>());
        problem->setA(things_[id].RHS);
        problem->setM(things_[id].LHS);
        //problem->setPrec(things_[id].prec);
        problem->setInitVec(things_[id].x);
        problem->setNEV(1);
        bool problem_set = problem->setProblem();
        assert(problem_set);
        Anasazi::GeneralizedDavidsonSolMgr<double,Epetra_MultiVector,Epetra_Operator> solver(
            problem, things_[id].anasazi_db);

        EpetraExt::RowMatrixToMatlabFile("myRHS.m",*(things_[id].RHS));
        EpetraExt::RowMatrixToMatlabFile("myLHS.m",*(things_[id].LHS));
        EpetraExt::VectorToMatlabFile("myX.m",*(things_[id].x));
        Anasazi::ReturnType returnval = solver.solve();
        std::cout << "solved" << std::endl;
        // Extract solution
        Anasazi::Eigensolution<double,Epetra_MultiVector> solution =
            solver.getProblem().getSolution();
        Anasazi::Value<double> eval = (solution.Evals)[0];
        things_[id].keff = eval.realpart;
        //things_[id].x=Teuchos::rcp((*solution.Evecs)(0));

        std::cout << things_[id].keff << std::endl;

        return 0;
    }

    int getResidual(const int id,double &resid) {
        return 0;
    }

private:
        int cid;
        map<int, AnasaziCnt> things_;
};
