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
#include "NOX.H"
#include "NOX_Epetra.H"
#include "NOX_Epetra_Interface_Required.H" // base class
#include "Epetra_LinearProblem.h"

#include "Epetra_MultiVector.h"
#include "Epetra_Operator.h"
#include <cassert>

using std::map;

class AndersonCnt{
public:
    /*
    Notes:
      solver manager needs a problem
      problem needs matrices
      the two above statements mean we can't constrcut them until solve time (consistent with Steven)
    */

    //
    // need solver
    // other things the solver needs
    Teuchos::RCP<Teuchos::ParameterList> anderson_db;
    Teuchos::RCP<NOX::Solver::Generic> solver;
    Teuchos::RCP<NOX::Epetra::Vector> soln;
    //maybe some other things about a specific solver
};

class ModelEvaluator : public NOX::Epetra::Interface::Required {
public:
ModelEvaluator(){}

// Destructor
~ModelEvaluator(){}

bool computeF(const Epetra_Vector& x,
              Epetra_Vector& f,
              NOX::Epetra::Interface::Required::FillType)
    {
      // Residual should be G(x) - x, but G(x) not yet evaluated, so
      // just set f to x for now. Actual residual will be computed elsewhere
      f = x;
      return true;
    }
};

class AndersonStore {
public:
    AndersonStore():
        cid(0)
    {}

    int new_data(const int depth, const double beta, const int start, Teuchos::RCP<Epetra_Vector> soln) {
        anderson_map[cid]=AndersonCnt();

        //setup parameterlist with defaults
        anderson_map[cid].anderson_db = Teuchos::parameterList();
        anderson_map[cid].anderson_db->set("Nonlinear Solver", "Anderson Accelerated Fixed-Point");
        if(depth==0){
            anderson_map[cid].anderson_db->sublist("Anderson Parameters").set("Storage Depth", 1);
            anderson_map[cid].anderson_db->sublist("Anderson Parameters").set("Acceleration Start Iteration", 10000);
        }
        else{
            anderson_map[cid].anderson_db->sublist("Anderson Parameters").set("Storage Depth", depth);
            anderson_map[cid].anderson_db->sublist("Anderson Parameters").set("Acceleration Start Iteration", start);
        }
        anderson_map[cid].anderson_db->sublist("Anderson Parameters").set("Mixing Parameter", beta);
        Teuchos::ParameterList& printParams = anderson_map[cid].anderson_db->sublist("Printing");

        printParams.set("MyPID", soln->Comm().MyPID());
        printParams.set("Output Precision", 3);
        printParams.set("Output Processor", 0);
        printParams.set("Output Information",
//            NOX::Utils::OuterIteration +
//            NOX::Utils::OuterIterationStatusTest +
//            NOX::Utils::Parameters +
//            NOX::Utils::Details +
            NOX::Utils::Warning +
            NOX::Utils::Debug +
            NOX::Utils::Error);

        // setup model evaluator
        Teuchos::RCP<ModelEvaluator> modelEvaluator = Teuchos::rcp(new ModelEvaluator());

        // Get solution cast into NOX vector
        anderson_map[cid].soln=Teuchos::RCP<NOX::Epetra::Vector>(new NOX::Epetra::Vector(soln, NOX::Epetra::Vector::CreateView));

        // create fake linear solver object
        //   This should go away if we get that interface fixed
        Teuchos::RCP<Teuchos::ParameterList> lsParams = Teuchos::parameterList();
        Teuchos::RCP<NOX::Epetra::LinearSystemAztecOO> linSys =
          Teuchos::rcp(new NOX::Epetra::LinearSystemAztecOO(printParams,*lsParams,modelEvaluator,*(anderson_map[cid].soln)));

        // create NOX group
        Teuchos::RCP<NOX::Epetra::Group> noxGroup =
          Teuchos::rcp(new NOX::Epetra::Group(printParams, modelEvaluator, *(anderson_map[cid].soln), linSys));

        // compute initial (fake) residual
        noxGroup->computeF();

        // create convergence tests
        Teuchos::RCP<NOX::StatusTest::NormUpdate> update =
          Teuchos::rcp(new NOX::StatusTest::NormUpdate(1.0e-8));
        Teuchos::RCP<NOX::StatusTest::MaxIters> maxiters =
          Teuchos::rcp(new NOX::StatusTest::MaxIters(100000));
        Teuchos::RCP<NOX::StatusTest::FiniteValue> fv =
          Teuchos::rcp(new NOX::StatusTest::FiniteValue);
        Teuchos::RCP<NOX::StatusTest::Combo> combo =
          Teuchos::rcp(new NOX::StatusTest::Combo(NOX::StatusTest::Combo::OR));
        combo->addStatusTest(update);
        combo->addStatusTest(maxiters);
        combo->addStatusTest(fv);

        // create solver
        anderson_map[cid].solver = NOX::Solver::buildSolver(noxGroup, combo, anderson_map[cid].anderson_db);

        cid++;
        return cid-1;
    }

    int delete_data(const int id){
        anderson_map.erase(id);
        return 0;
    }

    int step(const int id) {

        // Compute the correct residual vector in the NOX group
        const NOX::Abstract::Group& noxGroup = anderson_map[id].solver->getSolutionGroup();
        NOX::Abstract::Vector& resid = const_cast<NOX::Abstract::Vector&>(noxGroup.getF());
        resid.update(1.0, *(anderson_map[id].soln), -1.0);

        // Compute the next iterate with the NOX solver
        anderson_map[id].solver->step();

        // Overwrite soln with the new NOX iterate
        *(anderson_map[id].soln) = noxGroup.getX();

        return 0;
    }

    int reset_data(const int id) {
        anderson_map[id].solver->reset(*(anderson_map[id].soln));
        NOX::Abstract::Group& noxGroup = const_cast<NOX::Abstract::Group&>(anderson_map[id].solver->getSolutionGroup());

        // compute initial (fake) residual
        noxGroup.computeF();
        return 0;
    }

private:
        int cid;
        map<int, AndersonCnt> anderson_map;
};

// JFNK Interface
class JFNKCnt{
public:
    /*
    Notes:
      solver manager needs a problem
      problem needs matrices
      the two above statements mean we can't constrcut them until solve time (consistent with Steven)
    */

    //
    // need solver
    // other things the solver needs
    Teuchos::RCP<Teuchos::ParameterList> jfnk_db;
    Teuchos::RCP<NOX::Solver::Generic> solver;
    Teuchos::RCP<NOX::Epetra::Vector> soln;
    //maybe some other things about a specific solver
};

class ModelEvaluator_JFNK : public NOX::Epetra::Interface::Required {
public:
ModelEvaluator_JFNK(void (*functionptr)(),Teuchos::RCP<Epetra_Vector> x, Teuchos::RCP<Epetra_Vector> F){
    fptr=functionptr;
    xloc=x;
    Floc=F;
}

// Destructor
~ModelEvaluator_JFNK(){}

bool computeF(const Epetra_Vector& x,
              Epetra_Vector& f,
              NOX::Epetra::Interface::Required::FillType)
    {
      // Residual calculation
      *xloc = x;
      fptr();
      f = *Floc;
      return true;
    }
private:
    void (*fptr)() = NULL;
    Teuchos::RCP<Epetra_Vector> xloc;
    Teuchos::RCP<Epetra_Vector> Floc;
};

class JFNKStore {
public:
    JFNKStore():
        cid(0)
    {}

    int new_data(void (*functionptr)(), Teuchos::RCP<Epetra_Vector> soln, Teuchos::RCP<Epetra_Vector> F) {
        jfnk_map[cid]=JFNKCnt();

        //setup parameterlist with defaults
        jfnk_map[cid].jfnk_db = Teuchos::parameterList();
        jfnk_map[cid].jfnk_db->set("Nonlinear Solver", "Line Search Based");
        //jfnk_map[cid].jfnk_db->sublist("Anderson Parameters").set("Storage Depth", depth);
        //jfnk_map[cid].jfnk_db->sublist("Anderson Parameters").set("Mixing Parameter", beta);
        Teuchos::ParameterList& printParams = jfnk_map[cid].jfnk_db->sublist("Printing");
        printParams.set("MyPID", soln->Comm().MyPID());
        printParams.set("Output Precision", 3);
        printParams.set("Output Processor", 0);
        printParams.set("Output Information",
            NOX::Utils::OuterIteration +
            NOX::Utils::OuterIterationStatusTest +
            NOX::Utils::Parameters +
            NOX::Utils::Details +
            NOX::Utils::Warning +
            NOX::Utils::Debug +
            NOX::Utils::Error);

        Teuchos::ParameterList& searchParams = jfnk_map[cid].jfnk_db->sublist("Line Search");
        searchParams.set("Method", "Full Step");

        Teuchos::ParameterList& dirParams = jfnk_map[cid].jfnk_db->sublist("Direction");
        dirParams.set("Method", "Newton");
        Teuchos::ParameterList& newtonParams = dirParams.sublist("Newton");
        newtonParams.set("Forcing Term Method", "Constant");

        Teuchos::ParameterList& lsParams = newtonParams.sublist("Linear Solver");
        lsParams.set("Aztec Solver", "GMRES");
        lsParams.set("Max Iterations", 800);
        lsParams.set("Tolerance", 1e-4);
        //lsParams.set("Preconditioner", "New Ifpack");
        //lsParams.set("Preconditioner Reuse Policy", "Reuse");
        //lsParams.set("Max Age Of Prec", 5);

        // setup model evaluator
        Teuchos::RCP<ModelEvaluator_JFNK> modelEvaluator = Teuchos::rcp(new ModelEvaluator_JFNK(functionptr,soln,F));

        // Get solution cast into NOX vector
        jfnk_map[cid].soln=Teuchos::RCP<NOX::Epetra::Vector>(new NOX::Epetra::Vector(soln, NOX::Epetra::Vector::CreateView));

        // Create MF object
        Teuchos::RCP<NOX::Epetra::MatrixFree> MF = Teuchos::rcp(new NOX::Epetra::MatrixFree(printParams,modelEvaluator,*(jfnk_map[cid].soln),false));
        Teuchos::RCP<NOX::Epetra::Interface::Jacobian> iJac = MF;

        // create linear solver object
        Teuchos::RCP<NOX::Epetra::LinearSystemAztecOO> linSys =
          Teuchos::rcp(new NOX::Epetra::LinearSystemAztecOO(printParams,lsParams,modelEvaluator,iJac,MF,*(jfnk_map[cid].soln)));

        // create NOX group
        Teuchos::RCP<NOX::Epetra::Group> noxGroup =
          Teuchos::rcp(new NOX::Epetra::Group(printParams, modelEvaluator, *(jfnk_map[cid].soln), linSys));

        // create convergence tests
        Teuchos::RCP<NOX::StatusTest::NormF> absresid =
          Teuchos::rcp(new NOX::StatusTest::NormF(1.0e-8));
        Teuchos::RCP<NOX::StatusTest::NormF> relresid =
          Teuchos::rcp(new NOX::StatusTest::NormF(*noxGroup, 1.0e-2));
        Teuchos::RCP<NOX::StatusTest::NormUpdate> update =
          Teuchos::rcp(new NOX::StatusTest::NormUpdate(1.0e-5));
        Teuchos::RCP<NOX::StatusTest::NormWRMS> wrms =
          Teuchos::rcp(new NOX::StatusTest::NormWRMS(1.0e-2, 1.0e-8));
        Teuchos::RCP<NOX::StatusTest::Combo> converged =
          Teuchos::rcp(new NOX::StatusTest::Combo(NOX::StatusTest::Combo::AND));
        converged->addStatusTest(absresid);
        converged->addStatusTest(relresid);
        converged->addStatusTest(wrms);
        converged->addStatusTest(update);
        Teuchos::RCP<NOX::StatusTest::MaxIters> maxiters =
          Teuchos::rcp(new NOX::StatusTest::MaxIters(20));
        Teuchos::RCP<NOX::StatusTest::FiniteValue> fv =
          Teuchos::rcp(new NOX::StatusTest::FiniteValue);
        Teuchos::RCP<NOX::StatusTest::Combo> combo =
          Teuchos::rcp(new NOX::StatusTest::Combo(NOX::StatusTest::Combo::OR));
        combo->addStatusTest(fv);
        combo->addStatusTest(converged);
        combo->addStatusTest(maxiters);

        // create solver
        jfnk_map[cid].solver = NOX::Solver::buildSolver(noxGroup, combo, jfnk_map[cid].jfnk_db);

        cid++;
        return cid-1;
    }

    int delete_data(const int id){
        jfnk_map.erase(id);
        return 0;
    }

    int solve(const int id) {
        jfnk_map[id].solver->solve();
        return 0;
    }

private:
        int cid;
        map<int, JFNKCnt> jfnk_map;
};
