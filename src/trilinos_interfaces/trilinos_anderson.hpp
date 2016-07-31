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

    int new_data(const int depth, const double beta, Teuchos::RCP<Epetra_Vector> soln) {
        anderson_map[cid]=AndersonCnt();

        //setup parameterlist with defaults
        anderson_map[cid].anderson_db = Teuchos::parameterList();
        anderson_map[cid].anderson_db->set("Nonlinear Solver", "Anderson Accelerated Fixed-Point");
        anderson_map[cid].anderson_db->sublist("Anderson Parameters").set("Storage Depth", depth);
        anderson_map[cid].anderson_db->sublist("Anderson Parameters").set("Mixing Parameter", beta);
        Teuchos::ParameterList& printParams = anderson_map[cid].anderson_db->sublist("Printing");

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
          Teuchos::rcp(new NOX::StatusTest::MaxIters(1000));
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
        // do something to reset the nox iteration between statepoint solves
        return 0;
    }

private:
        int cid;
        map<int, AndersonCnt> anderson_map;
};
