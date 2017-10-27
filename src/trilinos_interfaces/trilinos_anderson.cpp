/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
/                          Futility Development Group                          !
/                             All rights reserved.                             !
/                                                                              !
/ Futility is a jointly-maintained, open-source project between the University !
/ of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
/ can be found in LICENSE.txt in the head directory of this repository.        !
/+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/
#ifdef FUTILITY_HAVE_Trilinos
#include "trilinos_anderson.hpp"

#include <Epetra_DataAccess.h>
#include <Epetra_SerialComm.h>

#include <exception>
#include <sstream>

namespace {
Teuchos::RCP<Epetra_Vector>
convertTpetra(Teuchos::RCP<ForPETRA_SelectedTypes::Vector> tvec)
{
    if (tvec->isDistributed()) {
        std::stringstream str;
        str << __FILE__ << ":" << __LINE__
            << " Distributed vectors not supported.\n";
        throw std::runtime_error(str.str());
    }
    const auto &map_in = tvec->getMap();
    const int n        = map_in->getGlobalNumElements();
    Epetra_SerialComm ecomm;
    auto map = Teuchos::rcp<Epetra_Map>(new Epetra_Map(n, 1, ecomm));
    // This is performing a copy of the input vector. This is because the
    // semantics for Epetra::View are very unclear, and the question of shared
    // ownership with the Tpetra vector is currently unanswered. Ideally, this
    // copy would be elided if not necessary, but further investigation is
    // needed to do so safely.
    Teuchos::RCP<Epetra_Vector> evec(new Epetra_Vector(
        Epetra_DataAccess::Copy, *map, tvec->getDataNonConst().get()));

    return evec;
}
}

AndersonCnt::AndersonCnt(int depth, double beta, int start,
                         Teuchos::RCP<Vector> soln_in)
{
    // Extract the communicator and rank from the passed vector
    const auto &map_in = soln_in->getMap();
    const int rank     = map_in->getComm()->getRank();
    const int n        = map_in->getGlobalNumElements();

    // Convert the incoming Tpetra vector to Epetra
    auto evec = convertTpetra(soln_in);

    // setup parameterlist with defaults
    anderson_db = Teuchos::parameterList();
    anderson_db->set("Nonlinear Solver", "Anderson Accelerated Fixed-Point");
    if (depth == 0) {
        anderson_db->sublist("Anderson Parameters").set("Storage Depth", 1);
        anderson_db->sublist("Anderson Parameters")
            .set("Acceleration Start Iteration", 10000);
    } else {
        anderson_db->sublist("Anderson Parameters").set("Storage Depth", depth);
        anderson_db->sublist("Anderson Parameters")
            .set("Acceleration Start Iteration", start);
    }
    anderson_db->sublist("Anderson Parameters").set("Mixing Parameter", beta);
    Teuchos::ParameterList &printParams = anderson_db->sublist("Printing");

    printParams.set("MyPID", rank);
    printParams.set("Output Precision", 3);
    printParams.set("Output Processor", 0);
    printParams.set("Output Information",
                    //            NOX::Utils::OuterIteration +
                    //            NOX::Utils::OuterIterationStatusTest +
                    //            NOX::Utils::Parameters +
                    //            NOX::Utils::Details +
                    NOX::Utils::Warning + NOX::Utils::Debug +
                        NOX::Utils::Error);

    // setup model evaluator
    Teuchos::RCP<ModelEvaluator> modelEvaluator =
        Teuchos::rcp(new ModelEvaluator());

    // Get solution cast into NOX vector
    soln = Teuchos::RCP<NOX::Epetra::Vector>(
        new NOX::Epetra::Vector(evec, NOX::Epetra::Vector::CreateView));

    // create fake linear solver object
    //   This should go away if we get that interface fixed
    Teuchos::RCP<Teuchos::ParameterList> lsParams = Teuchos::parameterList();
    Teuchos::RCP<NOX::Epetra::LinearSystemAztecOO> linSys =
        Teuchos::rcp(new NOX::Epetra::LinearSystemAztecOO(
            printParams, *lsParams, modelEvaluator, *soln));

    // create NOX group
    Teuchos::RCP<NOX::Epetra::Group> noxGroup = Teuchos::rcp(
        new NOX::Epetra::Group(printParams, modelEvaluator, *soln, linSys));

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
    solver = NOX::Solver::buildSolver(noxGroup, combo, anderson_db);

    return;
}

void AndersonCnt::step()
{
    // Compute the correct residual vector in the NOX group
    const NOX::Abstract::Group &noxGroup = solver->getSolutionGroup();
    NOX::Abstract::Vector &resid =
        const_cast<NOX::Abstract::Vector &>(noxGroup.getF());
    resid.update(1.0, *(soln), -1.0);

    // Compute the next iterate with the NOX solver
    solver->step();

    // Overwrite soln with the new NOX iterate
    *(soln) = noxGroup.getX();
    return;
}

void AndersonCnt::reset()
{
    solver->reset(*(soln));
    NOX::Abstract::Group &noxGroup =
        const_cast<NOX::Abstract::Group &>(solver->getSolutionGroup());

    // compute initial (fake) residual
    noxGroup.computeF();
    return;
}
#endif
