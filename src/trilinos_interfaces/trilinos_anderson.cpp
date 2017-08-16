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

////////////////////////////////////////////////////////////////////////////////
// JFNK
////////////////////////////////////////////////////////////////////////////////
JFNKCnt::JFNKCnt(void (*functionptr)(), Teuchos::RCP<Vector> soln_in,
                 Teuchos::RCP<Vector> F)
{
    auto e_soln = convertTpetra(soln_in);
    auto e_F = convertTpetra(F);

    // setup parameterlist with defaults
    int rank = soln_in->getMap()->getComm()->getRank();
    jfnk_db = Teuchos::parameterList();
    jfnk_db->set("Nonlinear Solver", "Line Search Based");
    Teuchos::ParameterList &printParams = jfnk_db->sublist("Printing");
    printParams.set("MyPID", rank);
    printParams.set("Output Precision", 3);
    printParams.set("Output Processor", 0);
    printParams.set(
        "Output Information",
        NOX::Utils::OuterIteration + NOX::Utils::OuterIterationStatusTest +
            NOX::Utils::Parameters + NOX::Utils::Details + NOX::Utils::Warning +
            NOX::Utils::Debug + NOX::Utils::Error);

    Teuchos::ParameterList &searchParams = jfnk_db->sublist("Line Search");
    searchParams.set("Method", "Full Step");

    Teuchos::ParameterList &dirParams = jfnk_db->sublist("Direction");
    dirParams.set("Method", "Newton");
    Teuchos::ParameterList &newtonParams = dirParams.sublist("Newton");
    newtonParams.set("Forcing Term Method", "Constant");

    Teuchos::ParameterList &lsParams = newtonParams.sublist("Linear Solver");
    lsParams.set("Aztec Solver", "GMRES");
    lsParams.set("Max Iterations", 800);
    lsParams.set("Tolerance", 1e-4);
    // lsParams.set("Preconditioner", "New Ifpack");
    // lsParams.set("Preconditioner Reuse Policy", "Reuse");
    // lsParams.set("Max Age Of Prec", 5);

    // setup model evaluator
    Teuchos::RCP<ModelEvaluator_JFNK> modelEvaluator =
        Teuchos::rcp(new ModelEvaluator_JFNK(functionptr, e_soln, e_F));

    // Get solution cast into NOX vector
    soln = Teuchos::RCP<NOX::Epetra::Vector>(
        new NOX::Epetra::Vector(e_soln, NOX::Epetra::Vector::CreateView));

    // Create MF object
    Teuchos::RCP<NOX::Epetra::MatrixFree> MF =
        Teuchos::rcp(new NOX::Epetra::MatrixFree(printParams, modelEvaluator,
                                                 *soln, false));
    Teuchos::RCP<NOX::Epetra::Interface::Jacobian> iJac = MF;

    // create linear solver object
    Teuchos::RCP<NOX::Epetra::LinearSystemAztecOO> linSys =
        Teuchos::rcp(new NOX::Epetra::LinearSystemAztecOO(
            printParams, lsParams, modelEvaluator, iJac, MF, *soln));

    // create NOX group
    Teuchos::RCP<NOX::Epetra::Group> noxGroup = Teuchos::rcp(
        new NOX::Epetra::Group(printParams, modelEvaluator, *soln, linSys));

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
    solver = NOX::Solver::buildSolver(noxGroup, combo, jfnk_db);

    return;
}
#endif

