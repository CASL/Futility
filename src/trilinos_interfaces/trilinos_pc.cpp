/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
/                          Futility Development Group                          !
/                             All rights reserved.                             !
/                                                                              !
/ Futility is a jointly-maintained, open-source project between the University !
/ of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
/ can be found in LICENSE.txt in the head directory of this repository.        !
/+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

#include "trilinos_pc.hpp"

int PCStore::new_data(Teuchos::ParameterList &params)
{
    const int option = params.get<int>("pc_option");
    // pc_option needs to be removed, or Trilinos will complain about
    // invalid parameters
    params.remove("pc_option");
    // Teuchos::ParameterList db
    pc_map[cid]  = PCCnt();
    auto &pc_obj = pc_map[cid];

    // setup parameterlist with defaults
    // eventually read this from somewhere
    if (option == 1) {
        pc_obj.pc_type = "Ifpack2";
    } else if (option == 2) {
        pc_obj.pc_type = "MueLu";
    }

    // RCP_ParameterList ifpack_db = Teuchos::sublist(db, "Ifpack Params");
    if (pc_obj.pc_type == "Ifpack2") {
        params.get("Ifpack Type", std::string("ILUT"));
        params.get("Ifpack Overlap", 0);
    } else if (pc_obj.pc_type == "MueLu") {
        pc_obj.pc_db.get("ML Default Type", std::string("SA"));
        params.get("max levels", 8);
        params.get("smoother: type", std::string("RILUK"));
        params.get("sa: damping factor", 1.0);
        params.get("smoother: sweeps", 3);
        params.get("smoother: overlap", 1);
        params.get("aggregation: type", std::string("uncoupled"));
        params.get("multigrid algorithm", "sa");
        params.get("sa: damping factor", 0.0);
        params.get("verbosity", "medium");
    }

    pc_obj.pc_db = params;
    cid++;
    return cid - 1;
}

int PCStore::setupPC_data(const int id, Teuchos::RCP<CrsMatrix> M)
{
    auto &pc_obj = pc_map[id];

    pc_obj.M = M;
    if (pc_obj.M->getComm()->getRank() == 0) {
        std::cout << "Setting up PC..." << std::endl;
    }
    if (pc_obj.pc_type == "Ifpack2") {
        typedef Ifpack2::Preconditioner<SC, LO, GO, NO> Preconditioner;

        std::string ifpack_type =
            pc_obj.pc_db.get("Ifpack Type", std::string("RILUK"));
        int overlap = pc_obj.pc_db.get("Ifpack Overlap", 0);

        Ifpack2::Factory ifpack2_factory;

        auto const_M = Teuchos::rcp_static_cast<const CrsMatrix>(pc_obj.M);

        Teuchos::RCP<Preconditioner> ifpack_prec =
            ifpack2_factory.create(ifpack_type, const_M);

        ifpack_prec->setParameters(pc_obj.pc_db);

        ifpack_prec->initialize();
        ifpack_prec->compute();

        pc_obj.pc = ifpack_prec;
        // Teuchos::set_extra_data(ifpack_prec, "ifpack_raw_pointer",
        //                         Teuchos::inOutArg(pc_obj.pc));
        // Teuchos::set_extra_data(M, "ifpack_mat_raw_pointer",
        //                         Teuchos::inOutArg(pc_obj.pc));
    } else if (pc_obj.pc_type == "MueLu") {
        pc_obj.pc = MueLu::CreateTpetraPreconditioner(
            Teuchos::rcp_static_cast<Operator>(M), pc_obj.pc_db);
    }
    if (pc_obj.M->getComm()->getRank() == 0)
        std::cout << "PC Constructed..." << std::endl;
    return 0;
}
