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
#include "Epetra_Operator.h"
#include "Epetra_InvOperator.h"
#include "Ifpack.h"
//#ifdef USE_ML
#include "ml_MultiLevelPreconditioner.h"
//#endif
#include <cassert>

class PCCnt{
public:
    /*
    Notes:

    */
    Teuchos::RCP<Epetra_CrsMatrix> M;
    Teuchos::RCP<Epetra_Operator> pc;
    Teuchos::ParameterList pc_db;
    std::string pc_type;
    //maybe some other things about a specific solver
};

class PCStore {
public:
    PCStore():
        cid(0)
    {}

    int new_data(Teuchos::ParameterList &params) {
        const int option = params.get<int>("pc_option");
        // pc_option needs to be removed, or Trilinos will complain about
        // invalid parameters
        params.remove("pc_option");
        //Teuchos::ParameterList db
        pc_map[cid]=PCCnt();
        //setup parameterlist with defaults
        //eventually read this from somewhere
        if(option==1){pc_map[cid].pc_type="IFPACK";}
        else if(option==2){pc_map[cid].pc_type="ML";}

        //RCP_ParameterList ifpack_db = Teuchos::sublist(db, "Ifpack Params");
        if(pc_map[cid].pc_type == "IFPACK"){
            params.get("Ifpack Type", std::string("ILU"));
            params.get("Ifpack Overlap", 0);
        }
        else if(pc_map[cid].pc_type == "ML"){
            pc_map[cid].pc_db.get("ML Default Type", std::string("SA"));
            //pc_map[cid].pc_db.get("smoother: type", std::string("Gauss-Seidel"));
            params.get("smoother: type", std::string("ILU"));
            params.get("aggregation: type", std::string("Uncoupled"));
            params.get("aggregation: damping factor", 0.0);
            params.get("smoother: damping factor", 1.0);
            params.get("smoother: sweeps", 3);
            params.get("smoother: ifpack overlap", 1);
            //params.get("PDE equations",2);
            params.get("max levels", 8);
            params.get("ML output", 10);
        }

        pc_map[cid].pc_db = params;
        cid++;
        return cid-1;
    }

    int delete_data(const int id){
        pc_map.erase(id);
    }

    int resetPC_data(const int id, Teuchos::RCP<Epetra_CrsMatrix> M) {
        pc_map[id].pc = Teuchos::null;
        pc_map[id].M = Teuchos::null;
        return setupPC_data(id, M);
    }

    int setupPC_data(const int id, Teuchos::RCP<Epetra_CrsMatrix> M) {
        pc_map[id].M=M;
        if(pc_map[id].M->Comm().MyPID()==0) std::cout << "Setting up PC..." << std::endl;
        if(pc_map[id].pc_type == "IFPACK"){
            Ifpack ifpack_factory;
            Teuchos::RCP<Ifpack_Preconditioner> ifpack_prec;

            std::string ifpack_type = pc_map[id].pc_db.get("Ifpack Type", std::string("ILU"));
            int overlap        = pc_map[id].pc_db.get("Ifpack Overlap", 0);

            ifpack_prec = Teuchos::rcp( ifpack_factory.Create(
                    ifpack_type, pc_map[id].M.get(), overlap ) );

            ifpack_prec->SetParameters(pc_map[id].pc_db);

            int err;
            err = ifpack_prec->Initialize();
            assert( err == 0 );
            err = ifpack_prec->Compute();
            assert( err == 0 );

            pc_map[id].pc = Teuchos::RCP<Epetra_Operator>(
                    new Epetra_InvOperator(ifpack_prec.getRawPtr()) );
            Teuchos::set_extra_data(ifpack_prec,"ifpack_raw_pointer",Teuchos::inOutArg(pc_map[id].pc));
            Teuchos::set_extra_data(M,"ifpack_mat_raw_pointer",Teuchos::inOutArg(pc_map[id].pc));
        }
        else if(pc_map[id].pc_type == "ML"){
            Teuchos::RCP<ML_Epetra::MultiLevelPreconditioner> ml_prec;

            std::string default_type = pc_map[id].pc_db.get("ML Default Type", std::string("DD"));
            bool override = false;
            ML_Epetra::SetDefaults(default_type, pc_map[id].pc_db, 0, 0,override);
            //pc_map[id].pc_db.get("ML output", 10);
            ml_prec = Teuchos::rcp( new ML_Epetra::MultiLevelPreconditioner(
                                    *(pc_map[id].M), pc_map[id].pc_db ) );
            //pc_map[id].pc_db.print();


            pc_map[id].pc = Teuchos::RCP<Epetra_Operator>(
                                    new Epetra_InvOperator(ml_prec.getRawPtr()) );
            Teuchos::set_extra_data(ml_prec,"ml_raw_pointer",Teuchos::inOutArg(pc_map[id].pc));
            Teuchos::set_extra_data(M,"ml_mat_raw_pointer",Teuchos::inOutArg(pc_map[id].pc));
        }
        if(pc_map[id].M->Comm().MyPID()==0) std::cout << "PC Constructed..." << std::endl;
        return 0;
    }

    Teuchos::RCP<Epetra_Operator> get_pc(const int id){
        return pc_map[id].pc;
    }

private:
        int cid;
        std::map<int, PCCnt> pc_map;
};
