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

#include <Teuchos_RCP.hpp>
#include <Tpetra_Operator.hpp>

#include <MueLu_CreateTpetraPreconditioner.hpp>

#include "trilinos_mat_vec.hpp"

#ifdef HAVE_MPI
#include <mpi.h>
#endif

#include <Ifpack2_Factory_decl.hpp>
#include <Teuchos_ParameterList.hpp>

class PCCnt : public ForPETRA_SelectedTypes {
public:
    Teuchos::RCP<CrsMatrix> M;
    Teuchos::RCP<Operator> pc;
    Teuchos::ParameterList pc_db;
    std::string pc_type;
    // maybe some other things about a specific solver
};

class PCStore : public ForPETRA_SelectedTypes {
public:
    PCStore() : cid(0)
    {
        return;
    }

    int new_data(Teuchos::ParameterList &params);
    
    int delete_data(const int id)
    {
        pc_map.erase(id);
    }

    int resetPC_data(const int id, Teuchos::RCP<CrsMatrix> M)
    {
        pc_map[id].pc = Teuchos::null;
        pc_map[id].M  = Teuchos::null;
        return setupPC_data(id, M);
    }

    int setupPC_data(const int id, Teuchos::RCP<CrsMatrix> M);

    Teuchos::RCP<Operator> get_pc(const int id)
    {
        return pc_map[id].pc;
    }

private:
    int cid;
    std::map<int, PCCnt> pc_map;
};
