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
#include <map>

#include <Epetra_LinearProblem.h>
#include <Epetra_MultiVector.h>
#include <Epetra_Operator.h>
#include <NOX.H>
#include <NOX_Epetra.H>
#include <NOX_Epetra_Interface_Required.H> // base class
#include <Teuchos_ParameterList.hpp>
#include <Teuchos_RCP.hpp>

#include "trilinos_mat_vec.hpp"

class AndersonCnt : public ForPETRA_SelectedTypes {
public:
    AndersonCnt()
    {
        return;
    }
    AndersonCnt(int depth, double beta, int start,
                Teuchos::RCP<Vector> soln_in);

    void step();

    void reset();
    /*
    Notes:
      solver manager needs a problem
      problem needs matrices
      the two above statements mean we can't constrcut them until solve time
    (consistent with Steven)
    */

    //
    // need solver
    // other things the solver needs
    Teuchos::RCP<Teuchos::ParameterList> anderson_db;
    Teuchos::RCP<NOX::Solver::Generic> solver;
    Teuchos::RCP<NOX::Epetra::Vector> soln;
    // maybe some other things about a specific solver
};

class ModelEvaluator : public NOX::Epetra::Interface::Required {
public:
    ModelEvaluator()
    {
    }

    // Destructor
    ~ModelEvaluator()
    {
    }

    bool computeF(const Epetra_Vector &x, Epetra_Vector &f,
                  NOX::Epetra::Interface::Required::FillType)
    {
        // Residual should be G(x) - x, but G(x) not yet evaluated, so
        // just set f to x for now. Actual residual will be computed elsewhere
        f = x;
        return true;
    }
};

class AndersonStore : public ForPETRA_SelectedTypes {
public:
    AndersonStore() : cid(0)
    {
    }

    int new_data(int depth, double beta, int start,
                 Teuchos::RCP<Vector> soln)
    {
        anderson_map[cid] = AndersonCnt(depth, beta, start, soln);

        cid++;
        return cid - 1;
    }

    int delete_data(int id)
    {
        anderson_map.erase(id);
        return 0;
    }

    int step(int id)
    {
        anderson_map[id].step();
        return 0;
    }

    int reset_data(int id)
    {
        anderson_map[id].reset();
        return 0;
    }

private:
    int cid;
    std::map<int, AndersonCnt> anderson_map;
};

// JFNK Interface
class JFNKCnt : public ForPETRA_SelectedTypes {
public:
    JFNKCnt()
    {
        return;
    }

    JFNKCnt(void (*functionptr)(), Teuchos::RCP<Vector> soln,
            Teuchos::RCP<Vector> F);
    /*
    Notes:
      solver manager needs a problem
      problem needs matrices
      the two above statements mean we can't constrcut them until solve time
    (consistent with Steven)
    */

    //
    // need solver
    // other things the solver needs
    Teuchos::RCP<Teuchos::ParameterList> jfnk_db;
    Teuchos::RCP<NOX::Solver::Generic> solver;
    Teuchos::RCP<NOX::Epetra::Vector> soln;
    // maybe some other things about a specific solver
};

class ModelEvaluator_JFNK : public NOX::Epetra::Interface::Required {
public:
    ModelEvaluator_JFNK(void (*functionptr)(), Teuchos::RCP<Epetra_Vector> x,
                        Teuchos::RCP<Epetra_Vector> F)
    {
        fptr = functionptr;
        xloc = x;
        Floc = F;
    }

    // Destructor
    ~ModelEvaluator_JFNK()
    {
    }

    bool computeF(const Epetra_Vector &x, Epetra_Vector &f,
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

class JFNKStore : public ForPETRA_SelectedTypes {
public:
    JFNKStore() : cid(0)
    {
    }

    int new_data(void (*functionptr)(), Teuchos::RCP<Vector> soln,
                 Teuchos::RCP<Vector> F)
    {
        jfnk_map[cid] = JFNKCnt(functionptr, soln, F);

        cid++;
        return cid - 1;
    }

    int delete_data(const int id)
    {
        jfnk_map.erase(id);
        return 0;
    }

    int solve(const int id)
    {
        jfnk_map[id].solver->solve();
        return 0;
    }

private:
    int cid;
    std::map<int, JFNKCnt> jfnk_map;
};
