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
#include "Teuchos_RCP.hpp"

#ifdef HAVE_MPI
#include <mpi.h>
#endif

#include "trilinos_mat_vec.hpp"

#include <AnasaziBasicEigenproblem.hpp>
#include <AnasaziTpetraAdapter.hpp>
#include <AnasaziGeneralizedDavidsonSolMgr.hpp>
#include <AnasaziTypes.hpp>
#include <BelosTpetraAdapter.hpp>
#include <BelosSolverFactory.hpp>
#include <BelosSolverManager.hpp>
#include <Teuchos_ParameterList.hpp>

class AnasaziCnt : public ForPETRA_SelectedTypes {
public:
    /*
    Notes:
      solver manager needs a problem
      problem needs matrices
      the two above statements mean we can't constrcut them until solve time
    (consistent with Steven)
    */

    // We're solving   LHS*x = k*RHS*x
    Teuchos::RCP<CrsMatrix> LHS;
    Teuchos::RCP<CrsMatrix> RHS;
    Teuchos::RCP<Operator> pc;
    bool haspc = false;
    double keff;
    int niters;
    int pc_id;
    Teuchos::RCP<Vector> x;
    Teuchos::ParameterList anasazi_db;
    // maybe some other things about a specific solver
};

class AnasaziStore {
    typedef AnasaziCnt::Vector Vector;
    typedef AnasaziCnt::CrsMatrix CrsMatrix;
    typedef TpetraMatCnt::Operator Operator;
    typedef Tpetra::MultiVector<> MultiVector;

public:
    AnasaziStore() : cid(0)
    {
    }

    int new_data(Teuchos::ParameterList &params);

    int delete_data(const int id)
    {
        anasazi_map.erase(id);
        return 0;
    }

    int setMat_data(const int id, Teuchos::RCP<CrsMatrix> LHS,
                    Teuchos::RCP<CrsMatrix> RHS)
    {
        anasazi_map[id].LHS = LHS;
        anasazi_map[id].RHS = RHS;
        return 0;
    }

    int setConvCrit_data(const int id, const double tol, const int maxit)
    {
        anasazi_map[id].anasazi_db.set("Convergence Tolerance", tol);
        anasazi_map[id].anasazi_db.set("Maximum Restarts", maxit);
        return 0;
    }

    int setX0_data(const int id, Teuchos::RCP<Vector> x0)
    {
        anasazi_map[id].x = x0;
        return 0;
    }

    int setPC_data(const int id, const int pc_id, Teuchos::RCP<Operator> pc)
    {
        // if(anasazi_map[id].LHS->Comm().MyPID()==0) std::cout << pc->Label()
        // << std::endl;
        anasazi_map[id].pc_id = pc_id;
        anasazi_map[id].pc    = pc;
        anasazi_map[id].haspc = true;
        return 0;
    }

    int solve(const int id);

    int getEigenvalue_data(const int id, double &keff)
    {
        keff = anasazi_map[id].keff;
        return 0;
    }

    int getIterations_data(const int id, int &niters)
    {
        niters = anasazi_map[id].niters;
        return 0;
    }

    int getResidual(const int id, double &resid);

    int getPCid_data(const int id, int &pcid, Teuchos::RCP<CrsMatrix> &M)
    {
        pcid = anasazi_map[id].pc_id;
        M    = anasazi_map[id].LHS;
        return 0;
    }

private:
    int cid;
    std::map<int, AnasaziCnt> anasazi_map;
};

class BelosCnt : public ForPETRA_SelectedTypes {
public:
    /*
    Notes:
      solver manager needs a problem
      problem needs matrices
      the two above statements mean we can't constrcut them until solve time
    (consistent with Steven)
    */
    Teuchos::RCP<CrsMatrix> A;
    Teuchos::RCP<Operator> pc;
    bool haspc = false;
    int num_iters;
    Teuchos::RCP<Vector> x;
    Teuchos::RCP<Vector> b;
    Teuchos::ParameterList belos_db;
    // maybe some other things about a specific solver

    ~BelosCnt()
    {
    }
};

class BelosStore : public ForPETRA_SelectedTypes {
public:
    BelosStore() : cid(0)
    {
    }

    int new_data(Teuchos::ParameterList &params);

    int delete_data(const int id)
    {
        belos_map.erase(id);
        return 0;
    }

    int setMat_data(const int id, Teuchos::RCP<CrsMatrix> A)
    {
        belos_map[id].A = A;
        return 0;
    }

    int setConvCrit_data(const int id, const double tol, const int maxit)
    {
        belos_map[id].belos_db.set("Convergence Tolerance", tol);
        belos_map[id].belos_db.set("Maximum Iterations", maxit);
        return 0;
    }

    int setX0_data(const int id, Teuchos::RCP<Vector> x0)
    {
        belos_map[id].x = x0;
        return 0;
    }

    int setb_data(const int id, Teuchos::RCP<Vector> b)
    {
        belos_map[id].b = b;
        return 0;
    }

    int setPC_data(const int id, Teuchos::RCP<Operator> pc)
    {
        belos_map[id].pc    = pc;
        belos_map[id].haspc = true;
        return 0;
    }

    int solve(const int id);

    int getIterations_data(const int id, int &niter)
    {
        niter = belos_map[id].num_iters;
        return 0;
    }

    int getResidual(const int id, double &resid);

private:
    int cid;
    std::map<int, BelosCnt> belos_map;
};
