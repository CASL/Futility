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

#include <cassert>

using std::map;

class TSCnt{
public:
    /*
    Notes:
    */
    void fptr() = 0;
    int n;
    double tol;
    Teuchos::ParameterList ts_db;
    //maybe some other things about a specific solver
};

class TSStore {
public:
    TSStore():
        cid(0)
    {}

    int new_data(void (*funptr)(), const int n,
                 const double tol,Teuchos::ParameterList &params) {
        const int option = params.get<int>("ts_option");
        // ts_option needs to be removed, or Trilinos will complain about
        // invalid parameters
        params.remove("ts_option");
        //Teuchos::ParameterList db
        ts_map[cid]=TSCnt();
        //setup parameterlist with defaults
        //eventually read this from somewhere

        ts_map[cid].fptr=funptr;
        ts_map[cid].n=n;
        ts_map[cid].tol=tol;
        ts_map[cid].ts_db = params;
        cid++;
        return cid-1;
    }

    int delete_data(const int id){
        ts_map.erase(id);
    }

    int step_data(const int id, const double tstart, const double tend,
                        const double* x, double* xdot) {

        return 0;
    }

private:
        int cid;
        map<int, TSCnt> ts_map;
};
