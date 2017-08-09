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

#include "trilinos_mat_vec.hpp"

class TSCnt : ForPETRA_SelectedTypes {
public:
    typedef void (*FunctionPointer)();

    TSCnt(){}

    TSCnt(FunctionPointer fptr, int n, double tol,
          Teuchos::ParameterList &params);

    int step(double tstart, double tend, const Vector &xtart, Vector &xend);

    FunctionPointer fptr = nullptr;
    int n;
    double tol;
    Teuchos::ParameterList ts_db;
    // maybe some other things about a specific solver
};

class TSStore : ForPETRA_SelectedTypes {
public:
    typedef TSCnt::FunctionPointer FunctionPointer;
    TSStore() : cid(0)
    {
    }

    int new_data(FunctionPointer fptr, int n, double tol,
                 Teuchos::ParameterList &params)
    {
        ts_map[cid] = TSCnt(fptr, n, tol, params);
        cid++;
        return cid - 1;
    }

    int delete_data(const int id)
    {
        ts_map.erase(id);
    }

    const TSCnt &operator[](int id) const
    {
        return ts_map.at(id);
    }

    TSCnt &operator[](int id)
    {
        return ts_map.at(id);
    }

private:
    int cid;
    std::map<int, TSCnt> ts_map;
};
