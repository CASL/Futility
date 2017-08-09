/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
/                          Futility Development Group                          !
/                             All rights reserved.                             !
/                                                                              !
/ Futility is a jointly-maintained, open-source project between the University !
/ of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
/ can be found in LICENSE.txt in the head directory of this repository.        !
/+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/
#include "trilinos_ts.hpp"

TSCnt::TSCnt(FunctionPointer fptr, int n, double tol,
             Teuchos::ParameterList &params)
    : fptr(fptr), n(n), tol(tol)
{
    params.remove("ts_option");

    // setup parameterlist with defaults
    // eventually read this from somewhere
    ts_db = params;

    return;
}

int TSCnt::step(double tstart, double tend, const Vector &xstart, Vector &xend)
{
    return 0;
}
