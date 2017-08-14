/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
/                          Futility Development Group                          !
/                             All rights reserved.                             !
/                                                                              !
/ Futility is a jointly-maintained, open-source project between the University !
/ of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
/ can be found in LICENSE.txt in the head directory of this repository.        !
/+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

#include "trilinos_mat_vec.hpp"

TpetraVecCnt::TpetraVecCnt(int n, int nloc, MPI_Comm rawComm)
    : comm(new SelectedComm(rawComm)),
      map(new Map(n, nloc, 1, comm)),
      vec(new Vector(map)),
      localView(vec->getLocalView<HostSpace>()),
      havedistMap(false)
{
    // This is unneccesary, since this Tpetra::Vector<> constructor
    // initializes to zero
    vec->putScalar(0.0);
}

void TpetraVecCnt::defineMapData(const int id, const int nloc, const int *gid)
{
    Teuchos::ArrayView<const GO> gid_view(gid, nloc);
    distMap       = Teuchos::rcp(new Map(-1, gid_view, 1, comm));
    importer      = Teuchos::rcp(new Import(distMap, map));
    distvec       = Teuchos::rcp(new Vector(distMap));
    distLocalView = distvec->getLocalView<HostSpace>();
    havedistMap   = true;

    return;
}
