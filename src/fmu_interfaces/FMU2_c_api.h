/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
/                          Futility Development Group                          !
/                             All rights reserved.                             !
/                                                                              !
/ Futility is a jointly-maintained, open-source project between the University !
/ of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
/ can be found in LICENSE.txt in the head directory of this repository.        !
/+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/
#pragma once

typedef void* C_FMU2Slave;

extern "C" void InitilizeFMU2_Slave(C_FMU2Slave fmu2_slave, int id, char* guid, char* modelIdentifier, char* unzipDirectory, char* instanceName);

extern "C" void doStepFMU2_Slave(C_FMU2Slave fmu2_slave, double h);
