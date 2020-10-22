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

extern "C" void setupExperimentFMU2_Slave(C_FMU2Slave fmu2_slave, bool toleranceDefined, double tolerance, double startTime, double stopTimeDefined, double stopTime);

// valueReference is the id of the variable you want to get from the FMU model.
// valueReferences are defined in the FMU XML model def
extern "C" void getRealFMU2_Slave(C_FMU2Slave fmu2_slave, int valueReference, double& val);

extern "C" void setRealFMU2_Slave(C_FMU2Slave fmu2_slave, int valueReference, double val);

extern "C" void doStepFMU2_Slave(C_FMU2Slave fmu2_slave, double h);
