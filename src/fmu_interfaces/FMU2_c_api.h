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

extern "C" C_FMU2Slave InitilizeFMU2_Slave(int id, char* guid, char* modelIdentifier, char* unzipDirectory, char* instanceName);

extern "C" void clearFMU2_Slave(C_FMU2Slave fmu2_slave);

extern "C" void setupExperimentFMU2_Slave(C_FMU2Slave fmu2_slave, bool toleranceDefined, double tolerance, double startTime, bool stopTimeDefined, double stopTime);

// valueReference is the id of the variable you want to get from the FMU model.
// valueReference defined in the FMU XML model def
extern "C" void getRealFMU2_Slave(C_FMU2Slave fmu2_slave, int valueReference, double& val);

extern "C" void setRealFMU2_Slave(C_FMU2Slave fmu2_slave, int valueReference, double val);

extern "C" void getIntegerFMU2_Slave(C_FMU2Slave fmu2_slave, int valueReference, int& val);

extern "C" void setIntegerFMU2_Slave(C_FMU2Slave fmu2_slave, int valueReference, int val);

extern "C" void getBooleanFMU2_Slave(C_FMU2Slave fmu2_slave, bool valueReference, bool& val);

extern "C" void setBooleanFMU2_Slave(C_FMU2Slave fmu2_slave, bool valueReference, bool val);

extern "C" void doStepFMU2_Slave(C_FMU2Slave fmu2_slave, double h);

extern "C" void serializeStateFMU2_Slave(C_FMU2Slave fmu2_slave);
