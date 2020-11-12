/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
/                          Futility Development Group                          !
/                             All rights reserved.                             !
/                                                                              !
/ Futility is a jointly-maintained, open-source project between the University !
/ of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
/ can be found in LICENSE.txt in the head directory of this repository.        !
/+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/
#pragma once

#include "FMU2.h"
#include <iostream>
#include <string.h>
#include "FMU2_c_api.h"


C_FMU2Slave InitilizeFMU2_Slave(int slave_id, char* guid, char* modelIdentifier, char* unzipDirectory, char* instanceName)
{
  fmikit::FMU2Slave *s = new fmikit::FMU2Slave(guid, modelIdentifier, unzipDirectory, instanceName);
  s->instantiate(false);  // TODO: enable logging
  return (C_FMU2Slave)s;
}

void setupExperimentFMU2_Slave(C_FMU2Slave fmu2_slave, bool toleranceDefined, double tolerance, double startTime, bool stopTimeDefined, double stopTime)
{
  reinterpret_cast<fmikit::FMU2Slave*>(fmu2_slave)->setupExperiment(toleranceDefined, tolerance, startTime, stopTimeDefined, stopTime);
  reinterpret_cast<fmikit::FMU2Slave*>(fmu2_slave)->enterInitializationMode();
  reinterpret_cast<fmikit::FMU2Slave*>(fmu2_slave)->exitInitializationMode();
}

void getRealFMU2_Slave(C_FMU2Slave fmu2_slave, int valueReference, double& val)
{
  val = reinterpret_cast<fmikit::FMU2Slave*>(fmu2_slave)->getReal(valueReference);
}

void setRealFMU2_Slave(C_FMU2Slave fmu2_slave, int valueReference, double val)
{
  reinterpret_cast<fmikit::FMU2Slave*>(fmu2_slave)->setReal(valueReference, val);
}

void getIntegerFMU2_Slave(C_FMU2Slave fmu2_slave, int valueReference, int& val)
{
  val = reinterpret_cast<fmikit::FMU2Slave*>(fmu2_slave)->getInteger(valueReference);
}

void setIntegerFMU2_Slave(C_FMU2Slave fmu2_slave, int valueReference, int val)
{
  reinterpret_cast<fmikit::FMU2Slave*>(fmu2_slave)->setInteger(valueReference, val);
}

void getBooleanFMU2_Slave(C_FMU2Slave fmu2_slave, bool valueReference, bool& val)
{
  val = reinterpret_cast<fmikit::FMU2Slave*>(fmu2_slave)->getBoolean(valueReference);
}

void setBooleanFMU2_Slave(C_FMU2Slave fmu2_slave, bool valueReference, bool val)
{
  reinterpret_cast<fmikit::FMU2Slave*>(fmu2_slave)->setBoolean(valueReference, val);
}

void setNoRewindFlagFMU2_Slave(C_FMU2Slave fmu2_slave, bool noRw)
{
  reinterpret_cast<fmikit::FMU2Slave*>(fmu2_slave)->setNoRewindFlag(noRw);
}

void doStepFMU2_Slave(C_FMU2Slave fmu2_slave, double h)
{
  reinterpret_cast<fmikit::FMU2Slave*>(fmu2_slave)->doStep(h);
}

void clearFMU2_Slave(C_FMU2Slave fmu2_slave)
{
  reinterpret_cast<fmikit::FMU2Slave*>(fmu2_slave)->~FMU2Slave();
}

void serializeStateFMU2_Slave(C_FMU2Slave fmu2_slave)
{
  reinterpret_cast<fmikit::FMU2Slave*>(fmu2_slave)->getStateSlave();
}

void deSerializeStateFMU2_Slave(C_FMU2Slave fmu2_slave){
  reinterpret_cast<fmikit::FMU2Slave*>(fmu2_slave)->loadStateSlave();
}
