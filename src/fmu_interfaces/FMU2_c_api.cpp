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


C_FMU2Base InitilizeFMU2_Base(int slave_id, char* guid, char* modelIdentifier, char* unzipDirectory, char* instanceName)
{
  fmikit::FMU2 *fmu_ptr;
  if (slave_id >= 0)
    // Call init of FMU2Slave
    fmu_ptr = new fmikit::FMU2Slave(guid, modelIdentifier, unzipDirectory, instanceName);
  else
    // Call init of FMU2Model
    fmu_ptr = new fmikit::FMU2Model(guid, modelIdentifier, unzipDirectory, instanceName);
  fmu_ptr->instantiate(false);  // TODO: enable logging
  return (C_FMU2Base)fmu_ptr;
}

void setupExperimentFMU2_Base(C_FMU2Base fmu2_base, bool toleranceDefined, double tolerance, double startTime, bool stopTimeDefined, double stopTime, bool finalizeInitialization)
{
  reinterpret_cast<fmikit::FMU2*>(fmu2_base)->setupExperiment(toleranceDefined, tolerance, startTime, stopTimeDefined, stopTime);
  if(finalizeInitialization){
    reinterpret_cast<fmikit::FMU2*>(fmu2_base)->enterInitializationMode();
    reinterpret_cast<fmikit::FMU2*>(fmu2_base)->exitInitializationMode();
  }
}

void enterInitializationModeFMU2_Base(C_FMU2Base fmu2_base)
{
  reinterpret_cast<fmikit::FMU2*>(fmu2_base)->enterInitializationMode();
}

void exitInitializationModeFMU2_Base(C_FMU2Base fmu2_base)
{
  reinterpret_cast<fmikit::FMU2*>(fmu2_base)->exitInitializationMode();
}

void getRealFMU2_Base(C_FMU2Base fmu2_base, int valueReference, double& val)
{
  val = reinterpret_cast<fmikit::FMU2*>(fmu2_base)->getReal(valueReference);
}

void setRealFMU2_Base(C_FMU2Base fmu2_base, int valueReference, double val)
{
  reinterpret_cast<fmikit::FMU2*>(fmu2_base)->setReal(valueReference, val);
}

void getIntegerFMU2_Base(C_FMU2Base fmu2_base, int valueReference, int& val)
{
  val = reinterpret_cast<fmikit::FMU2*>(fmu2_base)->getInteger(valueReference);
}

void setIntegerFMU2_Base(C_FMU2Base fmu2_base, int valueReference, int val)
{
  reinterpret_cast<fmikit::FMU2*>(fmu2_base)->setInteger(valueReference, val);
}

void getBooleanFMU2_Base(C_FMU2Base fmu2_base, bool valueReference, bool& val)
{
  val = reinterpret_cast<fmikit::FMU2*>(fmu2_base)->getBoolean(valueReference);
}

void setBooleanFMU2_Base(C_FMU2Base fmu2_base, bool valueReference, bool val)
{
  reinterpret_cast<fmikit::FMU2*>(fmu2_base)->setBoolean(valueReference, val);
}

void clearFMU2_Base(C_FMU2Base fmu2_base)
{
  reinterpret_cast<fmikit::FMU2*>(fmu2_base)->~FMU2();
}

// Methods for Co-Simulation only
void doStepFMU2_Slave(C_FMU2Slave fmu2_slave, double h)
{
  reinterpret_cast<fmikit::FMU2Slave*>(fmu2_slave)->doStep(h);
}

void setNoRewindFlagFMU2_Slave(C_FMU2Slave fmu2_slave, bool noRw)
{
  reinterpret_cast<fmikit::FMU2Slave*>(fmu2_slave)->setNoRewindFlag(noRw);
}

void serializeStateFMU2_Slave(C_FMU2Slave fmu2_slave)
{
  reinterpret_cast<fmikit::FMU2Slave*>(fmu2_slave)->getStateSlave();
}

void deSerializeStateFMU2_Slave(C_FMU2Slave fmu2_slave){
  reinterpret_cast<fmikit::FMU2Slave*>(fmu2_slave)->loadStateSlave();
}
