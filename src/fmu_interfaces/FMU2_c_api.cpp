/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
/                          Futility Development Group                          !
/                             All rights reserved.                             !
/                                                                              !
/ Futility is a jointly-maintained, open-source project between the University !
/ of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
/ can be found in LICENSE.txt in the head directory of this repository.        !
/+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/
#pragma once

#include "fmi2Functions.h"
#include "fmi2FunctionTypes.h"
#include "fmi2TypesPlatform.h"
#include "FMU.h"
#include "FMU2.h"
#include <iostream>
#include <string.h>
#include "FMU2_c_api.h"

bool verbosity = false;

C_FMU2Slave InitilizeFMU2_Slave(int slave_id, char* guid, char* modelIdentifier, char* unzipDirectory, char* instanceName)
{
  std::cout << "FMU2_Slave Init guid: " << guid << std::endl;
  std::cout << "FMU2_Slave Init modelIdentifier: " << modelIdentifier << std::endl;
  std::cout << "FMU2_Slave Init unzipDirectory: " << unzipDirectory << std::endl;
  std::cout << "FMU2_Slave Init instanceName: " << instanceName << std::endl;
  // return reinterpret_cast<void*>(new fmikit::FMU2Slave(guid, modelIdentifier, unzipDirectory, instanceName));
  fmikit::FMU2Slave *s = new fmikit::FMU2Slave(guid, modelIdentifier, unzipDirectory, instanceName);
  s->instantiate(false);  // TODO: enable logging
  std::cout << "FMU2_Slave Init ptr: " << s << std::endl;
  std::cout << "FMU2_Slave Init id: " << slave_id << " init done!" << std::endl;
  return (C_FMU2Slave)s;
}

void setupExperimentFMU2_Slave(C_FMU2Slave fmu2_slave, bool toleranceDefined, double tolerance, double startTime, bool stopTimeDefined, double stopTime)
{
  std::cout << "FMU2_Slave  setupExperiment startTime: " << startTime << std::endl;
  std::cout << "FMU2_Slave  setupExperiment stopTime: " << stopTime << std::endl;
  std::cout << "FMU2_Slave  setupExperiment tol: " << tolerance << std::endl;
  reinterpret_cast<fmikit::FMU2Slave*>(fmu2_slave)->setupExperiment(toleranceDefined, tolerance, startTime, stopTimeDefined, stopTime);
  reinterpret_cast<fmikit::FMU2Slave*>(fmu2_slave)->enterInitializationMode();
  reinterpret_cast<fmikit::FMU2Slave*>(fmu2_slave)->exitInitializationMode();
  std::cout << "FMU2_Slave  setupExperiment done! " << std::endl;
}

void getRealFMU2_Slave(C_FMU2Slave fmu2_slave, int valueReference, double& val)
{
  if(verbosity) std::cout << "FMU2_Slave  getReal address: " << fmu2_slave << std::endl;
  if(verbosity) std::cout << "FMU2_Slave  getReal valueReference: " << valueReference << std::endl;
  val = reinterpret_cast<fmikit::FMU2Slave*>(fmu2_slave)->getReal(valueReference);
  if(verbosity) std::cout << "FMU2_Slave  getReal value: " << val << std::endl;
}

void setRealFMU2_Slave(C_FMU2Slave fmu2_slave, int valueReference, double val)
{
  std::cout << "FMU2_Slave setReal address: " << fmu2_slave << std::endl;
  std::cout << "FMU2_Slave setReal valueReference: " << valueReference << std::endl;
  std::cout << "FMU2_Slave setReal val: " << val << std::endl;
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

void doStepFMU2_Slave(C_FMU2Slave fmu2_slave, double h)
{
  if(verbosity) std::cout << "FMU2_Slave doStep address: " << fmu2_slave << std::endl;
  if(verbosity) std::cout << "FMU2_Slave doStep dt: " << h << std::endl;
  reinterpret_cast<fmikit::FMU2Slave*>(fmu2_slave)->doStep(h);
  if(verbosity) std::cout << "FMU2_Slave doStep done! " << std::endl;
}

void clearFMU2_Slave(C_FMU2Slave fmu2_slave)
{
  if(verbosity) std::cout << "FMU2_Slave clear address: " << fmu2_slave << std::endl;
  reinterpret_cast<fmikit::FMU2Slave*>(fmu2_slave)->~FMU2Slave();
  if(verbosity) std::cout << "FMU2_Slave clear done! " << std::endl;
}

void serializeStateFMU2_Slave(C_FMU2Slave fmu2_slave)
{
  if(verbosity) std::cout << "FMU2_Slave serializeState address: " << fmu2_slave << std::endl;
  reinterpret_cast<fmikit::FMU2Slave*>(fmu2_slave)->getStateSlave();
}

void deSerializeStateFMU2_Slave(C_FMU2Slave fmu2_slave){
  if(verbosity) std::cout << "FMU2_Slave deSerializeState address: " << fmu2_slave << std::endl;
  reinterpret_cast<fmikit::FMU2Slave*>(fmu2_slave)->loadStateSlave();
}
