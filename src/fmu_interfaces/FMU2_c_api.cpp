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

//void InitilizeFMU2_Slave(void* slave_ptr, int slave_id, const std::string &guid, const std::string &modelIdentifier, const std::string &unzipDirectory, const std::string &instanceName)
C_FMU2Slave InitilizeFMU2_Slave(int slave_id, char* guid, char* modelIdentifier, char* unzipDirectory, char* instanceName)
{
  std::string guid_str(guid);
  std::string modelIdentifier_str(modelIdentifier);
  std::string unzipDirectory_str(unzipDirectory);
  std::string instanceName_str(instanceName);
  std::cout << "FMU2_Slave Init guid: " << guid_str << std::endl;
  std::cout << "FMU2_Slave Init modelIdentifier: " << modelIdentifier_str << std::endl;
  std::cout << "FMU2_Slave Init unzipDirectory: " << unzipDirectory_str << std::endl;
  std::cout << "FMU2_Slave Init instanceName: " << instanceName_str << std::endl;
  // return reinterpret_cast<void*>(new fmikit::FMU2Slave(guid, modelIdentifier, unzipDirectory, instanceName));
  fmikit::FMU2Slave *s = new fmikit::FMU2Slave(guid, modelIdentifier, unzipDirectory, instanceName);
  std::cout << "FMU2_Slave Init ptr: " << s << std::endl;
  std::cout << "FMU2_Slave Init id: " << slave_id << " init done!" << std::endl;
  return (C_FMU2Slave)s;
}

void setupExperimentFMU2_Slave(C_FMU2Slave fmu2_slave, bool toleranceDefined, double tolerance, double startTime, double stopTimeDefined, double stopTime)
{
  reinterpret_cast<fmikit::FMU2Slave*>(fmu2_slave)->setupExperiment(toleranceDefined, tolerance, startTime, stopTimeDefined, stopTime);
}

void getRealFMU2_Slave(C_FMU2Slave fmu2_slave, int valueReference, double& val)
{
  std::cout << "FMU2_Slave  getReal address: " << fmu2_slave << std::endl;
  std::cout << "FMU2_Slave  getReal valueReference: " << valueReference << std::endl;
  val = reinterpret_cast<fmikit::FMU2Slave*>(fmu2_slave)->getReal(valueReference);
  std::cout << "FMU2_Slave getReal value" << val << std::endl;
}

void setRealFMU2_Slave(C_FMU2Slave fmu2_slave, int valueReference, double val)
{
  reinterpret_cast<fmikit::FMU2Slave*>(fmu2_slave)->setReal(valueReference, val);
}

void doStepFMU2_Slave(C_FMU2Slave fmu2_slave, double h)
{
  std::cout << "FMU2_Slave address: " << fmu2_slave << std::endl;
  std::cout << "FMU2_Slave Takeing step: " << h << std::endl;
  reinterpret_cast<fmikit::FMU2Slave*>(fmu2_slave)->doStep(h);
}
