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
void InitilizeFMU2_Slave(void* slave_ptr, int slave_id, char* guid, char* modelIdentifier, char* unzipDirectory, char* instanceName)
{
  std::string guid_str(guid);
  std::string modelIdentifier_str(modelIdentifier);
  std::string unzipDirectory_str(unzipDirectory);
  std::string instanceName_str(instanceName);
  std::cout << "FMU2_Slave " << slave_id << " init ..." << std::endl;
  std::cout << "FMU2_Slave guid: " << guid_str << std::endl;
  std::cout << "FMU2_Slave modelIdentifier: " << modelIdentifier_str << std::endl;
  std::cout << "FMU2_Slave unzipDirectory: " << unzipDirectory_str << std::endl;
  std::cout << "FMU2_Slave instanceName: " << instanceName_str << std::endl;
  slave_ptr = reinterpret_cast<void*>(new fmikit::FMU2Slave(guid, modelIdentifier, unzipDirectory, instanceName));
  std::cout << "FMU2_Slave: " << slave_id << " init done!" << std::endl;
}

void doStepFMU2_Slave(C_FMU2Slave fmu2_slave, double h)
{
  std::cout << "FMU2_Slave Takeing step: " << h << std::endl;
  reinterpret_cast<fmikit::FMU2Slave*>(fmu2_slave)->doStep(h);
}
