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

void InitilizeFMU2_Slave(void* slave_ptr, int slave_id)
{
  std::cout << "FMU2_Slave " << slave_id << " init done" << std::endl;
  const std::string guid = "abcde";
  const std::string modelIdentifier = "abcde";
  const std::string unzipDirectory = "abcde";
  const std::string instanceName = "abcde";
  slave_ptr = reinterpret_cast<void*>(new fmikit::FMU2Slave(guid, modelIdentifier, unzipDirectory, instanceName));
}

void doStepFMU2_Slave(C_FMU2Slave fmu2_slave, double h)
{
  std::cout << "Takeing step " << h << std::endl;
  reinterpret_cast<fmikit::FMU2Slave*>(fmu2_slave)->doStep(h);
}
