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
#include <stdlib.h>
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
  free(fmu2_base);
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

// Methods for Model Exchange only
void setTimeFMU2_Model(C_FMU2Model fmu2_model, double t){
  reinterpret_cast<fmikit::FMU2Model*>(fmu2_model)->setTime(t);
}

void enterEventModeFMU2_Model(C_FMU2Model fmu2_model){
  reinterpret_cast<fmikit::FMU2Model*>(fmu2_model)->enterEventMode();
}

void enterContinuousTimeModeFMU2_Model(C_FMU2Model fmu2_model){
  reinterpret_cast<fmikit::FMU2Model*>(fmu2_model)->enterContinuousTimeMode();
}

void getDerivativesFMU2_Model(C_FMU2Model fmu2_model, double derivatives[], size_t nx){
  reinterpret_cast<fmikit::FMU2Model*>(fmu2_model)->getDerivatives(derivatives, nx);
}

void completedIntegratorStepFMU2_Model(C_FMU2Model fmu2_model, bool& completed_step){
  completed_step = reinterpret_cast<fmikit::FMU2Model*>(fmu2_model)->completedIntegratorStep();
}

void getContinuousStatesFMU2_Model(C_FMU2Model fmu2_model, double x[], size_t nx){
  reinterpret_cast<fmikit::FMU2Model*>(fmu2_model)->getContinuousStates(x, nx);
}

void setContinuousStatesFMU2_Model(C_FMU2Model fmu2_model, const double x[], size_t nx){
  reinterpret_cast<fmikit::FMU2Model*>(fmu2_model)->setContinuousStates(x, nx);
}

void getEventIndicatorsFMU2_Model(C_FMU2Model fmu2_model, double eventIndicators[], size_t ni){
  reinterpret_cast<fmikit::FMU2Model*>(fmu2_model)->getEventIndicators(eventIndicators, ni);
}
