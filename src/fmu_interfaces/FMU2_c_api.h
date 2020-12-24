/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
/                          Futility Development Group                          !
/                             All rights reserved.                             !
/                                                                              !
/ Futility is a jointly-maintained, open-source project between the University !
/ of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
/ can be found in LICENSE.txt in the head directory of this repository.        !
/+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/
#pragma once

typedef void* C_FMU2Base;
typedef void* C_FMU2Slave;
typedef void* C_FMU2Model;

// Common FMU functions
extern "C" C_FMU2Base InitilizeFMU2_Base(int id, char* guid, char* modelIdentifier, char* unzipDirectory, char* instanceName);

extern "C" void clearFMU2_Base(C_FMU2Base fmu2_base);

extern "C" void setupExperimentFMU2_Base(C_FMU2Base fmu2_base, bool toleranceDefined, double tolerance, double startTime, bool stopTimeDefined, double stopTime, bool finalizeInitialization);

extern "C" void enterInitializationModeFMU2_Base(C_FMU2Base fmu2_base);

extern "C" void exitInitializationModeFMU2_Base(C_FMU2Base fmu2_base);

extern "C" void getRealFMU2_Base(C_FMU2Base fmu2_base, int valueReference, double& val);

extern "C" void setRealFMU2_Base(C_FMU2Base fmu2_base, int valueReference, double val);

extern "C" void getIntegerFMU2_Base(C_FMU2Base fmu2_base, int valueReference, int& val);

extern "C" void setIntegerFMU2_Base(C_FMU2Base fmu2_base, int valueReference, int val);

extern "C" void getBooleanFMU2_Base(C_FMU2Base fmu2_base, bool valueReference, bool& val);

extern "C" void setBooleanFMU2_Base(C_FMU2Base fmu2_base, bool valueReference, bool val);

// Methods for Co-Simulation FMUs only
extern "C" void setNoRewindFlagFMU2_Slave(C_FMU2Slave fmu2_slave, bool noRw);

extern "C" void doStepFMU2_Slave(C_FMU2Slave fmu2_slave, double h);

extern "C" void serializeStateFMU2_Slave(C_FMU2Slave fmu2_slave);

extern "C" void deSerializeStateFMU2_Slave(C_FMU2Slave fmu2_slave);

// Methods for Model Exchange FMUs only
extern "C" void setTimeFMU2_Model(C_FMU2Model fmu2_model, double t);

extern "C" void enterEventModeFMU2_Model(C_FMU2Model fmu2_model);

extern "C" void getDerivativesFMU2_Model(C_FMU2Model fmu2_model, double derivatives[], size_t nx);

extern "C" void completedIntegratorStepFMU2_Model(C_FMU2Model fmu2_model, bool& completed_step);

extern "C" void getContinuousStatesFMU2_Model(C_FMU2Model fmu2_model, double x[], size_t nx);

extern "C" void setContinuousStatesFMU2_Model(C_FMU2Model fmu2_model, const double x[], size_t nx);

extern "C" void getEventIndicatorsFMU2_Model(C_FMU2Model fmu2_model, double eventIndicators[], size_t ni);
