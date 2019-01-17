# CMake generated Testfile for 
# Source directory: /home/nfherrin/research/MPACT/Futility/unit_tests/testLinearSolver_Multigrid
# Build directory: /home/nfherrin/research/MPACT/Futility/Futilitybuild/unit_tests/testLinearSolver_Multigrid
# 
# This file includes the relevant testing commands required for 
# testing this directory and lists subdirectories to be tested as well.
add_test(Futility_testLinearSolver_Multigrid "/home/nfherrin/research/MPACT/Futility/Futilitybuild/unit_tests/testLinearSolver_Multigrid/Futility_testLinearSolver_Multigrid.exe")
set_tests_properties(Futility_testLinearSolver_Multigrid PROPERTIES  FAIL_REGULAR_EXPRESSION "The following Teuchos::RCPNode objects were created" LABELS "Futility" PROCESSORS "-1" REQUIRED_FILES "/home/nfherrin/research/MPACT/Futility/Futilitybuild/unit_tests/testLinearSolver_Multigrid/Futility_testLinearSolver_Multigrid.exe" TIMEOUT "1500")
