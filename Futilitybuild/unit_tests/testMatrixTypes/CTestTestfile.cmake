# CMake generated Testfile for 
# Source directory: /home/nfherrin/research/MPACT/Futility/unit_tests/testMatrixTypes
# Build directory: /home/nfherrin/research/MPACT/Futility/Futilitybuild/unit_tests/testMatrixTypes
# 
# This file includes the relevant testing commands required for 
# testing this directory and lists subdirectories to be tested as well.
add_test(Futility_testMatrixTypes "/home/nfherrin/research/MPACT/Futility/Futilitybuild/unit_tests/testMatrixTypes/Futility_testMatrixTypes.exe")
set_tests_properties(Futility_testMatrixTypes PROPERTIES  FAIL_REGULAR_EXPRESSION "The following Teuchos::RCPNode objects were created" LABELS "Futility" PROCESSORS "1" REQUIRED_FILES "/home/nfherrin/research/MPACT/Futility/Futilitybuild/unit_tests/testMatrixTypes/Futility_testMatrixTypes.exe" TIMEOUT "1500")
add_test(Futility_testMatrixTypesParallel "/home/nfherrin/research/MPACT/Futility/Futilitybuild/unit_tests/testMatrixTypes/Futility_testMatrixTypesParallel.exe")
set_tests_properties(Futility_testMatrixTypesParallel PROPERTIES  FAIL_REGULAR_EXPRESSION "The following Teuchos::RCPNode objects were created" LABELS "Futility" PROCESSORS "-1" REQUIRED_FILES "/home/nfherrin/research/MPACT/Futility/Futilitybuild/unit_tests/testMatrixTypes/Futility_testMatrixTypesParallel.exe" TIMEOUT "1500")
