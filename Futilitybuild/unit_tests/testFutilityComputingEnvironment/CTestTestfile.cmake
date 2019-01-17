# CMake generated Testfile for 
# Source directory: /home/nfherrin/research/MPACT/Futility/unit_tests/testFutilityComputingEnvironment
# Build directory: /home/nfherrin/research/MPACT/Futility/Futilitybuild/unit_tests/testFutilityComputingEnvironment
# 
# This file includes the relevant testing commands required for 
# testing this directory and lists subdirectories to be tested as well.
add_test(Futility_testFutilityComputingEnvironment "/home/nfherrin/research/MPACT/Futility/Futilitybuild/unit_tests/testFutilityComputingEnvironment/Futility_testFutilityComputingEnvironment.exe")
set_tests_properties(Futility_testFutilityComputingEnvironment PROPERTIES  FAIL_REGULAR_EXPRESSION "The following Teuchos::RCPNode objects were created" LABELS "Futility" PROCESSORS "1" REQUIRED_FILES "/home/nfherrin/research/MPACT/Futility/Futilitybuild/unit_tests/testFutilityComputingEnvironment/Futility_testFutilityComputingEnvironment.exe" TIMEOUT "1500")
