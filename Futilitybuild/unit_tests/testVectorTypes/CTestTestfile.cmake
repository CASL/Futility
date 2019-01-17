# CMake generated Testfile for 
# Source directory: /home/nfherrin/research/MPACT/Futility/unit_tests/testVectorTypes
# Build directory: /home/nfherrin/research/MPACT/Futility/Futilitybuild/unit_tests/testVectorTypes
# 
# This file includes the relevant testing commands required for 
# testing this directory and lists subdirectories to be tested as well.
add_test(Futility_testVectorTypes "/home/nfherrin/research/MPACT/Futility/Futilitybuild/unit_tests/testVectorTypes/Futility_testVectorTypes.exe")
set_tests_properties(Futility_testVectorTypes PROPERTIES  FAIL_REGULAR_EXPRESSION "The following Teuchos::RCPNode objects were created" LABELS "Futility" PROCESSORS "1" REQUIRED_FILES "/home/nfherrin/research/MPACT/Futility/Futilitybuild/unit_tests/testVectorTypes/Futility_testVectorTypes.exe" TIMEOUT "1500")
add_test(Futility_testVectorTypesParallel "/home/nfherrin/research/MPACT/Futility/Futilitybuild/unit_tests/testVectorTypes/Futility_testVectorTypesParallel.exe")
set_tests_properties(Futility_testVectorTypesParallel PROPERTIES  FAIL_REGULAR_EXPRESSION "The following Teuchos::RCPNode objects were created" LABELS "Futility" PROCESSORS "-1" REQUIRED_FILES "/home/nfherrin/research/MPACT/Futility/Futilitybuild/unit_tests/testVectorTypes/Futility_testVectorTypesParallel.exe" TIMEOUT "1500")
