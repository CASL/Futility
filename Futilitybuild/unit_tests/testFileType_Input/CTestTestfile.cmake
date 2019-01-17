# CMake generated Testfile for 
# Source directory: /home/nfherrin/research/MPACT/Futility/unit_tests/testFileType_Input
# Build directory: /home/nfherrin/research/MPACT/Futility/Futilitybuild/unit_tests/testFileType_Input
# 
# This file includes the relevant testing commands required for 
# testing this directory and lists subdirectories to be tested as well.
add_test(Futility_testFileType_Input "/home/nfherrin/research/MPACT/Futility/Futilitybuild/unit_tests/testFileType_Input/Futility_testFileType_Input.exe")
set_tests_properties(Futility_testFileType_Input PROPERTIES  FAIL_REGULAR_EXPRESSION "The following Teuchos::RCPNode objects were created" LABELS "Futility" PROCESSORS "1" REQUIRED_FILES "/home/nfherrin/research/MPACT/Futility/Futilitybuild/unit_tests/testFileType_Input/Futility_testFileType_Input.exe" TIMEOUT "1500")
