# CMake generated Testfile for 
# Source directory: /home/nfherrin/research/MPACT/Futility/unit_tests/testFileType_Checkpoint
# Build directory: /home/nfherrin/research/MPACT/Futility/Futilitybuild/unit_tests/testFileType_Checkpoint
# 
# This file includes the relevant testing commands required for 
# testing this directory and lists subdirectories to be tested as well.
add_test(Futility_testFileType_Checkpoint "/home/nfherrin/research/MPACT/Futility/Futilitybuild/unit_tests/testFileType_Checkpoint/Futility_testFileType_Checkpoint.exe")
set_tests_properties(Futility_testFileType_Checkpoint PROPERTIES  FAIL_REGULAR_EXPRESSION "The following Teuchos::RCPNode objects were created" LABELS "Futility" PROCESSORS "1" REQUIRED_FILES "/home/nfherrin/research/MPACT/Futility/Futilitybuild/unit_tests/testFileType_Checkpoint/Futility_testFileType_Checkpoint.exe" TIMEOUT "1500")
