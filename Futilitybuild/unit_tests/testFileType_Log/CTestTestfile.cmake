# CMake generated Testfile for 
# Source directory: /home/nfherrin/research/MPACT/Futility/unit_tests/testFileType_Log
# Build directory: /home/nfherrin/research/MPACT/Futility/Futilitybuild/unit_tests/testFileType_Log
# 
# This file includes the relevant testing commands required for 
# testing this directory and lists subdirectories to be tested as well.
add_test(Futility_testFileType_Log "/home/nfherrin/research/MPACT/Futility/Futilitybuild/unit_tests/testFileType_Log/Futility_testFileType_Log.exe")
set_tests_properties(Futility_testFileType_Log PROPERTIES  FAIL_REGULAR_EXPRESSION "The following Teuchos::RCPNode objects were created" LABELS "Futility" PROCESSORS "1" REQUIRED_FILES "/home/nfherrin/research/MPACT/Futility/Futilitybuild/unit_tests/testFileType_Log/Futility_testFileType_Log.exe" TIMEOUT "1500")
