# CMake generated Testfile for 
# Source directory: /home/nfherrin/research/MPACT/Futility/unit_tests/testFileType_Base
# Build directory: /home/nfherrin/research/MPACT/Futility/Futilitybuild/unit_tests/testFileType_Base
# 
# This file includes the relevant testing commands required for 
# testing this directory and lists subdirectories to be tested as well.
add_test(Futility_testFileType_Base "/home/nfherrin/research/MPACT/Futility/Futilitybuild/unit_tests/testFileType_Base/Futility_testFileType_Base.exe")
set_tests_properties(Futility_testFileType_Base PROPERTIES  FAIL_REGULAR_EXPRESSION "The following Teuchos::RCPNode objects were created" LABELS "Futility" PROCESSORS "1" REQUIRED_FILES "/home/nfherrin/research/MPACT/Futility/Futilitybuild/unit_tests/testFileType_Base/Futility_testFileType_Base.exe" TIMEOUT "1500")
