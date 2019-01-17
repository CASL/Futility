# CMake generated Testfile for 
# Source directory: /home/nfherrin/research/MPACT/Futility/unit_tests/testVTUFiles
# Build directory: /home/nfherrin/research/MPACT/Futility/Futilitybuild/unit_tests/testVTUFiles
# 
# This file includes the relevant testing commands required for 
# testing this directory and lists subdirectories to be tested as well.
add_test(Futility_testVTUFiles "/home/nfherrin/research/MPACT/Futility/Futilitybuild/unit_tests/testVTUFiles/Futility_testVTUFiles.exe")
set_tests_properties(Futility_testVTUFiles PROPERTIES  FAIL_REGULAR_EXPRESSION "The following Teuchos::RCPNode objects were created" LABELS "Futility" PROCESSORS "1" REQUIRED_FILES "/home/nfherrin/research/MPACT/Futility/Futilitybuild/unit_tests/testVTUFiles/Futility_testVTUFiles.exe" TIMEOUT "1500")
