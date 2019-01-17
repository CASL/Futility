# CMake generated Testfile for 
# Source directory: /home/nfherrin/research/MPACT/Futility/unit_tests/testGeom_Box
# Build directory: /home/nfherrin/research/MPACT/Futility/Futilitybuild/unit_tests/testGeom_Box
# 
# This file includes the relevant testing commands required for 
# testing this directory and lists subdirectories to be tested as well.
add_test(Futility_testGeom_Box "/home/nfherrin/research/MPACT/Futility/Futilitybuild/unit_tests/testGeom_Box/Futility_testGeom_Box.exe")
set_tests_properties(Futility_testGeom_Box PROPERTIES  FAIL_REGULAR_EXPRESSION "The following Teuchos::RCPNode objects were created" LABELS "Futility" PROCESSORS "1" REQUIRED_FILES "/home/nfherrin/research/MPACT/Futility/Futilitybuild/unit_tests/testGeom_Box/Futility_testGeom_Box.exe" TIMEOUT "1500")
