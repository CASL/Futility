# CMake generated Testfile for 
# Source directory: /home/nfherrin/research/MPACT/Futility/unit_tests/testGeom_CircCyl
# Build directory: /home/nfherrin/research/MPACT/Futility/Futilitybuild/unit_tests/testGeom_CircCyl
# 
# This file includes the relevant testing commands required for 
# testing this directory and lists subdirectories to be tested as well.
add_test(Futility_testGeom_CircCyl "/home/nfherrin/research/MPACT/Futility/Futilitybuild/unit_tests/testGeom_CircCyl/Futility_testGeom_CircCyl.exe")
set_tests_properties(Futility_testGeom_CircCyl PROPERTIES  FAIL_REGULAR_EXPRESSION "The following Teuchos::RCPNode objects were created" LABELS "Futility" PROCESSORS "1" REQUIRED_FILES "/home/nfherrin/research/MPACT/Futility/Futilitybuild/unit_tests/testGeom_CircCyl/Futility_testGeom_CircCyl.exe" TIMEOUT "1500")
