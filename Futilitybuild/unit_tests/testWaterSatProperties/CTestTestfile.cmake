# CMake generated Testfile for 
# Source directory: /home/nfherrin/research/MPACT/Futility/unit_tests/testWaterSatProperties
# Build directory: /home/nfherrin/research/MPACT/Futility/Futilitybuild/unit_tests/testWaterSatProperties
# 
# This file includes the relevant testing commands required for 
# testing this directory and lists subdirectories to be tested as well.
add_test(Futility_testWaterSatProperties "/home/nfherrin/research/MPACT/Futility/Futilitybuild/unit_tests/testWaterSatProperties/Futility_testWaterSatProperties.exe")
set_tests_properties(Futility_testWaterSatProperties PROPERTIES  FAIL_REGULAR_EXPRESSION "The following Teuchos::RCPNode objects were created" LABELS "Futility" PROCESSORS "1" REQUIRED_FILES "/home/nfherrin/research/MPACT/Futility/Futilitybuild/unit_tests/testWaterSatProperties/Futility_testWaterSatProperties.exe" TIMEOUT "1500")
