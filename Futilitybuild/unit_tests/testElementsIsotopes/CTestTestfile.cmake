# CMake generated Testfile for 
# Source directory: /home/nfherrin/research/MPACT/Futility/unit_tests/testElementsIsotopes
# Build directory: /home/nfherrin/research/MPACT/Futility/Futilitybuild/unit_tests/testElementsIsotopes
# 
# This file includes the relevant testing commands required for 
# testing this directory and lists subdirectories to be tested as well.
add_test(Futility_testElementsIsotopes "/home/nfherrin/research/MPACT/Futility/Futilitybuild/unit_tests/testElementsIsotopes/Futility_testElementsIsotopes.exe")
set_tests_properties(Futility_testElementsIsotopes PROPERTIES  FAIL_REGULAR_EXPRESSION "The following Teuchos::RCPNode objects were created" LABELS "Futility" PROCESSORS "1" REQUIRED_FILES "/home/nfherrin/research/MPACT/Futility/Futilitybuild/unit_tests/testElementsIsotopes/Futility_testElementsIsotopes.exe" TIMEOUT "1500")
