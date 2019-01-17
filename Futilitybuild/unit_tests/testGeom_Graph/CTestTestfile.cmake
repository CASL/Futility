# CMake generated Testfile for 
# Source directory: /home/nfherrin/research/MPACT/Futility/unit_tests/testGeom_Graph
# Build directory: /home/nfherrin/research/MPACT/Futility/Futilitybuild/unit_tests/testGeom_Graph
# 
# This file includes the relevant testing commands required for 
# testing this directory and lists subdirectories to be tested as well.
add_test(Futility_testGeom_Graph "/home/nfherrin/research/MPACT/Futility/Futilitybuild/unit_tests/testGeom_Graph/Futility_testGeom_Graph.exe")
set_tests_properties(Futility_testGeom_Graph PROPERTIES  FAIL_REGULAR_EXPRESSION "The following Teuchos::RCPNode objects were created" LABELS "Futility" PROCESSORS "1" REQUIRED_FILES "/home/nfherrin/research/MPACT/Futility/Futilitybuild/unit_tests/testGeom_Graph/Futility_testGeom_Graph.exe" TIMEOUT "1500")
