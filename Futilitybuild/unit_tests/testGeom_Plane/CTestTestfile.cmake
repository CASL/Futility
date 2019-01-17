# CMake generated Testfile for 
# Source directory: /home/nfherrin/research/MPACT/Futility/unit_tests/testGeom_Plane
# Build directory: /home/nfherrin/research/MPACT/Futility/Futilitybuild/unit_tests/testGeom_Plane
# 
# This file includes the relevant testing commands required for 
# testing this directory and lists subdirectories to be tested as well.
add_test(Futility_testGeom_Plane "/home/nfherrin/research/MPACT/Futility/Futilitybuild/unit_tests/testGeom_Plane/Futility_testGeom_Plane.exe")
set_tests_properties(Futility_testGeom_Plane PROPERTIES  FAIL_REGULAR_EXPRESSION "The following Teuchos::RCPNode objects were created" LABELS "Futility" PROCESSORS "1" REQUIRED_FILES "/home/nfherrin/research/MPACT/Futility/Futilitybuild/unit_tests/testGeom_Plane/Futility_testGeom_Plane.exe" TIMEOUT "1500")
