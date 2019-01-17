# CMake generated Testfile for 
# Source directory: /home/nfherrin/research/MPACT/Futility/unit_tests/testGeom_Poly
# Build directory: /home/nfherrin/research/MPACT/Futility/Futilitybuild/unit_tests/testGeom_Poly
# 
# This file includes the relevant testing commands required for 
# testing this directory and lists subdirectories to be tested as well.
add_test(Futility_testGeom_Poly "/home/nfherrin/research/MPACT/Futility/Futilitybuild/unit_tests/testGeom_Poly/Futility_testGeom_Poly.exe")
set_tests_properties(Futility_testGeom_Poly PROPERTIES  FAIL_REGULAR_EXPRESSION "The following Teuchos::RCPNode objects were created" LABELS "Futility" PROCESSORS "1" REQUIRED_FILES "/home/nfherrin/research/MPACT/Futility/Futilitybuild/unit_tests/testGeom_Poly/Futility_testGeom_Poly.exe" TIMEOUT "1500")
