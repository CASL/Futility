# CMake generated Testfile for 
# Source directory: /home/nfherrin/research/MPACT/Futility/unit_tests/testExtendedMath
# Build directory: /home/nfherrin/research/MPACT/Futility/Futilitybuild/unit_tests/testExtendedMath
# 
# This file includes the relevant testing commands required for 
# testing this directory and lists subdirectories to be tested as well.
add_test(Futility_testExtendedMath "/home/nfherrin/research/MPACT/Futility/Futilitybuild/unit_tests/testExtendedMath/Futility_testExtendedMath.exe")
set_tests_properties(Futility_testExtendedMath PROPERTIES  FAIL_REGULAR_EXPRESSION "The following Teuchos::RCPNode objects were created" LABELS "Futility" PROCESSORS "1" REQUIRED_FILES "/home/nfherrin/research/MPACT/Futility/Futilitybuild/unit_tests/testExtendedMath/Futility_testExtendedMath.exe" TIMEOUT "1500")
