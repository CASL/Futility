# CMake generated Testfile for 
# Source directory: /home/nfherrin/research/MPACT/Futility/unit_tests/testBLAS
# Build directory: /home/nfherrin/research/MPACT/Futility/Futilitybuild/unit_tests/testBLAS
# 
# This file includes the relevant testing commands required for 
# testing this directory and lists subdirectories to be tested as well.
add_test(Futility_testBLAS "/home/nfherrin/research/MPACT/Futility/Futilitybuild/unit_tests/testBLAS/Futility_testBLAS.exe")
set_tests_properties(Futility_testBLAS PROPERTIES  FAIL_REGULAR_EXPRESSION "The following Teuchos::RCPNode objects were created" LABELS "Futility" PROCESSORS "1" REQUIRED_FILES "/home/nfherrin/research/MPACT/Futility/Futilitybuild/unit_tests/testBLAS/Futility_testBLAS.exe" TIMEOUT "1500")
