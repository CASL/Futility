# CMake generated Testfile for 
# Source directory: /home/nfherrin/research/MPACT/Futility/unit_tests/testDBC
# Build directory: /home/nfherrin/research/MPACT/Futility/Futilitybuild/unit_tests/testDBC
# 
# This file includes the relevant testing commands required for 
# testing this directory and lists subdirectories to be tested as well.
add_test(Futility_testDBC "/home/nfherrin/research/env/gcc-5.4.0/common_tools/cmake-3.10.2/bin/cmake" "-DTEST_CONFIG=${CTEST_CONFIGURATION_TYPE}" "-P" "/home/nfherrin/research/MPACT/Futility/Futilitybuild/unit_tests/testDBC/Futility_testDBC.cmake")
set_tests_properties(Futility_testDBC PROPERTIES  LABELS "Futility" PASS_REGULAR_EXPRESSION "OVERALL FINAL RESULT: TEST PASSED .Futility_testDBC." PROCESSORS "1" REQUIRED_FILES "/home/nfherrin/research/MPACT/Futility/Futilitybuild/unit_tests/testDBC/testDBC.exe" TIMEOUT "1500")
