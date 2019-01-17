# CMake generated Testfile for 
# Source directory: /home/nfherrin/research/MPACT/Futility/unit_tests/testFileType_XML
# Build directory: /home/nfherrin/research/MPACT/Futility/Futilitybuild/unit_tests/testFileType_XML
# 
# This file includes the relevant testing commands required for 
# testing this directory and lists subdirectories to be tested as well.
add_test(Futility_testFileType_XML "/home/nfherrin/research/MPACT/Futility/Futilitybuild/unit_tests/testFileType_XML/Futility_testFileType_XML.exe")
set_tests_properties(Futility_testFileType_XML PROPERTIES  FAIL_REGULAR_EXPRESSION "The following Teuchos::RCPNode objects were created" LABELS "Futility" PROCESSORS "1" REQUIRED_FILES "/home/nfherrin/research/MPACT/Futility/Futilitybuild/unit_tests/testFileType_XML/Futility_testFileType_XML.exe" TIMEOUT "1500")
