#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
#                          Futility Development Group                          !
#                             All rights reserved.                             !
#                                                                              !
# Futility is a jointly-maintained, open-source project between the University !
# of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
# can be found in LICENSE.txt in the head directory of this repository.        !
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
INCLUDE(Futility_CreateUnitTest)
INCLUDE(Futility_CopyFiles)

SET(ADDITIONAL_FILES
  subtest01/testA.inp
  subtest01/subtest01.tex.gold
  subtest02/testA.inp
  subtest02/testB.inp
  subtest02/subtest02_a.tex.gold
  subtest02/subtest02_b.tex.gold
  subtest03/testA.inp
  subtest03/testC.f90
  subtest03/subtest03.tex.gold
  subtest04/testA.inp
  subtest04/testC.f90
  subtest04/testD.txt
  subtest04/subtest04.tex.gold
  subtest05/testA.inp
  subtest05/testC.f90
  subtest05/testD.txt
  subtest05/subtest05.tex.gold
  subtest06/testA.inp
  subtest06/testB.inp
  subtest06/subtest06_a.tex.gold
  subtest06/subtest06_b.tex.gold
  subtest07/testA.inp
  subtest07/subtest07.tex.gold
  subtest08/testA.inp
  subtest08/subtest08.tex.gold
  subtest09/testA.inp
  subtest09/testB.f90
  subtest09/subtest09.tex.gold
  subtest10/testA.inp
)
Futility_CopyFiles(ADDITIONAL_FILES)
UNSET(ADDITIONAL_FILES)

ASSERT_DEFINED(Futility_SOURCE_DIR)
SET(UTILPATH ${Futility_SOURCE_DIR}/cmake/GenerateRequirementsTable.py)

SET(TESTNAME testGenReqTables)
TRIBITS_ADD_ADVANCED_TEST(${TESTNAME}
  FAIL_FAST
  TIMEOUT 120
  ADDED_TEST_NAME_OUT TESTNAME_OUT
  OVERALL_NUM_TOTAL_CORES_USED 1

  #Subtest01
  TEST_0
    CMND python
    ARGS ${UTILPATH} --path subtest01/ --ext inp
  TEST_1
    CMND diff
    ARGS requirements.tex subtest01/subtest01.tex.gold

  #Subtest02
  TEST_2
    CMND python
    ARGS ${UTILPATH} --path subtest02/ --ext .inp --output outputman
  TEST_3
    CMND diff
    ARGS -s subtest02/subtest02_a.tex.gold subtest02/subtest02_b.tex.gold --to-file=outputman.tex
    PASS_REGULAR_EXPRESSION "are identical"

  #Subtest03
  TEST_4
    CMND python
    ARGS ${UTILPATH} --path subtest03/ --ext .inp --ext .f90
  TEST_5
    CMND diff
    ARGS requirements.tex subtest03/subtest03.tex.gold

  #Subtest04
  TEST_6
    CMND python
    ARGS ${UTILPATH} --path subtest04/ --ext .inp --ext .f90 --ext .txt
  TEST_7
    CMND diff
    ARGS requirements.tex subtest04/subtest04.tex.gold

  #Subtest05
  TEST_8
    CMND python
    ARGS ${UTILPATH} --path subtest05/ --ext .inp --ext .f90 --ext .txt --skip-no-require
  TEST_9
    CMND diff
    ARGS requirements.tex subtest05/subtest05.tex.gold

  #Subtest06
  TEST_10
    CMND python
    ARGS ${UTILPATH} --path subtest06/ --ext .inp
  TEST_11
    CMND diff
    ARGS -s subtest06/subtest06_a.tex.gold subtest06/subtest06_b.tex.gold --to-file=requirements.tex
    PASS_REGULAR_EXPRESSION "are identical"

  #Subtest07
  TEST_12
    CMND python
    ARGS ${UTILPATH} --path subtest07/ --ext .inp
  TEST_13
    CMND diff
    ARGS requirements.tex subtest07/subtest07.tex.gold

  #Subtest08
  TEST_14
    CMND python
    ARGS ${UTILPATH} --path subtest08/ --ext .inp
  TEST_15
    CMND diff
    ARGS requirements.tex subtest08/subtest08.tex.gold

  #Subtest9
  TEST_16
    CMND python
    ARGS ${UTILPATH} --path subtest09/ --ext .inp --ext .f90
  TEST_17
    CMND diff
    ARGS requirements.tex subtest09/subtest09.tex.gold

  #Subtest10
  TEST_18
    CMND python
    ARGS ${UTILPATH} --path subtest10/ --ext .inp
    PASS_REGULAR_EXPRESSION "@beginreq was found but @endreq was not found in file"
)
FUTILITY_POST_ADD_TEST(${TESTNAME} ${TESTNAME_OUT} "BASIC")

UNSET(UTILPATH)