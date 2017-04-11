# @HEADER
# ************************************************************************
#
#            Trilinos: An Object-Oriented Solver Framework
#                 Copyright (2001) Sandia Corporation
#
#
# Copyright (2001) Sandia Corporation. Under the terms of Contract
# DE-AC04-94AL85000, there is a non-exclusive license for use of this
# work by or on behalf of the U.S. Government.  Export of this program
# may require a license from the United States Government.
#
# 1. Redistributions of source code must retain the above copyright
# notice, this list of conditions and the following disclaimer.
#
# 2. Redistributions in binary form must reproduce the above copyright
# notice, this list of conditions and the following disclaimer in the
# documentation and/or other materials provided with the distribution.
#
# 3. Neither the name of the Corporation nor the names of the
# contributors may be used to endorse or promote products derived from
# this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY SANDIA CORPORATION "AS IS" AND ANY
# EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
# PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL SANDIA CORPORATION OR THE
# CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
# EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
# PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
# PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
# LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
# NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
# SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#
# NOTICE:  The United States Government is granted for itself and others
# acting on its behalf a paid-up, nonexclusive, irrevocable worldwide
# license in this data to reproduce, prepare derivative works, and
# perform publicly and display publicly.  Beginning five (5) years from
# July 25, 2001, the United States Government is granted for itself and
# others acting on its behalf a paid-up, nonexclusive, irrevocable
# worldwide license in this data to reproduce, prepare derivative works,
# distribute copies to the public, perform publicly and display
# publicly, and to permit others to do so.
#
# NEITHER THE UNITED STATES GOVERNMENT, NOR THE UNITED STATES DEPARTMENT
# OF ENERGY, NOR SANDIA CORPORATION, NOR ANY OF THEIR EMPLOYEES, MAKES
# ANY WARRANTY, EXPRESS OR IMPLIED, OR ASSUMES ANY LEGAL LIABILITY OR
# RESPONSIBILITY FOR THE ACCURACY, COMPLETENESS, OR USEFULNESS OF ANY
# INFORMATION, APPARATUS, PRODUCT, OR PROCESS DISCLOSED, OR REPRESENTS
# THAT ITS USE WOULD NOT INFRINGE PRIVATELY OWNED RIGHTS.
#
# ************************************************************************
# @HEADER


IF("${PETSC_INCLUDE_DIRS}" STREQUAL "" AND "${PETSC_LIBRARY_DIRS}" STREQUAL "") 
  GLOBAL_SET(PETSC_INCLUDE_DIRS "$ENV{PETSC_DIR}/include;$ENV{PETSC_DIR}/$ENV{PETSC_ARCH}/include")
  GLOBAL_SET(PETSC_LIBRARY_DIRS "$ENV{PETSC_DIR}/$ENV{PETSC_ARCH}/lib")
ENDIF()

TRIBITS_TPL_FIND_INCLUDE_DIRS_AND_LIBRARIES( PETSC
  REQUIRED_HEADERS petsc.h petscconf.h
  REQUIRED_LIBS_NAMES petsc flapack fblas
  )

## The rest of this script is dedicated to finding the correct linker flags

# Determine location of conf
IF(EXISTS "$ENV{PETSC_DIR}/include/petsc") # >= 3.6.0
  SET(PETSC_CONF_RULES "$ENV{PETSC_DIR}/lib/petsc/conf/rules")
  SET(PETSC_CONF_VARIABLES "$ENV{PETSC_DIR}/lib/petsc/conf/variables")
  SET(PETSC_CONF_PETSCVARIABLES "$ENV{PETSC_DIR}/lib/petsc/conf/petscvariables")
  INCLUDE_DIRECTORIES($ENV{PETSC_DIR}/include/petsc)
ELSEIF(EXISTS "$ENV{PETSC_DIR}/$ENV{PETSC_ARCH}/include/petsc") # >= 3.6.0
  SET(PETSC_CONF_RULES "$ENV{PETSC_DIR}/lib/petsc/conf/rules")
  SET(PETSC_CONF_VARIABLES "$ENV{PETSC_DIR}/lib/petsc/conf/variables")
  SET(PETSC_CONF_PETSCVARIABLES "$ENV{PETSC_DIR}/lib/petsc/conf/petscvariables")
  INCLUDE_DIRECTORIES($ENV{PETSC_DIR}/$ENV{PETSC_ARCH}/include/petsc)
ELSEIF(EXISTS "$ENV{PETSC_DIR}/include/petscconf.h")   # > 2.3.3
  SET(PETSC_CONF_RULES "$ENV{PETSC_DIR}/conf/rules")
  SET(PETSC_CONF_VARIABLES "$ENV{PETSC_DIR}/conf/variables")
  SET(PETSC_CONF_PETSCVARIABLES "$ENV{PETSC_DIR}/conf/petscvariables")
ELSEIF(EXISTS "$ENV{PETSC_DIR}/$ENV{PETSC_ARCH}/include/petscconf.h")   # > 2.3.3
  SET(PETSC_CONF_RULES "$ENV{PETSC_DIR}/conf/rules")
  SET(PETSC_CONF_VARIABLES "$ENV{PETSC_DIR}/conf/variables")
  SET(PETSC_CONF_PETSCVARIABLES "$ENV{PETSC_DIR}/$ENV{PETSC_ARCH}/conf/petscvariables")
ELSEIF(EXISTS "$ENV{PETSC_DIR}/bmake/$ENV{PETSC_ARCH}/petscconf.h") # <= 2.3.3
  SET(PETSC_CONF_RULES "$ENV{PETSC_DIR}/bmake/common/rules")
  SET(PETSC_CONF_VARIABLES "$ENV{PETSC_DIR}/bmake/common/variables")
#ELSEIF(PETSC_DIR)
#  MESSAGE(SEND_ERROR "The pair PETSC_DIR=$ENV{PETSC_DIR} PETSC_ARCH=$ENV{PETSC_ARCH} do not specify a valid PETSc installation")
ENDIF()

IF(PETSC_CONF_RULES AND PETSC_CONF_VARIABLES)
  # A temporary makefile to probe the PETSc configuration
  SET(petsc_config_makefile "${PROJECT_BINARY_DIR}/Makefile.petsc")
  FILE(WRITE "${petsc_config_makefile}"
"## This file was autogenerated by FindPETSc.cmake
include ${PETSC_CONF_PETSCVARIABLES}
show :
	-@echo -n \${\${VARIABLE}}
")

  # Pull data from the temp Makefile
  FIND_PROGRAM(MAKE_EXECUTABLE NAMES make gmake)
  MACRO(PETSC_GET_VARIABLE name var)
    SET(${var} "NOTFOUND" CACHE INTERNAL "Cleared" FORCE)
    EXECUTE_PROCESS(COMMAND ${MAKE_EXECUTABLE} --no-print-directory -f ${petsc_config_makefile} show VARIABLE=${name}
      OUTPUT_VARIABLE ${var}
      RESULT_VARIABLE petsc_return)
  ENDMACRO(PETSC_GET_VARIABLE)
  petsc_get_variable (PETSC_LIB                PETSC_LINKER_VAR)
  file (REMOVE ${petsc_config_makefile})
ENDIF()

#add linker variables
LINK_LIBRARIES(${PETSC_LINKER_VAR})
