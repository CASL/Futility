classFile = open('parameterListClasses.f90','w')
initsFile = open('parameterListInitMethods.f90','w')
genericsFile = open('parameterListGenerics.f90','w')
methodsFile = open('parameterListMethods.f90','w')
hdf5readFile = open('parameterListH5read.f90','w')
equalsFile = open('parameterListEquality.f90','w')
addNodeFile = open('parameterListAddNode.f90','w')

types = ['SBK','SNK','SLK','SSK','SDK','STR']
ranks = [0, 1, 2, 3, 4, 5, 6, 7]

def getTypeName(type):
  if type == "SBK":
    return "LOGICAL(SBK)"
  elif type == "SNK":
    return "INTEGER(SNK)"
  elif type == "SLK":
    return "INTEGER(SLK)"
  elif type == "SSK":
    return "REAL(SSK)"
  elif type == "SDK":
    return "REAL(SDK)"
  elif type == "STR":
    return "TYPE(StringType)"

def getAttributes(rank):
  if rank > 0:
    return ",ALLOCATABLE"
  else:
    return ""

def getDimension(rank):
  if rank == 0:
    return ""
  else:
    return "(" + ",".join([":" for i in range(rank)]) + ")"

def getClassSubName(type, rank):
  return type + str(rank)

def getWriteFormat(type):
  if type == "SBK":
    return "l"
  elif type == "SNK" or type == "SLK":
    return "i12"
  elif type == "SSK" or type == "SDK":
    return "g20.14"
  elif type == "STR":
    return "a"

def getWrite(type, indexes=""):
  if type == "STR":
    return "CHAR(this%p" + indexes + ")"
  else:
    return "this%p" + indexes

# Write the enumerations
classFile.write("!> List of enumerations to delineate various parameter types\n" +
                "INTEGER(SIK),PARAMETER,PUBLIC :: PL_DATA_TYPE_NULL=0_SIK\n" +
                "INTEGER(SIK),PARAMETER,PUBLIC :: PL_DATA_TYPE_TREE=1_SIK\n")
i = 1
for type in types:
  for rank in ranks:
    i += 1
    classFile.write("INTEGER(SIK),PARAMETER,PUBLIC :: PL_DATA_TYPE_" + getClassSubName(type, rank) + "=" + str(i) + "_SIK\n")
classFile.write("\n")

# Write the names
classFile.write("!> List of names for each data type\n" +
                "CHARACTER(LEN=*),PARAMETER,DIMENSION(0:" + str(len(types)*len(ranks)+1) + "),PUBLIC :: paramTypeNames = &\n" +
                "       ['NULL                      ', &\n" +
                "        'TYPE(ParamType)           '")
for type in types:
  for rank in ranks:
    if rank > 0:
      name = str(rank) + "-D ARRAY "
    else:
      name = ""
    name = name + getTypeName(type)
    classFile.write(", &\n        '" + "{:<26}".format(name) + "'")
classFile.write("]\n\n")

# Write the classes
for type in types:
  for rank in ranks:
    classFile.write("!> @brief defines the parameter tree node data extension for Rank-" + str(rank) + "\n" +
                    "TYPE,EXTENDS(Param_Base) :: Param_" + getClassSubName(type, rank) + "\n" +
                    "  PRIVATE\n" +
                    "  " + getTypeName(type) + getAttributes(rank) + " :: p" + getDimension(rank) + "\n" +
                    "  CONTAINS\n" +
                    "    !> @copybrief ParameterLists::editToText_" + getClassSubName(type, rank) + "\n" +
                    "    !> @copydetails ParameterLists::editToText_" + getClassSubName(type, rank) + "\n" +
                    "    PROCEDURE,PASS,PRIVATE :: editToText => editToText_" + getClassSubName(type, rank) + "\n")
    if ((type == 'SBK' or type == 'STR') and rank > 3):
      pass
    else:
      classFile.write("    !> @copybrief ParameterLists::editToH5_" + getClassSubName(type, rank) + "\n" +
                      "    !> @copydetails ParameterLists::editToH5_" + getClassSubName(type, rank) + "\n" +
                      "    PROCEDURE,PASS,PRIVATE :: editToH5 => editToH5_" + getClassSubName(type, rank) + "\n")
    classFile.write("ENDTYPE Param_" + getClassSubName(type, rank) + "\n\n")

# Write all the init method definitions for ParamNode
initsFile.write("    !> @copybrief ParameterLists::initChar\n" +
                "    !> @copydetails ParameterLists::initChar\n" +
                "    PROCEDURE,PASS,NON_OVERRIDABLE :: initCHAR\n")
initsFile.write("    " + "\n    ".join(["\n    ".join(
  "!> @copybrief Parameterlists::init" + getClassSubName(type, rank) + "\n" +
  "    !> @copydetails Parameterlists::init" + getClassSubName(type, rank) + "\n" +
  "    PROCEDURE,PASS,NON_OVERRIDABLE :: init" + getClassSubName(type, rank)
  for rank in ranks) for type in types]))

# Write the list of get methods for the tree
genericsFile.write("    !List of get methods\n")
genericsFile.write("    !> @copybrief ParameterLists::getTree\n" +
                   "    !> @copybrief ParameterLists::getTree\n" +
                   "    PROCEDURE,PASS,PRIVATE :: getTree\n")
genericsFile.write("    !> @copybrief ParameterLists::getIterator\n" +
                   "    !> @copybrief ParameterLists::getIterator\n" +
                   "    PROCEDURE,PASS,PRIVATE :: getIterator\n")
genericsFile.write("    !> @copybrief ParameterLists::getCHAR\n" +
                   "    !> @copybrief ParameterLists::getCHAR\n" +
                   "    PROCEDURE,PASS,PRIVATE :: getCHAR\n")
genericsFile.write("    !> @copybrief ParameterLists::getStrCHAR\n" +
                   "    !> @copybrief ParameterLists::getStrCHAR\n" +
                   "    PROCEDURE,PASS,PRIVATE :: getStrCHAR\n")
for type in types:
  for rank in ranks:
    genericsFile.write("    !> @copybrief ParameterLists::get" + getClassSubName(type, rank) + "\n" +
                       "    !> @copydetails ParameterLists::get" + getClassSubName(type, rank) + "\n" +
                       "    PROCEDURE,PASS,PRIVATE :: get" + getClassSubName(type, rank) + "\n")
genericsFile.write("    GENERIC :: get => getTree,getIterator,getCHAR,getStrCHAR, &\n        " + ", &\n        ".join([",".join("get" + getClassSubName(type, rank) for rank in ranks) for type in types]) + "\n")

# Write the list of add methods for the tree
genericsFile.write("    !List of add methods\n")
genericsFile.write("    !> @copybrief ParameterLists::addTree\n" +
                   "    !> @copydetails ParameterLists::addTree\n" +
                   "    PROCEDURE,PASS,PRIVATE :: addTree\n")
genericsFile.write("    !> @copybrief ParameterLists::addNode\n" +
                   "    !> @copydetails ParameterLists::addNode\n" +
                   "    PROCEDURE,PASS,PRIVATE :: addNode\n")
genericsFile.write("    !> @copybrief ParameterLists::addCHAR\n" +
                   "    !> @copydetails ParameterLists::addCHAR\n" +
                   "    PROCEDURE,PASS,PRIVATE :: addCHAR\n")
for type in types:
  for rank in ranks:
    genericsFile.write("    !> @copybrief ParameterLists::add" + getClassSubName(type, rank) + "\n" +
                       "    !> @copydetails ParameterLists::add" + getClassSubName(type, rank) + "\n" +
                       "    PROCEDURE,PASS,PRIVATE :: add" + getClassSubName(type, rank) + "\n")
genericsFile.write("    GENERIC :: add => addTree,addNode,addCHAR," + ", &\n        ".join([",".join("add" + getClassSubName(type, rank) for rank in ranks) for type in types]) + "\n")

# Write the list of set methods for the tree
genericsFile.write("    !List of set methods\n")
genericsFile.write("    !> @copybrief ParameterLists::setChar\n" +
                   "    !> @copydetails ParameterLists::setChar\n" +
                   "    PROCEDURE,PASS,PRIVATE :: setCHAR\n")
for type in types:
  for rank in ranks:
    genericsFile.write("    !> @copybrief ParameterLists::set" + getClassSubName(type, rank) + "\n" +
                       "    !> @copydetails ParameterLists::set" + getClassSubName(type, rank) + "\n" +
                       "    PROCEDURE,PASS,PRIVATE :: set" + getClassSubName(type, rank) + "\n")
genericsFile.write("    GENERIC :: set => setCHAR," + ", &\n        ".join([",".join("set" + getClassSubName(type, rank) for rank in ranks) for type in types]) + "\n")

# Write all the init methods
for type in types:
  for rank in ranks:
    methodsFile.write("!\n" +
                      "!-------------------------------------------------------------------------------\n" +
                      "!> @brief Initializes a rank-" + str(rank) + " parameter container of type " + getTypeName(type) + "\n" +
                      "!> @param this the node to initialize data on\n" +
                      "!> @param name the name of the parameter to initialize\n" +
                      "!> @param val the values to use for initializing the node\n" +
                      "!> @param description the description of the value; optional\n" +
                      "!>\n" +
                      "SUBROUTINE init" + getClassSubName(type, rank) + "(this,name,val,description)\n" +
                      "  CLASS(ParamNode),TARGET,INTENT(INOUT) :: this\n" +
                      "  CHARACTER(LEN=*),INTENT(IN) :: name\n" +
                      "  " + getTypeName(type) + ",INTENT(IN) :: val" + getDimension(rank) + "\n" +
                      "  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description\n" +
                      "\n" +
                      "  IF(.NOT.ALLOCATED(this%val)) THEN\n" +
                      "    CALL this%initBase(name,description)\n" +
                      "    this%dataType = PL_DATA_TYPE_" + getClassSubName(type, rank) + "\n" +
                      "    ALLOCATE(Param_" + getClassSubName(type, rank) + " :: this%val)\n" +
                      "  ENDIF\n" +
                      "  SELECTTYPE(ptr => this%val); TYPE IS(Param_" + getClassSubName(type, rank) + ")\n" +
                      "    ptr%p = val\n" +
                      "  ENDSELECT\n" +
                      "\n" +
                      "ENDSUBROUTINE init" + getClassSubName(type, rank) + "\n")

# Write all the get methods
for type in types:
  for rank in ranks:
    methodsFile.write("!\n" +
                      "!-------------------------------------------------------------------------------\n" +
                      "!> @brief Gets a rank-" + str(rank) + " parameter of type " + getTypeName(type) + "\n" +
                      "!> @param this the node to get data from\n" +
                      "!> @param name the name of the parameter to get\n" +
                      "!> @param val the values retrieved from the node\n" +
                      "!> @param default the value to assign to @c val if the parameter is not found; optional\n" +
                      "!>\n" +
                      "!> If the parameter is not found and @c default is not provided, an error is thrown.\n" +
                      "!>\n" +
                      "SUBROUTINE get" + getClassSubName(type, rank) + "(this,name,val,default)\n" +
                      "  CLASS(ParamType),INTENT(IN) :: this\n" +
                      "  CHARACTER(LEN=*),INTENT(IN) :: name\n" +
                      "  " + getTypeName(type) + getAttributes(rank) + ",INTENT(OUT) :: val" + getDimension(rank) + "\n" +
                      "  " + getTypeName(type) + ",INTENT(IN),OPTIONAL :: default" + getDimension(rank) + "\n" +
                      "  !\n" +
                      "  CLASS(ParamNode),POINTER :: node\n" +
                      "\n" +
                      "  CALL this%getNode_Name(name,node)\n" +
                      "  IF(errorChecks_get(node,PRESENT(default),name,PL_DATA_TYPE_" + getClassSubName(type, rank) + ") == 0) THEN\n" +
                      "    IF(ASSOCIATED(node)) THEN\n" +
                      "      SELECTTYPE(ptr => node%val); TYPE IS(Param_" + getClassSubName(type, rank) + ")\n" +
                      "        val=ptr%p\n" +
                      "      ENDSELECT\n" +
                      "    ELSE\n" +
                      "      val=default\n" +
                      "    ENDIF\n" +
                      "  ENDIF\n" +
                      "\n" +
                      "ENDSUBROUTINE get" + getClassSubName(type, rank) + "\n")

# Write all the set methods
for type in types:
  for rank in ranks:
    methodsFile.write("!\n" +
                      "!-------------------------------------------------------------------------------\n" +
                      "!> @brief Sets a rank-" + str(rank) + " parameter of type " + getTypeName(type) + "\n" +
                      "!> @param this the node to set data on\n" +
                      "!> @param name the name of the parameter to set\n" +
                      "!> @param val the values to set to the parameter\n" +
                      "!> @param addmissing logical to indicate if the parameter should be created if not found; optional\n" +
                      "!>\n" +
                      "!> If the parameter is not found and @c addmissing is not set to true, an error is thrown.\n" +
                      "!>\n" +
                      "SUBROUTINE set" + getClassSubName(type, rank) + "(this,name,val,description,addmissing)\n" +
                      "  CLASS(ParamType),INTENT(INOUT) :: this\n" +
                      "  CHARACTER(LEN=*),INTENT(IN) :: name\n" +
                      "  " + getTypeName(type) + ",INTENT(IN) :: val" + getDimension(rank) + "\n" +
                      "  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description\n" +
                      "  LOGICAL(SBK),INTENT(IN),OPTIONAL :: addmissing\n" +
                      "  !\n" +
                      "  LOGICAL(SBK) :: laddmissing\n" +
                      "  CLASS(ParamNode),POINTER :: node\n" +
                      "\n" +
                      "  laddmissing=.FALSE.\n" +
                      "  IF(PRESENT(addmissing)) laddmissing=addmissing\n" +
                      "  IF(laddmissing) THEN\n" +
                      "    CALL this%getOrCreateNode_Name(name,node,.TRUE.)\n" +
                      "  ELSE\n" +
                      "    CALL this%getNode_Name(name,node)\n" +
                      "  ENDIF\n" +
                      "  IF(errorChecks_set(node,laddmissing,name,PL_DATA_TYPE_" + getClassSubName(type, rank) + ") == 0) THEN\n" +
                      "    CALL node%init" + getClassSubName(type, rank) + "(CHAR(node%name),val,description)\n" +
                      "  ENDIF\n" +
                      "\n" +
                      "ENDSUBROUTINE set" + getClassSubName(type, rank) + "\n")

# Write all the add methods
for type in types:
  for rank in ranks:
    methodsFile.write("!\n" +
                      "!-------------------------------------------------------------------------------\n" +
                      "!> @brief Adds a rank-" + str(rank) + " parameter of type " + getTypeName(type) + "\n" +
                      "!> @param this the node to add data to\n" +
                      "!> @param name the name of the parameter to set\n" +
                      "!> @param val the values to add to the parameter\n" +
                      "!> @param description the description of the value; optional\n" +
                      "!>\n" +
                      "SUBROUTINE add" + getClassSubName(type, rank) + "(this,name,val,description)\n" +
                      "  CLASS(ParamType),INTENT(INOUT) :: this\n" +
                      "  CHARACTER(LEN=*),INTENT(IN) :: name\n" +
                      "  " + getTypeName(type) + ",INTENT(IN) :: val" + getDimension(rank) + "\n" +
                      "  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description\n" +
                      "  !\n" +
                      "  CLASS(ParamNode),POINTER :: node\n" +
                      "\n" +
                      "  CALL this%getOrCreateNode_Name(name,node,.TRUE.)\n" +
                      "  IF(errorChecks_add(node,name,PL_DATA_TYPE_" + getClassSubName(type, rank) + ") == 0) THEN\n" +
                      "    CALL node%init" + getClassSubName(type, rank) + "(CHAR(node%name),val,description)\n" +
                      "  ENDIF\n" +
                      "\n" +
                      "ENDSUBROUTINE add" + getClassSubName(type, rank) + "\n")

def createEditArray(file, type, rank):
  file.write("  CALL this%getString('',editString)\n")
  file.write("  WRITE(UNIT=funit,FMT='(\",\",a)') editString//' ! '//description\n")

def createEditScalar(file, type):
  file.write("  WRITE(UNIT=funit,FMT='(" + getWriteFormat(type) + ",a)') " + getWrite(type) + ",' ! '//description\n")

def createEdit(file, type, rank):
  if rank > 0:
    createEditArray(file, type, rank)
  else:
    createEditScalar(file, type)

# Write all the edit methods
for type in types:
  for rank in ranks:
    methodsFile.write("!\n" +
                      "!-------------------------------------------------------------------------------\n" +
                      "!> @brief edits a rank-" + str(rank) + " parameter of type " + getTypeName(type) + "\n" +
                      "!> @param this the parameter to edit\n" +
                      "!> @param funit the file unit to write to\n" +
                      "!> @param indent the additional indentation with which to prefix the parameter\n" +
                      "!> @param prefix the additional prefix to prepend to the parameter\n" +
                      "!> @param shift the additional spacing to add before the data\n" +
                      "!> @param description the description of the parameter\n" +
                      "!> @param paddtw logical to indicate if the parameter type should be padded to force alignment;\n" +
                      "!>               optional, defaults to false\n" +
                      "!>\n" +
                      "SUBROUTINE editToText_" + getClassSubName(type, rank) + "(this,funit,indent,prefix,shift,description,paddtw)\n" +
                      "  CLASS(Param_" + getClassSubName(type, rank) + "),INTENT(IN) :: this\n" +
                      "  INTEGER(SIK),INTENT(IN) :: funit\n" +
                      "  INTEGER(SIK),INTENT(IN) :: indent\n" +
                      "  TYPE(StringType),INTENT(IN) :: prefix\n" +
                      "  INTEGER(SIK),INTENT(IN) :: shift\n" +
                      "  CHARACTER(LEN=*),INTENT(IN) :: description\n" +
                      "  LOGICAL(SBK),INTENT(IN) :: paddtw\n" +
                      "  !\n")
    if rank > 0:
      methodsFile.write("  TYPE(StringType) :: editString\n")
    createEdit(methodsFile, type, rank)
    methodsFile.write("\n" +
                      "ENDSUBROUTINE editToText_" + getClassSubName(type, rank) + "\n")

# Write the HDF5 write method
for type in types:
  for rank in ranks:
    if ((type == 'SBK' or type == 'STR') and rank > 3):
      pass
    else:
      methodsFile.write("!\n" +
                        "!-------------------------------------------------------------------------------\n" +
                        "!> @brief Writes a rank-" + str(rank) + "parameter of type " + getTypeName(type) + " to an HDF5 file\n" +
                        "!> @param this the parameter to write\n" +
                        "!> @param file the HDF5 file type\n" +
                        "!> @param path the location to write the parameter to\n" +
                        "!>\n" +
                        "SUBROUTINE editToH5_" + getClassSubName(type, rank) + "(this,file,path)\n" +
                        "  CLASS(Param_" + getClassSubName(type, rank) + "),INTENT(IN) :: this\n" +
                        "  CLASS(HDF5FileType),INTENT(INOUT) :: file\n" +
                        "  CHARACTER(LEN=*),INTENT(IN) :: path\n" +
                        "  !\n" +
                        "\n" +
                        "  REQUIRE(file%isInit)\n" +
                        "  REQUIRE(file%isOpen())\n" +
                        "\n" +
                        "  CALL file%fwrite(path,this%p)\n" +
                        "\n" +
                        "ENDSUBROUTINE editToH5_" + getClassSubName(type, rank) + "\n")

# Write the SOFTEQ(...) interfaces
methodsFile.write("!\n" +
                  "!-------------------------------------------------------------------------------\n" +
                  "!> @brief Performs a soft equals operation on two Param_Base objects\n" +
                  "!> @param lhs the first parameter to compare\n" +
                  "!> @param lhs the second parameter to compare\n" +
                  "!> @param tol the tolerance of the comparison\n" +
                  "!> @returns equals the result of the comparison\n" +
                  "!>\n" +
                  "IMPURE ELEMENTAL FUNCTION SOFTEQ_Param(lhs,rhs,tol) RESULT(equals)\n" +
                  "  CLASS(Param_Base),INTENT(IN) :: lhs\n" +
                  "  CLASS(Param_Base),INTENT(IN) :: rhs\n" +
                  "  REAL(SDK),INTENT(IN) :: tol\n" +
                  "  LOGICAL(SBK) :: equals\n" +
                  "\n" +
                  "  equals = SAME_TYPE_AS(lhs,rhs)\n" +
                  "  IF(.NOT.equals) RETURN\n" +
                  "\n" +
                  "  SELECTTYPE(lhs)\n")
for type in ['SSK', 'SDK']:
  for rank in ranks:
    if rank == 0:
      methodsFile.write("  TYPE IS(Param_" + getClassSubName(type, rank) + ")\n" +
                        "    SELECTTYPE(rhs); TYPE IS(Param_" + getClassSubName(type, rank) + ")\n" +
                        "      equals = SOFTEQ(lhs%p,rhs%p,REAL(tol," + type + "))\n" +
                        "    ENDSELECT\n")
    else:
      methodsFile.write("  TYPE IS(Param_" + getClassSubName(type, rank) + ")\n" +
                        "    SELECTTYPE(rhs); TYPE IS(Param_" + getClassSubName(type, rank) + ")\n" +
                        "      equals = ALL(SOFTEQ(lhs%p,rhs%p,REAL(tol," + type + ")))\n" +
                        "    ENDSELECT\n")
methodsFile.write("  CLASS DEFAULT\n" +
                  "    equals = (lhs == rhs)\n" +
                  "  ENDSELECT\n" +
                  "\n" +
                  "ENDFUNCTION SOFTEQ_Param\n")


# Write the HDF5 reader helper file
hdf5readFile.write("!\n" +
                   "!-------------------------------------------------------------------------------\n" +
                   "!> @brief Reads a @c ParamNode from an HDF5 file\n" +
                   "!> @param this the parameter to write\n" +
                   "!> @param file the HDF5 file type\n" +
                   "!> @param path the location to write the parameter to\n" +
                   "!>\n" +
                   "SUBROUTINE readNodeFromH5(this,file,path,dataType,rank)\n" +
                   "  CLASS(ParamNode),INTENT(INOUT) :: this\n" +
                   "  CLASS(HDF5FileType),INTENT(INOUT) :: file\n" +
                   "  CHARACTER(LEN=*),INTENT(IN) :: path\n" +
                   "  TYPE(StringType),INTENT(IN) :: dataType\n" +
                   "  INTEGER(SIK),INTENT(IN) :: rank\n" +
                   "  !\n" +
                   "  TYPE(StringType) :: description\n")
for type in types:
  for rank in ranks:
    if (type == 'STR' or type == 'SBK') and rank > 3:
      pass
    else:
      hdf5readFile.write("  " + getTypeName(type) + getAttributes(rank) + " :: read" + getClassSubName(type, rank) + getDimension(rank) + "\n")
hdf5readFile.write("\n" +
                  "  REQUIRE(file%isInit)\n" +
                  "  REQUIRE(file%isOpen())\n" +
                  "  REQUIRE(.NOT.ALLOCATED(this%val))\n" +
                  "\n" +
                  "  !Get the description if there is one\n" +
                  "  IF(file%has_attribute(path,'description')) THEN\n" +
                  "    CALL file%read_attribute(path,'description',description)\n" +
                  "  ELSE\n" +
                  "    description = ''\n" +
                  "  ENDIF\n" +
                  "  SELECTCASE(CHAR(dataType))\n" +
                  "  CASE('Tree')\n" +
                  "    CALL this%initTree(CHAR(this%name),DESCRIPTION=CHAR(description))\n")
for type in types:
  if type == 'SBK':
    pass
  else:
    hdf5readFile.write("  CASE('" + type + "')\n" +
                        "    SELECTCASE(rank)\n")
    for rank in ranks:
      if type == 'STR' and rank > 3:
        pass
      else:
        hdf5readFile.write("    CASE(" + str(rank) + ")\n" +
                           "      CALL file%fread(path,read" + getClassSubName(type, rank) +")\n")
        if type == 'STR':
          if rank == 0:
            hdf5readFile.write("      IF(read" + getClassSubName(type, rank) + " == 'T' .OR. read" + getClassSubName(type, rank) + " == 'F') THEN\n")
          else:
            hdf5readFile.write("      IF(ALL(read" + getClassSubName(type, rank) + " == 'T' .OR. read" + getClassSubName(type, rank) + " == 'F') .AND. SIZE(read" + getClassSubName(type, rank) + ") > 0) THEN\n")
          hdf5readFile.write("        CALL file%fread(path,readSBK" + str(rank) + ")\n"
                            "        CALL this%initSBK" + str(rank) + "(CHAR(this%name),readSBK" + str(rank) + ",CHAR(description))\n" +
                            "      ELSE\n  ")
        hdf5readFile.write("      CALL this%init" + getClassSubName(type, rank) + "(CHAR(this%name),read" + getClassSubName(type, rank) + ",CHAR(description))\n")
        if type == 'STR':
          hdf5readFile.write("      ENDIF\n")
    hdf5readFile.write("    ENDSELECT\n")
hdf5readFile.write("  ENDSELECT\n" +
                   "\n" +
                   "ENDSUBROUTINE readNodeFromH5")

# Write the == operator
equalsFile.write("!\n" +
                 "!-------------------------------------------------------------------------------\n" +
                 "!> @brief Equality operator for 2 @c Param_Base objects\n" +
                 "!> @param lhs the first parameter to compare\n" +
                 "!> @param rhs the second parameter to compare\n" +
                 "!> @returns bool the result of the equliaty comparison\n" +
                 "!>\n" +
                 "FUNCTION equals_ParamBase(lhs,rhs) RESULT(bool)\n" +
                 "  CLASS(Param_Base),INTENT(IN) :: lhs\n" +
                 "  CLASS(Param_Base),INTENT(IN) :: rhs\n" +
                 "  LOGICAL(SBK) :: bool\n" +
                 "\n" +
                 "  SELECTTYPE(LHS)\n")
for type in types:
  for rank in ranks:
    equalsFile.write("  TYPE IS(Param_" + getClassSubName(type, rank) + ")\n" +
                     "    SELECTTYPE(rhs)\n" +
                     "    TYPE IS(Param_" + getClassSubName(type, rank) + ")\n")
    if rank > 0:
      equalsFile.write("      IF(ALL(SHAPE(lhs%p) == SHAPE(rhs%p))) THEN\n")
      if type == 'SSK' or type == 'SDK':
        equalsFile.write("        bool = ALL(lhs%p .APPROXEQ. rhs%p)\n")
      else:
        equalsFile.write("        bool = ALL(lhs%p == rhs%p)\n")
      equalsFile.write("      ELSE\n" +
                       "        bool = .FALSE.\n" +
                       "      ENDIF\n")
    else:
      if type == 'SSK' or type == 'SDK':
        equalsFile.write("      bool = (lhs%p .APPROXEQ. rhs%p)\n")
      else:
        equalsFile.write("      bool = (lhs%p == rhs%p)\n")
    equalsFile.write("    CLASS DEFAULT\n" +
                     "      bool = .FALSE.\n" +
                     "    ENDSELECT\n")
equalsFile.write("  CLASS DEFAULT\n" +
                 "    bool = .FALSE.\n" +
                 "  ENDSELECT\n" +
                 "\n" +
                 "ENDFUNCTION equals_ParamBase\n")

# Write the addNode method
addNodeFile.write("!\n" +
                  "!-------------------------------------------------------------------------------\n" +
                  "!> @brief Adds a @c ParamNode to a @c ParamType\n" +
                  "!> @param this the @c ParamType to add to\n" +
                  "!> @param name the name of the node to add\n" +
                  "!> @param val the node to add to @c this\n" +
                  "!>\n" +
                  "SUBROUTINE addNode(this,name,val)\n" +
                  "  CLASS(ParamType),INTENT(INOUT) :: this\n" +
                  "  CHARACTER(LEN=*),INTENT(IN) :: name\n" +
                  "  CLASS(ParamNode),INTENT(IN) :: val\n" +
                  "\n" +
                  "  IF(ALLOCATED(val%val)) THEN\n" +
                  "    SELECTTYPE(ptr => val%val)\n")
for type in types:
  for rank in ranks:
    addNodeFile.write("    TYPE IS(Param_" + getClassSubName(type, rank) + ")\n" +
                      "      CALL this%add" + getClassSubName(type,rank) + "(name,ptr%p,CHAR(val%description))\n")
addNodeFile.write("    ENDSELECT\n" +
                  "  ENDIF\n" +
                  "\n" +
                  "ENDSUBROUTINE addNode\n")