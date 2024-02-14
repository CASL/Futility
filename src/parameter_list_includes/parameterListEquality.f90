!
!-------------------------------------------------------------------------------
!> @brief Equality operator for 2 @c Param_Base objects
!> @param lhs the first parameter to compare
!> @param rhs the second parameter to compare
!> @returns bool the result of the equliaty comparison
!>
FUNCTION equals_ParamBase(lhs,rhs) RESULT(bool)
  CLASS(Param_Base),INTENT(IN) :: lhs
  CLASS(Param_Base),INTENT(IN) :: rhs
  LOGICAL(SBK) :: bool

  SELECTTYPE(LHS)
  TYPE IS(Param_SBK0)
    SELECTTYPE(rhs)
    TYPE IS(Param_SBK0)
      bool = (lhs%p == rhs%p)
    CLASS DEFAULT
      bool = .FALSE.
    ENDSELECT
  TYPE IS(Param_SBK1)
    SELECTTYPE(rhs)
    TYPE IS(Param_SBK1)
      IF(ALL(SHAPE(lhs%p) == SHAPE(rhs%p))) THEN
        bool = ALL(lhs%p == rhs%p)
      ELSE
        bool = .FALSE.
      ENDIF
    CLASS DEFAULT
      bool = .FALSE.
    ENDSELECT
  TYPE IS(Param_SBK2)
    SELECTTYPE(rhs)
    TYPE IS(Param_SBK2)
      IF(ALL(SHAPE(lhs%p) == SHAPE(rhs%p))) THEN
        bool = ALL(lhs%p == rhs%p)
      ELSE
        bool = .FALSE.
      ENDIF
    CLASS DEFAULT
      bool = .FALSE.
    ENDSELECT
  TYPE IS(Param_SBK3)
    SELECTTYPE(rhs)
    TYPE IS(Param_SBK3)
      IF(ALL(SHAPE(lhs%p) == SHAPE(rhs%p))) THEN
        bool = ALL(lhs%p == rhs%p)
      ELSE
        bool = .FALSE.
      ENDIF
    CLASS DEFAULT
      bool = .FALSE.
    ENDSELECT
  TYPE IS(Param_SBK4)
    SELECTTYPE(rhs)
    TYPE IS(Param_SBK4)
      IF(ALL(SHAPE(lhs%p) == SHAPE(rhs%p))) THEN
        bool = ALL(lhs%p == rhs%p)
      ELSE
        bool = .FALSE.
      ENDIF
    CLASS DEFAULT
      bool = .FALSE.
    ENDSELECT
  TYPE IS(Param_SBK5)
    SELECTTYPE(rhs)
    TYPE IS(Param_SBK5)
      IF(ALL(SHAPE(lhs%p) == SHAPE(rhs%p))) THEN
        bool = ALL(lhs%p == rhs%p)
      ELSE
        bool = .FALSE.
      ENDIF
    CLASS DEFAULT
      bool = .FALSE.
    ENDSELECT
  TYPE IS(Param_SBK6)
    SELECTTYPE(rhs)
    TYPE IS(Param_SBK6)
      IF(ALL(SHAPE(lhs%p) == SHAPE(rhs%p))) THEN
        bool = ALL(lhs%p == rhs%p)
      ELSE
        bool = .FALSE.
      ENDIF
    CLASS DEFAULT
      bool = .FALSE.
    ENDSELECT
  TYPE IS(Param_SBK7)
    SELECTTYPE(rhs)
    TYPE IS(Param_SBK7)
      IF(ALL(SHAPE(lhs%p) == SHAPE(rhs%p))) THEN
        bool = ALL(lhs%p == rhs%p)
      ELSE
        bool = .FALSE.
      ENDIF
    CLASS DEFAULT
      bool = .FALSE.
    ENDSELECT
  TYPE IS(Param_SNK0)
    SELECTTYPE(rhs)
    TYPE IS(Param_SNK0)
      bool = (lhs%p == rhs%p)
    CLASS DEFAULT
      bool = .FALSE.
    ENDSELECT
  TYPE IS(Param_SNK1)
    SELECTTYPE(rhs)
    TYPE IS(Param_SNK1)
      IF(ALL(SHAPE(lhs%p) == SHAPE(rhs%p))) THEN
        bool = ALL(lhs%p == rhs%p)
      ELSE
        bool = .FALSE.
      ENDIF
    CLASS DEFAULT
      bool = .FALSE.
    ENDSELECT
  TYPE IS(Param_SNK2)
    SELECTTYPE(rhs)
    TYPE IS(Param_SNK2)
      IF(ALL(SHAPE(lhs%p) == SHAPE(rhs%p))) THEN
        bool = ALL(lhs%p == rhs%p)
      ELSE
        bool = .FALSE.
      ENDIF
    CLASS DEFAULT
      bool = .FALSE.
    ENDSELECT
  TYPE IS(Param_SNK3)
    SELECTTYPE(rhs)
    TYPE IS(Param_SNK3)
      IF(ALL(SHAPE(lhs%p) == SHAPE(rhs%p))) THEN
        bool = ALL(lhs%p == rhs%p)
      ELSE
        bool = .FALSE.
      ENDIF
    CLASS DEFAULT
      bool = .FALSE.
    ENDSELECT
  TYPE IS(Param_SNK4)
    SELECTTYPE(rhs)
    TYPE IS(Param_SNK4)
      IF(ALL(SHAPE(lhs%p) == SHAPE(rhs%p))) THEN
        bool = ALL(lhs%p == rhs%p)
      ELSE
        bool = .FALSE.
      ENDIF
    CLASS DEFAULT
      bool = .FALSE.
    ENDSELECT
  TYPE IS(Param_SNK5)
    SELECTTYPE(rhs)
    TYPE IS(Param_SNK5)
      IF(ALL(SHAPE(lhs%p) == SHAPE(rhs%p))) THEN
        bool = ALL(lhs%p == rhs%p)
      ELSE
        bool = .FALSE.
      ENDIF
    CLASS DEFAULT
      bool = .FALSE.
    ENDSELECT
  TYPE IS(Param_SNK6)
    SELECTTYPE(rhs)
    TYPE IS(Param_SNK6)
      IF(ALL(SHAPE(lhs%p) == SHAPE(rhs%p))) THEN
        bool = ALL(lhs%p == rhs%p)
      ELSE
        bool = .FALSE.
      ENDIF
    CLASS DEFAULT
      bool = .FALSE.
    ENDSELECT
  TYPE IS(Param_SNK7)
    SELECTTYPE(rhs)
    TYPE IS(Param_SNK7)
      IF(ALL(SHAPE(lhs%p) == SHAPE(rhs%p))) THEN
        bool = ALL(lhs%p == rhs%p)
      ELSE
        bool = .FALSE.
      ENDIF
    CLASS DEFAULT
      bool = .FALSE.
    ENDSELECT
  TYPE IS(Param_SLK0)
    SELECTTYPE(rhs)
    TYPE IS(Param_SLK0)
      bool = (lhs%p == rhs%p)
    CLASS DEFAULT
      bool = .FALSE.
    ENDSELECT
  TYPE IS(Param_SLK1)
    SELECTTYPE(rhs)
    TYPE IS(Param_SLK1)
      IF(ALL(SHAPE(lhs%p) == SHAPE(rhs%p))) THEN
        bool = ALL(lhs%p == rhs%p)
      ELSE
        bool = .FALSE.
      ENDIF
    CLASS DEFAULT
      bool = .FALSE.
    ENDSELECT
  TYPE IS(Param_SLK2)
    SELECTTYPE(rhs)
    TYPE IS(Param_SLK2)
      IF(ALL(SHAPE(lhs%p) == SHAPE(rhs%p))) THEN
        bool = ALL(lhs%p == rhs%p)
      ELSE
        bool = .FALSE.
      ENDIF
    CLASS DEFAULT
      bool = .FALSE.
    ENDSELECT
  TYPE IS(Param_SLK3)
    SELECTTYPE(rhs)
    TYPE IS(Param_SLK3)
      IF(ALL(SHAPE(lhs%p) == SHAPE(rhs%p))) THEN
        bool = ALL(lhs%p == rhs%p)
      ELSE
        bool = .FALSE.
      ENDIF
    CLASS DEFAULT
      bool = .FALSE.
    ENDSELECT
  TYPE IS(Param_SLK4)
    SELECTTYPE(rhs)
    TYPE IS(Param_SLK4)
      IF(ALL(SHAPE(lhs%p) == SHAPE(rhs%p))) THEN
        bool = ALL(lhs%p == rhs%p)
      ELSE
        bool = .FALSE.
      ENDIF
    CLASS DEFAULT
      bool = .FALSE.
    ENDSELECT
  TYPE IS(Param_SLK5)
    SELECTTYPE(rhs)
    TYPE IS(Param_SLK5)
      IF(ALL(SHAPE(lhs%p) == SHAPE(rhs%p))) THEN
        bool = ALL(lhs%p == rhs%p)
      ELSE
        bool = .FALSE.
      ENDIF
    CLASS DEFAULT
      bool = .FALSE.
    ENDSELECT
  TYPE IS(Param_SLK6)
    SELECTTYPE(rhs)
    TYPE IS(Param_SLK6)
      IF(ALL(SHAPE(lhs%p) == SHAPE(rhs%p))) THEN
        bool = ALL(lhs%p == rhs%p)
      ELSE
        bool = .FALSE.
      ENDIF
    CLASS DEFAULT
      bool = .FALSE.
    ENDSELECT
  TYPE IS(Param_SLK7)
    SELECTTYPE(rhs)
    TYPE IS(Param_SLK7)
      IF(ALL(SHAPE(lhs%p) == SHAPE(rhs%p))) THEN
        bool = ALL(lhs%p == rhs%p)
      ELSE
        bool = .FALSE.
      ENDIF
    CLASS DEFAULT
      bool = .FALSE.
    ENDSELECT
  TYPE IS(Param_SSK0)
    SELECTTYPE(rhs)
    TYPE IS(Param_SSK0)
      bool = (lhs%p .APPROXEQ. rhs%p)
    CLASS DEFAULT
      bool = .FALSE.
    ENDSELECT
  TYPE IS(Param_SSK1)
    SELECTTYPE(rhs)
    TYPE IS(Param_SSK1)
      IF(ALL(SHAPE(lhs%p) == SHAPE(rhs%p))) THEN
        bool = ALL(lhs%p .APPROXEQ. rhs%p)
      ELSE
        bool = .FALSE.
      ENDIF
    CLASS DEFAULT
      bool = .FALSE.
    ENDSELECT
  TYPE IS(Param_SSK2)
    SELECTTYPE(rhs)
    TYPE IS(Param_SSK2)
      IF(ALL(SHAPE(lhs%p) == SHAPE(rhs%p))) THEN
        bool = ALL(lhs%p .APPROXEQ. rhs%p)
      ELSE
        bool = .FALSE.
      ENDIF
    CLASS DEFAULT
      bool = .FALSE.
    ENDSELECT
  TYPE IS(Param_SSK3)
    SELECTTYPE(rhs)
    TYPE IS(Param_SSK3)
      IF(ALL(SHAPE(lhs%p) == SHAPE(rhs%p))) THEN
        bool = ALL(lhs%p .APPROXEQ. rhs%p)
      ELSE
        bool = .FALSE.
      ENDIF
    CLASS DEFAULT
      bool = .FALSE.
    ENDSELECT
  TYPE IS(Param_SSK4)
    SELECTTYPE(rhs)
    TYPE IS(Param_SSK4)
      IF(ALL(SHAPE(lhs%p) == SHAPE(rhs%p))) THEN
        bool = ALL(lhs%p .APPROXEQ. rhs%p)
      ELSE
        bool = .FALSE.
      ENDIF
    CLASS DEFAULT
      bool = .FALSE.
    ENDSELECT
  TYPE IS(Param_SSK5)
    SELECTTYPE(rhs)
    TYPE IS(Param_SSK5)
      IF(ALL(SHAPE(lhs%p) == SHAPE(rhs%p))) THEN
        bool = ALL(lhs%p .APPROXEQ. rhs%p)
      ELSE
        bool = .FALSE.
      ENDIF
    CLASS DEFAULT
      bool = .FALSE.
    ENDSELECT
  TYPE IS(Param_SSK6)
    SELECTTYPE(rhs)
    TYPE IS(Param_SSK6)
      IF(ALL(SHAPE(lhs%p) == SHAPE(rhs%p))) THEN
        bool = ALL(lhs%p .APPROXEQ. rhs%p)
      ELSE
        bool = .FALSE.
      ENDIF
    CLASS DEFAULT
      bool = .FALSE.
    ENDSELECT
  TYPE IS(Param_SSK7)
    SELECTTYPE(rhs)
    TYPE IS(Param_SSK7)
      IF(ALL(SHAPE(lhs%p) == SHAPE(rhs%p))) THEN
        bool = ALL(lhs%p .APPROXEQ. rhs%p)
      ELSE
        bool = .FALSE.
      ENDIF
    CLASS DEFAULT
      bool = .FALSE.
    ENDSELECT
  TYPE IS(Param_SDK0)
    SELECTTYPE(rhs)
    TYPE IS(Param_SDK0)
      bool = (lhs%p .APPROXEQ. rhs%p)
    CLASS DEFAULT
      bool = .FALSE.
    ENDSELECT
  TYPE IS(Param_SDK1)
    SELECTTYPE(rhs)
    TYPE IS(Param_SDK1)
      IF(ALL(SHAPE(lhs%p) == SHAPE(rhs%p))) THEN
        bool = ALL(lhs%p .APPROXEQ. rhs%p)
      ELSE
        bool = .FALSE.
      ENDIF
    CLASS DEFAULT
      bool = .FALSE.
    ENDSELECT
  TYPE IS(Param_SDK2)
    SELECTTYPE(rhs)
    TYPE IS(Param_SDK2)
      IF(ALL(SHAPE(lhs%p) == SHAPE(rhs%p))) THEN
        bool = ALL(lhs%p .APPROXEQ. rhs%p)
      ELSE
        bool = .FALSE.
      ENDIF
    CLASS DEFAULT
      bool = .FALSE.
    ENDSELECT
  TYPE IS(Param_SDK3)
    SELECTTYPE(rhs)
    TYPE IS(Param_SDK3)
      IF(ALL(SHAPE(lhs%p) == SHAPE(rhs%p))) THEN
        bool = ALL(lhs%p .APPROXEQ. rhs%p)
      ELSE
        bool = .FALSE.
      ENDIF
    CLASS DEFAULT
      bool = .FALSE.
    ENDSELECT
  TYPE IS(Param_SDK4)
    SELECTTYPE(rhs)
    TYPE IS(Param_SDK4)
      IF(ALL(SHAPE(lhs%p) == SHAPE(rhs%p))) THEN
        bool = ALL(lhs%p .APPROXEQ. rhs%p)
      ELSE
        bool = .FALSE.
      ENDIF
    CLASS DEFAULT
      bool = .FALSE.
    ENDSELECT
  TYPE IS(Param_SDK5)
    SELECTTYPE(rhs)
    TYPE IS(Param_SDK5)
      IF(ALL(SHAPE(lhs%p) == SHAPE(rhs%p))) THEN
        bool = ALL(lhs%p .APPROXEQ. rhs%p)
      ELSE
        bool = .FALSE.
      ENDIF
    CLASS DEFAULT
      bool = .FALSE.
    ENDSELECT
  TYPE IS(Param_SDK6)
    SELECTTYPE(rhs)
    TYPE IS(Param_SDK6)
      IF(ALL(SHAPE(lhs%p) == SHAPE(rhs%p))) THEN
        bool = ALL(lhs%p .APPROXEQ. rhs%p)
      ELSE
        bool = .FALSE.
      ENDIF
    CLASS DEFAULT
      bool = .FALSE.
    ENDSELECT
  TYPE IS(Param_SDK7)
    SELECTTYPE(rhs)
    TYPE IS(Param_SDK7)
      IF(ALL(SHAPE(lhs%p) == SHAPE(rhs%p))) THEN
        bool = ALL(lhs%p .APPROXEQ. rhs%p)
      ELSE
        bool = .FALSE.
      ENDIF
    CLASS DEFAULT
      bool = .FALSE.
    ENDSELECT
  TYPE IS(Param_STR0)
    SELECTTYPE(rhs)
    TYPE IS(Param_STR0)
      bool = (lhs%p == rhs%p)
    CLASS DEFAULT
      bool = .FALSE.
    ENDSELECT
  TYPE IS(Param_STR1)
    SELECTTYPE(rhs)
    TYPE IS(Param_STR1)
      IF(ALL(SHAPE(lhs%p) == SHAPE(rhs%p))) THEN
        bool = ALL(lhs%p == rhs%p)
      ELSE
        bool = .FALSE.
      ENDIF
    CLASS DEFAULT
      bool = .FALSE.
    ENDSELECT
  TYPE IS(Param_STR2)
    SELECTTYPE(rhs)
    TYPE IS(Param_STR2)
      IF(ALL(SHAPE(lhs%p) == SHAPE(rhs%p))) THEN
        bool = ALL(lhs%p == rhs%p)
      ELSE
        bool = .FALSE.
      ENDIF
    CLASS DEFAULT
      bool = .FALSE.
    ENDSELECT
  TYPE IS(Param_STR3)
    SELECTTYPE(rhs)
    TYPE IS(Param_STR3)
      IF(ALL(SHAPE(lhs%p) == SHAPE(rhs%p))) THEN
        bool = ALL(lhs%p == rhs%p)
      ELSE
        bool = .FALSE.
      ENDIF
    CLASS DEFAULT
      bool = .FALSE.
    ENDSELECT
  TYPE IS(Param_STR4)
    SELECTTYPE(rhs)
    TYPE IS(Param_STR4)
      IF(ALL(SHAPE(lhs%p) == SHAPE(rhs%p))) THEN
        bool = ALL(lhs%p == rhs%p)
      ELSE
        bool = .FALSE.
      ENDIF
    CLASS DEFAULT
      bool = .FALSE.
    ENDSELECT
  TYPE IS(Param_STR5)
    SELECTTYPE(rhs)
    TYPE IS(Param_STR5)
      IF(ALL(SHAPE(lhs%p) == SHAPE(rhs%p))) THEN
        bool = ALL(lhs%p == rhs%p)
      ELSE
        bool = .FALSE.
      ENDIF
    CLASS DEFAULT
      bool = .FALSE.
    ENDSELECT
  TYPE IS(Param_STR6)
    SELECTTYPE(rhs)
    TYPE IS(Param_STR6)
      IF(ALL(SHAPE(lhs%p) == SHAPE(rhs%p))) THEN
        bool = ALL(lhs%p == rhs%p)
      ELSE
        bool = .FALSE.
      ENDIF
    CLASS DEFAULT
      bool = .FALSE.
    ENDSELECT
  TYPE IS(Param_STR7)
    SELECTTYPE(rhs)
    TYPE IS(Param_STR7)
      IF(ALL(SHAPE(lhs%p) == SHAPE(rhs%p))) THEN
        bool = ALL(lhs%p == rhs%p)
      ELSE
        bool = .FALSE.
      ENDIF
    CLASS DEFAULT
      bool = .FALSE.
    ENDSELECT
  CLASS DEFAULT
    bool = .FALSE.
  ENDSELECT

ENDFUNCTION equals_ParamBase
