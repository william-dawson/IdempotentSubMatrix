MODULE SetModule
  IMPLICIT NONE
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  TYPE, PUBLIC :: Set_t
     INTEGER, DIMENSION(:), ALLOCATABLE :: index_list
  END TYPE Set_t
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  INTERFACE Set_t
     MODULE PROCEDURE :: ConstructSet
  END INTERFACE
CONTAINS!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> Construct a set from a string.
  FUNCTION ConstructSet(set_list) RESULT(set)
    !> Input string describing the set
    CHARACTER(len=100), INTENT(IN) :: set_list
    !> Set to construct
    TYPE(Set_t) :: set
    !! Local Variables
    CHARACTER(len=100) :: temp_str
    INTEGER :: start_str, end_str, estart_str
    INTEGER :: entries, ec
    INTEGER :: II

    !! Remove the yaml prettyness [, ]
    temp_str = set_list
    end_str = LEN(TRIM(temp_str)) - 1
    DO start_str = 1, end_str
       IF (temp_str(start_str:start_str) .EQ. '[') THEN
          EXIT
       END IF
    END DO
    start_str = start_str + 1

    !! Count the entries
    entries = 1
    DO II = start_str, end_str
       IF (temp_str(II:II) .EQ. ',') THEN
          entries = entries + 1
       END IF
    END DO
    ALLOCATE(set%index_list(entries))

    !! Read the entries
    ec = 1
    estart_str = start_str
    DO II = start_str, end_str
       IF (temp_str(II:II) .EQ. ',') THEN
          READ(temp_str(estart_str:II-1), *) set%index_list(ec)
          ec = ec + 1
          estart_str = II + 1
       END IF
    END DO
    READ(temp_str(estart_str:end_str), *) set%index_list(ec)
  END FUNCTION ConstructSet
END MODULE SetModule
