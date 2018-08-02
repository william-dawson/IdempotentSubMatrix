PROGRAM IdempotentSub
  USE DataTypesModule, ONLY : NTREAL
  USE IdempotencyModule, ONLY : ComputeIdempotency
  USE InverseSolversModule, ONLY : Invert
  USE ProcessGridModule, ONLY : ConstructProcessGrid, IsRoot, global_grid
  USE PSMatrixAlgebraModule, ONLY: MatrixMultiply
  USE PSMatrixModule, ONLY : Matrix_ps, ConstructMatrixFromMatrixMarket
  USE SolverParametersModule, ONLY : SolverParameters_t
  USE SetModule, ONLY : Set_t
  USE MPI
  IMPLICIT NONE
  !! Input Parameters
  CHARACTER(len=80) :: subset_file
  CHARACTER(len=80) :: density_file
  CHARACTER(len=80) :: overlap_file
  CHARACTER(len=80) :: output_file
  REAL(NTREAL) :: threshold, convergence_threshold
  !! Set Processing
  TYPE(Set_t), DIMENSION(:), ALLOCATABLE :: set_list
  REAL(NTREAL), DIMENSION(:), ALLOCATABLE :: values
  !! Matrices
  TYPE(Matrix_ps) :: SMat, DMat
  TYPE(Matrix_ps) :: SInvMat, WMat
  TYPE(SolverParameters_t) :: sp
  !! Temporary Variables
  CHARACTER(len=80) :: argument
  CHARACTER(len=80) :: argument_value
  INTEGER :: II
  INTEGER :: provided
  INTEGER :: ierr

  !! Setup MPI
  CALL MPI_Init_thread(MPI_THREAD_SERIALIZED, provided, ierr)

  CALL ConstructProcessGrid(MPI_COMM_WORLD)

  !! Process the input parameters.
  DO II=1,command_argument_count(),2
     CALL get_command_argument(II,argument)
     CALL get_command_argument(II+1,argument_value)
     SELECT CASE(argument)
     CASE('--overlap')
        overlap_file = argument_value
     CASE('--density')
        density_file = argument_value
     CASE('--subset')
        subset_file = argument_value
     CASE('--output')
        output_file = argument_value
     CASE('--threshold')
        READ(argument_value,*) threshold
     CASE('--convergence_threshold')
        READ(argument_value,*) convergence_threshold
     END SELECT
  END DO
  sp = SolverParameters_t(converge_diff_in=convergence_threshold, &
       & threshold_in = threshold)

  !! Get the sets
  CALL ProcessSet

  !! Read the matrices
  CALL ConstructMatrixFromMatrixMarket(SMat, overlap_file)
  CALL ConstructMatrixFromMatrixMarket(DMat, density_file)

  !! Convert The Density Matrix To The Mulliken Basis
  CALL Invert(SMat, SInvMat, sp)
  CALL MatrixMultiply(SInvMat, DMat, WMat, threshold_in=threshold)

  !! Main Calculation
  ALLOCATE(values(SIZE(set_list)))
  CALL ComputeIdempotency(WMat, set_list, threshold, values)

  !! Write To File For Checking
  CALL WriteOutput(values, output_file)

  !! Cleanup
  IF (ALLOCATED(set_list)) THEN
     DEALLOCATE(set_list)
  END IF
  IF (ALLOCATED(values)) THEN
     DEALLOCATE(values)
  END IF

  CALL MPI_Finalize(ierr)
CONTAINS!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  SUBROUTINE ProcessSet
    !! Local Varaibles
    INTEGER, PARAMETER :: IO = 1
    CHARACTER(LEN=100) :: ReadBuffer
    INTEGER :: II, nlines

    nlines = 0
    OPEN(UNIT=IO, FILE=subset_file)
    DO
       READ(IO, "(A)", END=10) ReadBuffer
       nlines = nlines + 1
    END DO
10  CLOSE(IO)

    ALLOCATE(set_list(nlines-1))

    OPEN(UNIT=IO, FILE=subset_file)
    READ(IO, "(A)", END=10) ReadBuffer
    DO II = 2, nlines
       READ(IO, "(A)", END=10) ReadBuffer
       set_list(II-1) = Set_t(ReadBuffer)
    END DO
    CLOSE(IO)
  END SUBROUTINE ProcessSet
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  SUBROUTINE WriteOutput(values, file)
    !> Values to write out
    REAL(NTREAL), DIMENSION(:), INTENT(IN) :: values
    !> Name of the file to write to
    CHARACTER(len=80) :: file
    !! Local variables
    INTEGER, PARAMETER :: IO = 1
    INTEGER :: II
    CHARACTER(len=80) :: temp

    IF (IsRoot(global_grid)) THEN
       OPEN(UNIT=IO, FILE=file)
       WRITE(IO,'(A)') "values:"
       DO II = 1, SIZE(values)
          WRITE(temp,*) values(II)
          WRITE(IO,"(A A)") "-", TRIM(temp)
       END DO
       CLOSE(IO)
    END IF
  END SUBROUTINE WriteOutput
END PROGRAM IdempotentSub
