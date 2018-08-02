MODULE IdempotencyModule
  USE DataTypesModule, ONLY: NTREAL
  USE PSMatrixAlgebraModule, ONLY : MatrixMultiply, IncrementMatrix, &
       & MatrixNorm
  USE PSMatrixModule, ONLY : Matrix_ps, GetMatrixTripletList, &
       & ConstructEmptyMatrix, FillMatrixFromTripletList, DestructMatrix, &
       & PrintMatrix
  USE SetModule, ONLY : Set_t
  USE TripletListModule, ONLY : TripletList_r, ConstructTripletList, &
       & DestructTripletList, GetTripletAt, AppendToTripletList
  USE TripletModule, ONLY : Triplet_r
  IMPLICIT NONE
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  PUBLIC :: ComputeIdempotency
CONTAINS!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  SUBROUTINE ComputeIdempotency(Mat, sets, threshold, values)
    !> Matrix to compute
    TYPE(Matrix_ps), INTENT(IN) :: Mat
    !> Sets to compute
    TYPE(Set_t), DIMENSION(:), INTENT(IN) :: sets
    !> Threshold for matrix multiplication
    REAL(NTREAL), INTENT(IN) :: threshold
    !> Values we are computing
    REAL(NTREAL), DIMENSION(:), INTENT(INOUT) :: values
    !! Local Variables
    TYPE(TripletList_r) :: triplets
    TYPE(TripletList_r) :: sublist
    TYPE(Matrix_ps) :: submat
    !! Temporary Variables
    INTEGER :: II

    CALL GetMatrixTripletList(Mat, triplets)

    DO II = 1, SIZE(sets)
       CALL ConstructTripletList(sublist)
       CALL FilterTriplets(triplets, sublist, sets(II))

       CALL ConstructEmptyMatrix(submat, Mat)
       CALL FillMatrixFromTripletList(submat, sublist)

       values(II) = MatCompute(submat, threshold)

       CALL DestructMatrix(submat)
       CALL DestructTripletList(sublist)
    END DO

    !! Cleanup
    CALL DestructTripletList(triplets)
  END SUBROUTINE ComputeIdempotency
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  SUBROUTINE FilterTriplets(start, filtered, set)
    !> Starting triplets
    TYPE(TripletList_r), INTENT(IN) :: start
    !> Filtered Triplets
    TYPE(TripletList_r), INTENT(INOUT) :: filtered
    !> Set for filtering
    TYPE(Set_t) :: set
    !! Temporary Variables
    TYPE(Triplet_r) :: triplet
    LOGICAL :: row_in, col_in
    INTEGER :: II

    DO II = 1, start%CurrentSize
       CALL GetTripletAt(start, II, triplet)
       row_in = IsInSet(triplet%index_row, set%index_list)
       col_in = IsInSet(triplet%index_column, set%index_list)

       IF (row_in .AND. col_in) THEN
          CALL AppendToTripletList(filtered, triplet)
       END IF
    END DO
  END SUBROUTINE FilterTriplets
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  FUNCTION IsInSet(val, set) RESULT(IsIn)
    !> Value to check
    INTEGER, INTENT(IN) :: val
    !> List of integers
    INTEGER, DIMENSION(:), INTENT(IN) :: set
    !> Return value
    LOGICAL :: IsIn
    !! Local variables
    INTEGER :: II

    IsIn = .FALSE.

    DO II = 1, SIZE(set)
       IF (val .EQ. set(II)) THEN
          IsIn = .TRUE.
          EXIT
       END IF
    END DO
  END FUNCTION IsInSet
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  FUNCTION MatCompute(matrix, threshold) RESULT(deviation)
    !> Matrix to compute
    TYPE(Matrix_ps) :: matrix
    !> Result is the deviation from idempotency of this matrix
    REAL(NTREAL) :: deviation
    !> Threshold for multiplciation
    REAL(NTREAL) :: threshold
    !! Temporary Matrix
    TYPE(Matrix_ps) :: mat2

    CALL MatrixMultiply(matrix, matrix, mat2, threshold_in=threshold)
    CALL IncrementMatrix(matrix, mat2, alpha_in=REAL(-1.0,NTREAL), &
         & threshold_in=threshold)
    deviation = MatrixNorm(mat2)
  END FUNCTION MatCompute
END MODULE IdempotencyModule
