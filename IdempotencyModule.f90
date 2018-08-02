MODULE IdempotencyModule
  USE DataTypesModule, ONLY: NTREAL
  USE PSMatrixModule, ONLY : Matrix_ps, GetMatrixTripletList
  USE SetModule, ONLY : Set_t
  USE TripletListModule, ONLY : TripletList_r, ConstructTripletList, &
       & DestructTripletList, GetTripletAt
  USE TripletModule, ONLY : Triplet_r
  IMPLICIT NONE
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  PUBLIC :: ComputeIdempotency
CONTAINS!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  SUBROUTINE ComputeIdempotency(Mat, sets, threshold)
    !> Matrix to compute
    TYPE(Matrix_ps), INTENT(IN) :: Mat
    !> Sets to compute
    TYPE(Set_t), DIMENSION(:), INTENT(IN) :: sets
    !> Threshold for matrix multiplication
    REAL(NTREAL), INTENT(IN) :: threshold
    !! Local Variables
    TYPE(TripletList_r) :: triplets
    TYPE(TripletList_r) :: sublist
    !! Temporary Variables
    INTEGER :: II

    CALL GetMatrixTripletList(Mat, triplets)

    DO II = 1, SIZE(sets)
       CALL ConstructTripletList(sublist)
       CALL FilterTriplets(triplets, sublist, sets(II))
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
    INTEGER :: II

    DO II = 1, start%CurrentSize
       CALL GetTripletAt(start, II, triplet)
    END DO
  END SUBROUTINE
END MODULE IdempotencyModule
