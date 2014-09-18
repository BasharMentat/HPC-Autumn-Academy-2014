PROGRAM char_arguments
  IMPLICIT NONE

  CHARACTER(LEN=10) :: w1, w2, w3
  INTEGER :: res

  PRINT *, "give 3 word:"
  READ *, w1, w2, w3
  PRINT *,  w1
  PRINT *,  w2
  PRINT *,  w3

  CALL char_manip(w1, w2, w3, res)

  PRINT *,"That was ", res, "letters and signs"

CONTAINS

  SUBROUTINE char_manip(w1, w2, w3, length)
    IMPLICIT NONE

    CHARACTER(LEN=*), INTENT(IN) :: w1, w2, w3
    INTEGER :: length
    PRINT  *, w1," ", w2, " ", w3
    length = LEN(w1) + LEN(w2) + LEN(w3)

  END SUBROUTINE char_manip
END PROGRAM char_arguments
