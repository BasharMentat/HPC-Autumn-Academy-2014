PROGRAM Newton
USE double
IMPLICIT NONE

REAL (KIND = DP) :: f_x, root

PRINT *, "X Log(X) is :"
READ *, f_x

PRINT *, "then x is:"

CALL NewtonRaphson(f_x, root)
PRINT *, root

CONTAINS
  REAL (KIND = DP) function init(x) RESULT(res)
    REAL (KIND = DP), INTENT(IN) :: x
    res = x * LOG(x)
    IF (res.LT.1.0_DP) THEN
      PRINT *, "Invalid value!"
      STOP
    END IF
  END function init

  REAL (KIND = DP) function deriv(x) RESULT(res)
    REAL (KIND = DP), INTENT(IN) :: x
    res = LOG(X) + 1
  END Function deriv

  SUBROUTINE NewtonRaphson(A, root)
    REAL (KIND = DP), INTENT(IN)  :: A
    REAL (KIND = DP) :: x1, x0, decrement
    REAL (KIND = DP), INTENT(OUT) :: root

    x0 = -1.23e20_DP
    x1 = A

    DO WHILE(x1.NE.x0)
      x0 = x1
      decrement = (init(x0) - A) / deriv(x0)
      x1 = x0 - decrement

    END DO

    root = x1
  END SUBROUTINE NewtonRaphson
END PROGRAM Newton
