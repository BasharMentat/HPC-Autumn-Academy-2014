PROGRAM sin_taylor
  IMPLICIT NONE
  DOUBLE PRECISION :: x, fullterm, Sum
  INTEGER :: I,N, Order, facindex

  PRINT *, "X is :"
  READ *, x

  x = shift_to_pi(x)


  PRINT *, "Truncation of Taylor Series at order:"
  READ *, Order

  N = INT((Order - 1) / 2 ) + 1
  Sum = 0.0

  DO I=1,N

    facindex = 2*(I-1) + 1

    IF (MOD(I,2).EQ.1) THEN
      fullterm = term(x,I)/factorial(facindex)
    ELSE
      fullterm = (-1)*(term(x,I)/factorial(facindex))
    END IF
    Sum = Sum + fullterm

  END DO

  PRINT *, "Result is :", Sum, "up to O(", Order, ")"
  PRINT *, "Error (relative to FORTRAN intrinsic) is :", Sum - SIN(x)


CONTAINS

  DOUBLEPRECISION FUNCTION shift_to_pi(x) Result(shift)
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(IN) :: x
    DOUBLE PRECISION, PARAMETER :: pi = 3.14159

    shift = MOD(x,2*pi)
    IF (shift.LT.pi) THEN
      RETURN
    ELSE
      shift = 2*pi - shift
    END IF

  END FUNCTION shift_to_pi

  RECURSIVE FUNCTION factorial(n) RESULT(res)
    IMPLICIT NONE
    DOUBLE PRECISION :: res
    INTEGER, INTENT(IN) :: n
    IF (N.EQ.0) THEN
      res = 1
    ELSE
      res = n*factorial(n-1)

    END IF
  END FUNCTION factorial

  RECURSIVE FUNCTION term(x,n) RESULT (t)
    IMPLICIT NONE
    DOUBLE PRECISION :: t
    DOUBLE PRECISION, INTENT(IN) :: x
    INTEGER, INTENT(IN) :: n

    IF (N.EQ.1) THEN
      t = x
    ELSE
      t = x*x*term(x,n-1)
    END IF
  END FUNCTION term

END PROGRAM sin_taylor
