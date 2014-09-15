!Program that calculates the sum of 2 real numbers with a function
PROGRAM sum_func

IMPLICIT NONE
REAL :: A, B, S = 0.0, funcsum

PRINT *, "insert first number:"
READ *, A
PRINT *, "insert second number:"
READ *, B

S = funcsum(A,B)
PRINT *, "sum is:",S

END PROGRAM sum_func

FUNCTION funcsum(A,B)
  REAL :: funcsum
  funcsum = A + B
  RETURN
END FUNCTION funcsum
