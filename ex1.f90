PROGRAM matsum

IMPLICIT NONE
INTEGER :: m,n
REAL DIMENSION(m,n):: A, B, S = 0.0, funcsum

PRINT *, "insert dimensions"
READ *, m,n

read
S = funcsum(A,B)
PRINT *, "sum is:",S

END PROGRAM matsum

FUNCTION funcsum(A,B)
  REAL DIMENSION(m,n):: funcsum
  funcsum = A + B
  RETURN
END FUNCTION funcsum

FUNCTION read(X,m,n)
  INTEGER :: i, j, m, n
  REAL DIMENSION(m,n) :: X
  DO i=1,m
    DO j=1,n
      READ *, X(i,j)
    ENDDO
  ENDDO
END FUNCTION read
