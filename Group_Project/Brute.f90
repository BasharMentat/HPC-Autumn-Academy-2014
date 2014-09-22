PROGRAM MaTMuL
USE double

  IMPLICIT NONE

INTEGER :: m,n,o,p
REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE :: result, left, right

PRINT *, "Reading the left matrix dimensions:"
READ *, m,n

ALLOCATE(left(m,n))
PRINT *, "Reading the left matrix to multiply:"
READ *, left

PRINT *, "Reading the right matrix dimensions:"
READ *, o,p

ALLOCATE(right(o,p))
PRINT *, "Reading the right matrix to multiply:"
READ *, right

ALLOCATE(result(m,p))
result = 0.0_DP
CALL MyMatmul(result,left,right,m,n,o,p)

PRINT *, "the result of the matrix multiplication is:"
CALL MatPrint(result,m,p)

result = MulTrans(left,right,m,n,o,p)
CALL MatPrint(result,m,p)

DEALLOCATE(left)
DEALLOCATE(right)

CONTAINS

SUBROUTINE MyMatmul(result,left,right,M,N,O,P)
  IMPLICIT NONE
  INTEGER :: M,N,O,P
  INTEGER :: I,J,K
  REAL(KIND=DP) :: result(M,P), left(M,N), right(O,P)

  IF (N.NE.O) THEN
    PRINT *, "Warning: Invalid Matrix dimensions! Multiplication not possible"
  END IF

  right = TRANSPOSE(right)

  DO J=1,M
    DO K=1,P

      result(J,K)= SUM( left(J,:)*right(K,:))

    END DO
  END DO

END SUBROUTINE MyMatmul

FUNCTION MulTrans(A,B,M,N,O,P) RESULT(C)
!! Brute-force multiplication with transposition for memory-efficiency
!! Only works for square matrices

  IMPLICIT NONE
  INTEGER :: M,N,O,P
  INTEGER :: I,J,K
  REAL(KIND=DP) :: C(M,P), A(M,N), B(O,P)

  IF (N.NE.O) THEN
    PRINT *, "Warning: Invalid Matrix dimensions! Multiplication not possible"
    STOP
  END IF

!! Initialize result matrixas 0.0 in double precision

 C = 0.0_DP

  PRINT *, SIZE(A)
!! using size avoids problems mixing up the index limits
  A = TRANSPOSE(A)
  PRINT *, SIZE(A)

  DO I=1,SIZE(B,2)
    DO J=1,SIZE(A,2)
      DO K=1,SIZE(A,1)

        C(J,I) = C(J,I) + A(K,J)*B(K,J)

      END DO
    END DO
  END DO

  PRINT *, B
  PRINT *, C

  A = TRANSPOSE(A)

END Function MulTrans


SUBROUTINE MatPrint(A,m,n)
  IMPLICIT NONE
  INTEGER :: m,n, I,J
  REAL(KIND=DP),  DIMENSION (m,n) :: A

  DO I=1,M
    print *, (A(I,J), J=1,N)
  END DO

END SUBROUTINE MatPrint

END PROGRAM MaTMuL
