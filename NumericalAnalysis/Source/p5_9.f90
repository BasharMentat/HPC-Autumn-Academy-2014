<<<<<<< HEAD
PROGRAM MaTMuL
INTEGER :: m,n,o,p
REAL, DIMENSION(:,:), ALLOCATABLE :: result, left, right

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
result = 0
CALL MyMatmul(result,left,right,m,n,o,p)

PRINT *, "the result of the matrix multiplication is:"
CALL MatPrint(result,m,p)

DEALLOCATE(left)
DEALLOCATE(right)

END PROGRAM MaTMuL

SUBROUTINE MyMatmul(result,left,right,M,N,O,P)
  IMPLICIT NONE
  INTEGER :: M,N,O,P
  INTEGER :: I,J,K
  REAL :: result(M,P), left(M,N), right(O,P)

  IF (N.NE.O) THEN
    PRINT *, "Warning: Invalid Matrix dimensions! Multiplication not possible"
  END IF

  DO J=1,M
    DO K=1,P

      result(J,K)= SUM( left(J,:)*right(:,K))

    END DO
  END DO

END SUBROUTINE MyMatmul


SUBROUTINE MatPrint(A,m,n)
  IMPLICIT NONE
  INTEGER :: m,n, I,J
  REAL,  DIMENSION (m,n) :: A

  DO I=1,M
    print *, (A(I,J), J=1,N)
  END DO

END SUBROUTINE MatPrint
=======
PROGRAM hand_matmul

END PROGRAM hand_matmul
>>>>>>> 9468f27979f3a86b8e9ba3cabbff5c345da8d490
