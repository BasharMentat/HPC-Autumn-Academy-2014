PROGRAM GaussianElim

!!Program used to investigate solving linear systems (Gaussian elimination).The
!!program uses LU decomposition carried with both the Doolittle(outer-product,
!!lower triangular matrix multiplication) and the pivoting generalized Crout
!!algorithm.

!!Double precision
USE double
USE matrix_operations

IMPLICIT NONE

REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE :: A,L,U
REAL(KIND=DP), DIMENSION(:), ALLOCATABLE   :: B
INTEGER :: m,n, I,J

PRINT *, "Reading the matrix dimensions:"
READ *, m,n

ALLOCATE(A(m,n))
ALLOCATE(L(m,n))
ALLOCATE(U(m,n))

PRINT *, "Reading the matrix to decompose:"
READ *, A

CALL LU_Crout(A,L,U)

PRINT *, "Lower triangular part"
CALL MatPrint(L,m,n)
PRINT *, "Upper traingular part"
CALL MatPrint(U,m,n)

PRINT *, "check decomposition is accurate:"
CALL MatPrint(MATMUL(L,U),m,n)


DEALLOCATE(A)
DEALLOCATE(L,U)
CONTAINS

  SUBROUTINE MatPrint(A,m,n)
    IMPLICIT NONE
    INTEGER :: m,n, I,J
    REAL(KIND=DP),  DIMENSION (m,n) :: A

    DO I=1,M
      print *, (A(I,J), J=1,N)
    END DO

  END SUBROUTINE MatPrint

END PROGRAM GaussianElim
