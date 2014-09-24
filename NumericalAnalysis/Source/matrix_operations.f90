MODULE matrix_operations
  USE double
  IMPLICIT NONE

CONTAINS

  SUBROUTINE LU_Doolittle(A,L,U)
  !! Doolittle algorithm for LU decomposition

  !!Declarations
  REAL(KIND=DP), DIMENSION(:,:), INTENT(IN) :: A
  REAL(KIND=DP), DIMENSION(:,:), INTENT(OUT):: L,U
  INTEGER :: m,n, I,J, COUNT = 0

  m = SIZE(A,1)
  n = SIZE(A,2)

  PRINT *, "Size of A is:", m,n

  IF (m.NE.n) THEN !! check that the matrix is squared
    PRINT *, "Warning : Matrix is not square! Cannot decompose."
    STOP
  END IF

  L=0.0_DP

  !!Initialisation (L as unit matrix)
  DO I=1,m
    L(I,I) = 1.0_DP
  END DO

  !!Initialise U to be equal to A
  U=A

  DO I=1,m


    DO J= 1, I-1

      PRINT *,COUNT
      !!Underflow check
      IF (ABS(U(J,J)) > 1.0E-12_DP) THEN
        !!calculate L elements by going to the corresponding column in U
        !!and scaling it by the diagonal element
        L(I,J) = U(I,J) / U(J,J)
      END IF
      U(I,:) = U(I,:) - L(I,J)*U(J,:) !!effectively U = U - |L><U|
      COUNT = COUNT + 1
    END DO

  END DO

  END SUBROUTINE LU_Doolittle
  
END MODULE matrix_operations
