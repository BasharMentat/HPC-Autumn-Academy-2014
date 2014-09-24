MODULE matrix_operations
  USE double
  IMPLICIT NONE

CONTAINS

  SUBROUTINE LU_Crout(A,L,U)
       REAL(KIND=DP), DIMENSION(:,:), INTENT(IN) :: A
       REAL(KIND=DP), DIMENSION(:,:), INTENT(OUT):: L,U
       INTEGER :: m,n, I,J,K
       m = size(A,1)
       n = size(A,2)

       DO I=1,m
         L(I,1) = A(I,1)
         U(I,I) = 1.0_DP
       END DO

       DO J = 2,m
         U(1, J) = A(1, J) / L(1, 1)
       END DO

       DO I = 2,m

           DO j = 2,I

             DO K=1,J
               L(I, J) = A(I, J) - L(I, K) * U( K, J)
             END DO

           END DO

           DO J = I+1,m

             DO K=1,J
               U(I, J) = (A(I, J) - L(I,K) * U(K,J)) / L(I, I)
             END DO

           END DO

       END DO

  END SUBROUTINE LU_Crout

  SUBROUTINE LU_Doolittle(A,L,U)
  !! Doolittle algorithm for LU decomposition

  !!Declarations
  REAL(KIND=DP), DIMENSION(:,:), INTENT(IN) :: A
  REAL(KIND=DP), DIMENSION(:,:), INTENT(OUT):: L,U
  INTEGER :: m,n, I,J

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


      !!Underflow check
      IF (ABS(U(J,J)) > 1.0E-12_DP) THEN
        !!calculate L elements by going to the corresponding column in U
        !!and scaling it by the diagonal element
        L(I,J) = U(I,J) / U(J,J)
      END IF
      U(I,:) = U(I,:) - L(I,J)*U(J,:) !!effectively U = U - |L><U|

    END DO

  END DO

  END SUBROUTINE LU_Doolittle

END MODULE matrix_operations
