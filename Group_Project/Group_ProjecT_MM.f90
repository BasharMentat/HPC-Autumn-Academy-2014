PROGRAM group_project
USE double
IMPLICIT NONE

  INTERFACE

    FUNCTION Brute(A,B,C,M,N,O,P) RESULT(C)
    !! Function that that multiplies two matrices A(m x n) and B(o x p)
    !! and returns the result as a matrix C (m x p) and gives a warning if
    !! n is not equal to o and stops the execution since multiplication is not
    !! possible

      INTEGER, INTENT(IN):: M,N,O,P
      INTEGER :: I,J,K
      REAL(KIND=DP), INTENT(IN) :: A(M,N), B(O,P)
      REAL(KIND=DP), INTENT(OUT) :: C(M,P)

  END INTERFACE

    END FUNCTION

  INTERFACE

    FUNCTION MulTrans(A,B,C,M,N,O,P) RESULT(C)
    !! Function that that multiplies two matrices A(m x n) and B(o x p)
    !! and returns the result as a matrix C (m x p) and gives a warning if
    !! n is not equal to o and stops the execution since multiplication is not
    !! possible

      INTEGER, INTENT(IN):: M,N,O,P
      INTEGER :: I,J,K
      REAL(KIND=DP), INTENT(IN) :: A(M,N), B(O,P)
      REAL(KIND=DP), INTENT(OUT) :: C(M,P)

    END FUNCTION

  END INTERFACE


CONTAINS

  FUNCTION Brute(A,B,C,M,N,O,P) RESULT(C)
  !! Function that that multiplies two matrices A(m x n) and B(o x p)
  !! and returns the result as a matrix C (m x p) and gives a warning if
  !! n is not equal to o and stops the execution since multiplication is not
  !! possible

  IMPLICIT NONE
  INTEGER :: M,N,O,P
  INTEGER :: I,J,K
  REAL(KIND=DP), INTENT(IN) :: A(M,N), B(O,P)
  REAL(KIND=DP), INTENT(OUT):: C(M,P)

  IF (N.NE.O) THEN
    PRINT *, "Warning: Invalid Matrix dimensions! Multiplication not possible"
    STOP
  END IF

  !! Initialize result matrixas 0.0 in double precision
  C = 0.0_DP

  DO I=1,SIZE(B,2) !!outer loop from 1 to P
    DO J=1,SIZE(A,1) !! outer loop from 1 to M

      DO K=1,SIZE(A,2) !! inner loop on multiplying index from 1 to n (N = O)

        C(J,I) = C(J,I) + A(J,K)*B(K,J) !! A istraversed by rows, B by columns

        !!A is traversed inefficiently in the FORTRAN case (fastest index should
        !! vary fastest)


      END DO

    END DO
  END DO

  END Function Brute

  FUNCTION MulTrans(A,B,M,N,O,P) RESULT(C)
  !! Brute-force multiplication with transposition for memory-efficiency
  !! Only works for square matrices

  IMPLICIT NONE
  INTEGER :: M,N,O,P
  INTEGER :: I,J,K
  REAL(KIND=DP), INTENT(IN) :: A(M,N), B(O,P)
  REAL(KIND=DP), INENT(OUT) :: C(M,P)

  IF (N.NE.O) THEN
    PRINT *, "Warning: Invalid Matrix dimensions! Multiplication not possible"
    STOP
  END IF

  IF ((M.NE.P).OR.(M.NE.N)) THEN


  !! Initialize result matrixas 0.0 in double precision
  C = 0.0_DP
  !! Calculate the transpose of A with the transpose intrinsic
  A = TRANSPOSE(A)


  !! using size avoids problems mixing up the index limits

  DO I=1,SIZE(B,2)
    DO J=1,SIZE(A,1)
      DO K=1,SIZE(A,2)

        C(J,I) = C(J,I) + A(K,J)*B(K,J)

      END DO
    END DO
  END DO

END Function MulTrans

SUBROUTINE MatPrint(A,m,n)
  IMPLICIT NONE
  INTEGER :: m,n, I,J
  REAL,  DIMENSION (m,n) :: A

  DO I=1,M
    print *, (A(I,J), J=1,N)
  END DO

END SUBROUTINE MatPrint

END PROGRAM group_project
