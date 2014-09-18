PROGRAM MAIN
    USE double
    IMPLICIT NONE
    REAL(KIND=dp), DIMENSION(5, 5) :: A = 0.0_dp
    REAL(KIND=dp), DIMENSION(5) :: Z
    INTEGER :: I, N
    DO N = 1,10
        CALL RANDOM_NUMBER(Z)
        DO I = 1,5
            A(:, I) = A(:, I) + Z*Z(I)
        END DO
    END DO
    WRITE (*,'(5(1X,5F10.6/))') A
    CALL CHOLESKY(A)
    DO I = 1,5
        A(:I-1,I) = 0.0
    END DO
    WRITE (*, '(5(1X,5F10.6/))') A
    WRITE (*, '(5(1X,5F10.6/))') MATMUL(A, TRANSPOSE(A))
CONTAINS

SUBROUTINE CHOLESKY(A)
    USE double
    IMPLICIT NONE
    INTEGER :: J, N
    REAL(KIND=dp) :: A(:, :), X
    N = UBOUND(A,1)
    DO J = 1, N
        X = SQRT(A(J,J)-DOT_PRODUCT(A(J, :J-1), A(J, :J-1)))
        A(J,J) = X
        IF (J < N) &
            A(J+1:, J) = (A(J+1:, J) - &
                MATMUL(A(J+1:, :J-1), A(J,: J-1))) / X
    END DO
END SUBROUTINE CHOLESKY

END PROGRAM MAIN
