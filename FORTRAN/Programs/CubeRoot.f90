PROGRAM CubeRoot
    USE double
    IMPLICIT NONE
    REAL(KIND=DP) :: target, result
    INTEGER :: i, k
mainloop: DO
        PRINT *, 'Type in a real number'
        READ (*, *, IOSTAT=k) target
        IF (k < 0) THEN
            STOP
        ELSE IF (k > 0) THEN
            PRINT *, 'Some sort of horrible I/O error'
            STOP 1
        END IF 
        IF (target .EQ. 0.0_DP) THEN
            PRINT *, 'The cube root of zero is, er, zero'
            CYCLE
        END IF
        result = Newton(target)
        PRINT *, result, result**3
    END DO mainloop

CONTAINS

    FUNCTION Newton (source)
        REAL(KIND=DP) :: Newton
        REAL(KIND=DP), INTENT(IN) :: source
        REAL(KIND=DP) :: current
        REAL(KIND=DP), DIMENSION(5) :: previous
!
! This is cheating, but the hardest part of Newton-Raphson solution of
! Nth roots is getting the starting value, and doing so properly would
! merely be confusing.  So use a horrible hack to get an approximation.
!
        current = 1.1*CMPLX(source,KIND=KIND(0.0))**0.3
        DO i = 1,5
            previous(i) = 0.0_DP
        END DO
loop:   DO
            current = current - &
                value(current,source)/derivative(current)
            PRINT *, current
            DO i = 1,5
                if (current .EQ. previous(i)) EXIT loop
            END DO
            DO i = 1,4
                previous(i+1) = previous(i)
            END DO
            previous(1) = current
        END DO loop
        Newton = current
    END FUNCTION Newton

FUNCTION value (arg, targ)
    USE double
    IMPLICIT NONE
    REAL(KIND=DP) :: value, arg, targ
    value = arg**3-targ
END FUNCTION value

FUNCTION derivative (arg)
    USE double
    IMPLICIT NONE
    REAL(KIND=DP) :: derivative, arg
    derivative = 3*arg**2
END FUNCTION derivative

END PROGRAM CubeRoot
