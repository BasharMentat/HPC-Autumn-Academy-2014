! Don't worry about this - it is described in the next lecture.  All it
! does is declare an interface for the functions used here.

MODULE declarations
    INTERFACE

    FUNCTION copy (arg)
        IMPLICIT NONE
        REAL :: copy, arg
    END FUNCTION copy

    FUNCTION series ()
        IMPLICIT NONE
        REAL :: series
    END FUNCTION series

    FUNCTION decode (arg)
        IMPLICIT NONE
        REAL :: decode
        CHARACTER(LEN=*) :: arg
    END FUNCTION decode

    END INTERFACE
END MODULE declarations



FUNCTION copy (arg)
    IMPLICIT NONE
    REAL :: copy, arg
    copy = arg
END FUNCTION copy

FUNCTION decode (arg)
!
! This is not truly portable, but you would be unlucky to have problems
! nowadays, and ISO C actually requires '0' to '9' to be contiguous in
! numeric value.  Note that it is not the best way to code this,
! numerically, but is intended to show the issues.
!
    IMPLICIT NONE
    REAL :: decode, t, s
    CHARACTER(LEN=*) :: arg
    INTEGER :: i
    CHARACTER(LEN=*), PARAMETER :: format = &
        "('Invalid format of number: ''',A,'''')"
    t = 0.0
    s = 0.0
    DO i = 1,LEN(arg)
        IF (arg(i:i) .EQ. ' ') THEN
            CONTINUE
        ELSE IF (arg(i:i) .EQ. '.' .AND. s .EQ. 0.0) THEN
            s = 1.0
        ELSE IF (arg(i:i) .GE. '0' .AND. arg(i:i) .LE. '9') THEN
            t = t*10.0+(ICHAR(arg(i:i))-ICHAR('0'))
            s = s*0.1
        ELSE
            WRITE (*, format) arg
            STOP 1
        END IF
    END DO
    decode = t*s
END FUNCTION decode

FUNCTION series ()
!
! This is Leibnitz's series for pi, with pairs of terms collected
! together and simplified.  Please note that it is not a particularly
! poorly (or well) behaved series, and is NOT being summed very cleverly,
! but this is done deliberately to demonstrate the effect of limited
! precision.
!
    IMPLICIT NONE
    INTEGER :: n
    REAL :: series, t, y
    CHARACTER(LEN=*), PARAMETER :: format = &
        "('The series converged after ',I9,' iterations')"
    n = 0
    t = 0.0
    DO
       y = t
       t = t+2.0/((4.0*n+1.0)*(4.0*n+3.0))
       n = n+1
       IF (y .EQ. t) EXIT
    END DO
    WRITE (*, format) n
    series = 4.0*t
END FUNCTION series

PROGRAM verysimple
    USE declarations
    IMPLICIT NONE
    REAL, PARAMETER :: a  = 3.14159265358979323846, &
        b  = 314159.265358979323846, p = 1.0
    REAL :: c, d, e, f, g
    CHARACTER(LEN=100) :: buf1, buf2
    c = 3.14159265358979323846
    d = 314159.265358979323846
    OPEN (UNIT=1, FILE='inaccuracy.data', ACTION='READ')
    READ (1, '(A)') buf1
    WRITE (*, '(3X,A)') buf1(1:25)
    READ (buf1, *) e
    READ (1, '(A)') buf2
    WRITE (*, '(3X,A)') buf2(1:25)
    READ (buf2, *) f
    WRITE (*, '(1X)')
!
! Display pi and 10^5*pi calculated directly in several ways, and note
! the discrepancies.
!
    WRITE (*, '(F25.20)') a
    WRITE (*, '(F25.15)') b
    WRITE (*, '(F25.15)') 1.0e5*a
    WRITE (*, '(F25.20)') c
    WRITE (*, '(F25.15)') d
    WRITE (*, '(F25.15)') 1.0e5*c
    WRITE (*, '(F25.20)') e
    WRITE (*, '(F25.15)') f
    WRITE (*, '(F25.15)') 1.0e5*e
    WRITE (*, '(F25.20)') 4.0*atan(1.0)
    WRITE (*, '(F25.20)') 4.0*atan(p)
    WRITE (*, '(F25.15)') 4.0e5*atan(1.0)
!
! For arcane historical reasons, Fortran does not allow I/O in functions
! called from I/O lists; in THIS case, it wouldn't be a good idea even
! if in compilers that do allow it, as both are writing to the same unit.
!
    g = copy(3.14159265358979323846)
    WRITE (*, '(F25.20)') g
    g = copy(314159.265358979323846)
    WRITE (*, '(F25.15)') g
    g = copy(a)
    WRITE (*, '(F25.20)') g
    g = copy(c)
    WRITE (*, '(F25.20)') g
    WRITE (*, *)
!
! Note the loss of accuracy, especially in the last one.
!
    g = decode(buf1)
    WRITE (*, '(F25.20)') g
    g = decode(buf2)
    WRITE (*, '(F25.15)') g
    g = series()
    WRITE (*, '(F25.20)') g
END PROGRAM verysimple
