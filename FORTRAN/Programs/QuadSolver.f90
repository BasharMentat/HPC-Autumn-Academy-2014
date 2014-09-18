PROGRAM QES
    IMPLICIT NONE
    INTEGER :: a, b, c, D
    REAL  :: Real_Part, Imag_Part
    PRINT *, "Type in values for a, b and c"
    READ *, a, b, c
match: IF (a /= 0) THEN
        ! Calculate discriminant
        D = b*b-4*a*c
        IF (D == 0) THEN         ! one root
            PRINT *, "Root is ", -b/(2.0*a)
        ELSE IF (D > 0) THEN       ! real roots
            PRINT *, "Roots are", (-b+SQRT(REAL(D)))/(2.0*a), &
                "and", (-b-SQRT(REAL(D)))/(2.0*a)
        ELSE               ! complex roots
            Real_Part = -b/(2.0*a)
            Imag_Part = (SQRT(REAL(-D))/(2.0*a))
            PRINT *, "1st Root", Real_Part, "+", Imag_Part, "i"
            PRINT *, "2nd Root", Real_Part, "-", Imag_Part, "i"
        END IF
    ELSE match               ! a == 0
        PRINT *, "Not a quadratic equation"
    END IF match
END PROGRAM QES
