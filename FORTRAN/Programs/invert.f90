PROGRAM INVERT
    IMPLICIT NONE
    REAL :: Value, Inverse
    PRINT *, "Type in a value to invert"
    READ *, Value
    Inverse = 1.0/Value
    PRINT *, "Value", Value, "  Inverse", Inverse
END PROGRAM INVERT
