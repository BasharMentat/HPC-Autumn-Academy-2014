PROGRAM function_example
  REAL :: A,B
  PRINT *, "input 2 numbers:"
  READ *, A, B

  PRINT *, "difference is", diff(a,b)

CONTAINS

    ELEMENTAL function diff(a,b)
    REAL, INTENT(IN) :: a,b
    diff = a-b
    RETURN
    END function diff

END PROGRAM function_example
