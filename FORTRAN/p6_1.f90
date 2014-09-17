PROGRAM SubRoutine
  REAL :: result, second, third
  PRINT *, "input two numbers"
  READ *, second, third

  CALL SR1(result,second,third)

END PROGRAM SubRoutine

SUBROUTINE SR1(result, second, third)
  REAL, INTENT(OUT):: result
  REAL, INTENT(IN) :: second, third

  result = second - third
  PRINT *, result

END SUBROUTINE SR1
