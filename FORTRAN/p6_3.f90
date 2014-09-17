PROGRAM gamble
INTEGER :: I, SCORE
PRINT *, "Throwing dice"

DO I=1,50
  SCORE = dice()
  PRINT *, "Score is:",SCORE
END DO

CONTAINS
  REAL function dice()
  REAL :: Z, Y

  CALL RANDOM_NUMBER(Z)

  CALL RANDOM_NUMBER(Y)

  dice = INT((Z*6) + 1) + INT((Y*6) + 1)
  RETURN
  END function dice
END PROGRAM gamble
