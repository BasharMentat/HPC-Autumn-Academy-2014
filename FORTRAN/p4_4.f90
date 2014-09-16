PROGRAM triangle
  INTEGER :: A,B,C
  PRINT *, "Sides of length"
  READ *, A,B,C

  IF ((A.EQ.B).AND.(B.EQ.C)) THEN
    PRINT *, "Equilateral"
  ELSE IF ((A.EQ.B).OR.(B.EQ.C).OR.(C.EQ.A)) THEN
    PRINT *, "Isoceles"
  ELSE
    PRINT *, "Scalene"
  ENDIF

END PROGRAM triangle
