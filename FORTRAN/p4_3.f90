PROGRAM vector
  INTEGER :: X, I, COUNT = 0
  PRINT *, "Introduce 10 numbers"

  DO I = 1,10
    READ *, X
    IF ((X>0).AND.(MOD(X,5).EQ.0)) THEN
      COUNT = COUNT + 1
    END IF
  END DO

  PRINT *,"Number of multiples of 5 is : ", COUNT

END PROGRAM vector
