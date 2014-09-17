PROGRAM one
  INTEGER :: X=-99

  outer: DO
    READ *, X
    IF (X.EQ.0) EXIT

    IF (X.EQ.13) THEN
      PRINT *, "unlucky!"
      STOP
    END IF

    READ *, X

    DO WHILE (X.NE.1)
      IF (MOD(X,2).EQ.0) THEN
        X = X / 2
        PRINT *, X
      ELSE IF (MOD(X,2).EQ.1) THEN
        X = X*3 + 1
        PRINT *, X
      END IF

    END DO

  END DO outer

END PROGRAM one
