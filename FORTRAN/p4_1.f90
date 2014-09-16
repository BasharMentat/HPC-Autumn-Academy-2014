PROGRAM LOOPS
  INTEGER :: N=2,I, COUNT =0

  COUNT = 0
  PRINT *, "first loop"
  DO I=2,N/2,3
    COUNT = COUNT + 1
  END DO

  PRINT *, COUNT
  COUNT = 0

  PRINT *, "second loop"
  DO I=1,N,4
    COUNT = COUNT + 1
  END DO
  PRINT *, COUNT
  COUNT = 0


  PRINT *, "third loop"
  DO I=3,N**2,5
    COUNT = COUNT + 1
  END DO
  PRINT *, COUNT
  COUNT = 0

  N = 15

  PRINT *, "first loop"
  DO I=2,N/2,3
    COUNT = COUNT + 1
  END DO

  PRINT *, COUNT
  COUNT = 0

  PRINT *, "second loop"
  DO I=1,N,4
    COUNT = COUNT + 1
  END DO
  PRINT *, COUNT
  COUNT = 0


  PRINT *, "third loop"
  DO I=3,N**2,5
    COUNT = COUNT + 1
  END DO
  PRINT *, COUNT
  COUNT = 0

END PROGRAM LOOPS
