PROGRAM test
REAL, DIMENSION(2,3) :: X
INTEGER, DIMENSION(2)  :: size_dim
READ *, X
size_dim = shape(X)
PRINT *, size_dim(1)
END PROGRAM test
