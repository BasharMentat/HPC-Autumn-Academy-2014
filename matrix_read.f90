!program that reads a matrix using a function
PROGRAM read_mat

IMPLICIT NONE

REAL :: m,n,i,j
REAL DIMENSION(m,n) :: A

PRINT *, "insert dimensions"
READ *, m,n

DO i=1,m
  DO j=1,n
    READ *, A(i,j)
  ENDDO
ENDDO

DO i=1,m
  DO j=1,n
    PRINT *, A(i,j)
  ENDDO
ENDDO


END PROGRAM read_mat
