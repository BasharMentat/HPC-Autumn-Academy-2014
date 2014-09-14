!program that reads a matrix using a function
PROGRAM read_mat

IMPLICIT NONE

INTEGER :: m,n,i,j
PRINT *, "insert dimensions"
READ *, m,n

REAL, DIMENSION(m,n) :: A

DO i=1,m
  DO j=1,n
    READ (10,*) A(i,j)
  ENDDO
ENDDO

DO i=1,m
  DO j=1,n
    PRINT *, A(i,j)
  ENDDO
ENDDO


END PROGRAM read_mat
