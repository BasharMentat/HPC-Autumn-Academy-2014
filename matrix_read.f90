!program that reads a matrix using a function
PROGRAM read_mat

IMPLICIT NONE

INTEGER :: i,j

REAL, DIMENSION(2,3) :: A

DO i=1,2
  DO j=1,3
    READ (*,*) A(i,j)
  ENDDO
ENDDO

DO i=1,2
  DO j=1,3
    WRITE(*,1F1.1, ADVANCE = "NO")  A(i,j)
  ENDDO
  WRITE(*,*) ""
ENDDO


END PROGRAM read_mat
