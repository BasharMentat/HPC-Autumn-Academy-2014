!program that reads a matrix using a function
PROGRAM read_mat

INTEGER :: i,j

REAL, DIMENSION(2,3) :: A

DO i=1,2
  DO j=1,3
    READ (*,*) A(i,j)
  END DO
END DO

DO i=1,2
  WRITE(*,10)  (A(i,j),j=1,3)
END DO
10  FORMAT(10f5.1)

END PROGRAM read_mat
