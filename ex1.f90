PROGRAM matrix_sum

REAL :: m,n
REAL MSUM

PRINT *, "enter the dimensions of the matrices:"
READ *, m, n
PRINT *, m,n

MSUM(m,n)
END PROGRAM matrix_sum

REAL FUNCTION MSUM(m,n)

  REAL DIMENSION(m,n) :: A,B,C
  INTEGER :: i,j

!Read first array
  PRINT *, "Read the first array entry by entry"
  DO i = 1,m
    DO j =1,n
      READ (10,*) A(i,j)
    ENDDO
  ENDDO

!Read second array
  PRINT *, "Read the second array entry by entry"
  DO i = 1,m
    DO j =1,n
      READ (10,*) A(i,j)
    ENDDO
  ENDDO

!Calculate the sum
  PRINT *, "Calculate the sum of the two arrays"
  DO i = 1,m
    DO j =1,n
      C(i,j) = A(i,j) + B(i,j)
    ENDDO
  ENDDO

!Print the sum of the two arrays
  PRINT *, "The sum is:"
  DO i = 1,m
    DO j =1,n
      PRINT(10.*) C(i,j)
    ENDDO
  ENDDO

RETURN
END
