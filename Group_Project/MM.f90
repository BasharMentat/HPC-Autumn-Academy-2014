PROGRAM matrix_group_exercise

    IMPLICIT NONE

    INTERFACE
        FUNCTION   mat_mul_no_transpose(A,B) result(C)

            real(8) , dimension(:,:) , intent(in) :: A , B
            real(8) , dimension(size(A,1),size(A,2)) :: C

        END FUNCTION
    END INTERFACE

    INTERFACE
        FUNCTION   mat_mul_with_transpose(A,B) result(C)

            real(8) , dimension(:,:) , intent(inout) :: A , B
            real(8) , dimension(size(A,1),size(A,1)) :: C

        END FUNCTION
    END INTERFACE

    !! - Variable declarations
    real(8) , allocatable , dimension(:,:) :: A , B , C 

    integer(8) ::  choice  , t

    integer(8) ::  mb, n

    integer(8) :: i , j , k 

    real(8) :: t1 , t2 , ta , tb , tc, td    


    !! - Get the user to input the size of the matrices. 
    print *, "What is the size of the matrix (n)?"
    print *, "Remember this will require 3*8*n*n bytes"
    read *, n
    !! - calculate the number of megabytes the matrices will require to store.
    mb = 3*8*n*n/1000000

    print *, "This will require",mb,"MB"
    print *, ""
    print *, ""

    !! - Allocate and initialise the matrices to a given value
    allocate(A(n,n))
    allocate(B(n,n))
    allocate(C(n,n))
    A = 1.0
    B = 2.0
    C = 0.0

    
    !! - Get the user input an option for how the program will run
    print *, "Select from the following:"
    print *, "1 for matmul no transpose"
    print *, "2 for matmul with transpose"
    print *, "3 for DGEMM"
    print *, "4 for all the above"
    read *, choice

    print *, "How many times do you want to repeat?"
    read *, t
    call cpu_time(t1) !! - This will time the overall main block of computation

    DO  I = 1 , t
    !! - Use the function that doesn't take the transpose and so the B matrix is
    !! - accessed by striding through the memory (bad!)
    IF (choice .eq. 1) C = mat_mul_no_transpose(A,B)    
    !! - Use the function that also takes the transpose. The function retransposes the 
    !! - matrix so that on output B is the same as input
    IF (choice .eq. 2) C = mat_mul_with_transpose(A,B)
    !! - Use the intrinsic matmul function
    IF (choice .eq. 3) C = matmul(A,B)
    END DO

    !!  - Do all three and time each individually
    IF (choice .eq. 4) THEN

        call cpu_time(ta)
        C = mat_mul_no_transpose(A,B)
        call cpu_time(tb)
        C = mat_mul_with_transpose(A,B)  
        call cpu_time(tc)
        C = matmul(A,B)
        call cpu_time(td)

        print *, "Time w/o transpose:     ",tb-ta,"seconds"
        print *, "Time w/ transpose:        ",tc-tb,"seconds"
        print *, "Time with intrinsic DGEMM:",td-tc,"seconds"

    END IF

    call cpu_time(t2)


    print *, "total time",t2-t1

END PROGRAM matrix_group_exercise

!! - This function multiplies two matrices A and B the natural way 
FUNCTION mat_mul_no_transpose(A,B) RESULT(C)

    real(8) , dimension(:,:) , intent(in) :: A , B

    real(8) , dimension(size(A,1),size(A,2)) :: C

    integer(8) :: i , j , k

    DO i = 1 , size(A,1)
        DO j = 1 , size(B,2)

            C(i,j) = dot_product(A(:,i),B(j,:))
        
        END DO
    END DO

    return

END FUNCTION mat_mul_no_transpose


!! - This function multiplies two matrices A and B but first transposes the matrix B
!! - such that when performing the dot_product function the vectors are contiguous
!! - in memory.

FUNCTION mat_mul_with_transpose(A,B) RESULT(C)

    real(8) , dimension(:,:) , intent(inout) :: A , B

    real(8) , dimension(size(A,1),size(A,2)) :: C

    integer(8) :: i , j

    B = transpose(B) !! - Take the transpose of the matrix

    DO i = 1 , size(A,2)    !! - Calculate each coefficent of the matrix
        DO j = 1 , size(B,2)
    
           C(i,j) = dot_product(A(:,i),B(:,j))
    
        END DO
    END DO

    B = transpose(B)    !! - Retranspose the matrix so on output the matrix is the same.

    return

END FUNCTION 


