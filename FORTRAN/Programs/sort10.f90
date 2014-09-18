PROGRAM sort10
        IMPLICIT NONE
        INTEGER, DIMENSION(1:10) :: nums
        INTEGER :: temp, J, K
! --- Read in the data
        WRITE (UNIT=*, FMT=*) 'Type ten integers each on a new line' 
        DO J = 1, 10
                READ (UNIT=*, FMT=*) nums(J)
        END DO 
! --- Sort the numbers into ascending order of magnitude 
L1:     DO J = 1, 9
L2:             DO K = J+1, 10
                        IF(nums(J) > nums(K)) THEN
                                temp = nums(K)
                                nums(K) = nums(J)
                                nums(J) = temp
                        END IF
                END DO L2
        END DO L1
! --- Write out the sorted list
        DO J = 1, 10
                WRITE (UNIT=*, FMT=*) 'Rank ', J, ' Value is ', nums(J)
        END DO 
END PROGRAM sort10
