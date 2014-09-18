PROGRAM example1
! Comments start with an exclamation mark
        IMPLICIT NONE
        INTEGER :: hours, mins, secs, temp
        PRINT *, 'Type the hours, minutes and seconds'
        READ *, hours, mins, secs
        temp = 60*( hours*60 + mins) + secs
        WRITE (*,*) 'Time in seconds =', temp
END PROGRAM example1
