PROGRAM message
    IMPLICIT NONE
    CHARACTER :: mess*72, date*14, name*40 
    mess = 'Program run on' 
    mess(30:) = 'by' 
    READ *, date, name 
    mess(16:29) = date 
    mess(33:) = name
    PRINT *, mess
END PROGRAM message
