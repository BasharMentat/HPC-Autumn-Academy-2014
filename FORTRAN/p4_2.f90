PROGRAM vector
  INTEGER :: length, angle
  REAL :: angle_r, x, y
  REAL, PARAMETER :: pi = 3.14159
  PRINT *, "Input the radius and angle of your vector"
  READ *, length, angle
  angle_r = angle * (2 * pi) / 180
  x = length * COS(angle_r)
  y = length * SIN(angle_r)
  PRINT *, "(",x,y,")"
END PROGRAM vector
