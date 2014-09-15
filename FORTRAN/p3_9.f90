PROGRAM radius
  
  REAL :: r, A_circle, V_sphere
  REAL, PARAMETER :: pi = 3.14159

  PRINT *, "type in a radius:"
  READ *, r

  A_circle = pi * (r**2)
  V_sphere = pi * (4/3) * (r**3)

  PRINT *, "Area of the coresponding circle is:", A_circle
  PRINT *, "Volume of the coresponding sphere is:", V_sphere


END PROGRAM radius
