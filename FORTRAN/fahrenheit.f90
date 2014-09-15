PROGRAM MAIN
  INTEGER :: degrees_F, degrees_C;
  PRINT *, "The temperature in Fahrenheit is:"
  READ *, degrees_F
  degrees_C = 5 * (degrees_F - 32) / 9
  PRINT *, "The temperature in Celsius is: ", degrees_C
END PROGRAM
