program helloworld
  use omp_lib
  implicit none

!$OMP PARALLEL
!$OMP CRITICAL
  print *,'Hello from thread number',OMP_GET_THREAD_NUM()
!$OMP END CRITICAL
!$OMP END PARALLEL

PRINT *, "outside the parallel region thread number is:", OMP_GET_THREAD_NUM()
end program helloworld
