PROGRAM main
  USE mpi
  IMPLICIT NONE
  INTEGER :: error, nprocs, myproc, namelen
  CHARACTER(LEN=MPI_MAX_PROCESSOR_NAME) :: procname

  CALL MPI_Init(error)

  CALL MPI_Comm_size (MPI_COMM_WORLD, nprocs, error)
  CALL MPI_Comm_rank (MPI_COMM_WORLD, myproc, error)
  CALL MPI_Get_processor_name(procname, namelen, error)

  PRINT *, "This is process", myproc ,"in a communicator of", nprocs, &
  &"processes running on procesor ", procname

  CALL MPI_Finalize(error)
END PROGRAM main
