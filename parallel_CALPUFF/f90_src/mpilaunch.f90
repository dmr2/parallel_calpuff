
SUBROUTINE mpilaunch 

!---------------------------------------------------------
! Purpose:  launch mpi 
! Revised:  12 Jul 2011 Original version. (D.Yin)
!---------------------------------------------------------

USE mpif
USE mpiranktasks

IMPLICIT none

!local variables and their descriptions
INTEGER :: ierr

!start of the executable code 
CALL MPI_INIT(ierr)
CALL MPI_COMM_RANK(MPI_COMM_WORLD, mpirank, ierr)
CALL MPI_COMM_SIZE(MPI_COMM_WORLD, mpinumtasks, ierr)

END SUBROUTINE mpilaunch 
