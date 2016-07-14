
MODULE mpiranktasks

!---------------------------------------------------------
! Purpose:  hold mpi rank and number of tasks info
! Revised:  12 Jul 2011 Original version. (D.Yin)
!---------------------------------------------------------

IMPLICIT none

!local variable and their descriptions 
SAVE
INTEGER :: mpirank, mpinumtasks

END MODULE mpiranktasks
