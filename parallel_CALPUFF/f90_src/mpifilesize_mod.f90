
  MODULE mpifilesize

!---------------------------------------------------------
! Purpose:  hold the mpi file size information
! Revised:  12 May 23 by Mike Kleeman and DJ Rasmussen
!---------------------------------------------------------

      IMPLICIT none

!local variable and their descriptions 
      SAVE

! 8-byte integer needed for large file support
      INTEGER*8 :: mpifilebytes(1000)
      INTEGER io8, io9, io10

  END MODULE mpifilesize
