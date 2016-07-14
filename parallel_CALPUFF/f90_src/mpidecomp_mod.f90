      MODULE mpidecomp
!---------------------------------------------------------
! Purpose:  hold the mpi decomposition variable information
! Revised:  12 July 3 by DJ Rasmussen

! --- MPI receptor domain decomposition variables
!
!  MYSAMB        - integer -   The first(bottom) row number in the 
!                            1D decomposition of the sampling grid

!  MYSAMT        - integer -   The last(top) row number in the 
!                            1D decomposition of the sampling grid

!  MPIFIRSTREC   - integer  -  the first discrete (non-gridded) 
!	   	             receptor of the 1D decomposition

!  MPIILASTREC    - integer  -  the last discrete (non-gridded)
!			     receptor of the 1D decomposition 

!  MPIFIRSTCTREC - integer  -  the first discrete (non-gridded) 
!	   	             complex terrain receptor of the 
!                            1D decomposition

!  MPILASTCTREC  - integer  -  the last discrete (non-gridded)
!			     complex terrain receptor of the 
!                            1D decomposition 
!---------------------------------------------------------

      IMPLICIT none

!local variable and their descriptions 
      SAVE

! variables to hold domain decomposition information for each process
      INTEGER :: mysamb, mysamt
      INTEGER :: mpifirstrec, mpilastrec
      INTEGER :: mpifirstctrec, mpilastctrec

      END MODULE mpidecomp
