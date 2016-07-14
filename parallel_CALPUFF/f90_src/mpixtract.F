c----------------------------------------------------------------------
      subroutine mpixtract(datarr,nxmax,nymax,nxact,nyact,outarr)
c----------------------------------------------------------------------
c
c --- PARALLEL CALPUFF (V5.8)                                 MPIXTRACT
c
c     Original code written by J. Scire, SRC
c
c     Modified by D.J. Rasmussen
c                 Department of Civil and Environmental Engieering
c                 University of California, Davis 
c
c --- PURPOSE:  Extract the active portion of a 2-D array
c
c --- UPDATE
c --- PARALLEL(V5.8)120710  (DMR): Modified for PARALLEL CALPUFF
c --- V5.0-V5.4     000602  (DGS): add message to "stop"
c --- V4.0-V5.0     971107  (DGS): NYACT compared to NYMAX (not NXMAX)
c
c --- INPUTS:
c   DATARR(nxmax,nymax) - real    - Full data array
c                 NXMAX - integer - First dimension of data array
c                 NYMAX - integer - Second dimension of data array
c                 NXACT - integer - Number of active elements of the
c                                   array (first dimension)
c                 NYACT - integer - Number of active elements of the
c                                   array (second dimension)
c        Parameters:
c           IO6
c
c --- OUTPUT:
c   OUTARR(nxact,nyact) - real    - Output array consisting only
c                                   of the active elements of the
c                                   full input array
c
c --- MPIXTRACT called by:  OUTPUT
c --- MPIXTRACT calls:      none
c
c----------------------------------------------------------------------
c
c dmr 
      use mpidecomp
c dmr
      include 'params.puf'
c
      real datarr(nxmax,nymax),outarr(nxact,nyact)
c dmr    
      integer nt
c dmr
c
c --- Check that values of array dimensions are reasonable
      if(nxact.le.0.or.nxact.gt.nxmax.or.
     1   nyact.le.0.or.nyact.gt.nymax)then
            write(io6,*)'ERROR in subr. XTRACT -- Invalid values ',
     1      'of array dimensions input -- NXACT = ',nxact,' NYACT = ',
     2      nyact,' NXMAX = ',nxmax,' NYMAX = ',nymax
            write(*,*)
            stop 'Halted in XTRACT -- see list file.'
      endif

c --- Extract the active portion of the input data array
      do 100 i=1,nxact
        nt = 0
c      do 100 j=1,nyact
c dmr
        do 100 j=mysamb,mysamt
c dmr
          nt = nt+1
c dmr
          outarr(i,nt)=datarr(i,j)
c dmr
c      outarr(i,j)=datarr(i,j)
100   continue
c
      return
      end
