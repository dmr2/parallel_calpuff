c----------------------------------------------------------------------
      subroutine wrdatucr(iounit,cname,outarr,ni,nj)
c----------------------------------------------------------------------
c
c --- PARALLEL CALPUFF(V5.8)                                   WRDATUCR
c
c     Written by D.J. Rasmussen
c                Department of Civil and Environmental Engineering
c                University of California, Davis
c
c --- PURPOSE:  Write an uncompressed discrete CTSG receptor
c               concentration/flux data record
c               (one 15-character identifier and a 2-D data array)
c
c --- INPUTS:
c          IOUNIT - integer      - Fortran unit no. of output file
c           CNAME - character*15 - Species identifier
c   OUTARR(ni,nj) - real array   - Array of concentration data (g/m**3)
c                                  or dry/wet flux data (g/m**2/s)
c              NI - integer      - Number of sampling grid points in the
c                                  X direction
c              NJ - integer      - Number of sampling grid points in the
c                                  Y direction
c
c --- OUTPUT:  none
c
c --- WRDATUCR called by:  OUTPUT
c --- WRDATUCR calls:      none
c
c----------------------------------------------------------------------
c
c dmr
      use mpif
      use mpiranktasks
      use mpifilesize
      use mpidecomp

c --- Include parameter statements
      include 'params.puf'
c
      include 'ctsgdat.puf'        
c dmr
      real outarr(ni,nj)
      character*15 cname
c dmr
      integer irec_size, ilen, istat(MPI_STATUS_SIZE)
      integer(kind=MPI_OFFSET_KIND) myoffset
c dmr
c      write(iounit)cname,outarr
c dmr

      if (mpirank.eq.0) then
c --- simulate fortran block
        ilen = len(cname)
        irec_size = (4*(nctrec*nj))+ilen ! assume 4 bytes for ints and floats
        call MPI_FILE_WRITE_AT(iounit,mpifilebytes(iounit),irec_size,4,
     1                         MPI_BYTE,istat,ierr) 
        mpifilebytes(iounit)=mpifilebytes(iounit)+4
        call MPI_FILE_WRITE_AT(iounit,mpifilebytes(iounit),cname,ilen,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(iounit)=mpifilebytes(iounit)+ilen
      endif ! mpirank.eq.0

c --- all processes know the current global file pointer location
      call MPI_BARRIER(MPI_COMM_WORLD,ierr)
      call MPI_BCAST(mpifilebytes(iounit),1,MPI_INTEGER,0,
     1                MPI_COMM_WORLD,ierr)

c --- all processes determine their offset
      if(mpirank.eq.0) then
        myoffset = mpifilebytes(iounit)
      else
        myoffset = mpifilebytes(iounit)+((mpifirstctrec-1)*nj)*4
      endif ! mpirank.eq.0

c --- all processes write their portion of the array       
      call MPI_FILE_WRITE_AT(iounit,myoffset,outarr,(ni*nj),
     1                       MPI_REAL,istat,ierr)

c --- root process finishes writing the block
      if(mpirank.eq.0) then
        mpifilebytes(iounit)=mpifilebytes(iounit)+((nctrec*nj)*4) 
        call MPI_FILE_WRITE_AT(iounit,mpifilebytes(iounit),irec_size,4,
     1                         MPI_BYTE,istat,ierr) 
        mpifilebytes(iounit)=mpifilebytes(iounit)+4
      endif
c dmr
      return
      end
