c----------------------------------------------------------------------
      subroutine wrdatu(iounit,cname,outarr,ni,nj)
c----------------------------------------------------------------------
c
c --- CALPUFF    Version: 5.8      Level: 940430                  WRDAT
c     Original code written by J. Scire, SRC
c
c     Modified by D.J. Rasmussen
c                 Department of Civil and Environmental Engineering
c                 University of California, Davis
c
c --- PURPOSE:  Write an uncompressed gridded concentration or dry/wet
c               flux data record
c               (one 15-character identifier and a 2-D data array)
c
c --- INPUTS:
c          IOUNIT - integer      - Fortran unit no. of output file
c           CNAME - character*15 - Species identifier
c   OUTARR(ni,nj) - real array   - Array of concentration data (g/m**3)
c                                  or dry/wet flux data (g/m**2/s)
c
c dmr  Modified for parallel I/O 
c              NI - integer      - Number of sampling grid points in the
c                                  X direction
c              NJ - integer      - Number of sampling grid points in the
c                                  Y direction
c
c --- OUTPUT:  none
c
c --- WRDAT called by:  OUTPUT
c --- WRDAT calls:      none
c
c----------------------------------------------------------------------
c
c dmr
      use mpif
      use mpiranktasks
      use mpifilesize
      use mpidecomp
      include 'params.puf'
c dmr
      real outarr(ni,nj)
      character*15 cname
c dmr
      integer irec_size, ilen, istat(MPI_STATUS_SIZE), tot_nj
      integer(kind=MPI_OFFSET_KIND) myoffset
c dmr

c      write(iounit)cname,outarr
c dmr
c --- sum all j-dimensions across all processes
      call MPI_REDUCE(nj,tot_nj,1,MPI_INTEGER,MPI_SUM,0,
     1                MPI_COMM_WORLD, ierr)
        
c --- simulate fortran block
      if (mpirank.eq.0) then
        ilen = len(cname)
        irec_size = (4*(ni*tot_nj))+ilen ! assume 4 bytes for ints and floats
        call MPI_FILE_WRITE_AT(iounit,mpifilebytes(iounit),irec_size,4,
     1                         MPI_BYTE,istat,ierr) 
        mpifilebytes(iounit)=mpifilebytes(iounit)+4
        call MPI_FILE_WRITE_AT(iounit,mpifilebytes(iounit),cname,ilen,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(iounit)=mpifilebytes(iounit)+ilen
      endif ! mpirank.eq.0

c --- all processes know the current global file pointer location
      call MPI_BARRIER(MPI_COMM_WORLD,ierr)
      call MPI_BCAST(mpifilebytes(iounit),1,MPI_INTEGER8,0,
     1               MPI_COMM_WORLD,ierr)

c --- all processes determine their offset
      if(mpirank.eq.0) then
        myoffset = mpifilebytes(iounit)
      else
        myoffset = mpifilebytes(iounit) + ((mysamb-1)*ni)*4
      endif ! mpirank.eq.0

c --- all processes write their portion of the array       
      call MPI_FILE_WRITE_AT(iounit,myoffset,outarr,
     1                       ((mysamt - mysamb+1)*ni),
     2                        MPI_REAL,istat,ierr)

c --- root process finishes writing the block
      if(mpirank.eq.0) then
        mpifilebytes(iounit)=mpifilebytes(iounit)+((ni*tot_nj)*4)
        call MPI_FILE_WRITE_AT(iounit,mpifilebytes(iounit),irec_size,4,
     1                         MPI_BYTE,istat,ierr) 
        mpifilebytes(iounit)=mpifilebytes(iounit)+4
      endif
c dmr
      return
      end
