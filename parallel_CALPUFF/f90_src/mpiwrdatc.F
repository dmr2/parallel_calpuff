c----------------------------------------------------------------------
      subroutine wrdatc(iounit,cname,outarr,ni,nj,myiisum,tot_ni)
c----------------------------------------------------------------------
c
c --- PARALLEL CALPUFF (V5.8)                                    WRDATC
c     Original code written by J. Scire, SRC
c
c     Modified by D.J. Rasmussen
c                 Department of Civil and Environmental Engineering
c                 University of California, Davis
c               
c --- PURPOSE:  Write a compressed gridded concentration or dry/wet
c               flux data record
c               (one 15-character identifier and a 2-D data array)
c
c --- UPDATE
c --- PARALLEL(V5.8)120710  (DMR): Modified for PARALLEL CALPUFF
c
c --- INPUTS:
c          IOUNIT - integer      - Fortran unit no. of output file
c           CNAME - character*15 - Species identifier
c   OUTARR(NI,NJ) - real array   - Array of concentration data (g/m**3)
c                                  or dry/wet flux data (g/m**2/s)
c              NI - integer      - Number of sampling grid points in the
c                                  I direction
c              NJ - integer      - Number of sampling grid points in the
c                                  J direction
c           MY_NI - integer      - Number of output values for each process
c         MYIISUM - integer      - Running sum of output values for each
c                                  process to be used for file offset
c          TOT_NI - integer      - Number of output values for all processes
c
c --- OUTPUT:  none
c
c --- WRDATC called by:  COMPRS
c --- WRDATC calls:      none
c
c----------------------------------------------------------------------
c
c dmr
        use mpif
        use mpiranktasks
        use mpifilesize
c dmr
      real outarr(ni,nj)
      character*15 cname
c dmr
      integer irec_size, this_asize, ilen, istat(MPI_STATUS_SIZE)
      integer tot_asize, tot_ni, myiisum
      integer(kind=MPI_OFFSET_KIND) myoffset
c dmr
c      write(iounit)cname,outarr
c dmr
      this_asize = ni*nj
c
c       print*,"rank: ",mpirank,"mfb: ",mpifilebytes(iounit)
c       print*,"rank: ",mpirank,"iounit: ",iounit

      if (mpirank.eq.0) then
c        print*, "mpifilebytes(iounit): ", mpifilebytes(iounit)
c        call flush(6)
c        print*, "tot_ni: ",tot_ni
c        call flush(6)
        tot_asize = tot_ni*nj
c --- calculate total size of conc. name and data array 
        ilen = len(cname)
        irec_size = (4*(tot_asize))+ilen ! assume 4 bytes for ints and floats
c --- simulate Fortran block
        call MPI_FILE_WRITE_AT(iounit,mpifilebytes(iounit),irec_size,4,
     1                         MPI_BYTE,istat, ierr) 
        mpifilebytes(iounit)=mpifilebytes(iounit)+4
        call MPI_FILE_WRITE_AT(iounit,mpifilebytes(iounit),cname,ilen,
     1                         MPI_BYTE,istat, ierr)
        mpifilebytes(iounit)=mpifilebytes(iounit)+ilen
c        print*, "wrote irec_size"
c        call flush(6)
      endif ! mpirank.eq.0

c       print*,"rank: ",mpirank,"mfb: ",mpifilebytes(iounit)
c --- all processes know current file pointer location
c     call MPI_BARRIER(MPI_COMM_WORLD,ierr)

c --- 8-byte integer needed for large file support
c      call MPI_BCAST(mpifilebytes(iounit),1,MPI_INTEGER8,0,
c     1               MPI_COMM_WORLD,ierr)

c       print*,"rank: ",mpirank,"mfb from BCAST: ",mpifilebytes(iounit)
c        stop
c --- each process writes their portion of the data array
c       myoffset = mpifilebytes(iounit)+(sum(all_ni(1:mpirank)))*4
       myoffset = mpifilebytes(iounit)+(myiisum*4)
c       print*,"rank: ",mpirank,"myoffset: ",myoffset,"asze: ",this_asize
       call MPI_FILE_WRITE_AT(iounit,myoffset,outarr,this_asize,
     1                        MPI_REAL,istat,ierr)

c --- root process finishes writing the block
      if(mpirank.eq.0) then
        mpifilebytes(iounit)=mpifilebytes(iounit)+(tot_asize)*4
        call MPI_FILE_WRITE_AT(iounit,mpifilebytes(iounit),irec_size,4,
     1                         MPI_BYTE,istat,ierr) 
        mpifilebytes(iounit)=mpifilebytes(iounit)+4
      endif ! mpirank.eq.0
      
      return
      end
