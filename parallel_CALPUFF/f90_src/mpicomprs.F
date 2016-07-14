c----------------------------------------------------------------------
      subroutine comprs(xdat,nwords,xwork,nwork,clabel,io)
c----------------------------------------------------------------------
c
c --- PARALLEL CALPUFF    Version: 5.8      Level: 000602        COMPRS
c     Original code written by J. Scire, EARTH TECH
c
c     Modified by D.J. Rasmussen
c                 Department of Civil and Environmental Engineering
c                 University of California, Davis
c
c --- PURPOSE:  Compress an array of concentrations, dry fluxes,
c               or wet fluxes by replacing strings of zero values
c               with a negative code indicating the number of zero
c               values
c
c --- UPDATE
c --- PARALLEL(V5.8)120723  (DMR): Modified for PARALLEL CALPUFF
c --- V5.3-V5.4     000602  (DGS): add message to "stop"
c
c --- INPUTS:
c        XDAT(nwords) - real array - Array of uncompressed data to be
c                                    output
c              NWORDS - integer    - Number of values in data array
c        XWORK(nwork) - real array - Work array to temporarily store
c                                    compressed array
c               NWORK - integer    - Dimension of work array - NWORK
c                                    must be >= NWORDS
c              CLABEL - char*15    - Character record header
c                  IO - integer    - Unit number of output file
c
c     Use /MPIRANKTASKS/ variables:
c         MPINUMTASKS, MPIRANK 
c     Use /MPIFILESIZE/ variables:
c         MPIFILEBYTES
c
c     Parameters: IO6
c
c --- OUTPUT:  none
c
c --- COMPRS called by: OUTPUT
c
c --- COMPRS calls:     WRDATC
c----------------------------------------------------------------------
c dmr
      use mpif
      use mpiranktasks
      use mpifilesize

c for profiling only
c      use timing
c dmr
c --- Include parameter statements
      include 'params.puf'
      include 'grid.puf'

      real xdat(nwords),xwork(nwork)
c      real endpts(2),allendpts(2*mpinumtasks)
      character*15 clabel
c dmr
      integer irec_size, ii_global, istat(MPI_STATUS_SIZE)
      integer*8 pkg(2),recpkg(2)
      real myright,myleft,ngbr2rt,ngbr2lft
      integer myii, myiisum
c debug variables
      integer nwords_tot_before, nwords0, nwords1
      double precision time1, time2
c dmr
c --- Check that work array is sized large enough
      if(nwork.lt.nwords)then
         write(io6,*)'ERROR in Subr. COMPRS -- Work array ',
     1   'dimension is too small -- NWORK = ',nwork,' NWORDS = ',
     2   nwords
         write(*,*)
         stop 'Halted in COMPRS -- see list file.'
      endif
c
c dmr debug
c      call MPI_REDUCE(nwords,nwords_tot_before,1,MPI_INT, MPI_SUM, 0,
c     1                   MPI_COMM_WORLD, ierr)
c      if(mpirank.eq.0) then
c        print*,'nwords total before compression: ', nwords_tot_before
c      endif  
c dmr debug
c
c --- Replace all zeroes with negative coded integer
      nzero=0
      ii=0
      do 100 i=1,nwords
c
         if(xdat(i).eq.0.0)then
            nzero=nzero+1
            go to 100
         else if(xdat(i).lt.0.0)then
            write(io6,*)'ERROR in Subr. COMPRS -- Negative value ',
     1      'encountered with COMPRESS option on -- I = ',i,
     2      ' XDAT(i) = ',xdat(i)
            write(io6,*)'COMPRESS option cannot be used when data ',
     1      'values are negative'
            write(*,*)
            stop 'Halted in COMPRS -- see list file.'
         endif
c
         if(nzero.eq.0)then
            ii=ii+1
            xwork(ii)=xdat(i)
         else
            ii=ii+1
            xwork(ii)=-(float(nzero)+0.0001)
            nzero=0
            ii=ii+1
            xwork(ii)=xdat(i)
         endif
100   continue
c
      if(nzero.gt.0)then
         ii=ii+1
        xwork(ii)=-(float(nzero)+0.0001)
      endif
c dmr
c      write(io)ii
c dmr
c re-name "ii" for clarity        
       myii = ii
c-- mjk debug
c      ileft=0
c      do ii=1,this_ii
c       if(xwork(ii).gt.0)then
c        ileft=ileft+1
c       else
c        ileft=ileft-int(xwork(ii))
c       endif
c      enddo
c      call MPI_REDUCE(ileft,nwords0,1,MPI_INT, MPI_SUM, 0,
c     1                   MPI_COMM_WORLD, ierr)
c      if(mpirank.eq.0)print*,'nwords0=',nwords0
c-- mjk debug
c dmr debug
c        write(io6,*) "this xwork:"
c        do ii=1, this_ii
c          write(io6,*) xwork(ii)
c        enddo
c dmr debug

c --- START: remove redundant zeros at the start and end of adjacent data blocks
c        
c dmr debug
       time1 = mpi_wtime()
c dmr debug

c --- find array end pts. and check to see if this array is zero (i.e. has only one end point)
      if(myii.ne.1) then
        myleft=xwork(1)
        myright=xwork(myii)
      else
        myleft=xwork(1)
        myright= 0
      endif

c --- all processes send end pts  
      if(mpirank.ne.mpinumtasks-1) then
        call MPI_SEND(myright,1,MPI_REAL,mpirank+1,mpirank,
     1                MPI_COMM_WORLD,istat,ierr)
      endif
      if(mpirank.ne.0)then
        call MPI_SEND(myleft,1,MPI_REAL,mpirank-1,mpirank,
     1                MPI_COMM_WORLD,istat,ierr)
      endif

c --- all processes receive end pts 
      if(mpirank.eq.0) then 
        call MPI_RECV(ngbr2rt,1,MPI_REAL,mpirank+1,mpirank+1,
     1                MPI_COMM_WORLD,istat,ierr)        
      elseif(mpirank.eq.mpinumtasks-1) then
        call MPI_RECV(ngbr2lft,1,MPI_REAL,mpirank-1,mpirank-1,
     1                MPI_COMM_WORLD,istat,ierr)        
      else
        call MPI_RECV(ngbr2lft,1,MPI_REAL,mpirank-1,mpirank-1,
     1                MPI_COMM_WORLD,istat,ierr)        
        call MPI_RECV(ngbr2rt,1,MPI_REAL,mpirank+1,mpirank+1,
     1                MPI_COMM_WORLD,istat,ierr)        
      endif

c --- modify each processes xwork and its count of concentrations
        if(mpirank.ne.0)then
          if(ngbr2lft.lt.0.AND.myleft.lt.0)then
c            xwork(1)=xwork(1)+int(ngbr2lft)
            xwork(1)=xwork(1)+ngbr2lft
          endif         
        endif
        if(mpirank.ne.mpinumtasks-1) then
          if(ngbr2rt.lt.0.AND.myright.lt.0) then
            myii = myii - 1
          endif
        endif

c --- END: remove redundant zeros at the start and end of adjacent data blocks

c dmr debug
c      time2 = mpi_wtime()
c      time_rzeros = time_rzeros + time2 - time1
c dmr debug

c ---  calculate total number of both non-negative and negative values across all mpi tasks

c dmr debug
c       time1 = mpi_wtime()
c dmr debug

      if(mpinumtasks.eq.1) goto 210
        myiisum = 0
        iitot=0

c --- Cascade variable "ii" and file pointer loc between all procs (asynchronous)
     
c --- all procs but root receive "ii" and file pointer location
      if(mpirank.ne.0)then
        recpkg = 0 ! vector op.
        call MPI_RECV(recpkg,2,MPI_INTEGER8,mpirank-1,mpirank-1,
     1                MPI_COMM_WORLD,istat,ierr)
c        print*, "rank: ",mpirank,"recii: ",recpkg(1)," received!"
c        call flush(6)
c        print*, "rank: ",mpirank,"fileptrloc: ",recpkg(2)," received!"
c        call flush(6)
c --- compute running sum of ii
        myiisum = myii + recpkg(1)
c        print*, "rank: ",mpirank,"myiisum: ",myiisum
c --- assign offset
        myoffset = recpkg(1) ! offset is equal to myii of previous proc
c        print*, "rank: ",mpirank,"myoffset: ",myoffset
c        call flush(6)
c --- assign mpifilebytes & add 31 byts for headers written by rank 0
        mpifilebytes(io)=recpkg(2)+31
      endif

c --- all procs but last proc send buffer with "ii" and file pointer location
      if(mpirank.ne.mpinumtasks-1)then
        pkg = 0  ! vector op. 
        if(mpirank.eq.0)then
          myoffset = 0
          pkg(1)=myii
          pkg(2)=mpifilebytes(io)
        else
          pkg(1)=myiisum
          pkg(2)=recpkg(2)
        endif
        call MPI_SEND(pkg,2,MPI_INTEGER8,mpirank+1,mpirank,
     1                MPI_COMM_WORLD,istat,ierr)
c        print*, "rank: ",mpirank,"myiisum: ",pkg(1)," sent!"
c        call flush(6)
c        print*, "rank: ",mpirank,"fileptrloc: ",pkg(2)," sent!"
c        call flush(6)
      endif

c --- last proc sends "myiisum" to root proc to write to disk
      if(mpirank.eq.mpinumtasks-1)then
        ii_global = myiisum
        call MPI_SEND(ii_global,1,MPI_INT,0,mpirank,
     1                MPI_COMM_WORLD,istat,ierr)
      elseif(mpirank.eq.0)then
        call MPI_RECV(ii_global,1,MPI_INT,mpinumtasks-1,mpinumtasks-1,
     1                MPI_COMM_WORLD,istat,ierr)              
      endif

c dmr debug
c      time2 = mpi_wtime()
c      timegather = timegather + time2 - time1
c dmr debug
210   continue

c --- Write the data records (header, compressed data record)
      if(mpirank.eq.0)then
c --- simulate Fortan block
         irec_size = 4 ! assume 4 bytes for ints and floats
         call MPI_FILE_WRITE_AT(io,mpifilebytes(io),irec_size,4,
     1                          MPI_BYTE,istat,ierr)
         mpifilebytes(io)=mpifilebytes(io)+4
         call MPI_FILE_WRITE_AT(io,mpifilebytes(io),ii_global,4,
     1                          MPI_BYTE,istat,ierr)
         mpifilebytes(io)=mpifilebytes(io)+4
         call MPI_FILE_WRITE_AT(io,mpifilebytes(io),irec_size,4,
     1                          MPI_BYTE,istat,ierr)
         mpifilebytes(io)=mpifilebytes(io)+4
      endif

c dmr debug
c      ileft=0
c      do ii=1,this_ii
c       if(xwork(ii).gt.0)then
c        ileft=ileft+1
c       else
c        ileft=ileft-int(xwork(ii))
c       endif
c      enddo
c      call MPI_REDUCE(ileft,nwords1,1,MPI_INT, MPI_SUM, 0,
c     1                   MPI_COMM_WORLD, ierr)
c      if(mpirank.eq.0)print*,'nwords1=',nwords1
c      if(mpirank.eq.0.AND.nwords0.ne.nwords1) stop
c dmr debug

c dmr
c      call wrdat(io,clabel,xwork,ii,1)
c dmr 
c dmr debug
c       time1 = mpi_wtime()
c dmr debug
      call wrdatc(io,clabel,xwork,myii,1,myoffset,ii_global)
c dmr debug
c      time2 = mpi_wtime()
c      timewrdat = timewrdat + time2 - time1
c dmr debug
c
c      write(io6,*)"mpi_send/receive:",time_rzeros,"mpigatherii:",
c     1            timegather,"mpiwrdat",timewrdat
      return
      end
