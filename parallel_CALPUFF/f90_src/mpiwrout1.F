      subroutine wrout1(title)
c----------------------------------------------------------------------
c
c --- CALPUFF    Version: 5.8      Level: 050128                 WROUT1
c     Original code written by J. Scire, Earth Tech
c
c     Modified by D.J. Rasmussen
c                 Department of Civil and Environmental Engineering
c                 University of California, Davis
c
c --- PURPOSE:  Write the header records to the concentration,
c               dry flux, wet flux, and visibility output files
c                Concentration file: CONC.DAT (IO8)
c                Dry flux file:      DFLX.DAT (IO9)
c                Wet flux file:      WFLX.DAT (IO10)
c                Visibility files:   VISB.DAT (IO11)
c                                    TK2D.DAT (IO13)
c                                    RHO2D.DAT (IO14)
c               and to the FOG processing output file
c                Fog data file:      FOG.DAT  (IO12)
c
c --- UPDATE
c --- PARALLEL(V5.8)120723  (DMR): Modified for PARALLEL CALPUFF
c --- V5.72-V5.725  050128  (DGS): Add TK2D.DAT and RHO2D.DAT files
c                   050128  (DGS): Add call to COORDSVER and write info
c                                  to output file headers
c --- V5.7-V5.72    031017  (DGS): Revise format for source contribution
c                                  output option; use full begin/end
c                                  times as hour and second
c                                  (Dataset Version 2.1)
c --- V5.5-V5.7     030402  (DGS): Add list file unit to JULDAY call
c                   030402  (DGS): Revised header format with control
c                                  file images, and /MAP/ variables
c                   010901  (FRR): Add i2dmet for VISB.DAT header
c --- V5.0-V5.4     000602_2(DGS): Add LCOMPRS for FOG.DAT header
c --- V5.0-V5.4     000602  (DGS): IGRDVL changed to NVL2
c                   000602  (DGS): add FOG-model file (FOG.DAT)
c --- V4.0-V5.0     971107  (DGS): uses species-groups
c                   971107  (DGS): add number of lines from LNEMARB.DAT
c
c --- INPUTS:
c             TITLE(3) - Char.*80    - Run title (3 lines of
c                                      80 char. each)
c       Use /MPIRANKTASKS/ variables:
c             MPINUMTASKS, MPIRANK
c       Use /MPIFILESIZE/ variables:
c             MPIFILEBYTES, IO8, IO9, IO10
c       Common block /AR1/ variables:
c             NAR1,CNAMAR1(mxarea)
c       Common block /AR2/ variables:
c             NAR2,CID3(mxarea)
c       Common block /CTSGDAT/ variables:
c             NHILL, NCTREC, XRCT(mxrect), YRCT(mxrect),
c             ELRECT(mxrect), IHILL(mxrect)
c       Common block /DATEHR/ variables:
c             XBTZ
c       Common block /FOG/ variables:
c             (all)
c       Common block /GEN/ variables:
c             IBYR, IBMO, IBDY, IBHR, IRLG, IBCOMP, JBCOMP, IECOMP,
c             JECOMP, IBSAMP, JBSAMP, IESAMP, JESAMP, MESHDN,
c             NGRUP, CGRUP(mxgrup)
c       Common block /GRID/ variables:
c             NX, NY, NZ, DGRID, XORIG, YORIG
c       Common block /LN1/ variables:
c             NLINES,CNAMLN1(mxlines)
c       Common block /LN2/ variables:
c             NLN2,CID5(mxlines)
c       Common block /MAP/ variables:
c             iutmzn,feast,fnorth,rnlat0,relon0,xlat1,xlat2,
c             pmap,utmhem,datum,daten,clat0,clon0,clat1,clat2
c       Common block /METHD/ variables:
c             NSSTA, NEARS(mxnx,mxny), XSSTA(mxss), YSSTA(mxss), I2DMET
c       Common block /NONGRD/ variables:
c             NREC
c       Common block /OUTPT/ variables:
c             ICON, IDRY, IWET, IVIS, IT2D, IRHO, IOUTOP(7,mxspec),
c             LCOMPRS, IPRU, IFOG, MSOURCE
c       Common block /PT1/ variables:
c             NPT1,CNAMPT1(mxpt1)
c       Common block /PT2/ variables:
c             NPT2,CID2(mxpt2)
c       Common block /QA/ variables:
c             VER, LEVEL, NCOMMOUT
c       Common block /VOL1/ variables:
c             NVL1,CNAMVL1(mxvol)
c       Common block /VOL2/ variables:
c             NVL2,CID4(mxvol)
c       Common block /WRKSPC/ variables:
c             TMP3(mxrec), TMP4(mxrec), TMP5(mxrect), TMP6(mxrect)
c       Parameters:
c             MXNZ, MXNZP1, MXGRUP, MXNZMP1,  MXPT1, MXPT2, MXREC,
c             MXNX, MXNY, MXSS, MXHILL, MXRECT,
c             MXPT1,MXPT2,MXAREA,MXLINES,MXVOL,
c             IO6, IO8, IO9, IO10, IO11, IO12, IO13, IO14, MMODEL, IOX
c
c --- OUTPUT:  none
c
c --- WROUT1 called by:  SETUP
c --- WROUT1 calls:      JULDAY, INCR, COORDSVER,
c                        XTRACT, XTRACTI, WRDAT, WRINT
c
c----------------------------------------------------------------------
c
c dmr
      use mpif
      use mpiranktasks
      use mpifilesize
c dmr
c --- Include parameters
      include 'params.puf'

c --- Include common blocks
      include 'ar1.puf'
      include 'ar2.puf'
      include 'ctsgdat.puf'
      include 'datehr.puf'
      include 'fog.puf'
      include 'gen.puf'
      include 'grid.puf'
      include 'ln1.puf'
      include 'ln2.puf'
      include 'map.puf'
      include 'methd.puf'
      include 'nongrd.puf'
      include 'outpt.puf'
      include 'pt1.puf'
      include 'pt2.puf'
      include 'qa.puf'
      include 'vol1.puf'
      include 'vol2.puf'
      include 'wrkspc.puf'
c
      real xkmsta(mxss),ykmsta(mxss)
      integer itmp(mxnx,mxny)
c dmr
      integer istat(MPI_STATUS_SIZE), irec_size, ilen
c dmr
      character*80 title(3)
      character*15 csout(mxgrup),cdfout(mxgrup),cwfout(mxgrup)
      character*15 cvsout(1),ct2out,crhoout
      character*15 cname
c
      character*16 conset,dryset,wetset,visset,dataver
      character*16 t2dset,rhoset
      character*33 blank33
      character*64 datamod
      character*132 comment1,blank
c
      character*50 verdoc

      data iavg/1/,ione/1/

      data blank33/'                                 '/

c --- Configure output documentation
      data conset/'CONC.DAT'/, dataver/'2.1'/
      data dryset/'DFLX.DAT'/, wetset/'WFLX.DAT'/
      data visset/'VISB.DAT'/, t2dset/'TK2D.DAT'/, rhoset/'RHO2D.DAT'/
      data datamod/'No-Obs file structure with embedded control file'/
      data comment1/'Produced by CALPUFF Version: '/

c --- Set blank (132 characters)
      blank(1:33)=blank33
      blank(34:66)=blank33
      blank(67:99)=blank33
      blank(100:132)=blank33

c --- Construct the version-level comment string
      j=30
      do i=1,8
         if(ver(i:i).NE.' ') then
            comment1(j:j)=ver(i:i)
            j=j+1
         endif
      enddo
      j=j+1
      comment1(j:j+7)=' Level: '
      j=j+8
      do i=1,8
         if(level(i:i).NE.' ') then
            comment1(j:j)=level(i:i)
            j=j+1
         endif
      enddo
c
c --- Obtain COORDS version information
      call COORDSVER(io6,verdoc)
c
c --- Complete the starting time specification
      nsecdt=3600
      isecb=0
c --- Compute the starting TIME for the first hour (subtract 1hr)
      call JULDAY(io6,ibyr,ibmo,ibdy,ijulb)
      nhrinc=-1
      iyrb=ibyr
      ihrb=ibhr
      call INCR(io6,iyrb,ijulb,ihrb,nhrinc)
c
c --- Skip unnecessary calculations if no output files are generated
      isum=icon+idry+iwet
      if(isum.eq.0)go to 100
c
c --- Set internal variab
      xorigkm=0.001*xorig
      yorigkm=0.001*yorig
      dgridkm=0.001*dgrid
      dxkm=dgridkm
      dykm=dgridkm
c --- NSPOUT, NDFOUT, and NWFOUT are the number of species stored in the
c --- concentration, dry flux, and wet flux output files, respectively
c --- NVSOUT is the number of visibility-related parameters in the
c --- visibility output file
c --- NT2OUT is number of parameters (1) in 2D Temperature output file
c --- NRHOOUT is number of parameters (1) in 2D Density output file
      nspout=0
      ndfout=0
      nwfout=0
      nvsout=1
      nt2out=1
      nrhoout=1
      do 10 i=1,ngrup
      if(ioutop(2,i).eq.1)nspout=nspout+1
      if(ioutop(4,i).eq.1)ndfout=ndfout+1
      if(ioutop(6,i).eq.1)nwfout=nwfout+1
10    continue

c -----------------------------------
c --- Add dataset and comment records
c -----------------------------------
c
c --- Record #1 - File Declaration -- 24 words
c
c --- dmr ******** modified for parallel I/O **********
      if(mpirank.eq.0) then
c      if(ICON.eq.1)write(io8) conset,dataver,datamod
        if(ICON.eq.1) then
c ---  simulate fortran block
         irec_size = (16*2)+64
         call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),irec_size,4,
     1                           MPI_BYTE,istat,ierr) 
         mpifilebytes(io8)=mpifilebytes(io8)+4
         call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),conset,16,
     1                         MPI_BYTE,istat,ierr)
         mpifilebytes(io8)=mpifilebytes(io8)+16
         call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),dataver,16,
     1                          MPI_BYTE,istat,ierr)
         mpifilebytes(io8)=mpifilebytes(io8)+16
         call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),datamod,64,
     1                          MPI_BYTE,istat,ierr)
         mpifilebytes(io8)=mpifilebytes(io8)+64
         call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),irec_size,4,
     1                           MPI_BYTE,istat,ierr)
         mpifilebytes(io8)=mpifilebytes(io8)+4
        endif ! ICON.eq.1
c      if(IDRY.eq.1)write(io9) dryset,dataver,datamod
        if(IDRY.eq.1) then
c --- simulate fortran block
         irec_size = (16*2)+64
         call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),irec_size,4,
     1                           MPI_BYTE,istat,ierr)
         mpifilebytes(io9)=mpifilebytes(io9)+4
         call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),dryset,16,
     1                           MPI_BYTE,istat,ierr)
         mpifilebytes(io9)=mpifilebytes(io9)+16
         call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),dataver,16,
     1                           MPI_BYTE,istat,ierr)
         mpifilebytes(io9)=mpifilebytes(io9)+16
         call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),datamod,64,
     1                          MPI_BYTE,istat,ierr)
         mpifilebytes(io9)=mpifilebytes(io9)+64
         call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),irec_size,4,
     1                           MPI_BYTE,istat,ierr)
         mpifilebytes(io9)=mpifilebytes(io9)+4
        endif ! IDRY.eq.1
c      if(IWET.eq.1)write(io10) wetset,dataver,datamod
        if(IWET.eq.1) then
c --- simulate fortran block
         irec_size = (16*2)+64
         call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),irec_size,4,
     1                           MPI_BYTE,istat,ierr)
         mpifilebytes(io10)=mpifilebytes(io10)+4
         call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),wetset,16,
     1                           MPI_BYTE,istat,ierr)
         mpifilebytes(io10)=mpifilebytes(io10)+16
         call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),dataver,16,
     1                           MPI_BYTE,istat,ierr)
         mpifilebytes(io10)=mpifilebytes(io10)+16
         call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),datamod,64,
     1                          MPI_BYTE,istat,ierr)
         mpifilebytes(io10)=mpifilebytes(io10)+64
         call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),irec_size,4,
     1                           MPI_BYTE,istat,ierr)
         mpifilebytes(io10)=mpifilebytes(io10)+4
        endif ! IWET.eq.1
       if(IVIS.eq.1)write(io11) visset,dataver,datamod
       if(IT2D.eq.1)write(io13) t2dset,dataver,datamod
       if(IRHO.eq.1)write(io14) rhoset,dataver,datamod
      endif ! mpirank.eq.0
c
c --- Record #2 - Number of comment lines -- 1 word
      ncom=ncommout+2
c dyin
      if(mpirank.eq.0) then
c      if(ICON.eq.1)write(io8) ncom
        if(ICON.eq.1) then
         irec_size = 4
         call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),irec_size,4,
     1                          MPI_BYTE,istat,ierr)
         mpifilebytes(io8)=mpifilebytes(io8)+4
         call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),ncom,4,
     1                          MPI_BYTE,istat,ierr)
         mpifilebytes(io8)=mpifilebytes(io8)+4
         call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),irec_size,4,
     1                          MPI_BYTE,istat,ierr)
         mpifilebytes(io8)=mpifilebytes(io8)+4
       endif ! ICON.eq.1
c      if(IDRY.eq.1)write(io9) ncom
        if(IDRY.eq.1) then
         irec_size = 4
         call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),irec_size,4,
     1                          MPI_BYTE,istat,ierr)
         mpifilebytes(io9)=mpifilebytes(io9)+4
         call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),ncom,4,
     1                          MPI_BYTE,istat,ierr)
         mpifilebytes(io9)=mpifilebytes(io9)+4
         call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),irec_size,4,
     1                          MPI_BYTE,istat,ierr)
         mpifilebytes(io9)=mpifilebytes(io9)+4
       endif ! IDRY.eq.1
c      if(IWET.eq.1)write(io10) ncom
        if(IWET.eq.1) then
         irec_size = 4
         call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),irec_size,4,
     1                          MPI_BYTE,istat,ierr)
         mpifilebytes(io10)=mpifilebytes(io10)+4
         call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),ncom,4,
     1                          MPI_BYTE,istat,ierr)
         mpifilebytes(io10)=mpifilebytes(io10)+4
         call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),irec_size,4,
     1                          MPI_BYTE,istat,ierr)
         mpifilebytes(io10)=mpifilebytes(io10)+4
       endif ! IWET.eq.1
      if(IVIS.eq.1)write(io11) ncom
      if(IT2D.eq.1)write(io13) ncom
      if(IRHO.eq.1)write(io14) ncom
      endif ! mpirank.eq.0
c
c --- Record #3 to NCOM+2 (Comment record section) -- 33 words each
c
c
      if(mpirank.eq.0) then
c      if(ICON.eq.1)write(io8) comment1
        if(ICON.eq.1) then
         ilen = len(comment1)
         irec_size = ilen
         call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),irec_size,4,
     1                          MPI_BYTE,istat,ierr)
         mpifilebytes(io8)=mpifilebytes(io8)+4
         call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),comment1,ilen,
     1                          MPI_BYTE,istat,ierr)
         mpifilebytes(io8)=mpifilebytes(io8)+ilen
         call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),irec_size,4,
     1                          MPI_BYTE,istat,ierr)
         mpifilebytes(io8)=mpifilebytes(io8)+4
        endif ! ICON.eq.1
c      if(IDRY.eq.1)write(io9) comment1
        if(IDRY.eq.1) then
         ilen = len(comment1)
         irec_size = ilen
         call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),irec_size,4,
     1                          MPI_BYTE,istat,ierr)
         mpifilebytes(io9)=mpifilebytes(io9)+4
         call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),comment1,ilen,
     1                          MPI_BYTE,istat,ierr)
         mpifilebytes(io9)=mpifilebytes(io9)+ilen
         call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),irec_size,4,
     1                          MPI_BYTE,istat,ierr)
         mpifilebytes(io9)=mpifilebytes(io9)+4
        endif ! IDRY.eq.1
c      if(IWET.eq.1)write(io10) comment1
        if(IWET.eq.1) then
         ilen = len(comment1)
         irec_size = ilen
         call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),irec_size,4,
     1                          MPI_BYTE,istat,ierr)
         mpifilebytes(io10)=mpifilebytes(io10)+4
         call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),comment1,ilen,
     1                          MPI_BYTE,istat,ierr)
         mpifilebytes(io10)=mpifilebytes(io10)+ilen
         call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),irec_size,4,
     1                          MPI_BYTE,istat,ierr)
         mpifilebytes(io10)=mpifilebytes(io10)+4
        endif ! IWET.eq.1
       if(IVIS.eq.1)write(io11) comment1
       if(IT2D.eq.1)write(io13) comment1
       if(IRHO.eq.1)write(io14) comment1
      endif ! mpirank.eq.0
c
c --- Report COORDS version
      comment1=blank
      comment1(1:36)='Internal Coordinate Transformations '
      comment1(37:86)=verdoc
c
      if(mpirank.eq.0) then 
c      if(ICON.eq.1)write(io8) comment1
        if(ICON.eq.1) then
         ilen = len(comment1)
         irec_size = ilen
         call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),irec_size,4,
     1                          MPI_BYTE,istat,ierr)
         mpifilebytes(io8)=mpifilebytes(io8)+4
         call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),comment1,ilen,
     1                          MPI_BYTE,istat,ierr)
         mpifilebytes(io8)=mpifilebytes(io8)+ilen
         call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),irec_size,4,
     1                          MPI_BYTE,istat,ierr)
         mpifilebytes(io8)=mpifilebytes(io8)+4
        endif ! ICON.eq.1
c      if(IDRY.eq.1)write(io9) comment1
        if(IDRY.eq.1) then
         ilen = len(comment1)
         irec_size = ilen
         call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),irec_size,4,
     1                          MPI_BYTE,istat,ierr)
         mpifilebytes(io9)=mpifilebytes(io9)+4
         call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),comment1,ilen,
     1                          MPI_BYTE,istat,ierr)
         mpifilebytes(io9)=mpifilebytes(io9)+ilen
         call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),irec_size,4,
     1                          MPI_BYTE,istat,ierr)
         mpifilebytes(io9)=mpifilebytes(io9)+4
        endif ! IDRY.eq.1
c      if(IWET.eq.1)write(io10) comment1
        if(IWET.eq.1) then
         ilen = len(comment1)
         irec_size = ilen
         call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),irec_size,4,
     1                          MPI_BYTE,istat,ierr)
         mpifilebytes(io10)=mpifilebytes(io10)+4
         call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),comment1,ilen,
     1                          MPI_BYTE,istat,ierr)
         mpifilebytes(io10)=mpifilebytes(io10)+ilen
         call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),irec_size,4,
     1                          MPI_BYTE,istat,ierr)
         mpifilebytes(io10)=mpifilebytes(io10)+4
        endif ! IWET.eq.1
       if(IVIS.eq.1)write(io11) comment1
       if(IT2D.eq.1)write(io13) comment1
       if(IRHO.eq.1)write(io14) comment1
      endif  ! mpirank.eq.0
     
c --- Go to beginning of the scratch file with the control file image
      REWIND(iox)
c --- Loop over records
      do i=1,ncommout
         comment1=blank
         read(iox,'(a132)') comment1
c
         if(mpirank.eq.0) then
c        if(ICON.eq.1)write(io8) comment1
          if(ICON.eq.1) then
           ilen = len(comment1)
           irec_size = ilen
            call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),irec_size,4,
     1                              MPI_BYTE,istat,ierr)
            mpifilebytes(io8)=mpifilebytes(io8)+4
            call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),comment1,ilen,
     1                              MPI_BYTE,istat,ierr)
            mpifilebytes(io8)=mpifilebytes(io8)+ilen
            call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),irec_size,4,
     1                               MPI_BYTE,istat,ierr)
            mpifilebytes(io8)=mpifilebytes(io8)+4
          endif ! ICON.eq.1
c         if(IDRY.eq.1)write(io9) comment1
          if(IDRY.eq.1) then
           ilen = len(comment1)
           irec_size = ilen
            call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),irec_size,4,
     1                              MPI_BYTE,istat,ierr)
            mpifilebytes(io9)=mpifilebytes(io9)+4
            call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),comment1,ilen,
     1                              MPI_BYTE,istat,ierr)
            mpifilebytes(io9)=mpifilebytes(io9)+ilen
            call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),irec_size,4,
     1                               MPI_BYTE,istat,ierr)
            mpifilebytes(io9)=mpifilebytes(io9)+4
          endif ! IDRY.eq.1
c        if(IWET.eq.1)write(io10) comment1
          if(IWET.eq.1) then
           ilen = len(comment1)
           irec_size = ilen
            call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),irec_size,4,
     1                              MPI_BYTE,istat,ierr)
            mpifilebytes(io10)=mpifilebytes(io10)+4
            call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),comment1,
     1                              ilen,MPI_BYTE,istat,ierr)
            mpifilebytes(io10)=mpifilebytes(io10)+ilen
            call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),irec_size,4,
     1                               MPI_BYTE,istat,ierr)
            mpifilebytes(io10)=mpifilebytes(io10)+4
          endif ! IDRY.eq.1
          if(IVIS.eq.1)write(io11) comment1
          if(IT2D.eq.1)write(io13) comment1
          if(IRHO.eq.1)write(io14) comment1
         endif ! mpirank.eq.0 
      enddo ! i=1, ncommout

      CLOSE(iox)
c -------------------------------------------------------------------
c --- Previous header structure follows, with old record #1 augmented
c --- to include i2dmet and /MAP/ variables
c frr Additional flag i2dmet: if =1: 2D array of RH
c --- Add TIME to seconds; source counts by type; and MSOURCE option
c -------------------------------------------------------------------
c
c --- HEADER RECORD #NCOM+3 -- General run parameters
c
      if(mpirank.eq.0) then 
c      if(ICON.eq.1)write(io8)mmodel,ver,level,iyrb,ijulb,ihrb,isecb,
c     1 xbtz,irlg,iavg,nsecdt,nx,ny,dxkm,dykm,ione,xorigkm,yorigkm,nssta,
c     2 ibcomp,iecomp,jbcomp,jecomp,ibsamp,jbsamp,iesamp,jesamp,
c     3 meshdn,npt1,npt2,nar1,nar2,nlines,nln2,nvl1,nvl2,msource,
c     4 nrec,nctrec,lsamp,nspout,lcomprs,i2dmet,
c     5 iutmzn,feast,fnorth,rnlat0,relon0,xlat1,xlat2,
c     6 pmap,utmhem,datum,daten,clat0,clon0,clat1,clat2
       if(ICON.eq.1) then
c 45 ints, reals (45*4), 2 logicals (2*4), 132 char. bytes
        irec_size = 320
        call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),irec_size,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io8)=mpifilebytes(io8)+4
c mmodel - character*12
        call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),mmodel,12,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io8)=mpifilebytes(io8)+12
c ver - character*12
        call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),ver,12,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io8)=mpifilebytes(io8)+12
c level - character*12
        call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),level,12,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io8)=mpifilebytes(io8)+12
c assume 4 bytes for ints and reals
        call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),iyrb,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io8)=mpifilebytes(io8)+4
        call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),ijulb,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io8)=mpifilebytes(io8)+4
        call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),ihrb,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io8)=mpifilebytes(io8)+4
        call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),isecb,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io8)=mpifilebytes(io8)+4
        call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),xbtz,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io8)=mpifilebytes(io8)+4
        call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),irlg,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io8)=mpifilebytes(io8)+4
        call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),iavg,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io8)=mpifilebytes(io8)+4
        call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),nsecdt,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io8)=mpifilebytes(io8)+4
        call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),nx,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io8)=mpifilebytes(io8)+4
        call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),ny,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io8)=mpifilebytes(io8)+4
        call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),dxkm,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io8)=mpifilebytes(io8)+4
        call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),dykm,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io8)=mpifilebytes(io8)+4
        call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),ione,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io8)=mpifilebytes(io8)+4
        call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),xorigkm,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io8)=mpifilebytes(io8)+4
        call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),yorigkm,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io8)=mpifilebytes(io8)+4
        call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),nssta,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io8)=mpifilebytes(io8)+4
        call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),ibcomp,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io8)=mpifilebytes(io8)+4
        call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),iecomp,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io8)=mpifilebytes(io8)+4
        call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),jbcomp,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io8)=mpifilebytes(io8)+4
        call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),jecomp,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io8)=mpifilebytes(io8)+4
        call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),ibsamp,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io8)=mpifilebytes(io8)+4
        call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),jbsamp,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io8)=mpifilebytes(io8)+4
        call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),iesamp,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io8)=mpifilebytes(io8)+4
        call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),jesamp,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io8)=mpifilebytes(io8)+4
        call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),meshdn,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io8)=mpifilebytes(io8)+4
        call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),npt1,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io8)=mpifilebytes(io8)+4
        call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),npt2,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io8)=mpifilebytes(io8)+4
        call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),nar1,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io8)=mpifilebytes(io8)+4
        call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),nar2,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io8)=mpifilebytes(io8)+4
        call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),nlines,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io8)=mpifilebytes(io8)+4
        call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),nln2,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io8)=mpifilebytes(io8)+4
        call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),nvl1,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io8)=mpifilebytes(io8)+4
        call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),nvl2,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io8)=mpifilebytes(io8)+4
        call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),msource,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io8)=mpifilebytes(io8)+4
        call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),nrec,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io8)=mpifilebytes(io8)+4
        call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),nctrec,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io8)=mpifilebytes(io8)+4
c lsamp - logical, assume 4 bytes
        call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),lsamp,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io8)=mpifilebytes(io8)+4
c assume 4 bytes for ints and reals
        call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),nspout,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io8)=mpifilebytes(io8)+4
c lcomprs - logical, assume 4 bytes
        call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),lcomprs,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io8)=mpifilebytes(io8)+4
c assume 4 bytes for ints and reals
        call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),i2dmet,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io8)=mpifilebytes(io8)+4
        call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),iutmzn,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io8)=mpifilebytes(io8)+4
        call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),feast,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io8)=mpifilebytes(io8)+4
        call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),fnorth,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io8)=mpifilebytes(io8)+4
        call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),rnlat0,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io8)=mpifilebytes(io8)+4
        call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),relon0,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io8)=mpifilebytes(io8)+4
        call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),xlat1,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io8)=mpifilebytes(io8)+4
        call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),xlat2,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io8)=mpifilebytes(io8)+4
c pmap - character*8
        call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),pmap,8,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io8)=mpifilebytes(io8)+8
c utmhem - character*4
        call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),utmhem,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io8)=mpifilebytes(io8)+4
c datum - character*8
        call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),datum,8,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io8)=mpifilebytes(io8)+8
c daten - character*12
        call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),daten,12,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io8)=mpifilebytes(io8)+12
c clat0 - character*16
        call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),clat0,16,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io8)=mpifilebytes(io8)+16
c clon0 - character*16
        call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),clon0,16,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io8)=mpifilebytes(io8)+16
c clat1 - character*16
        call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),clat1,16,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io8)=mpifilebytes(io8)+16
c clat2 - character*16
        call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),clat2,16,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io8)=mpifilebytes(io8)+16
        call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),irec_size,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io8)=mpifilebytes(io8)+4
       endif ! ICON.eq.1
c
c      if(IDRY.eq.1)write(io9)mmodel,ver,level,iyrb,ijulb,ihrb,isecb,
c     1 xbtz,irlg,iavg,nsecdt,nx,ny,dxkm,dykm,ione,xorigkm,yorigkm,nssta,
c     2 ibcomp,iecomp,jbcomp,jecomp,ibsamp,jbsamp,iesamp,jesamp,
c     3 meshdn,npt1,npt2,nar1,nar2,nlines,nln2,nvl1,nvl2,msource,
c     4 nrec,nctrec,lsamp,ndfout,lcomprs,i2dmet,
c     5 iutmzn,feast,fnorth,rnlat0,relon0,xlat1,xlat2,
c     6 pmap,utmhem,datum,daten,clat0,clon0,clat1,clat2
c
       if(IDRY.eq.1) then
c 45 ints, reals (45*4), 2 logicals (2*4), 132 char. bytes
        irec_size = 320
        call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),irec_size,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io9)=mpifilebytes(io9)+4
c mmodel - character*12
        call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),mmodel,12,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io9)=mpifilebytes(io9)+12
c ver - character*12
        call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),ver,12,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io9)=mpifilebytes(io9)+12
c level - character*12
        call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),level,12,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io9)=mpifilebytes(io9)+12
c assume 4 bytes for ints and reals
        call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),iyrb,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io9)=mpifilebytes(io9)+4
        call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),ijulb,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io9)=mpifilebytes(io9)+4
        call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),ihrb,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io9)=mpifilebytes(io9)+4
        call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),isecb,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io9)=mpifilebytes(io9)+4
        call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),xbtz,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io9)=mpifilebytes(io9)+4
        call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),irlg,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io9)=mpifilebytes(io9)+4
        call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),iavg,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io9)=mpifilebytes(io9)+4
        call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),nsecdt,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io9)=mpifilebytes(io9)+4
        call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),nx,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io9)=mpifilebytes(io9)+4
        call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),ny,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io9)=mpifilebytes(io9)+4
        call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),dxkm,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io9)=mpifilebytes(io9)+4
        call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),dykm,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io9)=mpifilebytes(io9)+4
        call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),ione,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io9)=mpifilebytes(io9)+4
        call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),xorigkm,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io9)=mpifilebytes(io9)+4
        call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),yorigkm,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io9)=mpifilebytes(io9)+4
        call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),nssta,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io9)=mpifilebytes(io9)+4
        call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),ibcomp,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io9)=mpifilebytes(io9)+4
        call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),iecomp,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io9)=mpifilebytes(io9)+4
        call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),jbcomp,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io9)=mpifilebytes(io9)+4
        call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),jecomp,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io9)=mpifilebytes(io9)+4
        call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),ibsamp,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io9)=mpifilebytes(io9)+4
        call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),jbsamp,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io9)=mpifilebytes(io9)+4
        call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),iesamp,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io9)=mpifilebytes(io9)+4
        call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),jesamp,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io9)=mpifilebytes(io9)+4
        call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),meshdn,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io9)=mpifilebytes(io9)+4
        call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),npt1,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io9)=mpifilebytes(io9)+4
        call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),npt2,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io9)=mpifilebytes(io9)+4
        call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),nar1,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io9)=mpifilebytes(io9)+4
        call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),nar2,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io9)=mpifilebytes(io9)+4
        call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),nlines,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io9)=mpifilebytes(io9)+4
        call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),nln2,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io9)=mpifilebytes(io9)+4
        call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),nvl1,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io9)=mpifilebytes(io9)+4
        call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),nvl2,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io9)=mpifilebytes(io9)+4
        call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),msource,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io9)=mpifilebytes(io9)+4
        call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),nrec,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io9)=mpifilebytes(io9)+4
        call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),nctrec,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io9)=mpifilebytes(io9)+4
c lsamp - logical, assume 4 bytes
        call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),lsamp,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io9)=mpifilebytes(io9)+4
c assume 4 bytes for ints and reals
        call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),nspout,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io9)=mpifilebytes(io9)+4
c lcomprs - logical, assume 4 bytes
        call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),lcomprs,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io9)=mpifilebytes(io9)+4
c assume 4 bytes for ints and reals
        call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),i2dmet,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io9)=mpifilebytes(io9)+4
        call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),iutmzn,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io9)=mpifilebytes(io9)+4
        call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),feast,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io9)=mpifilebytes(io9)+4
        call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),fnorth,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io9)=mpifilebytes(io9)+4
        call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),rnlat0,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io9)=mpifilebytes(io9)+4
        call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),relon0,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io9)=mpifilebytes(io9)+4
        call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),xlat1,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io9)=mpifilebytes(io9)+4
        call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),xlat2,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io9)=mpifilebytes(io9)+4
c pmap - character*8
        call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),pmap,8,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io9)=mpifilebytes(io9)+8
c utmhem - character*4
        call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),utmhem,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io9)=mpifilebytes(io9)+4
c datum - character*8
        call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),datum,8,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io9)=mpifilebytes(io9)+8
c daten - character*12
        call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),daten,12,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io9)=mpifilebytes(io9)+12
c clat0 - character*16
        call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),clat0,16,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io9)=mpifilebytes(io9)+16
c clon0 - character*16
        call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),clon0,16,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io9)=mpifilebytes(io9)+16
c clat1 - character*16
        call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),clat1,16,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io9)=mpifilebytes(io9)+16
c clat2 - character*16
        call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),clat2,16,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io9)=mpifilebytes(io9)+16
        call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),irec_size,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io9)=mpifilebytes(io9)+4
       endif ! IDRY.eq.1
c
c      if(IWET.eq.1)write(io10)mmodel,ver,level,iyrb,ijulb,ihrb,isecb,
c     1 xbtz,irlg,iavg,nsecdt,nx,ny,dxkm,dykm,ione,xorigkm,yorigkm,nssta,
c     2 ibcomp,iecomp,jbcomp,jecomp,ibsamp,jbsamp,iesamp,jesamp,
c     3 meshdn,npt1,npt2,nar1,nar2,nlines,nln2,nvl1,nvl2,msource,
c     4 nrec,nctrec,lsamp,nwfout,lcomprs,i2dmet,
c     5 iutmzn,feast,fnorth,rnlat0,relon0,xlat1,xlat2,
c     6 pmap,utmhem,datum,daten,clat0,clon0,clat1,clat2
c
       if(IWET.eq.1) then
c 45 ints, reals (45*4), 2 logicals (2*4), 132 char. bytes
        irec_size = 320
        call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),irec_size,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io10)=mpifilebytes(io10)+4
c mmodel - character*12
        call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),mmodel,12,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io10)=mpifilebytes(io10)+12
c ver - character*12
        call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),ver,12,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io10)=mpifilebytes(io10)+12
c level - character*12
        call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),level,12,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io10)=mpifilebytes(io10)+12
c assume 4 bytes for ints and reals
        call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),iyrb,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io10)=mpifilebytes(io10)+4
        call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),ijulb,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io10)=mpifilebytes(io10)+4
        call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),ihrb,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io10)=mpifilebytes(io10)+4
        call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),isecb,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io10)=mpifilebytes(io10)+4
        call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),xbtz,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io10)=mpifilebytes(io10)+4
        call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),irlg,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io10)=mpifilebytes(io10)+4
        call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),iavg,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io10)=mpifilebytes(io10)+4
        call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),nsecdt,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io10)=mpifilebytes(io10)+4
        call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),nx,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io10)=mpifilebytes(io10)+4
        call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),ny,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io10)=mpifilebytes(io10)+4
        call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),dxkm,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io10)=mpifilebytes(io10)+4
        call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),dykm,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io10)=mpifilebytes(io10)+4
        call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),ione,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io10)=mpifilebytes(io10)+4
        call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),xorigkm,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io10)=mpifilebytes(io10)+4
        call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),yorigkm,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io10)=mpifilebytes(io10)+4
        call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),nssta,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io10)=mpifilebytes(io10)+4
        call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),ibcomp,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io10)=mpifilebytes(io10)+4
        call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),iecomp,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io10)=mpifilebytes(io10)+4
        call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),jbcomp,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io10)=mpifilebytes(io10)+4
        call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),jecomp,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io10)=mpifilebytes(io10)+4
        call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),ibsamp,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io10)=mpifilebytes(io10)+4
        call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),jbsamp,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io10)=mpifilebytes(io10)+4
        call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),iesamp,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io10)=mpifilebytes(io10)+4
        call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),jesamp,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io10)=mpifilebytes(io10)+4
        call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),meshdn,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io10)=mpifilebytes(io10)+4
        call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),npt1,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io10)=mpifilebytes(io10)+4
        call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),npt2,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io10)=mpifilebytes(io10)+4
        call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),nar1,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io10)=mpifilebytes(io10)+4
        call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),nar2,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io10)=mpifilebytes(io10)+4
        call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),nlines,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io10)=mpifilebytes(io10)+4
        call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),nln2,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io10)=mpifilebytes(io10)+4
        call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),nvl1,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io10)=mpifilebytes(io10)+4
        call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),nvl2,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io10)=mpifilebytes(io10)+4
        call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),msource,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io10)=mpifilebytes(io10)+4
        call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),nrec,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io10)=mpifilebytes(io10)+4
        call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),nctrec,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io10)=mpifilebytes(io10)+4
c lsamp - logical, assume 4 bytes
        call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),lsamp,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io10)=mpifilebytes(io10)+4
c assume 4 bytes for ints and reals
        call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),nspout,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io10)=mpifilebytes(io10)+4
c lcomprs - logical, assume 4 bytes
        call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),lcomprs,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io10)=mpifilebytes(io10)+4
c assume 4 bytes for ints and reals
        call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),i2dmet,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io10)=mpifilebytes(io10)+4
        call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),iutmzn,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io10)=mpifilebytes(io10)+4
        call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),feast,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io10)=mpifilebytes(io10)+4
        call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),fnorth,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io10)=mpifilebytes(io10)+4
        call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),rnlat0,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io10)=mpifilebytes(io10)+4
        call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),relon0,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io10)=mpifilebytes(io10)+4
        call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),xlat1,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io10)=mpifilebytes(io10)+4
        call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),xlat2,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io10)=mpifilebytes(io10)+4
c pmap - character*8
        call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),pmap,8,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io10)=mpifilebytes(io10)+8
c utmhem - character*4
        call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),utmhem,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io10)=mpifilebytes(io10)+4
c datum - character*8
        call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),datum,8,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io10)=mpifilebytes(io10)+8
c daten - character*12
        call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),daten,12,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io10)=mpifilebytes(io10)+12
c clat0 - character*16
        call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),clat0,16,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io10)=mpifilebytes(io10)+16
c clon0 - character*16
        call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),clon0,16,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io10)=mpifilebytes(io10)+16
c clat1 - character*16
        call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),clat1,16,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io10)=mpifilebytes(io10)+16
c clat2 - character*16
        call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),clat2,16,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io10)=mpifilebytes(io10)+16
        call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),irec_size,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io10)=mpifilebytes(io10)+4
      endif ! IWET.eq.1
        
      if(IVIS.eq.1)write(io11)mmodel,ver,level,iyrb,ijulb,ihrb,isecb,
     1 xbtz,irlg,iavg,nsecdt,nx,ny,dxkm,dykm,ione,xorigkm,yorigkm,nssta,
     2 ibcomp,iecomp,jbcomp,jecomp,ibsamp,jbsamp,iesamp,jesamp,
     3 meshdn,npt1,npt2,nar1,nar2,nlines,nln2,nvl1,nvl2,msource,
     4 nrec,nctrec,lsamp,nvsout,lcomprs,i2dmet,
     5 iutmzn,feast,fnorth,rnlat0,relon0,xlat1,xlat2,
     6 pmap,utmhem,datum,daten,clat0,clon0,clat1,clat2


      if(IT2D.eq.1)write(io13)mmodel,ver,level,iyrb,ijulb,ihrb,isecb,
     1 xbtz,irlg,iavg,nsecdt,nx,ny,dxkm,dykm,ione,xorigkm,yorigkm,nssta,
     2 ibcomp,iecomp,jbcomp,jecomp,ibsamp,jbsamp,iesamp,jesamp,
     3 meshdn,npt1,npt2,nar1,nar2,nlines,nln2,nvl1,nvl2,msource,
     4 nrec,nctrec,lsamp,nt2out,lcomprs,i2dmet,
     5 iutmzn,feast,fnorth,rnlat0,relon0,xlat1,xlat2,
     6 pmap,utmhem,datum,daten,clat0,clon0,clat1,clat2


      if(IRHO.eq.1)write(io14)mmodel,ver,level,iyrb,ijulb,ihrb,isecb,
     1 xbtz,irlg,iavg,nsecdt,nx,ny,dxkm,dykm,ione,xorigkm,yorigkm,nssta,
     2 ibcomp,iecomp,jbcomp,jecomp,ibsamp,jbsamp,iesamp,jesamp,
     3 meshdn,npt1,npt2,nar1,nar2,nlines,nln2,nvl1,nvl2,msource,
     4 nrec,nctrec,lsamp,nrhoout,lcomprs,i2dmet,
     5 iutmzn,feast,fnorth,rnlat0,relon0,xlat1,xlat2,
     6 pmap,utmhem,datum,daten,clat0,clon0,clat1,clat2
      endif ! mpirank.eq.0

c
c --- HEADER RECORD #NCOM+4 -- Run title
c   
      if(mpirank.eq.0) then
c      if(ICON.eq.1)write(io8)title
       if(ICON.eq.1) then
c title(3) - char. *80
       ilen = len(title(1))
       irec_size = 3*ilen
        call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),irec_size,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io8)=mpifilebytes(io8)+4
         do n=1,3
          call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),title(n),ilen,
     1                           MPI_BYTE,istat,ierr)
          mpifilebytes(io8)=mpifilebytes(io8)+ilen
         enddo
        call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),irec_size,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io8)=mpifilebytes(io8)+4
       endif ! ICON.eq.1
c      if(IDRY.eq.1)write(io9)title
       if(IDRY.eq.1) then
c title(3) - char. *80
       ilen = len(title(1))
       irec_size = 3*ilen
        call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),irec_size,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io9)=mpifilebytes(io9)+4
         do n=1,3
          call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),title(n),ilen,
     1                           MPI_BYTE,istat,ierr)
          mpifilebytes(io9)=mpifilebytes(io9)+ilen
         enddo
        call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),irec_size,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io9)=mpifilebytes(io9)+4
       endif ! IDRY.eq.1
c      if(IWET.eq.1)write(io10)title
       if(IWET.eq.1) then
c title(3) - char. *80
       ilen = len(title(1))
       irec_size = 3*ilen
        call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),irec_size,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io10)=mpifilebytes(io10)+4
         do n=1,3
          call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),title(n),ilen,
     1                           MPI_BYTE,istat,ierr)
          mpifilebytes(io10)=mpifilebytes(io10)+ilen
         enddo
        call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),irec_size,4,
     1                         MPI_BYTE,istat,ierr)
        mpifilebytes(io10)=mpifilebytes(io10)+4
       endif ! IWET.eq.1
      if(IVIS.eq.1)write(io11)title
      if(IT2D.eq.1)write(io13)title
      if(IRHO.eq.1)write(io14)title
      endif ! mpirank.eq.0
c
c --- HEADER RECORD #NCOM+5 -- List of species-groups output
      nc=0
      nd=0
      nw=0
      do 20 k=1,ngrup

c --- Set output concentration/layer labels
      if(ioutop(2,k).eq.1)then
         nc=nc+1
         write(csout(nc)(1:12),'(a12)')cgrup(k)
         csout(nc)(13:15)='  1'
      endif
c --- Set output dry flux labels
      if(ioutop(4,k).eq.1)then
         nd=nd+1
         write(cdfout(nd)(1:12),'(a12)')cgrup(k)
         cdfout(nd)(13:15)=' DF'
      endif
c
c --- Set output wet flux labels
      if(ioutop(6,k).eq.1)then
         nw=nw+1
         write(cwfout(nw)(1:12),'(a12)')cgrup(k)
         cwfout(nw)(13:15)=' WF'
      endif
20    continue
c
c --- Set output visibility labels
      nv=1
      write(cvsout(nv)(1:12),'(a12)') ' REL HUM (%)   '
      write(ct2out(1:12),'(a12)') ' TEMP 2D (K)   '
      write(crhoout(1:12),'(a12)') ' RHO 2D (K)    '
c
c write(io8)(csout(n),n=1,nspout)
c
      if(mpirank.eq.0) then
        if(ICON.eq.1) then
c --- character*15 csout
         ilen = len(csout(1))
         irec_size = nspout*ilen
         call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),irec_size,4,
     1                          MPI_BYTE,istat,ierr)
         mpifilebytes(io8)=mpifilebytes(io8)+4
          do n = 1,nspout
           call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),csout(n),ilen,
     1                            MPI_BYTE,istat,ierr)
           mpifilebytes(io8)=mpifilebytes(io8)+ilen
          enddo
         call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),irec_size,4,
     1                          MPI_BYTE,istat,ierr)
         mpifilebytes(io8)=mpifilebytes(io8)+4
        endif ! ICON.eq.1
c      if(IDRY.eq.1)write(io9)(cdfout(n),n=1,ndfout)
        if(IDRY.eq.1) then
c --- character*15 cdfout
         ilen = len(cdfout(1))
         irec_size = ndfout*ilen
         call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),irec_size,4,
     1                          MPI_BYTE,istat,ierr)
         mpifilebytes(io9)=mpifilebytes(io9)+4
          do n = 1,ndfout
           call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),cdfout(n),ilen,
     1                            MPI_BYTE,istat,ierr)
           mpifilebytes(io9)=mpifilebytes(io9)+ilen
          enddo
         call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),irec_size,4,
     1                          MPI_BYTE,istat,ierr)
         mpifilebytes(io9)=mpifilebytes(io9)+4
        endif ! IDRY.eq.1
c      if(IWET.eq.1)write(io10)(cwfout(n),n=1,nwfout)
        if(IWET.eq.1) then
c --- character*15 cdfout
         ilen = len(cwfout(1))
         irec_size = nwfout*ilen
         call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),irec_size,4,
     1                          MPI_BYTE,istat,ierr)
         mpifilebytes(io10)=mpifilebytes(io10)+4
          do n = 1,nwfout
           call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),cwfout(n),
     1                            ilen,MPI_BYTE,istat,ierr)
           mpifilebytes(io10)=mpifilebytes(io10)+ilen
          enddo
         call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),irec_size,4,
     1                          MPI_BYTE,istat,ierr)
         mpifilebytes(io10)=mpifilebytes(io10)+4
        endif ! IWET.eq.1
      if(IVIS.eq.1)write(io11)(cvsout(n),n=1,nvsout)
      if(IT2D.eq.1)write(io13)(ct2out)
      if(IRHO.eq.1)write(io14)(crhoout)
      endif ! mpirank.eq.0
c
c --- HEADER RECORD #NCOM+6 -- Discrete (non-gridded) receptor data
c
c --- Convert receptor coordinates from grid units back to UTM (km)
      if(mpirank.eq.0) then 
        if(nrec.gt.0)then
            do 30 i=1,nrec
            tmp3(i)=xng(i)*dgridkm+xorigkm
            tmp4(i)=yng(i)*dgridkm+yorigkm
30          continue
c
c
c         if(ICON.eq.1)write(io8)(tmp3(n),n=1,nrec),
c     1    (tmp4(n2),n2=1,nrec),(elevng(n3),n3=1,nrec)
           if(ICON.eq.1) then
             irec_size = 4*(nrec*3)! assume 4 bytes for floats and ints
             call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),irec_size,4,
     1                              MPI_BYTE,istat,ierr)
             mpifilebytes(io8)=mpifilebytes(io8)+4
               do n=1,nrec
                call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),tmp3(n),4,
     1                                 MPI_BYTE,istat,ierr)
                mpifilebytes(io8)=mpifilebytes(io8)+4
               enddo
               do n2=1,nrec
                call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),tmp4(n2),4,
     1                                 MPI_BYTE,istat,ierr)
                mpifilebytes(io8)=mpifilebytes(io8)+4
               enddo
              do n3=1,nrec
                call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),elevng(n3),
     1                                4,MPI_BYTE,istat,ierr)
                mpifilebytes(io8)=mpifilebytes(io8)+4
               enddo
             call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),irec_size,4,
     1                              MPI_BYTE,istat,ierr)
             mpifilebytes(io8)=mpifilebytes(io8)+4
           endif ! (ICON.eq.1)
c         if(IDRY.eq.1)write(io9)(tmp3(n),n=1,nrec),
c     1    (tmp4(n2),n2=1,nrec),(elevng(n3),n3=1,nrec)
           if(IDRY.eq.1) then
             irec_size = 4*(nrec*3)! assume 4 bytes for floats and ints
             call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),irec_size,4,
     1                              MPI_BYTE,istat,ierr)
             mpifilebytes(io9)=mpifilebytes(io9)+4
               do n=1,nrec
                call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),tmp3(n),4,
     1                                 MPI_BYTE,istat,ierr)
                mpifilebytes(io9)=mpifilebytes(io9)+4
               enddo
               do n2=1,nrec
                call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),tmp4(n2),4,
     1                                 MPI_BYTE,istat,ierr)
                mpifilebytes(io9)=mpifilebytes(io9)+4
               enddo
              do n3=1,nrec
                call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),elevng(n3),
     1                                 4,MPI_BYTE,istat,ierr)
                mpifilebytes(io9)=mpifilebytes(io9)+4
               enddo
             call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),irec_size,4,
     1                              MPI_BYTE,istat,ierr)
             mpifilebytes(io9)=mpifilebytes(io9)+4
           endif ! (IDRY.eq.1)
c         if(IWET.eq.1)write(io10)(tmp3(n),n=1,nrec),
c     1    (tmp4(n2),n2=1,nrec),(elevng(n3),n3=1,nrec)
          if(IWET.eq.1) then
            irec_size = 4*(nrec*3)! assume 4 bytes for floats and ints
             call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),irec_size,4,
     1                              MPI_BYTE,istat,ierr)
             mpifilebytes(io10)=mpifilebytes(io10)+4
               do n=1,nrec
                call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),tmp3(n),
     1                                 4,MPI_BYTE,istat,ierr)
                mpifilebytes(io10)=mpifilebytes(io10)+4
               enddo
               do n2=1,nrec
                call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),tmp4(n2),
     1                                 4,MPI_BYTE,istat,ierr)
                mpifilebytes(io10)=mpifilebytes(io10)+4
               enddo
              do n3=1,nrec
                call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),
     1                                 elevng(n3),4,MPI_BYTE,istat,ierr)
                mpifilebytes(io10)=mpifilebytes(io10)+4
               enddo
             call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),irec_size,4,
     1                              MPI_BYTE,istat,ierr)
             mpifilebytes(io10)=mpifilebytes(io10)+4
           endif ! (IWET.eq.1)
         if(IVIS.eq.1)write(io11)(tmp3(n),n=1,nrec),
     1    (tmp4(n2),n2=1,nrec),(elevng(n3),n3=1,nrec)
         if(IT2D.eq.1)write(io13)(tmp3(n),n=1,nrec),
     1    (tmp4(n2),n2=1,nrec),(elevng(n3),n3=1,nrec)
         if(IRHO.eq.1)write(io14)(tmp3(n),n=1,nrec),
     1    (tmp4(n2),n2=1,nrec),(elevng(n3),n3=1,nrec)
        endif ! nrec.gt.0
      endif ! mpirank.eq.0
c
c --- HEADER RECORD #NCOM+7 -- Complex terrain receptor data
c
c --- Convert CTSG receptor coordinates from grid units back to UTM (km)

      if(mpirank.eq.0) then
        if(nctrec.gt.0)then
            do 40 i=1,nctrec
            tmp5(i)=xrct(i)*dgridkm+xorigkm
            tmp6(i)=yrct(i)*dgridkm+yorigkm
40          continue
c
c         if(ICON.eq.1)write(io8)(tmp5(n),n=1,nctrec),
c     1    (tmp6(n2),n2=1,nctrec),(elrect(n3),n3=1,nctrec),
c     2    (ihill(n4),n4=1,nctrec)
c
           if(ICON.eq.1) then
            irec_size=(4*nctrec)*4
            call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),irec_size,4,
     1                             MPI_BYTE,istat,ierr)
            mpifilebytes(io8)=mpifilebytes(io8)+4
             do n=1,nctrec
              call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),tmp5(n),4,
     1                               MPI_BYTE,istat,ierr)
              mpifilebytes(io8)=mpifilebytes(io8)+4
             enddo
             do n2=1,nctrec
              call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),tmp6(n2),4,
     1                               MPI_BYTE,istat,ierr)
              mpifilebytes(io8)=mpifilebytes(io8)+4
             enddo
             do n3=1,nctrec
              call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),elrect(n3),4,
     1                               MPI_BYTE,istat,ierr)
              mpifilebytes(io8)=mpifilebytes(io8)+4
             enddo
             do n4=1,nctrec
              call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),ihill(n4),4,
     1                               MPI_BYTE,istat,ierr)
              mpifilebytes(io8)=mpifilebytes(io8)+4
             enddo
            call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),irec_size,4,
     1                             MPI_BYTE,istat,ierr)
            mpifilebytes(io8)=mpifilebytes(io8)+4
           endif ! ICON.eq.1
c         if(IDRY.eq.1)write(io9)(tmp5(n),n=1,nctrec),
c     1    (tmp6(n2),n2=1,nctrec),(elrect(n3),n3=1,nctrec),
c     2    (ihill(n4),n4=1,nctrec)
           if(IDRY.eq.1) then
            irec_size=(4*nctrec)*4
            call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),irec_size,4,
     1                             MPI_BYTE,istat,ierr)
            mpifilebytes(io9)=mpifilebytes(io9)+4
             do n=1,nctrec
              call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),tmp5(n),4,
     1                               MPI_BYTE,istat,ierr)
              mpifilebytes(io9)=mpifilebytes(io9)+4
             enddo
             do n2=1,nctrec
              call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),tmp6(n2),4,
     1                               MPI_BYTE,istat,ierr)
              mpifilebytes(io9)=mpifilebytes(io9)+4
             enddo
             do n3=1,nctrec
              call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),elrect(n3),4,
     1                               MPI_BYTE,istat,ierr)
              mpifilebytes(io9)=mpifilebytes(io9)+4
             enddo
             do n4=1,nctrec
              call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),ihill(n4),4,
     1                               MPI_BYTE,istat,ierr)
              mpifilebytes(io9)=mpifilebytes(io9)+4
             enddo
            call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),irec_size,4,
     1                             MPI_BYTE,istat,ierr)
            mpifilebytes(io9)=mpifilebytes(io9)+4
           endif ! IDRY.eq.1
c         if(IWET.eq.1)write(io10)(tmp5(n),n=1,nctrec),
c     1    (tmp6(n2),n2=1,nctrec),(elrect(n3),n3=1,nctrec),
c     2    (ihill(n4),n4=1,nctrec)
           if(IWET.eq.1) then
            irec_size=(4*nctrec)*4
            call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),irec_size,4,
     1                             MPI_BYTE,istat,ierr)
            mpifilebytes(io10)=mpifilebytes(io10)+4
             do n=1,nctrec
              call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),tmp5(n),4,
     1                               MPI_BYTE,istat,ierr)
              mpifilebytes(io10)=mpifilebytes(io10)+4
             enddo
             do n2=1,nctrec
              call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),tmp6(n2),4,
     1                               MPI_BYTE,istat,ierr)
              mpifilebytes(io10)=mpifilebytes(io10)+4
             enddo
             do n3=1,nctrec
              call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),elrect(n3),
     1                               4,MPI_BYTE,istat,ierr)
              mpifilebytes(io10)=mpifilebytes(io10)+4
             enddo
             do n4=1,nctrec
              call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),ihill(n4),
     1                               4,MPI_BYTE,istat,ierr)
              mpifilebytes(io10)=mpifilebytes(io10)+4
             enddo
            call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),irec_size,4,
     1                             MPI_BYTE,istat,ierr)
            mpifilebytes(io10)=mpifilebytes(io10)+4
           endif ! IWET.eq.1
c         if(IVIS.eq.1)write(io11)(tmp5(n),n=1,nctrec),
c     1    (tmp6(n2),n2=1,nctrec),(elrect(n3),n3=1,nctrec),
c     2    (ihill(n4),n4=1,nctrec)
c         if(IT2D.eq.1)write(io13)(tmp5(n),n=1,nctrec),
c     1    (tmp6(n2),n2=1,nctrec),(elrect(n3),n3=1,nctrec),
c     2    (ihill(n4),n4=1,nctrec)
c         if(IRHO.eq.1)write(io14)(tmp5(n),n=1,nctrec),
c     1    (tmp6(n2),n2=1,nctrec),(elrect(n3),n3=1,nctrec),
c     2    (ihill(n4),n4=1,nctrec)
        endif ! nctrec.gt.0
      endif ! mpirank.eq.0
c
c --- HEADER RECORDs #NCOM+8 to 15 -- Source names
c     (not written to VISIBILITY files)
c
      if(mpirank.eq.0) then 
        if(npt1.GT.0) then
         itype=1
c         if(ICON.eq.1)write(io8) itype,(cnampt1(n),n=1,npt1)
c cnampt1 - character*16
         if(ICON.eq.1) then
          ilen = len(cnampt1(1))
          irec_size = (npt1*ilen)+4
          call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),irec_size,4,
     1                           MPI_BYTE,istat,ierr)
          mpifilebytes(io8)=mpifilebytes(io8)+4
          call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),itype,4,
     1                           MPI_BYTE,istat,ierr)
          mpifilebytes(io8)=mpifilebytes(io8)+4
          do n=1,npt1
           call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),cnampt1(n),
     1                            ilen,MPI_BYTE,istat,ierr)
           mpifilebytes(io8)=mpifilebytes(io8)+ilen
          enddo
          call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),irec_size,4,
     1                            MPI_BYTE,istat,ierr)
          mpifilebytes(io8)=mpifilebytes(io8)+4
         endif ! ICON.eq.1
c         if(IDRY.eq.1)write(io9) itype,(cnampt1(n),n=1,npt1)
         if(IDRY.eq.1) then
          ilen = len(cnampt1(1))
          irec_size = (npt1*ilen)+4
          call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),irec_size,4,
     1                           MPI_BYTE,istat,ierr)
          mpifilebytes(io9)=mpifilebytes(io9)+4
          call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),itype,4,
     1                           MPI_BYTE,istat,ierr)
          mpifilebytes(io9)=mpifilebytes(io9)+4
          do n=1,npt1
           call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),cnampt1(n),
     1                            ilen,MPI_BYTE,istat,ierr)
           mpifilebytes(io9)=mpifilebytes(io9)+ilen
          enddo
          call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),irec_size,4,
     1                            MPI_BYTE,istat,ierr)
          mpifilebytes(io9)=mpifilebytes(io9)+4
         endif ! IDRY.eq.1
c         if(IWET.eq.1)write(io10) itype,(cnampt1(n),n=1,npt1)
         if(IWET.eq.1) then
          ilen = len(cnampt1(1))
          irec_size = (npt1*ilen)+4
          call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),irec_size,4,
     1                           MPI_BYTE,istat,ierr)
          mpifilebytes(io10)=mpifilebytes(io10)+4
          call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),itype,4,
     1                           MPI_BYTE,istat,ierr)
          mpifilebytes(io10)=mpifilebytes(io10)+4
          do n=1,npt1
           call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),cnampt1(n),
     1                            ilen,MPI_BYTE,istat,ierr)
           mpifilebytes(io10)=mpifilebytes(io10)+ilen
          enddo
          call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),irec_size,4,
     1                            MPI_BYTE,istat,ierr)
          mpifilebytes(io10)=mpifilebytes(io10)+4
         endif ! IWET.eq.1
        endif ! npt1.GT.0
      endif ! mpirank.eq.0
c
      if(mpirank.eq.0) then
        if(npt2.GT.0) then
         itype=2
c         if(ICON.eq.1)write(io8) itype,(cid2(n),n=1,npt2)
         if(ICON.eq.1) then
c cid2 - character*16
          ilen=len(cid2(1))
          irec_size = (npt2*ilen)+4
          call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),irec_size,4,
     1                           MPI_BYTE,istat,ierr)
          mpifilebytes(io8)=mpifilebytes(io8)+4
          call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),itype,4,
     1                           MPI_BYTE,istat,ierr)
          mpifilebytes(io8)=mpifilebytes(io8)+4
           do n=1,npt2
            call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),cid2(n),ilen,
     1                             MPI_BYTE,istat,ierr)
            mpifilebytes(io8)=mpifilebytes(io8)+ilen
           enddo
          call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),irec_size,4,
     1                           MPI_BYTE,istat,ierr)
          mpifilebytes(io8)=mpifilebytes(io8)+4
         endif ! ICON.eq.1
c         if(IDRY.eq.1)write(io9) itype,(cid2(n),n=1,npt2)
         if(IDRY.eq.1) then
c cid2 - character*16
          ilen=len(cid2(1))
          irec_size = (npt2*ilen)+4
          call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),irec_size,4,
     1                           MPI_BYTE,istat,ierr)
          mpifilebytes(io9)=mpifilebytes(io9)+4
          call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),itype,4,
     1                           MPI_BYTE,istat,ierr)
          mpifilebytes(io9)=mpifilebytes(io9)+4
           do n=1,npt2
            call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),cid2(n),ilen,
     1                             MPI_BYTE,istat,ierr)
            mpifilebytes(io9)=mpifilebytes(io9)+ilen
           enddo
          call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),irec_size,4,
     1                           MPI_BYTE,istat,ierr)
          mpifilebytes(io9)=mpifilebytes(io9)+4
         endif ! IDRY.eq.1
c         if(IWET.eq.1)write(io10) itype,(cid2(n),n=1,npt2)
         if(IWET.eq.1) then
c cid2 - character*16
          ilen=len(cid2(1))
          irec_size = (npt2*ilen)+4
          call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),irec_size,4,
     1                           MPI_BYTE,istat,ierr)
          mpifilebytes(io10)=mpifilebytes(io10)+4
          call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),itype,4,
     1                           MPI_BYTE,istat,ierr)
          mpifilebytes(io10)=mpifilebytes(io10)+4
           do n=1,npt2
            call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),cid2(n),ilen,
     1                             MPI_BYTE,istat,ierr)
            mpifilebytes(io10)=mpifilebytes(io10)+ilen
           enddo
          call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),irec_size,4,
     1                           MPI_BYTE,istat,ierr)
          mpifilebytes(io10)=mpifilebytes(io10)+4
         endif ! IWET.eq.1
        endif ! npt2.gt.0
      endif ! mpirank.eq.0
c
      if(mpirank.eq.0) then
        if(nar1.GT.0) then
         itype=3
c         if(ICON.eq.1)write(io8) itype,(cnamar1(n),n=1,nar1)
          if(ICON.eq.1)then
c cnamar1 - character*16
           ilen=len(cnamar1(1))
           irec_size=(nar1*ilen)+4
           call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),irec_size,4,
     1                            MPI_BYTE,istat,ierr)
           mpifilebytes(io8)=mpifilebytes(io8)+4
           call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),itype,4,
     1                            MPI_BYTE,istat,ierr)
           mpifilebytes(io8)=mpifilebytes(io8)+4
           do n=1,nar1
            call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),cnamar1(n),
     1                             ilen,MPI_BYTE,istat,ierr)
            mpifilebytes(io8)=mpifilebytes(io8)+ilen
           enddo
           call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),irec_size,4,
     1                             MPI_BYTE,istat,ierr)
           mpifilebytes(io8)=mpifilebytes(io8)+4
          endif ! ICON.eq.1
c         if(IDRY.eq.1)write(io9) itype,(cnamar1(n),n=1,nar1)
          if(IDRY.eq.1)then
c cnamar1 - character*16
           ilen=len(cnamar1(1))
           irec_size=(nar1*ilen)+4
           call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),irec_size,4,
     1                            MPI_BYTE,istat,ierr)
           mpifilebytes(io9)=mpifilebytes(io9)+4
           call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),itype,4,
     1                            MPI_BYTE,istat,ierr)
           mpifilebytes(io9)=mpifilebytes(io9)+4
           do n=1,nar1
            call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),cnamar1(n),
     1                             ilen,MPI_BYTE,istat,ierr)
            mpifilebytes(io9)=mpifilebytes(io9)+ilen
           enddo
           call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),irec_size,4,
     1                             MPI_BYTE,istat,ierr)
           mpifilebytes(io9)=mpifilebytes(io9)+4
          endif ! IDRY.eq.1
c         if(IWET.eq.1)write(io10) itype,(cnamar1(n),n=1,nar1)
          if(IWET.eq.1)then
c cnamar1 - character*16
           ilen=len(cnamar1(1))
           irec_size=(nar1*ilen)+4
           call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),irec_size,4,
     1                            MPI_BYTE,istat,ierr)
           mpifilebytes(io10)=mpifilebytes(io10)+4
           call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),itype,4,
     1                            MPI_BYTE,istat,ierr)
           mpifilebytes(io10)=mpifilebytes(io10)+4
           do n=1,nar1
            call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),cnamar1(n),
     1                             ilen,MPI_BYTE,istat,ierr)
            mpifilebytes(io10)=mpifilebytes(io10)+ilen
           enddo
           call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),irec_size,4,
     1                             MPI_BYTE,istat,ierr)
           mpifilebytes(io10)=mpifilebytes(io10)+4
          endif ! IWET.eq.1
        endif ! nar1.GT.0
      endif ! mpirank.eq.0
c
      if(mpirank.eq.0) then
        if(nar2.GT.0) then
         itype=4
c         if(ICON.eq.1)write(io8) itype,(cid3(n),n=1,nar2)
         if(ICON.eq.1)then
c cid3 - character*16
          ilen=len(cid3(1))
          irec_size=(nar2*ilen)+4
          call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),irec_size,4,
     1                           MPI_BYTE,istat,ierr)
          mpifilebytes(io8)=mpifilebytes(io8)+4
          call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),itype,4,
     1                           MPI_BYTE,istat,ierr)
          mpifilebytes(io8)=mpifilebytes(io8)+4
          do n=1,nar2
           call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),cid3(n),ilen,
     1                            MPI_BYTE,istat,ierr)
           mpifilebytes(io8)=mpifilebytes(io8)+ilen
          enddo
          call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),irec_size,4,
     1                           MPI_BYTE,istat,ierr)
          mpifilebytes(io8)=mpifilebytes(io8)+4
         endif ! ICON.eq.1
c         if(IDRY.eq.1)write(io9) itype,(cid3(n),n=1,nar2)
         if(IDRY.eq.1)then
c cid3 - character*16
          ilen=len(cid3(1))
          irec_size=(nar2*ilen)+4
          call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),irec_size,4,
     1                           MPI_BYTE,istat,ierr)
          mpifilebytes(io9)=mpifilebytes(io9)+4
          call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),itype,4,
     1                           MPI_BYTE,istat,ierr)
          mpifilebytes(io9)=mpifilebytes(io9)+4
          do n=1,nar2
           call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),cid3(n),ilen,
     1                            MPI_BYTE,istat,ierr)
           mpifilebytes(io9)=mpifilebytes(io9)+ilen
          enddo
          call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),irec_size,4,
     1                           MPI_BYTE,istat,ierr)
          mpifilebytes(io9)=mpifilebytes(io9)+4
         endif ! IDRY.eq.1
c         if(IWET.eq.1)write(io10) itype,(cid3(n),n=1,nar2)
         if(IWET.eq.1)then
c cid3 - character*16
          ilen=len(cid3(1))
          irec_size=(nar2*ilen)+4
          call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),irec_size,4,
     1                           MPI_BYTE,istat,ierr)
          mpifilebytes(io10)=mpifilebytes(io10)+4
          call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),itype,4,
     1                           MPI_BYTE,istat,ierr)
          mpifilebytes(io10)=mpifilebytes(io10)+4
          do n=1,nar2
           call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),cid3(n),ilen,
     1                            MPI_BYTE,istat,ierr)
           mpifilebytes(io10)=mpifilebytes(io10)+ilen
          enddo
          call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),irec_size,4,
     1                           MPI_BYTE,istat,ierr)
          mpifilebytes(io10)=mpifilebytes(io10)+4
         endif ! IWET.eq.1
        endif ! nar2.GT.0
      endif ! mpirank.eq.0
c
      if(nlines.GT.0) then
         itype=5
         if(mpirank.eq.0) then
c         if(ICON.eq.1)write(io8) itype,(cnamln1(n),n=1,nlines)
          if(ICON.eq.1)then
c cnamln1 - character*16
           ilen=len(cnamln1(1))
           irec_size=(nlines*ilen)+4
           call MPI_FILE_WRITE_AT(io8,irec_size,4,MPI_BYTE,
     1                                istat,ierr)
           mpifilebytes(io8)=mpifilebytes(io8)+4
           call MPI_FILE_WRITE_AT(io8,itype,4,MPI_BYTE,istat,ierr)
           mpifilebytes(io8)=mpifilebytes(io8)+4
           do n=1,nlines
            call MPI_FILE_WRITE_AT(io8,cnamln1(n),ilen,
     1                                 MPI_BYTE,istat,ierr)
            mpifilebytes(io8)=mpifilebytes(io8)+4
           enddo
           call MPI_FILE_WRITE_AT(io8,irec_size,4,MPI_BYTE,
     1                                istat,ierr)
           mpifilebytes(io8)=mpifilebytes(io8)+4
          endif ! ICON.eq.1
c         if(IDRY.eq.1)write(io9) itype,(cnamln1(n),n=1,nlines)
          if(IDRY.eq.1)then
c cnamln1 - character*16
           ilen=len(cnamln1(1))
           irec_size=(nlines*ilen)+4
           call MPI_FILE_WRITE_AT(io9,irec_size,4,MPI_BYTE,
     1                                istat,ierr)
           mpifilebytes(io9)=mpifilebytes(io9)+4
           call MPI_FILE_WRITE_AT(io9,itype,4,MPI_BYTE,istat,ierr)
           mpifilebytes(io9)=mpifilebytes(io9)+4
           do n=1,nlines
            call MPI_FILE_WRITE_AT(io9,cnamln1(n),ilen,
     1                                 MPI_BYTE,istat,ierr)
            mpifilebytes(io9)=mpifilebytes(io9)+4
           enddo
           call MPI_FILE_WRITE_AT(io9,irec_size,4,MPI_BYTE,
     1                                istat,ierr)
           mpifilebytes(io9)=mpifilebytes(io9)+4
          endif ! IDRY.eq.1
c         if(IWET.eq.1)write(io10) itype,(cnamln1(n),n=1,nlines)
          if(IWET.eq.1)then
c cnamln1 - character*16
           ilen=len(cnamln1(1))
           irec_size=(nlines*ilen)+4
           call MPI_FILE_WRITE_AT(io10,irec_size,4,MPI_BYTE,
     1                                istat,ierr)
           mpifilebytes(io10)=mpifilebytes(io10)+4
           call MPI_FILE_WRITE_AT(io10,itype,4,MPI_BYTE,istat,ierr)
           mpifilebytes(io10)=mpifilebytes(io10)+4
           do n=1,nlines
            call MPI_FILE_WRITE_AT(io10,cnamln1(n),ilen,
     1                                 MPI_BYTE,istat,ierr)
            mpifilebytes(io10)=mpifilebytes(io10)+4
           enddo
           call MPI_FILE_WRITE_AT(io10,irec_size,4,MPI_BYTE,
     1                                istat,ierr)
           mpifilebytes(io10)=mpifilebytes(io10)+4
          endif ! IWET.eq.1
         endif
      endif
c
      if(nln2.GT.0) then
         itype=6
         if(mpirank.eq.0) then
c         if(ICON.eq.1)write(io8) itype,(cid5(n),n=1,nln2)
          if(ICON.eq.1)then
c cid5 - character*16
           ilen=len(cid5(1))
           irec_size=(nln2*ilen)+4
           call MPI_FILE_WRITE_AT(io8,irec_size,4,MPI_BYTE,
     1                                istat,ierr)
           mpifilebytes(io8)=mpifilebytes(io8)+4
           call MPI_FILE_WRITE_AT(io8,itype,4,MPI_BYTE,istat,ierr)
           mpifilebytes(io8)=mpifilebytes(io8)+4
           do n=1,nln2
            call MPI_FILE_WRITE_AT(io8,cid5(n),ilen,MPI_BYTE,
     1                                 istat,ierr)
            mpifilebytes(io8)=mpifilebytes(io8)+4
           enddo
           call MPI_FILE_WRITE_AT(io8,irec_size,4,MPI_BYTE,
     1                                istat,ierr)
           mpifilebytes(io8)=mpifilebytes(io8)+4
          endif ! ICON.eq.1            
c         if(IDRY.eq.1)write(io9) itype,(cid5(n),n=1,nln2)
          if(IDRY.eq.1)then
c cid5 - character*16
           ilen=len(cid5(1))
           irec_size=(nln2*ilen)+4
           call MPI_FILE_WRITE_AT(io9,irec_size,4,MPI_BYTE,
     1                                istat,ierr)
           mpifilebytes(io9)=mpifilebytes(io9)+4
           call MPI_FILE_WRITE_AT(io9,itype,4,MPI_BYTE,istat,ierr)
           mpifilebytes(io9)=mpifilebytes(io9)+4
           do n=1,nln2
            call MPI_FILE_WRITE_AT(io9,cid5(n),ilen,MPI_BYTE,
     1                                 istat,ierr)
            mpifilebytes(io9)=mpifilebytes(io9)+4
           enddo
           call MPI_FILE_WRITE_AT(io9,irec_size,4,MPI_BYTE,
     1                                istat,ierr)
           mpifilebytes(io9)=mpifilebytes(io9)+4
          endif ! IDRY.eq.1            
c         if(IWET.eq.1)write(io10) itype,(cid5(n),n=1,nln2)
          if(IWET.eq.1)then
c cid5 - character*16
           ilen=len(cid5(1))
           irec_size=(nln2*ilen)+4
           call MPI_FILE_WRITE_AT(io10,irec_size,4,MPI_BYTE,
     1                                istat,ierr)
           mpifilebytes(io10)=mpifilebytes(io10)+4
           call MPI_FILE_WRITE_AT(io10,itype,4,MPI_BYTE,istat,ierr)
           mpifilebytes(io10)=mpifilebytes(io10)+4
           do n=1,nln2
            call MPI_FILE_WRITE_AT(io10,cid5(n),ilen,MPI_BYTE,
     1                                 istat,ierr)
            mpifilebytes(io10)=mpifilebytes(io10)+4
           enddo
           call MPI_FILE_WRITE_AT(io10,irec_size,4,MPI_BYTE,
     1                                istat,ierr)
           mpifilebytes(io10)=mpifilebytes(io10)+4
          endif ! IWET.eq.1            
         endif
      endif
c
      if(mpirank.eq.0) then
        if(nvl1.GT.0) then
         itype=7
c         if(ICON.eq.1)write(io8) itype,(cnamvl1(n),n=1,nvl1)
         if(ICON.eq.1)then
c cnamvl1 - character*16
          ilen=len(cnamvl1(1))
          irec_size=(nvl1*ilen)+4
          call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),irec_size,4,
     1                          MPI_BYTE,istat,ierr)
          mpifilebytes(io8)=mpifilebytes(io8)+4
          call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),itype,4,
     1                           MPI_BYTE,istat,ierr)
          mpifilebytes(io8)=mpifilebytes(io8)+4
          do n=1,nvl1
           call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),cnamvl1(n),ilen,
     1                           MPI_BYTE,istat,ierr)
           mpifilebytes(io8)=mpifilebytes(io8)+ilen
          enddo
          call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),irec_size,4,
     1                          MPI_BYTE,istat,ierr)
          mpifilebytes(io8)=mpifilebytes(io8)+4
         endif ! ICON.eq.1
c         if(IDRY.eq.1)write(io9) itype,(cnamvl1(n),n=1,nvl1)
         if(IDRY.eq.1)then
c cnamvl1 - character*16
          ilen=len(cnamvl1(1))
          irec_size=(nvl1*ilen)+4
          call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),irec_size,4,
     1                          MPI_BYTE,istat,ierr)
          mpifilebytes(io9)=mpifilebytes(io9)+4
          call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),itype,4,
     1                           MPI_BYTE,istat,ierr)
          mpifilebytes(io9)=mpifilebytes(io9)+4
          do n=1,nvl1
           call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),cnamvl1(n),ilen,
     1                           MPI_BYTE,istat,ierr)
           mpifilebytes(io9)=mpifilebytes(io9)+ilen
          enddo
          call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),irec_size,4,
     1                          MPI_BYTE,istat,ierr)
          mpifilebytes(io9)=mpifilebytes(io9)+4
         endif ! IDRY.eq.1
c         if(IWET.eq.1)write(io10) itype,(cnamvl1(n),n=1,nvl1)
         if(IWET.eq.1)then
c cnamvl1 - character*16
          ilen=len(cnamvl1(1))
          irec_size=(nvl1*ilen)+4
          call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),irec_size,4,
     1                          MPI_BYTE,istat,ierr)
          mpifilebytes(io10)=mpifilebytes(io10)+4
          call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),itype,4,
     1                           MPI_BYTE,istat,ierr)
          mpifilebytes(io10)=mpifilebytes(io10)+4
          do n=1,nvl1
           call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),cnamvl1(n),
     1                          ilen,MPI_BYTE,istat,ierr)
           mpifilebytes(io10)=mpifilebytes(io10)+ilen
          enddo
          call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),irec_size,4,
     1                          MPI_BYTE,istat,ierr)
          mpifilebytes(io10)=mpifilebytes(io10)+4
         endif ! IWET.eq.1
        endif ! nv11.GT.0
      endif ! mpirank.eq.0
c
      if(mpirank.eq.0) then
        if(nvl2.GT.0) then
         itype=8
c         if(ICON.eq.1)write(io8) itype,(cid4(n),n=1,nvl2)
         if(ICON.eq.1)then
c cid4 - character*16
         ilen=len(cid4(1))
         irec_size=(nv12*ilen)+4
          call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),irec_size,4,
     1                          MPI_BYTE,istat,ierr)
          mpifilebytes(io8)=mpifilebytes(io8)+4
          call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),itype,4,
     1                          MPI_BYTE,istat,ierr)
          mpifilebytes(io8)=mpifilebytes(io8)+4
          do n=1,nv12
           call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),cid4(n),ilen,
     1                           MPI_BYTE,istat,ierr)
           mpifilebytes(io8)=mpifilebytes(io8)+ilen
          enddo
          call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),irec_size,4,
     1                          MPI_BYTE,istat,ierr)
          mpifilebytes(io8)=mpifilebytes(io8)+4
         endif ! ICON.eq.1
c         if(IDRY.eq.1)write(io9) itype,(cid4(n),n=1,nvl2)
         if(IDRY.eq.1)then
c cid4 - character*16
         ilen=len(cid4(1))
         irec_size=(nv12*ilen)+4
          call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),irec_size,4,
     1                          MPI_BYTE,istat,ierr)
          mpifilebytes(io9)=mpifilebytes(io9)+4
          call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),itype,4,
     1                          MPI_BYTE,istat,ierr)
          mpifilebytes(io9)=mpifilebytes(io9)+4
          do n=1,nv12
           call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),cid4(n),ilen,
     1                           MPI_BYTE,istat,ierr)
           mpifilebytes(io9)=mpifilebytes(io9)+ilen
          enddo
          call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),irec_size,4,
     1                          MPI_BYTE,istat,ierr)
          mpifilebytes(io9)=mpifilebytes(io9)+4
         endif ! IDRY.eq.1
c         if(IWET.eq.1)write(io10) itype,(cid4(n),n=1,nvl2)
         if(IWET.eq.1)then
c cid4 - character*16
         ilen=len(cid4(1))
         irec_size=(nv12*ilen)+4
          call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),irec_size,4,
     1                          MPI_BYTE,istat,ierr)
          mpifilebytes(io10)=mpifilebytes(io10)+4
          call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),itype,4,
     1                          MPI_BYTE,istat,ierr)
          mpifilebytes(io10)=mpifilebytes(io10)+4
          do n=1,nv12
           call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),cid4(n),ilen,
     1                           MPI_BYTE,istat,ierr)
           mpifilebytes(io10)=mpifilebytes(io10)+ilen
          enddo
          call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),irec_size,4,
     1                          MPI_BYTE,istat,ierr)
          mpifilebytes(io10)=mpifilebytes(io10)+4
         endif ! IWET.eq.1
        endif ! nv12.GT.0
      endif ! mpirank.eq.0
c
c
c --- HEADER RECORDS
c ---     #NCOM+8 -- Nearest Surface Station for VISIBILITY ONLY
c ---     #NCOM+9 -- X coord (UTM) of stations for VISIBILITY ONLY
c ---    #NCOM+10 -- Y coord (UTM) of stations for VISIBILITY ONLY
c
c frr (4/02) if 2D RH file, no need for nearest station info (nssta can
c            be =0)
c
c            
c ---
c      if(IVIS.eq.1)then
      if((IVIS.eq.1 .OR. IT2D.eq.1 .OR. IRHO.eq.1)
     &                            .and. i2dmet.eq.0 )then
c
c ---    Check if active portion of the MET grid is less than the full
c ---    dimension (active portion is extracted for output purposes)
         ifullm=0
         if(nx.eq.mxnx.and.ny.eq.mxny)then
            ifullm=1
         endif
c ---    Nearest Station Array (rec #6)
         cname=' NEARS         '
         if(ifullm.eq.1)then
            if(mpirank.eq.0) then
            if(IVIS.eq.1)call wrint(io11,cname,nears,nx,ny)
            if(IT2D.eq.1)call wrint(io13,cname,nears,nx,ny)
            if(IRHO.eq.1)call wrint(io14,cname,nears,nx,ny)
            endif
         else
         if(mpirank.eq.0) then
            call xtracti(nears,mxnx,mxny,nx,ny,itmp)
            if(IVIS.eq.1)call wrint(io11,cname,itmp,nx,ny)
            if(IT2D.eq.1)call wrint(io13,cname,itmp,nx,ny)
            if(IRHO.eq.1)call wrint(io14,cname,itmp,nx,ny)
            endif
         endif
c ---    Convert station coordinates to UTM from meters relative to
c ---    met grid origin
         do is=1,nssta
            xkmsta(is)=xorigkm+0.001*xssta(is)
            ykmsta(is)=yorigkm+0.001*yssta(is)
         enddo
c ---    X-coord (km) of each surface station (rec #7)
         cname=' XSSTA-UTM     '
         if(mpirank.eq.0) then
         if(IVIS.eq.1)call wrdat(io11,cname,xkmsta,nssta,1)
         if(IT2D.eq.1)call wrdat(io13,cname,xkmsta,nssta,1)
         if(IRHO.eq.1)call wrdat(io14,cname,xkmsta,nssta,1)
         endif
c ---    Y-coord (km) of each surface station (rec #8)
         cname=' YSSTA-UTM     '
         if(mpirank.eq.0) then
         if(IVIS.eq.1)call wrdat(io11,cname,ykmsta,nssta,1)
         if(IT2D.eq.1)call wrdat(io13,cname,ykmsta,nssta,1)
         if(IRHO.eq.1)call wrdat(io14,cname,ykmsta,nssta,1)
         endif
      endif
c
100   continue

c --- Section for FOG.DAT
      if(IFOG.GT.0) then
         if(LPMODE) then
c ---       Plume mode: write second header record with
c ---       downwind distances in meters from source
            if(mpirank.eq.0) then
            write(io12) ftitle,nfrec,nfpts,ipcp,ifyr,ifdays,lpmode
            write(io12) lcomprs,xrfog
            write(io12) ftitle,nfrec,nfpts,ipcp,ifyr,ifdays,lpmode
            write(io12) lcomprs
            endif
         endif
      endif

      return
      end
