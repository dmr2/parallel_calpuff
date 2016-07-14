c----------------------------------------------------------------------
      subroutine openot(nln2,idryflg,mhill,mfpt2,mfvl2)
c----------------------------------------------------------------------
c
c --- CALPUFF    Version: 5.8      Level: 050128                 OPENOT
c     Original code written by J. Scire, D. Strimaitis, SRC
c
c     Modified by D.J. Rasmussen
c                 Department of Civil and Environmental Engineering
c                 University of California, Davis
c
c --- PURPOSE:  Open all input/output files other than input control
c ---           file (IO5) and output list file (IO6)
c
c --- UPDATE
c --- PARALLEL(V5.8)120723  (DMR): Modified for PARALLEL CALPUFF
c --- V5.71-V5.725  050128  (DGS): add TK2D.DAT for 2D temperature
c                   050128  (DGS): add RHO2D.DAT for 2D density
c --- V5.71-V5.74   040715  (DGS): add METFM=5 (AERMET)
c --- V5.7-V5.71    030528  (DGS): MBCON=2 opens a binary CONC.DAT file
c --- V5.4-V5.7     030402  (DGS): replace OPENAB with explicit ASCII
c                                  file assumption
c --- V5.4-V5.4     000602_3(DGS): add H2O2.DAT for aqueous chemistry
c --- V5.3-V5.4     000602  (DGS): NVOLDAT replaces IGRDVL, and
c                                  open multiple VOLEMARB.DAT files
c                   000602  (DGS): open multiple PTEMARB.DAT and
c                                  BAEMARB.DAT files
c                   000602  (DGS): include FOG.DAT file
c --- V5.2-V5.3     991222  (DGS): add BCON.DAT
c --- V5.0-V5.2     991104  (DGS): YYYY for year in TRACK header
c --- V5.0-V5.0     990228d (DGS): add MASSBAL.DAT
c --- V5.0-V5.0     990228c (DGS): add FLUXBDY.DAT and MASSFLX.DAT
c --- V5.0-V5.0     990228a (DGS): allow array of CALMET.DAT filenames
c --- V5.0-V5.0     980918  (DGS): include MCHEM=4
c --- V5.0-V5.0     980807  (DGS): allow either unformatted of formatted
c                                  Variable emissions file for VOLUMES
c --- V5.0-V5.0     980731  (DGS): augment output to DEBUG.DAT
c --- V5.0-V5.0     980515  (DGS): add COASTLN.DAT file
c --- V5.0-V5.0     980304  (DGS): add RESTART file
c --- V4.0-V5.0     971107  (DGS): add LNEMARB.DAT file
c --- V4.0-V4.1     971107  (DGS): allow either unformatted of formatted
c                                  Variable emissions file for POINTS
c                   971107  (DGS): open PROFILE.DAT when MTINV=1 for
c                                  getting inversion strength
c
c --- INPUTS:
c               NLN2 - integer - Number of buoyant line sources with
c                                variable location and emissions
c                                (LNEMARB.DAT - arbitrary emissions)
c     IDRYFLG(mxspec) - integer - Array of dry deposition flags for
c                       array     each pollutant
c                                   0 = No deposition
c                                   1 = Resistance model - gas
c                                   2 = Resistance model - particle
c                                   3 = User-specified dep. velocities
c               MHILL - integer - Flag controlling use of CTDM-format
c                                 hill & receptor information (for CTSG)
c                                   0 = No file (CTSG option not used)
c                                   1 = HILL.DAT and HILLRCT.DAT files
c                                   2 = No file (hill data from OPTHILL
c                                       are supplied in Subgroup 6b, and
c                                       CTSG receptors are supplied in
c                                       Subgroup 6c)
c    Use /MPIRANKTASKS/ variables:
c       MPINUMTASKS, MPIRANK
c    Use /MPIFILESIZE/ variables:
c       MPIFILEBYTES, IO8, IO9, IO10
c
c     Common block /CHEMDAT/ variables:
c           MOZ
c     Common block /FLAGS/ variables:
c           MCHEM, MDRY, MDISP, MCTSG, MTURBVW, MTINV, MSGTIBL, MBCON,
c           MAQCHEM
c     Common block /GEN/ variables:
c           METFM, MRESTART
c     Common block /OUTPT/ variables:
c           ICON, IVIS, IT2D, IRHO, IDRY, IWET, IMFLX, IMBAL, IFOG,
c           LDEBUG
c     Common block /FILNAM/ variables:
c           METDATL, ISCDAT, PLMDAT, CONDAT, DFDAT, WFDAT, VISDAT,
c           T2DDAT, RHODAT, PTDAT(mxemdat), ARDAT(mxemdat),
c           VOLDAT(mxemdat), LNDAT, VDDAT, OZDAT, CHEMDAT, HILDAT,
c           RCTDAT, PRFDAT, SFCDAT, RSTARTB, CSTDAT, BDTDAT, FLXDAT,
c           BALDAT, DEBUG, NPTDAT, NARDAT, NVOLDAT, FOGDAT, H2O2DAT
c     Parameters: IO3, IO6, IO7, IO11, IO12, IO13, IO14,
c                 IO15, IO19 IO20, IO22, IO23, IO24, IO25, IO28, IO29,
c                 IO30, IO31, IO32, IO35, IO36, IO37, MXSPEC,
c                 IOPT2, IOAR2, IOVOL, MXEMDAT, MXMETDAT
c
c --- OUTPUT:
c  MFPT2(mxemdat) - integer    - Formatted PTEMARB.DAT file?
c                                 (0:NO, 1:YES)
c  MFVL2(mxemdat) - integer    - Formatted VOLEMARB.DAT file?
c                                 (0:NO, 1:YES)
c
c --- OPENOT called by:  SETUP
c --- OPENOT calls:      (OPENAB)
c----------------------------------------------------------------------

c dmr 
      use mpif
      use mpiranktasks
      use mpifilesize
c dmr
c --- Include parameter statements
      include 'params.puf'

c --- Include common blocks
      include 'chemdat.puf'
      include 'filnam.puf'
      include 'flags.puf'
      include 'gen.puf'
      include 'outpt.puf'
c
      integer idryflg(mxspec),mfpt2(mxemdat),mfvl2(mxemdat)
c dmr
      character*360 cmd
      logical filefound
c dmr
      character*7 cstat
      logical lprofile
c
      data cstat/'unknown'/, lprofile/.FALSE./
c --- Open Restart file (may or may not exist)
      if(mrestart.EQ.1 .OR. mrestart.EQ.3) 
     &   open(io3,file=rstartb,form='unformatted',status='old')
c
c --- Open Primary met file
      if(metfm.EQ.1) then
c ---    open CALMET meteorological data file (CALMET.DAT)
         open(io7,file=metdatl(1),status='old',form='unformatted')
      elseif(metfm.EQ.2) then
c ---    open ISC meteorological data file (ISCMET.DAT)
         open(io7,file=iscdat,status='old')
      elseif(metfm.EQ.3) then
c ---    open PLM meteorological data file (PLMMET.DAT)
         open(io7,file=plmdat,status='old')
      elseif(metfm.EQ.4 .OR. metfm.EQ.5) then
c ---    open PROFILE meteorological data file (PROFILE.DAT)
         open(io31,file=prfdat,status='old')
         lprofile=.TRUE.
c ---    open SURFACE meteorological data file (SURFACE.DAT)
         open(io32,file=sfcdat,status='old')
c ---    Discard header record of SURFACE.DAT for METFM=5
         if(metfm.EQ.5) read(io32,*)
      endif
c
c --- Open PROFILE.DAT if inversion strength data are needed
      if(.not.lprofile .AND.
     &   mtinv.EQ.1) open(io31,file=prfdat,status='old')
c
c -- dmr ********** modified for Parallel I/O ************
c
c --- open output concentration file (CONC.DAT)
        call MPI_BARRIER(MPI_COMM_WORLD,ierr)
c --- Check to see if concentration output file exists
      if(mpirank.eq.0)then
        inquire (file=condat,exist=filefound)
        if(filefound)then
c --- Delete concentration output file if it exists
          write(cmd,'("/bin/rm ", A)' ) trim(condat)
          call system(cmd)
        endif
      endif
c
      io8 = 0
      if(ICON.eq.1) call MPI_FILE_OPEN(MPI_COMM_WORLD, condat, 
     1    MPI_MODE_CREATE+MPI_MODE_WRONLY, MPI_INFO_NULL, io8,
     2    ierr)
      if(io8.gt.size(mpifilebytes))then
       print*,'increase the size of mpifilebytes > ',io8
       print*,'in module mpifilesize_mod.f90'
       stop 'marker1 in mpiopenot'
      endif
c dmr
c      if(ICON.eq.1)open(io8,file=condat,status=cstat,
c     1  form='unformatted')
c dmr
c --- open dry flux output file (DFLX.DAT)
        call MPI_BARRIER(MPI_COMM_WORLD,ierr)
c --- Check to see if dry flix output file exists
      if(mpirank.eq.0)then
        inquire (file=dfdat,exist=filefound)
        if(filefound)then
c --- Delete output file if it exists 
          write(cmd,'("/bin/rm ", A)' ) trim(dfdat)
          call system(cmd)
        endif
      endif
c
      io9 = 0
      if(IDRY.eq.1) call MPI_FILE_OPEN(MPI_COMM_WORLD, dfdat, 
     1    MPI_MODE_CREATE+MPI_MODE_WRONLY, MPI_INFO_NULL, io9,
     2    ierr)
      if(io9.gt.size(mpifilebytes))then
       print*,'increase the size of mpifilebytes > ',io9
       print*,'in module mpifilesize_mod.f90'
       stop 'marker1 in mpiopenot'
      endif
c dmr
c --- open output dry flux file (DFLX.DAT)
c      if(IDRY.eq.1)open(io9,file=dfdat,status=cstat,
c     1  form='unformatted')
c dmr
c --- open output wet flux file (WFLX.DAT)
        call MPI_BARRIER(MPI_COMM_WORLD,ierr)
c --- Check to see if wet flix output file exists
      if(mpirank.eq.0)then
        inquire (file=wfdat,exist=filefound)
        if(filefound)then
c --- Delete output file if it exists 
          write(cmd,'("/bin/rm ", A)' ) trim(wfdat)
          call system(cmd)
        endif
      endif
c
      io10 = 0
      if(IWET.eq.1) call MPI_FILE_OPEN(MPI_COMM_WORLD, wfdat, 
     1    MPI_MODE_CREATE+MPI_MODE_WRONLY, MPI_INFO_NULL, io10,
     2    ierr)
      if(io10.gt.size(mpifilebytes))then
       print*,'increase the size of mpifilebytes > ',io10
       print*,'in module mpifilesize_mod.f90'
       stop 'marker1 in mpiopenot'
      endif
c dmr
c --- open output wet flux file (WFLX.DAT)
c      if(IWET.eq.1)open(io10,file=wfdat,status=cstat,
c     1  form='unformatted')
c
c --- open visibility-related file (VISB.DAT)
      if(mpirank.eq.0) then
        if(IVIS.eq.1)open(io11,file=visdat,status=cstat,
     1    form='unformatted')
      endif
c --- open 2D temperature file (TK2D.DAT)
      if(mpirank.eq.0) then
        if(IT2D.eq.1)open(io13,file=t2ddat,status=cstat,
     1    form='unformatted')
      endif
c --- open 2D density file (RHO2D.DAT)
      if(mpirank.eq.0) then
        if(IRHO.eq.1)open(io14,file=rhodat,status=cstat,
     1    form='unformatted')
      endif
c --- open BOUNDARY CONCENTRATION file (BCON.DAT)
      if(mbcon.eq.1) open(io15,file=bcndat,status='old')
      if(mbcon.eq.2) open(io15,file=bcndat,status='old',
     &  form='unformatted')
c
c --- open output fog-model file (FOG.DAT)
      if(mpirank.eq.0) then
        if(IFOG.eq.1)open(io12,file=fogdat,status=cstat,
     1    form='unformatted')
      endif
c --- open POINT SOURCE emissions file(s) (PTEMARB.DAT)
c --- (stationary point sources with arbitrary variation in emissions)
      do i=1,nptdat
         io=iopt2+i-1
c ---    Assume ASCII file (formatted)
c        call OPENAB(io,ptdat(i),mfpt2(i))
         mfpt2(i)=1
         open(io,file=ptdat(i),status='old')
      enddo
c
c --- open BUOYANT AREA SOURCE file(s)  (BAEMARB.DAT)
c --- (area sources with arbitrary variation in location & emissions)
      do i=1,nardat
         io=ioar2+i-1
         open(io,file=ardat(i),status='old')
      enddo
c
c --- open buoyant LINE SOURCE file  (LNEMARB.DAT)
c --- (line sources with arbitrary variation in location & emissions)
      if(nln2.gt.0)then
         open(io19,file=lndat,status='old')
      endif
c
c --- open VOLUME SOURCE file(s) (VOLEMARB.DAT)
c --- (volume sources with arbitrary variation in location & emissions)
      do i=1,nvoldat
         io=iovol+i-1
c ---    Assume ASCII file (formatted)
c        call OPENAB(io,voldat(i),mfvl2(i))
         mfvl2(i)=1
         open(io,file=voldat(i),status='old')
      enddo
c
c --- open DEPOSITION VELOCITY file (VD.DAT) (if user-specified
c     deposition velocities are used for any species AND computation
c     of dry deposition is requested
      if(mdry.eq.1)then
         do 10 i=1,mxspec
         if(idryflg(i).eq.3)then
            open(io20,file=vddat,status='old')
            go to 12
         endif
10       continue
12       continue
      endif
c
c --- open OZONE data file (OZONE.DAT) (if chemical transformation
c     is computed AND hourly ozone input is requested)
      if((mchem.EQ.1 .OR. mchem.EQ.3 .OR. mchem.EQ.4) .AND .moz.EQ.1)
     &   open(io22,file=ozdat,status='old')
c
c --- open H2O2 data file (H2O2.DAT) (if aqueous phase chemical
c     transformation is computed AND hourly H2O2 input is requested)
      if(maqchem.EQ.1 .AND .mh2o2.EQ.1)
     &   open(io23,file=h2o2dat,status='old')
c
c --- open CHEMICAL TRANSFORMATION file (CHEM.DAT) (if user-specified
c     chemical transformation rates are used)
      if(mchem.eq.2)open(io24,file=chemdat,status='old')
c
c --- open TURBULENCE data file (PROFILE.DAT) if needed
      if(.not.lprofile) then
c ---    Need turbulence only for MDISP=1,5
         if(mdisp.EQ.1 .OR. mdisp.EQ.5) then
c ---       Open file only when explicitly requested
            if(mturbvw.LT.4) open(io31,file=prfdat,status='old')
         endif
      endif
c
c --- open CTSG hill information files (HILL.DAT, HILLRCT.DAT)
c     (if CTDM processors are used to create them)
      if(mctsg.eq.1.and.mhill.eq.1) then
         open(io28,file=hildat,status='old')
         open(io29,file=rctdat,status='old')
      endif
c
c --- open Coast Line(s) file (COASTLN.DAT)
c     (if sub-grid TIBL module is used)
      if(msgtibl.eq.1) then
         open(io25,file=cstdat,status='old')
      endif
c
c --- open Mass Flux files (FLUXBDY.DAT, MASSFLX.DAT)
c     (if mass flux output option is selected)
      if(imflx.eq.1) then
         open(io35,file=bdydat,status='old')
         if(mpirank.eq.0)then
           open(io36,file=flxdat,status=cstat)
         endif
      endif
c
c --- open Mass Balance file (MASSBAL.DAT)
c     (if mass balance output option is selected)
      if(imbal.eq.1) then
        if(mpirank.eq.0) then
         open(io37,file=baldat,status=cstat)
        endif
      endif
c
c --- open Puff/Slug TRACKING file (DEBUG.LST) (if in DEBUG mode)
      if(LDEBUG) then
        if(mpirank.eq.0) then
          open(io30,file=debug,status=cstat)
c ---    Write column headers for Puffs/Slugs
          write(io30,301)
          write(io30,302)
        endif
      endif
301   format('                              ---- PUFF/ Old SLUG end --',
     &       '-------')
302   format('YYYYJJJHH   ipnum cd   zfnl   x(metG)   y(metG)',
     &       '    sigyB    sigzB          QM          QU   zimax',
     &       '  rflctn    dpbl jdstab   Length')
c
      return
      end
