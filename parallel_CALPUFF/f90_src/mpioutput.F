c----------------------------------------------------------------------
      subroutine output(istep,isrcmode,ktype,ksource)
c----------------------------------------------------------------------
c
c --- CALPUFF    Version: 5.8      Level: 050128                 OUTPUT
c     Original Code written by J. Scire, Earth Tech
c
c     Modified by D.J. Rasmussen
c                 Department of Civil and Environmental Engineering
c                 University of California, Davis
c
c --- PURPOSE:  Output concentration, dry & wet deposition flux
c               fields
c
c               Print concentrations every "ICFRQ" hours (if ICPRT=1)
c               Print dry fluxes every "IDFRQ" hours (if IDPRT=1)
c               Print wet fluxes every "IWFRQ" hours (if IWPRT=1)
c
c               Write specified concentrations and fluxes to disk
c               hourly -- concentration file (IO8), dry flux file (IO9),
c               wet flux file (IO10)
c
c --- UPDATE
c --- PARALLEL(V5.8)120723  (DMR): Modified for PARALLEL CALPUFF
c --- V5.72-V5.725  050128  (DGS): Add TK2D.DAT & RHO2D.DAT output files
c --- V5.7-V5.72    031017  (DGS): Add ISRCMODE switch to distinguish
c                                  between source contribution output
c                                  and total output;
c                                  Augment time record with both begin
c                                  and end times;
c                                  Add new record with source name and
c                                  location
c --- V5.2-V5.7     030402  (FRR): Add 2D met arrays (i2dmet)
c --- V5.0-V5.2     991104  (DGS): YYYY format for year
c --- V5.0-V5.0     990228d (DGS): add mass balance output
c --- V5.0-V5.0     990228c (DGS): add mass flux output
c --- V5.0-V5.0     981025  (DGS): full list-file output for fluxes
c --- V4.0-V5.0     971107  (DGS): add user-units in LIST output
c                   971107  (DGS): change from species list to
c                                  species-groups for output
c
c --- INPUTS:
c              ISTEP   - integer    - Time step number
c           ISRCMODE   - integer    - Source contribution mode
c                                     0: output for all sources
c                                     1: source contribution output
c              KTYPE   - integer    - Source type
c            KSOURCE   - integer    - Source number

c     Use /MPIRANKTASKS/ variables:
c          MPINUMTASKS, MPIRANK
c     Use /MPIDECOMP/ variables:
c          MYSAMB, MYSAMT, MPIFIRSTREC, MPILASTREC,
c          MPIFIRSTCTREC, MPILASTCTREC
c     Use /MPIFILESIZE/ variables:
c          MPIFILEBYTES, IO8, IO9, IO10

c     Common block /AR1/ variables:
c          NAR1,CNAMAR1(mxarea),NVERT1(mxarea),
c          XAR1GRD(mxvertp1,mxarea),YAR1GRD(mxvertp1,mxarea)
c     Common block /AR2/ variables:
c          NAR2,CID3(mxarea),NVERT2(mxarea),
c          XAR2GRD(mxvertp1,mxarea),YAR2GRD(mxvertp1,mxarea)
c     Common block /CHIFLX/ variables
c          CHISAM(mxnxg,mxnyg,mxspec), DFSAM(mxnxg,mxnyg,mxspec),
c          WFSAM(mxnxg,mxnyg,mxspec), CHIREC(mxrec,mxspec),
c          DFREC(mxrec,mxspec), WFREC(mxrec,mxspec),
c          CHICT(mxrect,mxspec)
c     Common block /CTSGDAT/ variables
c          NCTREC, XRCT(mxrect), YRCT(mxrect), ELRECT(mxrect),
c          IHILL(mxrect)
c     Common block /DATEHR/ variables
c          NYR, NJUL, NHR
c     Common block /GEN/ variables
c          NSPEC, CSPEC(mxgrup),
c          NGRUP, CGRUP(mxgrup), ISPGRP(mxspec)
c     Common block /GRID/ variables
c          DGRID, XORIG, YORIG,
c          NXSAM, NYSAM, LSAMP
c     Common block /LN1/ variables:
c          NLINES,CNAMLN1(mxlines),XL1BEGGRD(mxlines),YL1BEGGRD(mxlines)
c          XL1ENDGRD(mxlines),YL1ENDGRD(mxlines)
c     Common block /LN2/ variables:
c          NLN2,CID5(mxlines),XL2BEGGRD(mxlines),YL2BEGGRD(mxlines),
c          XL2ENDGRD(mxlines),YL2ENDGRD(mxlines)
c     Common block /METHD/ variables
c          NSSTA, I2DMET , NXM, NYM
c     Common block /METHR/ variables
c          IRHSS(mxss), IRH2D(mxnx,mxny),
c          TEMPSS(mxss), TEMP2D(mxnx,mxny)
c     Common block /NONGRD/ variables
c          NREC
c     Common block /OUTPT/ variables
c          ICON, IDRY, IWET, ICPRT, IDPRT, IWPRT, ICFRQ, IDFRQ, IWFRQ,
c          IVIS, IT2D, IRHO,
c          IOUTOP(7,mxspec), LCOMPRS, IPRTU, IMFLX, IMBAL
c     Common block /PT1/ variables:
c          NPT1,CNAMPT1(mxpt1),,XPT1GRD(mxpt1),YPT1GRD(pt1)
c     Common block /PT2/ variables:
c          NPT2,CID2(mxpt2),TIEM2(8,mxpt2)
c     Common block /VOL1/ variables:
c          NVL1,CNAMVL1(mxvol),XVL1GRD(mxvol),YVL1GRD(mxvol)
c     Common block /VOL2/ variables:
c          NVL2,CID4(mxvol),XVL2GRD(mxvol),YVL2GRD(mxvol)
c     Common block /WRKSPC/ variables
c          TMP1(mxnx,mxny),
c          TMP3(mxrec),TMP5(mxrect),TMP7(mxnxg,mxnyg),TMP8(mxnxg,mxnyg)
c
c     Parameters: MXNXG, MXNYG, MXSPEC, MXREC, MXRECT, MXNZP1, MXSS,
c                 MXPT1,MXPT2,MXAREA,MXLINES,MXVOL,  MXNX,MXNY
c                 IO6, IO11, IO13, IO14
c
c --- OUTPUT:  none
c
c --- OUTPUT called by: COMP
c --- OUTPUT calls:     OUTSAM, WRDATC, WRDATUCR, WRDAT, COMPRS, MFLXGRP,
c                       MFLXGRP, MFLXOUT, MBALOUT
c----------------------------------------------------------------------
c
c dmr
      use mpif
      use mpiranktasks 
      use mpifilesize
      use mpidecomp
c dmr

c --- Include parameter statements
      include 'params.puf'

      real xtmp(mxspec),rprtu(5)
      integer istore(mxgrup)
      character*70 messag
      character*16 csrcnam
      character*15 cname
      character*15 cnameRh, cnameTK, cnameDN
      character*13 cunits,funits
      character*12 cspecsv(mxspec)
      character*1  cprtu(4)
      logical ldate
c
c frr(09/01)
      integer itmp(mxnx,mxny)

c dmr
      integer istat(MPI_STATUS_SIZE), irec_size, ilen
      integer mpi_nwords, mpi_tmp8, mpinysam, mpinrec, mpinctrec
      integer allendpts(mpinumtasks*2), alloffset(mpinumtasks),asize
      integer recvcnts(mpinumtasks), cnt, myendpts(2)
      real tmpchirec(mxspec,mxrec), tmpchict(mxspec,mxrect)
      real tmpdfrec(mxspec, mxrec), tmpwfrec(mxspec,mxrec)
      real mpichirec(mxspec,mxrec), mpiwfrec(mxspec,mxrec)
      real mpidfrec(mxspec,mxrec), mpichict(mxspec,mxrec)
c dmr
      include 'chiflx.puf'
      include 'ctsgdat.puf'
      include 'datehr.puf'
      include 'methd.puf'
      include 'methr.puf'
      include 'gen.puf'
      include 'grid.puf'
      include 'nongrd.puf'
      include 'outpt.puf'
      include 'wrkspc.puf'

c --- Include source commons for source names
      include 'ar1.puf'
      include 'ar2.puf'
      include 'ln1.puf'
      include 'ln2.puf'
      include 'pt1.puf'
      include 'pt2.puf'
      include 'vol1.puf'
      include 'vol2.puf'
c
      data ldate/.true./
c
c --- Set character for scale of output units (m:milli,u:micro,n:nano)
      data cprtu/' ','m','u','n'/
c --- Set units conversion factor
      data rprtu/1.0,1.0e3,1.0e6,1.0e9,1.0/
c --- Construct units name for concentrations (list file)
      cunits='(Odour Units)'
      if(iprtu.LT.5) then
         cunits='( g/m**3)    '
         cunits(2:2)=cprtu(iprtu)
      endif

c --- Set grid coordinates in km
      xorigkm=0.001*xorig
      yorigkm=0.001*yorig
      dgridkm=0.001*dgrid

c --- Identify source
      csrcnam='                '
      if(isrcmode.EQ.1) then
c ---    Output for a specific source
         if(ktype.EQ.1) then
            csrcnam=cnampt1(ksource)
            xmapkm=xpt1grd(ksource)*dgridkm+xorigkm
            ymapkm=ypt1grd(ksource)*dgridkm+yorigkm
         elseif(ktype.EQ.2) then
            csrcnam=cid2(ksource)
            xmapkm=tiem2(1,ksource)*dgridkm+xorigkm
            ymapkm=tiem2(2,ksource)*dgridkm+yorigkm
         elseif(ktype.EQ.3) then
            csrcnam=cnamar1(ksource)
            xmapkm=xar1grd(1+nvert1(ksource),ksource)*dgridkm+xorigkm
            ymapkm=yar1grd(1+nvert1(ksource),ksource)*dgridkm+yorigkm
         elseif(ktype.EQ.4) then
            csrcnam=cid3(ksource)
            xmapkm=xar2grd(1+nvert2(ksource),ksource)*dgridkm+xorigkm
            ymapkm=yar2grd(1+nvert2(ksource),ksource)*dgridkm+yorigkm
         elseif(ktype.EQ.5) then
            csrcnam=cnamln1(ksource)
            xgrd=0.5*(xlbeggrd(ksource)+xlendgrd(ksource))
            ygrd=0.5*(ylbeggrd(ksource)+ylendgrd(ksource))
            xmapkm=xgrd*dgridkm+xorigkm
            ymapkm=ygrd*dgridkm+yorigkm
         elseif(ktype.EQ.6) then
            csrcnam=cid5(ksource)
            xgrd=0.5*(xl2beggrd(ksource)+xl2endgrd(ksource))
            ygrd=0.5*(yl2beggrd(ksource)+yl2endgrd(ksource))
            xmapkm=xgrd*dgridkm+xorigkm
            ymapkm=ygrd*dgridkm+yorigkm
         elseif(ktype.EQ.7) then
            csrcnam=cnamvl1(ksource)
            xmapkm=xvl1grd(ksource)*dgridkm+xorigkm
            ymapkm=yvl1grd(ksource)*dgridkm+yorigkm
         elseif(ktype.EQ.8) then
            csrcnam=cid4(ksource)
            xmapkm=xvl2grd(ksource)*dgridkm+xorigkm
            ymapkm=yvl2grd(ksource)*dgridkm+yorigkm
         else
            write(io6,*) 'FATAL error in OUTPUT - invalid source'
            write(io6,*) 'Source Type and number = ',ktype,ksource
            stop 'Subr. OUTPUT:  Invalid Source'
         endif
      else
c ---    Output for all sources (total)
         if(ktype.EQ.0 .AND. ksource.EQ.1) then
            csrcnam='TOTAL           '
            xmapkm=xorigkm
            ymapkm=yorigkm
         else
            write(io6,*) 'FATAL error in OUTPUT - invalid source'
            write(io6,*) 'Source Type and number = ',ktype,ksource
            stop 'Subr. OUTPUT:  Invalid Source'
         endif
      endif

c --- Construct units name for fluxes  (list file)
      funits='(-----------)'
      if(iprtu.LT.5) then
         funits='( g/m**2/s)  '
         funits(2:2)=cprtu(iprtu)
      endif
c
c --- Check if active portion of the sampling arrays is less than
c --- the full dimension (if so, active portion is extracted for
c --- output purposes
      ifull=0
      if(lsamp) then
        if(nxsam.eq.mxnxg.and.nysam.eq.mxnyg)then
            ifull=1
         endif
      endif
c --- Set date-time at both the start and end of the averaging period
      nyrb=nyr
      njulb=njul
      nhrb=nhr
      nhrinc=-1
      call INCR(io6,nyrb,njulb,nhrb,nhrinc)
      nsecb=0
      nsece=0

c ---------------------------------------------------------
c --- PROCESS OUTPUT SPECIES-GROUPS
c ---------------------------------------------------------
c --- Conc/flux data for each species that are in a group are summed
c --- prior to output, and given the name of the species-group.  The
c --- summation is done at the array location of the first species
c --- in the group.
c
c --- Definitions ---
c --- ISPGRP(i)       :group index for species i
c --- CGRUP(j)        :output species name for group j
c --- ISTORE(j)       :index of conc/flux arrays used to store
c                      summed results for group j
c ---------------------------------------------------------
c --- Clear istore array
      do is=1,nspec
         istore(is)=0
      enddo
c --- Group species and store
      do is=1,nspec
         if(istore(ispgrp(is)).EQ.0) then
c ---       First species in this group; just store array location
            istore(ispgrp(is))=is
         else
c ---       Add results for this species to the existing sum
c ---       Gridded receptors
            if(LSAMP) then
               do iy=1,nysam
                  do ix=1,nxsam
                     chisam(ix,iy,istore(ispgrp(is)))=
     &               chisam(ix,iy,istore(ispgrp(is)))+chisam(ix,iy,is)
                     dfsam(ix,iy,istore(ispgrp(is)))=
     &               dfsam(ix,iy,istore(ispgrp(is)))+dfsam(ix,iy,is)
                     wfsam(ix,iy,istore(ispgrp(is)))=
     &               wfsam(ix,iy,istore(ispgrp(is)))+wfsam(ix,iy,is)
                  enddo
               enddo
            endif
c ---       Discrete receptors
            do ir=1,nrec
               chirec(ir,istore(ispgrp(is)))=
     &         chirec(ir,istore(ispgrp(is)))+chirec(ir,is)
               dfrec(ir,istore(ispgrp(is)))=
     &         dfrec(ir,istore(ispgrp(is)))+dfrec(ir,is)
               wfrec(ir,istore(ispgrp(is)))=
     &         wfrec(ir,istore(ispgrp(is)))+wfrec(ir,is)
            enddo
c ---       CTSG receptors
            do ir=1,nctrec
               chict(ir,istore(ispgrp(is)))=
     &         chict(ir,istore(ispgrp(is)))+chict(ir,is)
            enddo
c ---       Mass flux arrays
            if(imflx.EQ.1 .AND. isrcmode.NE.1) call MFLXGRP(istore
     &                                             (ispgrp(is)),is)
         endif
      enddo
c ---------------------------------------------------------
c --- WRITE CONCENTRATIONS TO DISK   (g/m**3)
c ---------------------------------------------------------
c
c --- Output date/hour times use 0-23 convention so that hour 24
c --- of day 12 starts at 23 0000 on day 12 and ends at 00 0000
c --- on day 13.
      if(icon.ne.1)go to 492
c
c dmr
c      write(io8)nyrb,njulb,nhrb,nsecb,nyr,njul,nhr,nsece
c      write(io8)ktype,ksource,csrcnam,xmapkm,ymapkm
c dmr
c
c --- Write date/time and source data records
        if (mpirank.eq.0) then
c --- simulate Fortran block
          irec_size = 4*8 ! assume 4 bytes for ints and floats
          call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),irec_size,4,
     1                           MPI_BYTE,istat,ierr)
          mpifilebytes(io8)=mpifilebytes(io8)+4
          call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),nyrb,4,
     1                           MPI_BYTE,istat,ierr)
          mpifilebytes(io8)=mpifilebytes(io8)+4
          Call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),njulb,4,
     1                           MPI_BYTE,istat,ierr)
          mpifilebytes(io8)=mpifilebytes(io8)+4
          call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),nhrb,4,
     1                           MPI_BYTE,istat,ierr)
          mpifilebytes(io8)=mpifilebytes(io8)+4
          call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),nsecb,4,
     1                           MPI_BYTE,istat,ierr)
          mpifilebytes(io8)=mpifilebytes(io8)+4
          call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),nyr,4,
     1                           MPI_BYTE,istat,ierr)
          mpifilebytes(io8)=mpifilebytes(io8)+4
          call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),njul,4,
     1                           MPI_BYTE,istat,ierr)
          mpifilebytes(io8)=mpifilebytes(io8)+4
          call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),nhr,4,
     1                           MPI_BYTE,istat,ierr)
          mpifilebytes(io8)=mpifilebytes(io8)+4
          call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),nsece,4,
     1                           MPI_BYTE,istat,ierr)
          mpifilebytes(io8)=mpifilebytes(io8)+4
          call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),irec_size,4,
     1                           MPI_BYTE,istat,ierr)
          mpifilebytes(io8)=mpifilebytes(io8)+4
c --- csrcnam - character*16
          ilen = len(csrcnam)
          irec_size = (4*4)+ilen ! assume 4 bytes for ints and floats
          call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),irec_size,4,
     1                           MPI_BYTE,istat,ierr)
          mpifilebytes(io8)=mpifilebytes(io8)+4
          call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),ktype,4,
     1                           MPI_BYTE,istat,ierr)
          mpifilebytes(io8)=mpifilebytes(io8)+4
          call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),ksource,4,
     1                           MPI_BYTE,istat,ierr)
          mpifilebytes(io8)=mpifilebytes(io8)+4
          call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),csrcnam,ilen,
     1                           MPI_BYTE,istat,ierr)
          mpifilebytes(io8)=mpifilebytes(io8)+ilen
          call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),xmapkm,4,
     1                           MPI_BYTE,istat,ierr)
          mpifilebytes(io8)=mpifilebytes(io8)+4
          call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),ymapkm,4,
     1                           MPI_BYTE,istat,ierr)
          mpifilebytes(io8)=mpifilebytes(io8)+4
          call MPI_FILE_WRITE_AT(io8,mpifilebytes(io8),irec_size,4,
     1                           MPI_BYTE,istat,ierr)
          mpifilebytes(io8)=mpifilebytes(io8)+4
        endif ! mpirank.eq.0
c dmr
      do 400 ig=1,ngrup
c --- Identify array storage location for this group
      i=istore(ig)
c
c --- Only species-groups specified are stored on disk
      if(ioutop(2,ig).eq.1)then
         cname=cgrup(ig)
         cname(13:15)='  1'
c
c ---    Gridded receptor concentrations
         if(lsamp) then
            if(ifull.eq.1)then
               if(lcomprs)then
c ---             Write compressed data records
c                  call comprs(chisam(1,1,i),mxnxyg,tmp8,mxnxyg,
c     1              cname,io8)
c dmr   
                  mpi_nwords = mxnxg*((mysamt-mysamb)+1)
                  call comprs(chisam(1:mxnxg,mysamb:mysamt,i),mpi_nwords,
     1                        tmp8,mxnxyg,cname,io8)
c dmr 
               else
c ---             Write uncompressed data record
c dmr
c                  call wrdat(io8,cname,chisam(1,1,i),nxsam,nysam)
                  mpinysam = ((mysamt-mysamb)+1)
                  call wrdatu(io8,cname,chisam(1:mxnxg,mysamb:mysamt,i),
     1                       nxsam,mpinysam)
c dmr  
               endif
            else
c --- extract just the assigned decomposition from the entire domain
c               call xtract(chisam(1,1,i),mxnxg,mxnyg,nxsam,nysam,tmp7)
c dmr
               mpinysam = ((mysamt-mysamb)+1)
               call mpixtract(chisam(1,1,i),mxnxg,mxnyg,
     1                     nxsam,mpinysam,tmp7)

c dmr
               if(lcomprs)then
c ---             Write compressed data records
c dmr
c                  nwords=nxsam*nysam
c                  call comprs(tmp7,nwords,tmp8,mxnxyg,cname,io8)
c dmr
                   mpi_nwords = nxsam*((mysamt-mysamb)+1)
                   call comprs(tmp7,mpi_nwords,tmp8,mxnxyg,cname,io8)
c dmr
               else
c ---             Write uncompressed data record
c                  call wrdat(io8,cname,tmp7,nxsam,nysam)
c dmr   
                   call wrdatu(io8,cname,tmp7,nxsam,mpinysam)
c dmr   
               endif
            endif
         endif
c
c ---    Discrete receptor concentrations
         if(nrec .GT. 0) then
            if(lcomprs)then
c ---          Write compressed data records
c               call comprs(chirec(1,i),nrec,tmp3,mxrec,cname,io8)
c dmr
               mpinrec = (mpilastrec-mpifirstrec)+1
               call comprs(chirec(mpifirstrec:mpilastrec,i),mpinrec,
     1                     tmp3,mxrec,cname,io8)
c dmr
            else
c ---          Write uncompressed data record
c               call wrdat(io8,cname,chirec(1,i),nrec,1)
c dmr        
               mpinrec = (mpilastrec-mpifirstrec)+1
               call wrdatur(io8,cname,chirec(mpifirstrec:mpilastrec,i),
     1                    mpinrec,1)
c dmr
            endif 
         endif 
c
c ---    Discrete CTSG receptor concentrations
         if(nctrec .GT. 0) then
            if(lcomprs)then
c ---          Write compressed data records
c               call comprs(chict(1,i),nctrec,tmp5,mxrect,cname,io8)
c dmr
               mpinctrec = (mpilastctrec-mpifirstctrec)+1
               call comprs(chict(mpifirstctrec:mpilastctrec,i),
     1                     mpinctrec,tmp5,mxrect,cname,io8)
c dmr
            else
c ---          Write uncompressed data record
c               call wrdat(io8,cname,chict(1,i),nctrec,1)
c dmr
               mpinctrec = (mpilastctrec-mpifirstctrec)+1
               call wrdatucr(io8,cname,chict(mpifirstctrec:mpilastctrec,
     1                       i),mpinctrec,1)
c dmr
            endif 
         endif 
      endif 
c        
400   continue
492   continue
c ---------------------------------------------------------
c --- WRITE DRY FLUXES TO DISK  (g/m**2/s)
c ---------------------------------------------------------
c
c --- Output date/hour times use 0-23 convention so that hour 24
c --- of day 12 starts at 23 0000 on day 12 and ends at 00 0000
c --- on day 13.
      if(idry.ne.1)go to 592
c
c --- Write date/time and source data records
      if (mpirank.eq.0) then
c      write(io9)nyrb,njulb,nhrb,nsecb,nyr,njul,nhr,nsece
c      write(io9)ktype,ksource,csrcnam,xmapkm,ymapkm
c --- simulate Fortran block
          irec_size = 4*8 ! assume 4 bytes for ints and floats
          call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),irec_size,4,
     1                           MPI_BYTE,istat,ierr)
          mpifilebytes(io9)=mpifilebytes(io9)+4
          call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),nyrb,4,
     1                           MPI_BYTE,istat,ierr)
          mpifilebytes(io9)=mpifilebytes(io9)+4
          Call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),njulb,4,
     1                           MPI_BYTE,istat,ierr)
          mpifilebytes(io9)=mpifilebytes(io9)+4
          call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),nhrb,4,
     1                           MPI_BYTE,istat,ierr)
          mpifilebytes(io9)=mpifilebytes(io9)+4
          call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),nsecb,4,
     1                           MPI_BYTE,istat,ierr)
          mpifilebytes(io9)=mpifilebytes(io9)+4
          call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),nyr,4,
     1                           MPI_BYTE,istat,ierr)
          mpifilebytes(io9)=mpifilebytes(io9)+4
          call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),njul,4,
     1                           MPI_BYTE,istat,ierr)
          mpifilebytes(io9)=mpifilebytes(io9)+4
          call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),nhr,4,
     1                           MPI_BYTE,istat,ierr)
          mpifilebytes(io9)=mpifilebytes(io9)+4
          call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),nsece,4,
     1                           MPI_BYTE,istat,ierr)
          mpifilebytes(io9)=mpifilebytes(io9)+4
          call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),irec_size,4,
     1                           MPI_BYTE,istat,ierr)
          mpifilebytes(io9)=mpifilebytes(io9)+4
c --- csrcnam - character*16
          ilen = len(csrcnam)
          irec_size = (4*4)+ilen ! assume 4 bytes for ints and floats
          call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),irec_size,4,
     1                           MPI_BYTE,istat,ierr)
          mpifilebytes(io9)=mpifilebytes(io9)+4
          call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),ktype,4,
     1                           MPI_BYTE,istat,ierr)
          mpifilebytes(io9)=mpifilebytes(io9)+4
          call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),ksource,4,
     1                           MPI_BYTE,istat,ierr)
          mpifilebytes(io9)=mpifilebytes(io9)+4
          call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),csrcnam,ilen,
     1                           MPI_BYTE,istat,ierr)
          mpifilebytes(io9)=mpifilebytes(io9)+ilen
          call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),xmapkm,4,
     1                           MPI_BYTE,istat,ierr)
          mpifilebytes(io9)=mpifilebytes(io9)+4
          call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),ymapkm,4,
     1                           MPI_BYTE,istat,ierr)
          mpifilebytes(io9)=mpifilebytes(io9)+4
          call MPI_FILE_WRITE_AT(io9,mpifilebytes(io9),irec_size,4,
     1                           MPI_BYTE,istat,ierr)
          mpifilebytes(io9)=mpifilebytes(io9)+4
        endif ! mpirank.eq.0
c
      do 500 ig=1,ngrup
c --- Identify array storage location for this group
      i=istore(ig)
c
c --- Only species specified are stored on disk
      if(ioutop(4,ig).eq.1)then
         cname=cgrup(ig)
         cname(13:15)=' DF'
c
c ---    Gridded receptor dry fluxes
         if(lsamp) then
            if(ifull.eq.1)then
               if(lcomprs)then
c ---             Write compressed data records
c                  call comprs(dfsam(1,1,i),mxnxyg,tmp8,mxnxyg,
c     1              cname,io9)
c dmr
                  mpi_nwords = mxnxg*((mysamt-mysamb)+1)
                  call comprs(dfsam(1:mxnxg,mysamb:mysamt,i),mpi_nwords,
     1                          tmp8,mxnxyg,cname,io9)
c dmr
               else
c ---             Write uncompressed data record
c                  call wrdat(io9,cname,dfsam(1,1,i),nxsam,nysam)
c dmr
                  mpinysam = ((mysamt-mysamb)+1)
                  call wrdatu(io9,cname,dfsam(1:mxnxg,mysamb:mysamt,i),
     1                        nxsam,mpinysam)
c dmr
               endif
            else
c               call xtract(dfsam(1,1,i),mxnxg,mxnyg,nxsam,nysam,tmp7)
c dmr
               mpinysam = ((mysamt-mysamb)+1)
               call mpixtract(dfsam(1,1,i),mxnxg,mxnyg,
     1                        nxsam,mpinysam,tmp7)
c dmr
               if(lcomprs)then
c ---             Write compressed data records
c                  nwords=nxsam*nysam
c                  call comprs(tmp7,nwords,tmp8,mxnxyg,cname,io9)
c dmr
                  mpi_nwords = nxsam*((mysamt-mysamb)+1)
                  call comprs(tmp7,mpi_nwords,tmp8,mxnxyg,cname,io9)
c dmr
               else
c ---             Write uncompressed data record
c                  call wrdat(io9,cname,tmp7,nxsam,nysam)
c dmr
                  call wrdatu(io9,cname,tmp7,nxsam,mpinysam)
c dmr
               endif
            endif
         endif
c
c ---    Discrete receptor dry fluxes
         if(nrec .GT. 0) then
            if(lcomprs)then
c ---          Write compressed data records
c               call comprs(dfrec(1,i),nrec,tmp3,mxrec,cname,io9)
c dmr
               mpinrec = (mpilastrec-mpifirstrec)+1
               call comprs(dfrec(mpifirstrec:mpilastrec,i),mpinrec,tmp3,
     1                     mxrec,cname,io9)
c dmr
            else
c ---          Write uncompressed data record
c               call wrdat(io9,cname,dfrec(1,i),nrec,1)
c dmr
               mpinrec = (mpilastrec-mpifirstrec)+1
               call wrdatur(io9,cname,dfrec(mpifirstrec:mpilastrec,i),
     1                     mpinrec,1)
c dmr
            endif
         endif
      endif
500   continue
592   continue
c ---------------------------------------------------------
c --- WRITE WET FLUXES TO DISK  (g/m**2/s)
c ---------------------------------------------------------
c
c --- Output date/hour times use 0-23 convention so that hour 24
c --- of day 12 starts at 23 0000 on day 12 and ends at 00 0000
c --- on day 13.
      if(iwet.ne.1)go to 692
c
c --- Write date/time and source data records
      if (mpirank.eq.0) then
c      write(io10)nyrb,njulb,nhrb,nsecb,nyr,njul,nhr,nsece
c      write(io10)ktype,ksource,csrcnam,xmapkm,ymapkm
c --- simulate Fortran block
          irec_size = 4*8 ! assume 4 bytes for ints and floats
          call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),irec_size,4,
     1                           MPI_BYTE,istat,ierr)
          mpifilebytes(io10)=mpifilebytes(io10)+4
          call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),nyrb,4,
     1                           MPI_BYTE,istat,ierr)
          mpifilebytes(io10)=mpifilebytes(io10)+4
          Call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),njulb,4,
     1                           MPI_BYTE,istat,ierr)
          mpifilebytes(io10)=mpifilebytes(io10)+4
          call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),nhrb,4,
     1                           MPI_BYTE,istat,ierr)
          mpifilebytes(io10)=mpifilebytes(io10)+4
          call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),nsecb,4,
     1                           MPI_BYTE,istat,ierr)
          mpifilebytes(io10)=mpifilebytes(io10)+4
          call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),nyr,4,
     1                           MPI_BYTE,istat,ierr)
          mpifilebytes(io10)=mpifilebytes(io10)+4
          call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),njul,4,
     1                           MPI_BYTE,istat,ierr)
          mpifilebytes(io10)=mpifilebytes(io10)+4
          call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),nhr,4,
     1                           MPI_BYTE,istat,ierr)
          mpifilebytes(io10)=mpifilebytes(io10)+4
          call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),nsece,4,
     1                           MPI_BYTE,istat,ierr)
          mpifilebytes(io10)=mpifilebytes(io10)+4
          call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),irec_size,4,
     1                           MPI_BYTE,istat,ierr)
          mpifilebytes(io10)=mpifilebytes(io10)+4
c --- csrcnam - character*16
          ilen = len(csrcnam)
          irec_size = (4*4)+ilen ! assume 4 bytes for ints and floats
          call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),irec_size,4,
     1                           MPI_BYTE,istat,ierr)
          mpifilebytes(io10)=mpifilebytes(io10)+4
          call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),ktype,4,
     1                           MPI_BYTE,istat,ierr)
          mpifilebytes(io10)=mpifilebytes(io10)+4
          call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),ksource,4,
     1                           MPI_BYTE,istat,ierr)
          mpifilebytes(io10)=mpifilebytes(io10)+4
          call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),csrcnam,ilen,
     1                           MPI_BYTE,istat,ierr)
          mpifilebytes(io10)=mpifilebytes(io10)+ilen
          call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),xmapkm,4,
     1                           MPI_BYTE,istat,ierr)
          mpifilebytes(io10)=mpifilebytes(io10)+4
          call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),ymapkm,4,
     1                           MPI_BYTE,istat,ierr)
          mpifilebytes(io10)=mpifilebytes(io10)+4
          call MPI_FILE_WRITE_AT(io10,mpifilebytes(io10),irec_size,4,
     1                           MPI_BYTE,istat,ierr)
          mpifilebytes(io10)=mpifilebytes(io10)+4
      endif 
c
      do 600 ig=1,ngrup
c --- Identify array storage location for this group
      i=istore(ig)
c
c --- Only species specified are stored on disk
      if(ioutop(6,ig).eq.1)then
         cname=cgrup(ig)
         cname(13:15)=' WF'
c
c ---    Gridded receptor wet fluxes
         if(lsamp) then
            if(ifull.eq.1)then
               if(lcomprs)then
c ---             Write compressed data records
c                  call comprs(wfsam(1,1,i),mxnxyg,tmp8,mxnxyg,
c     1              cname,io10)
c dmr
                  mpi_nwords = mxnxg*((mysamt-mysamb)+1)
                  call comprs(wfsam(1:mxnxg,mysamb:mysamt,i),mpi_nwords,
     1                        tmp8,mxnxyg,cname,io10)
c dmr
               else
c ---             Write uncompressed data record
c                  call wrdat(io10,cname,wfsam(1,1,i),nxsam,nysam)
c dmr
                  mpinysam = ((mysamt-mysamb)+1)
                  call wrdatu(io10,cname,wfsam(1:mxnxg,mysamb:mysamt,i),
     1                       nxsam,mpinysam)
c dmr
               endif
            else
c               call xtract(wfsam(1,1,i),mxnxg,mxnyg,nxsam,nysam,tmp7)
c dmr   
               mpinysam = ((mysamt-mysamb)+1)
               call mpixtract(wfsam(1,1,i),mxnxg,mxnyg,
     1                     nxsam,mpinysam,tmp7)
c dmr
               if(lcomprs)then
c ---             Write compressed data records
c                  nwords=nxsam*nysam
c                  call comprs(tmp7,nwords,tmp8,mxnxyg,cname,io10)
c dmr
                  mpi_nwords = nxsam*((mysamt-mysamb)+1)
                  call comprs(tmp7,mpi_nwords,tmp8,mxnxyg,cname,io10)
c dmr
               else
c ---             Write uncompressed data record
c                  call wrdat(io10,cname,tmp7,nxsam,nysam)
c dmr
                  call wrdatu(io10,cname,tmp7,nxsam,mpinysam)
c dmr
               endif
            endif
         endif
c
c ---    Discrete receptor wet fluxes
         if(nrec .GT. 0) then
            if(lcomprs)then
c ---          Write compressed data records
c               call comprs(wfrec(1,i),nrec,tmp3,mxrec,cname,io10)
c dmr
               mpinrec = (mpilastrec-mpifirstrec)+1
               call comprs(wfrec(mpifirstrec:mpilastrec,i),mpinrec,tmp3,
     1                     mxrec,cname,io10)
c dmr
            else
c ---          Write uncompressed data record
c               call wrdat(io10,cname,wfrec(1,i),nrec,1)
c dmr
               mpinrec = (mpilastrec-mpifirstrec)+1
               call wrdatur(io10,cname,wfrec(mpifirstrec:mpilastrec,i),
     1                      mpinrec,1)
c dmr
            endif
         endif
      endif
600   continue
692   continue
c
c ---------------------------------------------------------
c --- WRITE VISIBILITY-RELATED DATA TO DISK (RH, T2D)
c ---------------------------------------------------------
c
c --- Output date/hour times use 0-23 convention so that hour 24
c --- of day 12 starts at 23 0000 on day 12 and ends at 00 0000
c --- on day 13.
      if((IVIS.ne.1 .AND. IT2D.ne.1 .AND. IRHO.ne.1)
     &                           .OR. isrcmode.NE.0)go to 792
c
c --- Write date/time data record
      if (mpirank.eq.0) then
      if(IVIS.eq.1)write(io11)nyrb,njulb,nhrb,nsecb,nyr,njul,nhr,nsece
      if(IT2D.eq.1)write(io13)nyrb,njulb,nhrb,nsecb,nyr,njul,nhr,nsece
      if(IRHO.eq.1)write(io14)nyrb,njulb,nhrb,nsecb,nyr,njul,nhr,nsece
      endif
c
c --- Set variable names
      cnameRH=' REL HUM (%)   '
      cnameTK=' TEMP 2D (K)   '
      cnameDN=' RHO 2D (kg/m3)'
c
c frr (09/01) new calmet format (2D RH)
      if(i2dmet.eq.1) then
         if(IVIS.eq.1) then
c ---       Reported RH at CALMET gridpoints
            call xtracti(irh2d,mxnx,mxny,nxm,nym,itmp)
            if (mpirank.eq.0) then
            call wrint(io11,cnameRH,itmp,nxm,nym)
            endif
         endif
         if(IT2D.eq.1) then
c ---       Reported TK at CALMET gridpoints
            call xtract(temp2d,mxnx,mxny,nxm,nym,tmp1)
            if (mpirank.eq.0) then
            call wrdat(io13,cnameTK,tmp1,nxm,nym)
            endif
         endif
         if(IRHO.eq.1) then
c ---       Reported Rho at CALMET gridpoints
            call xtract(rho2d,mxnx,mxny,nxm,nym,tmp1)
            if (mpirank.eq.0) then
            call wrdat(io14,cnameDN,tmp1,nxm,nym)
            endif
         endif
      elseif(i2dmet.eq.0) then
         if(IVIS.eq.1) then
c ---       Reported RH at surface stations
            if (mpirank.eq.0) then
            call wrint(io11,cnameRH,irhss,nssta,1)
            endif
         endif
         if(IT2D.eq.1) then
c ---       Reported TK at surface stations
            if (mpirank.eq.0) then
            call wrdat(io13,cnameTK,tempss,nssta,1)
            endif
         endif
         if(IRHO.eq.1) then
c ---       Reported Rho at surface stations
            if (mpirank.eq.0) then
            call wrdat(io14,cnameDN,rhoss,nssta,1)
            endif
         endif
      else
         write(*,*)'Subr. OUTPUT:  Invalid I2DMET = ',i2dmet
         stop
      endif

c      call wrint(io11,cname,irhss,nssta,1)
c
792   continue


c --- Set format for List-File heading text
1050  format(//,'------ Contribution from Source: ',a16,//)
1051  format(//,'------ Contribution from ALL Sources',//)

c ---------------------------------------------------------
c --- PRINT CONCENTRATIONS TO LIST FILE  (user units)
c ---------------------------------------------------------
      if(icprt.ne.1)go to 192
      if(mod(istep,icfrq).ne.0)go to 192
      if(.not.LSAMP)go to 101
c
c --- Gridded receptor concentrations
c -----------------------------------
      messag='CONCENTRATIONS '
      messag(16:28)=cunits
      messag(31:38)='SPECIES:'
c
      do 100 ig=1,ngrup
c --- Identify array storage location for this group
      i=istore(ig)
c
      if(ioutop(1,ig).eq.1)then

         if(isrcmode.EQ.1) then
            write(io6,1050) csrcnam
         else
            write(io6,1051)
         endif

         write(messag(40:51),'(a12)')cgrup(ig)
c ---    Scale units for IPRTU=2,3,4
         if(iprtu.GT.1 .AND. iprtu.LT.5) then
            do iy=1,nysam
               do ix=1,nxsam
                  chisam(ix,iy,i)=chisam(ix,iy,i)*rprtu(iprtu)
               enddo
            enddo
         endif
c 
       call outsam(chisam(1,1,i),idum,1,5,ldate,messag,nxsam,nysam)
c
      endif
100   continue
101   continue
      if(nrec.eq.0)go to 111
c
c --- Discrete receptor concentrations
c ------------------------------------
c --- Create label of species to be printed
      ns=0
      do ig=1,ngrup
         if(ioutop(1,ig).eq.1)then
            ns=ns+1
            cspecsv(ns)=cgrup(ig)
         endif
      enddo
      if(ns.EQ.0) goto 111

      if(isrcmode.EQ.1) then
         write(io6,1050) csrcnam
      else
         write(io6,1051)
      endif

      write(io6,106)cunits,nyr,nmo,nday,njul,nhr,
     1  (cspecsv(n),n=1,ns)
106   format(//1x,'DISCRETE RECEPTOR CONCENTRATIONS ',a13,t74,
     1 'year: ',i4,2x,'month: ',i2,2x,'day: ',i2,2x,' Julian day: ',i3,
     2 2x,'hour(0-23): ',i2/
     3 1x,'Receptor No.',5x,10a12)
c dmr
c --- Calculate offsets and sizes for MPI collective communication
      myendpts(1)=mpifirstrec
      myendpts(2)=mpilastrec
c
      call MPI_ALLGATHER(myendpts,2,MPI_INT,allendpts,
     1                   2,MPI_INT,MPI_COMM_WORLD,ierr)
c
      cnt=1
      do i=1, mpinumtasks*2,2
        alloffset(cnt) = (allendpts(i)-1)*mxspec
        cnt = cnt + 1
      enddo
      cnt=1
      do i=1, mpinumtasks*2,2
        recvcnts(cnt)=((allendpts(i+1)-allendpts(i))+1)*mxspec
        cnt=cnt+1
      enddo
      asize = ((mpilastrec-mpifirstrec)+1)*mxspec
c --- Transpose chirec array for MPI collective communication
        do i = mpifirstrec,mpilastrec
        do j = 1, mxspec
         tmpchirec(j,i) = chirec(i,j)
        enddo
        enddo
c
c --- Gather all discrete receptor concentrations
      call MPI_ALLGATHERV(tmpchirec(1:mxspec,mpifirstrec:mpilastrec),
     1                    asize,MPI_REAL,mpichirec,recvcnts,alloffset,
     2                    MPI_REAL,MPI_COMM_WORLD,ierr)
c
c --- Transpose tmpchirec array before printing to list file
        do i = 1,nrec
        do j = 1, mxspec
         chirec(i,j) = mpichirec(j,i)
        enddo
        enddo
c dmr        
      do 110 ir=1,nrec
c ---    Scale & store only those groups to be printed in a work array
         ns=0
         do ig=1,ngrup
            if(ioutop(1,ig).eq.1)then
               ns=ns+1
               xtmp(ns)=chirec(ir,istore(ig))*rprtu(iprtu)
            endif
         enddo
c
c ---    Write the discrete receptor concentrations
         write(io6,108)ir,(xtmp(n),n=1,ns)
108      format(1x,i6,8x,10(1pe11.4,1x))
110   continue
111   continue
      if(nctrec.eq.0)goto 192
c
c --- Complex terrain (CTSG) receptor concentrations
c --------------------------------------------------
c --- Create label of species to be printed
      ns=0
      do ig=1,ngrup
         if(ioutop(1,ig).eq.1)then
            ns=ns+1
            cspecsv(ns)=cgrup(ig)
         endif
      enddo
      if(ns.EQ.0) goto 192

      if(isrcmode.EQ.1) then
         write(io6,1050) csrcnam
      else
         write(io6,1051)
      endif

      write(io6,116)cunits,nyr,nmo,nday,njul,nhr,
     1  (cspecsv(n),n=1,ns)
116   format(//1x,'CTSG RECEPTOR CONCENTRATIONS ',a13,t74,
     1 'year: ',i4,2x,'month: ',i2,2x,'day: ',i2,2x,' Julian day: ',i3,
     2 2x,'hour(0-23): ',i2/,4x,'No.',4x,'X met',5x,'Y met',
     3 4x,'ELEV.',4x,'Hill No.',3x,10a12)
c dmr
c --- Calculate offsets and sizes for MPI collective communication
      myendpts(1)=mpifirstrec
      myendpts(2)=mpilastrec
c
      call MPI_ALLGATHER(myendpts,2,MPI_INT,allendpts,
     1                   2,MPI_INT,MPI_COMM_WORLD,ierr)
c
      cnt=1
      do i=1, mpinumtasks*2,2
        alloffset(cnt) = (allendpts(i)-1)*mxspec
        cnt = cnt + 1
      enddo
      cnt=1
      do i=1, mpinumtasks*2,2
        recvcnts(cnt)=((allendpts(i+1)-allendpts(i))+1)*mxspec
        cnt=cnt+1
      enddo
      asize = ((mpilastctrec-mpifirstctrec)+1)*mxspec
c --- Transpose chict array for MPI collective communication
        do i = mpifirstctrec,mpilastctrec
        do j = 1, mxspec
         tmpchict(j,i) = chict(i,j)
        enddo
        enddo
c
c --- Gather all discrete receptor concentrations
      call MPI_ALLGATHERV(tmpchict(1:mxspec,mpifirstctrec:mpilastctrec)
     1                    ,asize,MPI_REAL,mpichict,recvcnts,alloffset,
     2                    MPI_REAL,MPI_COMM_WORLD,ierr)
c
c --- Transpose tmpchict array before printing to list file
        do i = 1,nctrec
        do j = 1, mxspec
         chict(i,j) = mpichict(j,i)
        enddo
        enddo
c dmr        
      do 120 i=1,nctrec
c ---    Scale & store only those groups to be printed in a work array
         ns=0
         do ig=1,ngrup
            if(ioutop(1,ig).eq.1)then
               ns=ns+1
               xtmp(ns)=chict(i,istore(ig))*rprtu(iprtu)
            endif
         enddo
         write(io6,118)i,xrct(i),yrct(i),elrect(i),ihill(i),
     1    (xtmp(n),n=1,ns)
118      format(1x,i5,3x,f7.3,3x,f7.3,3x,f6.1,3x,i5,6x,10(1pe11.4,1x))
120   continue

192   continue
c ---------------------------------------------------------
c --- PRINT DRY FLUXES TO LIST FILE
c ---------------------------------------------------------
      if(idprt.ne.1)go to 292
      if(mod(istep,idfrq).ne.0)go to 292
c
      if(LSAMP) then
c ---    Gridded receptors
c ------------------------
         messag='DRY FLUXES '
         messag(12:24)=funits
         messag(31:38)='SPECIES:'
c
         do ig=1,ngrup
c ---       Identify array storage location for this group
            i=istore(ig)
            if(ioutop(3,ig).eq.1)then

               if(isrcmode.EQ.1) then
                  write(io6,1050) csrcnam
               else
                  write(io6,1051)
               endif

               write(messag(40:51),'(a12)')cgrup(ig)
c ---          Scale units for IPRTU=2,3,4
               if(iprtu.GT.1 .AND. iprtu.LT.5) then
                  do iy=1,nysam
                     do ix=1,nxsam
                        dfsam(ix,iy,i)=dfsam(ix,iy,i)*rprtu(iprtu)
                     enddo
                  enddo
               endif
c
               call outsam(dfsam(1,1,i),idum,1,5,ldate,
     1                     messag,nxsam,nysam)
c
            endif
         enddo
      endif
c
      if(nrec.GT.0) then
c ---    Discrete receptors
c -------------------------
c ---    Create label of species to be printed
         ns=0
         do ig=1,ngrup
            if(ioutop(3,ig).eq.1)then
               ns=ns+1
               cspecsv(ns)=cgrup(ig)
            endif
         enddo
         if(ns.GT.0) then

            if(isrcmode.EQ.1) then
               write(io6,1050) csrcnam
            else
               write(io6,1051)
            endif

            write(io6,206)funits,nyr,nmo,nday,njul,nhr,
     1                    (cspecsv(n),n=1,ns)
206         format(//1x,'DISCRETE RECEPTOR DRY FLUXES ',a13,t74,
     1      'year: ',i4,2x,'month: ',i2,2x,'day: ',i2,2x,
     2      ' Julian day: ',i3,2x,'hour(0-23): ',i2/
     3      1x,'Receptor No.',5x,10a12)
c dmr
c --- Calculate offsets and sizes for MPI collective communication
      myendpts(1)=mpifirstrec
      myendpts(2)=mpilastrec
c
      call MPI_ALLGATHER(myendpts,2,MPI_INT,allendpts,
     1                   2,MPI_INT,MPI_COMM_WORLD,ierr)
c
      cnt=1
      do i=1, mpinumtasks*2,2
        alloffset(cnt) = (allendpts(i)-1)*mxspec
        cnt = cnt + 1
      enddo
      cnt=1
      do i=1, mpinumtasks*2,2
        recvcnts(cnt)=((allendpts(i+1)-allendpts(i))+1)*mxspec
        cnt=cnt+1
      enddo
      asize = ((mpilastrec-mpifirstrec)+1)*mxspec
c --- Transpose dfrec array for MPI collective communication
        do i = mpifirstrec,mpilastrec
        do j = 1, mxspec
         tmpdfrec(j,i) = dfrec(i,j)
        enddo
        enddo
c
c --- Gather all discrete receptor concentrations
      call MPI_ALLGATHERV(tmpdfrec(1:mxspec,mpifirstrec:mpilastrec),
     1                    asize,MPI_REAL,mpidfrec,recvcnts,alloffset,
     2                    MPI_REAL,MPI_COMM_WORLD,ierr)
c
c --- Transpose tmpdfrec array before printing to list file
        do i = 1,nrec
        do j = 1, mxspec
         dfrec(i,j) = mpidfrec(j,i)
        enddo
        enddo
c dmr        
            do ir=1,nrec
c ---          Scale & store only those groups to be printed
               is=0
               do ig=1,ngrup
                  if(ioutop(3,ig).eq.1)then
                     is=is+1
                     xtmp(is)=dfrec(ir,istore(ig))*rprtu(iprtu)
                  endif
               enddo
c ---          Write the discrete receptor dry fluxes
               write(io6,208)ir,(xtmp(n),n=1,ns)
208            format(1x,i6,8x,10(1pe11.4,1x))
            enddo
         endif
      endif

292   continue
c ---------------------------------------------------------
c --- PRINT WET FLUXES TO LIST FILE
c ---------------------------------------------------------
      if(iwprt.ne.1)go to 392
      if(mod(istep,iwfrq).ne.0)go to 392
c
      if(LSAMP) then
c ---    Gridded receptors
c ------------------------
         messag='WET FLUXES '
         messag(12:24)=funits
         messag(31:38)='SPECIES:'
c
         do ig=1,ngrup
c ---       Identify array storage location for this group
            i=istore(ig)
            if(ioutop(5,ig).eq.1)then

               if(isrcmode.EQ.1) then
                  write(io6,1050) csrcnam
               else
                  write(io6,1051)
               endif

               write(messag(40:51),'(a12)')cgrup(ig)
c ---          Scale units for IPRTU=2,3,4
               if(iprtu.GT.1 .AND. iprtu.LT.5) then
                  do iy=1,nysam
                     do ix=1,nxsam
                        wfsam(ix,iy,i)=wfsam(ix,iy,i)*rprtu(iprtu)
                     enddo
                  enddo
               endif
c
               call outsam(wfsam(1,1,i),idum,1,5,ldate,messag,nxsam,
     1                     nysam)
c
            endif
         enddo
      endif
c
      if(nrec.GT.0) then
c ---    Discrete receptors
c -------------------------
c ---    Create label of species to be printed
         ns=0
         do ig=1,ngrup
            if(ioutop(5,ig).eq.1)then
               ns=ns+1
               cspecsv(ns)=cgrup(ig)
            endif
         enddo
         if(ns.GT.0) then

            if(isrcmode.EQ.1) then
               write(io6,1050) csrcnam
            else
               write(io6,1051)
            endif

            write(io6,306)funits,nyr,nmo,nday,njul,nhr,
     1                    (cspecsv(n),n=1,ns)
306         format(//1x,'DISCRETE RECEPTOR WET FLUXES ',a13,t74,
     1      'year: ',i4,2x,'month: ',i2,2x,'day: ',i2,2x,
     2      ' Julian day: ',i3,2x,'hour(0-23): ',i2/
     3      1x,'Receptor No.',5x,10a12)
c dmr
c --- Processes gather global receptor concentrations through an
c      asynchronous cascading send and receive method
c    NEEDS TO BE COMPLETED -- UNSURE IF CURRENT "ALLGATHER" METHOD WILL
c    WORK WITH SEVERAL PROCESSORS...

c --- Calculate offsets and sizes for MPI collective communication
      myendpts(1)=mpifirstrec
      myendpts(2)=mpilastrec

      call MPI_ALLGATHER(myendpts,2,MPI_INT,allendpts,
     1                   2,MPI_INT,MPI_COMM_WORLD,ierr)

      cnt=1
      do i=1, mpinumtasks*2,2
        alloffset(cnt) = (allendpts(i)-1)*mxspec
        cnt = cnt + 1
      enddo
      cnt=1
      do i=1, mpinumtasks*2,2
        recvcnts(cnt)=((allendpts(i+1)-allendpts(i))+1)*mxspec
        cnt=cnt+1
      enddo
      asize = ((mpilastrec-mpifirstrec)+1)*mxspec
c --- Transpose wfrec array for MPI collective communication
        do i = mpifirstrec,mpilastrec
        do j = 1, mxspec
         tmpwfrec(j,i) = wfrec(i,j)
        enddo
        enddo
c
c --- Gather all discrete receptor concentrations
      call MPI_ALLGATHERV(tmpwfrec(1:mxspec,mpifirstrec:mpilastrec),
     1                    asize,MPI_REAL,mpiwfrec,recvcnts,alloffset,
     2                    MPI_REAL,MPI_COMM_WORLD,ierr)
c
c --- Transpose tmpwfrec array before printing to list file
        do i = 1,nrec
        do j = 1, mxspec
         wfrec(i,j) = mpiwfrec(j,i)
        enddo
        enddo
c dmr        
            do ir=mpifirstrec,mpilastrec
c ---          Scale & store only those groups to be printed
               is=0
               do ig=1,ngrup
                  if(ioutop(5,ig).eq.1)then
                     is=is+1
                     xtmp(is)=wfrec(ir,istore(ig))*rprtu(iprtu)
                  endif
               enddo
c
c ---          Write the discrete receptor wet fluxes
               write(io6,208)ir,(xtmp(n),n=1,ns)
308            format(1x,i6,8x,10(1pe11.4,1x))
            enddo
         endif
      endif

392   continue

c ------------------------------------------------------------
c --- Write MASS FLUXES for species-groups to MASSFLX.DAT file
c ------------------------------------------------------------
      if(mpirank.eq.0) then
        if(imflx.EQ.1 .AND. isrcmode.EQ.0) call MFLXOUT(istore)
      endif

c ------------------------------------------------------------
c --- Write MASS BALANCE for species to MASSBAL.DAT file
c ------------------------------------------------------------
      if(mpirank.eq.0) then
        if(imbal.EQ.1 .AND. isrcmode.EQ.0) call MBALOUT
      endif

      return
      end
