      Subroutine CONV2CAL

c******************************************************************************
c
c     CONV2CAL Module of ISC2CAL - Conversion Program for ISCST3 to CALPUFF
c           input files.
c
c     PURPOSE: Converts ISCST3 variables to appropriate CALPUFF variables
c
c     NOTES:  Many of the CALPUFF variables are not directly compatible with
c             the ISCST variables.  The user should manually check the created
c             CALPUFF input file and edit the values that are necessary.  The
c             hardwired values are denoted by "(DEF)".
c
c     PROGRAMMER: Vincent Tino
c
c     ORIGINAL DATE: February 19, 1996
c     FIRST MODIFICATION: March 18, 1996 - Added MOD4 Variables (dependencies
c                 of Met Files)
c     SECOND MODIFICATION: May 9, 1996 - Added Input Group 0 (Filenames)
c     THIRD MODIFICATION: June 27, 1996 - Added MOD5 Variables
c
c --- Updates:
c ---          08/02 (DGS) - Move statement labels 200 and 210 (LF95 compile)
c ---                      - Fix several format number errors (LF95 compile)
c ---          03/01 (DGS) - Change IBTZ to XBTZ
c ---          03/01 (DGS) - Revise begin hour logic to avoid IBHR=0
c ---          11/00 (DGS) - Revise VOLEMARB variables
c ---          11/00 (DGS) - Remove CALPUFF XLAT,XLONG
c ---          11/00 (DGS) - Replace CALPUFF XTZ with IBTZ
c ---          11/00 (DGS) - Add PRIME variables BUILDLEN, XBADJ,
c                            and YBADJ, and new CALPUFF switch MBDW
c ---          07/98 (DGS) - Remove SVMIN,SWMIN so that new default
c                            arrays will be used in CALPUFF
c ---          07/98 (DGS) - Test for non-zero building dimensions
c                            to signal downwash params modified
c ---          12/97 (DGS) - Add NSPT1,NSAR1,NSVL1,NSLN1 variables to
c                            output file (CALPUFF.INP)
c ---          12/97 (DGS) - Update default settings
c ---          12/97 (DGS) - Allow C*40 ISC pathnames to files
c ---          12/97 (DGS) - Increase field width for writing source data
c
c******************************************************************************

C***  VARIABLE DECLARATIONS
      include 'MAIN1.INC'

C***  LOCAL VARIABLES
      CHARACTER appc*35, linebreak(50),
     &            AINPGRP7*45, AINPGRP10*25
      CHARACTER*8 cpid(nsrc),caid(nsrc),cvid(nsrc)
      LOGICAL ldw(nsrc),lqfact(nsrc)
      REAL cpxs(nsrc),cpys(nsrc), cphs(nsrc), cpzs(nsrc),
     &     cpds(nsrc), cpvs(nsrc), cpts(nsrc), cpqs(nsrc), dw(nsrc),
     &     cvxs(nsrc), cvys(nsrc), cvzs(nsrc), cvqs(nsrc),
     &     cvhs(nsrc), cvyinit(nsrc), cvzinit(nsrc),
     &     cazs(nsrc), caqs(nsrc),
     &     cahs(nsrc), cazinit(nsrc), caxvert(nsrc,4), cayvert(nsrc,4),
     &     cpfact(nsrc,nqf),cafact(nsrc,nqf),cvfact(nsrc,nqf)
      INTEGER inqfp(nsrc),inqfa(nsrc),inqfv(nsrc)
      INTEGER ipvary(nsrc),iavary(nsrc),ivvary(nsrc)

      REAL plx0(6), ptg0(6)


C - Input Group 0
      CHARACTER*40 metdat, iscdat, plmdat, prfdat, sfcdat, puflst,
     &      condat, dfdat,  wfdat,  visdat, ptdat,  voldat, ardat,
     &      ozdat, vddat, chemdat,hildat, rctdat, debugfile
      LOGICAL lcfiles

C - Input Group 1
      INTEGER IBYR,IBMO,IBDY,IBHR,XBTZ,IRLG,NSPEC,NSE,ITEST,METFM,
     &      METRUN
      REAL    AVET

C - Input Group 2
      INTEGER MGAUSS,MCTADJ,MCTSG,MSLUG,MTRANS,MTIP,MSHEAR,MSPLIT,
     &      MCHEM,MWET,MDRY,MDISP,MTURBVW,MDISP2,MROUGH,MPARTL,
     &      MBDW,MREG

C - Input Group 3

C - Input Group 4
      INTEGER NX,NY,NZ,IUTMZN,IBCOMP,IECOMP,JBCOMP,JECOMP,IBSAMP,
     &      JBSAMP,IESAMP,JESAMP,MESHDN
      LOGICAL LSAMP
      REAL    DGRIDKM,XORIGKM,YORIGKM
      REAL    XGRIDKM,YGRIDKM

C - Input Group 5
      INTEGER ICON,IDRY,IWET,IVIS,ICPRT,IDPRT,IWPRT,ICFRQ,IDFRQ,
     &       IWFRQ,IPRTU,IMESG,NPFDEB,NN1,NN2
      LOGICAL LDEBUG,LCOMPRS

C - Input Group 6
      INTEGER NHILL,NCTREC,MHILL
      REAL    XHILL2M,ZHILL2M,XCTDMKM,YCTDMKM

C - Input Group 7
C - Input Group 8
C - Input Group 9
      INTEGER IVEG,nint
      REAL    RCUTR,RGR,REACTR

C - Input Group 10
C - Input Group 11
      INTEGER MOZ
      REAL    BCKO3,BCKNH3,RNITE1,RNITE2,RNITE3

C - Input Group 12
      INTEGER MHFTSZ,JSUP,IURB1,IURB2,ILANDUIN,ISIGMAV,IMIXCTDM,
     &      MXNEW,MXSAM,NSPLIT,IRESPLIT(24)
      REAL  SYTDEP,CONK1,CONK2,TBD,Z0IN,XLAIIN,ANEMHT,XMXLEN,XSAMLEN,
     &      SYMIN,SZMINc,SVMIN,SWMIN,WSCALM,XMAXZI,XMINZI,SL2PF,
     &      ZISPLIT,ROLDMAX

C - Input Group 13
      INTEGER NPT1,IPTU,NPT2,nspt1

C - Input Group 14
      INTEGER NAR1,IARU,NAR2,nsar1

C - Input Group 15
      INTEGER NLINES,ILNU,MXNSEG,NLRISE,nsln1
      REAL    XL,HBL,WBL,WML,DXL,FPRIMEL

C - Input Group 16
      INTEGER NVL1,IVLU,NVL2,nsvl1

C - Input Group 17



      DATA linebreak/50*'-'/
      DATA XORIGKM/99999999./,YORIGKM/99999999./,XGRIDKM/-99999999./,
     &     YGRIDKM/-99999999./

c --- Control variable for adding border to receptor domain to
c --- define the modeling domain
      data borderkm/0.5/

C***  SET CALPUFF DEFAULTS
      DATA metdat/'CALMET.DAT'/,iscdat/'ISCMET.DAT'/,
     &      plmdat/'PLMMET.DAT'/,prfdat/'PROFILE.DAT'/,
     &      sfcdat/'SURFACE.DAT'/,puflst/'CALPUFF.LST'/,
     &      condat/'CONC.DAT'/,dfdat/'DFLX.DAT'/,wfdat/'WFLX.DAT'/,
     &      visdat/'VISB.DAT'/,ptdat/'PTEMARB.DAT'/,
     &      voldat/'VOLEM.DAT'/,ardat/'BAEMARB.DAT'/,
     &      ozdat/'OZONE.DAT'/,
     &      vddat/'VD.DAT'/,chemdat/'CHEM.DAT'/,
     &      hildat/'HILL.DAT'/,rctdat/'HILLRCT.DAT'/,
     &      debugfile/'DEBUG.DAT'/
     &      LCFILES/.TRUE./
      DATA METRUN/0/,NSPEC/1/,NSE/1/,ITEST/2/,METFM/2/,AVET/60.0/
      DATA MGAUSS/1/,MCTADJ/1/,MCTSG/0/,MSLUG/0/,MTRANS/1/,MTIP/1/,
     &       MSHEAR/0/,MSPLIT/0/,MCHEM/0/,MWET/0/,MDRY/0/,MDISP/3/,
     &       MTURBVW/0/,MDISP2/4/,
     &       MROUGH/0/,MPARTL/0/,MBDW/1/,MREG/0/
c     &       MROUGH/0/,MPARTL/0/,MBDW/1/,MREG/0/
      DATA NX/2/,NY/2/,NZ/1/,IUTMZN/0/,XBTZ/6/,
     &       LSAMP/.FALSE./,IBCOMP/1/,IECOMP/2/,
     &       JBCOMP/1/,JECOMP/2/IBSAMP/0/,JBSAMP/0/,IESAMP/0/,
     &       JESAMP/0/,MESHDN/1/
      DATA ICON/1/,IDRY/1/,IWET/1/,IVIS/0/,LCOMPRS/.TRUE./,ICPRT/0/,
     &       IDPRT/0/,IWPRT/0/,ICFRQ/1/,IDFRQ/1/,IWFRQ/1/,IPRTU/3/,
     &       IMESG/1/,
     &       LDEBUG/.FALSE./,NPFDEB/1/,NN1/1/,NN2/10/
      DATA NHILL/0/,NCTREC/0/,MHILL/0/,XHILL2M/1.0/,ZHILL2M/1.0/,
     &       XCTDMKM/0/,YCTDMKM/0/
      DATA RCUTR/30.0/,RGR/5.0/,REACTR/8.0/,NINT/9./,IVEG/1/
      DATA MOZ/0/,BCKO3/80.0/,BCKNH3/10.0/,RNITE1/0.2/,RNITE2/2.0/,
     &       RNITE3/2.0/
      DATA SYTDEP/550.0/,MHFTSZ/0/,JSUP/5/,CONK1/0.01/,CONK2/0.1/,
     &       TBD/0.5/,IURB1/10/,IURB2/19/,ILANDUIN/20/,Z0IN/0.25/,
     &       XLAIIN/3.0/,ANEMHT/10.0/,ISIGMAV/2/,IMIXCTDM/0/,
     &       XMXLEN/1.0/,XSAMLEN/1.0/,MXNEW/99/,
     &       MXSAM/99/,SYMIN/1.0/,SZMINc/1.0/,SVMIN/0.5/,SWMIN/0.016/,
     &       WSCALM/0.5/,XMAXZI/3000.0/,XMINZI/50.0/,
     &       appc/'0.50, 0.50, 0.50, 0.50, 0.35, 0.35 '/,SL2PF/10.0/,
     &       NSPLIT/3/,IRESPLIT/17*0,1,6*0/,ZISPLIT/100.0/,
     &       ROLDMAX/0.25/
      DATA NPT1/0/,IPTU/1/,NPT2/0/
      DATA NAR1/0/,IARU/1/,NAR2/0/
      DATA NLINES/0/,ILNU/1/,MXNSEG/7/,NLRISE/6/,XL/0.0/,HBL/0.0/,
     &       WBL/0.0/,WML/0.0/,DXL/0.0/,FPRIMEL/0.0/
      DATA NVL1/0/,IVLU/1/,NVL2/0/
      data nspt1/0/,nsar1/0/,nsln1/0/,nsvl1/0/

C***  PROCESS UTILITY CONVERSIONS

      IO = IOCPUNIT

C***  PROCESS FROM ME PATHWAY VARIABLES

c  "INPUTFIL"
      if (METFRM.eq.'CARD'.or.METFRM.eq.'UNFORM') then
        iscdat='            '
        write (*,1000)
1000  format(' WARNING:  ISC Meteorological file not compatible with CAL
     &PUFF!',/,'   CONVERT CARD OR UNFORMATTED FILE TO ASCII!')
      else
        iscdat=metinp
      end if

c  if "STARTEND"
      if (idatetyp.eq.1) then
        IBYR = ISYR
        IBMO = ISMN
        IBDY = ISDY
        IBHR = ISHR
        IRLG = ((IEJDAY - ISJDAY)*24)+(IEHR-IBHR+1)

c   else if "DAYRANGE"
      else if (idatetyp.eq.2) then

c     process year from SURFFILE CARD
        if (ISYEAR.gt.2000) then
          IBYR = ISYEAR - 2000
        else
          IBYR = ISYEAR - 1900
        end if

c       if "Single Julian Day"
        if (irangetyp.eq.1) then
        call grday (IBYR,JDAY,IBMO,IBDY)
        IBHR = 1
        IRLG = 24

c       if "Julian Day Range"
        else if (irangetyp.eq.2) then
        call grday (IBYR,JDAYB,IBMO,IBDY)
        IBHR = 1
        IRLG = (JDAYE-JDAYB+1)*24

c       if "Single Gregorian Day"
        else if (irangetyp.eq.3) then
        IBMO = IM
        IBDY = ID
        IBHR = 1
        IRLG = 24

c       if "Gregorian Day Range"
        else if (irangetyp.eq.4) then
        IBMO = IMN1
        IBDY = IDY1
        IRLG = (JDAYE-JDAYB+1)*24
        else
        end if

c   else process 1 year using input year of SFC file.
      else
        if (ISYEAR.gt.2000) then
          IBYR = ISYEAR - 2000
        else
          IBYR = ISYEAR - 1900
        end if
        IBMO = 1
        IBDY = 1
        IBHR = 1
        if (mod(IBYR,4).eq.0) then
          IRLG = 8784
        else
          IRLG = 8760
        end if
      end if

C***  CONVERT ISC LOGICALS TO INTEGER FLAGS

c-- RUNORNOT
      if (RUN) then
        ITEST = 2
      else
        ITEST = 1
      end if

c-- MODELOPT IF REGULTORY DEFAULT

      if (DFAULT) then

      MTRANS = 0
      MTIP = 1

      if (RURAL) then
        plx0(1) = 0.07
        plx0(2) = 0.07
        plx0(3) = 0.10
        plx0(4) = 0.15
        plx0(5) = 0.35
        plx0(6) = 0.55
        ptg0(5) = 0.020
        ptg0(6) = 0.035
      else if (URBAN) then
        plx0(1) = 0.15
        plx0(2) = 0.15
        plx0(3) = 0.20
        plx0(4) = 0.25
        plx0(5) = 0.30
        plx0(6) = 0.30
        ptg0(5) = 0.020
        ptg0(6) = 0.035
      end if

c-- MODELOPT IF NOT REGULTORY DEFAULT

      else if (.NOT.DFAULT) then

      if (NOSTD) then
        MTIP = 0
      else
        MTIP = 1
      end if

      if (GRDRIS) then
        MTRANS = 1
      else
        MTRANS = 0
      end if

      if (USERP) then
        do jj = 1,6
          plx0(jj) = PUSER(jj,1)
        end do
      else
        if (RURAL) then
          plx0(1) = 0.07
          plx0(2) = 0.07
          plx0(3) = 0.10
          plx0(4) = 0.15
          plx0(5) = 0.35
          plx0(6) = 0.55
        else if (URBAN) then
          plx0(1) = 0.15
          plx0(2) = 0.15
          plx0(3) = 0.20
          plx0(4) = 0.25
          plx0(5) = 0.30
          plx0(6) = 0.30
        end if
      end if

      if (USERDT) then
        do jj = 1,6
          ptg0(jj) = DTUSER(jj,1)
        end do
      else
        if (RURAL) then
          ptg0(5) = 0.020
          ptg0(6) = 0.035
        else if (URBAN) then
          ptg0(5) = 0.020
          ptg0(6) = 0.035
        end if
      end if

      end if

c-- END REGULATORY DEFAULT OPTIONS

      if (DDEP.or.DEPOS) then
        MDRY = 1
        IDRY = 1
c       IDPRT = 1
        IDDEPTYP = 1
      else
        MDRY = 0
        IDRY = 0
c       IDPRT = 0
        IDDEPTYP = 0
      end if
      if (WDEP.or.DEPOS) then
        MWET = 1
        IWET = 1
c       IWPRT = 1
      else
        MWET = 0
        IWET = 0
c       IWPRT = 0
      end if
      if (CONC) then
        ICON = 1
c       ICPRT = 1
      else
        ICON = 0
c       ICPRT = 0
      end if


C***  DETERMINE MET and COMPUTATIONAL GRID VARIABLES

c --  Find max and min source or receptor coordinates
      do i=1,numsrc
        if (AXS(i).lt.XORIGKM) XORIGKM = AXS(i)
        if (AYS(i).lt.YORIGKM) YORIGKM = AYS(i)
        if (AXS(i).gt.XGRIDKM) XGRIDKM = AXS(i)
        if (AYS(i).gt.YGRIDKM) YGRIDKM = AYS(i)
      end do
      do i=1,numrec
        if (AXR(i).lt.XORIGKM) XORIGKM = AXR(i)
        if (AYR(i).lt.YORIGKM) YORIGKM = AYR(i)
        if (AXR(i).gt.XGRIDKM) XGRIDKM = AXR(i)
        if (AYR(i).gt.YGRIDKM) YGRIDKM = AYR(i)
      end do

c --  Extend
      XORIGKM = (XORIGKM/1000.) - borderkm
      YORIGKM = (YORIGKM/1000.) - borderkm
      XGRIDKM = (XGRIDKM/1000.) + borderkm
      YGRIDKM = (YGRIDKM/1000.) + borderkm

      DGRIDKM = AMAX1(((XGRIDKM-XORIGKM)/NX),((YGRIDKM-YORIGKM)/NY))

      IECOMP = NX
      JECOMP = NY

C*** DETERMINE DEPOSITION VARIABLES

      if (POLLUT.eq.'SO2') then
        AINPGRP7  = ' 0.1509, 1000.0,    8.0,    0.0,    0.0'
        AINPGRP10 = '    3.0e-5,     0.0      '
      else if (POLLUT.eq.'NOX') then
        AINPGRP7  = ' 0.1656,    1.0,    8.0,    5.0,    3.5'
        AINPGRP10 = '       0.0,     0.0      '
      else
      end if

C***  PROCESS SOURCE INFORMATION

      npt1 = 0
      nvl1 = 0
      nar1 = 0

C--- PROCESS EMISSION FACTORS
c--- if any emission factor is nonzero, then set the logical to true.

      do i = 1, numsrc
c       do j = 1, nqf
c         if (qfact(j,i).ne.0.0) then
          if (qflag(i).ne.'      ') then
            lqfact(i)=.true.
            goto 100
          end if
c       end do
100   end do

      do i=1,numsrc

      if (SRCTYP(i).eq.'POINT') then
        npt1 = npt1 + 1
        cpid(npt1) = srcid(i)
        cpxs(npt1) = axs(i)/1000.
        cpys(npt1) = ays(i)/1000.
        cphs(npt1) = ahs(i)
        cpzs(npt1) = azs(i)
        cpds(npt1) = ads(i)
        cpvs(npt1) = avs(i)
        cpts(npt1) = ats(i)
        cpqs(npt1) = aqs(i)

c ---   Set downwash indicators
        ldw(npt1) = .FALSE.
        dw(npt1) = 0.0       
        do j=1,nsec
          if ((ADSBW(j,i).ne.0.0).OR.(ADSBH(j,i).ne.0.0)) then
            ldw(npt1) = .TRUE.
            dw(npt1) = 1.0
c ---       Non-zero building length signals PRIME downwash
            if (ADSBL(j,i).GT.0.0) mbdw=2
          end if
        end do
        if (lqfact(i)) then

c ---     Increment number of point-source-species with var. emissions
          nspt1=nspt1+1

          if (qflag(i).eq.'SEASON') then
            inqfp(npt1) = 96
            ipvary(npt1) = 3
            do k=1,4
              do j=1,24
                cpfact(npt1,(k-1)*24+j) = qfact(k,i)
              end do
            end do
            goto 200
          else if (qflag(i).eq.'MONTH ') then
            inqfp(npt1) = 12
            ipvary(npt1) = 2
          else if (qflag(i).eq.'HROFDY') then
            inqfp(npt1) = 24
            ipvary(npt1) = 1
          else if (qflag(i).eq.'STAR  ') then
            inqfp(npt1) = 36
            ipvary(npt1) = 4
          else if (qflag(i).eq.'SEASHR') then
            inqfp(npt1) = 96
            ipvary(npt1) = 3
          end if
          do j=1,inqfp(npt1)
            cpfact(npt1,j) = qfact(j,i)
          end do
200       continue
        end if

      else if (SRCTYP(i).eq.'VOLUME') then
        nvl1 = nvl1 + 1
        cvid(nvl1) = srcid(i)
        cvxs(nvl1) = AXS(I)/1000.
        cvys(nvl1) = AYS(I)/1000.
        cvzs(nvl1) = AZS(I)
        cvqs(nvl1) = AQS(I)
        cvhs(nvl1) = AHS(I)
        cvyinit(nvl1) = ASYINI(I)
        cvzinit(nvl1) = ASZINI(I)
        if (lqfact(i)) then

c ---     Increment number of volume-source-species with var. emissions
          nsvl1=nsvl1+1

          if (qflag(i).eq.'SEASON') then
            inqfv(nvl1) = 96
            ivvary(nvl1) = 3
            do k=1,4
              do j=1,24
                cvfact(nvl1,(k-1)*24+j) = qfact(k,i)
              end do
            end do
            goto 210
          else if (qflag(i).eq.'MONTH ') then
            inqfv(nvl1) = 12
            ivvary(nvl1) = 2
          else if (qflag(i).eq.'HROFDY') then
            inqfv(nvl1) = 24
            ivvary(nvl1) = 1
          else if (qflag(i).eq.'STAR  ') then
            inqfv(nvl1) = 36
            ivvary(nvl1) = 4
          else if (qflag(i).eq.'SEASHR') then
            inqfv(nvl1) = 96
            ivvary(nvl1) = 3
          end if
          do j=1,inqfv(nvl1)
            cvfact(nvl1,j) = qfact(j,i)
          end do
210       continue
        end if

      else if (SRCTYP(i).eq.'AREA') then
c ---   Special case:  Area-source algorithm is best simulated with
c ---                  SLUGS, so use SLUG treatment if any area
c ---                  sources are included
        MSLUG=1
        nar1 = nar1 + 1
        caid(nar1) = srcid(i)
        cazs(nar1) = AZS(I)
        caqs(nar1) = AQS(I)
        cahs(nar1) = AHS(I)
        cazinit(nar1) = ASZINI(I)
        do j=1,4
          caxvert(nar1,j) = AXVERT(j,i)
          cayvert(nar1,j) = AYVERT(j,i)
        end do
        if (lqfact(i)) then

c ---     Increment number of area-source-species with var. emissions
          nsar1=nsar1+1

          if (qflag(i).eq.'SEASON') then
            inqfa(nar1) = 96
            iavary(nar1) = 3
            do k=1,4
              do j=1,24
                cafact(nar1,(k-1)*24+j) = qfact(k,i)
              end do
            end do
            goto 220
          else if (qflag(i).eq.'MONTH ') then
            inqfa(nar1) = 12
            iavary(nar1) = 2
          else if (qflag(i).eq.'HROFDY') then
            inqfa(nar1) = 24
            iavary(nar1) = 1
          else if (qflag(i).eq.'STAR  ') then
            inqfa(nar1) = 36
            iavary(nar1) = 4
          else if (qflag(i).eq.'SEASHR') then
            inqfa(nar1) = 96
            iavary(nar1) = 3
          end if
          do j=1,inqfa(nar1)
            cafact(nar1,j) = qfact(j,i)
          end do
        end if

      end if

220   end do

C***  PROCESS MISCELLANEOUS ISC VARIABLES

      ANEMHT = zref

      if (URBAN) then
        ILANDUIN = 10
      else if (RURAL) then
        ILANDUIN = 20
      else
        ILANDUIN = 20
      end if

C***  PROCESS KEYWORD LOGICALS UNKNOWN TO CALPUFF

c-- CO MODELOPT Pathway
      if (DDPLETE) Write(*,*) ' ISC2PUF cannot convert "DRYDPLT"'
      if (WDPLETE) Write(*,*) ' ISC2PUF cannot convert "WETDPLT"'
      if (NOBID)   Write(*,*) ' ISC2PUF cannot convert "NOBID" '
      if (NOCALM)  Write(*,*) ' ISC2PUF cannot convert "NOCALM" '
      if (MSGPRO)  Write(*,*) ' ISC2PUF cannot convert "MSGPRO"'
      if (NOCMPL)  Write(*,*) ' ISC2PUF cannot convert "NOCMPL"'
      if (NOSMPL)  Write(*,*) ' ISC2PUF cannot convert "NOSMPL"'


C*****************************************************************************
C***  WRITE OUT FORMATTED CALPUFF INPUT FILE

      write (IO,*) TITLE1
      write (IO,*) TITLE2
      write (IO,*)
      write (IO,*) linebreak
      write (IO,*)
      write (IO,*)'              CALPUFF MODEL CONTROL FILE'
      write (IO,*)

      write (IO,*) linebreak
      write (IO,*) 'INPUT GROUP:  0'
      write (IO,9000) '  ! METDAT =  ',metdat,  '  !      '
      write (IO,9000) '  ! ISCDAT =  ',iscdat,  '  !      '
      write (IO,9000) '  ! PLMDAT =  ',plmdat,  '  !      '
      write (IO,9000) '  ! PRFDAT =  ',prfdat,  '  !      '
      write (IO,9000) '  ! SFCDAT =  ',sfcdat,  '  !      '
      write (IO,9000) '  ! PUFLST =  ',puflst,  '  !      '
      write (IO,9000) '  ! CONDAT =  ',condat,  '  !      '
      write (IO,9000) '  ! DFDAT =   ',dfdat,   '  !      '
      write (IO,9000) '  ! WFDAT =   ',wfdat,   '  !      '
      write (IO,9000) '  ! VISDAT =  ',visdat,  '  !      '
      write (IO,9000) '  ! PTDAT =   ',ptdat,   '  !      '
      write (IO,9000) '  ! VOLDAT =  ',voldat,  '  !      '
      write (IO,9000) '  ! ARDAT =   ',ardat,   '  !      '
      write (IO,9000) '  ! OZDAT =   ',ozdat,   '  !      '
      write (IO,9000) '  ! VDDAT =   ',vddat,   '  !      '
      write (IO,9000) '  ! CHEMDAT = ',chemdat, '  !      '
      write (IO,9000) '  ! HILDAT =  ',hildat,  '  !      '
      write (IO,9000) '  ! RCTDAT =  ',rctdat,  '  !      '
      write (IO,9000) '  ! DEBUG =   ',debugfile,'  !      '
      write (IO,9001) '  ! LCFILES = ',lcfiles, '  !      '
      write (IO,*)
      write (IO,*) '!END!'
9000  format (1x,a14,a40,a9)
9001  format (1x,a14,l1,11x,a9)


      write (IO,*) linebreak
      write (IO,*) 'INPUT GROUP:  1'
      write (IO,9010) '  ! METRUN = ',METRUN,    '  ! (DEF)'
      write (IO,9010) '  ! IBYR =   ',IBYR,      '  !      '
      write (IO,9010) '  ! IBMO =   ',IBMO,      '  !      '
      write (IO,9010) '  ! IBDY =   ',IBDY,      '  !      '
      write (IO,9010) '  ! IBHR =   ',IBHR,      '  !      '
      write (IO,9010) '  ! XBTZ =   ',XBTZ,      '  ! (DEF)'
      write (IO,9010) '  ! IRLG =   ',IRLG,      '  !      '
      write (IO,9010) '  ! NSPEC =  ',NSPEC,     '  ! (DEF)'
      write (IO,9010) '  ! NSE =    ',NSE,       '  ! (DEF)'
      write (IO,9010) '  ! ITEST =  ',ITEST,     '  !      '
      write (IO,9010) '  ! METFM =  ',METFM,     '  ! (DEF)'
      write (IO,9011) '  ! AVET =   ',AVET,      '  ! (DEF)'
      write (IO,*)
      write (IO,*) '!END!'
9010  format (1x,a13,i4,a9)
9011  format (1x,a13,F4.1,a9)
      write (IO,*)

      write (IO,*) linebreak
      write (IO,*) 'INPUT GROUP:  2'
      write (IO,9020) '  ! MGAUSS =  ',MGAUSS,  '  ! (DEF)'
      write (IO,9020) '  ! MCTADJ =  ',MCTADJ,  '  ! (DEF)'
      write (IO,9020) '  ! MCTSG =   ',MCTSG,   '  ! (DEF)'
      write (IO,9020) '  ! MSLUG =   ',MSLUG,   '  !      '
      write (IO,9020) '  ! MTRANS =  ',MTRANS,  '  !      '
      write (IO,9020) '  ! MTIP =    ',MTIP,    '  !      '
      write (IO,9020) '  ! MBDW =    ',MBDW,    '  !      '
      write (IO,9020) '  ! MSHEAR =  ',MSHEAR,  '  ! (DEF)'
      write (IO,9020) '  ! MSPLIT =  ',MSPLIT,  '  ! (DEF)'
      write (IO,9020) '  ! MCHEM =   ',MCHEM,   '  ! (DEF)'
      write (IO,9020) '  ! MWET =    ',MWET,    '  !      '
      write (IO,9020) '  ! MDRY =    ',MDRY,    '  !      '
      write (IO,9020) '  ! MDISP =   ',MDISP,   '  ! (DEF)'
      write (IO,9020) '  ! MTURBVW = ',MTURBVW, '  ! (DEF)'
      write (IO,9020) '  ! MDISP2 =  ',MDISP2,  '  ! (DEF)'
      write (IO,9020) '  ! MROUGH =  ',MROUGH,  '  ! (DEF)'
      write (IO,9020) '  ! MPARTL =  ',MPARTL,  '  ! (DEF)'
      write (IO,9020) '  ! MREG =    ',MREG,    '  ! (DEF)'
      write (IO,*)
      write (IO,*) '!END!'
9020  format (1x,a14,i1,a9)
      write (IO,*)

      write (IO,*) linebreak
      write (IO,*) 'INPUT GROUP:  3'
      write (IO,9030) '  ! CSPEC = ', POLLUT,' !  !END!'
      write (IO,9031) '  ! ',POLLUT,' = 1, 1,',IDDEPTYP,', 0  !'
      write (IO,*)
      write (IO,*) '!END!'
9030  format (1x,a12,a8,a9)
9031  format (1x,a4,2a8,i2,a6)
      write (IO,*)

      write (IO,*) linebreak
      write (IO,*) 'INPUT GROUP:  4'
      write (IO,9041) '  ! NX =        ',NX,      '  ! (DEF)'
      write (IO,9041) '  ! NY =        ',NY,      '  ! (DEF)'
      write (IO,9041) '  ! NZ =        ',NZ,      '  ! (DEF)'
      write (IO,9042) '  ! DGRIDKM =   ',DGRIDKM, '  !      '
      write (IO,*)    '   ! ZFACE =     0., 5000.0    ! (DEF)'
      write (IO,9042) '  ! XORIGKM =   ',XORIGKM, '  !      '
      write (IO,9042) '  ! YORIGKM =   ',YORIGKM, '  !      '
      write (IO,9041) '  ! IUTMZN =    ',IUTMZN,  '  ! (DEF)'
      write (IO,9041) '  ! IBCOMP =    ',IBCOMP,  '  !      '
      write (IO,9041) '  ! JBCOMP =    ',JBCOMP,  '  !      '
      write (IO,9041) '  ! IECOMP =    ',IECOMP,  '  !      '
      write (IO,9041) '  ! JECOMP =    ',JECOMP,  '  !      '
      write (IO,9040) '  ! LSAMP =     ',LSAMP,   '  ! (DEF)'
      write (IO,9041) '  ! IBSAMP =    ',IBSAMP,  '  ! (DEF)'
      write (IO,9041) '  ! JBSAMP =    ',JBSAMP,  '  ! (DEF)'
      write (IO,9041) '  ! IESAMP =    ',IESAMP,  '  ! (DEF)'
      write (IO,9041) '  ! JESAMP =    ',JESAMP,  '  ! (DEF)'
      write (IO,9041) '  ! MESHDN =    ',MESHDN,  '  ! (DEF)'
      write (IO,*)
      write (IO,*) '!END!'
9040  format (1x,a13,8x,l1,a9)
9041  format (1x,a13,6x,i3,a9)
9042  format (1x,a13,f9.2,a9)
      write (IO,*)

      write (IO,*) linebreak
      write (IO,*) 'INPUT GROUP:  5'
      write (IO,9050) '  ! ICON =    ',ICON,     '  !      '
      write (IO,9050) '  ! IDRY =    ',IDRY,     '  !      '
      write (IO,9050) '  ! IWET =    ',IWET,     '  !      '
      write (IO,9050) '  ! IVIS =    ',IVIS,     '  ! (DEF)'
      write (IO,9052) '  ! LCOMPRS = ',LCOMPRS,  '  ! (DEF)'
      write (IO,9050) '  ! ICPRT =   ',ICPRT,    '  !      '
      write (IO,9050) '  ! IDPRT =   ',IDPRT,    '  !      '
      write (IO,9050) '  ! IWPRT =   ',IWPRT,    '  !      '
      write (IO,9050) '  ! ICFRQ =   ',ICFRQ,    '  ! (DEF)'
      write (IO,9050) '  ! IDFRQ =   ',IDFRQ,    '  ! (DEF)'
      write (IO,9050) '  ! IWFRQ =   ',IWFRQ,    '  ! (DEF)'
      write (IO,9050) '  ! IPRTU =   ',IPRTU,    '  ! (DEF)'
      write (IO,9050) '  ! IMESG =   ',IMESG,    '  ! (DEF)'
      write (IO,9051) '  ! ',POLLUT,' = ',ICPRT,ICON,IDPRT,IDRY,
     &                IWPRT,IWET,' !'
      write (IO,9052) '  ! LDEBUG =  ',LDEBUG,   '  ! (DEF)'
      write (IO,9050) '  ! NPFDEB =  ',NPFDEB,   '  ! (DEF)'
      write (IO,9050) '  ! NN1 =     ',NN1,      '  ! (DEF)'
      write (IO,9050) '  ! NN2 =     ',NN2,      '  ! (DEF)'
      write (IO,*)
      write (IO,*) '!END!'
9050  format (1x,a13,i3,a9)
9051  format (1x,a4,a8,a3,5(i2,','),i2,a2)
9052  format (1x,a13,2x,l1,a9)
      write (IO,*)

      write (IO,*) linebreak
      write (IO,*) 'INPUT GROUP:  6A,6B,6C'
      write (IO,9060) '  ! NHILL =   ',NHILL,       '  ! (DEF)'
      write (IO,9060) '  ! NCTREC =  ',NCTREC,      '  ! (DEF)'
      write (IO,9060) '  ! MHILL =   ',MHILL,       '  ! (DEF)'
      write (IO,9061) '  ! XHILL2M = ',XHILL2M,     '  ! (DEF)'
      write (IO,9061) '  ! ZHILL2M = ',ZHILL2M,     '  ! (DEF)'
      write (IO,9061) '  ! XCTDMKM = ',XCTDMKM,     '  ! (DEF)'
      write (IO,9061) '  ! YCTDMKM = ',YCTDMKM,     '  ! (DEF)'
      write (IO,*)
      write (IO,*) '!END!'
      write (IO,*) ' GROUPS 6B & 6C MUST BE INPUT BY USER'
9060  format (1x,a14,i5,a9)
9061  format (1x,a14,f5.1,a9)
      write (IO,*)

      write (IO,*) linebreak
      write (IO,*) 'INPUT GROUP:  7'
      if (idry.eq.1) then
        write (IO,*) '  ! ',POLLUT,' = ',AINPGRP7,'  !'
      else
      end if
      write (IO,*)
      write (IO,*) '!END!'
      write (IO,*)

      write (IO,*) linebreak
      write (IO,*) 'INPUT GROUP:  8'
      write (IO,*) ' MUST BE INPUT BY USER'
      write (IO,*)
      write (IO,*) '!END!'
      write (IO,*)

      write (IO,*) linebreak
      write (IO,*) 'INPUT GROUP:  9'
      write (IO,9090) '  ! RCUTR =  ',RCUTR, '  ! (DEF)'
      write (IO,9090) '  ! RGR =    ',RGR,   '  ! (DEF)'
      write (IO,9090) '  ! REACTR = ',REACTR,'  ! (DEF)'
      write (IO,9091) '  ! NINT =   ',NINT,  '  ! (DEF)'
      write (IO,9091) '  ! IVEG =   ',IVEG,  '  ! (DEF)'
      write (IO,*)
      write (IO,*) '!END!'
9090  format (1x,a13,f5.1,a9)
9091  format (1x,a13,i1,a9)
      write (IO,*)

      write (IO,*) linebreak
      write (IO,*) 'INPUT GROUP:  10'
      if (iwet.eq.1) then
        write (IO,*) '  ! ',POLLUT,' = ',AINPGRP10,'  !'
      else
      end if
      write (IO,*)
      write (IO,*) '!END!'
      write (IO,*)

      write (IO,*) linebreak
      write (IO,*) 'INPUT GROUP:  11'
      write (IO,9110) '  ! MOZ =    ',MOZ,    '  ! (DEF)'
      write (IO,9111) '  ! BCKO3 =  ',BCKO3,  '  ! (DEF)'
      write (IO,9111) '  ! BCKNH3 = ',BCKNH3, '  ! (DEF)'
      write (IO,9111) '  ! RNITE1 = ',RNITE1, '  ! (DEF)'
      write (IO,9111) '  ! RNITE2 = ',RNITE2, '  ! (DEF)'
      write (IO,9111) '  ! RNITE3 = ',RNITE3, '  ! (DEF)'
      write (IO,*)
      write (IO,*) '!END!'
9110  format (1x,a13,i5,a9)
9111  format (1x,a13,f5.1,a9)
      write (IO,*)

      write (IO,*) linebreak
      write (IO,*) 'INPUT GROUP:  12'
      write (IO,9120) '  ! SYTDEP =   ',SYTDEP,     '  ! (DEF)'
      write (IO,9121) '  ! MHFTSZ =   ',MHFTSZ,     '  ! (DEF)'
      write (IO,9121) '  ! JSUP =     ',JSUP,       '  ! (DEF)'
      write (IO,9120) '  ! CONK1 =    ',CONK1,      '  ! (DEF)'
      write (IO,9120) '  ! CONK2 =    ',CONK2,      '  ! (DEF)'
      write (IO,9120) '  ! TBD =      ',TBD,        '  ! (DEF)'
      write (IO,9121) '  ! IURB1 =    ',IURB1,      '  ! (DEF)'
      write (IO,9121) '  ! IURB2 =    ',IURB2,      '  ! (DEF)'
      write (IO,9121) '  ! ILANDUIN = ',ILANDUIN,   '  !      '
      write (IO,9120) '  ! Z0IN =     ',Z0IN,       '  ! (DEF)'
      write (IO,9120) '  ! XLAIIN =   ',XLAIIN,     '  ! (DEF)'
      write (IO,9120) '  ! ANEMHT =   ',ANEMHT,     '  !      '
      write (IO,9121) '  ! ISIGMAV =  ',ISIGMAV,    '  ! (DEF)'
      write (IO,9121) '  ! IMIXCTDM = ',IMIXCTDM,   '  ! (DEF)'
      write (IO,9120) '  ! XMXLEN =   ',XMXLEN,     '  ! (DEF)'
      write (IO,9120) '  ! XSAMLEN =  ',XSAMLEN,    '  ! (DEF)'
      write (IO,9121) '  ! MXNEW =    ',MXNEW,      '  ! (DEF)'
      write (IO,9121) '  ! MXSAM =    ',MXSAM,      '  ! (DEF)'
      write (IO,9120) '  ! SYMIN =    ',SYMIN,      '  ! (DEF)'
      write (IO,9120) '  ! SZMIN =    ',SZMINc,     '  ! (DEF)'

c      write (IO,9120) '  ! SVMIN =    ',SVMIN,      '  ! (DEF)'
c      write (IO,9120) '  ! SWMIN =    ',SWMIN,      '  ! (DEF)'

      write (IO,9120) '  ! WSCALM =   ',WSCALM,     '  ! (DEF)'
      write (IO,9120) '  ! XMAXZI =   ',XMAXZI,     '  ! (DEF)'
      write (IO,9120) '  ! XMINZI =   ',XMINZI,     '  ! (DEF)'
      write (IO,9126) '  ! WSCAT =    ',UCAT,       '  !      '
      write (IO,9122) '  ! PLX0 =     ',plx0,       '  !      '
      write (IO,9123) '  ! PTG0 =     ',ptg0(5),ptg0(6),'  !      '
      write (IO,9124) '  ! PPC =      ',appc,       '  ! (DEF)'
      write (IO,9120) '  ! SL2PF =    ',SL2PF,      '  ! (DEF)'
      write (IO,9121) '  ! NSPLIT =   ',NSPLIT,     '  ! (DEF)'
      write (IO,9125) '  ! IRESPLIT = ',IRESPLIT,   '  ! (DEF)'
      write (IO,9120) '  ! ZISPLIT =  ',ZISPLIT,    '  ! (DEF)'
      write (IO,9120) '  ! ROLDMAX =  ',ROLDMAX,    '  ! (DEF)'

      write (IO,*)
      write (IO,*) '!END!'
9120  format (1x,a15,f7.2,a9)
9121  format (1x,a15,i7,a9)
9122  format (1x,a15,5(f7.2,', '),f7.2,a9)
9123  format (1x,a15,f7.3,', ',f7.3,a9)
9124  format (1x,a15,a35,a9)
9125  format (1x,a15,23(i1,', '),i1,a9)
9126  format (1x,a15,4(f7.2,', '),f7.2,a9)
      write (IO,*)

      write (IO,*) linebreak
      write (IO,*) 'INPUT GROUP:  13'
      write (IO,9130) '  ! NPT1 = ',npt1, '  !      '
      write (IO,9130) '  ! IPTU = ',iptu, '  ! (DEF)'
      write (IO,9130) '  ! NSPT1= ',nspt1, '  !      '
      write (IO,9130) '  ! NPT2 = ',NPT2, '  ! (DEF)'
      write (IO,*)
      write (IO,*) '!END!'
      write (IO,*)
      write (IO,*) 'Subgroup (13b)'
      do i=1,npt1
        write (IO,9133) i,'  ! SRCNAM = ',cpid(i),'   ! '
c-emi **** EMI MODIFICATION   9/5/97   ****
c-emi   Allow the emissions to be written in scientific notation for very small
c-emi   and very large values.
c-emi   write (IO,9131) i,'  ! X = ',cpxs(i),cpys(i),cphs(i),cpzs(i),
c-emi&                  cpds(i),cpvs(i),cpts(i),dw(i),cpqs(i),
c-emi&                  ' !  !END!'
        if(cpqs(i).GT.0.1 .AND. cpqs(i).LT.9999.)then
          write (IO,9131) i,'  ! X = ',cpxs(i),cpys(i),cphs(i),cpzs(i),
     &                  cpds(i),cpvs(i),cpts(i),dw(i),cpqs(i),
     &                  ' !  !END!'
        else
          write (IO,9139) i,'  ! X = ',cpxs(i),cpys(i),cphs(i),cpzs(i),
     &                  cpds(i),cpvs(i),cpts(i),dw(i),cpqs(i),
     &                  ' !  !END!'
        endif
c-emi **** End of EMI MODIFICATION   9/5/97   ****
      end do
      write (IO,*)
      write (IO,*) 'Subgroup (13c)'
      do i=1,npt1
        if (ldw(i).AND.nsec.eq.36) then
          write (IO,9133) i,'  ! SRCNAM = ',cpid(i),'   ! '
c-emi **** EMI MODIFICATION   9/5/97   ****
c-emi   Switched the output order so that the heights are written before
c-emi   the widths following BPIP output convention.
          write (IO,9132) i,'  !  HEIGHT = ',(ADSBH(j,i),j=1,nsec),
     &                    ' !'
          write (IO,9132) i,'  !  WIDTH =  ',(ADSBW(j,i),j=1,nsec),
     &                    ' !'
c-emi **** End of EMI MODIFICATION   9/5/97   ****
c
c ---     PRIME building data
          if(mbdw.EQ.2) then
          write (IO,9132) i,'  !  LENGTH = ',(ADSBL(j,i),j=1,nsec),
     &                    ' !'
          write (IO,9132) i,'  !  XBADJ =  ',(ADSBX(j,i),j=1,nsec),
     &                    ' !'
          write (IO,9132) i,'  !  YBADJ =  ',(ADSBY(j,i),j=1,nsec),
     &                    ' !'
          endif

          write (IO,*)
          write (IO,*) '!END!'
        end if
      end do
      write (IO,*)
      write (IO,*) 'Subgroup (13d)'
      do i=1,npt1
        if (inqfp(i).ne.0) then
          write (IO,9133) i,'  ! SRCNAM = ',cpid(i),'   ! '
          write (IO,9134) i,'  ! IVARY  = ',ipvary(i),'   ! '
          if (ipvary(i).eq.1) then
            write (IO,9135) i,'  ! ',pollut,' = ',(cpfact(i,j),
     &            j=1,24),'   ! '
          else if (ipvary(i).eq.2) then
            write (IO,9136) i,'  ! ',pollut,' = ',(cpfact(i,j),
     &            j=1,12),'   ! '
          else if (ipvary(i).eq.3) then
            write (IO,9137) i,'  ! ',pollut,' = ',(cpfact(i,j),
     &            j=1,96),'   ! '
          else if (ipvary(i).eq.4) then
            write (IO,9138) i,'  ! ',pollut,' = ',(cpfact(i,j),
     &            j=1,36),'   ! '
          end if
          write (IO,'(a6)') ' !END!'
        end if
      end do

cvrt 9130  format (1x,a11,i2,a9)
cvrt 9131  format (1x,i2,a8,2(f10.4,', '),4(f7.2,', '),f6.1,', ',f3.1,
cvrt     &        ', ',f8.3,a9)
cvrt 9132  format (1x,i2,a14,5(6(f6.2,', '),/,17x),5(f6.2,', '),f6.2,a2)
cvrt 9133  format (1x,i2,a13,a8,a5)
cvrt 9134  format (1x,i2,a13,i2,a5)
cvrt 9135  format (1x,i2,a4,a6,a3,3(6(f6.2,', '),/,17x),5(f6.2,', '),f6.2,
cvrt     &            a5)
cvrt 9136  format (1x,i2,a4,a6,a3,1(6(f6.2,', '),/,17x),5(f6.2,', '),f6.2,
cvrt     &            a5)
cvrt 9137  format (1x,i2,a4,a6,a3,15(6(f6.2,', '),/,17x),5(f6.2,', '),f6.2,
cvrt     &            a5)
cvrt 9138  format (1x,i2,a4,a6,a3,5(6(f6.2,', '),/,17x),5(f6.2,', '),f6.2,
cvrt     &            a5)
9130  format (1x,a11,i8,a9)
9131  format (1x,i8,a8,2(f10.4,', '),4(f7.2,', '),f6.1,', ',f3.1,
     &        ', ',f8.3,a9)
9132  format (1x,i8,a14,5(6(f6.2,', '),/,17x),5(f6.2,', '),f6.2,a2)
9133  format (1x,i8,a13,a8,a5)
9134  format (1x,i8,a13,i2,a5)
9135  format (1x,i8,a4,a6,a3,3(6(f6.2,', '),/,17x),5(f6.2,', '),f6.2,
     &            a5)
9136  format (1x,i8,a4,a6,a3,1(6(f6.2,', '),/,17x),5(f6.2,', '),f6.2,
     &            a5)
9137  format (1x,i8,a4,a6,a3,15(6(f6.2,', '),/,17x),5(f6.2,', '),f6.2,
     &            a5)
9138  format (1x,i8,a4,a6,a3,5(6(f6.2,', '),/,17x),5(f6.2,', '),f6.2,
     &            a5)
c-emi **** EMI MODIFICATION   9/5/97   ****
9139  format (1x,i8,a8,2(f10.4,', '),4(f7.2,', '),f6.1,', ',f3.1,
     &        ', ',e12.3,a9)
c-emi **** End of EMI MODIFICATION   9/5/97   ****
      write (IO,*)

      write (IO,*) linebreak
      write (IO,*) 'INPUT GROUP:  14'
      write (IO,9140) '  ! NAR1 = ',nar1, ' !      '
      write (IO,9140) '  ! IARU = ',iaru, ' ! (DEF)'
      write (IO,9140) '  ! NSAR1= ',nsar1, ' !      '
      write (IO,9140) '  ! NAR2 = ',NAR2, ' ! (DEF)'
      write (IO,*)
      write (IO,*) '!END!'
      write (IO,*)
      write (IO,*) 'Subgroup (14b)'
      do i=1,nar1
        write (IO,9133) i,'  ! SRCNAM = ',caid(i),'   ! '
c-emi **** EMI MODIFICATION   9/5/97   ****
c-emi   Allow the emissions to be written in scientific notation for very small
c-emi   and very large values.
c-emi   write (IO,9141) i,'  ! X = ',cahs(i),cazs(i),cazinit(i),caqs(i)
c-emi&                  ,' !  !END!'
        if(caqs(i).GT.0.1 .AND. caqs(i).LT.9999.)then
         write (IO,9141) i,'  ! X = ',cahs(i),cazs(i),cazinit(i),caqs(i)
     &                  ,' !  !END!'
        else
         write (IO,9143) i,'  ! X = ',cahs(i),cazs(i),cazinit(i),caqs(i)
     &                  ,' !  !END!'
        endif
c-emi **** End of EMI MODIFICATION   9/5/97   ****
      end do
      write (IO,*)
      write (IO,*) 'Subgroup (14c)'
      do i=1,nar1
        write (IO,9133) i,'  ! SRCNAM = ',caid(i),'   ! '
        write (IO,9142) i,'  ! XVERT = ',(caxvert(i,j),j=1,4),' !'
        write (IO,9142) i,'  ! YVERT = ',(cayvert(i,j),j=1,4),' !'
        write (IO,*) '!END!'
      end do
      write (IO,*)
      write (IO,*) 'Subgroup (14d)'
      do i=1,nar1
        if (inqfa(i).ne.0) then
          write (IO,9133) i,'  ! SRCNAM = ',caid(i),'   ! '
          write (IO,9134) i,'  ! IVARY  = ',iavary(i),'   ! '
          if (iavary(i).eq.1) then
            write (IO,9135) i,'  ! ',pollut,' = ',(cafact(i,j),
     &            j=1,24),'   ! '
          else if (iavary(i).eq.2) then
            write (IO,9136) i,'  ! ',pollut,' = ',(cafact(i,j),
     &            j=1,12),'   ! '
          else if (iavary(i).eq.3) then
            write (IO,9137) i,'  ! ',pollut,' = ',(cafact(i,j),
     &            j=1,96),'   ! '
          else if (iavary(i).eq.4) then
            write (IO,9138) i,'  ! ',pollut,' = ',(cafact(i,j),
     &            j=1,36),'   ! '
          end if
          write (IO,'(a6)') ' !END!'
        end if
      end do
cvrt 9140  format (1x,a11,i2,a9)
cvrt 9141  format (1x,i2,a8,3(f8.2,', '),e11.4,a9)
cvrt 9142  format (1x,i2,a12,3(f10.4,', '),f10.4,a2)
9140  format (1x,a11,i8,a9)
9142  format (1x,i8,a12,3(f10.4,', '),f10.4,a2)
c-emi **** EMI MODIFICATION   9/5/97   ****
c-emi 9141  format (1x,i8,a8,3(f8.2,', '),e11.4,a9)
9141  format (1x,i8,a8,3(f8.2,', '),f8.3,a9)
9143  format (1x,i8,a8,3(f8.2,', '),e12.3,a9)
c-emi **** End of EMI MODIFICATION   9/5/97   ****
      write (IO,*)

      write (IO,*) linebreak
      write (IO,*) 'INPUT GROUP:  15'
      write (IO,9150) '  ! NLINES =  ',NLINES,      ' !  (DEF)'
      write (IO,9150) '  ! ILNU =    ',ILNU,        ' !  (DEF)'
      write (IO,9150) '  ! NSLN1 =   ',NSLN1,       ' !  (DEF)'
      write (IO,9150) '  ! MXNSEG =  ',MXNSEG,      ' !  (DEF)'
      write (IO,9150) '  ! NLRISE =  ',NLRISE,      ' !  (DEF)'
      write (IO,9151) '  ! XL =      ',XL,          ' !  (DEF)'
      write (IO,9151) '  ! HBL =     ',HBL,         ' !  (DEF)'
      write (IO,9151) '  ! WBL =     ',WBL,         ' !  (DEF)'
      write (IO,9151) '  ! WML =     ',WML,         ' !  (DEF)'
      write (IO,9151) '  ! DXL =     ',DXL,         ' !  (DEF)'
      write (IO,9151) '  ! FPRIMEL = ',FPRIMEL,     ' !  (DEF)'
      write (IO,*)
      write (IO,*) '!END!'
9150  format (1x,a14,i7,a9)
9151  format (1x,a14,f7.2,a9)

      write (IO,*)

      write (IO,*) linebreak
      write (IO,*) 'INPUT GROUP:  16'
      write (IO,9160) '  ! NVL1=    ',nvl1,     ' !       '
      write (IO,9160) '  ! IVLU =   ',ivlu,     ' !  (DEF)'
      write (IO,9160) '  ! NSVL1=   ',nsvl1,    ' !       '
      write (IO,9160) '  ! NVL2=    ',nvl2,     ' !  (DEF)'
      write (IO,*)
      write (IO,*) '!END!'
      write (IO,*)
      write (IO,*) 'Subgroup (16b)'
      do i=1,nvl1
        write (IO,9133) i,'  ! SRCNAM = ',cvid(i),'   ! '
c-emi **** EMI MODIFICATION   9/5/97   ****
c-emi   Allow the emissions to be written in scientific notation for very small
c-emi   and very large values.
c-emi   write (IO,9162) i,'  ! X = ',cvxs(i),cvys(i),cvhs(i),cvzs(i),
c-emi&               cvyinit(i),cvzinit(i),cvqs(i),' !  !END!'
        if(cvqs(i).GT.0.1 .AND. cvqs(i).LT.9999.)then
          write (IO,9162) i,'  ! X = ',cvxs(i),cvys(i),cvhs(i),cvzs(i),
     &               cvyinit(i),cvzinit(i),cvqs(i),' !  !END!'
        else
          write (IO,9163) i,'  ! X = ',cvxs(i),cvys(i),cvhs(i),cvzs(i),
     &               cvyinit(i),cvzinit(i),cvqs(i),' !  !END!'
        endif
c-emi **** End of EMI MODIFICATION   9/5/97   ****
      end do
      write (IO,*)
      write (IO,*) 'Subgroup (16c)'
      do i=1,nvl1
        if (inqfv(i).ne.0) then
          write (IO,9133) i,'  ! SRCNAM = ',cvid(i),'   ! '
          write (IO,9134) i,'  ! IVARY  = ',ivvary(i),'   ! '
          if (ivvary(i).eq.1) then
            write (IO,9135) i,'  ! ',pollut,' = ',(cvfact(i,j),
     &            j=1,24),'   ! '
          else if (ivvary(i).eq.2) then
            write (IO,9136) i,'  ! ',pollut,' = ',(cvfact(i,j),
     &            j=1,12),'   ! '
          else if (ivvary(i).eq.3) then
            write (IO,9137) i,'  ! ',pollut,' = ',(cvfact(i,j),
     &            j=1,96),'   ! '
          else if (ivvary(i).eq.4) then
            write (IO,9138) i,'  ! ',pollut,' = ',(cvfact(i,j),
     &            j=1,36),'   ! '
          end if
          write (IO,'(a6)') ' !END!'
        end if
      end do
cvrt 9160  format (1x,a13,i2,a9)
9161  format (1x,a13,f6.1,a9)
cvrt 9162  format (1x,i2,a8,2(f10.4,', '),4(f7.2,', '),f8.4,a9)
9160  format (1x,a13,i8,a9)
9162  format (1x,i8,a8,2(f10.4,', '),4(f7.2,', '),f8.4,a9)
c-emi **** EMI MODIFICATION   9/5/97   ****
9163  format (1x,i8,a8,2(f10.4,', '),4(f7.2,', '),e12.3,a9)
c-emi **** End of EMI MODIFICATION   9/5/97   ****
      write (IO,*)

      write (IO,*) linebreak
      write (IO,*) 'INPUT GROUP:  17'
      write (IO,9170) '  ! NREC = ', numrec,' !'
      write (IO,*)
      write (IO,*) '!END!'
      do i=1,numrec
        write (IO,9171) i,'  ! X = ',(AXR(i)/1000.),(AYR(i)/1000.),
     &                  AZELEV(i),' !  !END!'
      end do
9170  format (1x,a11,i5,a2)
9171  format (1x,i8,a8,2(f10.4,', '),f9.3,a9)
      write (IO,*)

      return
      end

c----------------------------------------------------------------------
      subroutine grday(iyr,ijul,imo,iday)
c----------------------------------------------------------------------
c
c --- ISC2PUF    Version:  1.0     Level:  960226                 GRDAY
c                J. Scire, SRC
c
c --- PURPOSE:  Compute the Gregorian date (month, day) from the
c               Julian day
c
c --- INPUTS:
c           IYR - integer      - Year
c          IJUL - integer      - Julian day
c     Parameters: IO6
c
c --- OUTPUT:
c           IMO - integer      - Month
c          IDAY - integer      - Day
c
c --- GRDAY called by:  CONV2CAL
c --- GRDAY calls:      none
c----------------------------------------------------------------------
c
c
      integer kday(12,2)
      data kday/31,59,90,120,151,181,212,243,273,304,334,365,
     1          31,60,91,121,152,182,213,244,274,305,335,366/
c
c
      ileap=1
      if(mod(iyr,4).eq.0)ileap=2
      if(ijul.lt.1.or.ijul.gt.kday(12,ileap))go to 11
c
      do 10 i=1,12
      if(ijul.gt.kday(i,ileap))go to 10
      imo=i
      iday=ijul
      if(imo.ne.1)iday=ijul-kday(imo-1,ileap)
      return
10    continue
c
11    continue
      write(*,12)iyr,ijul
12    format(//2x,'ERROR in SUBR. GRDAY -- invalid Julian day '//2x,
     1 'iyr = ',i5,3x,'ijul = ',i5)
      stop
      end

