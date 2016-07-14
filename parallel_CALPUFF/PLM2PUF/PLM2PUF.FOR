      PROGRAM PLM2PUF
c******************************************************************************
c
c     PLM2PUF - Conversion Program for AUSPLUME to AUSPUFF
c           input files.
c
c     PURPOSE: Converts AUSPLUME variables to appropriate AUSPUFF variables 
c
c     NOTES:  Many of the AUSPUFF variables are not directly compatible with 
c             the AUSPLUME variables.  The user should manually check the created
c             AUSPUFF input file and edit the values that are necessary.  The 
c             hardwired values are denoted by "(DEF)".
c
c     UPDATE:
c --- 960625 to 980731 (DGS):  Remove SVMIN,SWMIN from output so that
c                              AUSPUFF default arrays are used;
c                              Write emissions factors in E-format;
c                              Restore older specification for odor
c                              emission units;
c                              Use default filenames for versions 1,2;
c                              Reset WSCALM,SYMIN,SZMIN,ISIGMAV default;
c                              Set downwash flag to 0 if BW,BH are zero
c
c     CALLED BY: NONE
c     CALLS:     READPLM,CONV2CAL
c
c     PROGRAMMER: Vincent Tino
c
c     ORIGINAL DATE: June 25, 1996
c
c******************************************************************************

      character*70 inpfile,outfile
      

      call GETCOM('PLM2PUF ',inpfile,outfile)
      OPEN(UNIT=10,FILE=inpfile,STATUS='OLD')
      OPEN(UNIT=11,FILE=outfile,STATUS='UNKNOWN')

      write (*,9000)
9000  format (15x,'***********************************************',/
     &       ,15x,'*                                             *',/ 
     &       ,15x,'*                  PLM2PUF                    *',/ 
     &       ,15x,'*                                             *',/ 
     &       ,15x,'*        AUSPLUME to AUSPUFF INPUT FILE       *',/ 
     &       ,15x,'*              CONVERSION UTILITY             *',/ 
     &       ,15x,'*                                             *',/ 
     &       ,15x,'*         Level: 1.0    Version: 980731       *',/ 
     &       ,15x,'*                                             *',/
     &       ,15x,'***********************************************',//)

C--- READ IN AUSPLUME VARIABLES
      CALL READPLM

C--- CONVERT AUSPLUME VARIABLES TO AUSPUFF AND CREATE AUSPUFF.INP
      CALL CONV2CAL

      WRITE(*,9113)
 9113 FORMAT(/15X,'***********************************************',/
     &       ,15X,'******** PLM2PUF Finishes Successfully ********',/
     &       ,15x,'***********************************************',/)

      CLOSE(10)
      CLOSE(11)

      STOP
      END      
      SUBROUTINE GETCOM (MODEL,INPFIL,OUTFIL)
C***********************************************************************
C
C        ADAPTED FROM PCCODE Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Controls Retrieving Input and Output File Names From
C                 the Command Line for PCs
C
C        PROGRAMMER: Roger Brode
C
C        DATE:    March 2, 1992
C
C        MODIFIED:   Jayant Hardikar, PES, Inc.
C                    - Length of command line for Lahey version changed
C                      from 80 to 120 characters - 4/19/93
C                    - Adapted for DEPMET/PMERGE - 7/29/94
C        MODIFIED:   D.G. Strimaitis, EARTH TECH
C                    - Length of command line increased to 130, although
C                      Lahey function takes only 127 characters,
C                      and pathnames increased to 70    - 5/96
C        MODIFIED:   D.G. Strimaitis, EARTH TECH
C                    - Remove MACRO constructs for compiler  - 8/02
C
C        INPUTS:  Command Line
C
C        OUTPUTS: Input Runstream File Name
C                 Output Print File Name
C
C        CALLED FROM:   MAIN
C***********************************************************************
C
C     Variable Declarations
c-DGS- Increase size of in/out filenames to match MENU limits (70)
      CHARACTER*70 INPFIL,OUTFIL
c     CHARACTER*40 INPFIL,OUTFIL
      CHARACTER*8 MODEL
c$IF DEFINED (MICRO)
cC     Declare 2-Byte Integer for Field Number of Command Line Argument
c      INTEGER*2 IARG
c$ELSEIF DEFINED (LAHEY)
C     Declare the COMLIN Variable to Hold Contents of Command Line for Lahey
      CHARACTER*130 COMLIN
      INTEGER*2 LOCB(10),LOCE(10)
      LOGICAL INFLD
c$ENDIF

c$IF DEFINED (MICRO)
cC************************************************************MICRO START
cC     Use Microsoft Functions NARGS and GETARG To Retrieve
cC     Contents of Command Line
c      IFCNT = NARGS()
cC     IFCNT Is The Number Of Arguments on Command Line Including Program
c      IF (IFCNT .NE. 3) THEN
cC        Error on Command Line.  Write Error Message and STOP
c         WRITE(*,660) MODEL
c         STOP
c      ELSE
cC        Retrieve First Argument as Input File Name
c         IARG = 1
c         CALL GETARG(IARG,INPFIL,ISTAT)
cC        Retrieve Second Argument as Output File Name
c         IARG = 2
c         CALL GETARG(IARG,OUTFIL,ISTAT)
c      END IF
cC************************************************************MICRO STOP

c$ELSEIF DEFINED (LAHEY)
C************************************************************LAHEY START
C     Use Lahey Function GETCL To Retrieve Contents of Command Line.
C     Retrieve Input and Output File Names From the COMLIN Variable.
      CALL GETCL(COMLIN)

      INFLD = .FALSE.
      IFCNT = 0
      DO 100 I = 1, 130
         IF (.NOT.INFLD .AND. COMLIN(I:I) .NE. ' ') THEN
            INFLD = .TRUE.
            IFCNT = IFCNT + 1
            LOCB(IFCNT) = I
         ELSE IF (INFLD .AND. COMLIN(I:I) .EQ. ' ') THEN
            INFLD = .FALSE.
            LOCE(IFCNT) = I - 1
         END IF
 100  CONTINUE
      IF (IFCNT .NE. 2) THEN
C        Error on Command Line.  Write Error Message and STOP
         WRITE(*,660) MODEL
         STOP
      END IF
      INPFIL = COMLIN(LOCB(1):LOCE(1))
      OUTFIL = COMLIN(LOCB(2):LOCE(2))
C************************************************************LAHEY STOP

c$ENDIF

  660 FORMAT (' COMMAND LINE ERROR: ',A8,' input_file output_file')

      RETURN
      END

      SUBROUTINE READPLM
c******************************************************************************
c
c     READPLM Module of PLM2PUF - Conversion Program for AUSPLUME to AUSPUFF
c           input files.
c
c     PURPOSE: Reads AUSPLUME file 
c
c     CALLED BY: PLM2PUF
c     CALLS:     NONE
c
c     PROGRAMMER: Vincent Tino
c
c     ORIGINAL DATE: June 25, 1996
C
C        MODIFIED:   D.G. Strimaitis, EARTH TECH
C                    - Declare UNIT1
c
c******************************************************************************
      integer UNIT1
      PARAMETER (UNIT1=10)

      PARAMETER (MXSRCS=100,MXGROUPS=11,MXRATES=96,MXNV=20,
     &      MXREC=1000,MXGRID=100)

      CHARACTER TITLE(240)
      CHARACTER CGROUP(MXSRCS,MXGROUPS)*6,
     &      SRC_ID(MXSRCS)*6,QUNITS(16),CUNITS(32)
      CHARACTER*80 RESFIL,METFIL,TERFIL,PLTFIL,FREFIL,SAVFIL
      REAL VER,SRCFIELD(MXSRCS,10),SRCRATES(MXSRCS,MXRATES),
     &      SRCV(MXSRCS,MXNV,3),REC(MXREC),GRIDX(MXGRID),GRIDY(MXGRID),
     &      BETA(2),DTHDZI,ZREF,SIGTIM,BCKGRD,Z0,DECAY,Z0MAST,P(6,6),
     &      DTHDZ(6,6),SPIRAL(6),EGAN(6),UCATS(5)
      INTEGER TERAIN,NGRPS,NSRCS,SRCNUM(MXSRCS),IGROUP(MXGROUPS),
     &      NRATES(MXSRCS),NVS,NXA,NYA,IRANKCON,CURVE(4)
      LOGICAL LCARTD,LCARTG,L3MTH,L90DAY,LMEAN,LAVHRS(9),LCONC,LSTMET,
     &      PRTALL,LTOPN,LPLOT1,LMAX50,LSAVE,IGNORP,IGNORT,GRDRIS,
     &      DNWASH,PENET,LBUOYY,LBUOYZ,LZ0Y,LZ0Z,SMOOTH,LWAKE,LPLOT2

      COMMON /C1/TITLE,SRC_ID,QUNITS,CUNITS
      COMMON /L1/DNWASH,LCARTD,LCARTG,LCONC,PENET,GRDRIS,LWAKE,
     &           LZ0Y,LZ0Z,IGNORP
      COMMON /R1/VER,P,DTHDZ,ZREF,SRCRATES,SRCFIELD,GRIDX,GRIDY,
     &           REC,Z0,Z0MAST,EGAN,UCATS
      COMMON /I1/NRATES,NSRCS,SRCNUM,NXA,NYA,NDISCR,ISHR,NMINS,NGRPS,
     &           CURVE,IWPROF
      COMMON /F1/METFIL,RESFIL


      READ(UNIT1,449)VER
 449  FORMAT(F5.0)

      READ(UNIT1,450)ITITLE,(TITLE(i),i=1,ITITLE)
 450  FORMAT(I4,1X,240A1)

C  Source ID's may be alpha-numeric with
C  up to 6 characters and/or numerals.
      READ(UNIT1,451)TERAIN,NGRPS,NSRCS
 451  FORMAT(1X,I1,I4,I4)
      DO J=1,NGRPS
        READ(UNIT1,452)IGROUP(J),(CGROUP(I,J),I=1,IGROUP(J))
 452  FORMAT(I8,(11A6))
      END DO  

C  Read source data.
      DO I=1,NSRCS
        READ(UNIT1,453)SRC_ID(I),SRCNUM(I),(SRCFIELD(I,J),J=1,9),
     &      NRATES(I),NVS,(SRCRATES(I,J),J=1,NRATES(I))
 453  FORMAT(A6,I2,5E14.0/E16.0,3E14.0,2I3,E14.0/(E16.0,4E14.0))
        READ(UNIT1,454)((SRCV(I,J,L),L=1,3),J=1,NVS)
 454  FORMAT((2(F9.0,2E14.0)))
      END DO

C  Read receptor locations.

C     Discrete receptors
      READ(UNIT1,'(L2,I5)')LCARTD,JCOUNT
      BACKSPACE(UNIT1)
      IEND=MIN(JCOUNT,6)
      READ(UNIT1,455)LCARTD,JCOUNT,(REC(I+1),I=0,IEND)
 455  FORMAT(L2,I5,3X,7F10.0)
      LINES=(JCOUNT+2)/8
      DO L=1,LINES
        ISTART=(L-1)*8+7
        IEND=MIN((ISTART+7),JCOUNT)
        READ(UNIT1,465)(REC(I+1),I=ISTART,IEND)
 465  FORMAT(8F10.0)
      END DO
      JCOUNT=JCOUNT+1
CV      IF(ICOUNT+JCOUNT.GT.ARRSIZ)GOTO 99
      NDISCR=JCOUNT/2

C     X grid values (or RADII if LCARTG=f)
      READ(UNIT1,'(L2,I5)')LCARTG,NXA
      BACKSPACE(UNIT1)
      IEND=MIN(NXA,7)
      READ(UNIT1,455)LCARTG,NXA,(GRIDX(I),I=1,IEND)
      LINES=(NXA+0.1)/8
      DO L=1,LINES
        ISTART=(L-1)*8+8
        IEND=MIN((ISTART+7),NXA)
        READ(UNIT1,465)(GRIDX(I),I=ISTART,IEND)
      END DO

C     Y grid values (or BEARINGS if LCARTG=f)
      READ(UNIT1,'(L2,I5)')LCARTG,NYA
      BACKSPACE(UNIT1)
      IEND=MIN(NYA,7)
      READ(UNIT1,455)LCARTG,NYA,(GRIDY(I),I=1,IEND)
      LINES=(NYA+0.1)/8
      DO L=1,LINES
        ISTART=(L-1)*8+8
        IEND=MIN((ISTART+7),NYA)
        READ(UNIT1,465)(GRIDY(I),I=ISTART,IEND)
      END DO
      IF(NXA.GT.MXGRID.OR.NYA.GT.MXGRID)STOP'*** MXGRID too small.'


C     Read all remaining parameters.
      READ(UNIT1,456)LEN2,QUNITS,LEN3,CUNITS,CONFAC,NMINS
 456  FORMAT(I3,16A1,I3,32A1,E14.6,I3)
      IF(VER.LT.1.6)THEN
      READ(UNIT1,457)L3MTH,L90DAY,LMEAN,LAVHRS,LCONC,LSTMET,PRTALL,
     *    LTOPN,LMAX50,LSAVE,IGNORP,IGNORT,GRDRIS,DNWASH,PENET,
     *    LBUOYY,LBUOYZ,LZ0Y,LZ0Z,SMOOTH,LWAKE,CURVE,IWPROF,ISHR,MODE
 457    FORMAT(1X,29L1,7I2)
      ELSE
      READ(UNIT1,458)L3MTH,L90DAY,LMEAN,LAVHRS,LCONC,LSTMET,PRTALL,
     *    LTOPN,LPLOT1,LMAX50,LSAVE,IGNORP,IGNORT,GRDRIS,DNWASH,
     *    PENET,LBUOYY,LBUOYZ,LZ0Y,LZ0Z,SMOOTH,LWAKE,CURVE,IWPROF,ISHR,
     *    MODE
 458    FORMAT(1X,30L1,4X,7I2)
      IF(NMINS.NE.0)THEN
        L3MTH=.FALSE.
        L90DAY=.FALSE.
        LMEAN=.FALSE.
        DO I=1,9
          LAVHRS(I)=.FALSE.
        END DO
      END IF
      IF(LPLOT1) IRANKCON=1
      LPLOT2=.FALSE.
      PLOTFREQ=-1
      ENDIF
      READ(UNIT1,459)BETA,DTHDZI,ZREF,SIGTIM,BCKGRD,Z0,DECAY,Z0MAST,P,
     *  DTHDZ,SPIRAL,EGAN,UCATS
 459  FORMAT(2F6.0,F8.0,F5.0,F4.0,E14.0,F6.0,E14.0,F6.0/(6F11.0))
      IF(Z0MAST.LE.0)Z0MAST=0.03

C     Read the additional information required by ver.2 contour data output.
      IF(VER.GE.2)THEN
        READ(UNIT1,461)LPLOT2,IRANKCON,PLOTFREQ
 461    FORMAT(L1,1X,I3,1X,1PE13.3)
      ENDIF

C    GUI modification version 3.0
C    Read in the names for the input and requested output files.
      IF(VER.GE.3.0)THEN
        READ(UNIT1,'()')
        READ(UNIT1,462)RESFIL
        READ(UNIT1,462)METFIL
        IF(TERAIN.NE.0) then
            READ(UNIT1,462)TERFIL
            write (*,1000)
        end if
        IF(LPLOT1) THEN
            READ(UNIT1,462)PLTFIL
            WRITE (*,1001)
        END IF
        IF(LPLOT2) THEN
            READ(UNIT1,462)FREFIL
            WRITE (*,1002)
        END IF
        IF(LSAVE) THEN
            READ(UNIT1,462)SAVFIL
            WRITE (*,1003)
        END IF
462     FORMAT(A80)
1000  FORMAT(' WARNING: Terrain specified in terrain file! ',/,
     &        '  Information NOT converted!')
1001  FORMAT(' WARNING: Concentration plot file name specified in CFG '
     &        ,'file!',/,'  Information NOT converted!')
1002  FORMAT(' WARNING: Frequency plot filename specified in CFG file! '
     &        ,/,'  Information NOT converted!')
1003  FORMAT(' WARNING: Save filename specified in CFG file! '
     &        ,/,'  Information NOT converted!')
      ENDIF

C  Count up the number of averaging intervals between 1 hour and 7 days.
      NAVHRS=0
      DO 3 I=1,9
        IF(LAVHRS(I))NAVHRS=NAVHRS+1
   3  CONTINUE

      CLOSE (UNIT1)

      RETURN
      END
      Subroutine CONV2CAL

c******************************************************************************
c
c     CONV2CAL Module of PLM2PUF - Conversion Program for AUSPLUME to AUSPUFF
c           input files.
c
c     PURPOSE: Converts AUSPLUME variables to appropriate AUSPUFF variables
c
c     NOTES:  Many of the AUSPUFF variables are not directly compatible with
c             the AUSPLUME variables.  The user should manually check the created
c             AUSPUFF input file and edit the values that are necessary.  The
c             hardwired values are denoted by "(DEF)".
c
c     UPDATE:
c --- 960625 to 980731 (DGS):  Remove SVMIN,SWMIN from output so that
c                              AUSPUFF default arrays are used; 
c                              Write emissions factors in E-format;
c                              Restore older specification for odor
c                              emission units;
c                              Use default filenames for versions 1,2;
c                              Reset WSCALM,SYMIN,SZMIN,ISIGMAV default;
c                              Set downwash flag to 0 if BW,BH are zero
c
c     CALLED BY: PLM2PUF
c     CALLS:     NONE
c
c     PROGRAMMER: Vincent Tino
c
c     ORIGINAL DATE: June 25, 1996
c
c******************************************************************************

C***  VARIABLE DECLARATIONS

      PARAMETER (IO=11)
      PARAMETER (MXSRCS=100,MXGROUPS=11,MXRATES=96,MXNV=20,
     &      MXREC=1000,MXGRID=100,NUMUNITS=14)

      CHARACTER TITLE(240),SRC_ID(MXSRCS)*6
      CHARACTER QUNITS(16),CUNITS(32)
      CHARACTER*80 METFIL,RESFIL
      REAL VER,SRCFIELD(MXSRCS,10),SRCRATES(MXSRCS,MXRATES),
     &      REC(MXREC),GRIDX(MXGRID),GRIDY(MXGRID),
     &      ZREF,Z0,Z0MAST,P(6,6),EGAN(6),UCATS(5),
     &      DTHDZ(6,6)
      INTEGER NSRCS,SRCNUM(MXSRCS),NGRPS,CURVE(4),
     &      NRATES(MXSRCS),NXA,NYA
      LOGICAL LCARTD,LCARTG,LCONC,
     &      GRDRIS,LWAKE,
     &      DNWASH,PENET,LZ0Y,LZ0Z,IGNORP

      COMMON /C1/TITLE,SRC_ID,QUNITS,CUNITS
      COMMON /L1/DNWASH,LCARTD,LCARTG,LCONC,PENET,GRDRIS,LWAKE,
     &           LZ0Y,LZ0Z,IGNORP
      COMMON /R1/VER,P,DTHDZ,ZREF,SRCRATES,SRCFIELD,GRIDX,GRIDY,
     &           REC,Z0,Z0MAST,EGAN,UCATS
      COMMON /I1/NRATES,NSRCS,SRCNUM,NXA,NYA,NDISCR,ISHR,NMINS,NGRPS,
     &           CURVE,IWPROF
      COMMON /F1/METFIL,RESFIL


C***  LOCAL VARIABLES
      CHARACTER POLLUT*10,linebreak(50)
      CHARACTER*80 metdat,iscdat,plmdat,prfdat,sfcdat,puflst,
     &      condat,dfdat,wfdat,visdat,ptdat,voldat,ardat,
     &      ozdat,vddat,chemdat,hildat,rctdat,debugfile

      LOGICAL LCFILES,LSAMP,LCOMPRS,LDEBUG
      REAL cpxs(MXSRCS),cpys(MXSRCS), cphs(MXSRCS), cpzs(MXSRCS),
     &     cpds(MXSRCS), cpvs(MXSRCS), cpts(MXSRCS), cpqs(MXSRCS),
     &     cvxs(MXSRCS), cvys(MXSRCS), cvzs(MXSRCS), cvqs(MXSRCS),
     &     cvhs(MXSRCS), cvyinit(MXSRCS), cvzinit(MXSRCS),
     &     cazs(MXSRCS), caqs(MXSRCS), dw(MXSRCS),
     &     cahs(MXSRCS), cazinit(MXSRCS), caxvert(MXSRCS,4),
     &     cayvert(MXSRCS,4),adsbw(MXSRCS,36),adsbh(MXSRCS,36),
     &     AXR(MXREC),AYR(MXREC),AZELEV(MXREC)
      REAL XORIGKM,YORIGKM,XGRIDKM,YGRIDKM,DTR
      REAL WSCAT(5),plx0(6), ptg0(6), PPC(6)
      INTEGER IRESPLIT(24), METRUN
      CHARACTER*6 CPID(MXSRCS),CAID(MXSRCS),CVID(MXSRCS)
      INTEGER IPVARY(MXSRCS),IAVARY(MXSRCS),IVVARY(MXSRCS)
      REAL CPFAC(MXSRCS,MXRATES),CAFAC(MXSRCS,MXRATES),
     &      CVFAC(MXSRCS,MXRATES)
      LOGICAL LPFAC(MXSRCS),LAFAC(MXSRCS),LVFAC(MXSRCS),FOUND
      INTEGER NSPT1,NSAR1,NSLN1,NSVL1
      CHARACTER*12 QUTEST(NUMUNITS)

C--- GENERAL DATA FOR CONVERSION PROCESSING

      DATA  XORIGM/ 99999999./,YORIGM/ 99999999/,
     &      XGRIDM/-99999999./,YGRIDM/-99999999./,
     &      pi/3.1415927/,linebreak/50*'-'/

c-- DATA FOR EMISSIONS UNITS TESTING.  MUST BE 16 CHARACTERS LONG
      DATA  QUTEST/'grams/second    ','kg/hour         ',
     &             'tonnes/year     ','OUV/second      ',
     &             'OUV/min         ','OU m**3/s       ',
     &             'OU m**3/min     ',
     &             'GRAMS/SECOND    ','KG/HOUR         ',
     &             'TONNES/YEAR     ','OUV/SECOND      ',
     &             'OUV/MIN         ','OU M**3/S       ',
     &             'OU M**3/MIN     '/

C***  SET AUSPUFF DEFAULTS
      DATA metdat/'AUSMET.DAT'/,iscdat/'ISCMET.DAT'/,
     &      plmdat/'PLMMET.DAT'/,prfdat/'PROFILE.DAT'/,
     &      sfcdat/'SURFACE.DAT'/,puflst/'AUSPUFF.LST'/,
     &      condat/'CONC.DAT'/,dfdat/'DFLX.DAT'/,wfdat/'WFLX.DAT'/,
     &      visdat/'VISB.DAT'/,ptdat/'PTEMARB.DAT'/,
     &      voldat/'VOLEM.DAT'/,ardat/'BAEMARB.DAT'/,
     &      ozdat/'OZONE.DAT'/,vddat/'VD.DAT'/,chemdat/'CHEM.DAT'/,
     &      hildat/'HILL.DAT'/,rctdat/'HILLRCT.DAT'/,
     &      debugfile/'DEBUG.DAT'/
     &      LCFILES/.TRUE./

      DATA METRUN/1/,IBYR/00/,IBMO/01/,IBDY/01/,IBHR/00/,IRLG/8760/,
     &       NSPEC/1/,NSE/1/,ITEST/2/,METFM/3/,AVET/60.0/

      DATA MGAUSS/1/,MCTADJ/1/,MCTSG/0/,MSLUG/0/,MTRANS/1/,MTIP/1/,
     &       MSHEAR/0/,MSPLIT/0/,MCHEM/0/,MWET/1/,MDRY/0/,MDISP/3/,
     &       MTURBVW/0/,MDISP2/4/,
     &       MROUGH/0/,MPARTL/0/,MREG/0/

      DATA POLLUT/'OTHER'/,IDDEPTYP/0/

      DATA NX/2/,NY/2/,NZ/1/,DGRIDKM/1.0/,XORIGKM/0./,
     &     YORIGKM/0./,IUTMZN/0/,XLAT/45.0/,XLONG/90.0/,XTZ/6/,
     &       IBCOMP/1/,IECOMP/2/,JBCOMP/1/,JECOMP/2/,LSAMP/.FALSE./,
     &       IBSAMP/0/,JBSAMP/0/,IESAMP/0/,JESAMP/0/,MESHDN/1/

      DATA ICON/1/,IDRY/1/,IWET/1/,IVIS/0/,LCOMPRS/.TRUE./,ICPRT/0/,
     &       IDPRT/0/,IWPRT/0/,ICFRQ/1/,IDFRQ/1/,IWFRQ/1/,IPRTU/1/,
     &       IMESG/1/,LDEBUG/.FALSE./,NPFDEB/1/,NN1/1/,NN2/10/

      DATA NHILL/0/,NCTREC/0/,MHILL/0/,XHILL2M/1.0/,ZHILL2M/1.0/,
     &       XCTDMKM/0/,YCTDMKM/0/

      DATA RCUTR/30.0/,RGR/5.0/,REACTR/8.0/,NINT/9./,IVEG/1/

      DATA MOZ/0/,BCKO3/80.0/,BCKNH3/10.0/,RNITE1/0.2/,RNITE2/2.0/,
     &       RNITE3/2.0/

      DATA SYTDEP/9999.0/,MHFTSZ/0/,JSUP/5/,CONK1/0.01/,CONK2/0.1/,
     &       TBD/0.5/,IURB1/10/,IURB2/19/,ILANDUIN/20/,Z0IN/0.25/,
     &       XLAIIN/3.0/,ANEMHT/10.0/,ISIGMAV/1/,IMIXCTDM/0/,
     &       XMXLEN/5.0/,XSAMLEN/5.0/,MXNEW/99/,
     &       MXSAM/5/,SYMIN/1.0/,SZMINc/1.0/,SVMIN/0.5/,SWMIN/0.016/,
     &       WSCALM/0.5/,XMAXZI/3000.0/,XMINZI/20.0/,
     &       WSCAT/1.54,3.09,5.14,8.23,10.8/,
     &       PLX0/0.07,0.07,0.1,0.15,0.35,0.55/,
     &       PTG0/0.0,0.0,0.0,0.0,0.02,0.035/
     &       ppc/0.50, 0.50, 0.50, 0.50, 0.35, 0.35/,SL2PF/100.0/,
     &       NSPLIT/3/,IRESPLIT/17*0,1,6*0/,ZISPLIT/100.0/,
     &       ROLDMAX/0.25/

      DATA NPT1/0/,NSPT1/0/,NPT2/0/,NSEC/36/

      DATA NAR1/0/,NSAR1/0/,NAR2/0/,CAZINIT/MXSRCS*0.0/

      DATA NLINES/0/,NSLN1/0/,MXNSEG/7/,NLRISE/6/,XL/0.0/,HBL/0.0/,
     &       WBL/0.0/,WML/0.0/,DXL/0.0/,FPRIMEL/0.0/

      DATA NVL1/0/,NSVL1/0/,IGRDVL/0/,VEFFHT/0.0/,VSIGYI/0.0/,
     &       VSIGZI/0.0/

      DATA AZELEV/MXREC*0.0/

C*****************************************************************************
C***  CONVERT AUSPLUME VARIABLES

C*** PROCESS FILENAMES

      if(ver.GE.3.0) then
         PLMDAT = METFIL
         PUFLST = RESFIL
      endif

C*** PROCESS SOURCEGROUP OPTIONS

      IF (NGRPS.NE.1) THEN
        WRITE (*,475)
475   FORMAT (' WARNING: Source groupings are incompatible with ',
     &      ' AUSPUFF!',
     &      /,'  USE MULTIPLE RUNS OR MULTIPLE EMISSION RATES!')
      END IF

C*** PROCESS SOURCES

      DO I=1,NSRCS
        IF (SRCNUM(I).EQ.1) THEN
          NPT1=NPT1+1
          CPID(NPT1)=SRC_ID(I)
          CPXS(NPT1)=SRCFIELD(I,1)/1000.
          CPYS(NPT1)=SRCFIELD(I,2)/1000.
          CPZS(NPT1)=SRCFIELD(I,3)
          CPHS(NPT1)=SRCFIELD(I,4)
          CPDS(NPT1)=SRCFIELD(I,5)
          CPTS(NPT1)=SRCFIELD(I,6)
          CPVS(NPT1)=SRCFIELD(I,7)

c ---     Building Downwash
          dw(i)=0.0
          IF (LWAKE) THEN
c ---       Test for zero building height or width
            bdwtest=SRCFIELD(I,8) * SRCFIELD(I,9)
            if(bdwtest.GT.0.0) then
c ---          Turn on downwash for this source
               dw(i)=1.0
               DO J=1,NSEC
                 ADSBW(NPT1,J)=SRCFIELD(I,8)
                 ADSBH(NPT1,J)=SRCFIELD(I,9)
               END DO
            endif
          END IF

          IF (NRATES(I).EQ.1) THEN
            CPQS(NPT1)=SRCRATES(I,1)
            IPVARY(NPT1) = 0
            LPFAC(NPT1) = .FALSE.
          ELSE IF (NRATES(I).EQ.24) THEN
            CPQS(NPT1)=1.0
            IPVARY(NPT1) = 1
            LPFAC(NPT1) = .TRUE.
            nspt1 = nspt1 + 1
          ELSE IF (NRATES(I).EQ.12) THEN
            CPQS(NPT1)=1.0
            IPVARY(NPT1) = 2
            LPFAC(NPT1) = .TRUE.
            nspt1 = nspt1 + 1
          ELSE IF (NRATES(I).EQ.96) THEN
            CPQS(NPT1)=1.0
            IPVARY(NPT1) = 3
            LPFAC(NPT1) = .TRUE.
            nspt1 = nspt1 + 1
          ELSE IF (NRATES(I).EQ.36) THEN
            CPQS(NPT1)=1.0
            IPVARY(NPT1) = 4
            LPFAC(NPT1) = .TRUE.
            nspt1 = nspt1 + 1
          ELSE IF (NRATES(I).EQ.11) THEN
            CPQS(NPT1)=1.0
            IPVARY(NPT1) = 5
            LPFAC(NPT1) = .TRUE.
            nspt1 = nspt1 + 1
          ELSE
            WRITE(*,500)
500   FORMAT (' ERROR: Number of POINT SOURCE variable emission rates ',
     &        'incompatible with AUSPUFF!',
     &        /,'  EDIT AUSPUFF INPUT FILE MANUALLY!')
          END IF
          DO J=1,NRATES(I)
            CPFAC(NPT1,J) = SRCRATES(I,J)
          END DO

        ELSE IF (SRCNUM(I).EQ.2) THEN
          NAR1=NAR1+1
          CAID(NAR1)=SRC_ID(I)
          DIST2VERT=SRCFIELD(I,8)/2.0
          CAXVERT(NAR1,1)=(SRCFIELD(I,1)-DIST2VERT)/1000.
          CAYVERT(NAR1,1)=(SRCFIELD(I,2)-DIST2VERT)/1000.
          CAXVERT(NAR1,2)=(SRCFIELD(I,1)-DIST2VERT)/1000.
          CAYVERT(NAR1,2)=(SRCFIELD(I,2)+DIST2VERT)/1000.
          CAXVERT(NAR1,3)=(SRCFIELD(I,1)+DIST2VERT)/1000.
          CAYVERT(NAR1,3)=(SRCFIELD(I,2)+DIST2VERT)/1000.
          CAXVERT(NAR1,4)=(SRCFIELD(I,1)+DIST2VERT)/1000.
          CAYVERT(NAR1,4)=(SRCFIELD(I,2)-DIST2VERT)/1000.

          CAZS(NAR1)=SRCFIELD(I,3)
          CAHS(NAR1)=SRCFIELD(I,4)
          WRITE(*,525)
525   FORMAT (' WARNING: Area Source Requires Initial Sigma-Z Value!',
     &        /,'  EDIT AUSPUFF INPUT FILE MANUALLY!')

          IF (NRATES(I).EQ.1) THEN
            CAQS(NAR1)=SRCRATES(I,1)
            IAVARY(NAR1) = 0
            LAFAC(NAR1) = .FALSE.
          ELSE IF (NRATES(I).EQ.24) THEN
            CAQS(NAR1)=1.0
            IAVARY(NAR1) = 1
            LAFAC(NAR1) = .TRUE.
            nsar1 = nsar1 + 1
          ELSE IF (NRATES(I).EQ.12) THEN
            CAQS(NAR1)=1.0
            IAVARY(NAR1) = 2
            LAFAC(NAR1) = .TRUE.
            nsar1 = nsar1 + 1
          ELSE IF (NRATES(I).EQ.96) THEN
            CAQS(NAR1)=1.0
            IAVARY(NAR1) = 3
            LAFAC(NAR1) = .TRUE.
            nsar1 = nsar1 + 1
          ELSE IF (NRATES(I).EQ.36) THEN
            CAQS(NAR1)=1.0
            IAVARY(NAR1) = 4
            LAFAC(NAR1) = .TRUE.
            nsar1 = nsar1 + 1
          ELSE IF (NRATES(I).EQ.11) THEN
            CAQS(NAR1)=1.0
            IAVARY(NAR1) = 5
            LAFAC(NAR1) = .TRUE.
            nsar1 = nsar1 + 1
          ELSE
            WRITE(*,550)
550   FORMAT (' ERROR: Number of AREA SOURCE variable emission rates ',
     &        'incompatible with AUSPUFF!',
     &        /,'  EDIT AUSPUFF INPUT FILE MANUALLY!')
          END IF
          DO J=1,NRATES(I)
            CAFAC(NAR1,J) = SRCRATES(I,J)
          END DO

        ELSE IF (SRCNUM(I).EQ.3) THEN
          NVL1=NVL1+1
          CVID(NVL1)=SRC_ID(I)
          CVXS(NVL1)=SRCFIELD(I,1)/1000.
          CVYS(NVL1)=SRCFIELD(I,2)/1000.
          CVZS(NVL1)=SRCFIELD(I,3)
          CVHS(NVL1)=SRCFIELD(I,4)
          CVYINIT(NVL1)=SRCFIELD(I,8)
          CVZINIT(NVL1)=SRCFIELD(I,9)

          IF (NRATES(I).EQ.1) THEN
            CVQS(NVL1)=SRCRATES(I,1)
            IVVARY(NVL1) = 0
            LVFAC(NVL1) = .FALSE.
          ELSE IF (NRATES(I).EQ.24) THEN
            CVQS(NVL1)=1.0
            IVVARY(NVL1) = 1
            LVFAC(NVL1) = .TRUE.
            nsvl1 = nsvl1 + 1
          ELSE IF (NRATES(I).EQ.12) THEN
            CVQS(NVL1)=1.0
            IVVARY(NVL1) = 2
            LVFAC(NVL1) = .TRUE.
            nsvl1 = nsvl1 + 1
          ELSE IF (NRATES(I).EQ.96) THEN
            CVQS(NVL1)=1.0
            IVVARY(NVL1) = 3
            LVFAC(NVL1) = .TRUE.
            nsvl1 = nsvl1 + 1
          ELSE IF (NRATES(I).EQ.36) THEN
            CVQS(NVL1)=1.0
            IVVARY(NVL1) = 4
            LVFAC(NVL1) = .TRUE.
            nsvl1 = nsvl1 + 1
          ELSE IF (NRATES(I).EQ.11) THEN
            CVQS(NVL1)=1.0
            IVVARY(NVL1) = 5
            LVFAC(NVL1) = .TRUE.
            nsvl1 = nsvl1 + 1
          ELSE
            WRITE(*,600)
600   FORMAT (' ERROR: Number of VOLUME SOURCE variable emission rates '
     &        ,'incompatible with AUSPUFF!',
     &        /,'  EDIT AUSPUFF INPUT FILE MANUALLY!')
          END IF
          DO J=1,NRATES(I)
            CVFAC(NVL1,J) = SRCRATES(I,J)
          END DO

        END IF

      END DO

C--- PROCESS INPUT EMISSIONS UNITS

      DO 30 J=1,numunits
        ii = len(qutest(j))

        DO 20 I=1,ii
          IF (QUNITS(I).EQ.QUTEST(J)(I:I)) THEN
            found = .true.
            GOTO 20
          ELSE
            FOUND = .FALSE.
            GOTO 30
          END IF
20      END DO
      IF (FOUND) THEN
        IUNITS = J
        GOTO 31
      ELSE
      END IF
30    END DO

31    IF (IUNITS.EQ.1 .OR. IUNITS.EQ.8) THEN
        IPTU = 1
        IARU = 1
        ILNU = 1
        IVLU = 1
        IPRTU = 1
      ELSE IF (IUNITS.EQ.2 .OR. IUNITS.EQ.9) THEN
        IPTU = 2
        IARU = 2
        ILNU = 2
        IVLU = 2
        IPRTU = 1
      ELSE IF (IUNITS.EQ.3 .OR. IUNITS.EQ.10) THEN
        IPTU = 4
        IARU = 4
        ILNU = 4
        IVLU = 4
        IPRTU = 1
      ELSE IF (IUNITS.EQ.4 .OR. IUNITS.EQ.11) THEN
        IPTU = 5
        IARU = 5
        ILNU = 5
        IVLU = 5
        IPRTU = 5
      ELSE IF (IUNITS.EQ.5 .OR. IUNITS.EQ.12) THEN
        IPTU = 6
        IARU = 6
        ILNU = 6
        IVLU = 6
        IPRTU = 5
      ELSE IF (IUNITS.EQ.6 .OR. IUNITS.EQ.13) THEN
        IPTU = 5
        IARU = 5
        ILNU = 5
        IVLU = 5
        IPRTU = 5
      ELSE IF (IUNITS.EQ.7 .OR. IUNITS.EQ.14) THEN
        IPTU = 6
        IARU = 6
        ILNU = 6
        IVLU = 6
        IPRTU = 5
      ELSE
        WRITE (*,35)
35    FORMAT (' WARNING:  Emissions units not recognized by AUSPUFF!',
     &        /,'  EDIT AUSPUFF INPUT FILE MANUALLY!')
c ---   Set units to g/s as a general default
        IPTU = 1
        IARU = 1
        ILNU = 1
        IVLU = 1
        IPRTU = 1

      END IF


C*** PROCESS RECEPTORS

      ICOUNTREC=0
      DTR = PI/180.

C--- DISCRETE

      IF (LCARTD) THEN
        DO I=1,NDISCR
          ICOUNTREC=ICOUNTREC+1
          AXR(ICOUNTREC)=REC(I*2)
          AYR(ICOUNTREC)=REC(((I-1)*2)+1)
        END DO
      ELSE
        DO I=1,NDISCR
          ICOUNTREC=ICOUNTREC+1
          AXR(ICOUNTREC)=REC(((I-1)*2)+1)*COS(REC(I*2)*DTR)
          AYR(ICOUNTREC)=REC(((I-1)*2)+1)
        END DO
      END IF

C--- GRIDDED

      IF (LCARTG) THEN
        DO 50 I=1,NXA
          DO 40 J=1,NYA
            ICOUNTREC=ICOUNTREC+1
            AXR(ICOUNTREC)=GRIDX(I)
            AYR(ICOUNTREC)=GRIDY(J)
40        CONTINUE
50      CONTINUE
      ELSE
        DO 70 I=1,NXA
          DO 60 J=1,NYA
            ICOUNTREC=ICOUNTREC+1
            AXR(ICOUNTREC)=GRIDX(I)*SIN(GRIDY(J)*DTR)
            AYR(ICOUNTREC)=GRIDX(I)*COS(GRIDY(J)*DTR)
60        CONTINUE
70      CONTINUE
      END IF

      NUMREC = ICOUNTREC

C*** PROCESS MODEL OPTIONS

C--- DEPOSITION OPTIONS
      if (LCONC) THEN
        MWET = 0
        MDRY = 0
        ICON = 1
        IDRY = 0
        IWET = 0
        ICPRT= 0
        IDPRT= 0
        IWPRT= 0
      ELSE
        MWET = 1
        MDRY = 1
        ICON = 1
        IDRY = 1
        IWET = 1
        ICPRT= 0
        IDPRT= 0
        IWPRT= 0
        WRITE(*,650)
650   FORMAT (' WARNING: Deposition Selected!  Particle Specific ',
     &        ' Information NOT converted!',/,
     &        '  EDIT AUSPUFF INPUT FILE MANUALLY!')
        WRITE(*,651)
651   FORMAT (' WARNING: Deposition Selected!  Data for AUSPUFF Input ',
     &        ' Groups 7,8, and/or 10 NOT converted!',/,
     &        '  EDIT AUSPUFF INPUT FILE MANUALLY!')
      END IF

C---  DETERMINE MET and COMPUTATIONAL GRID VARIABLES

c --  Find max and min source and/or receptor coordinates
      do i=1,NPT1
        if (CPXS(i).lt.XORIGM) XORIGM = CPXS(i)
        if (CPYS(i).lt.YORIGM) YORIGM = CPYS(i)
        if (CPXS(i).gt.XGRIDM) XGRIDM = CPXS(i)
        if (CPYS(i).gt.YGRIDM) YGRIDM = CPYS(i)
      end do
      do i=1,NAR1
        do j=1,4
          if (CAXVERT(i,j).lt.XORIGM) XORIGM = CAXVERT(i,j)
          if (CAYVERT(i,j).lt.YORIGM) YORIGM = CAYVERT(i,j)
          if (CAXVERT(i,j).gt.XGRIDM) XGRIDM = CAXVERT(i,j)
          if (CAYVERT(i,j).gt.YGRIDM) YGRIDM = CAYVERT(i,j)
        end do
      end do
      do i=1,NVL1
        if (CVXS(i).lt.XORIGM) XORIGM = CVXS(i)
        if (CVYS(i).lt.YORIGM) YORIGM = CVYS(i)
        if (CVXS(i).gt.XGRIDM) XGRIDM = CVXS(i)
        if (CVYS(i).gt.YGRIDM) YGRIDM = CVYS(i)
      end do

      do i=1,NUMREC
        if (AXR(i).lt.XORIGM) XORIGM = AXR(i)
        if (AYR(i).lt.YORIGM) YORIGM = AYR(i)
        if (AXR(i).gt.XGRIDM) XGRIDM = AXR(i)
        if (AYR(i).gt.YGRIDM) YGRIDM = AYR(i)
      end do

c --  Extend
      XORIGKM = (XORIGM/1000.) - 0.1
      YORIGKM = (YORIGM/1000.) - 0.1
      XGRIDKM = (XGRIDM/1000.) + 0.1
      YGRIDKM = (YGRIDM/1000.) + 0.1

      DGRIDKM = AMAX1(((XGRIDKM-XORIGKM)/NX),((YGRIDKM-YORIGKM)/NY))

      IECOMP = NX
      JECOMP = NY


C--- STACK TIP DOWNWASH

      IF (DNWASH) THEN
        MTIP=1
      ELSE
        MTIP=0
      END IF

C--- POTENTIAL TEMPERATURE GRADIENT

      DO I=1,4
        PTG0(I) = 0.0
      END DO
      PTG0(5) = DTHDZ(6,5)
      PTG0(6) = DTHDZ(6,6)

C--- WIND POWER LAW EXPONENT

      IF (IGNORP.and.IWPROF.eq.1) THEN
        PLX0(1) = 0.15
        PLX0(2) = 0.15
        PLX0(3) = 0.20
        PLX0(4) = 0.25
        PLX0(5) = 0.40
        PLX0(6) = 0.60
        WRITE(*,656)
656   FORMAT (' WARNING: Using "Irwin Urban" Wind Power Law Exponents!')
      ELSE IF (IGNORP.and.IWPROF.eq.2) THEN
        PLX0(1) = 0.07
        PLX0(2) = 0.07
        PLX0(3) = 0.10
        PLX0(4) = 0.15
        PLX0(5) = 0.35
        PLX0(6) = 0.55
        WRITE(*,657)
657   FORMAT (' WARNING: Using "Irwin Rural" Wind Power Law Exponents!')
      ELSE IF (IGNORP.and.IWPROF.eq.3) THEN
        PLX0(1) = 0.10
        PLX0(2) = 0.15
        PLX0(3) = 0.20
        PLX0(4) = 0.25
        PLX0(5) = 0.30
        PLX0(6) = 0.30
        WRITE(*,658)
658   FORMAT (' WARNING: Using "ISCST" Wind Power Law Exponents! ')
      ELSE IF (.NOT.IGNORP.and.IWPROF.eq.4) then
        DO I=1,6
          PLX0(I) = P(1,I)
        END DO
        WRITE(*,659)
659   FORMAT(' WARNING: Using "User Defined" Wind Power Law Exponents!')
      END IF

C--- WIND SPEED CATEGORIES

      DO I=1,5
        WSCAT(I) = UCATS(I)
      END DO

C--- ANEMOMETER HEIGHT (ZREF)

      ANEMHT = ZREF

C--- SURFACE ROUGHNESS

      Z0IN = Z0
      if (LZ0Y.or.LZ0Z) then
        MROUGH = 1
      else 
        MROUGH = 0
      end if

C--- PARTIAL PLUME PENETRATION

      IF (PENET) THEN
        MPARTL=1
      ELSE
        MPARTL=0
      END IF

C--- VERTICAL WIND SHEAR

      IF (ISHR.EQ.0) THEN
        MSHEAR=0
      ELSE IF (ISHR.EQ.1) THEN
        MSHEAR=1
      ELSE
        WRITE(*,660)
660   FORMAT (' WARNING: User-specified Wind Directional Shears! '
     &        ,/,' Information NOT converted!')
      END IF

C--- GRADUAL PLUME RISE
      IF (GRDRIS) THEN
        MTRANS=1
      ELSE
        MTRANS=0
      END IF

C--- PLUME PATH COEFFICIENTS

      MCTADJ = 3
      DO I=1,6
        PPC(I)=1-EGAN(I)
      END DO

C--- AVERAGING TIME
      IF (NMINS.GT.60) THEN
        AVET = 60.
      ELSE IF (NMINS.LE.0) THEN
        AVET = 60.
      ELSE 
        AVET = NMINS
      END IF

C--- DISPERSION CURVE INFORMATION

      IF (CURVE(1).NE.CURVE(3).OR.CURVE(2).NE.CURVE(4)) WRITE (*,665)
      IF (CURVE(1).NE.CURVE(2)) WRITE (*,666)

      IF (CURVE(1).EQ.1) THEN
        MDISP = 3
      ELSE IF (CURVE(1).EQ.2) THEN
        MDISP = 3
        WRITE (*,667)        
      ELSE IF (CURVE(1).EQ.3) THEN
        MDISP = 1
        MDISP2 = 3
        MTURBVW = 4
      END IF

665   FORMAT(' WARNING: Dispersion curves differ by source height!'
     &      ,/,'  USING CURVES FOR SOURCES <100m HIGH!')
666   FORMAT(' WARNING: Dispersion curve methods differ!'
     &      ,/,'  USING METHOD SPECIFIED FOR HORIZONTAL CURVES!')
667   FORMAT(' WARNING: Briggs rural dispersion curve selected!'
     &      ,/,'  USING PASQUILL-GIFFORD CURVES!')

C--- WRITE OTHER WARNINGS TO SCREEN

      write (*,670)
670   FORMAT (' WARNING: Starting Date and Length of Run not converted!'
     &      ,/,'  USING ALL MET DATA BY DEFAULT!')


C*****************************************************************************
C***  WRITE OUT FORMATTED AUSPUFF INPUT FILE

      write (IO,8000) (TITLE(I),i=1,80)
      write (IO,8000) (TITLE(I),i=81,160)
      write (IO,8000) (TITLE(I),i=161,240)
      write (IO,*)
      write (IO,*) linebreak
      write (IO,*)
      write (IO,*)'              AUSPUFF MODEL CONTROL FILE'
      write (IO,*)
8000  format (80a1)

      write (IO,*) linebreak
      write (IO,*) 'INPUT GROUP:  0'
      write (IO,9000) '  ! METDAT =  ',metdat,  '  ! (DEF)'
      write (IO,9000) '  ! ISCDAT =  ',iscdat,  '  ! (DEF)'
      write (IO,9000) '  ! PLMDAT =  ',plmdat,  '  !      '
      write (IO,9000) '  ! PRFDAT =  ',prfdat,  '  ! (DEF)'
      write (IO,9000) '  ! SFCDAT =  ',sfcdat,  '  ! (DEF)'
      write (IO,9000) '  ! PUFLST =  ',puflst,  '  !      '
      write (IO,9000) '  ! CONDAT =  ',condat,  '  ! (DEF)'
      write (IO,9000) '  ! DFDAT =   ',dfdat,   '  ! (DEF)'
      write (IO,9000) '  ! WFDAT =   ',wfdat,   '  ! (DEF)'
      write (IO,9000) '  ! VISDAT =  ',visdat,  '  ! (DEF)'
      write (IO,9000) '  ! PTDAT =   ',ptdat,   '  ! (DEF)'
      write (IO,9000) '  ! VOLDAT =  ',voldat,  '  ! (DEF)'
      write (IO,9000) '  ! ARDAT =   ',ardat,   '  ! (DEF)'
      write (IO,9000) '  ! OZDAT =   ',ozdat,   '  ! (DEF)'
      write (IO,9000) '  ! VDDAT =   ',vddat,   '  ! (DEF)'
      write (IO,9000) '  ! CHEMDAT = ',chemdat, '  ! (DEF)'
      write (IO,9000) '  ! HILDAT =  ',hildat,  '  ! (DEF)'
      write (IO,9000) '  ! RCTDAT =  ',rctdat,  '  ! (DEF)'
      write (IO,9000) '  ! DEBUG =   ',debugfile,'  ! (DEF)'
      write (IO,9001) '  ! LCFILES = ',lcfiles, '  ! (DEF)'
      write (IO,*)
      write (IO,'(a5)') '!END!'
9000  format (1x,a14,a80,a9)
9001  format (1x,a14,l1,11x,a9)


      write (IO,*) linebreak
      write (IO,*) 'INPUT GROUP:  1'
      write (IO,9010) '  ! METRUN = ',METRUN,    '  ! (DEF)'
      write (IO,9010) '  ! IBYR =   ',IBYR,      '  ! (DEF)'
      write (IO,9010) '  ! IBMO =   ',IBMO,      '  ! (DEF)'
      write (IO,9010) '  ! IBDY =   ',IBDY,      '  ! (DEF)'
      write (IO,9010) '  ! IBHR =   ',IBHR,      '  ! (DEF)'
      write (IO,9010) '  ! IRLG =   ',IRLG,      '  ! (DEF)'
      write (IO,9010) '  ! NSPEC =  ',NSPEC,     '  ! (DEF)'
      write (IO,9010) '  ! NSE =    ',NSE,       '  ! (DEF)'
      write (IO,9010) '  ! ITEST =  ',ITEST,     '  ! (DEF)'
      write (IO,9010) '  ! METFM =  ',METFM,     '  ! (DEF)'
      write (IO,9011) '  ! AVET =   ',AVET,      '  !      '
      write (IO,*)
      write (IO,'(a5)') '!END!'
9010  format (1x,a13,i4,a9)
9011  format (1x,a13,F4.1,a9)
      write (IO,*)

      write (IO,*) linebreak
      write (IO,*) 'INPUT GROUP:  2'
      write (IO,9020) '  ! MGAUSS =  ',MGAUSS,  '  ! (DEF)'
      write (IO,9020) '  ! MCTADJ =  ',MCTADJ,  '  ! (DEF)'
      write (IO,9020) '  ! MCTSG =   ',MCTSG,   '  ! (DEF)'
      write (IO,9020) '  ! MSLUG =   ',MSLUG,   '  ! (DEF)'
      write (IO,9020) '  ! MTRANS =  ',MTRANS,  '  !      '
      write (IO,9020) '  ! MTIP =    ',MTIP,    '  !      '
      write (IO,9020) '  ! MSHEAR =  ',MSHEAR,  '  !      '
      write (IO,9020) '  ! MSPLIT =  ',MSPLIT,  '  ! (DEF)'
      write (IO,9020) '  ! MCHEM =   ',MCHEM,   '  ! (DEF)'
      write (IO,9020) '  ! MWET =    ',MWET,    '  !      '
      write (IO,9020) '  ! MDRY =    ',MDRY,    '  !      '
      write (IO,9020) '  ! MDISP =   ',MDISP,   '  ! (DEF)'
      write (IO,9020) '  ! MTURBVW = ',MTURBVW, '  ! (DEF)'
      write (IO,9020) '  ! MDISP2 =  ',MDISP2,  '  ! (DEF)'
      write (IO,9020) '  ! MROUGH =  ',MROUGH,  '  ! (DEF)'
      write (IO,9020) '  ! MPARTL =  ',MPARTL,  '  !      '
      write (IO,9020) '  ! MREG =    ',MREG,    '  ! (DEF)'
      write (IO,*)
      write (IO,'(a5)') '!END!'
9020  format (1x,a14,i1,a9)
      write (IO,*)

      write (IO,*) linebreak
      write (IO,*) 'INPUT GROUP:  3'
      write (IO,9030) '  ! CSPEC = ', POLLUT,' !  !END!'
      write (IO,9031) '  ! ',POLLUT,' = 1, 1,',IDDEPTYP,', 0  !'
      write (IO,*)
      write (IO,'(a5)') '!END!'
9030  format (1x,a12,a8,a9)
9031  format (1x,a4,2a8,i2,a6)
      write (IO,*)

      write (IO,*) linebreak
      write (IO,*) 'INPUT GROUP:  4'
      write (IO,9041) '  ! NX =        ',NX,      '  ! (DEF)'
      write (IO,9041) '  ! NY =        ',NY,      '  ! (DEF)'
      write (IO,9041) '  ! NZ =        ',NZ,      '  ! (DEF)'
      write (IO,9042) '  ! DGRIDKM =   ',DGRIDKM, '  !      '
      write (IO,*)    '  ! ZFACE =     0.0, 5000.0   ! (DEF)'
      write (IO,9042) '  ! XORIGKM =   ',XORIGKM, '  !      '
      write (IO,9042) '  ! YORIGKM =   ',YORIGKM, '  !      '
      write (IO,9041) '  ! IUTMZN =    ',IUTMZN,  '  ! (DEF)'
      write (IO,9042) '  ! XLAT =      ',XLAT,    '  ! (DEF)'
      write (IO,9042) '  ! XLONG =     ',XLONG,   '  ! (DEF)'
      write (IO,9042) '  ! XTZ =       ',XTZ,     '  ! (DEF)'
      write (IO,9041) '  ! IBCOMP =    ',IBCOMP,  '  ! (DEF)'
      write (IO,9041) '  ! JBCOMP =    ',JBCOMP,  '  ! (DEF)'
      write (IO,9041) '  ! IECOMP =    ',IECOMP,  '  !      '
      write (IO,9041) '  ! JECOMP =    ',JECOMP,  '  !      '
      write (IO,9040) '  ! LSAMP =     ',LSAMP,   '  ! (DEF)'
      write (IO,9041) '  ! IBSAMP =    ',IBSAMP,  '  ! (DEF)'
      write (IO,9041) '  ! JBSAMP =    ',JBSAMP,  '  ! (DEF)'
      write (IO,9041) '  ! IESAMP =    ',IESAMP,  '  ! (DEF)'
      write (IO,9041) '  ! JESAMP =    ',JESAMP,  '  ! (DEF)'
      write (IO,9041) '  ! MESHDN =    ',MESHDN,  '  ! (DEF)'
      write (IO,*)
      write (IO,'(a5)') '!END!'
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
      write (IO,'(a5)') '!END!'
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
      write (IO,'(a5)') '!END!'
      write (IO,*) ' GROUPS 6B & 6C MUST BE INPUT BY USER'
9060  format (1x,a14,i5,a9)
9061  format (1x,a14,f5.1,a9)
      write (IO,*)

      write (IO,*) linebreak
      write (IO,*) 'INPUT GROUP:  7'
      if (idry.eq.1) then
        write (IO,*) ' GROUP 7 MUST BE INPUT BY USER'
      else
      end if
      write (IO,*)
      write (IO,'(a5)') '!END!'
      write (IO,*)

      write (IO,*) linebreak
      write (IO,*) 'INPUT GROUP:  8'
      write (IO,*) ' GROUP 8 MUST BE INPUT BY USER'
      write (IO,*)
      write (IO,'(a5)') '!END!'
      write (IO,*)

      write (IO,*) linebreak
      write (IO,*) 'INPUT GROUP:  9'
      write (IO,9090) '  ! RCUTR =  ',RCUTR, '  ! (DEF)'
      write (IO,9090) '  ! RGR =    ',RGR,   '  ! (DEF)'
      write (IO,9090) '  ! REACTR = ',REACTR,'  ! (DEF)'
      write (IO,9090) '  ! NINT =   ',NINT,  '  ! (DEF)'
      write (IO,9091) '  ! IVEG =   ',IVEG,  '  ! (DEF)'
      write (IO,*)
      write (IO,'(a5)') '!END!'
9090  format (1x,a13,f5.1,a9)
9091  format (1x,a13,i5,a9)
      write (IO,*)

      write (IO,*) linebreak
      write (IO,*) 'INPUT GROUP:  10'
      if (iwet.eq.1) then
        write (IO,*) ' GROUP 10 MUST BE INPUT BY USER'
      else
      end if
      write (IO,*)
      write (IO,'(a5)') '!END!'
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
      write (IO,'(a5)') '!END!'
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
      write (IO,9121) '  ! ILANDUIN = ',ILANDUIN,   '  ! (DEF)'
      write (IO,9120) '  ! Z0IN =     ',Z0IN,       '  !      '
      write (IO,9120) '  ! XLAIIN =   ',XLAIIN,     '  ! (DEF)'
      write (IO,9120) '  ! ANEMHT =   ',ANEMHT,     '  !      '
      write (IO,9121) '  ! ISIGMAV =  ',ISIGMAV,    '  ! (DEF)'
      write (IO,9121) '  ! IMIXCTDM = ',IMIXCTDM,   '  ! (DEF)'
      write (IO,9120) '  ! XMXLEN =   ',XMXLEN,     '  ! (DEF)'
      write (IO,9120) '  ! XSAMLEN =  ',XSAMLEN,    '  ! (DEF)'
      write (IO,9121) '  ! MXNEW =    ',MXNEW,      '  ! (DEF)'
      write (IO,9120) '  ! MXSAM =    ',MXSAM,      '  ! (DEF)'
      write (IO,9120) '  ! SYMIN =    ',SYMIN,      '  ! (DEF)'
      write (IO,9120) '  ! SZMIN =    ',SZMINc,     '  ! (DEF)'

c --- Do not write these variables:  let AUSPUFF supply the default
c --- values because these are now arrays
c     write (IO,9120) '  ! SVMIN =    ',SVMIN,      '  ! (DEF)'
c     write (IO,9120) '  ! SWMIN =    ',SWMIN,      '  ! (DEF)'

      write (IO,9120) '  ! WSCALM =   ',WSCALM,     '  ! (DEF)'
      write (IO,9120) '  ! XMAXZI =   ',XMAXZI,     '  ! (DEF)'
      write (IO,9120) '  ! XMINZI =   ',XMINZI,     '  ! (DEF)'
      write (IO,9126) '  ! WSCAT =    ',wscat,      '  !      '
      write (IO,9122) '  ! PLX0 =     ',plx0,       '  !      '
      write (IO,9123) '  ! PTG0 =     ',ptg0(5),ptg0(6),'  !      '
      write (IO,9122) '  ! PPC =      ',ppc,        '  !      '
      write (IO,9120) '  ! SL2PF =    ',SL2PF,      '  ! (DEF)'
      write (IO,9121) '  ! NSPLIT =   ',NSPLIT,     '  ! (DEF)'
      write (IO,9125) '  ! IRESPLIT = ',IRESPLIT,   '  ! (DEF)'
      write (IO,9120) '  ! ZISPLIT =  ',ZISPLIT,    '  ! (DEF)'
      write (IO,9120) '  ! ROLDMAX =  ',ROLDMAX,    '  ! (DEF)'

      write (IO,*)
      write (IO,'(a5)') '!END!'
9120  format (1x,a15,f7.2,a9)
9121  format (1x,a15,i7,a9)
9122  format (1x,a15,5(f7.2,', '),f7.2,a9)
9123  format (1x,a15,f7.3,', ',f7.3,a9)
9124  format (1x,a15,a35,a9)
9125  format (1x,a15,23(i1,','),i1,a9)
9126  format (1x,a15,4(f7.2,', '),f7.2,a9)
      write (IO,*)

      write (IO,*) linebreak
      write (IO,*) 'INPUT GROUP:  13'
      write (IO,9130) '  ! NPT1 =  ',npt1, '  !      '
      write (IO,9130) '  ! IPTU =  ',iptu, '  !      '
      write (IO,9130) '  ! NSPT1 = ',nspt1, '  !      '
      write (IO,9130) '  ! NPT2 =  ',NPT2, '  !      '
      write (IO,'(a5)') '!END!'
      write (IO,*)
      write (IO,*) 'Subgroup (13b)'
      do i=1,npt1
        write (IO,9133) i,'  ! SRCNAM = ',cpid(i),'  !  '
        write (IO,9131) i,'  ! X = ',cpxs(i),cpys(i),cphs(i),cpzs(i),
     &                  cpds(i),cpvs(i),cpts(i),dw(i),cpqs(i),
     &                  ' !  !END!'
      end do
      write (IO,*)
      write (IO,*) 'Subgroup (13c)'
      do i=1,npt1
        if (dw(i).EQ.1.0) then
          write (IO,9133) i,'  !  SRCNAM = ',cpid(i),'  !  '
          write (IO,9132) i,'  !  WIDTH =  ',(ADSBW(I,J),j=1,nsec),
     &                    ' !'
          write (IO,9132) i,'  !  HEIGHT = ',(ADSBH(I,J),j=1,nsec),
     &                    ' !'
          write (IO,'(a5)') '!END!'
        end if
      end do
      write (IO,*)
      write (IO,*) 'Subgroup (13d)'
      do i=1,npt1
        if (lpfac(i)) then
          write (IO,9133) i,'  !  SRCNAM = ',cpid(i),'  !  '
          write (IO,9134) i,'  !  IVARY  = ',ipvary(i),'  !  '
          if (ipvary(i).eq.0) then
            write (IO,9135) i,'  !  ',pollut,' = ',(CPFAC(i,j),
     &          j=1,1),'  !  '
          else if (ipvary(i).eq.1) then
            write (IO,9136) i,'  !  ',pollut,' = ',(CPFAC(i,j),
     &          j=1,24),'  !  '
          else if (ipvary(i).eq.2) then
            write (IO,9137) i,'  !  ',pollut,' = ',(CPFAC(i,j),
     &          j=1,12),'  !  '
          else if (ipvary(i).eq.3) then
            write (IO,9138) i,'  !  ',pollut,' = ',(CPFAC(i,j),
     &          j=1,96),'  !  '
          else if (ipvary(i).eq.4) then
            write (IO,9139) i,'  !  ',pollut,' = ',(CPFAC(i,j),
     &          j=1,36),'  !  '
          else if (ipvary(i).eq.5) then
            CPFAC(i,12) = CPFAC(I,11)
            write (IO,9137) i,'  !  ',pollut,' = ',(CPFAC(i,j),
     &          j=1,12),'  !  '
          end if
          write (IO,'(a5)') '!END!'
        else
        end if
      end do
9130  format(1x,a12,i1,a9)
9131  format(1x,i2,a8,7(f8.3,', '),f3.1,', ',f8.3,a9)
9132  format(1x,i2,a14,5(6(f6.2,', '),/,17x),5(f6.2,', '),f6.2,a2)
9133  format(1x,i2,a14,a6,a5)
9134  format(1x,i2,a14,i1,a5)
9135  format(1x,i2,a5,a6,a3,e10.5,a5)
9136  format(1x,i2,a5,a6,a3,3(6(e10.5,', '),/,17x),5(e10.5,', '),
     &      e10.5,a5)
9137  format(1x,i2,a5,a6,a3,1(6(e10.5,', '),/,17x),5(e10.5,', '),
     &      e10.5,a5)
9138  format(1x,i2,a5,a6,a3,15(6(e10.5,', '),/,17x),5(e10.5,', '),
     &      e10.5,a5)
9139  format(1x,i2,a5,a6,a3,5(6(e10.5,', '),/,17x),5(e10.5,', '),
     &      e10.5,a5)

      write (IO,*)

      write (IO,*) linebreak
      write (IO,*) 'INPUT GROUP:  14'
      write (IO,9140) '  ! NAR1 =  ',nar1, ' !      '
      write (IO,9140) '  ! IARU =  ',iaru, ' !      '
      write (IO,9140) '  ! NSAR1 = ',nsar1, ' !      '
      write (IO,9140) '  ! NAR2 =  ',NAR2, ' !      '
      write (IO,'(a5)') '!END!'
      write (IO,*)
      write (IO,*) 'Subgroup (14b)'
      do i=1,nar1
        write (IO,9143) i,'  !  SRCNAM = ',caid(i),'  !  '
        write (IO,9141) i,'  ! X = ',cahs(i),cazs(i),cazinit(i),caqs(i)
     &                  ,' !  !END!'
      end do
      write (IO,*)
      write (IO,*) 'Subgroup (14c)'
      do i=1,nar1
        write (IO,9143) i,'  ! SRCNAM = ',caid(i),'  !  '
        write (IO,9142) i,'  ! XVERT = ',(caxvert(i,j),j=1,4),' !'
        write (IO,9142) i,'  ! YVERT = ',(cayvert(i,j),j=1,4),' !'
        write (IO,'(a5)') '!END!'
      end do
      write (IO,*)
      write (IO,*) 'Subgroup (14d)'
      do i=1,nar1
        if (lafac(i)) then
          write (IO,9143) i,'  !  SRCNAM = ',caid(i),'  !  '
          write (IO,9144) i,'  !  IVARY  = ',iavary(i),'  !  '
          if (iavary(i).eq.0) then
            write (IO,9135) i,'  !  ',pollut,' = ',(CAFAC(i,j),
     &          j=1,1),'  !  '
          else if (iavary(i).eq.1) then
            write (IO,9136) i,'  !  ',pollut,' = ',(CAFAC(i,j),
     &          j=1,24),'  !  '
          else if (iavary(i).eq.2) then
            write (IO,9137) i,'  !  ',pollut,' = ',(CAFAC(i,j),
     &          j=1,12),'  !  '
          else if (iavary(i).eq.3) then
            write (IO,9138) i,'  !  ',pollut,' = ',(CAFAC(i,j),
     &          j=1,96),'  !  '
          else if (iavary(i).eq.4) then
            write (IO,9139) i,'  !  ',pollut,' = ',(CAFAC(i,j),
     &          j=1,36),'  !  '
          else if (iavary(i).eq.5) then
            CAFAC(i,12) = CAFAC(I,11)
            write (IO,9137) i,'  !  ',pollut,' = ',(CAFAC(i,j),
     &          j=1,12),'  !  '
          end if
          write (IO,'(a5)') '!END!'
        else
        end if
      end do
9140  format (1x,a12,i2,a9)
9141  format (1x,i2,a8,3(f8.3,', '),f8.3,a9)
9142  format (1x,i2,a12,3(f8.3,', '),f8.3,a2)
9143  format (1x,i2,a14,a6,a5)
9144  format (1x,i2,a14,i1,a5)
      write (IO,*)

      write (IO,*) linebreak
      write (IO,*) 'INPUT GROUP:  15'
      write (IO,9150) '  ! NLINES =  ',NLINES,      ' !  (DEF)'
      write (IO,9150) '  ! ILNU =    ',ilnu,        ' !  (DEF)'
      write (IO,9150) '  ! NSLN1 =   ',nsln1,       ' !  (DEF)'
      write (IO,9150) '  ! MXNSEG =  ',MXNSEG,      ' !  (DEF)'
      write (IO,9150) '  ! NLRISE =  ',NLRISE,      ' !  (DEF)'
      write (IO,9151) '  ! XL =      ',XL,          ' !  (DEF)'
      write (IO,9151) '  ! HBL =     ',HBL,         ' !  (DEF)'
      write (IO,9151) '  ! WBL =     ',WBL,         ' !  (DEF)'
      write (IO,9151) '  ! WML =     ',WML,         ' !  (DEF)'
      write (IO,9151) '  ! DXL =     ',DXL,         ' !  (DEF)'
      write (IO,9151) '  ! FPRIMEL = ',FPRIMEL,     ' !  (DEF)'
      write (IO,*)
      write (IO,'(a5)') '!END!'
9150  format (1x,a14,i7,a9)
9151  format (1x,a14,f7.2,a9)

      write (IO,*)

      write (IO,*) linebreak
      write (IO,*) 'INPUT GROUP:  16'
      write (IO,9160) '  ! NVL1=    ',nvl1,     ' !       '
      write (IO,9160) '  ! IVLU =   ',ivlu,     ' !       '
      write (IO,9160) '  ! NSVL1 =  ',nsvl1,    ' !       '
      write (IO,9160) '  ! IGRDVL = ',IGRDVL,   ' !  (DEF)'
      write (IO,9161) '  ! VEFFHT = ',VEFFHT,   ' !  (DEF)'
      write (IO,9161) '  ! VSIGYI = ',VSIGYI,   ' !  (DEF)'
      write (IO,9161) '  ! VSIGZI = ',VSIGZI,   ' !  (DEF)'
      write (IO,'(a5)') '!END!'
      write (IO,*)
      write (IO,*) 'Subgroup (16b)'
      do i=1,nvl1
        write (IO,9143) i,'  !  SRCNAM = ',cvid(i),'  !  '
        write (IO,9162) i,'  ! X = ',cvxs(i),cvys(i),cvhs(i),cvzs(i),
     &               cvyinit(i),cvzinit(i),cvqs(i),' !  !END!'
      end do
      write (IO,*)
      write (IO,*) 'Subgroup (16c)'
      do i=1,nvl1
        if (lvfac(i)) then
          write (IO,9143) i,'  !  SRCNAM = ',cvid(i),'  !  '
          write (IO,9144) i,'  !  IVARY  = ',ivvary(i),'  !  '
          if (ivvary(i).eq.0) then
            write (IO,9135) i,'  !  ',pollut,' = ',(CVFAC(i,j),
     &          j=1,1),'  !  '
          else if (ivvary(i).eq.1) then
            write (IO,9136) i,'  !  ',pollut,' = ',(CVFAC(i,j),
     &          j=1,24),'  !  '
          else if (ivvary(i).eq.2) then
            write (IO,9137) i,'  !  ',pollut,' = ',(CVFAC(i,j),
     &          j=1,12),'  !  '
          else if (ivvary(i).eq.3) then
            write (IO,9138) i,'  !  ',pollut,' = ',(CVFAC(i,j),
     &          j=1,96),'  !  '
          else if (ivvary(i).eq.4) then
            write (IO,9139) i,'  !  ',pollut,' = ',(CVFAC(i,j),
     &          j=1,36),'  !  '
          else if (ivvary(i).eq.5) then
            CVFAC(i,12) = CVFAC(I,11)
            write (IO,9137) i,'  !  ',pollut,' = ',(CVFAC(i,j),
     &          j=1,12),'  !  '
          end if
          write (IO,'(a5)') '!END!'
        else
        end if
      end do
9160  format (1x,a13,i2,a9)
9161  format (1x,a13,f6.1,a9)
9162  format (1x,i2,a8,6(f8.3,', '),f8.3,a9)
      write (IO,*)

      write (IO,*) linebreak
      write (IO,*) 'INPUT GROUP:  17'
      write (IO,9170) '  ! NREC = ', numrec,' !'
      write (IO,*)
      write (IO,'(a5)') '!END!'
      do i=1,numrec
        write (IO,9171) i,'  ! X = ',(AXR(i)/1000.),(AYR(i)/1000.),
     &                  AZELEV(i),' !  !END!'
      end do
9170  format (1x,a11,i5,a2)
9171  format (1x,i4,a8,2(f9.3,', '),f9.3,a9)
      write (IO,*)

      CLOSE (IO)

      RETURN
      END


