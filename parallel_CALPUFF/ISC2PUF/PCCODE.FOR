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
C                    - MACRO construct removed for LF95  - 8/02
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


      SUBROUTINE DATIME ( DCALL, TCALL )
C***********************************************************************
C                 DATIME Module
C
C        PURPOSE: Obtain the system date and time
C
C        PROGRAMMER: Jim Paumier, PES, Inc.
C
C        DATE:    April 15, 1994
C
C        INPUTS:  none
C
C        OUTPUTS: Date and time in character format
C
C        MODIFIED:   D.G. Strimaitis, EARTH TECH
C                    - MACRO construct removed for LF95  - 8/02
C
C        CALLED FROM:  RUNTIME
C***********************************************************************
C
C     Variable Declarations
c$IF DEFINED (MICRO)
c      INTEGER*2 IPTHR, IPTMIN, IPTSEC, IPTHUN, IPTYR, IPTMON, IPTDAY
c$ENDIF
      CHARACTER DCALL*8, TCALL*11

      DCALL = ' '
      TCALL = ' '

C     The Following Statements Support Either the Microsoft or
C        the Lahey Fortran Compilers for the PC Versions of the Code.
C        For Porting Model to Other Systems, Change Date & Time Function
C        Calls

c$IF DEFINED (MICRO)
c         CALL GETDAT(IPTYR, IPTMON, IPTDAY)
c         CALL GETTIM(IPTHR, IPTMIN, IPTSEC, IPTHUN)
c$ELSEIF DEFINED (LAHEY)
         CALL DATE(DCALL)
         CALL TIME(TCALL)
c$ENDIF


c$IF DEFINED (MICRO)
cC        Convert Year to Two Digits
c         IPTYR = IPTYR - 100 * INT(IPTYR/100)
cC        Write Date and Time to Character Variables, DCALL & TCALL
c         WRITE(DCALL, '(2(I2.2,1H/),I2.2)' ) IPTMON, IPTDAY, IPTYR
c         WRITE(TCALL, '(2(I2.2,1H:),I2.2, 1H.,I2.2)' ) IPTHR, IPTMIN,
c     &                                                IPTSEC, IPTHUN
c$ENDIF

      RETURN
      END

      SUBROUTINE FILOPN
C***********************************************************************
C                 FILOPN Module
C
C        PURPOSE: Obtain the system date and time
C
C        PROGRAMMER: Roger Brode, PES, Inc.
C
C        DATE:    December 6, 1994
C
C        INPUTS:  Input filename, INPFIL
C                 Output filename, OUTFIL
C
C        OUTPUTS: Openned files
C
C        CALLED FROM:  HEADER
C
C        MODIFIED:   D.G. Strimaitis, EARTH TECH
C                    - MACRO construct removed for LF95  - 8/02
C
C        ERROR HANDLING:   Checks errors openning files
C***********************************************************************
C
C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'

C     OPEN Input Runstream File, Unit INUNIT=5
      DUMMY = 'RUN-STRM'
      OPEN (UNIT=INUNIT,FILE=INPFIL,ERR=99,STATUS='OLD')

cvrt  OPEN CALPUFF INPUT FILE.
      DUMMY = 'OUTPUT'
c$IF DEFINED (MICRO)
c      OPEN (UNIT=IOCPUNIT,FILE=OUTFIL,ERR=99,STATUS='UNKNOWN')
c$ELSEIF DEFINED (LAHEY)
      OPEN (UNIT=IOCPUNIT,FILE=OUTFIL,CARRIAGE CONTROL='FORTRAN',
     &      ERR=99,STATUS='UNKNOWN')
c$ENDIF

C     OPEN Print Output File, Unit IOUNIT=6
      DUMMY = 'OUTPUT'
c$IF DEFINED (MICRO)
c      OPEN (UNIT=IOUNIT,FILE=dummy,ERR=99,STATUS='UNKNOWN')
c$ELSEIF DEFINED (LAHEY)
      OPEN (UNIT=IOUNIT,FILE=dummy,CARRIAGE CONTROL='FORTRAN',
     &      ERR=99,STATUS='UNKNOWN')
c$ENDIF

C     Write Out Update to the Screen
      WRITE(*,909)
cvrt 909  FORMAT('+','Now Processing SETUP Information')
 909  FORMAT('+','Now CONVERTING ISC input to CALPUFF input')

      GO TO 1000

C     WRITE Error Message:  Error Opening File
 99   CALL ERRHDL(PATH,MODNAM,'E','500',DUMMY)

      write(*,*) 'Input File : ', inpfil
      write(*,*) 'Output file: ', outfil
      pause


C     Check for Error Opening Runstream File and STOP
      IF (DUMMY .EQ. 'RUN-STRM') THEN
         WRITE(*,919)
 919     FORMAT('+','Error Opening Runstream Input File!  Aborting.')
         STOP
      END IF

 1000 CONTINUE

      RETURN
      END

      SUBROUTINE HEADER
C***********************************************************************
C                 HEADER Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Control Page Feed and Header Information for
C                 Printed File Output
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    September 28, 1993
C
C        MODIFIED:   Replace DEPLETE parameter for plume depletion option
C                    with DDPLETE and WDPLETE in the list of model options
C                    for Wet & Dry depletion.
C                    D. Strimaitis, SRC - 11/8/93
C
C        MODIFIED:   Header modified for draft version of model with new
C                    area source and deposition algorithms - 9/28/93
C
C        MODIFIED:   To add DEPLETE parameter for plume depletion option
C                    to the list of model options
C                    D. Strimaitis, SRC - 2/15/93
C        INPUTS:  Page Number from COMMON
C
C        OUTPUTS: Page Feed and Header
C
C        MODIFIED:   D.G. Strimaitis, EARTH TECH
C                    - MACRO construct removed for LF95  - 8/02
C
C        CALLED FROM:  (This Is An Utility Program)
C***********************************************************************
C
C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
c$IF DEFINED (MICRO)
c      CHARACTER*1 FFEED
c$ENDIF
      CHARACTER RUNDAT*8, RUNTIM*8, DCALL*8, TCALL*11
      COMMON /DATTIM/ RUNDAT, RUNTIM

C     Variable Initializations
      MODNAM = 'HEADER'

C     Increment Page Number Counter
      IPAGE = IPAGE + 1

C     Retrieve Date and Time Variables for First Call
      IF (IPAGE .EQ. 1) THEN
         RUNDAT = ' '
         RUNTIM = ' '

C        Get Date and Time using system-specific functions  ---   CALL DATIME
         CALL DATIME (DCALL, TCALL)

C        Store to variables in COMMON, and trim hundreths of seconds from TCALL
         RUNDAT = DCALL
         WRITE(RUNTIM,'(A8)') TCALL(1:8)
      END IF

C     Write Header to Printed Output File
c$IF DEFINED (MICRO)
cC     Assign ASCII Form Feed Character to Variable FFEED
c      FFEED = CHAR(12)
c      WRITE(IOUNIT,9028) FFEED, VERSN, TITLE1, RUNDAT
c$ELSE
      WRITE(IOUNIT,9028) VERSN, TITLE1, RUNDAT
c$ENDIF
      WRITE(IOUNIT,9029) TITLE2, RUNTIM
      WRITE(IOUNIT,9030) IPAGE
      WRITE(IOUNIT,9040) (MODOPS(I),I=1,17)

c$IF DEFINED (MICRO)
cC     Write an ASCII Form Feed Character (as Variable FFEED) for
cC     Carriage Control With The Microsoft Version.
c 9028 FORMAT(A1,'  *** ISCST3 - VERSION ',A5,' ***',4X,'*** ',A68,
c     &        ' ***',8X,A8)
c$ELSE
 9028 FORMAT('1',' *** ISCST3 - VERSION ',A5,' ***',4X,'*** ',A68,
     &       ' ***',8X,A8)
c$ENDIF
 9029 FORMAT(36X,'*** ',A68,' ***',8X,A8)
 9030 FORMAT(120X,'PAGE ',I3)
 9040 FORMAT(1X,'**MODELOPTs:',17(1X,A6)/)

      RETURN
      END
