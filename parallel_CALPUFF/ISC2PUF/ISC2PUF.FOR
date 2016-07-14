      PROGRAM ISC2PUF
C***********************************************************************
C         ISC2PUF - ISC3 to CALPUFF Input File Conversion Utility
C                       (Version 1.1 Dated 001101)
C
C       ISC2PUF takes an ISCST3 input file and converts the ISC3 switches
C       to the appropriate CALPUFF variables.  This conversion utility uses
C       many of the routines contained in ISC3 regarding the input, reading 
C       and processing of the ISC3 input file.  A new subroutine called 
C       CONV2PUF was created to create the conversions and write the new
C       CALPUFF input file.  It must be noted that the user must still manually
C       edit the CALPUFF input file to set the switches and variables not found
C       in ISC3 or not convertable to proper CALPUFF input variables.
C
c
c ---    V1.1, L001101 - Modified by D. Strimaitis, Earth Tech
c        (1) VOLEMARB source data revised
c        (2) XLAT, XLONG removed
c        (3) IBTZ replaces XTZ (moved to group 1)
c        (4) MBDW switch added (ISC or PRIME downwash)
c        (5) PRIME variables BUILDLEN, XBADJ, YBADJ added when present
c            in ISC file
c
c ---    V1.1, L980731 - Modified by D. Strimaitis, Earth Tech
c        (1) SVMIN,SWMIN removed so that new default arrays are assumed
c
c ---    V1.1, L980717 - Modified by D. Strimaitis, Earth Tech
c        (1) Test for building dnwash fixed in CONV2CAL
c
c ---    V1.1, L971219 - Modified by D. Strimaitis, Earth Tech
c        (1) Default setting updated
c        (2) Full ISC pathnames supported
c        (3) Parameters NSPT1, NSAR1, NSVL1, NSLN1 added
c        (4) Integer fields increased for writing source data
c
c ---    V1.1, L970930 - Modified by E.Insley - 9/97
c
c        (1) Large and small emission rates are written out using 
c            exponential rather than fixed format.
c        (2) Order of building dimensions changed to output building
c            heights first, then building widths.  This change was
c            cosmetic, but it maintains consistency with ISCST3 and 
c            BPIP.
C 
C
C ---    V1.0:
C        PROGRAMMED BY: Vincent R. Tino
C                       EARTH TECH, Inc.
C                       196 Baker Avenue
C                       Concord, MA 01742
C
C        DATE:    February 23 1996

C***********************************************************************
cvrt     PROGRAM ISCST3
C***********************************************************************
C                    ISC3 Short Term Model - ISCST3
C                         (Version Dated 95250)
C
C       This version of ISCST3 contains a revised AREA SOURCE algorithm
C       and revised DEPOSITION algorithm.  These Supplement C revisions had
C       been implemented independently, but are now merged in this version.
C       This version also includes a wet deposition algorithm, and open
C       pit source algorithm, and incorporates the COMPLEX1 algorithms for
C       receptor elevations above the release height.
C
C        PROGRAMMED BY: Roger W. Brode, Jayant A. Hardikar,
C                       and James O. Paumier
C                       Pacific Environmental Services, Inc.
C                       5001 S. Miami Blvd., Suite 300
C                       P.O. Box 12077
C                       Research Triangle Park, North Carolina  27709
C
C                       Deposition portions by:
C
C                       Yicheng Zhuang and David G. Strimaitis
C                       Sigma Research Corperation
C                       196 Baker Ave
C                       Concord, MA 01742
C
C        DATE:    September 7, 1995
C
C***********************************************************************
C
C        Version History of ISCST3
C
C          Date     Version    Alias      Developed From
C        --------   -------   -------    ---------------------
C        09-07-95   95250     ISCST3     ISCSTDFT (dated 94340)
C        12-06-94   94340     ISCSTDFT   DEPST    (dated 94067)
C                                        AREA-ST  (dated 93188), and
C                                        ISCST2   (dated 93109)
C        03-08-94   94067     DEPST      DEPST    (dated 94006)
C        01-06-94   94006     DEPST      ISCST2D  (dated 93046)
C        02-15-93   93046     ISCST2D    ISCST2   (dated 92273)
C        09-29-92   92273     ISCST2     ISCST2   (dated 92062)
C        03-02-92   92062     ISCST2
C
C        ISCSTDFT was formerly known as ISC-COMPDEP.
C
C***********************************************************************
C
C                 MAIN Module of the ISC3 Short Term Model - ISCST3
C                             PC VERSION Dated 95250
C
C        PURPOSE: Controls Overall Flow and Processing of ISCST3 Model
C
C        PROGRAMMED BY: Roger W. Brode
C                       Pacific Environmental Services, Inc.
C                       5001 S. Miami Blvd., Suite 300
C                       P.O. Box 12077
C                       Research Triangle Park, North Carolina  27709
C
C        DATE: September 7, 1995
C
C        INPUTS:  Command Line Options
C
C        OUTPUTS: Model Results
C
C        The ISC3 models have been developed for the U.S. Environmental
C        Protection Agency under Contract No. 68D30032.  The PES Project
C        Manager is Roger W. Brode.  For instructions on running the model,
C        refer to the User's Guide for the Industrial Source Complex (ISC3)
C        Dispersion Models, Volume I - User Instructions (EPA-454/B-95-003a).
C
C***********************************************************************
C
C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'

C     Variable Initializations                              ---   CALL VARINI
      CALL VARINI
      MODNAM = 'MAIN'
cvrt

      write (*,9000)
9000  format (15x,'***********************************************',/
     &       ,15x,'*                                             *',/ 
     &       ,15x,'*                  ISC2PUF                    *',/ 
     &       ,15x,'*                                             *',/ 
     &       ,15x,'*         ISCST to CALPUFF INPUT FILE         *',/ 
     &       ,15x,'*              CONVERSION UTILITY             *',/ 
     &       ,15x,'*                                             *',/ 
     &       ,15x,'*         Version: 1.1    Level: 001101       *',/ 
     &       ,15x,'*                                             *',/
     &       ,15x,'***********************************************',//)

cvrt
C     Open the Temporary File for Error Messages Generated from the Program
      OPEN(UNIT=IERUNT,FILE='ERRMSG.TMP',STATUS='UNKNOWN')

C     Close and Delete The Error Message Temporary Files To Avoid Re-reading
C     Messages if File Already Exists and READ Precedes WRITE.
      CLOSE(IERUNT,STATUS='DELETE')

C     Re-Open the Temporary File for Error Messages Generated from the Program
      OPEN(UNIT=IERUNT,FILE='ERRMSG.TMP',STATUS='UNKNOWN')

C     OPEN The Temporary File to Store Events for EVENT File;
C     Also Used to Store High Values for Summary Tables
      OPEN(UNIT=ITEVUT,FILE='EVENT.TMP',STATUS='UNKNOWN')
C     Initialize the Event Counter
      IEVENT = 0

C     Retrieve Input and Output File Names From Command Line,
C     ---   CALL GETCOM
      CALL GETCOM (' ISC2PUF',INPFIL,OUTFIL)

C     Open Input and Output Files                           ---   CALL FILOPN
      CALL FILOPN

C     Process The Model Setup Information                   ---   CALL SETUP
      CALL SETUP

C     Determine Number of Setup Messages by Message Type    ---   CALL TERRST
      CALL TERRST

      IF (.NOT.RUN .OR. FATAL .OR. IWRN .GT. 0) THEN
C        Write Out Summary Of Setup Error/Message Stats     ---   CALL SUMTBL
         WRITE(IOUNIT,9111)
 9111    FORMAT(//2X,'*** Message Summary For ISC3 Model Setup ***'/)
         CALL SUMTBL
      END IF

      IF (FATAL) THEN
         WRITE(IOUNIT,9112)
 9112    FORMAT(/4X,'**************************************',
     &          /4X,'*** SETUP Finishes UN-successfully ***',
     &          /4X,'**************************************'/)
      ELSE
         WRITE(IOUNIT,9113)
 9113    FORMAT(/1X,'***********************************',
     &          /1X,'*** SETUP Finishes Successfully ***',
     &          /1X,'***********************************'/)
      END IF

C     Print Summary of the Input Data                       ---   CALL INPSUM
cvrt      CALL INPSUM

cvrtcvrtcvrtcvrtcvrtcvrtcvrtcvrtcvrtcvrtcvrtcvrtcvrtcvrtcvrtcvrtcvrtcvrtcvrtcvrt
cvrt  NOW CONVERT TO CALPUFF VARIABLES

      call conv2cal

cvrtcvrtcvrtcvrtcvrtcvrtcvrtcvrtcvrtcvrtcvrtcvrtcvrtcvrtcvrtcvrtcvrtcvrtcvrtcvrt

      CALL HEADER
      WRITE(IOUNIT,9114)
 9114 FORMAT(/1X,'*** Message Summary : ISCST3 Model Execution ***'/)
C     Determine Number of Errors/Messages by Message Type   ---   CALL TERRST
      CALL TERRST
C     Write Summary of Message Stats for Model Execution    ---   CALL SUMTBL
      CALL SUMTBL

      IF (FATAL .OR. RUNERR) THEN
         WRITE(IOUNIT,9115)
 9115    FORMAT(/4X,'***************************************',
     &          /4X,'*** ISCST3 Finishes UN-successfully ***',
     &          /4X,'***************************************'/)
      ELSE
         WRITE(IOUNIT,9116)
 9116    FORMAT(/4X,'************************************',
     &          /4X,'*** ISCST3 Finishes Successfully ***',
     &          /4X,'************************************'/)
      END IF

      IF (ERRLST) THEN
C        OPEN and Write Out Permanent Error Message File    ---   CALL MSGWRT
         OPEN(UNIT=IERWRT,FILE=MSGFIL,STATUS='UNKNOWN',
     &        FORM='FORMATTED')
         CALL MSGWRT
cvrt         CLOSE(IERWRT)
         CLOSE(IERWRT,STATUS='DELETE')
      END IF

C     Close and Delete The Error Message And EVENT Temporary Files

      CLOSE(IERUNT,STATUS='DELETE')
      CLOSE(ITEVUT,STATUS='DELETE')

cvrt close and delete ALL the isc output files
      do ii=8,1000
        CLOSE(ii,STATUS='DELETE')
      end do
      CLOSE(IOUNIT,STATUS='DELETE')

CVRT  CLOSE THE CALPUFF INPUT FILE
      CLOSE(IOCPUNIT)
cvrt

      STOP
      END


      SUBROUTINE JULIAN(IYR,IMN,IDY,JDY)
C***********************************************************************
C                 JULIAN Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE:    CONVERT YR/MN/DY DATE TO JULIAN DAY (1-366),
C                    INCLUDES TEST FOR 100 AND 400 YEAR CORRECTIONS TO
C                    HANDLE 4 DIGIT YEARS BEYOND 2099 AND BEFORE 1901
C                    (WILL WORK WITH 2 DIGIT YR FOR PERIOD 1901-2099)
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        INPUTS:     YEAR,  IYR (2 OR 4 DIGIT)
C                    MONTH, IMN
C                    DAY,   IDY
C
C        OUTPUT:     JULIAN DAY,  JDY (1-366)
C
C        CALLED FROM:   DAYRNG
C
C        ERROR HANDLING:   Checks for Invalid Month or Day
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'
      INTEGER NDAY(12), IDYMAX(12)

C     Variable Initializations
      DATA NDAY/0,31,59,90,120,151,181,212,243,273,304,334/
      DATA IDYMAX/31,29,31,30,31,30,31,31,30,31,30,31/
      MODNAM = 'JULIAN'
      JDY = 0

C     Check for 2-digit Year Input and WRITE Warning Message
      IF (IYR .LT. 100) THEN
C        WRITE Warning Message  !  Routine Will Work for Years 1901-2099
         CALL ERRHDL(PATH,MODNAM,'W','360',KEYWRD)
      END IF

C     Check for Invalid Month or Day
      IF (IMN.LT.1 .OR. IMN.GT.12) THEN
C        WRITE Error Message    !  Invalid Month
         CALL ERRHDL(PATH,MODNAM,'E','203','MONTH')
         GO TO 999
      ELSE IF (IDY .GT. IDYMAX(IMN)) THEN
C        WRITE Error Message    !  Invalid Day
         CALL ERRHDL(PATH,MODNAM,'E','203','DAY')
         GO TO 999
      END IF

C     Determine JULIAN Day Number; For Non-Leap Year First
      IF ((MOD(IYR,4) .NE. 0) .OR.
     &    (MOD(IYR,100) .EQ. 0 .AND. MOD(IYR,400) .NE. 0)) THEN
C        Not a Leap Year
         IF (IMN.NE.2 .OR. (IMN.EQ.2 .AND. IDY.LE.28)) THEN
            JDY = IDY + NDAY(IMN)
         ELSE
C           WRITE Error Message    !  Invalid Date; 2/29 in a Non-Leap Year
            CALL ERRHDL(PATH,MODNAM,'E','370',' ')
         END IF
      ELSE
C        Leap Year
         JDY = IDY + NDAY(IMN)
         IF (IMN .GT. 2)  JDY = JDY + 1
      END IF

 999  CONTINUE

      RETURN
      END

      SUBROUTINE GREGOR(IYR,IMN,JDY,IDY)
C***********************************************************************
C                 GREGOR Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE:    CONVERT JULIAN DAY (1-366) TO DAY OF MONTH,
C                    INCLUDES TEST FOR 100 AND 400 YEAR CORRECTIONS TO
C                    HANDLE 4 DIGIT YEARS BEYOND 2099 AND BEFORE 1901
C                    (WILL WORK WITH 2 DIGIT YR FOR PERIOD 1901-2099)
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        INPUTS:     YEAR,       IYR (2 OR 4 DIGIT)
C                    MONTH,      IMN
C                    JULIAN DAY, JDY (1-366)
C
C        OUTPUT:     DAY OF MONTH, IDY
C
C        CALLED FROM:   METEXT
C
C        ERROR HANDLING:   Checks for Invalid Month or Day
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'
      INTEGER NDAY(12)

C     Variable Initializations
      DATA NDAY/0,31,59,90,120,151,181,212,243,273,304,334/
      MODNAM = 'GREGOR'

C     Check for Invalid Month or Julian Day
      IF (IMN.LT.1 .OR. IMN.GT.12) THEN
C        WRITE Error Message    ! Invalid Month
         CALL ERRHDL(PATH,MODNAM,'E','203','MONTH')
         GO TO 999
      ELSE IF (JDY.LT.1 .OR. JDY.GT.366) THEN
C        WRITE Error Message    ! Invalid Julian Day
         CALL ERRHDL(PATH,MODNAM,'E','203','Juli Day')
         GO TO 999
      END IF

C     Determine Day-of-Month Number; For Non-Leap Year First
      IF ((MOD(IYR,4) .NE. 0) .OR.
     &    (MOD(IYR,100).EQ.0 .AND. MOD(IYR,400).NE.0)) THEN
C        Not a Leap Year
         IDY = JDY - NDAY(IMN)
      ELSE
C        Leap Year
         IDY = JDY - NDAY(IMN)
         IF (IMN .GT. 2)  IDY = IDY - 1
      END IF

 999  CONTINUE

      RETURN
      END
      
      SUBROUTINE HRQEXT (IS)
C***********************************************************************
C*                  HRQEXT Module of AERMOD
C* 
C*         PURPOSE: To Assign Hourly Source Parameters
C* 
C*         PROGRAMMER:  Jayant Hardikar, Roger Brode
C* 
C*         DATE:    September 15, 1993
C* 
C*         INPUTS:  Variable QFLAG and Current Source Number Being Processed
C* 
C*         OUTPUTS: Source Arrays
C*          
C*         MODIFIED:  REMOVED THE 'POINT' SOURCE CONDITION, SO IT APPLIES 
C*                    TO ALL SOURCE TYPES, EXCEPT SAVING THE TEMP & VEL
C* 
C*         CALLED FROM:  HRLOOP
C************************************************************************
C*
C*    Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'
      CHARACTER RDFRM*20

      CHARACTER*8 HRSOID

C*    Variable Initializations
      MODNAM = 'HRQEXT'
C*
C*    READ Record to Buffers, A80 and 80A1
C*    Length of ISTRG is Set in PARAMETER Statement in MAIN1.INC
C     Setup READ format and ECHO format for runstream record,
C     based on the ISTRG PARAMETER (set in MAIN1.INC)
      WRITE(RDFRM,9100) ISTRG, ISTRG
 9100 FORMAT('(A',I3.3,',T1,',I3.3,'A1)')
      READ (IHREMI,RDFRM,ERR=99,END=999) RUNST1, (RUNST(I), I=1, ISTRG)
C*
C*    Convert Lower Case to Upper Case Letters              ---   CALL LWRUPR
      CALL LWRUPR
C*      
C*    Define Fields on Card                                 ---   CALL DEFINE
      CALL DEFINE
C*
C*    Get the Contents of the Fields                        ---   CALL GETFLD
      CALL GETFLD
C*
C*    Check for number of fields - error if less than 7.
      IF (IFC .LT. 7) THEN
         CALL ERRHDL(PATH,MODNAM,'E','201','HOUREMIS')
         GO TO 999
      END IF
C*         
C*    Assign the Feilds to Local Varables and Check The Numerical Field
C*
      CALL STONUM(FIELD(3), 40, HYEAR, IMIT)
      IHYEAR = HYEAR
      IF (IMIT .NE. 1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
      END IF

      CALL STONUM(FIELD(4), 40, HMON, IMIT)
      IHMON = HMON
      IF (IMIT .NE. 1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
      END IF

      CALL STONUM(FIELD(5), 40, HDAY, IMIT)
      IHDAY = HDAY
      IF (IMIT .NE. 1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
      END IF

      CALL STONUM(FIELD(6), 40, HHOUR, IMIT)
      IHHOUR = HHOUR
      IF (IMIT .NE. 1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
      END IF

      HRSOID = FIELD(7)

      IF (IFC .GE. 8) THEN
         CALL STONUM(FIELD(8), 40, HRQS, IMIT)
         IF (IMIT .NE. 1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         END IF
      ELSE
C*       Emission rate is missing - set to zero
         HRQS = 0.0
      END IF

      IF (IFC.EQ.10) THEN
C*       Also Assign Exit Temperature and Exit Velocity
         CALL STONUM(FIELD(9), 40, HRTS, IMIT)
         IF (IMIT .NE. 1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         END IF

         CALL STONUM(FIELD(10), 40, HRVS, IMIT)
         IF (IMIT .NE. 1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         END IF
      ELSE
C*       Some missing parameters - assign zeros to all
         HRTS = 0.0
         HRVS = 0.0
      ENDIF

C*    Check for Date and Time Consistency ; If Failed - Abort Program
      KURHRQ = IHYEAR*1000000 + IHMON*10000 + IHDAY*100 + IHHOUR
      IF (KURDAT .NE. KURHRQ ) THEN
C*       WRITE Error Message - Date mismatch
         WRITE(DUMMY,'(I8)') KURDAT
         CALL ERRHDL(PATH,MODNAM,'E','455',DUMMY)
         RUNERR = .TRUE.
      END IF


C*    Check for Source ID Consistency ; If Failed - Abort Program
      IF ( HRSOID .NE. SRCID(IS) ) THEN
         WRITE(DUMMY,'(A8)') SRCID(IS)
         CALL ERRHDL(PATH,MODNAM,'E','342',SRCID(IS))
         RUNERR = .TRUE.
      ENDIF

C*    Assign the Hourly Emission Parameters to the Stack Variables
      AQS(IS) = HRQS

      IF (SRCTYP(IS) .EQ. 'POINT') THEN
         ATS(IS) = HRTS
         AVS(IS) = HRVS
      ENDIF


C*    Perform QA Error Checking on Source Parameters
C*

      IF (ATS(IS) .EQ. 0.0) THEN
C*       Set Temperature to Small Negative Value for Ambient Releases
         ATS(IS) = -1.0E-5
      ELSE IF (ATS(IS) .GT. 2000.0) THEN
C*       WRITE Informational Message:  Exit Temp. > 2000K
         CALL ERRHDL(PATH,MODNAM,'I','320','HRTS')
      END IF

      IF (SRCTYP(IS) .EQ. 'POINT') THEN
         IF (AVS(IS) .LT. 0.0) THEN
C*          WRITE Informational Message:  Negative or Zero Exit Velocity
            CALL ERRHDL(PATH,MODNAM,'I','325','HRVS')
C*          Set to Small Value to Avoid Zero-divide and Underflow
            AVS(IS) = 1.0E-5
         ELSE IF (AVS(IS) .LT. 1.0E-5) THEN
C*          Set to Small Value to Avoid Zero-divide and Underflow
            AVS(IS) = 1.0E-5
         ELSE IF (AVS(IS) .GT. 50.0) THEN
C*          WRITE Informational Message:  Exit Velocity > 50.0 m/s
            CALL ERRHDL(PATH,MODNAM,'I','320','HRVS')
         END IF
      ENDIF

      GO TO 999

C*    Write Error Message for Error Reading Hourly Emissions File
 99   CALL ERRHDL(PATH,MODNAM,'E','510','HOUREMIS')
      RUNERR = .TRUE.

999   RETURN
      END


      SUBROUTINE ERRHDL(PATHWY,MODNAM,INERTP,INERCD,INPMSG)
C***********************************************************************
C                 ERRHDL Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: A General Error Handling Procedure
C
C        PROGRAMMER: Jeff Wang
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Error Code, Occur Locations
C
C        OUTPUTS: Error Message, Error Statistics..etc.
C
C        CALLED FROM:  (This Is An Utility Programm)
C***********************************************************************
C
C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'
      CHARACTER ERRMG1*50, PATHWY*2, INERTP*1, INERCD*3, ICODE*3,
     &          INPMSG*(*)
      LOGICAL FIND

C     Variable Initializations
      IERROR = IERROR + 1
      FIND = .FALSE.
      I = 1

C     Check for Occurrence of 'E' Error Type, and Set FATAL Switch
      IF (INERTP .EQ. 'E') THEN
         FATAL = .TRUE.
         NFATAL = NFATAL + 1
         IF (NFATAL .EQ. 999) THEN
C           Number Of Fatal Errors Has Reached Limit of 999
            ERRMG1 = 'Number of Fatal Errors Has Reached Limit of 999'
            MODNAM = 'ERRHDL'
            ICODE = '999'
            INPMSG = ' '
            WRITE(IERUNT,1111) PATHWY,INERTP,ICODE,ILINE,MODNAM,ERRMG1,
     &                         INPMSG
            GO TO 999
         ELSE IF (NFATAL .GT. 999) THEN
C           Skip Any More Error WRITEs
            GO TO 999
         END IF
      END IF

C     Go To Match The Error Massage
      DO WHILE (.NOT.FIND .AND. I.LE.IERRN)
         IF (INERCD .EQ. ERRCOD(I)) THEN
            IERRDX = I
            ERRMG1 = ERRMSG(I)
            FIND = .TRUE.
         END IF
         I = I + 1
      END DO

      IF (.NOT. FIND) THEN
         WRITE(ERRMG1,1001)
 1001    FORMAT('SYSTEM ERROR: MESSAGE NOT FOUND FOR THIS NUMBER!')
      END IF

C     Write Out The Error Message
      WRITE(IERUNT,1111) PATHWY,INERTP,INERCD,ILINE,MODNAM,ERRMG1,INPMSG
 1111 FORMAT(A2,1X,A1,A3,I6,1X,A6,':',A50,1X,A8)

 999  RETURN
      END

      SUBROUTINE TERRST
C***********************************************************************
C                 TERRST Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: To Determine Total Error/Message Statistics
C
C        PROGRAMMER:  Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Error Message Temporary File
C
C        OUTPUTS: Total Number of Messages by Message Type
C
C        CALLED FROM:  This is A Utility Program
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'
      CHARACTER ERRTP*1, ERRCD*3, ERRMG1*50, ERRMG2*8, INPFLD*3

C     Variable Initialization
      MODNAM = 'TERRST'
      IFTL = 0
      IWRN = 0
      INFO = 0
      ICLM = 0
      IMSG = 0
      EOF = .FALSE.

C     Rewind the Temporary Error/Message File
      REWIND IERUNT

      DO WHILE (.NOT. EOF)
         READ(IERUNT,1116,END=99,ERR=9999) PATH,ERRTP,ERRCD,IERRLN,
     &                                     MODNAM,ERRMG1,ERRMG2

C        Sort Error Group And Find The Index
         INPFLD = ERRCD
         CALL STONUM(INPFLD,3,FNUM,IMIT)

         IF (ERRTP .EQ. 'E') THEN
            IFTL = IFTL + 1
         ELSE IF (ERRTP .EQ. 'W') THEN
            IWRN = IWRN + 1
         ELSE IF (ERRTP .EQ. 'I') THEN
            INFO = INFO + 1
            IF (INT(FNUM) .EQ. 440) THEN
C              Message for Calm Hour, Increment Calm Counter
               ICLM = ICLM + 1
            END IF
            IF (INT(FNUM) .EQ. 460) THEN
C              Message for Calm Hour, Increment Calm Counter
               IMSG = IMSG + 1
            END IF
         END IF

         GO TO 11
 99      EOF = .TRUE.
 11      CONTINUE
      END DO

 1116 FORMAT(A2,1X,A1,A3,I6,1X,A6,1X,A50,1X,A8)

C     Use BACKSPACE To Reposition Temporary Error Message File Ahead of EOF;
C     This Is Needed in Order To Allow For Additional Message Writes
      BACKSPACE IERUNT

      GO TO 1000

C     WRITE Error Message: Error Reading Temp Error Message File
 9999 CALL ERRHDL(PATH,MODNAM,'E','510','ERRORMSG')

 1000 RETURN
      END

      SUBROUTINE SUMTBL
C***********************************************************************
C                 SUMTBL Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: To Print Out The Error Summary Table
C
C        PROGRAMMER:  Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Error Message Temporary File
C
C        OUTPUTS: Summary Of Errors
C
C        CALLED FROM:  This is A Utility Program
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'
      CHARACTER ERRTP*1, ERRCD*3, ERRMG1*50, ERRMG2*8

C     Variable Initialization
      MODNAM = 'SUMTBL'

C     Write Out The Total Error Statistics
      WRITE(IOUNIT,*) ' --------- Summary of Total Messages --------'
      WRITE(IOUNIT,*) ' '
      WRITE(IOUNIT,*) 'A Total of ',IFTL,' Fatal Error Message(s)'
      WRITE(IOUNIT,*) 'A Total of ',IWRN,' Warning Message(s)'
      WRITE(IOUNIT,*) 'A Total of ',INFO,' Informational Message(s)'
      IF (ICLM .GT. 0) THEN
         WRITE(IOUNIT,*) ' '
         WRITE(IOUNIT,*) 'A Total of ',ICLM,' Calm Hours Identified'
      END IF
      IF (IMSG .GT. 0) THEN
         WRITE(IOUNIT,*) ' '
         WRITE(IOUNIT,*) 'A Total of ',IMSG,' Missing Hours Identified'
      END IF
      WRITE(IOUNIT,*) ' '

C     Write Out All The Fatal Error Messages
      WRITE(IOUNIT,*) ' '
      WRITE(IOUNIT,*) '   ******** FATAL ERROR MESSAGES ******** '
      REWIND IERUNT
      EOF = .FALSE.
      J = 0
      DO WHILE (.NOT. EOF)
         READ(IERUNT,1116,END=99,ERR=9999) PATH,ERRTP,ERRCD,IERRLN,
     &                                     MODNAM,ERRMG1,ERRMG2
         IF (ERRTP .EQ. 'E') THEN
            J = J + 1
            WRITE(IOUNIT,1117) PATH,ERRTP,ERRCD,IERRLN,MODNAM,
     &                         ERRMG1,ERRMG2
         END IF
         GO TO 11
 99      EOF = .TRUE.
 11      CONTINUE
      END DO

C     If No Fatal Error Messages, Then Write 'NONE'
      IF (J .EQ. 0) THEN
         WRITE(IOUNIT,*) '              ***  NONE  ***         '
         WRITE(IOUNIT,*) ' '
      END IF

C     Write Out All The Warning Messages
      WRITE(IOUNIT,*) ' '
      WRITE(IOUNIT,*) '   ********   WARNING MESSAGES   ******** '
      REWIND IERUNT
      EOF = .FALSE.
      J = 0
      DO WHILE (.NOT. EOF)
         READ(IERUNT,1116,END=999,ERR=9999) PATH,ERRTP,ERRCD,IERRLN,
     &                                      MODNAM,ERRMG1,ERRMG2
         IF (ERRTP .EQ. 'W') THEN
            J = J + 1
            WRITE(IOUNIT,1117) PATH,ERRTP,ERRCD,IERRLN,MODNAM,
     &                         ERRMG1,ERRMG2
         END IF
         GO TO 111
 999     EOF = .TRUE.
 111     CONTINUE
      END DO

C     If No Warning Messages, Then Write 'NONE'
      IF (J .EQ. 0) THEN
         WRITE(IOUNIT,*) '              ***  NONE  ***        '
         WRITE(IOUNIT,*) ' '
      END IF

 1116 FORMAT(A2,1X,A1,A3,I6,1X,A6,1X,A50,1X,A8)
 1117 FORMAT(1X,A2,1X,A1,A3,I6,1X,A6,':',A50,1X,A8)

C     Use BACKSPACE To Reposition Temporary Error Message File Ahead of EOF;
C     This Is Needed in Order To Allow For Additional Message Writes
      BACKSPACE IERUNT

      GO TO 1000

C     WRITE Error Message: Error Reading Temp Error Message File
 9999 CALL ERRHDL(PATH,MODNAM,'E','510','ERRORMSG')

 1000 RETURN
      END

      SUBROUTINE MSGWRT
C***********************************************************************
C                 MSGWRT Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: To Print Out The Error Summary Table
C
C        PROGRAMMER: Jeff Wang
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Error Message File
C
C        OUTPUTS: The Error Message File
C
C        CALLED FROM:  This is A Utility Program
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'
      CHARACTER ERRTP*1, ERRCD*3, ERRMG1*50, ERRMG2*8

C     Variable Initialization
      MODNAM = 'MSGWRT'

C     Write Out The Header Of The Message File
      WRITE(IERWRT,*) ' '
      WRITE(IERWRT,*) '     *********** Error Message List ********* '
      WRITE(IERWRT,*) ' '
      WRITE(IERWRT,*) '   PW     --- Pathway                           '
      WRITE(IERWRT,*) '   Code   --- Error Type + Error Code           '
      WRITE(IERWRT,*) '   L#     --- The Line Number Where Error Occurs'
      WRITE(IERWRT,*) '   ModNam --- Module Name In Which Error Occurs '
      WRITE(IERWRT,*) '   Hints  --- Hints For The Possible Solution   '
      WRITE(IERWRT,*) '     ***************************************** '
      WRITE(IERWRT,*) ' '
      WRITE(IERWRT,1114)
      WRITE(IERWRT,1115)
 1114 FORMAT('PW CODE  L#  MODNAM  ',18X,'ERROR MESSAGES',20X,'HINTS')
 1115 FORMAT('-- ---- ---- ------  ',50('-'),' --------')
      WRITE(IERWRT,*) ' '
      REWIND IERUNT
      EOF = .FALSE.

      DO WHILE (.NOT. EOF)
         READ(IERUNT,1116,END=99,ERR=999) PATH,ERRTP,ERRCD,IERRLN,
     &                                    MODNAM,ERRMG1,ERRMG2
         WRITE(IERWRT,1117) PATH,ERRTP,ERRCD,IERRLN,
     &                      MODNAM,ERRMG1,ERRMG2
         GO TO 11
 99      EOF = .TRUE.
 11      CONTINUE
      END DO

 1116 FORMAT(A2,1X,A1,A3,I6,1X,A6,1X,A50,1X,A8)
 1117 FORMAT(A2,1X,A1,A3,I6,1X,A6,':',A50,1X,A8)

      GO TO 1000

C     WRITE Error Message: Error Reading Temp Error Message File
 999  CALL ERRHDL(PATH,MODNAM,'E','510','ERRORMSG')

 1000 RETURN
      END

C----------------------------------------------------------------------
C     Courtesy: Jay Sandhu
C               email: jsandhu@esri.com
C
C
C Please cite David H. Douglas, COLLECTED ALGORITHMS, Cambridge MA:
C Harvard Laboratory for Computer Graphics, 1974
C
C This is my reinvention buster.
C 1974 1974 1974 1974 1974 1974 1974 1974 1974 1974 1974 1974
C
C>>>PNPY
C     .................................................................
C
C        SUBROUTINE PNPOLY
C
C        PURPOSE
C           TO DETERMINE WHETHER A POINT IS INSIDE A POLYGON
C
C        USAGE
C           CALL PNPOLY (PX, PY, X, Y, N, INOUT )
C
C        DESCRIPTION OF THE PARAMETERS
C           PX      - X-COORDINATE OF POINT IN QUESTION.
C           PY      - Y-COORDINATE OF POINT IN QUESTION.
C           X       - N LONG VECTOR CONTAINING X-COORDINATES OF
C                     VERTICES OF POLYGON.
C           Y       - N LONG VECTOR CONTAINING Y-COORDINATES OF
C                     VERTICES OF POLYGON.
C           N       - NUMBER OF VERTICES IN THE POLYGON.
C           INOUT   - THE SIGNAL RETURNED:
C                     -1 IF THE POINT IS OUTSIDE OF THE POLYGON,
C                      0 IF THE POINT IS ON AN EDGE OR AT A VERTEX,
C                      1 IF THE POINT IS INSIDE OF THE POLYGON.
C
C        REMARKS
C           THE VERTICES MAY BE LISTED IN CLOCKWISE OR ANTICLOCKWISE
C           ORDER.  FOR THIS SUBROUTINE A POINT IS CONSIDERED INSIDE
C           THE POLYGON IF IT IS LOCATED IN THE ENCLOSED AREA DEFINED
C           BY THE LINE FORMING THE POLYGON.
C           THE INPUT POLYGON MAY BE A COMPOUND POLYGON CONSISTING
C           OF SEVERAL SEPARATE SUBPOLYGONS. IF SO, THE FIRST VERTEX
C           OF EACH SUBPOLYGON MUST BE REPEATED, AND WHEN CALCULATING
C           N, THESE FIRST VERTICES MUST BE COUNTED TWICE.
C           INOUT IS THE ONLY PARAMETER WHOSE VALUE IS CHANGED.
C           PNPOLY CAN HANDLE ANY NUMBER OF VERTICES IN THE POLYGON.
C           WRITTEN BY RANDOLPH FRANKLIN, UNIVERSITY OF OTTAWA, 6/72.
C
C        SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
C           NONE
C
C        METHOD
C           A VERTICAL SEMI-INFINITE LINE IS DRAWN UP FROM THE POINT
C           IN QUESTION. IF IT CROSSES THE POLYGON AN ODD NUMBER OF
C           TIMES, THE POINT IS INSIDE THE POLYGON.
C
C     .................................................................
C
      SUBROUTINE PNPOLY (PX,PY,X,Y,N,INOUT)

      REAL X(N),Y(N)
      LOGICAL IX,IY,JX,JY,EOR

C     EXCLUSIVE OR FUNCTION.
      EOR(IX,IY)=(IX.OR.IY).AND..NOT.(IX.AND.IY)

      INOUT=-1
      DO 4 I=1,N
         XI=X(I)-PX
         YI=Y(I)-PY
C        CHECK WHETHER THE POINT IN QUESTION IS AT THIS VERTEX.
         IF (XI.EQ.0.0.AND.YI.EQ.0.0) GO TO 2
C        J IS NEXT VERTEX NUMBER OF POLYGON.
         J=1+MOD(I,N)
         XJ=X(J)-PX
         YJ=Y(J)-PY
C        IS THIS LINE OF 0 LENGTH ?
         IF (XI.EQ.XJ.AND.YI.EQ.YJ) GO TO 4
         IX=XI.GE.0.0
         IY=YI.GE.0.0
         JX=XJ.GE.0.0
         JY=YJ.GE.0.0
C        CHECK WHETHER (PX,PY) IS ON VERTICAL SIDE OF POLYGON.
         IF (XI.EQ.0.0.AND.XJ.EQ.0.0.AND.EOR(IY,JY)) GO TO 2
C        CHECK WHETHER (PX,PY) IS ON HORIZONTAL SIDE OF POLYGON.
         IF (YI.EQ.0.0.AND.YJ.EQ.0.0.AND.EOR(IX,JX)) GO TO 2
C        CHECK WHETHER BOTH ENDS OF THIS SIDE ARE COMPLETELY 1) TO RIGHT
C        OF, 2) TO LEFT OF, OR 3) BELOW (PX,PY).
         IF (.NOT.((IY.OR.JY).AND.EOR(IX,JX))) GO TO 4
C        DOES THIS SIDE OBVIOUSLY CROSS LINE RISING VERTICALLY FROM (PX,PY
         IF (.NOT.(IY.AND.JY.AND.EOR(IX,JX))) GO TO 1
         INOUT=-INOUT
         GO TO 4

1        IF ((YI*XJ-XI*YJ)/(XJ-XI)) 4,2,3
2        INOUT=0
         RETURN
3        INOUT=-INOUT
4     CONTINUE

      RETURN
      END

      BLOCK DATA INIT
C***********************************************************************
C
C     BLOCK DATA SUBPROGRAM OF THE ISC - Version 2 MODEL
C
C     PURPOSE: Initialize Data in COMMON Blocks
C
C     MODIFIED:  To Include Terrain Grid pathway - 12/15/93
C
C     MODIFIED:  To Include WET DEPOSITION Arrays - 11/8/93
C
C     MODIFIED:  To Include New Area Source Arrays - 7/7/93
C
C     MODIFIED:  For revised DRY DEPOSITION code - 2/15/93
C
C     MODIFIED:  To Include TOXXFILE Option - 9/29/92
C
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN3.INC'


C***********************************************************************
C     Initialize Model Version Number, VERSN (Year, Julian Day), as a
C     Character Variable
C***********************************************************************

      DATA VERSN /'95250'/


C***********************************************************************
C     Input/Output File Units and Input/Output File Names
C***********************************************************************

      DATA INUNIT/ 5/, IOUNIT/ 6/, MFUNIT/19/, IERUNT/10/,
     &     IERWRT/11/, IDPUNT/12/, IDPUN2/14/, IRSUNT/15/,     
     &     IEVUNT/17/, ITEVUT/18/, IZUNIT/13/,
cvrt     &     IHREMI/16/
     &     IHREMI/16/, IOCPUNIT/7/
C*#
      DATA INPFIL/' '/,OUTFIL/' '/


C***********************************************************************
C     Initialize Keyword Array
C***********************************************************************

      DATA (KEYWD(I),I=1,IKN) /'STARTING','FINISHED','TITLEONE',
     &   'TITLETWO','MODELOPT','AVERTIME','POLLUTID','HALFLIFE',
     &   'DCAYCOEF','TERRHGTS','ELEVUNIT','FLAGPOLE','RUNORNOT',
     &   'EVENTFIL','SAVEFILE','INITFILE','MULTYEAR','ERRORFIL',
     &   'LOCATION','SRCPARAM','BUILDHGT','BUILDWID','LOWBOUND',
     &   'BUILDLEN','XBADJ   ','YBADJ   ',
     &   'EMISFACT','EMISUNIT','PARTDIAM','MASSFRAX','PARTDENS',
     &   'PARTSLIQ','PARTSICE','GAS-SCAV','CONCUNIT','DEPOUNIT',
     &   'HOUREMIS',
     &   'SRCGROUP','GRIDCART','GRIDPOLR','DISCCART','DISCPOLR',
     &   'BOUNDARY','BOUNDELV','INPUTFIL','ANEMHGHT','SURFDATA',
     &   'UAIRDATA','STARTEND','DAYRANGE','WDROTATE','WINDPROF',
     &   'DTHETADZ','WINDCATS','RECTABLE','MAXTABLE','DAYTABLE',
     &   'MAXIFILE','POSTFILE','PLOTFILE','TOXXFILE'/


C***********************************************************************
C     Initialize Miscellaneous Variables
C***********************************************************************

      DATA IPROC /366*1/, KAVE /NAVE*0/, EXPLIM /-50.0/
      DATA UCAT /1.54, 3.09, 5.14, 8.23, 10.8/
      DATA MODOPS /17*'      '/


C***********************************************************************
C     Initialize Default Wind Profile Exponents and DTHETADZ
C***********************************************************************

C     STAB. CLASS  A    B     C     D      E      F
C                 ***  ***   ***   ***    ***    ***
      DATA DTURB  /0.,  0.,   0.,   0.,   0.02, 0.035/,
     &     DTRUR  /0.,  0.,   0.,   0.,   0.02, 0.035/,
     &     PURB  /0.15, 0.15, 0.20, 0.25, 0.30, 0.30/,
     &     PRUR  /0.07, 0.07, 0.10, 0.15, 0.35, 0.55/


C***********************************************************************
C     Initialize Receptor Arrays
C***********************************************************************

      DATA AXR/NREC*0.0/, AYR/NREC*0.0/, AZELEV/NREC*0.0/,
     &     AZFLAG/NREC*0.0/


C***********************************************************************
C     Initialize Source Arrays  (Multi-dimensional Arrays Initialized in
C                                SUBROUTINE VARINI)
C***********************************************************************

      DATA AXS/NSRC*0.0/, AYS/NSRC*0.0/, AZS/NSRC*0.0/, AQS/NSRC*0.0/,
     &     AHS/NSRC*0.0/, ADS/NSRC*0.0/, AVS/NSRC*0.0/, ATS/NSRC*0.0/,
     &     ASYINI/NSRC*0.0/, ASZINI/NSRC*0.0/, AXINIT/NSRC*0.0/,
     &     AYINIT/NSRC*0.0/, AANGLE/NSRC*0.0/,
     &     SOPCRD/NSRC*'N'/


C***********************************************************************
C     Initialize Setup Status Arrays
C***********************************************************************

      DATA ICSTAT/20*0/, ISSTAT/20*0/, IRSTAT/20*0/, IMSTAT/20*0/,
     &     IOSTAT/20*0/, ITSTAT/20*0/


C***********************************************************************
C     Initialize Error Code and Message Arrays
C***********************************************************************

      DATA ERRCOD(1)/'100'/,
     &  ERRMSG(1)/'Invalid Pathway Specified. The Troubled Pathway is'/
      DATA ERRCOD(2)/'105'/,
     &  ERRMSG(2)/'Invalid Keyword Specified. The Troubled Keyword is'/
      DATA ERRCOD(3)/'110'/,
     &  ERRMSG(3)/'Keyword is Not Valid for This Pathway.  Keyword is'/
      DATA ERRCOD(4)/'115'/,
     &  ERRMSG(4)/'STARTING or FINISHED Out of Sequence:  Pathway =  '/
      DATA ERRCOD(5)/'120'/,
     &  ERRMSG(5)/'Pathway is Out of Sequence:  Pathway =            '/
      DATA ERRCOD(6)/'125'/,
     &  ERRMSG(6)/'Missing FINISHED-Runstream File Incomplete: ISTAT='/
      DATA ERRCOD(7)/'130'/,
     &  ERRMSG(7)/'Missing Mandatory Keyword.  The Missing Keyword is'/
      DATA ERRCOD(8)/'135'/,
     &  ERRMSG(8)/'Duplicate Nonrepeatable Keyword Specified:Keyword='/
      DATA ERRCOD(9)/'140'/,
     &  ERRMSG(9)/'Invalid Order of Keyword.  The Troubled Keyword is'/
      DATA ERRCOD(10)/'145'/,
     &  ERRMSG(10)/'Conflicting Options: MULTYEAR and Re-Start Option '/
      DATA ERRCOD(11)/'150'/,
     &  ERRMSG(11)/'Conflicting Options: MULTYEAR for Wrong Pollutant '/
      DATA ERRCOD(12)/'155'/,
     &  ERRMSG(12)/'Conflicting Decay Keyword. Inputs Ignored for     '/
      DATA ERRCOD(13)/'160'/,
     &  ERRMSG(13)/'Duplicate ORIG Secondary Keyword for GRIDPOLR:    '/
      DATA ERRCOD(14)/'170'/,
     &  ERRMSG(14)/'Invalid Secondary Keyword for Receptor Grid:      '/
      DATA ERRCOD(15)/'175'/,
     &  ERRMSG(15)/'Missing Secondary Keyword END for Receptor Grid:  '/
      DATA ERRCOD(16)/'180'/,
     &  ERRMSG(16)/'Conflicting Secondary Keyword for Receptor Grid:  '/
      DATA ERRCOD(17)/'185'/,
     &  ERRMSG(17)/'Missing Receptor Keywords. No Receptors Specified.'/
      DATA ERRCOD(18)/'190'/,
     &  ERRMSG(18)/'No Keywords for OU Path and No PERIOD/ANNUAL Aves.'/
      DATA ERRCOD(19)/'195'/,
     &  ERRMSG(19)/'Incompatible Option Used With SAVEFILE or INITFILE'/
      DATA ERRCOD(20)/'200'/,
     &  ERRMSG(20)/'Missing Parameter(s). No Options Specified For    '/
      DATA ERRCOD(21)/'201'/,
     &  ERRMSG(21)/'Not Enough Parameters Specified For the Keyword of'/
      DATA ERRCOD(22)/'202'/,
     &  ERRMSG(22)/'Too Many Parameters Specified For the Keyword of  '/
      DATA ERRCOD(23)/'203'/,
     &  ERRMSG(23)/'Invalid Parameter Specified.  Troubled Parameter: '/
      DATA ERRCOD(24)/'204'/,
     &  ERRMSG(24)/'Option Parameters Conflict.  Forced by Default to '/
      DATA ERRCOD(25)/'205'/,
     &  ERRMSG(25)/'No Option Parameter Setting.  Forced by Default to'/
      DATA ERRCOD(26)/'206'/,
     &  ERRMSG(26)/'Regulatory DFAULT Overrides Non-DFAULT Option For '/
      DATA ERRCOD(27)/'207'/,
     &  ERRMSG(27)/'No Parameters Specified. Default Values Will Used.'/
      DATA ERRCOD(28)/'208'/,
     &  ERRMSG(28)/'Illegal Numerical Field Encountered in            '/
      DATA ERRCOD(29)/'209'/,
     &  ERRMSG(29)/'Negative Value Appears For Non-negative Variable. '/
      DATA ERRCOD(30)/'210'/,
     &  ERRMSG(30)/'Number of Short Term Averages Exceeds Max:  NAVE= '/
      DATA ERRCOD(31)/'211'/,
     &  ERRMSG(31)/'Duplicate Averaging Period Specified for Keyword  '/
      DATA ERRCOD(32)/'212'/,
     &  ERRMSG(32)/'END Encountered Without (X,Y) Points Properly Set '/
      DATA ERRCOD(33)/'213'/,
     &  ERRMSG(33)/'ELEV Input Inconsistent With Option: Input Ignored'/
      DATA ERRCOD(34)/'214'/,
     &  ERRMSG(34)/'ELEV Input Inconsistent With Option: Defaults Used'/
      DATA ERRCOD(35)/'215'/,
     &  ERRMSG(35)/'FLAG Input Inconsistent With Option: Input Ignored'/
      DATA ERRCOD(36)/'216'/,
     &  ERRMSG(36)/'FLAG Input Inconsistent With Option: Defaults Used'/
      DATA ERRCOD(37)/'217'/,
     &  ERRMSG(37)/'More Than One Delimiter In A Field for Keyword    '/
      DATA ERRCOD(38)/'218'/,
     &  ERRMSG(38)/'Number of (X,Y) Points Not Match With Number Of   '/
      DATA ERRCOD(39)/'219'/,
     &  ERRMSG(39)/'Number Of Receptors Specified Exceeds Max:  NREC= '/
      DATA ERRCOD(40)/'220'/,
     &  ERRMSG(40)/'Missing Origin (Use Default = 0,0) In GRIDPOLR    '/
      DATA ERRCOD(41)/'221'/,
     &  ERRMSG(41)/'Missing Distance Setting In Polar Network         '/
      DATA ERRCOD(42)/'222'/,
     &  ERRMSG(42)/'Missing Degree Or Dist Setting In Polar Network   '/
      DATA ERRCOD(43)/'223'/,
     &  ERRMSG(43)/'Missing Distance or Degree Field in               '/
      DATA ERRCOD(44)/'224'/,
     &  ERRMSG(44)/'Number of Receptor Networks Exceeds Max:  NNET=   '/
      DATA ERRCOD(45)/'225'/,
     &  ERRMSG(45)/'Number of X-Coords Specified Exceeds Max:  IXM=   '/
      DATA ERRCOD(46)/'226'/,
     &  ERRMSG(46)/'Number of Y-Coords Specified Exceeds Max:  IYM=   '/
      DATA ERRCOD(47)/'227'/,
     &  ERRMSG(47)/'No Receptors Were Defined on the RE Pathway.      '/
      DATA ERRCOD(48)/'228'/,
     &  ERRMSG(48)/'Default(s) Used for Missing Parameters on Keyword '/
      DATA ERRCOD(49)/'229'/,
     &  ERRMSG(49)/'Too Many Parameters - Inputs Ignored on Keyword   '/
      DATA ERRCOD(50)/'230'/,
     &  ERRMSG(50)/'Not Enough Numerical Values Specified for         '/
      DATA ERRCOD(51)/'231'/,
     &  ERRMSG(51)/'Too Many Numerical Values Specified for           '/
      DATA ERRCOD(52)/'232'/,
     &  ERRMSG(52)/'Number Of Specified Sources Exceeds Maximum: NSRC='/
      DATA ERRCOD(53)/'233'/,
     &  ERRMSG(53)/'Building Dimensions Specified for Non-POINT Source'/
      DATA ERRCOD(54)/'234'/,
     &  ERRMSG(54)/'Too Many Sectors Input for                        '/
      DATA ERRCOD(55)/'235'/,
     &  ERRMSG(55)/'Number of Source Groups Exceeds Maximum:  NGRP=   '/
      DATA ERRCOD(56)/'236'/,
     &  ERRMSG(56)/'Not Enough BUILDHGTs Specified for SourceID       '/
      DATA ERRCOD(57)/'237'/,
     &  ERRMSG(57)/'Not Enough BUILDWIDs Specified for SourceID       '/
      DATA ERRCOD(58)/'238'/,
     &  ERRMSG(58)/'Not Enough LOWBOUNDs Specified for SourceID       '/
      DATA ERRCOD(59)/'239'/,
     &  ERRMSG(59)/'Not Enough QFACTs Specified for SourceID          '/
      DATA ERRCOD(60)/'240'/,
     &  ERRMSG(60)/'Inconsistent Number of Settling/Removal Cats for  '/
      DATA ERRCOD(61)/'242'/,
     &  ERRMSG(61)/'No Particle Size Categories Specified for SRCID   '/
      DATA ERRCOD(62)/'244'/,
     &  ERRMSG(62)/'Too Many Particle Categories Specified for        '/
      DATA ERRCOD(63)/'245'/,
     &  ERRMSG(63)/'No. of Settling/Removal Cats Exceeds Max:  NPDMAX='/
      DATA ERRCOD(64)/'248'/,
     &  ERRMSG(64)/'No Sources Were Defined on the SO Pathway.        '/
      DATA ERRCOD(65)/'250'/,
     &  ERRMSG(65)/'Duplicate XPNT/DIST or YPNT/DIR Specified for GRID'/
      DATA ERRCOD(66)/'252'/,
     &  ERRMSG(66)/'Duplicate Receptor Network ID Specified.  NETID = '/
      DATA ERRCOD(67)/'255'/,
     &  ERRMSG(67)/'Boundary Receptor Distances Not Defined Yet for   '/
      DATA ERRCOD(68)/'260'/,
     &  ERRMSG(68)/'Number of Emission Factors Exceeds Max:      NQF= '/
      DATA ERRCOD(69)/'270'/,
     &  ERRMSG(69)/'Number of High Values Specified Exceeds Max: NVAL='/
      DATA ERRCOD(70)/'280'/,
     &  ERRMSG(70)/'Number of Max Values Specified Exceeds Max:  NMAX='/
C     Message '290' specified below as array element 120.
      DATA ERRCOD(71)/'300'/,
     &  ERRMSG(71)/'Specified SRCID Has Not Been Defined Yet: KEYWORD='/
      DATA ERRCOD(72)/'310'/,
     &  ERRMSG(72)/'Attempt to Define Duplicate LOCATION Card for SRC:'/
      DATA ERRCOD(73)/'315'/,
     &  ERRMSG(73)/'Attempt to Define Duplicate SRCPARAM Card for SRC:'/
      DATA ERRCOD(74)/'320'/,
     &  ERRMSG(74)/'Source Parameter May Be Out-of-Range for Parameter'/
      DATA ERRCOD(75)/'325'/,
     &  ERRMSG(75)/'Negative Exit Velocity (Set=1.0E-5) for SRCID:    '/
      DATA ERRCOD(76)/'330'/,
     &  ERRMSG(76)/'Mass Fraction Parameters Do Not Sum to 1. for Src '/
      DATA ERRCOD(77)/'332'/,
     &  ERRMSG(77)/'Mass Fraction Parameter Out-of-Range for Source   '/
      DATA ERRCOD(78)/'334'/,
     &  ERRMSG(78)/'Particle Density Out-of-Range for Source          '/
      DATA ERRCOD(79)/'340'/,
     &  ERRMSG(79)/'Possible Error In ANHT of ANEMHGHT. The Value is  '/
      DATA ERRCOD(80)/'350'/,
     &  ERRMSG(80)/'Julian Day Out Of Range at                        '/
      DATA ERRCOD(81)/'360'/,
     &  ERRMSG(81)/'2-Digit Year Specified: Valid for Range 1901-2099 '/
      DATA ERRCOD(82)/'370'/,
     &  ERRMSG(82)/'Invalid Date: 2/29 In a Non-leap Year.            '/
      DATA ERRCOD(83)/'380'/,
     &  ERRMSG(83)/'This Input Variable is Out-of-Range:              '/
      DATA ERRCOD(84)/'385'/,
     &  ERRMSG(84)/'Averaging Period .NE. 1-Hr for TOXXFILE Option    '/
      DATA ERRCOD(85)/'400'/,
     &  ERRMSG(85)/'No Convergence Reached in SUB. CUBIC.  KURDAT=    '/
      DATA ERRCOD(86)/'410'/,
     &  ERRMSG(86)/'Flow Vector Out-of-Range.   KURDAT=               '/
      DATA ERRCOD(87)/'420'/,
     &  ERRMSG(87)/'Wind Speed Out-of-Range.   KURDAT=                '/
      DATA ERRCOD(88)/'430'/,
     &  ERRMSG(88)/'Ambient Temperature Data Out-of-Range.  KURDAT=   '/
      DATA ERRCOD(89)/'440'/,
     &  ERRMSG(89)/'Calm Hour Identified in Meteorology Data File at  '/
      DATA ERRCOD(90)/'450'/,
     &  ERRMSG(90)/'Error in Meteor. File - Record Out of Sequence at '/
      DATA ERRCOD(91)/'460'/,
     &  ERRMSG(91)/'Missing Hour Identified in Meteor. Data File at   '/
      DATA ERRCOD(92)/'470'/,
     &  ERRMSG(92)/'Mixing Height Value is < or = 0.0.   KURDAT=      '/
      DATA ERRCOD(93)/'500'/,
     &  ERRMSG(93)/'Fatal Error Occurs Opening the Data File of       '/
      DATA ERRCOD(94)/'510'/,
     &  ERRMSG(94)/'Fatal Error Occurs During Reading of the File of  '/
      DATA ERRCOD(95)/'520'/,
     &  ERRMSG(95)/'Fatal Error Occurs During Writing to the File of  '/
      DATA ERRCOD(96)/'530'/,
     &  ERRMSG(96)/'Error Occurs Reading Met Station or Year:File Says'/
      DATA ERRCOD(97)/'540'/,
     &  ERRMSG(97)/'No RECTABLE/MAXTABLE/DAYTABLE for Average Period  '/
      DATA ERRCOD(98)/'550'/,
     &  ERRMSG(98)/'File Unit/Name Conflict for the Output Option:    '/
      DATA ERRCOD(99)/'560'/,
     &  ERRMSG(99)/'User Specified File Unit < 20 for OU Keyword:     '/
      DATA ERRCOD(100)/'565'/,
     & ERRMSG(100)/'Possible Conflict With Dynamically Allocated FUNIT'/
      DATA ERRCOD(101)/'570'/,
     & ERRMSG(101)/'Problem Reading Temporary Event File for Event:   '/
      DATA ERRCOD(102)/'580'/,
     & ERRMSG(102)/'End of File Reached Trying to Read the File of    '/
      DATA ERRCOD(103)/'305'/,
     & ERRMSG(103)/'Terrain Grid Does Not Cover Modeling Area, Change:'/
      DATA ERRCOD(104)/'144'/,
     & ERRMSG(104)/'Conflicting Options: NOSMPL with FLAT Terrain     '/
      DATA ERRCOD(105)/'151'/,
     & ERRMSG(105)/'CO ELEVUNIT card obsolescent: use RE ELEVUNIT card'/
      DATA ERRCOD(106)/'152'/,
     & ERRMSG(106)/'ELEVUNIT card must be first for this Pathway:     '/
      DATA ERRCOD(107)/'153'/,
     & ERRMSG(107)/'Cannot use CO ELEVUNIT card with ELEVUNIT card for'/
      DATA ERRCOD(108)/'391'/,
     & ERRMSG(108)/'Aspect ratio (L/W) of area source greater than 10 '/
      DATA ERRCOD(109)/'392'/,
     & ERRMSG(109)/'Aspect ratio (L/W) of open pit is greater than 10 '/
      DATA ERRCOD(110)/'393'/,
     & ERRMSG(110)/'Terrain Grid Value Differs >50% From Source Elev. '/
      DATA ERRCOD(111)/'394'/,
     & ERRMSG(111)/'Terrain Grid Value Differs >50% From Receptor Elev'/
      DATA ERRCOD(112)/'322'/,
     & ERRMSG(112)/'Release Height Exceeds Effective Depth for OPENPIT'/
      DATA ERRCOD(113)/'243'/,
     & ERRMSG(113)/'Scav. Coef. may be out-of-range for SRCID         '/
      DATA ERRCOD(114)/'143'/,
     & ERRMSG(114)/'Conflicting Options: UNFORM with Dry or Wet Depos.'/
      DATA ERRCOD(115)/'435'/,
     & ERRMSG(115)/'Surface Roughness Length Out-of-Range.  KURDAT=   '/


      DATA ERRCOD(116)/'342'/,
     &  ERRMSG(116)/'Src ID Mismatch in Hourly Emissions File for ID ='/
      DATA ERRCOD(117)/'344'/,
     &  ERRMSG(117)/'Hourly Emission Rate is Zero for KURDAT ='/
C*----
C*#

      DATA ERRCOD(118)/'157'/,
     & ERRMSG(118)/'EMISUNIT Keyword Used With More Than 1 Output Type'/
      DATA ERRCOD(119)/'158'/,
     & ERRMSG(119)/'EMISUNIT Keyword Used With the Following Keyword: '/
      DATA ERRCOD(120)/'290'/,
     & ERRMSG(120)/'Number of Output Types Specified Exceeds Max:NTYP='/
      DATA ERRCOD(121)/'295'/,
     & ERRMSG(121)/'PERIOD and ANNUAL averages are both selected for  '/
      DATA ERRCOD(122)/'455'/,
     & ERRMSG(122)/'Date/time Mismatch: Hourly Emission File. KURDAT ='/
      DATA ERRCOD(123)/'323'/,
     & ERRMSG(123)/'No Particle Categories Specified for OPENPIT Src. '/

      END
