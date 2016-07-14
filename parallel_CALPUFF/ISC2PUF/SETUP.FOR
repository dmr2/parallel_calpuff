      SUBROUTINE SETUP
C***********************************************************************
C                 SETUP Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Controls Processing of Run SETUP Information
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C        MODIFIED BY D. Strimaitis, SRC (for GRIDDED TERRAIN Processing)
C
C        MODIFIED:  Default format for METFRM modified to eliminate the
C                   variable ZDM on input.
C                   BY:  J. Paumier, PES              DATE: 27 July 1994
C
C        DATE:    December 15, 1993
C
C        INPUTS:  Input Runstream File
C
C        OUTPUTS: Processing Option Switches
C                 Arrays of Source Parameters
C                 Arrays of Receptor Locations
C                 Meteorological Data Specifications
C                 Terrain Grid Data Specifications
C                 Output Options
C
C        CALLED FROM:   MAIN
C***********************************************************************
C
C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      LOGICAL NOPATH, NOKEY
      CHARACTER RDFRM*20, ECFRM*20, EVFRM*20

C     Variable Initializations
      MODNAM = 'SETUP'
      EOF = .FALSE.
      ILINE = 0

C     Setup READ format and ECHO format for runstream record,
C     based on the ISTRG PARAMETER (set in MAIN1.INC)
      WRITE(RDFRM,9100) ISTRG, ISTRG
 9100 FORMAT('(A',I3.3,',T1,',I3.3,'A1)')
      WRITE(ECFRM,9250) ISTRG
 9250 FORMAT('(1X,A',I3.3,')')
      WRITE(EVFRM,9300) ISTRG
 9300 FORMAT('(A',I3.3,')')

C     LOOP Through Input Runstream Records
      DO WHILE (.NOT. EOF)

C        Increment the Line Counter
         ILINE = ILINE + 1

C        READ Record to Buffers, as A80 and 80A1 for ISTRG = 80.
C        Length of ISTRG is Set in PARAMETER Statement in MAIN1.INC
         READ (INUNIT,RDFRM,END=999) RUNST1, (RUNST(I), I = 1, ISTRG)

C        Convert Lower Case to Upper Case Letters           ---   CALL LWRUPR
         CALL LWRUPR

C        Define Fields on Card                              ---   CALL DEFINE
         CALL DEFINE

C        Get the Contents of the Fields                     ---   CALL GETFLD
         CALL GETFLD

         IF (ECHO .AND.
     &            (FIELD(1).EQ.'OU' .AND. FIELD(2).EQ.'FINISHED')) THEN
C           Echo Last Input Card to Output File (Use Character Substring to
C           Avoid Echoing ^Z Which May Appear at "End of File" for Some
C           Editors).  Also, Allow for Shift in the Input Runstream File of
C           Up to 3 Columns.
            IF (LOCB(1) .EQ. 1) THEN
               WRITE(IOUNIT,9200) RUNST1(1:11)
 9200          FORMAT(' ',A11)
            ELSE IF (LOCB(1) .EQ. 2) THEN
               WRITE(IOUNIT,9210) RUNST1(1:12)
 9210          FORMAT(' ',A12)
            ELSE IF (LOCB(1) .EQ. 3) THEN
               WRITE(IOUNIT,9220) RUNST1(1:13)
 9220          FORMAT(' ',A13)
            ELSE IF (LOCB(1) .EQ. 4) THEN
               WRITE(IOUNIT,9230) RUNST1(1:14)
 9230          FORMAT(' ',A14)
            END IF
         ELSE IF (ECHO) THEN
C           Echo Full Input Card to Output File
            WRITE(IOUNIT,ECFRM) RUNST1
         END IF

C        If Blank Line, Then CYCLE to Next Card
         IF (BLINE) GO TO 11

C        Check for 'NO ECHO' In First Two Fields
         IF (FIELD(1) .EQ. 'NO' .AND. FIELD(2) .EQ. 'ECHO') THEN
            ECHO = .FALSE.
            GO TO 11
         END IF

C        Extract Pathway ID From Field 1                    ---   CALL EXPATH
         CALL EXPATH(FIELD(1),NOPATH)

C        For Invalid Pathway and Comment Lines Skip to Next Record
         IF (NOPATH .OR. PATH .EQ. '**') GO TO 11

C        Extract Keyword From Field 2                       ---   CALL EXKEY
         CALL EXKEY(FIELD(2),NOKEY)

C        When Keyword Is Wrong, Save Keyword and Skip To The Next Record
         IF (NOKEY) THEN
            PKEYWD = KEYWRD
            GO TO 11
         END IF

C        Check for Proper Order of Setup Cards              ---   CALL SETORD
         CALL SETORD

C        Process Input Card Based on Pathway
         IF (PATH .EQ. 'CO') THEN
C           Process COntrol Pathway Cards                   ---   CALL COCARD
            CALL COCARD
C           Echo Runstream Image to Temporary Event File (Except ELEVUNIT,
C                EVENTFIL, SAVEFILE, INITFILE & MULTYEAR)
            IF (KEYWRD.NE.'ELEVUNIT' .AND. KEYWRD.NE.'EVENTFIL' .AND.
     &          KEYWRD.NE.'SAVEFILE' .AND. KEYWRD.NE.'INITFILE' .AND.
     &          KEYWRD.NE.'MULTYEAR') THEN
               WRITE(ITEVUT,EVFRM) RUNST1
            END IF
         ELSE IF (PATH .EQ. 'SO') THEN
C           Process SOurce Pathway Cards                    ---   CALL SOCARD
            CALL SOCARD
C           Echo Runstream Image to Temporary Event File
            WRITE(ITEVUT,EVFRM) RUNST1
         ELSE IF (PATH .EQ. 'RE') THEN
C           Process REceptor Pathway Cards                  ---   CALL RECARD
            CALL RECARD
         ELSE IF (PATH .EQ. 'ME') THEN
C           Process MEteorology Pathway Cards               ---   CALL MECARD
            CALL MECARD
C           Echo Runstream Image to Temporary Event File (Except STARTEND
C           & DAYRANGE)
            IF (KEYWRD.NE.'STARTEND' .AND.
     &          KEYWRD.NE.'DAYRANGE') THEN
                WRITE(ITEVUT,EVFRM) RUNST1
            END IF
         ELSE IF (PATH .EQ. 'TG') THEN
C           Process Terrain Grid Pathway Cards              ---   CALL TGCARD
            CALL TGCARD
         ELSE IF (PATH .EQ. 'OU') THEN
C           Process OUtput Pathway Cards                    ---   CALL OUCARD
            CALL OUCARD
         END IF

C        Store the Current Keyword as the Previous Keyword
         PKEYWD = KEYWRD

C        Check for 'OU FINISHED' Card.  Exit DO WHILE Loop By Branching
C        to Statement 999 in Order to Avoid Reading a ^Z "End of File"
C        Marker That May Be Present For Some Editors.
         IF (PATH .EQ. 'OU' .AND. KEYWRD .EQ. 'FINISHED') THEN
            GO TO 999
         END IF

         GO TO 11
 999     EOF = .TRUE.
 11      CONTINUE
      END DO

C     Reinitialize Line Number Counter to Count Meteorology Data
      ILINE = 0

C     Check That All Pathways Were Finished
      IF (ICSTAT(20).NE.1 .OR. ISSTAT(20).NE.1 .OR. IRSTAT(20).NE.1 .OR.
     &    IMSTAT(20).NE.1 .OR. IOSTAT(20).NE.1) THEN
C        Runstream File Incomplete, Save I?STAT to IFSTAT and Write Message
         IFSTAT = ICSTAT(20)*10000 + ISSTAT(20)*1000 + IRSTAT(20)*100 +
     &            IMSTAT(20)*10 + IOSTAT(20)
         WRITE(DUMMY,'(I5.5)') IFSTAT
         CALL ERRHDL(PATH,MODNAM,'E','125',DUMMY)
      END IF

      RETURN
      END

      SUBROUTINE LWRUPR
C***********************************************************************
C                 LWRUPR Module of ISC2 Model
C
C        PURPOSE: Transfer All Characters From Lower Case To
C                 Upper Case (Using INDEX Intrinsic Function)
C                 Note that the CHAR*80 RUNST1 Variable Includes
C                 the Original Case for Echoing and for Later Use
C                 To Retrieve Filenames.
C
C        PROGRAMMER: Roger Brode, Kevin Stroupe
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Card Image (80 Character Array)
C                 Number of Characters in String, PARAMETER ISTRG
C
C        OUTPUTS: Input Runstream Card Image (Array) in Uppercase
C
C        CALLED FROM:   SETUP
C***********************************************************************
C
C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      CHARACTER UPCASE*26
      CHARACTER LWCASE*26

C     Variable Initializations
      DATA UPCASE/'ABCDEFGHIJKLMNOPQRSTUVWXYZ'/
      DATA LWCASE/'abcdefghijklmnopqrstuvwxyz'/
      MODNAM = 'LWRUPR'

      DO 20 I = 1, ISTRG
         IF (RUNST(I) .NE. ' ') THEN
            INDCHK = INDEX(LWCASE,RUNST(I))
            IF (INDCHK .NE. 0) THEN
               RUNST(I) = UPCASE(INDCHK:INDCHK)
            END IF
         END IF
 20   CONTINUE

      RETURN
      END

      SUBROUTINE DEFINE
C***********************************************************************
C                 DEFINE Module of ISC2 Model
C
C        PURPOSE: Defines Location of Fields on Runstream Input Image
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Card Image
C
C        OUTPUTS: Number of Fields on Card, IFC
C                 Beginning and Ending Columns of Fields, LOCB and LOCE
C
C        CALLED FROM:   SETUP
C***********************************************************************
C

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'

C     Variable Initializations
      MODNAM = 'DEFINE'

C     Initialize the Blank Line and In-field Status Indicators
      BLINE = .TRUE.
      INFLD = .FALSE.

      IF (ILINE .EQ. 1) THEN
C        Define the Starting Column for the Input File In Case File Is Shifted.
C        Allow for Shift of Up to 3 Columns
         LOCB(1) = 0
         IF (RUNST(1) .NE. ' ') THEN
            LOCB(1) = 1
         ELSE IF (RUNST(2) .NE. ' ') THEN
            LOCB(1) = 2
         ELSE IF (RUNST(3) .NE. ' ') THEN
            LOCB(1) = 3
         ELSE IF (RUNST(4) .NE. ' ') THEN
            LOCB(1) = 4
         ELSE
            LOCB(1) = 1
         END IF
         LOCE(1) = LOCB(1) + 1
         LOCB(2) = LOCB(1) + 3
         LOCE(2) = LOCB(1) + 10
      END IF

      IFC = 2

C     Loop Through the Pathway and Keyword Fields To Check for Blank Line
      DO 15 I = LOCB(1), LOCE(2)+1
         IF (RUNST(I) .NE. ' ') BLINE = .FALSE.
 15   CONTINUE

C     Loop through the Data Fields
      DO 20 I = LOCB(1)+12, ISTRG

         IF (.NOT.INFLD .AND. RUNST(I).NE.' ') THEN
C           Location is the Beginning of a Field
C           Set Mark of not Blank Line
            BLINE = .FALSE.
C           Set Mark of in a Field
            INFLD = .TRUE.
C           Increment the Field Counter
            IFC = IFC + 1
C           Record the Location of Beginning of the Field
            LOCB(IFC) = I
         ELSE IF (INFLD .AND. RUNST(I).EQ.' ') THEN
C           Location is the End of a Field
C           Set Mark of Not In a field
            INFLD = .FALSE.
C           Record the Location of Ending of the Field
            LOCE(IFC) = I - 1
         END IF

C        Check for End of Input String
C        (Length of ISTRG is Set as a PARAMETER in MAIN1.INC)
         IF (INFLD .AND. I.EQ.ISTRG) THEN
            LOCE(IFC) = ISTRG
         END IF

 20   CONTINUE

      RETURN
      END

      SUBROUTINE GETFLD
C***********************************************************************
C                 GETFLD Module of ISC2 Model
C
C        PURPOSE: Gets Contents of Fields on Runstream Input Image
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Card Image
C
C        OUTPUTS: Contents of Fields on Card
C
C        CALLED FROM:   SETUP
C***********************************************************************
C
C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'

C     Variable Initializations
      MODNAM = 'GETFLD'

      DO 25 I = 1, IFC
         IF (LOCE(I)-LOCB(I) .LE. 39) THEN
C           Field Satisfies Limit of 40 Characters
            WRITE(FIELD(I),9004) (RUNST(J),J=LOCB(I),LOCE(I))
         ELSE
C           Field Exceeds 40 Character Limit (May Be Valid for Met Format)
C           Truncate Field at 40 Characters
            WRITE(FIELD(I),9004) (RUNST(J),J=LOCB(I),LOCB(I)+39)
         END IF
 25   CONTINUE

 9004 FORMAT(40(A1:))

      RETURN
      END

      SUBROUTINE EXPATH(INPFLD,NOPATH)
C***********************************************************************
C                 EXPATH Module of ISC2 Model
C
C        PURPOSE: Extracts and Verifies Pathway ID from
C                 Runstream Input Card Image
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Card Image
C
C        OUTPUTS: The Extracted Pathway ID
C
C        CALLED FROM:   SETUP
C***********************************************************************
C
C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      CHARACTER INPFLD*2, PATHWY(IPN)*2
      LOGICAL NOPATH

C     Variable Initializations
      DATA (PATHWY(I),I = 1, IPN) /'CO','SO','RE','ME','TG','OU','**'/
      NOPATH = .TRUE.
      MODNAM = 'EXPATH'

C     Begin The Processing
      IF (INPFLD .NE. '  ') THEN
C        Check the Read-in Pathway
         PATH = INPFLD
         DO 100 I = 1, IPN
C           In Case of Match Set NOPATH to FALSE and Set Path Number, IPNUM
            IF (INPFLD .EQ. PATHWY(I)) THEN
               NOPATH = .FALSE.
               IPNUM = I
C              Exit to END
               GO TO 999
            END IF
 100     CONTINUE
C        In Case Of Invalid Pathway ID, Write Out Error Meassage
         IF (NOPATH) THEN
C           WRITE Error Message    ! Invalid Pathway ID
            CALL ERRHDL(PPATH,MODNAM,'E','100',PATH)
C           Reset the Pathway to the Previous Valid Pathway
            PATH = PPATH
         END IF
      ELSE
C        In Case of Blank Field Set Pathway to Previous Pathway
         NOPATH = .FALSE.
         PATH  = PPATH
         IPNUM = IPPNUM
      END IF

 999  RETURN
      END

      SUBROUTINE EXKEY(INPFLD,NOKEY)
C***********************************************************************
C                 EXKEY Module of ISC2 Model
C
C        PURPOSE: Extracts and Verifies Keyword from
C                 Runstream Input Card Image
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Card Image
C
C        OUTPUTS: The Extracted Keyword
C
C        CALLED FROM:   SETUP
C***********************************************************************
C

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      CHARACTER INPFLD*8
      LOGICAL NOKEY

C     Variable Initializations
      NOKEY  = .TRUE.
      MODNAM = 'EXKEY'

C     Begin The Processing
      IF (INPFLD .NE. '        ') THEN
C        Check the Read-in Keyword
         KEYWRD = INPFLD
         DO 100 I = 1, IKN
C           In Case of Match Set NOKEY to FALSE
            IF (INPFLD .EQ. KEYWD(I)) THEN
               NOKEY = .FALSE.
C              Exit to END
               GO TO 999
            END IF
 100     CONTINUE
C        When Illegal Keyword Output Error Message
         IF (NOKEY) THEN
C           WRITE Error Message    ! Invalid Keyword
            CALL ERRHDL(PATH,MODNAM,'E','105',KEYWRD)
         END IF
      ELSE
C        In Case of Blank Field, Keyword Is Set to Previous Keyword
         NOKEY  = .FALSE.
         KEYWRD = PKEYWD
      END IF

 999  RETURN
      END

      SUBROUTINE SETORD
C***********************************************************************
C                 SETORD Module of ISC2 Model
C
C        PURPOSE: To Check Run Stream Setup Images for Proper
C                 Order
C
C        MODIFIED:   To allow for skipping of TG pathway if no terrain
C                    grid is used.  Roger Brode, PES, Inc. - 11/7/94
C
C        INPUTS:  Input Runstream Card Image
C
C        OUTPUTS: Status Settings and Error Messages
C
C        CALLED FROM:   SETUP
C***********************************************************************
C
C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'

C     Variable Initializations
      MODNAM = 'SETORD'

      IF (KEYWRD .EQ. 'STARTING') THEN
         IF (ISTART .OR. .NOT.IFINIS) THEN
C           WRITE Error Message: Starting Out of Order
            CALL ERRHDL(PPATH,MODNAM,'E','115',PATH)
         ELSE IF (IPNUM .NE. IPPNUM+1) THEN
            IF (PATH.EQ.'OU' .AND. PPATH.EQ.'ME') THEN
C              TG Pathway has been omitted - Assume no TG file and no error
               LTGRID = .FALSE.
            ELSE
C              WRITE Error Message: Pathway Out of Order
               CALL ERRHDL(PPATH,MODNAM,'E','120',PATH)
            END IF
         END IF
C        Set Starting Indicator
         ISTART = .TRUE.
C        Set Finished Indicator
         IFINIS = .FALSE.
      ELSE IF (KEYWRD .EQ. 'FINISHED') THEN
         IF (IFINIS .OR. .NOT.ISTART) THEN
C           WRITE Error Message: Finished Out of Order
            CALL ERRHDL(PPATH,MODNAM,'E','115',PATH)
         ELSE IF (ISTART .AND. PATH.NE.PPATH) THEN
C           WRITE Warning Message: Pathway Out of Order
            CALL ERRHDL(PPATH,MODNAM,'E','120',PATH)
         END IF
C        Reset Starting Indicator
         ISTART = .FALSE.
C        Set Finished Indicator
         IFINIS = .TRUE.
      ELSE IF (.NOT.ISTART .OR. IFINIS) THEN
C        WRITE Error Message: Starting or Finished Out of Order
         CALL ERRHDL(PPATH,MODNAM,'E','115',PATH)
      ELSE IF (ISTART .AND. PATH.NE.PPATH) THEN
C        WRITE Warning Message: Pathway Out of Order
         CALL ERRHDL(PPATH,MODNAM,'E','120',PATH)
      END IF

C     Save Current Path and Path Number as Previous Path and Number
      PPATH = PATH
      IPPNUM = IPNUM

      RETURN
      END

      SUBROUTINE STONUM(STRVAR,LENGTH,FNUM,IMUTI)
C***********************************************************************
C                 STONUM Module of ISC2 Model
C
C        PURPOSE: Gets Number From A String Variable
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input String Variable
C                 Length of Character String
C
C        OUTPUTS: Numbers
C
C        CALLED FROM: (This Is A Utility Program)
C***********************************************************************
C
C     Variable Declarations
      CHARACTER STRVAR*(*), CHK, MODNAM*6, NUMS*10
      REAL FNUM, CNUM
      LOGICAL MEND, IN, NMARK, PMARK, DMARK, MMARK, EMARK

C     Variable Initialization
      MODNAM = 'STONUM'
      NUMS = '0123456789'
      I = 1
      MEND = .FALSE.
      IN = .FALSE.
      NMARK = .FALSE.
      PMARK = .FALSE.
      DMARK = .FALSE.
      MMARK = .FALSE.
      EMARK = .FALSE.
      CNUM  = 0.0
      IMUTI = 1
      FDEC  = 1.

C     Beginning the Processing
      DO WHILE (.NOT.MEND .AND. I.LE.LENGTH)
         CHK = STRVAR(I:I)
         IF (CHK .NE. ' ') THEN
            IN = .TRUE.
            IF (CHK.GE.'0' .AND. CHK.LE.'9') THEN
C              CHK is a Number, Assign a Value
               IF (.NOT. DMARK) THEN
                  CNUM = CNUM*10.+FLOAT(INDEX(NUMS,CHK)-1)
               ELSE
                  FDEC = FDEC/10.
                  FDC1 = FDEC*FLOAT(INDEX(NUMS,CHK)-1)
                  CNUM = CNUM+FDC1
               END IF
            ELSE
C              Handle The E-Type Real Number
               IF (.NOT.EMARK .AND. CHK.EQ.'E') THEN
                  EMARK = .TRUE.
                  IF (.NOT.NMARK) THEN
                     HEAD = CNUM
                  ELSE
                     HEAD = -CNUM
                  END IF
                  DMARK = .FALSE.
                  NMARK = .FALSE.
                  CNUM = 0.0
               ELSE IF (.NOT.PMARK .AND. CHK.EQ.'+') THEN
C                 Set Positive Indicator
                  PMARK = .TRUE.
               ELSE IF (.NOT.NMARK .AND. CHK.EQ.'-') THEN
C                 Set Negative Indicator
                  NMARK = .TRUE.
               ELSE IF (.NOT.DMARK .AND. CHK.EQ.'.') THEN
C                 Set Decimal Indicator
                  DMARK = .TRUE.
               ELSE IF (.NOT.MMARK .AND. CHK.EQ.'*' .AND.
     &                  .NOT.NMARK) THEN
C                 Set Repeat Number
                  MMARK = .TRUE.
                  IMUTI = INT(CNUM)
                  CNUM = 0.0
               ELSE
C                 Error Occurs, Set Switch and Exit Out Of The Subroutine
                  GO TO 9999
               END IF
            END IF
         ELSE IF (IN .AND. CHK.EQ.' ') THEN
            MEND = .TRUE.
         END IF
         I = I + 1
      END DO

      FNUM = CNUM

C     In Case Of Negative Field, Value Set to Negative
      IF (NMARK) THEN
         FNUM = -FNUM
      END IF

C     In Case of E-Format, Check for Exponents Out of Range
      IF (EMARK .AND. ABS(FNUM) .LE. 30.) THEN
         FNUM = HEAD*10**(FNUM)
      ELSE IF (EMARK .AND. ABS(FNUM) .GT. 30.) THEN
         IF (FNUM .LT. 0.0) THEN
            FNUM = 0.0
         ELSE IF (FNUM .GT. 0.0) THEN
            FNUM = HEAD * 10**30.
         END IF
         GO TO 9999
      END IF

      GO TO 1000

C     Set Error Switch for Illegal Numerical Field (WRITE Message and Handle
C     Error in Calling Routine)
 9999 IMUTI = -1

 1000 RETURN
      END

      SUBROUTINE SINDEX(ARRIN,IDIM,ELEM,INDEXS,FIND)
C***********************************************************************
C                 SINDEX Module of ISC2 Model
C
C        PURPOSE: Search The Index of An Input Array Element
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Character Element
C
C        OUTPUTS: Index Of This Element in An Array
C
C        CALLED FROM:  (This Is An Utility Programm)
C***********************************************************************
C
C     Variable Declarations
      CHARACTER*8 ARRIN(IDIM), ELEM
      CHARACTER MODNAM*6
      LOGICAL FIND

C     Variable Initializations
      MODNAM = 'SINDEX'
      FIND = .FALSE.
      I = 1
      INDEXS = 0

      DO WHILE (.NOT.FIND .AND. I.LE.IDIM)
         IF (ELEM .EQ. ARRIN(I)) THEN
            FIND = .TRUE.
            INDEXS = I
         END IF
         I = I + 1
      END DO

      RETURN
      END

      SUBROUTINE FSPLIT(PATHIN,KEYIN,INPFLD,LENGTH,DELIM,LFLAG,
     &                  BEGFLD,ENDFLD)
C***********************************************************************
C                 FSPLIT Module of ISC2 Model
C
C        PURPOSE: SPLIT A FIELD, BASED ON AN INPUT DELIMITER
C                 CHARACTER.  SETS A LOGICAL FLAG AND RETURNS
C                 BEGINNING AND ENDING PARTS OF FIELD.
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Pathway for Calling Routine
C                 Keyword for Calling Routine
C                 Input Field Variable
C                 Length of Input Character Field
C                 Delimiter Character
C
C        OUTPUTS: Logical Flag to Indicate Presence of Delimiter
C                 Beginning Part of Field (.LE. 8 Character)
C                 Ending Part of Field (.LE. 8 Character)
C
C        CALLED FROM: (This Is A Utility Program)
C***********************************************************************

C     Variable Declarations
      CHARACTER CHK, INPFLD*(*), DELIM, BEGFLD*8, ENDFLD*8, MODNAM*6,
     &          PATHIN*2, KEYIN*8
      LOGICAL LFLAG, MEND, IN

C     Variable Initialization
      MODNAM = 'FSPLIT'
      I = LENGTH
      IDELM = LENGTH
      BEGFLD = ' '
      ENDFLD = ' '
      MEND  = .FALSE.
      IN    = .FALSE.
      LFLAG = .FALSE.

C     Begin the Processing
      DO WHILE (.NOT.MEND .AND. I.GE.1)
         CHK = INPFLD(I:I)
         IF (CHK .NE. ' ') THEN
            IN = .TRUE.
C           Check for the Group Delimiter
            IF (.NOT.LFLAG .AND. CHK.EQ.DELIM) THEN
               LFLAG = .TRUE.
               IDELM = I
               ENDFLD = INPFLD(I+1:LENGTH)
               IF (I .EQ. 1) THEN
C                 Write Error Message for Invalid Range Parameter
                  CALL ERRHDL(PATHIN,MODNAM,'E','203',KEYIN)
                  GO TO 999
               END IF
            ELSE IF (LFLAG .AND. CHK.EQ.DELIM) THEN
C              WRITE Error Message  ! More Than One Delimiter in a Field
               CALL ERRHDL(PATHIN,MODNAM,'E','217',KEYIN)
            END IF
         ELSE IF (IN .AND. CHK.EQ.' ') THEN
            MEND = .TRUE.
            IF (LFLAG) THEN
               BEGFLD = INPFLD(1:IDELM-1)
            ELSE
               BEGFLD = INPFLD
            END IF
         END IF
         I = I - 1
      END DO

      IF (.NOT. MEND) THEN
         IF (LFLAG) THEN
            BEGFLD = INPFLD(1:IDELM-1)
         ELSE
            BEGFLD = INPFLD
         END IF
      END IF

C     In Case Of No Delimiter, Set ENDFLD = BEGFLD
      IF (.NOT. LFLAG) THEN
         ENDFLD = BEGFLD
      END IF

 999  RETURN
      END

      SUBROUTINE VARINI
C***********************************************************************
C                 VARINI Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: To Initialize Variables for Setup
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C        MODIFIED BY D. Strimaitis, SRC (for Wet & Dry DEPOSITION, INT.
C                                        TERRAIN, and GRIDDED TERRAIN
C                                        Processing)
C
C        DATE:    December 15, 1993
C
C        MODIFIED BY D. Strimaitis, SRC (for DEPOSITION)
C        (DATE:    February 15, 1993)
C
C        MODIFIED:  To Include TOXXFILE Option - 9/29/92
C
C        INPUTS:  None
C
C        OUTPUTS: Initialized Variables
C
C        CALLED FROM:   MAIN
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'

C     Variable Initializations
      MODNAM = 'VARINI'

      IPNUM  = 0
      IPPNUM = 0
      IERRST = 0
      IWARN  = 0
      NDUMP  = 0

C     Calculate Sector Width in Radians, DELTHP, Minimum Plume Height,
C     and Terrain Adjustment Factors for COMPLEX1
      DELTHP = 2.*PI/16.
      ZMIN = 10.0
      TCF(1) = 0.5
      TCF(2) = 0.5
      TCF(3) = 0.5
      TCF(4) = 0.5
      TCF(5) = 0.0
      TCF(6) = 0.0

C     Initialize the Logical Control Variables
      FATAL  = .FALSE.
      ISTART = .FALSE.
      IFINIS = .TRUE.
      ERRLST = .FALSE.
      DFAULT = .FALSE.
      CONC   = .FALSE.
      DEPOS  = .FALSE.
C     Add logicals to output just wet or just deposition fluxes
      DDEP   = .FALSE.
      WDEP   = .FALSE.
      RURAL  = .FALSE.
      URBAN  = .FALSE.
      GRDRIS = .FALSE.
      NOSTD  = .FALSE.
      NOBID  = .FALSE.
      NOCALM = .FALSE.
      MSGPRO = .FALSE.
      CLMPRO = .TRUE.
      PERIOD = .FALSE.
      ANNUAL = .FALSE.
      MONTH  = .FALSE.
      FLAT   = .TRUE.
      ELEV   = .FALSE.
      FLGPOL = .FALSE.
      RUN    = .FALSE.
      EVENTS = .FALSE.
      RSTSAV = .FALSE.
      RSTINP = .FALSE.
      MULTYR = .FALSE.
      DAYTAB = .FALSE.
      MXFILE = .FALSE.
      PPFILE = .FALSE.
      PLFILE = .FALSE.
C     Add TXFILE Variable for the TOXXFILE Option, 9/29/92
      TXFILE = .FALSE.
      ANPOST = .FALSE.
      ANPLOT = .FALSE.
      RECERR = .FALSE.
      RUNERR = .FALSE.
      NEWDAY = .TRUE.
      ENDMON = .FALSE.
      USERP  = .FALSE.
      USERDT = .FALSE.
      CALCS  = .FALSE.
      DEBUG  = .FALSE.
      WAKLOW = .FALSE.
      WAKE   = .FALSE.
      WAKESS = .FALSE.
      TALL   = .FALSE.
      SQUAT  = .FALSE.
      SSQUAT = .FALSE.
      ECHO   = .TRUE.
C     Add logicals to identify use wet and dry removal information
      LDPART  = .FALSE.
      LWPART  = .FALSE.
      LWGAS   = .FALSE.
C     Add logicals to control use of Wet & Dry plume depletion
      DDPLETE = .FALSE.
      WDPLETE = .FALSE.
C     Add logicals to control use of Simple and Complex Terrain Models
      NOSMPL = .FALSE.
      NOCMPL = .FALSE.
C     Add logical to control use of Gridded Terrain Processing
      LTGRID = .FALSE.

C*----   ISCSTM Modification: allow for NOCHKD option - jah 11/2/94                           
      NOCHKD = .FALSE.
C*----
C*#

C     Initialize Decay Coefficient to 0.0 (Urban SO2 Default Set in POLLUT)
      DECOEF = 0.0

C     Initialize the Source Arrays
      ISRC = 0
      DO 10 I = 1, NSRC
         DO 4 J = 1, NWET
            AGSCAV(J,I) = 0.0
 4       CONTINUE
         DO 5 J = 1, NSEC
            ADSBH(J,I)  = 0.0
            ADSBW(J,I)  = 0.0
            IDSWAK(J,I) = 0
 5       CONTINUE
         DO 6 J = 1, NQF
            QFACT(J,I) = 0.0
 6       CONTINUE
         QFLAG(I) = ' '
         DO 7 J = 1, NGRP
            IGROUP(I,J) = 0
 7       CONTINUE
         INPD(I) = 0
         DO 8 J = 1, NPDMAX
            APDIAM(J,I) = 0.0
            APHI(J,I)   = 0.0
            APDENS(J,I) = 0.0
            APSLIQ(J,I) = 0.0
            APSICE(J,I) = 0.0
 8       CONTINUE
         DO 9 J = 1, NVMAX
            AXVERT(J,I) = 0.0
            AYVERT(J,I) = 0.0
 9       CONTINUE
 10   CONTINUE

C     Counters for the Receptor Groups
      IREC = 0
      ISTA = .FALSE.
      IEND = .FALSE.
      IRXR = 0
      IRYR = 0
      IRZE = 0
      IRZF = 0
      IBND =  36
      IBELEV = 36
      NEWID = .TRUE.
C     Initialize ITAB, NXTOX, NYTOX Variables for the TOXXFILE Option, 9/29/92
      ITAB  = -9
      NXTOX = 0
      NYTOX = 0

C     Initialize Variables Associated with the Meteorology Data
      ISJDAY = 0
      IEJDAY = 366
      ISDATE = 0
      IEDATE = 99999999
      ISYR   = 0
      ISMN   = 0
      ISDY   = 0
      IEYR   = 99
      IEMN   = 99
      IEDY   = 99
      IPDATE = 0
      IPHOUR = 0
      NDAY   = 0
      INCRST = 1
      SFX = 0.0
      SFY = 0.0
      UAX = 0.0
      UAY = 0.0
      ROTANG = 0.0
      AFVLST = 360.
      AFV24  = 360.
      KSTMSG = 9
C     Set Threshold Wind Speed Value For Defining Calm Winds
      UMIN = 0.0

C     Specify Default Met Data Filename and Format.
C     Include u-star,L,z0,zd,ipcode, and prate in the format even
C     though these variables are used (read) only if DEPOSITION is
C     simulated.  The order of met variables is:
C       yr,mo,day,hr,rfvec,ws,tempk,stab,rmix,umix,ustar,el,z0,
C       zd,ipcode,prate

      METINP = ' '
cjop  METFRM = '(4I2,2F9.4,F6.1,I2,2F7.1,f9.4,f10.1,f8.4,f5.1,i4,f7.2)'
      METFRM = '(4I2,2F9.4,F6.1,I2,2F7.1,f9.4,f10.1,f8.4,i4,f7.2)'

C     Initialize the Results Arrays
      DO 95 M = 1, NTYP
         HRVAL(M) = 0.0
         DO 90 L = 1, NAVE
            NUMHRS(L) = 0
            NUMCLM(L) = 0
            NUMMSG(L) = 0
            DO 80 K = 1, NGRP
               DO 60 J = 1, NREC
                  AVEVAL(J,K,L,M) = 0.0
                  DO 50 I = 1, NVAL
                     HIVALU(J,I,K,L,M) = 0.0
                     NHIDAT(J,I,K,L,M) = 0
                     HCLMSG(J,I,K,L,M) = ' '
 50               CONTINUE
 60            CONTINUE
               DO 70 J = 1, NMAX
                  RMXVAL(J,K,L,M) = 0.0
                  MXDATE(J,K,L,M) = 0
                  MXLOCA(J,K,L,M) = 0
                  MCLMSG(J,K,L,M) = ' '
 70            CONTINUE
 80         CONTINUE
 90      CONTINUE
 95   CONTINUE
      IANHRS = 0
      IANCLM = 0
      IANMSG = 0
      DO 125 K = 1, NTYP
         DO 120 J = 1, NGRP
            DO 100 I = 1, NREC
               ANNVAL(I,J,K) = 0.0
 100        CONTINUE
            DO 110 I = 1, NVAL
               AMXVAL(I,J,K) = 0.0
               IMXLOC(I,J,K) = 0
 110        CONTINUE
 120     CONTINUE
 125  CONTINUE

C     Initialize the Outputs
      WRITE(TITLE1,130)
      WRITE(TITLE2,130)
 130  FORMAT(68(' '))
      IPAGE  = 0
      NHIVAL = 0
      NMXVAL = 0
      THRFRM = '(1X,I3,1X,A8,1X,I8,2(1X,F13.5),2(1X,F7.2),1X,F13.5)'
      PSTFRM = '(3(1X,F13.5),1X,F8.2,2X,A6,2X,A8,2X,I8,2X,A8)'
      PLTFRM = '(3(1X,F13.5),1X,F8.2,3X,A5,2X,A8,2X,A4,6X,A8)'
      DO 150 J = 1, NAVE
         INHI(J) = 0
         DO 140 I = 1, NVAL
            NHIAVE(I,J) = 0
 140     CONTINUE
         MAXAVE(J) = 0
         DO 145 I = 1, NGRP
            MAXFLE(I,J) = 0
            IPSTFL(I,J) = 0
            DO 144 K = 1, NVAL
               IPLTFL(K,I,J) = 0
 144        CONTINUE
 145     CONTINUE
 150  CONTINUE
      DO 160 I = 1, NGRP
         IANPST(I) = 0
         IANFRM(I) = 0
         IANPLT(I) = 0
 160  CONTINUE

C     Initialize the Number of Error/Warning/Informational Messages, and
c     The Number of Fatal Errors.
      IERROR = 0
      NFATAL = 0

      RETURN
      END
