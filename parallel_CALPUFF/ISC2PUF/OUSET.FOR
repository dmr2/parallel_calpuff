      SUBROUTINE OUCARD
C***********************************************************************
C                 OUCARD Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: To process OUtput Pathway card images
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        MODIFIED:   To add subroutine call for TOXXFILE option - 9/29/92
C
C        INPUTS:  Pathway (OU) and Keyword
C
C        OUTPUTS: Output Option Switches
C                 Output Setup Status Switches
C
C        CALLED FROM:   SETUP
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'

C     Variable Initializations
      MODNAM = 'OUCARD'

      IF (KEYWRD .EQ. 'STARTING') THEN
C        Set Status Switch
         IOSTAT(1) = IOSTAT(1) + 1
      ELSE IF (KEYWRD .EQ. 'RECTABLE') THEN
C        Process High Value Output Option                   ---   CALL OUHIGH
         CALL OUHIGH
C        Set Status Switch
         IOSTAT(2) = IOSTAT(2) + 1
      ELSE IF (KEYWRD .EQ. 'MAXTABLE') THEN
C        Process Maximum 50 Table Option                    ---   CALL OUMXVL
         CALL OUMXVL
C        Set Status Switch
         IOSTAT(3) = IOSTAT(3) + 1
      ELSE IF (KEYWRD .EQ. 'DAYTABLE') THEN
C        Process Daily Value Table Option                   ---   CALL OUDALY
         CALL OUDALY
C        Set Status Switch
         IOSTAT(4) = IOSTAT(4) + 1
      ELSE IF (KEYWRD .EQ. 'MAXIFILE') THEN
C        Process Maximum Value (Threshold) File Option      ---   CALL OUMXFL
         CALL OUMXFL
C        Set Status Switch
         IOSTAT(5) = IOSTAT(5) + 1
      ELSE IF (KEYWRD .EQ. 'POSTFILE') THEN
C        Process Postprocessing File Output Option          ---   CALL OUPOST
         CALL OUPOST
C        Set Status Switch
         IOSTAT(6) = IOSTAT(6) + 1
      ELSE IF (KEYWRD .EQ. 'PLOTFILE') THEN
C        Process Plotting File Output Option                ---   CALL OUPLOT
         CALL OUPLOT
C        Set Status Switch
         IOSTAT(7) = IOSTAT(7) + 1
      ELSE IF (KEYWRD .EQ. 'TOXXFILE') THEN
C        Process TOXXFILE Output Option                     ---   CALL OUTOXX
         CALL OUTOXX
C        Set Status Switch
         IOSTAT(8) = IOSTAT(8) + 1
      ELSE IF (KEYWRD .EQ. 'FINISHED') THEN
C        Set Status Switch
         IOSTAT(20) = IOSTAT(20) + 1
C        Check The Consistency of The Output Options
         CALL OUTQA
      ELSE
C        Write Error Message: Invalid Keyword for This Pathway
         CALL ERRHDL(PATH,MODNAM,'E','110',KEYWRD)
      END IF

      RETURN
      END

      SUBROUTINE OUTQA
C***********************************************************************
C                 OUTQA Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: To process OUtput Pathway card images QA Check
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        MODIFIED:  To Include TOXXFILE Option - 9/29/92
C
C        INPUTS:  Pathway (OU) and Keyword
C
C        OUTPUTS: Output Messages
C
C        CALLED FROM: OUCARD
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      LOGICAL OUTOPT
      CHARACTER KEYMSG*8, MSG1*3

C     Variable Initializations
      MODNAM = 'OUTQA'
      MSG1   = '-HR'
      OUTOPT = .FALSE.

C     Check If Missing Mandatory Keyword
      IF (IOSTAT(1) .EQ. 0) THEN
         CALL ERRHDL(PATH,MODNAM,'E','130','STARTING')
      END IF

C     Check For Lack of Any Output Option Cards
      DO 5 I = 2, 8
         IF (IOSTAT(I) .GT. 0) THEN
            OUTOPT = .TRUE.
         END IF
 5    CONTINUE
      IF (.NOT.OUTOPT .AND. .NOT.PERIOD .AND. .NOT.ANNUAL) THEN
C        WRITE Error Message - No Output Keywords and No PERIOD Averages
         CALL ERRHDL(PATH,MODNAM,'E','190','  ')
      END IF

      DO 10 IAVE = 1, NUMAVE
         IDCST1 = 0
         DO 20 IVAL = 1, NVAL
            IF (NHIAVE(IVAL,IAVE) .EQ. 1) THEN
               IDCST1 = 1
            END IF
 20      CONTINUE
         IF (IDCST1.EQ.0 .AND. MAXAVE(IAVE).EQ.0 .AND.
     &                         IDYTAB(IAVE).EQ.0) THEN
            WRITE(KEYMSG,'(I2.2,A3)') KAVE(IAVE), MSG1
            CALL ERRHDL(PATH,MODNAM,'W','540',KEYMSG)
         END IF
 10   CONTINUE

C     Check for DAYTABLE Option With SAVEFILE or INITFILE Options
      IF (DAYTAB .AND. (RSTSAV .OR. RSTINP)) THEN
C        WRITE Warning Message: DAYTABLE Results Overwritten on Re-start
         CALL ERRHDL(PATH,MODNAM,'W','195','DAYTABLE')
      END IF
C     Check for TOXXFILE Option With SAVEFILE or INITFILE Options
      IF (TXFILE .AND. (RSTSAV .OR. RSTINP)) THEN
C        WRITE Error Message: Incompatible Options
         CALL ERRHDL(PATH,MODNAM,'E','195','TOXXFILE')
      END IF

      RETURN
      END

      SUBROUTINE OUHIGH
C***********************************************************************
C                 OUHIGH Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: To process High Value By Receptor Table
C                 Output Selections
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Parameters
C
C        OUTPUTS: Output Option Switches
C
C        CALLED FROM:   OUCARD
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      CHARACTER LPRD*8, HPRD*8, NCHR1(10)*8, NCHR2(10)*4
      LOGICAL FOUND, RMARK
      DIMENSION HIGHST(NVAL), ILOCH(NAVE)

C     Variable Initializations
      DATA (NCHR1(I),I=1,10) /'FIRST','SECOND','THIRD','FOURTH',
     &                        'FIFTH','SIXTH','SEVENTH','EIGHTH',
     &                        'NINTH','TENTH'/
      DATA (NCHR2(I),I=1,10) /'1ST','2ND','3RD','4TH','5TH','6TH',
     &                        '7TH','8TH','9TH','10TH'/
      MODNAM = 'OUHIGH'
      FOUND  = .FALSE.

      DO 145 I = 1, NVAL
         HIGHST(I) = 0
 145  CONTINUE

      DO 150 I = 1, NAVE
         ILOCH(I) = 0
 150  CONTINUE

C     Check If Enough Fields
      IF (IFC .EQ. 2) THEN
C        Error Message: No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      ELSE IF (IFC .EQ. 3) THEN
C        Error Message: Not Enough Parameters
         CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
         GO TO 999
      ELSE IF (IFC .GT. 13) THEN
C        Error Message: Too Many Fields
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
         GO TO 999
      END IF

C     Retrieve Averaging Period
      IF (FIELD(3) .EQ. 'ALLAVE') THEN
C        Go For All Averaging Periods
         DO 40 I = 1, NUMAVE
            INHI(I) = 1
            ILOCH(I) = 1
 40      CONTINUE
         FOUND = .TRUE.
      ELSE IF (FIELD(3) .EQ. 'MONTH' .AND. MONTH) THEN
C        Set Value of IPRDT = 720 for MONTHly Averages
         IPRDT = 720
C        Search The Period to find out the Location
         DO 35 I = 1, NUMAVE
            IF (IPRDT .EQ. KAVE(I)) THEN
               FOUND = .TRUE.
               INHI(I) = 1
               ILOCH(I) = 1
            END IF
 35      CONTINUE
      ELSE
         CALL FSPLIT(PATH,KEYWRD,FIELD(3),40,'-',RMARK,LPRD,HPRD)
C        Single Time Period
         IF (HPRD .EQ. LPRD) THEN
            CALL STONUM(HPRD,8,FNUM,IMIT)
            IF (IMIT .NE. 1) THEN
C              Write Error Message:Invalid Numerical Field
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
               GO TO 115
            END IF
            IPRDT1 = INT(FNUM)
C           Search The Period to find out the Location
            DO 45 I = 1, NUMAVE
               IF (IPRDT1 .EQ. KAVE(I)) THEN
                  FOUND = .TRUE.
                  INHI(I) = 1
                  ILOCH(I) = 1
               END IF
 45         CONTINUE
         ELSE
C           Find The Lower Boundary
            CALL STONUM(LPRD,8,FNUM,IMIT)
            IF (IMIT .NE. 1) THEN
C              Write Error Message:Invalid Numerical Field
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
               GO TO 114
            END IF
            IPRDT1 = INT(FNUM)
C           Find The Upper Boundary
 114        CALL STONUM(HPRD,8,FNUM,IMIT)
            IF (IMIT .NE. 1) THEN
C              Write Error Message:Invalid Numerical Field
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
               GO TO 115
            END IF
            IPRDT2 = INT(FNUM)
C           Search The Period to find out the Location
            DO 50 I = 1, NUMAVE
               IF (KAVE(I).GE.IPRDT1 .AND.
     &             KAVE(I).LE.IPRDT2) THEN
                  FOUND = .TRUE.
                  INHI(I) = 1
                  ILOCH(I) = 1
               END IF
 50         CONTINUE
C           Multi Time Period
         END IF
      END IF

 115  CONTINUE

C     Check Averaging Period Against KAVE Array,
      IF (.NOT. FOUND) THEN
C        Error Message:E203 AVEPER Not Match With Pre-Defined One
         CALL ERRHDL(PATH,MODNAM,'E','203','AVEPER')
         GO TO 999
      END IF

C     Check for Previous NHIGHEST Card for This Averaging Period

C     Begin LOOP Through Fields
      DO 20 I = 4, IFC
C        Retrieve The High Value
         CALL FSPLIT(PATH,KEYWRD,FIELD(I),40,'-',RMARK,LPRD,HPRD)
C        Fit To The Status Array
         ISPRD = 0
         IEPRD = 0
         DO 30 J = 1, 10
            IF (LPRD.EQ.NCHR1(J) .OR.
     &          LPRD.EQ.NCHR2(J)) ISPRD = J
            IF (HPRD.EQ.NCHR1(J) .OR.
     &          HPRD.EQ.NCHR2(J)) IEPRD = J
 30      CONTINUE
         IF (ISPRD.EQ.0 .OR. IEPRD.EQ.0) THEN
C           Write Error Message:Illegal Parameter Field
            CALL ERRHDL(PATH,MODNAM,'E','203','HIVALU')
            GO TO 20
         END IF
         IF (ISPRD.GT.NVAL .OR. IEPRD.GT.NVAL) THEN
C           Write Error Message: High Value Requested Exceeds NVAL
            WRITE(DUMMY,'(I8)') NVAL
            CALL ERRHDL(PATH,MODNAM,'E','270',DUMMY)
            GO TO 20
         END IF
         DO 41 J = ISPRD,IEPRD
            HIGHST(J) = 1
 41      CONTINUE
C     End LOOP Through Fields
 20   CONTINUE

C     Set Array Switch to Indicate Which High Values to Report
C     And Set the Maximum Number of High Values, NHIVAL
      DO 125 I = 1, NUMAVE
         DO 120 J = 1, NVAL
            IF (HIGHST(J).EQ.1 .AND. ILOCH(I).EQ.1) THEN
               NHIAVE(J,I) = 1
               IF (J .GT. NHIVAL) THEN
                  NHIVAL = J
               END IF
            END IF
 120     CONTINUE
 125  CONTINUE

 999  RETURN
      END

      SUBROUTINE OUMXVL
C***********************************************************************
C                 OUMXVL Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: To Process Maximum (Overall) Value Table
C                 Output Selections
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Parameters
C
C        OUTPUTS: Output Option Switches
C
C        CALLED FROM:   OUCARD
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      LOGICAL FOUND

C     Variable Initializations
      MODNAM = 'OUMXVL'
      FOUND = .FALSE.

C     Check for Appropriate Number of Fields
      IF (IFC .EQ. 2) THEN
C        Error Message: No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      ELSE IF (IFC .EQ. 3) THEN
C        Error Message: Not Enough Parameters
         CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
         GO TO 999
      ELSE IF (IFC .GT. 4) THEN
C        Error Message: Too Many Fields
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
         GO TO 999
      END IF

C     Retrieve Averaging Period
      IF (FIELD(3) .EQ. 'ALLAVE') THEN
C        Go For All Averaging Periods
         DO 40 I = 1, NUMAVE
            MAXAVE(I) = 1
 40      CONTINUE
         FOUND = .TRUE.
      ELSE
         IF (FIELD(3) .EQ. 'MONTH' .AND. MONTH) THEN
C           Set Value of IPRDT = 720 for MONTHly Averages
            IPRDT = 720
         ELSE
            CALL STONUM(FIELD(3),40,FNUM,IMIT)
            IF (IMIT .NE. 1) THEN
C              Write Error Message:Invalid Numerical Field
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
               GO TO 999
            END IF
            IPRDT = INT(FNUM)
         END IF
C        Check Averaging Period Against KAVE Array
         J = 1
         DO WHILE (.NOT.FOUND .AND. J.LE.NUMAVE)
            IF (IPRDT .EQ. KAVE(J)) THEN
               FOUND = .TRUE.
               INDAVE = J
               MAXAVE(J) = 1
            END IF
            J = J + 1
         END DO
      END IF
      IF (.NOT. FOUND) THEN
C        Error Message: E203 AVEPER Not Match With Pre-Defined One
         CALL ERRHDL(PATH,MODNAM,'E','203','AVEPER')
         GO TO 999
      END IF

C     Set Number of Maximum Values to Sort
      CALL STONUM(FIELD(4),40,FNUM,IMIT)
      IF (IMIT .NE. 1) THEN
C        Write Error Message:Invalid Numerical Field
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         GO TO 999
      END IF
      INUM = INT(FNUM)
      IF (INUM .GT. NMAX) THEN
C        WRITE Error Message:  Maximum Value Selected Exceeds NMAX
         WRITE(DUMMY,'(I8)') NMAX
         CALL ERRHDL(PATH,MODNAM,'E','280',DUMMY)
         GO TO 999
      END IF

      IF (FIELD(3) .EQ. 'ALLAVE') THEN
C        Go For All Averaging Periods
         DO 50 I = 1, NUMAVE
            IMXVAL(I) = INUM
 50      CONTINUE
      ELSE
         IMXVAL(INDAVE) = INUM
      END IF

      IF (INUM .GT. NMXVAL) THEN
         NMXVAL = INUM
      END IF

 999  RETURN
      END

      SUBROUTINE OUDALY
C***********************************************************************
C                 OUDALY Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: To Process Daily Concurrent Value Table
C                 Output Selections
C
C        PROGRAMMER: Roger Brode
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Parameters
C
C        OUTPUTS: Output Option Switches
C
C        CALLED FROM:   OUCARD
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      LOGICAL FOUND

C     Variable Initializations
      MODNAM = 'OUDALY'

C     Check for Appropriate Number of Fields
      IF (IFC .EQ. 2) THEN
C        Error Message: No AvePer And High Value
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      ELSE IF (IFC .GT. NUMAVE+2) THEN
C        Error Message: Too Many Fields
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
         GO TO 999
      END IF

C     Retrieve Averaging Period(s)
      IF (FIELD(3) .EQ. 'ALLAVE') THEN
C        Go For All Averaging Periods
         DO 40 I = 1, NUMAVE
            IDYTAB(I) = 1
 40      CONTINUE
C        Set Logical Switch Indicating That Daily Value Tables Are Generated
         DAYTAB = .TRUE.
      ELSE
         DO 100 I = 3, IFC
            IF (FIELD(I) .EQ. 'MONTH' .AND. MONTH) THEN
C              Set Value of IPRDT = 720 for MONTHly Averages
               IPRDT = 720
            ELSE
               FOUND = .FALSE.
               CALL STONUM(FIELD(I),40,FNUM,IMIT)
               IF (IMIT .NE. 1) THEN
C                 Write Error Message:Invalid Numerical Field
                  CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
                  GO TO 999
               END IF
               IPRDT = INT(FNUM)
            END IF
C           Check Averaging Period Against KAVE Array
            J = 1
            DO WHILE (.NOT.FOUND .AND. J.LE.NUMAVE)
               IF (IPRDT .EQ. KAVE(J)) THEN
                  FOUND = .TRUE.
                  IDYTAB(J) = 1
               END IF
               J = J + 1
            END DO
            IF (.NOT. FOUND) THEN
C              Error Message:E203 KAVE Not Match With Pre-Defined One
               CALL ERRHDL(PATH,MODNAM,'E','203','AVEPER')
               GO TO 999
            END IF
 100     CONTINUE
C        Set Logical Switch Indicating That Daily Value Tables Are Generated
         DAYTAB = .TRUE.
      END IF

 999  RETURN
      END

      SUBROUTINE OUMXFL
C***********************************************************************
C                 OUMXFL Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: To Process Threshold Value Output Selections
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        MODIFIED:  To Change File Length Limit To 40 - 9/29/92
C        MODIFIED BY D. Strimaitis, SRC (for Wet & Dry DEPOSITION) - 11/8/93
C        MODIFIED:  To skip writing of header records if FATAL error
C                   has been encountered.  R.W. Brode, PES, Inc. - 6/20/95
C
C        INPUTS:  Input Runstream Parameters
C
C        OUTPUTS: Output Option Switches
C
C        CALLED FROM:   OUCARD
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      CHARACTER INPGRP*8, BUF80*80
      LOGICAL FOUND

C     Variable Initializations
      MODNAM = 'OUMXFL'

C     Check If Enough Field
      IF (IFC .EQ. 2) THEN
C        Error Message: No Fields
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      ELSE IF (IFC .LT. 6) THEN
C        Error Message: Not Enough Fields
         CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
         GO TO 999
      ELSE IF (IFC .GT. 7) THEN
C        Error Message: Too Many Fields
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
         GO TO 999
      END IF

C     Retrieve Averaging Period
      IF (FIELD(3) .EQ. 'MONTH' .AND. MONTH) THEN
C        Set Value of IPRDT = 720 for MONTHly Averages
         IPRDT = 720
      ELSE
         CALL STONUM(FIELD(3),40,FNUM,IMIT)
         IF (IMIT .NE. 1) THEN
C           Write Error Message:Invalid Numerical Field
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            GO TO 999
         END IF
         IPRDT = INT(FNUM)
      END IF

C     Check Averaging Period Against KAVE Array
      FOUND = .FALSE.
      J = 1
      DO WHILE (.NOT.FOUND .AND. J.LE.NUMAVE)
         IF (IPRDT .EQ. KAVE(J)) THEN
            FOUND = .TRUE.
            INDAVE = J
         END IF
         J = J + 1
      END DO
      IF (.NOT. FOUND) THEN
C        Error Message:E203 AVEPER Not Match With Pre-Defined One
         CALL ERRHDL(PATH,MODNAM,'E','203','AVEPER')
         GO TO 999
      END IF

C     Retrieve Source Group ID
      INPGRP = FIELD(4)
C     Check Source Group ID
      FOUND = .FALSE.
      J = 1
      DO WHILE (.NOT.FOUND .AND. J.LE.NUMGRP)
         IF (INPGRP .EQ. GRPID(J)) THEN
            FOUND = .TRUE.
            INDGRP = J
         END IF
         J = J + 1
      END DO
      IF (.NOT. FOUND) THEN
C        Error Message:E203 GRPID Not Match With Pre-Defined One
         CALL ERRHDL(PATH,MODNAM,'E','203','GRPID')
         GO TO 999
      END IF

C     Set Switch and Check for Previous MAXIFILE Card
C     for This Averaging Period & Group ID
      MAXFLE(INDGRP,INDAVE) = MAXFLE(INDGRP,INDAVE) + 1
      IF (MAXFLE(INDGRP,INDAVE) .GT. 1) THEN
C        WRITE Error Message
         CALL ERRHDL(PATH,MODNAM,'E','211',KEYWRD)
         GO TO 999
      END IF

C     Retrieve Threshold
      CALL STONUM(FIELD(5),40,FNUM,IMIT)
C     Check for Valid Threshold Value
      IF (IMIT .NE. 1) THEN
C        Write Error Message:Invalid Numerical Field
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         GO TO 999
      END IF
      THRESH(INDGRP,INDAVE) = FNUM

      IF ((LOCE(6)-LOCB(6)) .LE. 39) THEN
C        Retrieve Filename as Character Substring to Maintain Original Case
C        Also Check for Filename Larger Than 40 Characters
         THRFIL(INDGRP,INDAVE) = RUNST1(LOCB(6):LOCE(6))
      ELSE
C        WRITE Error Message:  THRFIL Field is Too Long
         CALL ERRHDL(PATH,MODNAM,'E','203',' FILNAM ')
      END IF

C     Retrieve File Unit If Input, or Assign File Unit and OPEN File
      IF (IFC .EQ. 7) THEN
         CALL STONUM(FIELD(7),40,FNUM,IMIT)
C        Check for Valid Threshold Value
         IF (IMIT .NE. 1) THEN
C           Write Error Message:Invalid Numerical Field
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            GO TO 999
         END IF
C        Check for Conflict With System Files
         IF (FNUM .LT. 20.) THEN
C           WRITE Error Message:  Invalid File Unit Specified
            CALL ERRHDL(PATH,MODNAM,'E','560',KEYWRD)
            GO TO 999
         ELSE IF (FNUM .GT. 100) THEN
C           WRITE Warning Message:  Suspect File Unit Specified
C           Unit May Conflict With Dynamically Allocated File Units
            CALL ERRHDL(PATH,MODNAM,'W','565',KEYWRD)
            IMXUNT(INDGRP,INDAVE) = INT(FNUM)
         ELSE
            IMXUNT(INDGRP,INDAVE) = INT(FNUM)
         END IF
      ELSE
C        Dynamically Allocate File Unit (100's)
         IMXUNT(INDGRP,INDAVE) = 100 + INDGRP*10 + INDAVE
         IF (INDGRP .GE. 10 .OR. INDAVE .GE. 10) THEN
C           WRITE Warning Message: Dynamic FUnit Allocation May Have Conflict
            CALL ERRHDL(PATH,MODNAM,'W','565',KEYWRD)
         END IF
      END IF

C     Check for Earlier Use of This Filename and File Unit
      FOUND = .FALSE.
      DO 200 J = 1, NUMAVE
         DO 100 I = 1, NUMGRP
            IF (I .NE. INDGRP .OR. J .NE. INDAVE) THEN
               IF (THRFIL(INDGRP,INDAVE) .EQ. THRFIL(I,J) .AND.
     &             IMXUNT(INDGRP,INDAVE) .EQ. IMXUNT(I,J)) THEN
                  FOUND = .TRUE.
               ELSE IF (THRFIL(INDGRP,INDAVE) .EQ. THRFIL(I,J) .AND.
     &                  IMXUNT(INDGRP,INDAVE) .NE. IMXUNT(I,J)) THEN
C                 Write Error Message: Conflicting Inputs
                  CALL ERRHDL(PATH,MODNAM,'E','550',KEYWRD)
                  GO TO 999
               ELSE IF (THRFIL(INDGRP,INDAVE) .NE. THRFIL(I,J) .AND.
     &                  IMXUNT(INDGRP,INDAVE) .EQ. IMXUNT(I,J)) THEN
C                 Write Error Message: Conflicting Inputs
                  CALL ERRHDL(PATH,MODNAM,'E','550',KEYWRD)
                  GO TO 999
               END IF
            END IF
 100     CONTINUE
 200  CONTINUE

      IF (.NOT. FOUND) THEN
C        First Time File is Identified - OPEN File
         OPEN(IMXUNT(INDGRP,INDAVE),ERR=99,FILE=THRFIL(INDGRP,INDAVE),
     &        IOSTAT=IOERRN,STATUS='UNKNOWN')
         IF (RSTINP) THEN
C           Results Arrays Are To Be Initialized From Re-start File.
C           Read Start Date From File and Rewind.
            DUMMY = 'INITFILE'
            READ(IRSUNT,ERR=919,END=919) ISDATE
            REWIND IRSUNT
C           Now Position MAXIFILE To End of File, But Not Past ISDATE.
            DUMMY = 'MAXIFILE'
            EOF = .FALSE.
            DO WHILE (.NOT. EOF)
               READ(IMXUNT(INDGRP,INDAVE),'(A80)',ERR=919,END=199) BUF80
               IF (BUF80(1:1) .NE. '*') THEN
C                 Record Is Not Part of Header - Read Date For This Record
                  READ(BUF80(15:22),'(I8)',ERR=919) IDAT
               END IF
               IF (IDAT .GT. ISDATE) THEN
C                 Date of MAXIFILE Event Is Greater Than Start Date
C                 From Save File.  Treat As End of File to Exit Loop.
                  GO TO 199
               END IF
               GO TO 11
 199           EOF = .TRUE.
 11            CONTINUE
            END DO
            EOF = .FALSE.
            BACKSPACE IMXUNT(INDGRP,INDAVE)
C           Skip Header Records
            GO TO 999
         END IF
      ELSE IF (FOUND .AND. RSTINP) THEN
C        This Run is a Re-start From an Earlier Run
C        Skip Header Records
         GO TO 999
      END IF

C     Write Header to File.  Skip Header if FATAL.
      IF (RUN .AND. .NOT.FATAL) THEN
         WRITE(IMXUNT(INDGRP,INDAVE),9005) VERSN, TITLE1
 9005    FORMAT('* ISCST3 (',A5,'): ',A68)
         WRITE(IMXUNT(INDGRP,INDAVE),9007) (MODOPS(I),I=1,17)
 9007    FORMAT('* MODELING OPTIONS USED:',/'* ',17(1X,A6))
         WRITE(IMXUNT(INDGRP,INDAVE),9010) CHRAVE(INDAVE),
     &               THRESH(INDGRP,INDAVE), GRPID(INDGRP), THRFRM
 9010    FORMAT('*',9X,'MAXI-FILE FOR ',A5,' VALUES ',
     &          '>= A THRESHOLD OF ',G12.4,
     &         /'*',9X,'FOR SOURCE GROUP: ',A8,
     &         /'*',9X,'FORMAT: ',A60)
         WRITE(IMXUNT(INDGRP,INDAVE),9020) CHIDEP(1,1), CHIDEP(2,1),
     &                                     CHIDEP(3,1)
 9020    FORMAT('*AVE',3X,'GRP',5X,'DATE',11X,'X',13X,'Y',8X,'ELEV',
     &          4X,'FLAG',3X,3A4,
     &          /'*___',2(1X,'________'),2(2X,'____________'),
     &          2(2X,'______'),'  ____________')
      END IF

      GO TO 999

C     WRITE Error Message for Error Opening File
 99   WRITE(DUMMY,'(5HMAXFL,I3.3)') IMXUNT(INDGRP,INDAVE)
      CALL ERRHDL(PATH,MODNAM,'E','500',DUMMY)

      GO TO 999

C     WRITE Error Message for Error Reading File
 919  CALL ERRHDL(PATH,MODNAM,'E','510',DUMMY)

C     Set Logical Switch Indicating That Maximum Value File(s) Are Generated
 999  MXFILE = .TRUE.

      RETURN
      END

      SUBROUTINE OUPOST
C***********************************************************************
C                 OUPOST Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: To Process Post-processor File Output Selections
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        MODIFIED:  To Change File Length Limit To 40 - 9/29/92
C        MODIFIED BY D. Strimaitis, SRC (for Wet & Dry DEPOSITION) - 11/8/93
C        MODIFIED:  To skip writing of header records if FATAL error
C                   has been encountered.  R.W. Brode, PES, Inc. - 6/20/95
C        MODIFIED:  To change buffer length for PLOT format to 132, and
C                   change read statement to allow for multiple output
C                   types (CONC/DEPOS/etc.).   R.W. Brode, PES, Inc., 6/20/95
C
C        INPUTS:  Input Runstream Parameters
C
C        OUTPUTS: Output Option Switches
C
C        CALLED FROM:   OUCARD
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      CHARACTER INPGRP*8, BUF132*132, HDRFRM*256
      LOGICAL FOUND

C     Variable Initializations
      MODNAM = 'OUPOST'

C     Create Header Format for Columns
      WRITE(HDRFRM,9020) NUMTYP, NUMTYP+2

C     Check If Enough Fields
      IF (IFC .EQ. 2) THEN
C        Error Message: No Fields
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      ELSE IF (IFC .LT. 6) THEN
C        Error Message: Not Enough Fields
         CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
         GO TO 999
      ELSE IF (IFC .GT. 7) THEN
C        Error Message: Too Many Fields
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
         GO TO 999
      END IF

C     Retrieve Averaging Period
      IF (FIELD(3) .EQ. 'PERIOD' .AND. PERIOD) THEN
C        Post File is for PERIOD Averages                   ---   CALL PERPST
         CALL PERPST
C        Exit to End
         GO TO 999
      ELSE IF (FIELD(3) .EQ. 'ANNUAL' .AND. ANNUAL) THEN
C        Post File is for PERIOD Averages                   ---   CALL PERPST
         CALL PERPST
C        Exit to End
         GO TO 999
      ELSE IF (FIELD(3).EQ.'PERIOD' .OR. FIELD(3).EQ.'ANNUAL') THEN
C        Period Post File Selected But No PERIOD Averages Calculated
C        WRITE Error Message: Invalid Averaging Period Selected for POSTFILE
         CALL ERRHDL(PATH,MODNAM,'E','203',' AVEPER ')
         GO TO 999
      ELSE IF (FIELD(3) .EQ. 'MONTH' .AND. MONTH) THEN
C        Set Value of IPRDT = 720 for MONTHly Averages
         IPRDT = 720
      ELSE
         CALL STONUM(FIELD(3),40,FNUM,IMIT)
         IF (IMIT .NE. 1) THEN
C           Write Error Message:Invalid Numerical Field
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            GO TO 999
         END IF
         IPRDT = INT(FNUM)
      END IF

C     Check Averaging Period Against KAVE Array
      FOUND = .FALSE.
      J = 1
      DO WHILE (.NOT.FOUND .AND. J.LE.NUMAVE)
         IF (IPRDT .EQ. KAVE(J)) THEN
            FOUND = .TRUE.
            INDAVE = J
         END IF
         J = J + 1
      END DO
      IF (.NOT. FOUND) THEN
C        Error Message:E203 AVEPER Not Match With Pre-Defined One
         CALL ERRHDL(PATH,MODNAM,'E','203','AVEPER')
         GO TO 999
      END IF

C     Retrieve Source Group ID
      INPGRP = FIELD(4)
C     Check Source Group ID
      FOUND = .FALSE.
      J = 1
      DO WHILE (.NOT.FOUND .AND. J.LE.NUMGRP)
         IF (INPGRP .EQ. GRPID(J)) THEN
            FOUND = .TRUE.
            INDGRP = J
         END IF
         J = J + 1
      END DO
      IF (.NOT. FOUND) THEN
C        Error Message:E203 GRPID Not Match With Pre-Defined One
         CALL ERRHDL(PATH,MODNAM,'E','203','GRPID')
         GO TO 999
      END IF

C     Retrieve Format Secondary Keyword
      IF (FIELD(5) .EQ. 'UNFORM') THEN
         IPSFRM(INDGRP,INDAVE) = 0
      ELSE IF (FIELD(5) .EQ. 'PLOT') THEN
         IPSFRM(INDGRP,INDAVE) = 1
      ELSE
C        Error Message: Invalid Format Specified for POSTFILE
         CALL ERRHDL(PATH,MODNAM,'E','203','FORMAT')
         GO TO 999
      END IF

C     Set Switch and Check for Previous POSTFILE Card
C     for This Averaging Period & Group ID
      IPSTFL(INDGRP,INDAVE) = IPSTFL(INDGRP,INDAVE) + 1
      IF (IPSTFL(INDGRP,INDAVE) .GT. 1) THEN
C        WRITE Error Message
         CALL ERRHDL(PATH,MODNAM,'E','211',KEYWRD)
         GO TO 999
      END IF

      IF ((LOCE(6)-LOCB(6)) .LE. 39) THEN
C        Retrieve Filename as Character Substring to Maintain Original Case
C        Also Check for Filename Larger Than 40 Characters
         PSTFIL(INDGRP,INDAVE) = RUNST1(LOCB(6):LOCE(6))
      ELSE
C        WRITE Error Message:  PSTFIL Field is Too Long
         CALL ERRHDL(PATH,MODNAM,'E','203',' FILNAM ')
      END IF

C     Retrieve File Unit If Input, or Assign File Unit and OPEN File
      IF (IFC .EQ. 7) THEN
         CALL STONUM(FIELD(7),40,FNUM,IMIT)
C        Check for Valid Threshold Value
         IF (IMIT .NE. 1) THEN
C           Write Error Message: Invalid Numerical Field
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            GO TO 999
         END IF
C        Check for Conflict With System Files
         IF (FNUM .LT. 20.) THEN
C           WRITE Error Message: Invalid File Unit Specified
            CALL ERRHDL(PATH,MODNAM,'E','560',KEYWRD)
            GO TO 999
         ELSE IF (FNUM .GT. 100) THEN
C           WRITE Warning Message:  Suspect File Unit Specified
C           Unit May Conflict With Dynamically Allocated File Units
            CALL ERRHDL(PATH,MODNAM,'W','565',KEYWRD)
            IPSUNT(INDGRP,INDAVE) = INT(FNUM)
         ELSE
            IPSUNT(INDGRP,INDAVE) = INT(FNUM)
         END IF
      ELSE
C        Dynamically Allocate File Unit (200's)
         IPSUNT(INDGRP,INDAVE) = 200 + INDGRP*10 + INDAVE
         IF (INDGRP .GE. 10 .OR. INDAVE .GE. 10) THEN
C           WRITE Warning Message: Dynamic FUnit Allocation May Have Conflict
            CALL ERRHDL(PATH,MODNAM,'W','565',KEYWRD)
         END IF
      END IF

C     Check for Earlier Use of This Filename and File Unit
      FOUND = .FALSE.
      DO 200 J = 1, NUMAVE
         DO 100 I = 1, NUMGRP
            IF (I .NE. INDGRP .OR. J .NE. INDAVE) THEN
               IF (PSTFIL(INDGRP,INDAVE) .EQ. PSTFIL(I,J) .AND.
     &             IPSUNT(INDGRP,INDAVE) .EQ. IPSUNT(I,J)) THEN
                  FOUND = .TRUE.
               ELSE IF (PSTFIL(INDGRP,INDAVE) .EQ. PSTFIL(I,J) .AND.
     &                  IPSUNT(INDGRP,INDAVE) .NE. IPSUNT(I,J)) THEN
C                 Write Error Message: Conflicting Inputs
                  CALL ERRHDL(PATH,MODNAM,'E','550',KEYWRD)
                  GO TO 999
               ELSE IF (PSTFIL(INDGRP,INDAVE) .NE. PSTFIL(I,J) .AND.
     &                  IPSUNT(INDGRP,INDAVE) .EQ. IPSUNT(I,J)) THEN
C                 Write Error Message: Conflicting Inputs
                  CALL ERRHDL(PATH,MODNAM,'E','550',KEYWRD)
                  GO TO 999
               END IF
            END IF
 100     CONTINUE
 200  CONTINUE

C     Check Against POSTFILEs for PERIOD Averages
      DO 300 I = 1, NUMGRP
         IF (PSTFIL(INDGRP,INDAVE) .EQ. ANNPST(I) .AND.
     &       IPSUNT(INDGRP,INDAVE) .EQ. IAPUNT(I)) THEN
           FOUND = .TRUE.
         ELSE IF (PSTFIL(INDGRP,INDAVE) .EQ. ANNPST(I) .AND.
     &            IPSUNT(INDGRP,INDAVE) .NE. IAPUNT(I)) THEN
C          Write Error Message: Conflicting Inputs
           CALL ERRHDL(PATH,MODNAM,'E','550',KEYWRD)
           GO TO 999
         ELSE IF (PSTFIL(INDGRP,INDAVE) .NE. ANNPST(I) .AND.
     &            IPSUNT(INDGRP,INDAVE) .EQ. IAPUNT(I)) THEN
C          Write Error Message: Conflicting Inputs
           CALL ERRHDL(PATH,MODNAM,'E','550',KEYWRD)
           GO TO 999
         END IF
 300  CONTINUE

      IF (.NOT. FOUND) THEN
C        First Time File is Identified - OPEN File
         IF (FIELD(5) .EQ. 'UNFORM') THEN
           OPEN(IPSUNT(INDGRP,INDAVE),ERR=99,FILE=PSTFIL(INDGRP,INDAVE),
     &          IOSTAT=IOERRN,FORM='UNFORMATTED',STATUS='UNKNOWN')
            IF (RSTINP) THEN
C              Results Arrays Are To Be Initialized From Re-start File.
C              Read Start Date From File and Rewind.
               DUMMY = 'INITFILE'
               READ(IRSUNT,ERR=919,END=919) ISDATE
               REWIND IRSUNT
C              Now Position POSTFILE To End of File, But Not Past ISDATE.
               DUMMY = 'POSTFILE'
               EOF = .FALSE.
               DO WHILE (.NOT. EOF)
                  READ(IPSUNT(INDGRP,INDAVE),ERR=919,END=199) IDAT
                  IF (IDAT .GT. ISDATE) THEN
C                    Date of POSTFILE Record Is Greater Than Start Date
C                    From Save File.  Treat As End of File to Exit Loop.
                     GO TO 199
                  END IF
                  GO TO 11
 199              EOF = .TRUE.
 11               CONTINUE
               END DO
               EOF = .FALSE.
               BACKSPACE IPSUNT(INDGRP,INDAVE)
C              Skip Header Records
               GO TO 999
            END IF
         ELSE IF (FIELD(5) .EQ. 'PLOT') THEN
           OPEN(IPSUNT(INDGRP,INDAVE),ERR=99,FILE=PSTFIL(INDGRP,INDAVE),
     &          IOSTAT=IOERRN,FORM='FORMATTED',STATUS='UNKNOWN')
            IF (RSTINP) THEN
C              Results Arrays Are To Be Initialized From Re-start File.
C              Read Start Date From File and Rewind.
               DUMMY = 'INITFILE'
               READ(IRSUNT,ERR=919,END=919) ISDATE
               REWIND IRSUNT
C              Now Position POSTFILE To End of File, But Not Past ISDATE.
               DUMMY = 'POSTFILE'
               EOF = .FALSE.
               DO WHILE (.NOT. EOF)
                  READ(IPSUNT(INDGRP,INDAVE),'(A132)',
     &                                        ERR=919,END=299) BUF132
                  IF (BUF132(1:1) .NE. '*') THEN
C                    Record Is Not Part of Header - Read Date For This Record
C                    First calculate start & end of date string based on NUMTYP
                     ISTR = 72 + 14*(NUMTYP-1)
                     ISTP = ISTR + 7
                     READ(BUF132(ISTR:ISTP),'(I8)',ERR=919) IDAT
                  END IF
                  IF (IDAT .GT. ISDATE) THEN
C                    Date of POSTFILE Record Is Greater Than Start Date
C                    From Save File.  Treat As End of File to Exit Loop.
                     GO TO 299
                  END IF
                  GO TO 21
 299              EOF = .TRUE.
 21               CONTINUE
               END DO
               EOF = .FALSE.
               BACKSPACE IPSUNT(INDGRP,INDAVE)
C              Skip Header Records
               GO TO 999
            END IF
         END IF
      ELSE IF (FOUND .AND. RSTINP) THEN
C        This Run is a Re-start From an Earlier Run
C        Skip Header Records
         GO TO 999
      END IF

C     Write Header to File for Formatted Plot Files.  Skip Header if FATAL.
      IF (RUN .AND. .NOT.FATAL .AND. FIELD(5) .EQ. 'PLOT') THEN
         WRITE(IPSUNT(INDGRP,INDAVE),9005) VERSN, TITLE1
 9005    FORMAT('* ISCST3 (',A5,'): ',A68)
         WRITE(IPSUNT(INDGRP,INDAVE),9007) (MODOPS(I),I=1,17)
 9007    FORMAT('* MODELING OPTIONS USED:',/'* ',17(1X,A6))
         WRITE(IPSUNT(INDGRP,INDAVE),9010) CHRAVE(INDAVE),GRPID(INDGRP),
     &                                     NUMREC, PSTFRM
 9010    FORMAT('*',9X,'POST/PLOT FILE OF CONCURRENT ',A5,' VALUES',
     &         ' FOR SOURCE GROUP: ',A8,
     &         /'*',9X,'FOR A TOTAL OF ',I5,' RECEPTORS.',
     &         /'*',9X,'FORMAT: ',A60)
         WRITE(IPSUNT(INDGRP,INDAVE),HDRFRM) (CHIDEP(1,ITYP),
     &                   CHIDEP(2,ITYP), CHIDEP(3,ITYP), ITYP=1,NUMTYP)
 9020 FORMAT('(''*'',8X,''X'',13X,''Y'',4X,',I1,'(2X,3A4),3X,''ZELEV'',
     &  5X,''AVE'',5X,''GRP'',7X,''DATE'',5X,''NET ID'',/,''*'',2X,',
     &  I1,'(''___________'',3X),''______  ______  ________  ________'',
     &  ''  ________'')')
      END IF

      GO TO 999

C     WRITE Error Message for Error Opening File
 99   WRITE(DUMMY,'(5HPSTFL,I3.3)') IPSUNT(INDGRP,INDAVE)
      CALL ERRHDL(PATH,MODNAM,'E','500',DUMMY)

      GO TO 999

C     WRITE Error Message for Error Reading File
 919  CALL ERRHDL(PATH,MODNAM,'E','510',DUMMY)

C     Set Logical Switch Indicating That Postprocessor File(s) Are Generated
 999  PPFILE = .TRUE.

      RETURN
      END

      SUBROUTINE OUPLOT
C***********************************************************************
C                 OUPLOT Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: To Process Plot File Output Selections
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        MODIFIED:  To Change File Length Limit To 40 - 9/29/92
C
C        INPUTS:  Input Runstream Parameters
C
C        OUTPUTS: Output Option Switches
C
C        CALLED FROM:   OUCARD
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      CHARACTER NCHR1(10)*8, NCHR2(10)*4, INPGRP*8
      LOGICAL FOUND

C     Variable Initializations
      DATA (NCHR1(I),I=1,10) /'FIRST','SECOND','THIRD','FOURTH',
     &                        'FIFTH','SIXTH','SEVENTH','EIGHTH',
     &                        'NINTH','TENTH'/
      DATA (NCHR2(I),I=1,10) /'1ST','2ND','3RD','4TH','5TH','6TH',
     &                        '7TH','8TH','9TH','10TH'/
      MODNAM = 'OUPLOT'

C     Check If Enough Field
      IF (IFC .EQ. 2) THEN
C        Error Message: No Fields
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      ELSE IF (IFC .LT. 5) THEN
C        Error Message: Not Enough Fields
         CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
         GO TO 999
      ELSE IF (IFC .GT. 7) THEN
C        Error Message: Too Many Fields
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
         GO TO 999
      END IF

C     Retrieve Averaging Period
      IF (FIELD(3) .EQ. 'PERIOD' .AND. PERIOD) THEN
C        Plot File is for PERIOD Averages                   ---   CALL PERPLT
         CALL PERPLT
C        Exit to End
         GO TO 999
      ELSE IF (FIELD(3) .EQ. 'ANNUAL' .AND. ANNUAL) THEN
C        Plot File is for PERIOD Averages                   ---   CALL PERPLT
         CALL PERPLT
C        Exit to End
         GO TO 999
      ELSE IF (FIELD(3).EQ.'PERIOD' .OR. FIELD(3).EQ.'ANNUAL') THEN
C        Period Plot File Selected But No PERIOD Averages Calculated
C        WRITE Error Message: Invalid Averaging Period Selected for PLOTFILE
         CALL ERRHDL(PATH,MODNAM,'E','203',' AVEPER ')
         GO TO 999
      ELSE IF (FIELD(3) .EQ. 'MONTH' .AND. MONTH) THEN
C        Set Value of IPRDT = 720 for MONTHly Averages
         IPRDT = 720
      ELSE
         CALL STONUM(FIELD(3),40,FNUM,IMIT)
         IF (IMIT .NE. 1) THEN
C           Write Error Message:Invalid Numerical Field
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            GO TO 999
         END IF
         IPRDT = INT(FNUM)
      END IF

C     Check Short Term Averaging Period Against KAVE Array
      FOUND = .FALSE.
      J = 1
      DO WHILE (.NOT.FOUND .AND. J.LE.NUMAVE)
         IF (IPRDT .EQ. KAVE(J)) THEN
            FOUND = .TRUE.
            INDAVE = J
         END IF
         J = J + 1
      END DO
      IF (.NOT. FOUND) THEN
C        Error Message:E203 AVEPER Not Match With Pre-Defined One
         CALL ERRHDL(PATH,MODNAM,'E','203','AVEPER')
         GO TO 999
      END IF

C     Retrieve Source Group ID
      INPGRP = FIELD(4)
C     Check Source Group ID
      FOUND = .FALSE.
      J = 1
      DO WHILE (.NOT.FOUND .AND. J.LE.NUMGRP)
         IF (INPGRP .EQ. GRPID(J)) THEN
            FOUND = .TRUE.
            INDGRP = J
         END IF
         J = J + 1
      END DO
      IF (.NOT. FOUND) THEN
C        Error Message:E203 GRPID Not Match With Pre-Defined One
         CALL ERRHDL(PATH,MODNAM,'E','203','GRPID')
         GO TO 999
      END IF

C     Retrieve High Value
      FOUND = .FALSE.
      DO 50 I = 1, 10
         IF (FIELD(5).EQ.NCHR1(I) .OR. FIELD(5).EQ.NCHR2(I)) THEN
            FOUND = .TRUE.
            INDVAL = I
         END IF
 50   CONTINUE
      IF (.NOT. FOUND) THEN
C        Error Message:E203 INDVAL Not Match With Options
         CALL ERRHDL(PATH,MODNAM,'E','203','HIVALU')
         GO TO 999
      END IF

C     Check High Value Specified Against Previous Options
      IF (NHIAVE(INDVAL,INDAVE) .NE. 1) THEN
C        WRITE Error Message
         CALL ERRHDL(PATH,MODNAM,'E','203','HIVALU')
         GO TO 999
      END IF

C     Set Switch and Check for Previous PLOTFILE Card
C     for This Averaging Period & Group ID
      IPLTFL(INDVAL,INDGRP,INDAVE) = IPLTFL(INDVAL,INDGRP,INDAVE) + 1
      IF (IPLTFL(INDVAL,INDGRP,INDAVE) .GT. 1) THEN
C        WRITE Error Message
         CALL ERRHDL(PATH,MODNAM,'E','211',KEYWRD)
         GO TO 999
      END IF

      IF ((LOCE(6)-LOCB(6)) .LE. 39) THEN
C        Retrieve Filename as Character Substring to Maintain Original Case
C        Also Check for Filename Larger Than 40 Characters
         PLTFIL(INDVAL,INDGRP,INDAVE) = RUNST1(LOCB(6):LOCE(6))
      ELSE
C        WRITE Error Message:  PLTFIL Field is Too Long
         CALL ERRHDL(PATH,MODNAM,'E','203',' FILNAM ')
      END IF

C     Retrieve File Unit If Input, or Assign File Unit and OPEN File
      IF (IFC .EQ. 7) THEN
         CALL STONUM(FIELD(7),40,FNUM,IMIT)
C        Check for Valid Threshold Value
         IF (IMIT .NE. 1) THEN
C           Write Error Message: Invalid Numerical Field
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            GO TO 999
         END IF
C        Check for Conflict With System Files
         IF (FNUM .LT. 20.) THEN
C           WRITE Error Message: Invalid File Unit Specified
            CALL ERRHDL(PATH,MODNAM,'E','560',KEYWRD)
            GO TO 999
         ELSE IF (FNUM .GT. 100) THEN
C           WRITE Warning Message:  Suspect File Unit Specified
C           Unit May Conflict With Dynamically Allocated File Units
            CALL ERRHDL(PATH,MODNAM,'W','565',KEYWRD)
            IPLUNT(INDVAL,INDGRP,INDAVE) = INT(FNUM)
         ELSE
            IPLUNT(INDVAL,INDGRP,INDAVE) = INT(FNUM)
         END IF
      ELSE
C        Dynamically Allocate File Unit (> 400)
         IPLUNT(INDVAL,INDGRP,INDAVE) = (INDVAL+3)*100+INDGRP*10+INDAVE
         IF (INDGRP .GE. 10 .OR. INDAVE .GE. 10) THEN
C           WRITE Warning Message: Dynamic FUnit Allocation May Have Conflict
            CALL ERRHDL(PATH,MODNAM,'W','565',KEYWRD)
         END IF
      END IF

C     Check for Earlier Use of This Filename and File Unit
      FOUND = .FALSE.
      DO 300 K = 1, NUMAVE
        DO 200 J = 1, NUMGRP
          DO 100 I = 1, NHIVAL
            IF (I.NE.INDVAL .OR. J.NE.INDGRP .OR. K.NE.INDAVE) THEN
              IF (PLTFIL(INDVAL,INDGRP,INDAVE) .EQ. PLTFIL(I,J,K) .AND.
     &            IPLUNT(INDVAL,INDGRP,INDAVE) .EQ. IPLUNT(I,J,K)) THEN
                FOUND = .TRUE.
              ELSEIF (PLTFIL(INDVAL,INDGRP,INDAVE).EQ.PLTFIL(I,J,K).AND.
     &                IPLUNT(INDVAL,INDGRP,INDAVE).NE.IPLUNT(I,J,K))THEN
C               Write Error Message: Conflicting Inputs
                CALL ERRHDL(PATH,MODNAM,'E','550',KEYWRD)
                GO TO 999
              ELSEIF (PLTFIL(INDVAL,INDGRP,INDAVE).NE.PLTFIL(I,J,K).AND.
     &                IPLUNT(INDVAL,INDGRP,INDAVE).EQ.IPLUNT(I,J,K))THEN
C               Write Error Message: Conflicting Inputs
                CALL ERRHDL(PATH,MODNAM,'E','550',KEYWRD)
                GO TO 999
              END IF
            END IF
 100      CONTINUE
 200    CONTINUE
 300  CONTINUE

C     Check Against PLOTFILEs for PERIOD Averages
      DO 400 I = 1, NUMGRP
         IF (PLTFIL(INDVAL,INDGRP,INDAVE) .EQ. ANNPLT(I) .AND.
     &       IPLUNT(INDVAL,INDGRP,INDAVE) .EQ. IPPUNT(I)) THEN
           FOUND = .TRUE.
         ELSE IF (PLTFIL(INDVAL,INDGRP,INDAVE) .EQ. ANNPLT(I) .AND.
     &            IPLUNT(INDVAL,INDGRP,INDAVE) .NE. IPPUNT(I)) THEN
C          Write Error Message: Conflicting Inputs
           CALL ERRHDL(PATH,MODNAM,'E','550',KEYWRD)
           GO TO 999
         ELSE IF (PLTFIL(INDVAL,INDGRP,INDAVE) .NE. ANNPLT(I) .AND.
     &            IPLUNT(INDVAL,INDGRP,INDAVE) .EQ. IPPUNT(I)) THEN
C          Write Error Message: Conflicting Inputs
           CALL ERRHDL(PATH,MODNAM,'E','550',KEYWRD)
           GO TO 999
         END IF
 400  CONTINUE

      IF (.NOT. FOUND) THEN
C        First Time File is Identified - OPEN File
         OPEN(IPLUNT(INDVAL,INDGRP,INDAVE),ERR=99,
     &        FILE=PLTFIL(INDVAL,INDGRP,INDAVE),
     &        IOSTAT=IOERRN,STATUS='UNKNOWN')
      END IF

C     Set Logical Switch Indicating That Plot File(s) Are Generated
      PLFILE = .TRUE.

      GO TO 999

C     WRITE Error Message for Error Opening File
 99   WRITE(DUMMY,'(5HPLTFL,I3.3)') IPLUNT(INDVAL,INDGRP,INDAVE)
      CALL ERRHDL(PATH,MODNAM,'E','500',DUMMY)

 999  RETURN
      END

      SUBROUTINE PERPST
C***********************************************************************
C                 PERPST Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: To Process Postprocessor File Output Selection for PERIOD
C                 Averages
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        MODIFIED:  To Change File Length Limit To 40 - 9/29/92
C
C        INPUTS:  Input Runstream Parameters
C
C        OUTPUTS: Output Option Switches
C
C        CALLED FROM:   OUPLOT
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      CHARACTER INPGRP*8
      LOGICAL FOUND

C     Variable Initializations
      MODNAM = 'PERPST'

C     Check If Too Many Fields
      IF (IFC .GT. 7) THEN
C        Error Message: Too Many Fields
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
         GO TO 999
      END IF

C     Retrieve Source Group ID
      INPGRP = FIELD(4)
C     Check Source Group ID
      FOUND = .FALSE.
      J = 1
      DO WHILE (.NOT.FOUND .AND. J.LE.NUMGRP)
         IF (INPGRP .EQ. GRPID(J)) THEN
            FOUND = .TRUE.
            INDGRP = J
         END IF
         J = J + 1
      END DO
      IF (.NOT. FOUND) THEN
C        Error Message: E203 GRPID Not Match With Pre-Defined One
         CALL ERRHDL(PATH,MODNAM,'E','203','GRPID')
         GO TO 999
      END IF

C     Set Switch and Check for Previous POSTFILE Card
C     for This Averaging Period & Group ID
      IANPST(INDGRP) = IANPST(INDGRP) + 1
      IF (IANPST(INDGRP) .GT. 1) THEN
C        WRITE Error Message
         CALL ERRHDL(PATH,MODNAM,'E','211',KEYWRD)
         GO TO 999
      END IF

C     Retrieve Format Secondary Keyword
      IF (FIELD(5) .EQ. 'UNFORM') THEN
         IANFRM(INDGRP) = 0
      ELSE IF (FIELD(5) .EQ. 'PLOT') THEN
         IANFRM(INDGRP) = 1
      ELSE
C        Error Message: Invalid Format Specified for POSTFILE
         CALL ERRHDL(PATH,MODNAM,'E','203','FORMAT')
         GO TO 999
      END IF

      IF ((LOCE(6)-LOCB(6)) .LE. 39) THEN
C        Retrieve Filename as Character Substring to Maintain Original Case
C        Also Check for Filename Larger Than 40 Characters
         ANNPST(INDGRP) = RUNST1(LOCB(6):LOCE(6))
      ELSE
C        WRITE Error Message:  ANNPST Field is Too Long
         CALL ERRHDL(PATH,MODNAM,'E','203',' FILNAM ')
      END IF

C     Retrieve File Unit If Input, or Assign File Unit and OPEN File
      IF (IFC .EQ. 7) THEN
         CALL STONUM(FIELD(7),40,FNUM,IMIT)
C        Check for Valid Threshold Value
         IF (IMIT .NE. 1) THEN
C           Write Error Message: Invalid Numerical Field
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            GO TO 999
         END IF
C        Check for Conflict With System Files
         IF (FNUM .LT. 20.) THEN
C           WRITE Error Message: Invalid File Unit Specified
            CALL ERRHDL(PATH,MODNAM,'E','560',KEYWRD)
            GO TO 999
         ELSE IF (FNUM .GT. 100) THEN
C           WRITE Warning Message:  Suspect File Unit Specified
C           Unit May Conflict With Dynamically Allocated File Units
            CALL ERRHDL(PATH,MODNAM,'W','565',KEYWRD)
            IAPUNT(INDGRP) = INT(FNUM)
         ELSE
            IAPUNT(INDGRP) = INT(FNUM)
         END IF
      ELSE
C        Dynamically Allocate File Unit (300's)
         IAPUNT(INDGRP) = 300 + INDGRP*10 - 5
         IF (INDGRP .GE. 10) THEN
C           WRITE Warning Message: Dynamic FUnit Allocation May Have Conflict
            CALL ERRHDL(PATH,MODNAM,'W','565',KEYWRD)
         END IF
      END IF

C     Check for Earlier Use of This Filename and File Unit
      FOUND = .FALSE.
      DO 100 I = 1, NUMGRP
         IF (I .NE. INDGRP) THEN
            IF (ANNPST(INDGRP) .EQ. ANNPST(I) .AND.
     &          IAPUNT(INDGRP) .EQ. IAPUNT(I)) THEN
              FOUND = .TRUE.
            ELSE IF (ANNPST(INDGRP) .EQ. ANNPST(I) .AND.
     &               IAPUNT(INDGRP) .NE. IAPUNT(I)) THEN
C             Write Error Message: Conflicting Inputs
              CALL ERRHDL(PATH,MODNAM,'E','550',KEYWRD)
              GO TO 999
            ELSE IF (ANNPST(INDGRP) .NE. ANNPST(I) .AND.
     &               IAPUNT(INDGRP) .EQ. IAPUNT(I)) THEN
C             Write Error Message: Conflicting Inputs
              CALL ERRHDL(PATH,MODNAM,'E','550',KEYWRD)
              GO TO 999
            END IF
         END IF
 100  CONTINUE

C     Check Against POSTFILEs for Short Term Averages
      DO 300 J = 1, NUMAVE
         DO 200 I = 1, NUMGRP
            IF (ANNPST(INDGRP) .EQ. PSTFIL(I,J) .AND.
     &          IAPUNT(INDGRP) .EQ. IPSUNT(I,J)) THEN
               FOUND = .TRUE.
            ELSE IF (ANNPST(INDGRP) .EQ. PSTFIL(I,J) .AND.
     &               IAPUNT(INDGRP) .NE. IPSUNT(I,J)) THEN
C              Write Error Message: Conflicting Inputs
               CALL ERRHDL(PATH,MODNAM,'E','550',KEYWRD)
               GO TO 999
            ELSE IF (ANNPST(INDGRP) .NE. PSTFIL(I,J) .AND.
     &               IAPUNT(INDGRP) .EQ. IPSUNT(I,J)) THEN
C              Write Error Message: Conflicting Inputs
               CALL ERRHDL(PATH,MODNAM,'E','550',KEYWRD)
               GO TO 999
            END IF
 200     CONTINUE
 300  CONTINUE

      IF (.NOT. FOUND) THEN
C        First Time File is Identified - OPEN File
         IF (FIELD(5) .EQ. 'UNFORM') THEN
            OPEN(IAPUNT(INDGRP),ERR=99,FILE=ANNPST(INDGRP),
     &           IOSTAT=IOERRN,FORM='UNFORMATTED',STATUS='UNKNOWN')
         ELSE IF (FIELD(5) .EQ. 'PLOT') THEN
            OPEN(IAPUNT(INDGRP),ERR=99,FILE=ANNPST(INDGRP),
     &           IOSTAT=IOERRN,FORM='FORMATTED',STATUS='UNKNOWN')
         END IF
      END IF

C     Set Logical Switch Indicating That Post File(s) Are Generated
      ANPOST = .TRUE.

      GO TO 999

C     WRITE Error Message for Error Opening File
 99   WRITE(DUMMY,'(5HPSTFL,I3.3)') IAPUNT(INDGRP)
      CALL ERRHDL(PATH,MODNAM,'E','500',DUMMY)

 999  RETURN
      END

      SUBROUTINE PERPLT
C***********************************************************************
C                 PERPLT Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: To Process Plot File Output Selection for PERIOD
C                 Averages
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        MODIFIED:  To Change File Length Limit To 40 - 9/29/92
C
C        INPUTS:  Input Runstream Parameters
C
C        OUTPUTS: Output Option Switches
C
C        CALLED FROM:   OUPLOT
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      CHARACTER INPGRP*8
      LOGICAL FOUND

C     Variable Initializations
      MODNAM = 'PERPLT'

C     Check If Too Many Fields
      IF (IFC .GT. 6) THEN
C        Error Message: Too Many Fields
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
         GO TO 999
      END IF

C     Retrieve Source Group ID
      INPGRP = FIELD(4)
C     Check Source Group ID
      FOUND = .FALSE.
      J = 1
      DO WHILE (.NOT.FOUND .AND. J.LE.NUMGRP)
         IF (INPGRP .EQ. GRPID(J)) THEN
            FOUND = .TRUE.
            INDGRP = J
         END IF
         J = J + 1
      END DO
      IF (.NOT. FOUND) THEN
C        Error Message: E203 GRPID Not Match With Pre-Defined One
         CALL ERRHDL(PATH,MODNAM,'E','203','GRPID')
         GO TO 999
      END IF

C     Set Switch and Check for Previous PLOTFILE Card
C     for This Averaging Period & Group ID
      IANPLT(INDGRP) = IANPLT(INDGRP) + 1
      IF (IANPLT(INDGRP) .GT. 1) THEN
C        WRITE Error Message
         CALL ERRHDL(PATH,MODNAM,'E','211',KEYWRD)
         GO TO 999
      END IF

      IF ((LOCE(6)-LOCB(6)) .LE. 39) THEN
C        Retrieve Filename as Character Substring to Maintain Original Case
C        Also Check for Filename Larger Than 40 Characters
         ANNPLT(INDGRP) = RUNST1(LOCB(5):LOCE(5))
      ELSE
C        WRITE Error Message:  ANNPLT Field is Too Long
         CALL ERRHDL(PATH,MODNAM,'E','203',' FILNAM ')
      END IF

C     Retrieve File Unit If Input, or Assign File Unit and OPEN File
      IF (IFC .EQ. 6) THEN
         CALL STONUM(FIELD(6),40,FNUM,IMIT)
C        Check for Valid Threshold Value
         IF (IMIT .NE. 1) THEN
C           Write Error Message: Invalid Numerical Field
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            GO TO 999
         END IF
C        Check for Conflict With System Files
         IF (FNUM .LT. 20.) THEN
C           WRITE Error Message: Invalid File Unit Specified
            CALL ERRHDL(PATH,MODNAM,'E','560',KEYWRD)
            GO TO 999
         ELSE IF (FNUM .GT. 100) THEN
C           WRITE Warning Message:  Suspect File Unit Specified
C           Unit May Conflict With Dynamically Allocated File Units
            CALL ERRHDL(PATH,MODNAM,'W','565',KEYWRD)
            IPPUNT(INDGRP) = INT(FNUM)
         ELSE
            IPPUNT(INDGRP) = INT(FNUM)
         END IF
      ELSE
C        Dynamically Allocate File Unit (300's)
         IPPUNT(INDGRP) = 300 + INDGRP*10
         IF (INDGRP .GE. 10) THEN
C           WRITE Warning Message: Dynamic FUnit Allocation May Have Conflict
            CALL ERRHDL(PATH,MODNAM,'W','565',KEYWRD)
         END IF
      END IF

C     Check for Earlier Use of This Filename and File Unit
      FOUND = .FALSE.
      DO 100 I = 1, NUMGRP
         IF (I .NE. INDGRP) THEN
            IF (ANNPLT(INDGRP) .EQ. ANNPLT(I) .AND.
     &          IPPUNT(INDGRP) .EQ. IPPUNT(I)) THEN
              FOUND = .TRUE.
            ELSE IF (ANNPLT(INDGRP) .EQ. ANNPLT(I) .AND.
     &               IPPUNT(INDGRP) .NE. IPPUNT(I)) THEN
C             Write Error Message: Conflicting Inputs
              CALL ERRHDL(PATH,MODNAM,'E','550',KEYWRD)
              GO TO 999
            ELSE IF (ANNPLT(INDGRP) .NE. ANNPLT(I) .AND.
     &               IPPUNT(INDGRP) .EQ. IPPUNT(I)) THEN
C             Write Error Message: Conflicting Inputs
              CALL ERRHDL(PATH,MODNAM,'E','550',KEYWRD)
              GO TO 999
            END IF
         END IF
 100  CONTINUE

C     Check Against PLOTFILEs for Short Term Averages
      DO 400 K = 1, NUMAVE
         DO 300 J = 1, NUMGRP
            DO 200 I = 1, NHIVAL
               IF (ANNPLT(INDGRP) .EQ. PLTFIL(I,J,K) .AND.
     &             IPPUNT(INDGRP) .EQ. IPLUNT(I,J,K)) THEN
                  FOUND = .TRUE.
               ELSE IF (ANNPLT(INDGRP) .EQ. PLTFIL(I,J,K) .AND.
     &                  IPPUNT(INDGRP) .NE. IPLUNT(I,J,K)) THEN
C                 Write Error Message: Conflicting Inputs
                  CALL ERRHDL(PATH,MODNAM,'E','550',KEYWRD)
                  GO TO 999
               ELSE IF (ANNPLT(INDGRP) .NE. PLTFIL(I,J,K) .AND.
     &                  IPPUNT(INDGRP) .EQ. IPLUNT(I,J,K)) THEN
C                 Write Error Message: Conflicting Inputs
                  CALL ERRHDL(PATH,MODNAM,'E','550',KEYWRD)
                  GO TO 999
               END IF
 200        CONTINUE
 300     CONTINUE
 400  CONTINUE

      IF (.NOT. FOUND) THEN
C        First Time File is Identified - OPEN File
         OPEN(IPPUNT(INDGRP),ERR=99,FILE=ANNPLT(INDGRP),
     &        IOSTAT=IOERRN,STATUS='UNKNOWN')
      END IF

C     Set Logical Switch Indicating That Plot File(s) Are Generated
      ANPLOT = .TRUE.

      GO TO 999

C     WRITE Error Message for Error Opening File
 99   WRITE(DUMMY,'(5HPLTFL,I3.3)') IPPUNT(INDGRP)
      CALL ERRHDL(PATH,MODNAM,'E','500',DUMMY)

 999  RETURN
      END

      SUBROUTINE OUTOXX
C***********************************************************************
C                 OUTOXX Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: To Process TOXXFILE Output Selections
C
C        PROGRAMMER: Roger Brode
C
C        DATE:    September 29, 1992
C
C        INPUTS:  Input Runstream Parameters
C
C        OUTPUTS: Output Option Switches
C
C        CALLED FROM:   OUCARD
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      CHARACTER BUF12*12
      LOGICAL FOUND

C     Variable Initializations
      MODNAM = 'OUTOXX'
      BUF12  = '            '
      IDUM = 0
      RDUM = 0.
      NIDUM = 3
      NRDUM = 9

C     Check If Enough Fields
      IF (IFC .EQ. 2) THEN
C        Error Message: No Fields
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      ELSE IF (IFC .LT. 5) THEN
C        Error Message: Not Enough Fields
         CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
         GO TO 999
      ELSE IF (IFC .GT. 6) THEN
C        Error Message: Too Many Fields
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
         GO TO 999
      END IF

C     Retrieve Averaging Period
      IF (FIELD(3) .EQ. 'MONTH' .AND. MONTH) THEN
C        Set Value of IPRDT = 720 for MONTHly Averages
         IPRDT = 720
      ELSE
         CALL STONUM(FIELD(3),40,FNUM,IMIT)
         IF (IMIT .NE. 1) THEN
C           Write Error Message:Invalid Numerical Field
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            GO TO 999
         END IF
         IPRDT = INT(FNUM)
      END IF

C     Check Averaging Period Against KAVE Array
      FOUND = .FALSE.
      J = 1
      DO WHILE (.NOT.FOUND .AND. J.LE.NUMAVE)
         IF (IPRDT .EQ. KAVE(J)) THEN
            FOUND = .TRUE.
            INDAVE = J
         END IF
         J = J + 1
      END DO
      IF (.NOT. FOUND) THEN
C        Error Message:E203 AVEPER Not Match With Pre-Defined One
         CALL ERRHDL(PATH,MODNAM,'E','203','AVEPER')
         GO TO 999
      END IF

C     Check for Averaging Period Other Than 1-HOUR, and Write Warning
      IF (IPRDT .NE. 1) THEN
         WRITE(DUMMY,'(2X,I4,2X)') IPRDT
         CALL ERRHDL(PATH,MODNAM,'W','385',DUMMY)
      END IF

C     Set Switch and Check for Previous TOXXFILE Card
C     for This Averaging Period
      ITOXFL(INDAVE) = ITOXFL(INDAVE) + 1
      IF (ITOXFL(INDAVE) .GT. 1) THEN
C        WRITE Error Message
         CALL ERRHDL(PATH,MODNAM,'E','211',KEYWRD)
         GO TO 999
      END IF

C     Retrieve Threshold
      CALL STONUM(FIELD(4),40,FNUM,IMIT)
C     Check for Valid Threshold Value
      IF (IMIT .NE. 1) THEN
C        Write Error Message:Invalid Numerical Field
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         GO TO 999
      END IF
      TOXTHR(INDAVE) = FNUM

      IF ((LOCE(5)-LOCB(5)) .LE. 39) THEN
C        Retrieve Filename as Character Substring to Maintain Original Case
C        Also Check for Filename Larger Than 40 Characters
         TOXFIL(INDAVE) = RUNST1(LOCB(5):LOCE(5))
      ELSE
C        WRITE Error Message:  TOXFIL Field is Too Long
         CALL ERRHDL(PATH,MODNAM,'E','203',' FILNAM ')
      END IF

C     Retrieve File Unit If Input, or Assign File Unit and OPEN File
      IF (IFC .EQ. 6) THEN
         CALL STONUM(FIELD(6),40,FNUM,IMIT)
C        Check for Valid Threshold Value
         IF (IMIT .NE. 1) THEN
C           Write Error Message:Invalid Numerical Field
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            GO TO 999
         END IF
C        Check for Conflict With System Files
         IF (FNUM .LT. 20.) THEN
C           WRITE Error Message:  Invalid File Unit Specified
            CALL ERRHDL(PATH,MODNAM,'E','560',KEYWRD)
            GO TO 999
         ELSE IF (FNUM .GT. 100) THEN
C           WRITE Warning Message:  Suspect File Unit Specified
C           Unit May Conflict With Dynamically Allocated File Units
            CALL ERRHDL(PATH,MODNAM,'W','565',KEYWRD)
            ITXUNT(INDAVE) = INT(FNUM)
         ELSE
            ITXUNT(INDAVE) = INT(FNUM)
         END IF
      ELSE
C        Dynamically Allocate File Unit (300's)
         ITXUNT(INDAVE) = 300 + INDAVE
         IF (INDAVE .GE. 5) THEN
C           WRITE Warning Message: Dynamic FUnit Allocation May Have Conflict
            CALL ERRHDL(PATH,MODNAM,'W','565',KEYWRD)
         END IF
      END IF

C     Check for Earlier Use of This Filename and File Unit
      FOUND = .FALSE.
      DO 100 I = 1, NUMAVE
         IF (I .NE. INDAVE) THEN
            IF (TOXFIL(INDAVE) .EQ. TOXFIL(I) .AND.
     &          ITXUNT(INDAVE) .EQ. ITXUNT(I)) THEN
               FOUND = .TRUE.
            ELSE IF (TOXFIL(INDAVE) .EQ. TOXFIL(I) .AND.
     &               ITXUNT(INDAVE) .NE. ITXUNT(I)) THEN
C              Write Error Message: Conflicting Inputs
               CALL ERRHDL(PATH,MODNAM,'E','550',KEYWRD)
               GO TO 999
            ELSE IF (TOXFIL(INDAVE) .NE. TOXFIL(I) .AND.
     &               ITXUNT(INDAVE) .EQ. ITXUNT(I)) THEN
C              Write Error Message: Conflicting Inputs
               CALL ERRHDL(PATH,MODNAM,'E','550',KEYWRD)
               GO TO 999
            END IF
         END IF
 100  CONTINUE

      IF (.NOT. FOUND) THEN
C        First Time File is Identified - OPEN File
         OPEN(ITXUNT(INDAVE),ERR=99,FILE=TOXFIL(INDAVE),
     &        FORM='UNFORMATTED',IOSTAT=IOERRN,STATUS='UNKNOWN')
      END IF

C     Write Header to File
      IF (RUN) THEN
         NUMPER = NHOURS/IPRDT
C        Write Header Records (BUF12 is used to fill out 80-character title)
         WRITE(ITXUNT(INDAVE)) TITLE1, BUF12
         WRITE(ITXUNT(INDAVE)) ISYEAR, NUMGRP, NUMREC, NUMPER, ITAB,
     &                         NXTOX, NYTOX, (IDUM,I=1,NIDUM)
         WRITE(ITXUNT(INDAVE)) TOXTHR(INDAVE), (RDUM,I=1,NRDUM)
      END IF

      GO TO 999

C     WRITE Error Message for Error Opening File
 99   WRITE(DUMMY,'(5HTOXFL,I3.3)') ITXUNT(INDAVE)
      CALL ERRHDL(PATH,MODNAM,'E','500',DUMMY)

      GO TO 999

C     WRITE Error Message for Error Reading File
 919  CALL ERRHDL(PATH,MODNAM,'E','510',DUMMY)

C     Set Logical Switch Indicating That Maximum Value File(s) Are Generated
 999  TXFILE = .TRUE.

      RETURN
      END
