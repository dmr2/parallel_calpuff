      SUBROUTINE MECARD
C***********************************************************************
C                 MECARD Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: To process MEteorology Pathway Card Images
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        MODIFIED:  To Include TOXXFILE Option - 9/29/92
C
C        INPUTS:  Pathway (ME) and Keyword
C
C        OUTPUTS: Meteorology Option Switches
C                 Meteorology Setup Status Switches
C
C        CALLED FROM:   SETUP
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'

C     Variable Initializations
      MODNAM = 'MECARD'

      IF (KEYWRD .EQ. 'STARTING') THEN
C        Set Status Switch
         IMSTAT(1) = IMSTAT(1) + 1
         IF (IMSTAT(1) .NE. 1) THEN
C           WRITE Error Message: Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         END IF
      ELSE IF (KEYWRD .EQ. 'INPUTFIL') THEN
C        Set Status Switch
         IMSTAT(2) = IMSTAT(2) + 1
         IF (IMSTAT(2) .NE. 1) THEN
C           WRITE Error Message: Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE
C           Process Meteorology File Information            ---   CALL METFIL
            CALL METFIL
         END IF
      ELSE IF (KEYWRD .EQ. 'ANEMHGHT') THEN
C        Set Status Switch
         IMSTAT(3) = IMSTAT(3) + 1
         IF (IMSTAT(3) .NE. 1) THEN
C           WRITE Error Message: Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE
C           Process Anemometer Height Information           ---   CALL ANEMHT
            CALL ANEMHT
         END IF
      ELSE IF (KEYWRD .EQ. 'SURFDATA') THEN
C        Set Status Switch
         IMSTAT(4) = IMSTAT(4) + 1
         IF (IMSTAT(4) .NE. 1) THEN
C           WRITE Error Message: Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE
C           Process Surface Data Information                ---   CALL SFDATA
            CALL SFDATA
         END IF
      ELSE IF (KEYWRD .EQ. 'UAIRDATA') THEN
C        Set Status Switch
         IMSTAT(5) = IMSTAT(5) + 1
         IF (IMSTAT(5) .NE. 1) THEN
C           WRITE Error Message: Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE
C           Process Upper Air Data Information              ---   CALL UADATA
            CALL UADATA
         END IF
      ELSE IF (KEYWRD .EQ. 'STARTEND') THEN
cvrt set flags for conv2cal
         idatetyp = 1
C        Set Status Switch
         IMSTAT(6) = IMSTAT(6) + 1
         IF (IMSTAT(6) .NE. 1) THEN
C           WRITE Error Message: Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE
C           Process Start and End Dates for Reading         ---   CALL STAEND
            CALL STAEND
         END IF
      ELSE IF (KEYWRD .EQ. 'DAYRANGE') THEN
cvrt set flags for conv2cal
         idatetyp = 2
C        Set Status Switch
         IMSTAT(7) = IMSTAT(7) + 1
C        Check for First Occurrence of DAYRANGE Card, and
C        Reinitialize IPROC Array
         IF (IMSTAT(7) .EQ. 1) THEN
            DO 10 I = 1, 366
               IPROC(I) = 0
 10         CONTINUE
         END IF
C        Process Days and Day Ranges for Processing         ---   CALL DAYRNG
         CALL DAYRNG
      ELSE IF (KEYWRD .EQ. 'WDROTATE') THEN
cvrt
             Write (*,1000) 
1000  format (' WARNING:  "WDROTATE" Keyword not compatible with ',
     &      'CALPUFF')

C        Set Status Switch
         IMSTAT(8) = IMSTAT(8) + 1
         IF (IMSTAT(8) .NE. 1) THEN
C           WRITE Error Message: Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE
C           Process Wind Direction Correction Option        ---   CALL WDROTA
            CALL WDROTA
         END IF
      ELSE IF (KEYWRD .EQ. 'WINDPROF') THEN
C        Set Status Switch
         IMSTAT(9) = IMSTAT(9) + 1
         IF (DFAULT) THEN
C           WRITE Warning Message and Ignore Inputs
            CALL ERRHDL(PATH,MODNAM,'W','206',KEYWRD)
         ELSE
C           Process Wind Speed Profile Exponents            ---   CALL WSPROF
            CALL WSPROF
C           Set Logical Flag Indicating User-specified P-Exponents
            USERP = .TRUE.
         END IF
      ELSE IF (KEYWRD .EQ. 'DTHETADZ') THEN
C        Set Status Switch
         IMSTAT(10) = IMSTAT(10) + 1
         IF (DFAULT) THEN
C           WRITE Warning Message and Ignore Inputs
            CALL ERRHDL(PATH,MODNAM,'W','206',KEYWRD)
         ELSE
C           Process Vertical Pot. Temperature Gradients     ---   CALL DTHETA
            CALL DTHETA
C           Set Logical Flag Indicating User-specified DThetaDZ
            USERDT = .TRUE.
         END IF
      ELSE IF (KEYWRD .EQ. 'WINDCATS') THEN
C        Set Status Switch
         IMSTAT(11) = IMSTAT(11) + 1
         IF (IMSTAT(11) .NE. 1) THEN
C           WRITE Error Message: Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE
C           Process Wind Speed Categories                   ---   CALL WSCATS
            CALL WSCATS
         END IF
      ELSE IF (KEYWRD .EQ. 'FINISHED') THEN
C        Set Status Switch
         IMSTAT(20) = IMSTAT(20) + 1
         IF (IMSTAT(20) .NE. 1) THEN
C           WRITE Error Message: Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
            GO TO 999
         END IF
C        Write Error Messages for Missing Mandatory Keyword(s)
         IF (IMSTAT(1) .EQ. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','130','STARTING')
         END IF
         IF (IMSTAT(2) .EQ. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','130','INPUTFIL')
         END IF
         IF (IMSTAT(3) .EQ. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','130','ANEMHGHT')
         END IF
         IF (IMSTAT(4) .EQ. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','130','SURFDATA')
         END IF
         IF (IMSTAT(5) .EQ. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','130','UAIRDATA')
         END IF

C        OPEN Met Data File                                 ---   CALL MEOPEN
         IF (IMSTAT(2) .NE. 0) THEN
            CALL MEOPEN
         END IF

         IF (MULTYR) THEN
C           Set the Increment for Saving Results, INCRST, Based on
C           ISYEAR, Surface Data Year, from SURFDATA Keyword
            IF ((MOD(ISYEAR,4) .NE. 0) .OR.
     &          (MOD(ISYEAR,100).EQ.0 .AND. MOD(ISYEAR,400).NE.0)) THEN
C              Not a Leap Year
               INCRST = 365
            ELSE
C              Leap Year
               INCRST = 366
            END IF
         END IF

C        Determine Number of Hours to be Processed, NHOURS, For Use
C        With the TOXXFILE Option - 9/29/92
         IF ((MOD(ISYEAR,4) .NE. 0) .OR.
     &       (MOD(ISYEAR,100).EQ.0 .AND. MOD(ISYEAR,400).NE.0)) THEN
C           Not a Leap Year
            ND = 365
         ELSE
C           Leap Year
            ND = 366
         END IF
         NDYS = 0
         DO 100 I = 1, ND
            IF (IPROC(I) .EQ. 1) THEN
               NDYS = NDYS + 1
            END IF
 100     CONTINUE
         NHOURS = NDYS * 24

      ELSE
C        Write Error Message: Invalid Keyword for This Pathway
         CALL ERRHDL(PATH,MODNAM,'E','110',KEYWRD)
      END IF

 999  RETURN
      END

      SUBROUTINE METFIL
C***********************************************************************
C                 METFIL Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Process Meteorology Input File Options
C                 From Runstream Input Image
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Meteorological Data Filename and Format
C
C        ERROR HANDLING:   Checks for No Parameters;
C                          Checks for No Format (uses default);
C                          Checks for Too Many Parameters
C
C        CALLED FROM:   MECARD
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'

C     Variable Initializations
      MODNAM = 'METFIL'

      IF (IFC .EQ. 3) THEN
C        Retrieve Met Data Filename as Character Substring to Maintain Case
         METINP = RUNST1(LOCB(3):LOCE(3))
C        Use Default Met Data Format: Initialized in SUBROUTINE VARINI to
CC        METFRM = '(4I2,2F9.4,F6.1,I2,2F7.1)'
      ELSE IF (IFC .EQ. 4) THEN
C        Retrieve Met Data Filename as Character Substring to Maintain Case
         METINP = RUNST1(LOCB(3):LOCE(3))
C        Check for Format String > 60 (Limit for METFRM Variable)
         IF ((LOCE(4)-LOCB(4)) .LE. 59) THEN
            IF ((LOCE(4)-LOCB(4)) .GT. 39) THEN
C              Retrieve Met Format as Char. Substring to Bypass Field Limit
               METFRM = RUNST1(LOCB(4):LOCE(4))
            ELSE
C              Retrieve Met Format From FIELD(4)
               METFRM = FIELD(4)
            END IF
         ELSE
C           WRITE Error Message:  METFRM Field is Too Long
            CALL ERRHDL(PATH,MODNAM,'E','203',' METFRM ')
         END IF
C        Check for Use of CARD Format With DFAULT Option
         IF (DFAULT .AND. METFRM .EQ. 'CARD') THEN
C           WRITE Error Message:  DFAULT With Non-DEFAULT Option (Due to
C           Hourly WINDPROF and DTHETADZ on CARD Format
            CALL ERRHDL(PATH,MODNAM,'E','206','CARD-MET')
         END IF
C        Check for Use of UNFORM format with Dry or Wet Deposition
         IF ((LWPART.OR.LDPART.OR.LWGAS) .AND. METFRM.EQ.'UNFORM') THEN
C           WRITE Error Message:  Can't use UNFORM with Dry or Wet Deposition
            CALL ERRHDL(PATH,MODNAM,'E','143',' ')
         END IF
      ELSE IF (IFC .GT. 4) THEN
C        WRITE Error Message           ! Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
      ELSE
C        WRITE Warning Message         ! No Parameters Specified
         CALL ERRHDL(PATH,MODNAM,'W','200',KEYWRD)
      END IF

      RETURN
      END

      SUBROUTINE ANEMHT
C***********************************************************************
C                 ANEMHT Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Process Anemometer Height Options
C                 From Runstream Input Image
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Anemometer Height, ZREF (m)
C
C        ERROR HANDLING:   Checks for No Parameters;
C                          Checks for No Units (uses default of m);
C                          Checks for Invalid or Suspicious Values of ZREF;
C                          Checks for Too Many Parameters
C
C        CALLED FROM:   MECARD
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'

C     Variable Initializations
      MODNAM = 'ANEMHT'

      IF (IFC .EQ. 3 .OR. IFC .EQ. 4) THEN
         CALL STONUM(FIELD(3),40,ANHT,IDUM)
C        Check The Numerical Field
         IF (IDUM.EQ.-1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            GO TO 999
         END IF
         IF (IFC .EQ. 4 .AND. FIELD(4) .EQ. 'FEET') THEN
            ANHT = 0.3048 * ANHT
         ELSE IF (IFC .EQ. 4 .AND. FIELD(4) .NE. 'METERS') THEN
C           WRITE Warning Message - Invalid ZRUNIT Parameter
            CALL ERRHDL(PATH,MODNAM,'W','203','ZRUNIT')
         END IF
         IF (ANHT .GT. 100.0 .AND. IDUM .EQ. 1) THEN
C           WRITE Warning Message - Possible Error In ANHT
            WRITE(DUMMY,'(F8.3)') ANHT
            CALL ERRHDL(PATH,MODNAM,'W','340',DUMMY)
            ZREF = ANHT
         ELSE IF (ANHT .GE. 2.0 .AND. IDUM .EQ. 1) THEN
            ZREF = ANHT
         ELSE IF (ANHT .GT. 0.0 .AND. IDUM .EQ. 1) THEN
C           WRITE Warning Message - Possible Error In ANHT
            CALL ERRHDL(PATH,MODNAM,'W','340',KEYWRD)
            ZREF = ANHT
         ELSE IF (ANHT .LE. 0.0 .AND. IDUM .EQ. 1) THEN
C           WRITE Error Message - Invalid Anemometer Height
            CALL ERRHDL(PATH,MODNAM,'E','203','Anem Hgt')
         ELSE IF (IDUM .NE. 1) THEN
C           WRITE Error Message - Invalid Numeric Field
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         END IF
      ELSE IF (IFC .GT. 4) THEN
C        WRITE Error Message           ! Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
      ELSE
C        WRITE Error Message           ! No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
      END IF

 999  RETURN
      END

      SUBROUTINE SFDATA
C***********************************************************************
C                 SFDATA Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Process Meteorology Surface Data Station Options
C                 From Runstream Input Image
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Meteorological Surface Data Station Identification
C
C        ERROR HANDLING:   Checks for Too Few Parameters;
C                          Checks for Invalid Numeric Fields;
C                          Checks for Too Many Parameters
C
C        CALLED FROM:   MECARD
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'

C     Variable Initializations
      MODNAM = 'SFDATA'

      IF (IFC .EQ. 2) THEN
C        WRITE Error Message           ! No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      ELSE IF (IFC .LT. 4) THEN
C        WRITE Error Message           ! Not Enough Parameters
         CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
         GO TO 999
      ELSE IF (IFC .GT. 7) THEN
C        WRITE Error Message           ! Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
         GO TO 999
      END IF

      CALL STONUM(FIELD(3),40,FNUM,IDUM)
C     Check The Numerical Field
      IF (IDUM .NE. 1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         GO TO 199
      END IF
      IDSURF = INT(FNUM)

 199  CALL STONUM(FIELD(4),40,FNUM,IDUM)
C     Check The Numerical Field
      IF (IDUM .NE. 1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         GO TO 299
      END IF
      ISYEAR = INT(FNUM)

 299  IF (IFC .GE. 5) THEN
C        Retrieve Surface Data Station Name (Optional)
         SFNAME = FIELD(5)
      ELSE
         SFNAME = 'UNKNOWN'
      END IF

      IF (IFC .EQ. 7) THEN
C        Retrieve Coordinates for Surface Data Location (Optional)
         CALL STONUM(FIELD(6),40,SFX,IDUM)
         IF (IDUM .NE. 1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         END IF
         CALL STONUM(FIELD(7),40,SFY,IDUM)
         IF (IDUM .NE. 1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         END IF
      END IF

 999  RETURN
      END

      SUBROUTINE UADATA
C***********************************************************************
C                 UADATA Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Process Meteorology Upper Air Data Station Options
C                 From Runstream Input Image
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Meteorological Upper Air Data Station Identification
C
C        ERROR HANDLING:   Checks for Too Few Parameters;
C                          Checks for Invalid Numeric Fields;
C                          Checks for Too Many Parameters
C
C        CALLED FROM:   MECARD
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'

C     Variable Initializations
      MODNAM = 'UADATA'

      IF (IFC .EQ. 2) THEN
C        WRITE Error Message           ! No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      ELSE IF (IFC .LT. 4) THEN
C        WRITE Error Message           ! Not Enough Parameters
         CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
         GO TO 999
      ELSE IF (IFC .GT. 7) THEN
C        WRITE Error Message           ! Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
         GO TO 999
      END IF

      CALL STONUM(FIELD(3),40,FNUM,IDUM)
C     Check The Numerical Field
      IF (IDUM .NE. 1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         GO TO 199
      END IF
      IDUAIR = INT(FNUM)

 199  CALL STONUM(FIELD(4),40,FNUM,IDUM)
C     Check The Numerical Field
      IF (IDUM .NE. 1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         GO TO 299
      END IF
      IUYEAR = INT(FNUM)

 299  IF (IFC .GE. 5) THEN
C        Retrieve Surface Data Station Name (Optional)
         UANAME = FIELD(5)
      ELSE
         UANAME = 'UNKNOWN'
      END IF

      IF (IFC .EQ. 7) THEN
C        Retrieve Coordinates for Surface Data Location (Optional)
         CALL STONUM(FIELD(6),40,UAX,IDUM)
         IF (IDUM .NE. 1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         END IF
         CALL STONUM(FIELD(7),40,UAY,IDUM)
         IF (IDUM .NE. 1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         END IF
      END IF

 999  RETURN
      END

      SUBROUTINE STAEND
C***********************************************************************
C                 STAEND Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Process Start and End Dates for Meteorology File
C                 From Runstream Input Image
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Start and End Dates to Read from Meteorological File
C
C        ERROR HANDLING:   Checks for Too Few Parameters;
C                          Checks for Invalid Numeric Fields;
C                          Checks for Too Many Parameters
C
C        CALLED FROM:   MECARD
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'

C     Variable Initializations
      MODNAM = 'STAEND'

      IF (IFC .EQ. 8) THEN
C        Process for YR, MD, DY
         CALL STONUM(FIELD(3),40,FNUM,IDUM1)
C        Check The Numerical Field
         IF (IDUM1 .EQ. -1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            GO TO 198
         END IF
         ISYR = INT(FNUM)
 198     CALL STONUM(FIELD(4),40,FNUM,IDUM2)
C        Check The Numerical Field
         IF (IDUM2 .EQ. -1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            GO TO 298
         END IF
         ISMN = INT(FNUM)
 298     CALL STONUM(FIELD(5),40,FNUM,IDUM3)
C        Check The Numerical Field
         IF (IDUM3 .EQ. -1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            GO TO 398
         END IF
         ISDY = INT(FNUM)
 398     CALL STONUM(FIELD(6),40,FNUM,IDUM4)
C        Check The Numerical Field
         IF (IDUM4 .EQ. -1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            GO TO 498
         END IF
         IEYR = INT(FNUM)
 498     CALL STONUM(FIELD(7),40,FNUM,IDUM5)
C        Check The Numerical Field
         IF (IDUM5 .EQ. -1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            GO TO 598
         END IF
         IEMN = INT(FNUM)
 598     CALL STONUM(FIELD(8),40,FNUM,IDUM6)
C        Check The Numerical Field
         IF (IDUM6 .EQ. -1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            GO TO 698
         END IF
         IEDY = INT(FNUM)
 698     IF (IDUM1 .NE. 1 .OR. IDUM2 .NE. 1 .OR. IDUM3 .NE. 1 .OR.
     &       IDUM4 .NE. 1 .OR. IDUM5 .NE. 1 .OR. IDUM6 .NE. 1) THEN
C           WRITE Error Message    ! Invalid Numeric Parameter
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         ELSE
C           Convert ISYR and IEYR to Four Digits
            IF (ISYR .GE. 50 .AND. ISYR .LE. 99) THEN
               ISYR = 1900 + ISYR
            ELSE IF (ISYR .LT. 50) THEN
               ISYR = 2000 + ISYR
            END IF
            IF (IEYR .GE. 50 .AND. IEYR .LE. 99) THEN
               IEYR = 1900 + IEYR
            ELSE IF (IEYR .LT. 50) THEN
               IEYR = 2000 + IEYR
            END IF
C           Calculate JULIAN Day for Start and End Dates
            CALL JULIAN (ISYR,ISMN,ISDY,ISJDAY)
            CALL JULIAN (IEYR,IEMN,IEDY,IEJDAY)
C           Convert Years Back to Two Digits
            ISYR = ISYR - 100*INT(ISYR/100)
            IEYR = IEYR - 100*INT(IEYR/100)
C           Use 0 for Start Hour and 24 for End Hour
            ISHR = 0
            IEHR = 24
            ISDATE = ISYR*1000000 + ISMN*10000 + ISDY*100 + ISHR
            IEDATE = IEYR*1000000 + IEMN*10000 + IEDY*100 + IEHR
         END IF
      ELSE IF (IFC .EQ. 10) THEN
C        Process for YR, MD, DY, HR
         CALL STONUM(FIELD(3),40,FNUM,IDUM1)
C        Check The Numerical Field
         IF (IDUM1 .EQ. -1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            GO TO 199
         END IF
         ISYR = INT(FNUM)
 199     CALL STONUM(FIELD(4),40,FNUM,IDUM2)
C        Check The Numerical Field
         IF (IDUM2 .EQ. -1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            GO TO 299
         END IF
         ISMN = INT(FNUM)
 299     CALL STONUM(FIELD(5),40,FNUM,IDUM3)
C        Check The Numerical Field
         IF (IDUM3 .EQ. -1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            GO TO 399
         END IF
         ISDY = INT(FNUM)
 399     CALL STONUM(FIELD(6),40,FNUM,IDUM4)
C        Check The Numerical Field
         IF (IDUM4 .EQ. -1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            GO TO 499
         END IF
         ISHR = INT(FNUM)
 499     CALL STONUM(FIELD(7),40,FNUM,IDUM5)
C        Check The Numerical Field
         IF (IDUM5 .EQ. -1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            GO TO 599
         END IF
         IEYR = INT(FNUM)
 599     CALL STONUM(FIELD(8),40,FNUM,IDUM6)
C        Check The Numerical Field
         IF (IDUM6 .EQ. -1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            GO TO 699
         END IF
         IEMN = INT(FNUM)
 699     CALL STONUM(FIELD(9),40,FNUM,IDUM7)
C        Check The Numerical Field
         IF (IDUM7 .EQ. -1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            GO TO 799
         END IF
         IEDY = INT(FNUM)
 799     CALL STONUM(FIELD(10),40,FNUM,IDUM8)
C        Check The Numerical Field
         IF (IDUM8 .EQ. -1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            GO TO 899
         END IF
         IEHR = INT(FNUM)
 899     IF (IDUM1 .NE. 1 .OR. IDUM2 .NE. 1 .OR. IDUM3 .NE. 1 .OR.
     &       IDUM4 .NE. 1 .OR. IDUM5 .NE. 1 .OR. IDUM6 .NE. 1 .OR.
     &       IDUM7 .NE. 1 .OR. IDUM8 .NE. 1) THEN
C           WRITE Error Message    ! Invalid Numeric Parameter
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         ELSE
C           Convert ISYR and IEYR to Four Digits
            IF (ISYR .GE. 50 .AND. ISYR .LE. 99) THEN
               ISYR = 1900 + ISYR
            ELSE IF (ISYR .LT. 50) THEN
               ISYR = 2000 + ISYR
            END IF
            IF (IEYR .GE. 50 .AND. IEYR .LE. 99) THEN
               IEYR = 1900 + IEYR
            ELSE IF (IEYR .LT. 50) THEN
               IEYR = 2000 + IEYR
            END IF
C           Calculate JULIAN Day for Start and End Dates
            CALL JULIAN (ISYR,ISMN,ISDY,ISJDAY)
            CALL JULIAN (IEYR,IEMN,IEDY,IEJDAY)
C           Convert Years Back to Two Digits
            ISYR = ISYR - 100*INT(ISYR/100)
            IEYR = IEYR - 100*INT(IEYR/100)
            ISDATE = ISYR*1000000 + ISMN*10000 + ISDY*100 + ISHR
            IEDATE = IEYR*1000000 + IEMN*10000 + IEDY*100 + IEHR
            IF (ISHR .NE. 0) THEN
C              Adjust Start Hour to One Hour Earlier
               ISDATE = ISDATE - 1
            END IF
         END IF
      ELSE IF (IFC .GT. 8) THEN
C        WRITE Error Message           ! Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
      ELSE
C        WRITE Error Message           ! Not Enough Parameters
         CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
      END IF

 999  RETURN
      END

      SUBROUTINE DAYRNG
C***********************************************************************
C                 DAYRNG Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Process the Selection of Days and Ranges of Days
C                 for Processing from the Meteorology File
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Array of Dates to Process from Meteorological File
C
C        ERROR HANDLING:   Checks for Too Few Parameters;
C                          Checks for Invalid Numeric Fields;
C                          Checks for Improper Combinations of Fields;
C                          Checks for Dates Out of Range
C
C        CALLED FROM:   MECARD
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      CHARACTER BEGRNG*8, ENDRNG*8, CMN1*8, CDY1*8, CMN2*8, CDY2*8
      CHARACTER BLNK08*8
      LOGICAL WRONG, RMARK, GMARK

C     Variable Initializations
      MODNAM = 'DAYRNG'
      DATA BLNK08/'        '/

      IF (IFC .LT. 3) THEN
C        WRITE Error Message           ! No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
      ELSE
         DO 40 I = 3, IFC
C           First Check For Range Marker (-) And Gregorian Day Marker (/)
C           Initialize Character Fields
            BEGRNG = BLNK08
            ENDRNG = BLNK08
            CMN1 = BLNK08
            CDY1 = BLNK08
            CMN2 = BLNK08
            CDY2 = BLNK08
            CALL FSPLIT(PATH,KEYWRD,FIELD(I),40,'-',RMARK,BEGRNG,ENDRNG)
            CALL FSPLIT(PATH,KEYWRD,BEGRNG,8,'/',GMARK,CMN1,CDY1)
            IF (RMARK .AND. GMARK) THEN
               CALL FSPLIT(PATH,KEYWRD,ENDRNG,8,'/',GMARK,CMN2,CDY2)
            END IF

            IF (.NOT.RMARK .AND. .NOT.GMARK) THEN
C              Field Must Be a Single Julian Day
cvrt set flags for conv2cal
               irangetyp = 1
               CALL STONUM(BEGRNG,8,ZDAY,IDUM)
C              Check The Numerical Field
               IF (IDUM .EQ. -1) THEN
                  CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
                  GO TO 40
               END IF
               JDAY = INT(ZDAY)
               IF (JDAY.GE.1 .AND. JDAY.LE.366 .AND. IDUM.EQ.1) THEN
                  IPROC(JDAY) = 1
               ELSE
C                 WRITE Error Message    ! Invalid Julian Day
                  CALL ERRHDL(PATH,MODNAM,'E','203','Juli Day')
               END IF
               IF (JDAY.LT.ISJDAY .OR. JDAY.GT.IEJDAY) THEN
C                 WRITE Warning Message  ! Julian Day Out-of-Range
                  WRITE(DUMMY,'(I8)') JDAY
                  CALL ERRHDL(PATH,MODNAM,'W','350',DUMMY)
               END IF

            ELSE IF (RMARK .AND. .NOT.GMARK) THEN
C              Field Must Be a Julian Day Range - Extract Beg & End
cvrt set flags for conv2cal
               irangetyp = 2
               CALL STONUM(BEGRNG,8,BZDAY,IDUM)
C              Check The Numerical Field
               IF (IDUM .EQ. -1) THEN
                  CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
                  WRONG = .TRUE.
               END IF
               CALL STONUM(ENDRNG,8,EZDAY,IDUM)
C              Check The Numerical Field
               IF (IDUM .EQ. -1) THEN
                  CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
                  WRONG = .TRUE.
               END IF
               IF (WRONG) GO TO 40
               JDAYB = INT(BZDAY)
               JDAYE = INT(EZDAY)
               IF ((JDAYB .LE. JDAYE) .AND. (JDAYB .GE. 1) .AND.
     &             (JDAYE .LE. 366)) THEN
                  DO 20 K = JDAYB, JDAYE
                     IPROC(K) = 1
 20               CONTINUE
               ELSE
C                 WRITE Error Message    ! Invalid Julian Day Range
                  CALL ERRHDL(PATH,MODNAM,'E','203','Juli Day')
               END IF
               IF (JDAYB.LT.ISJDAY .OR. JDAYE.GT.IEJDAY) THEN
C                 WRITE Warning Message  ! Julian Day Out-of-Range
                  WRITE(DUMMY,'(I3,1H-,I3)') JDAYB, JDAYE
                  CALL ERRHDL(PATH,MODNAM,'W','350',DUMMY)
               END IF

            ELSE IF (.NOT.RMARK .AND. GMARK) THEN
C               Field Must Be a Single Month/Day
cvrt set flags for conv2cal
               irangetyp = 3
               CALL STONUM(CMN1,8,ZMON,IDUM)
C              Check The Numerical Field
               IF (IDUM .EQ. -1) THEN
                  CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
                  WRONG = .TRUE.
               END IF
               CALL STONUM(CDY1,8,ZDAY,IDUM)
C              Check The Numerical Field
               IF (IDUM .EQ. -1) THEN
                  CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
                  WRONG = .TRUE.
               END IF
               IF (WRONG) GO TO 40
               IMN = INT(ZMON)
               IDY = INT(ZDAY)
cvrt
               im = imn
               id = idy
cvrt
               CALL JULIAN(ISYEAR,IMN,IDY,JDAY)
               IF (JDAY .GE. 1 .AND. JDAY .LE. 366) THEN
                  IPROC(JDAY) = 1
               ELSE
C                 WRITE Error Message    ! Invalid Julian Day
                  CALL ERRHDL(PATH,MODNAM,'E','203','Juli Day')
               END IF
               IF (JDAY.LT.ISJDAY .OR. JDAY.GT.IEJDAY) THEN
C                 WRITE Warning Message  ! Julian Day Out-of-Range
                  WRITE(DUMMY,'(I8)') JDAY
                  CALL ERRHDL(PATH,MODNAM,'W','350',DUMMY)
               END IF

            ELSE IF (RMARK .AND. GMARK) THEN
C              Field Must Be a Greg. Date Range (MN/DY-MN/DY)
cvrt set flags for conv2cal
               irangetyp = 4
               CALL STONUM(CMN1,8,ZMN1,IDUM)
C              Check The Numerical Field
               IF (IDUM .EQ. -1) THEN
                  CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
                  WRONG = .TRUE.
               END IF
               CALL STONUM(CDY1,8,ZDY1,IDUM)
C              Check The Numerical Field
               IF (IDUM .EQ. -1) THEN
                  CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
                  WRONG = .TRUE.
               END IF
               IF (WRONG) GO TO 41
               IMN1 = INT(ZMN1)
               IDY1 = INT(ZDY1)
 41            CALL STONUM(CMN2,8,ZMN2,IDUM)
C              Check The Numerical Field
               IF (IDUM .EQ. -1) THEN
                  CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
                  WRONG = .TRUE.
               END IF
               CALL STONUM(CDY2,8,ZDY2,IDUM)
C              Check The Numerical Field
               IF (IDUM .EQ. -1) THEN
                  CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
                  WRONG = .TRUE.
               END IF
               IF (WRONG) GO TO 40
               IMN2 = INT(ZMN2)
               IDY2 = INT(ZDY2)
               CALL JULIAN(ISYEAR,IMN1,IDY1,JDAYB)
               CALL JULIAN(ISYEAR,IMN2,IDY2,JDAYE)
               IF ((JDAYB .LE. JDAYE) .AND. (JDAYB .GE. 1) .AND.
     &             (JDAYE .LE. 366)) THEN
                  DO 30 K = JDAYB, JDAYE
                     IPROC(K) = 1
 30               CONTINUE
               ELSE
C                 WRITE Error Message    ! Invalid Julian Day
                  CALL ERRHDL(PATH,MODNAM,'E','203','Juli Day')
               END IF
               IF (JDAYB.LT.ISJDAY .OR. JDAYE.GT.IEJDAY) THEN
C                 WRITE Warning Message  ! Julian Day Out-of-Range
                  WRITE(DUMMY,'(I3,1H-,I3)') JDAYB, JDAYE
                  CALL ERRHDL(PATH,MODNAM,'W','350',DUMMY)
               END IF

            ELSE
C               WRITE Error Message    ! Invalid Field
                CALL ERRHDL(PATH,MODNAM,'E','203','DAYRANGE')
            END IF

 40      CONTINUE
      END IF

      RETURN
      END

      SUBROUTINE WDROTA
C***********************************************************************
C                 WDROTA Module of ISC2 Short Term Model - ISCST2
C
C     PURPOSE:    PROCESSES INPUT FOR ROTATING WIND DIRECTION DATA
C
C     PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C     INPUTS:     Input Runstream Image Parameters
C
C     OUTPUT:     Wind Direction Rotation Angle
C
C     CALLED FROM:   MECARD
C
C     ERROR HANDLING:   Checks for No Parameters;
C                       Checks for Too Many Parameters;
C                       Checks for Invalid Numeric Field
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'

C     Variable Initializations
      ROTANG = 0.0
      MODNAM = 'WDROTA'

      IF (IFC .EQ. 3) THEN
         CALL STONUM(FIELD(3),40,ROTANG,IDUM)
         IF (IDUM .NE. 1) THEN
C            WRITE Error Message  ! Invalid Numeric Field Encountered
             CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         ELSE IF (ABS(ROTANG) .GT. 180.0) THEN
C            WRITE Error Message       ! ROTANG Out of Range
             CALL ERRHDL(PATH,MODNAM,'E','380','ROTANG')
         END IF
      ELSE IF (IFC .GT. 3) THEN
C        WRITE Error Message           ! Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
      ELSE
C        WRITE Error Message           ! No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
      END IF

 999  RETURN
      END

      SUBROUTINE WSCATS
C***********************************************************************
C                 WSCATS Module of ISC2 Short Term Model - ISCST2
C
C     PURPOSE:    PROCESSES INPUT FOR WIND SPEED CATEGORIES
C
C     PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C     INPUTS:     Input Runstream Image Parameters
C
C     OUTPUT:     Array of Wind Speed Category Limits (5)
C
C     CALLED FROM:   MECARD
C
C     ERROR HANDLING:   Checks for No Parameters;
C                       Checks for Too Many Parameters;
C                       Checks for Invalid Numeric Fields;
C                       Checks for Wind Speed Category Decreasing
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'

C     Variable Initializations
      MODNAM = 'WSCATS'

      IF (IFC .EQ. 7) THEN
C        Fill UCAT Array
         DO 100 I = 3, IFC
            CALL STONUM(FIELD(I),40,ZNUM,IDUM)
            IF (IDUM .NE. 1) THEN
C              WRITE Error Message  ! Invalid Numeric Field Encountered
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            ELSE IF (ZNUM .LT. 1.0 .OR. ZNUM .GT. 20.0) THEN
C               WRITE Error Message       ! UCAT Out of Range
                CALL ERRHDL(PATH,MODNAM,'E','380','UCAT')
            ELSE
               IWS = I - 2
               UCAT(IWS) = ZNUM
               IF (IWS.GT.1 .AND. UCAT(IWS).LE.UCAT(IWS-1)) THEN
C                 WRITE Error Message    ! Invalid UCAT Value, LE Previous
                  CALL ERRHDL(PATH,MODNAM,'E','203','UCAT')
               END IF
            END IF
 100     CONTINUE
      ELSE IF (IFC .GT. 7) THEN
C        WRITE Error Message           ! Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
      ELSE
C        WRITE Error Message           ! No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
      END IF

      RETURN
      END

      SUBROUTINE WSPROF
C***********************************************************************
C                 WSPROF Module of ISC2 Short Term Model - ISCST2
C
C     PURPOSE:    PROCESSES INPUT FOR WIND SPEED PROFILE EXPONENTS
C
C     PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C     INPUTS:     Input Runstream Image Parameters
C
C     OUTPUT:     Array of Wind Speed Profile Exponents for Each Stability
C                 and Wind Speed Category
C
C     CALLED FROM:   MECARD
C
C     ERROR HANDLING:   Checks for No Parameters;
C                       Checks for Too Many Parameters;
C                       Checks for Invalid Numeric Fields;
C                       Checks for Invalid Stability Class Indicator
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'

C     Variable Initializations
      MODNAM = 'WSPROF'

      IF (IFC .GE. 4 .AND. IFC .LE. 9) THEN
         IWS = 0
C        Determine Stability Category Index from FIELD(3); Accepts Either
C        Character Inputs (A-F) or Numeric Inputs (1-6).
         IF (FIELD(3) .EQ. 'A' .OR. FIELD(3) .EQ. '1') THEN
            IST = 1
         ELSE IF (FIELD(3) .EQ. 'B' .OR. FIELD(3) .EQ. '2') THEN
            IST = 2
         ELSE IF (FIELD(3) .EQ. 'C' .OR. FIELD(3) .EQ. '3') THEN
            IST = 3
         ELSE IF (FIELD(3) .EQ. 'D' .OR. FIELD(3) .EQ. '4') THEN
            IST = 4
         ELSE IF (FIELD(3) .EQ. 'E' .OR. FIELD(3) .EQ. '5') THEN
            IST = 5
         ELSE IF (FIELD(3) .EQ. 'F' .OR. FIELD(3) .EQ. '6') THEN
            IST = 6
         ELSE
C           WRITE Error Message           ! Invalid Stability Class Indicator
            CALL ERRHDL(PATH,MODNAM,'E','203','INDKST')
C           Exit to END
            GO TO 999
         END IF

C        Fill PUSER Array
         DO 100 I = 4, IFC
            CALL STONUM(FIELD(I),40,ZNUM,IDUM)
C           Check The Numerical Field
            IF (IDUM .EQ. -1) THEN
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
               GO TO 100
            END IF
            IF (ZNUM .LT. 0.0 .OR. ZNUM .GT. 1.0) THEN
C              WRITE Error Message          ! PUSER Out of Range
               CALL ERRHDL(PATH,MODNAM,'E','380','PUSER')
            ELSE
               DO 90 J = 1,IDUM
                  IWS = IWS + 1
                  IF (IWS .LE. NWSCAT) THEN
                     PUSER(IST,IWS) = ZNUM
                  ELSE
C                    WRITE Error Message    ! Too Many Values
                     CALL ERRHDL(PATH,MODNAM,'E','231',KEYWRD)
                  END IF
 90            CONTINUE
            END IF
 100     CONTINUE

      ELSE IF (IFC .GT. 9) THEN
C        WRITE Error Message           ! Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
      ELSE
C        WRITE Error Message           ! No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
      END IF

 999  CONTINUE

      RETURN
      END

      SUBROUTINE DTHETA
C***********************************************************************
C                 DTHETA Module of ISC2 Short Term Model - ISCST2
C
C     PURPOSE:    PROCESSES INPUT FOR VERTICAL POTENTIAL TEMPERATURE
C                 GRADIENTS
C
C     PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C     INPUTS:     Input Runstream Image Parameters
C
C     OUTPUT:     Array of Vertical Potential Temperature Gradients for
C                 Each Stability and Wind Speed Category
C
C     CALLED FROM:   MECARD
C
C     ERROR HANDLING:   Checks for No Parameters;
C                       Checks for Too Many Parameters;
C                       Checks for Invalid Numeric Fields;
C                       Checks for Invalid Stability Class Indicator
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'

C     Variable Initializations
      MODNAM = 'DTHETA'

      IF (IFC .GE. 4 .AND. IFC .LE. 9) THEN
         IWS = 0

C        Determine Stability Category Index from FIELD(3); Accepts Either
C        Character Inputs (A-F) or Numeric Inputs (1-6).
         IF (FIELD(3) .EQ. 'A' .OR. FIELD(3) .EQ. '1') THEN
            IST = 1
         ELSE IF (FIELD(3) .EQ. 'B' .OR. FIELD(3) .EQ. '2') THEN
            IST = 2
         ELSE IF (FIELD(3) .EQ. 'C' .OR. FIELD(3) .EQ. '3') THEN
            IST = 3
         ELSE IF (FIELD(3) .EQ. 'D' .OR. FIELD(3) .EQ. '4') THEN
            IST = 4
         ELSE IF (FIELD(3) .EQ. 'E' .OR. FIELD(3) .EQ. '5') THEN
            IST = 5
         ELSE IF (FIELD(3) .EQ. 'F' .OR. FIELD(3) .EQ. '6') THEN
            IST = 6
         ELSE
C           WRITE Error Message           ! Invalid Stability Class Indicator
            CALL ERRHDL(PATH,MODNAM,'E','203','INDKST')
C           Exit to END
            GO TO 1999
         END IF

C        Fill DTUSER Array
         DO 100 I = 4, IFC
            CALL STONUM(FIELD(I),40,ZNUM,IDUM)
C           Check The Numerical Field
            IF (IDUM .EQ. -1) THEN
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
               GO TO 100
            END IF
            IF (ZNUM .LT. 0.0 .OR. ZNUM .GT. 0.2) THEN
C               WRITE Error Message       ! DTUSER Out of Range
                CALL ERRHDL(PATH,MODNAM,'E','380','DTUSER')
            ELSE
               DO 90 J = 1,IDUM
                  IWS = IWS + 1
                  IF (IWS .LE. NWSCAT) THEN
                     DTUSER(IST,IWS) = ZNUM
                  ELSE
C                    WRITE Error Message    ! Too Many Values
                     CALL ERRHDL(PATH,MODNAM,'E','231',KEYWRD)
                  END IF
 90            CONTINUE
            END IF
 100     CONTINUE

      ELSE IF (IFC .GT. 9) THEN
C        WRITE Error Message           ! Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
      ELSE
C        WRITE Error Message           ! No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
      END IF
 1999 CONTINUE

      RETURN
      END

      SUBROUTINE MEOPEN
C***********************************************************************
C                 MEOPEN Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Open The Input file for Hourly Meteorological Data,
C                 And Check Header Record
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Meteorology File Specifications
C
C        OUTPUTS: File OPEN Error Status
C
C        CALLED FROM:   SETUP
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'

C     Variable Initializations
      MODNAM = 'MEOPEN'

C     File Unit Initialized in BLOCK DATA INIT
C     File Format Set By Keyword "INPUTFIL" on "ME" pathway
C     OPEN Met Data File --- Either Unformatted or Formatted
C     READ In the Station Numbers and Years for Comparison to SETUP File

      IF (METFRM .EQ. 'UNFORM') THEN
         OPEN(UNIT=MFUNIT,ERR=999,FILE=METINP,FORM='UNFORMATTED',
     &        IOSTAT=IOERRN,STATUS='OLD')
         READ(MFUNIT,ERR=99,IOSTAT=IOERRN) ISSI, ISYI, IUSI, IUYI
      ELSE
         OPEN(UNIT=MFUNIT,ERR=999,FILE=METINP,FORM='FORMATTED',
     &        IOSTAT=IOERRN,STATUS='OLD')
         READ(MFUNIT,*,ERR=99,IOSTAT=IOERRN) ISSI, ISYI, IUSI, IUYI
      END IF

C     Convert 2-Digit Year to 4-Digit Value
      IF (ISYI .LE. 50) THEN
         ISYI = 2000 + ISYI
      ELSE IF (ISYI .LE. 99) THEN
         ISYI = 1900 + ISYI
      END IF
      IF (IUYI .LE. 50) THEN
         IUYI = 2000 + IUYI
      ELSE IF (IUYI .LE. 99) THEN
         IUYI = 1900 + IUYI
      END IF

C     Check Station IDs and Data Year for Errors
      IF (ISSI .NE. IDSURF) THEN
         WRITE(DUMMY,'(I8)') ISSI
         CALL ERRHDL(PATH,MODNAM,'E','530',DUMMY)
      END IF
      IF (ISYI .NE. ISYEAR) THEN
         WRITE(DUMMY,'(I8)') ISYI
         CALL ERRHDL(PATH,MODNAM,'E','530',DUMMY)
      END IF
      IF (IUSI .NE. IDUAIR) THEN
         WRITE(DUMMY,'(I8)') IUSI
         CALL ERRHDL(PATH,MODNAM,'E','530',DUMMY)
      END IF
      IF (IUYI .NE. IUYEAR) THEN
         WRITE(DUMMY,'(I8)') IUYI
         CALL ERRHDL(PATH,MODNAM,'E','530',DUMMY)
      END IF

      GO TO 1000

C     Write Out Error Message for File READ Error
 99   CALL ERRHDL(PATH,MODNAM,'E','510',' MET-INP')

      GO TO 1000

C     Write Out Error Message for File OPEN Error
 999  CALL ERRHDL(PATH,MODNAM,'E','500',' MET-INP')

c -----------------------------------------------------------
c DGS Close met file now!  (no longer needed for ISC2PUF)
c1000 RETURN
 1000 close(unit=mfunit)
      return
c -----------------------------------------------------------
      END
