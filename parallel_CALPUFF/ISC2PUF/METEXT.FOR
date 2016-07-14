      SUBROUTINE METEXT
C***********************************************************************
C                METEXT Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Controls Extraction and Quality Assurance of
C                 One Hour of Meteorological Data
C
C        PROGRAMMER: ROGER BRODE, JEFF WANG
C        MODIFIED BY D. Strimaitis, SRC (for Wet & Dry DEPOSITION)
C
C        DATE:    November 8, 1993
C
C        MODIFIED BY D. Strimaitis, SRC (for Dry DEPOSITION)
C        (DATE:    February 15, 1993)
C
C        MODIFIED:   To avoid potential math error due to negative
C                    ambient temperatures in calculating the square
C                    root of the stability parameter, RTOFS - 4/19/93
C
C        MODIFIED:
C        7/27/94     J. Paumier, PES, Inc.
C                    The variables for displacement height, ZDM and
C                    AZDM(), were removed from the input to and output
C                    from ISC-COMPDEP.  The following format statements
C                    also were affected: 9009, 9026, 9032, 9033
C
C*       7/27/94     J. Hardikar, PES, Inc.
C*                   Added code to calculate reference wind speed at 10m
C*                   to be used for OPENPIT source algorithms
C
C        INPUTS:  Meteorology File Specifications
C
C        OUTPUTS: Meteorological Variables for One Hour
C
C        CALLED FROM:   HRLOOP
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'
C     Declare Arrays for Use With Unformatted Input Met Files
      INTEGER IKST(24), NDAY(12)
      REAL AUREF(24), ATA(24), AAFV(24), AAFVR(24), AZI(2,24)
      COMMON /UNFMET/ IKST, AUREF, ATA, AAFV, AAFVR, AZI

C     Variable Initializations
      DATA NDAY/31,59,90,120,151,181,212,243,273,304,334,365/
      MODNAM = 'METEXT'
      PATH   = 'MX'

C     Save Value of Last YR/MN/DY/HR and Previous Hour
      IPDATE = KURDAT
      IPHOUR = IHOUR

C     READ Meteorology Data Based on Format --
C     When DRY deposition is modeled, U-star, L, and z0 (surface
C     roughness length) are read in addition to the standard RAMMET
C     data.  These must be provided at the end of each hourly record
C     for the FORMATTED ASCII, CARD, and FREE options.
C
C     When WET deposition is modeled, ipcode (precip.
C     code) and prate (precip. rate in mm/hr) must also be added to
C     each hourly record.
C     The format statement allows for all additional data:

 9009 FORMAT(4I2,2F9.4,F6.1,I2,2F7.1,2F8.4,F9.4,F10.1,F8.4,I4,F7.2)
cjop  FORMAT(4I2,2F9.4,F6.1,I2,2F7.1,2F8.4,F9.4,F10.1,F8.4,F5.1,I4,F7.2)

C     Initialize USTAR, EL, Z0M, IPCODE, AND PRATE to ZERO for hour
      USTAR=0.0
      EL=0.0
      Z0M=0.0
cjop  ZDM=0.0
      IPCODE=0
      PRATE=0.0

      IF (IMONTH .EQ. 12 .AND. IDAY .EQ. 31 .AND. IHOUR .EQ. 24) THEN
C        End of year has been reached - check for presence of header
C        record at beginning of next year for multi-year data files.
         IF (METFRM .NE. 'UNFORM') THEN
C           Multi-year data files applies only to ASCII files.
            READ(MFUNIT,*,END=1000,ERR=998,IOSTAT=IOERRN) JSSI, JSYI,
     &                                                    JUSI, JUYI
            IF (JSSI .NE. IDSURF .AND. JUSI .NE. IDUAIR) THEN
C              Station IDs don't match runstream input, assume that header
C              record is missing.  Backspace met file and continue processing.
               BACKSPACE MFUNIT
            END IF

            GO TO 1001

C           Error reading 'header record' - assume that header record is
C           missing.  Backspace met file and continue processing.
 998        BACKSPACE MFUNIT

         END IF
      END IF

1001  CONTINUE

      IF (LWPART .OR. LWGAS) THEN
C        WET Deposition -- Read Met. for Both Wet & Dry Deposition
         IF (METFRM .EQ. 'FREE') THEN
C           Read Hourly Records from ASCII File Using FREE Format -
C           Yr, Mn, Dy, Hr, AFV, UREF, TA, KST, ZIRUR, ZIURB, USTAR,
C           EL, Z0M, IPCODE, PRATE
            READ(MFUNIT,*,END=1000,ERR=99,IOSTAT=IOERRN) IYEAR, IMONTH,
     &           IDAY, IHOUR, AFV, UREF, TA, KST, ZIRUR, ZIURB, USTAR,
     &           EL, Z0M, IPCODE, PRATE
         ELSE IF (METFRM .EQ. 'CARD') THEN
C           Read Hourly Records from ASCII File Using CARD Format -
C           Yr, Mn, Dy, Hr, AFV, UREF, TA, KST, ZIRUR, ZIURB, P, DTDZ,
C           USTAR, EL, Z0M, IPCODE, PRATE
            READ(MFUNIT,9009,END=1000,ERR=99,IOSTAT=IOERRN) IYEAR,
     &           IMONTH, IDAY, IHOUR, AFV, UREF, TA, KST, ZIRUR, ZIURB,
     &           P, DTDZ, USTAR, EL, Z0M, IPCODE, PRATE
         ELSE
C           Read Hourly Records from Formatted ASCII File Using METFRM
            READ(MFUNIT,METFRM,END=1000,ERR=99,IOSTAT=IOERRN) IYEAR,
     &           IMONTH, IDAY, IHOUR, AFV, UREF, TA, KST, ZIRUR, ZIURB,
     &           USTAR, EL, Z0M, IPCODE, PRATE
         ENDIF
      ELSE IF (LDPART) THEN
C        Just DRY Deposition
         IF (METFRM .EQ. 'FREE') THEN
C           Read Hourly Records from ASCII File Using FREE Format -
C           Yr, Mn, Dy, Hr, AFV, UREF, TA, KST, ZIRUR, ZIURB, USTAR,
C           EL, Z0M
            READ(MFUNIT,*,END=1000,ERR=99,IOSTAT=IOERRN) IYEAR, IMONTH,
     &           IDAY, IHOUR, AFV, UREF, TA, KST, ZIRUR, ZIURB, USTAR,
     &           EL, Z0M
         ELSE IF (METFRM .EQ. 'CARD') THEN
C           Read Hourly Records from ASCII File Using CARD Format -
C           Yr, Mn, Dy, Hr, AFV, UREF, TA, KST, ZIRUR, ZIURB, P, DTDZ,
C           USTAR, EL, Z0M
            READ(MFUNIT,9009,END=1000,ERR=99,IOSTAT=IOERRN) IYEAR,
     &           IMONTH, IDAY, IHOUR, AFV, UREF, TA, KST, ZIRUR, ZIURB,
     &           P, DTDZ, USTAR, EL, Z0M
         ELSE
C           Read Hourly Records from Formatted ASCII File Using METFRM
            READ(MFUNIT,METFRM,END=1000,ERR=99,IOSTAT=IOERRN) IYEAR,
     &           IMONTH, IDAY, IHOUR, AFV, UREF, TA, KST, ZIRUR, ZIURB,
     &           USTAR, EL, Z0M
         ENDIF
      ELSE
C        No Deposition Met Needed
         IF (METFRM .EQ. 'UNFORM') THEN
C           Read 24-hour Records from RAMMET-type UNFORMatted File
            IF (NEWDAY) THEN
               READ(MFUNIT,END=1000,ERR=99,IOSTAT=IOERRN) IYEAR, IMONTH,
     &              DAY, IKST, AUREF, ATA, AAFV, AAFVR, AZI
               NEWDAY = .FALSE.
               IHOUR = 0
               JDAY = INT(DAY)
C              Determine Day of Month Number, IDAY     ---   CALL GREGOR
               CALL GREGOR(IYEAR,IMONTH,JDAY,IDAY)
            END IF
            IHOUR = IHOUR + 1
C           Specify Flow Vector for Previous Hour, AFVM1
            IF (IHOUR .EQ. 1) THEN
               AFVM1 = AFV24
            ELSE IF (IHOUR .LT. 24) THEN
               AFVM1 = AAFV(IHOUR-1)
            ELSE IF (IHOUR .EQ. 24) THEN
               AFVM1  = AAFV(IHOUR-1)
               AFV24  = AAFV(IHOUR)
               NEWDAY = .TRUE.
            END IF
            IF (AAFV(IHOUR) .EQ. AFVM1 .AND. AUREF(IHOUR) .EQ. 1.0) THEN
C              Set Wind Speed to 0.0 for Calm Hour
               AUREF(IHOUR) = 0.0
            END IF
C           Set Variables for the Current Hour
            AFV   = AAFVR(IHOUR)
            UREF  = AUREF(IHOUR)
            TA    = ATA(IHOUR)
            KST   = IKST(IHOUR)
            ZIRUR = AZI(1,IHOUR)
            ZIURB = AZI(2,IHOUR)
         ELSE IF (METFRM .EQ. 'FREE') THEN
C           Read Hourly Records from ASCII File Using FREE Format -
C           Yr, Mn, Dy, Hr, AFV, UREF, TA, KST, ZIRUR, ZIURB
            READ(MFUNIT,*,END=1000,ERR=99,IOSTAT=IOERRN) IYEAR, IMONTH,
     &           IDAY, IHOUR, AFV, UREF, TA, KST, ZIRUR, ZIURB
         ELSE IF (METFRM .EQ. 'CARD') THEN
C           Read Hourly Records from ASCII File Using CARD Format -
C           Yr, Mn, Dy, Hr, AFV, UREF, TA, KST, ZIRUR, ZIURB, P, DTDZ
            READ(MFUNIT,9009,END=1000,ERR=99,IOSTAT=IOERRN) IYEAR,
     &           IMONTH, IDAY, IHOUR, AFV, UREF, TA, KST, ZIRUR, ZIURB,
     &           P, DTDZ
         ELSE
C           Read Hourly Records from Formatted ASCII File Using METFRM
            READ(MFUNIT,METFRM,END=1000,ERR=99,IOSTAT=IOERRN) IYEAR,
     &           IMONTH, IDAY, IHOUR, AFV, UREF, TA, KST, ZIRUR, ZIURB
         END IF
      ENDIF

C     Retrieve Appropriate Mixing Height
      IF (RURAL) THEN
         ZI = ZIRUR
      ELSE IF (URBAN) THEN
         ZI = ZIURB
      END IF

C     Determine The Current Julian Day and Calculate Current Gregorian Date
C     First Convert Year to 4-Digit Value
      IF (IYEAR .GE. 50 .AND. IYEAR .LE. 99) THEN
         IYR = 1900 + IYEAR
      ELSE IF (IYEAR .LT. 50) THEN
         IYR = 2000 + IYEAR
      END IF
      IF (METFRM .NE. 'UNFORM') THEN
C        Determine Julian Day (Day of Year) Number, JDAY    ---   CALL JULIAN
         CALL JULIAN(IYR,IMONTH,IDAY,JDAY)
      END IF

C     Calculate Integer Variable for Current Date/Hour, KURDAT
      KURDAT = IYEAR*1000000 + IMONTH*10000 + IDAY*100 + IHOUR

      IF (MONTH .AND. IHOUR .EQ. 24) THEN
C        Check for the End of the Month
         IF (IMONTH .EQ. 1 .OR. (MOD(IYR,4) .NE. 0) .OR.
     &      (MOD(IYR,100) .EQ. 0 .AND. MOD(IYR,400) .NE. 0)) THEN
C           Not a Leap Year OR Month = January
            IF (JDAY .EQ. NDAY(IMONTH)) THEN
               ENDMON = .TRUE.
            END IF
         ELSE
C           Leap Year AND Month > January
            IF (JDAY .EQ. NDAY(IMONTH)+1) THEN
               ENDMON = .TRUE.
            END IF
         END IF
      END IF

      ILINE = ILINE + 1

      IF (ILINE .EQ. 1) THEN
C        Write Out Sample of the Meteorology Data
C        (Up to the First 24 Hours)                         ---   CALL METDAT
         CALL METDAT
      END IF

C     Check Data for Calms, Missing, Out-of-Range Values    ---   CALL METCHK
      CALL METCHK

C     Apply ROTANG Adjustment to Flow Vector
      IF (ROTANG .NE. 0.0) THEN
         AFV = AFV - ROTANG
         IF (AFV .LE. 0.0) THEN
            AFV = AFV + 360.
         END IF
      END IF

      IF ((.NOT.CLMHR .OR. .NOT.CLMPRO) .AND.
     &    (.NOT.MSGHR .OR. .NOT.MSGPRO)) THEN
C        Convert Flow Vector in Degrees to Wind Direction in Radians
         WDRAD = (AFV + 180.0) * DTORAD
C        Determine Nearest Ten Degree Sector
         IFVSEC = INT (AFV*0.10 + 0.4999)
         IF (IFVSEC .EQ. 0) IFVSEC = 36
C        Determine SIN and COS of WDRAD for Later Use in SUB. XYDIST
         WDSIN = SIN(WDRAD)
         WDCOS = COS(WDRAD)
      END IF

C     Set Stability Category Logical Flags and Adjust for KST > 6
      UNSTAB = .FALSE.
      NEUTRL = .FALSE.
      STABLE = .FALSE.
      IF (KST .GE. 1 .AND. KST .NE. KSTMSG) THEN
         IF (KST .GT. 6) KST = 6
         IF (KST .LT. 4) THEN
            UNSTAB = .TRUE.
         ELSE IF (KST .EQ. 4) THEN
            NEUTRL = .TRUE.
         ELSE IF (KST .GT. 4) THEN
            STABLE = .TRUE.
         END IF
      ELSE
         KST = KSTMSG
         MSGHR = .TRUE.
      END IF

C     Set Appropriate Wind Speed Category Index
      IF (UREF .LE. UCAT(1)) THEN
         IUCAT = 1
      ELSE IF (UREF .LE. UCAT(2)) THEN
         IUCAT = 2
      ELSE IF (UREF .LE. UCAT(3)) THEN
         IUCAT = 3
      ELSE IF (UREF .LE. UCAT(4)) THEN
         IUCAT = 4
      ELSE IF (UREF .LE. UCAT(5)) THEN
         IUCAT = 5
      ELSE
         IUCAT = 6
      END IF

C     Select Appropriate Power Law Exponent
      IF (KST .NE. KSTMSG .AND. METFRM .NE. 'CARD') THEN
         IF (USERP) THEN
            P = PUSER(KST,IUCAT)
         ELSE IF (URBAN) THEN
            P = PURB(KST)
         ELSE IF (RURAL) THEN
            P = PRUR(KST)
         END IF
      END IF

C*    Scale the Wind Speed from Anemometer Height to 10 meter

      UREF10 = UREF * (10.0/ZREF)**P

C*    Do Not Allow 10m Wind Speed < 1.0 m/s
      IF (UREF10 .LT. 1.0) THEN
         UREF10 = 1.0
      END IF

C     Select Appropriate delta theta/delta z
      IF (KST .NE. KSTMSG .AND. METFRM .NE. 'CARD') THEN
         IF (USERDT) THEN
            DTDZ = DTUSER(KST,IUCAT)
         ELSE IF (URBAN) THEN
            DTDZ = DTURB(KST)
         ELSE IF (RURAL) THEN
            DTDZ = DTRUR(KST)
         END IF
      END IF
      IF (DTDZ .GT. 0.0 .AND. TA .GT. 0.0) THEN
         S = G*DTDZ/TA
         RTOFS = SQRT(S)
      ELSE
         S = 1.0E-10
         RTOFS = 1.0E-10
      END IF

      IF (MSGHR) THEN
         IF (.NOT. MSGPRO) THEN
C           Set Flag for Runtime Met. Error to Prevent Further Calculations
            RUNERR = .TRUE.
C           WRITE Error Message:  Missing Meteorological Data
            WRITE(DUMMY,'(I8)') KURDAT
            CALL ERRHDL(PATH,MODNAM,'E','460',DUMMY)
         ELSE
C           WRITE Informational Message:  Missing Meteorological Data
            WRITE(DUMMY,'(I8)') KURDAT
            CALL ERRHDL(PATH,MODNAM,'I','460',DUMMY)
         END IF
      END IF

      GO TO 999

C     WRITE Error Message:  Error Reading Met Data File
 99   CALL ERRHDL(PATH,MODNAM,'E','510',' MET-INP ')
      RUNERR = .TRUE.
      GO TO 999

 1000 EOF = .TRUE.

 999  RETURN
      END

      SUBROUTINE METCHK
C***********************************************************************
C                 METCHK Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Performs Various Checks and Quality Assurance of
C                 One Hour of Meteorological Data
C
C        PROGRAMMER: JEFF WANG, ROGER BRODE
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Meteorological Variables for One Hour
C
C        OUTPUTS: Meteorological Data Error and Status Switches
C
C        CALLED FROM:   METEXT
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'

C     Variable Initializations
      MODNAM = 'METCHK'
      CLMHR  = .FALSE.
      MSGHR  = .FALSE.

C*----   ISCSTM Modification: allow for NOCHKD option - jah 11/2/94                           
      IF (.NOT. NOCHKD) THEN
C*       Check date for record out of sequence on the surface
C*       scaling file - NOCHKD=.TRUE. means no date check   ---   CALL CHKDAT
         CALL CHKDAT
      END IF
C*----
C*#   
C     Check Data for Calm Winds                             ---   CALL CHKCLM
      CALL CHKCLM
C     Check Data for Missing Data Indicators                ---   CALL CHKMSG
      CALL CHKMSG
C     Check Data for Out-of-Range Values                    ---   CALL METQA
      CALL METQA

      RETURN
      END

      SUBROUTINE CHKDAT
C***********************************************************************
C                 CHKDAT Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Checks Meteorological Data for Record Out of Sequence
C
C        PROGRAMMER: Roger Brode
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Date Variable
C
C        OUTPUTS: Date Error Messages
C
C        CALLED FROM:   METCHK
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'

C     Variable Initializations
      MODNAM = 'CHKDAT'

C     Check for Record Out of Sequence
      IF (METFRM.NE.'UNFORM' .AND. IPDATE.NE.0) THEN
         IF (KURDAT .LE. IPDATE) THEN
C           WRITE Error Message - Record Out of Sequence
            WRITE(DUMMY,'(I8)') KURDAT
            CALL ERRHDL(PATH,MODNAM,'E','450',DUMMY)
            RUNERR = .TRUE.
         ELSE IF (IHOUR.NE.1 .AND. (KURDAT-IPDATE).NE.1) THEN
C           WRITE Error Message - Record Out of Sequence
            WRITE(DUMMY,'(I8)') KURDAT
            CALL ERRHDL(PATH,MODNAM,'E','450',DUMMY)
            RUNERR = .TRUE.
         ELSE IF (IHOUR.EQ.1 .AND. IPHOUR.NE.24) THEN
C           WRITE Error Message - Record Out of Sequence
            WRITE(DUMMY,'(I8)') KURDAT
            CALL ERRHDL(PATH,MODNAM,'E','450',DUMMY)
            RUNERR = .TRUE.
         END IF
      END IF

      RETURN
      END

      SUBROUTINE CHKCLM
C***********************************************************************
C                 CHKCLM Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Checks One Hour Meteorological Data for Calm Winds
C
C        PROGRAMMER: ROGER BRODE, JEFF WANG
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Meteorological Variables for One Hour
C
C        OUTPUTS: Calm Hour Flag, CLMHR, and Message
C
C        CALLED FROM:   METCHK
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'

C     Variable Initializations
      MODNAM = 'CHKCLM'

C     Check Data for Calm Winds (<= Threshold Value, UMIN)
C     The Threshold Value is Initially Set = 0.0
      IF (UREF .GE. 0.0 .AND. UREF .LE. UMIN) THEN
         CLMHR = .TRUE.
C        WRITE Informational Message: Calm Hour
         WRITE(DUMMY,'(I8)') KURDAT
         CALL ERRHDL(PATH,MODNAM,'I','440',DUMMY)
         IF (.NOT. CLMPRO) THEN
            UREF = 1.0
         END IF
      END IF

      RETURN
      END

      SUBROUTINE CHKMSG
C***********************************************************************
C                 CHKMSG Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Checks One Hour Meteorological Data for Missing Data
C
C        PROGRAMMER: JEFF WANG
C
C        DATE:    March 2, 1992
C
C        MODIFIED:  To Change Temperature Range Check - 9/29/92
C
C        INPUTS:  Meteorological Variables for One Hour
C
C        OUTPUTS: Meteorological Data Error and Status Switches
C
C        CALLED FROM:   METCHK
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'

C     Variable Initializations
      MODNAM = 'CHKMSG'

C     Check Data for Missing Data Indicators
      IF (UREF .GE. 90.0 .OR. UREF .LE. -9.0) THEN
         MSGHR = .TRUE.
      ELSE IF (AFV .GT. 900. .OR. AFV .LE. -90.) THEN
         MSGHR = .TRUE.
      ELSE IF (KST .EQ. KSTMSG) THEN
         MSGHR = .TRUE.
      ELSE IF (TA .GT. 900. .OR. TA .LE. 0.) THEN
         MSGHR = .TRUE.
      ELSE IF (ZI .GT. 90000. .OR. ZI .LE. -90.) THEN
         MSGHR = .TRUE.
      END IF

      RETURN
      END

      SUBROUTINE METQA
C***********************************************************************
C                 METQA Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Performs Quality Assurance Checks for
C                 One Hour of Meteorological Data
C
C        PROGRAMMER: JEFF WANG, ROGER BRODE
C
C        DATE:    March 2, 1992
C
C        MODIFIED:  To check for errors reading surface variables for
C                   new deposition algorithms.  R. Brode, PES, 12/6/94
C
C        MODIFIED:  To Change Temperature Range Check Lower Limit To
C                   230 K - 9/29/92
C
C        INPUTS:  Meteorological Variables for One Hour
C
C        OUTPUTS: Meteorological Data Error and Status Switches
C
C        CALLED FROM:   METCHK
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'

C     Variable Initializations
      MODNAM = 'METQA'

C     Check Data for Out-of-Range Values:

C     Wind Direction Check:
      IF (AFV.LT.0.0 .OR. AFV.GT.360.0) THEN
C        WRITE Warning Message: Invalid Flow Vector; and Set MSGHR Flag
         WRITE(DUMMY,'(I8)') KURDAT
         CALL ERRHDL(PATH,MODNAM,'W','410',DUMMY)
         MSGHR = .TRUE.
      END IF

C     Wind Speed Range Check
      IF (UREF .LT. 0.0) THEN
C        WRITE Warning Message: Invalid Wind Speed; and Set MSGHR Flag
         WRITE(DUMMY,'(I8)') KURDAT
         CALL ERRHDL(PATH,MODNAM,'W','420',DUMMY)
         MSGHR = .TRUE.
      END IF
      IF (UREF .GT. 30.0) THEN
C        WRITE Warning Message: Wind Speed Over 30m/s
         WRITE(DUMMY,'(I8)') KURDAT
         CALL ERRHDL(PATH,MODNAM,'W','420',DUMMY)
      END IF

C     Ambient Temperature Check
      IF (TA.LT.230.0 .OR. TA.GT.320.0) THEN
C        WRITE Warning Message: Ambient Temperature May be Out-of-Range
         WRITE(DUMMY,'(I8)') KURDAT
         CALL ERRHDL(PATH,MODNAM,'W','430',DUMMY)
      END IF

C     Check for missing (i.e., zero) USTAR, EL (Monin-Obukhov Length),
C     and Z0M (surface roughness length) when deposition algorithms are used.
      IF (LWPART .OR. LDPART .OR. LWGAS) THEN
         IF (USTAR.EQ.0.0 .AND. EL.EQ.0.0 .AND. Z0M.EQ.0.0) THEN
C           WRITE Error Message:  Error reading meteorology file for deposition
            CALL ERRHDL(PATH,MODNAM,'E','510','DEP-MET')
C           Set Z0M to 1.0E-5 to avoid divide-by-zero error
            Z0M = 1.0E-5
            RUNERR = .TRUE.
         ELSE IF (Z0M .LT. 1.0E-5) THEN
C           WRITE Warning Message:  Surface roughness length out-of-range
            WRITE(DUMMY,'(I8)') KURDAT
            CALL ERRHDL(PATH,MODNAM,'W','435',DUMMY)
C           Set to 1.0E-5 to avoid divide-by-zero error
            Z0M = 1.0E-5
         END IF
      END IF

      RETURN
      END

      SUBROUTINE METDAT
C***********************************************************************
C                 METDAT Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Print Out The Summary Of The Meteorology Data
C
C        PROGRAMMER: JEFF WANG
C        MODIFIED BY D. Strimaitis, SRC (for Wet & Dry DEPOSITION)
C
C        DATE:    November 8, 1993
C
C        MODIFIED BY D. Strimaitis, SRC (for Dry DEPOSITION)
C        (DATE:    February 15, 1993)
C
C        INPUTS:  Meteorology Input Data
C
C        OUTPUTS: Printed Model Outputs
C
C        CALLED FROM:   METEXT
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'
C     Declare Arrays for Use With Unformatted Input Met Files
      INTEGER IKST(24)
      INTEGER IMNTH(24), INDY(24), INHR(24)
      REAL AUREF(24), ATA(24), AAFV(24), AAFVR(24), AZI(2,24)
      REAL APROF(24), ADTDZ(24)
C     Add LOCAL arrays for USTAR, EL, Z0M, IPCODE, PRATE
      REAL AUSTAR(24), AEL(24), AZ0M(24), APRATE(24)
      INTEGER IAPCODE(24)
      COMMON /UNFMET/ IKST, AUREF, ATA, AAFV, AAFVR, AZI

C     Variable Initializations
      MODNAM = 'METDAT'
      DO 10 IHR=1,24
         AUSTAR(IHR) = 0.
         AEL(IHR) = 0.
         AZ0M(IHR) = 0.
cjop     AZDM(IHR) = 0.
         IAPCODE(IHR) = 0
         APRATE(IHR) = 0.
10    CONTINUE

      IF (METFRM .EQ. 'UNFORM') THEN
         INUM = 24

      ELSE IF (METFRM .NE. 'CARD') THEN
C        READ In First 24 Hours of Data
         DO 100 I = 1, 24
            IMNTH(I) = IMONTH
            INDY(I)  = IDAY
            INHR(I)  = IHOUR
            AAFVR(I) = AFV
            AUREF(I) = UREF
            ATA(I)   = TA
            IKST(I)  = KST
            AZI(1,I) = ZIRUR
            AZI(2,I) = ZIURB
            IF (LWPART .OR. LWGAS) THEN
               AUSTAR(I) = USTAR
               AEL(I)    = EL
               AZ0M(I)   = Z0M
cjop           AZDM(I)   = ZDM
               APRATE(I) = PRATE
               IAPCODE(I)= IPCODE
            ELSE IF (LDPART) THEN
               AUSTAR(I) = USTAR
               AEL(I)    = EL
               AZ0M(I)   = Z0M
cjop           AZDM(I)   = ZDM
            ENDIF
            IF (METFRM .EQ. 'FREE') THEN
C              Read Hourly Records from ASCII File Using FREE Format
C              Yr, Mn, Dy, Hr, AFV, UREF, TA, KST, ZIRUR, ZIURB
C              (USTAR, EL, Z0M), (IPCODE, PRATE)
               IF (LWPART .OR. LWGAS) THEN
                  READ(MFUNIT,*,END=999,ERR=99,IOSTAT=IOERRN) IYEAR,
     &                 IMONTH,IDAY,IHOUR,AFV,UREF,TA,KST,ZIRUR,ZIURB,
     &                 USTAR,EL,Z0M,IPCODE,PRATE
               ELSE IF (LDPART) THEN
                  READ(MFUNIT,*,END=999,ERR=99,IOSTAT=IOERRN) IYEAR,
     &                 IMONTH,IDAY,IHOUR,AFV,UREF,TA,KST,ZIRUR,ZIURB,
     &                 USTAR,EL,Z0M
               ELSE
                  READ(MFUNIT,*,END=999,ERR=99,IOSTAT=IOERRN) IYEAR,
     &                 IMONTH,IDAY,IHOUR,AFV,UREF,TA,KST,ZIRUR,ZIURB
               ENDIF
            ELSE
C              Read Hourly Records from Formatted ASCII File Using METFRM
               IF (LWPART .OR. LWGAS) THEN
                  READ(MFUNIT,METFRM,END=999,ERR=99,IOSTAT=IOERRN)IYEAR,
     &                 IMONTH,IDAY,IHOUR,AFV,UREF,TA,KST,ZIRUR,ZIURB,
     &                 USTAR,EL,Z0M,IPCODE,PRATE
               ELSE IF (LDPART) THEN
                  READ(MFUNIT,METFRM,END=999,ERR=99,IOSTAT=IOERRN)IYEAR,
     &                 IMONTH,IDAY,IHOUR,AFV,UREF,TA,KST,ZIRUR,ZIURB,
     &                 USTAR,EL,Z0M
               ELSE
                  READ(MFUNIT,METFRM,END=999,ERR=99,IOSTAT=IOERRN)IYEAR,
     &                 IMONTH,IDAY,IHOUR,AFV,UREF,TA,KST,ZIRUR,ZIURB
               ENDIF
            END IF
 100     CONTINUE
C        Save Number of Records (Up to 24);  REWIND Met File, Skip First
C        Two Records, and Reset Variables to First Hour
 999     INUM = AMIN0(I,24)
         REWIND MFUNIT
         READ(MFUNIT,'(I2)') IDUM
         IF (METFRM .EQ. 'FREE') THEN
C           Read Hourly Records from ASCII File Using FREE Format
C           Yr, Mn, Dy, Hr, AFV, UREF, TA, KST, ZIRUR, ZIURB
C           (USTAR, EL, Z0M), (IPCODE, PRATE)
            IF (LWPART .OR. LWGAS) THEN
               READ(MFUNIT,*,END=999,ERR=99,IOSTAT=IOERRN) IYEAR,
     &              IMONTH,IDAY,IHOUR,AFV,UREF,TA,KST,ZIRUR,ZIURB,
     &              USTAR,EL,Z0M,IPCODE,PRATE
            ELSE IF (LDPART) THEN
               READ(MFUNIT,*,END=999,ERR=99,IOSTAT=IOERRN) IYEAR,
     &              IMONTH,IDAY,IHOUR,AFV,UREF,TA,KST,ZIRUR,ZIURB,
     &              USTAR,EL,Z0M
            ELSE
               READ(MFUNIT,*,END=999,ERR=99,IOSTAT=IOERRN) IYEAR,
     &             IMONTH,IDAY,IHOUR,AFV,UREF,TA,KST,ZIRUR,ZIURB
            ENDIF
         ELSE
C           Read Hourly Records from Formatted ASCII File Using METFRM
            IF (LWPART .OR. LWGAS) THEN
               READ(MFUNIT,METFRM,END=999,ERR=99,IOSTAT=IOERRN)IYEAR,
     &              IMONTH,IDAY,IHOUR,AFV,UREF,TA,KST,ZIRUR,ZIURB,
     &              USTAR,EL,Z0M,IPCODE,PRATE
            ELSE IF (LDPART) THEN
               READ(MFUNIT,METFRM,END=999,ERR=99,IOSTAT=IOERRN)IYEAR,
     &              IMONTH,IDAY,IHOUR,AFV,UREF,TA,KST,ZIRUR,ZIURB,
     &              USTAR,EL,Z0M
            ELSE
               READ(MFUNIT,METFRM,END=999,ERR=99,IOSTAT=IOERRN)IYEAR,
     &              IMONTH,IDAY,IHOUR,AFV,UREF,TA,KST,ZIRUR,ZIURB
            ENDIF
         END IF

      ELSE IF (METFRM .EQ. 'CARD') THEN
C        READ In First 24 Hours of Data
         DO 200 I = 1, 24
            IMNTH(I) = IMONTH
            INDY(I)  = IDAY
            INHR(I)  = IHOUR
            AAFVR(I) = AFV
            AUREF(I) = UREF
            ATA(I)   = TA
            IKST(I)  = KST
            AZI(1,I) = ZIRUR
            AZI(2,I) = ZIURB
            APROF(I) = P
            ADTDZ(I) = DTDZ
            IF (LWPART .OR. LWGAS) THEN
               AUSTAR(I) = USTAR
               AEL(I)    = EL
               AZ0M(I)   = Z0M
cjop           AZDM(I)   = ZDM
               APRATE(I) = PRATE
               IAPCODE(I)= IPCODE
            ELSE IF (LDPART) THEN
               AUSTAR(I) = USTAR
               AEL(I)    = EL
               AZ0M(I)   = Z0M
cjop           AZDM(I)   = ZDM
            ENDIF
C           Read Hourly Records from ASCII File Using CARD Format
C           Yr, Mn, Dy, Hr, AFV, UREF, TA, KST, ZIRUR, ZIURB, P, DTDZ
C           (USTAR, EL, Z0M), (IPCODE, PRATE)
cjop        FORMAT(4I2,2F9.4,F6.1,I2,2F7.1,2F8.4,F9.4,F10.1,F8.4,f5.1,
 9009       FORMAT(4I2,2F9.4,F6.1,I2,2F7.1,2F8.4,F9.4,F10.1,F8.4,
     &             I4,F7.2)
            IF (LWPART .OR. LWGAS) THEN
               READ(MFUNIT,9009,END=1000,ERR=99,IOSTAT=IOERRN) IYEAR,
     &              IMONTH,IDAY,IHOUR,AFV,UREF,TA,KST,ZIRUR,ZIURB,P,
     &              DTDZ,USTAR,EL,Z0M,IPCODE,PRATE
            ELSE IF (LDPART) THEN
               READ(MFUNIT,9009,END=1000,ERR=99,IOSTAT=IOERRN) IYEAR,
     &              IMONTH,IDAY,IHOUR,AFV,UREF,TA,KST,ZIRUR,ZIURB,P,
     &              DTDZ,USTAR,EL,Z0M
            ELSE
               READ(MFUNIT,9009,END=1000,ERR=99,IOSTAT=IOERRN) IYEAR,
     &              IMONTH,IDAY,IHOUR,AFV,UREF,TA,KST,ZIRUR,ZIURB,P,
     &              DTDZ
            ENDIF
 200     CONTINUE
 1000    INUM = AMIN0(I,24)
C        Save Number of Records (Up to 24);  REWIND Met File, Skip First
C        Two Records, and Reset Variables to First Hour
         REWIND MFUNIT
         READ(MFUNIT,'(I2)') IDUM
         IF (LWPART .OR. LWGAS) THEN
            READ(MFUNIT,9009,END=1000,ERR=99,IOSTAT=IOERRN) IYEAR,
     &           IMONTH,IDAY,IHOUR,AFV,UREF,TA,KST,ZIRUR,ZIURB,P,
     &           DTDZ,USTAR,EL,Z0M,IPCODE,PRATE
         ELSE IF (LDPART) THEN
            READ(MFUNIT,9009,END=1000,ERR=99,IOSTAT=IOERRN) IYEAR,
     &           IMONTH,IDAY,IHOUR,AFV,UREF,TA,KST,ZIRUR,ZIURB,P,
     &           DTDZ,USTAR,EL,Z0M
         ELSE
            READ(MFUNIT,9009,END=1000,ERR=99,IOSTAT=IOERRN) IYEAR,
     &           IMONTH,IDAY,IHOUR,AFV,UREF,TA,KST,ZIRUR,ZIURB,P,
     &           DTDZ
         ENDIF

      END IF

C     WRITE Out Header Information
      CALL HEADER
      WRITE(IOUNIT,9011) INUM
      WRITE(IOUNIT,9016) METINP, METFRM
      WRITE(IOUNIT,9020) IDSURF, IDUAIR, SFNAME, UANAME,
     &                   ISYEAR, IUYEAR
      IF (METFRM .NE. 'CARD') THEN
         WRITE(IOUNIT,9025)
      ELSE
         WRITE(IOUNIT,9026)
      END IF

C     WRITE Out First INUM Records (Up to 24)
      DO 300 I = 1, INUM
         IF (METFRM .EQ. 'UNFORM') THEN
            WRITE(IOUNIT,9032) IYEAR, IMONTH, IDAY, I, AAFVR(I),
     &            AUREF(I), ATA(I), IKST(I), AZI(1,I), AZI(2,I),
     &            AUSTAR(I), AEL(I), AZ0M(I), IAPCODE(I),
     &            APRATE(I)
         ELSE IF (METFRM .NE. 'CARD') THEN
            WRITE(IOUNIT,9032) IYEAR, IMNTH(I), INDY(I), INHR(I),
     &            AAFVR(I),AUREF(I),ATA(I), IKST(I), AZI(1,I), AZI(2,I),
     &            AUSTAR(I), AEL(I), AZ0M(I), IAPCODE(I),
     &            APRATE(I)
         ELSE
            WRITE(IOUNIT,9033) IYEAR, IMNTH(I), INDY(I), INHR(I),
     &            AAFVR(I),AUREF(I),ATA(I), IKST(I), AZI(1,I), AZI(2,I),
     &            APROF(I), ADTDZ(I), AUSTAR(I), AEL(I), AZ0M(I),
     &            IAPCODE(I), APRATE(I)
         END IF
 300  CONTINUE

C     Write Out Explanatory Message for Stability Class
      WRITE(IOUNIT,9050)

      GO TO 9999

 9011 FORMAT(/22X,'*** THE FIRST ',I3,' HOURS OF ',
     &       'METEOROLOGICAL DATA ***'/)
 9016 FORMAT(12X,'FILE: ',A40,' FORMAT: ',A60)
 9020 FORMAT(12X,'SURFACE STATION NO.: ',I6,20X,
     &       'UPPER AIR STATION NO.: ',I6/27X,'NAME: ',A40,3X,
     &       'NAME: ',A40/27X,'YEAR: ',I6,37X,'YEAR: ',I6)
 9025 FORMAT(/38X,'FLOW',4X,'SPEED',3X,'TEMP',5X,'STAB',4X,
     &       'MIXING HEIGHT (M)',4X,'USTAR',2X,'M-O LENGTH',3X,'Z-0',
     &       1X,'IPCODE',1X,'PRATE',
     &       /11X,'YEAR',2X,'MONTH',2X,'DAY',2X,'HOUR',4X,'VECTOR',
     &       3X,'(M/S)',4X,'(K)',5X,'CLASS',4X,'RURAL',4X,'URBAN',
     &       6X,'(M/S)',5X,'(M)',7X,'(M)',7X, '(mm/HR)',
     &       /75('- ')/)
 9026 FORMAT(/38X,'FLOW',4X,'SPEED',3X,'TEMP',5X,'STAB',4X,
     &       'MIXING HEIGHT (M)',4X,'WIND',4X,'VERT TEMP',
     &       5X,'USTAR',2X,'M-O LENGTH',3X,'Z-0',
     &       3X,'IPCODE',1X,'PRATE',
     &       /11X,'YEAR',2X,'MONTH',2X,'DAY',2X,'HOUR',4X,'VECTOR',
     &       3X,'(M/S)',4X,'(K)',5X,'CLASS',4X,'RURAL',4X,'URBAN',
     &       6X,'PROF',4X,'GRAD (K/M)',4X,'(M/S)',5X,'(M)',7X,'(M)',
     &           7X, '(mm/HR)',
     &       /75('- ')/)
 9032 FORMAT(12X,4(I2,4X),1X,F6.1,2X,F6.2,2X,F6.1,6X,I1,5X,2(F7.1,2X),
     &       F9.4,F10.1,F8.4,I4,F7.2)
 9033 FORMAT(12X,4(I2,4X),1X,F6.1,2X,F6.2,2X,F6.1,6X,I1,5X,2(F7.1,2X),
     &       2(F8.4,2X),F9.4,F10.1,F8.4,I4,F7.2)

cjop  FORMAT(12X,4(I2,4X),1X,F6.1,2X,F6.2,2X,F6.1,6X,I1,5X,2(F7.1,2X),
cjop &       F9.4,F10.1,F8.4,F5.1,I4,F7.2)
cjop  FORMAT(12X,4(I2,4X),1X,F6.1,2X,F6.2,2X,F6.1,6X,I1,5X,2(F7.1,2X),
cjop &       2(F8.4,2X),F9.4,F10.1,F8.4,F5.1,I4,F7.2)

 9050 FORMAT(///' *** NOTES:  STABILITY CLASS 1=A, 2=B, 3=C, 4=D, 5=E',
     &       ' AND 6=F.',
     &         /'             FLOW VECTOR IS DIRECTION TOWARD WHICH ',
     &       'WIND IS BLOWING.')

C     WRITE Error Message:  Error Reading Met Data Input File
 99   CALL ERRHDL(PATH,MODNAM,'E','510',' MET-INP ')
      RUNERR = .TRUE.

 9999 RETURN
      END
