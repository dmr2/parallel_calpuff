      SUBROUTINE RECARD
C***********************************************************************
C                 RECARD Module of ISC2 Model
C
C        PURPOSE: To process REceptor Pathway card images
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        MODIFIED:   To remove some restrictions on the order of
C                    the BOUNDELV keyword - 9/29/92
C
C        INPUTS:  Pathway (RE) and Keyword
C
C        OUTPUTS: Receptor Arrays
C                 Receptor Setup Status Switches
C
C        CALLED FROM:   SETUP
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'

C     Variable Initializations
      MODNAM = 'RECARD'

      IF (KEYWRD .EQ. 'STARTING') THEN
C        Initialize Counters and Set Status Switch
         IREC = 0
         INNET = 0
         NUMREC = 0
         IRXR = 0
         IRYR = 0
         IRZE = 0
         IRZF = 0
         IBND = 0
         IBELEV = 0
         PXSOID = ' '
         PESOID = ' '
         ISTA = .FALSE.
         IRSTAT(1) = IRSTAT(1) + 1
         IF (IRSTAT(1) .NE. 1) THEN
C           Error Message: Repeat Starting In Same Pathway
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         END IF
C        Flush the Working Arrays
         DO 10 I = 1, NREC
            ZETMP1(I) = 0.0
            ZETMP2(I) = 0.0
            ZFTMP1(I) = 0.0
            ZFTMP2(I) = 0.0
 10      CONTINUE
      ELSE IF (KEYWRD .EQ. 'GRIDCART') THEN
C        Set Status Switch
         IRSTAT(2) = IRSTAT(2) + 1
C        Process Cartesian Grid Receptor Network            ---   CALL RECART
         CALL RECART
      ELSE IF (KEYWRD .EQ. 'GRIDPOLR') THEN
C        Set Status Switch
         IRSTAT(3) = IRSTAT(3) + 1
C        Process Polar Receptor Network                     ---   CALL REPOLR
         CALL REPOLR
      ELSE IF (KEYWRD .EQ. 'DISCCART') THEN
C        Set Status Switch
         IRSTAT(4) = IRSTAT(4) + 1
C        Process Discrete Cartesian Receptor Locations      ---   CALL DISCAR
         CALL DISCAR
      ELSE IF (KEYWRD .EQ. 'DISCPOLR') THEN
C        Set Status Switch
         IRSTAT(5) = IRSTAT(5) + 1
C        Process Discrete Polar Receptor Locations          ---   CALL DISPOL
         CALL DISPOL
      ELSE IF (KEYWRD .EQ. 'BOUNDARY' .OR.
     &         KEYWRD .EQ. 'BOUNDELV') THEN
C        Set Status Switch
         IF (KEYWRD .EQ. 'BOUNDARY') THEN
            IRSTAT(6) = IRSTAT(6) + 1
         ELSE IF (KEYWRD .EQ. 'BOUNDELV') THEN
            IRSTAT(7) = IRSTAT(7) + 1
         END IF
C        Process Plant Boundary Receptor Locations          ---   CALL BOUNDR
         CALL BOUNDR
      ELSE IF (KEYWRD .EQ. 'ELEVUNIT') THEN
C        Set Status Switch
         IRSTAT(8) = IRSTAT(8) + 1
         IF (IRSTAT(8) .NE. 1) THEN
C           WRITE Error Message: Repeat Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE IF (IRSTAT(2) .GT. 0 .OR. IRSTAT(3) .GT. 0 .OR.
     &            IRSTAT(4) .GT. 0 .OR. IRSTAT(5) .GT. 0 .OR.
     &            IRSTAT(6) .GT. 0 .OR. IRSTAT(7) .GT. 0) THEN
C           Write Error Message: ELEVUNIT must be first card after STARTING
            CALL ERRHDL(PATH,MODNAM,'E','152','  RE')
         ELSE IF (ICSTAT(10) .NE. 0) THEN
C           Write Error Message: Use of obsolescent CO ELEVUNIT card with
C           RE ELEVUNIT card
            CALL ERRHDL(PATH,MODNAM,'E','153',' RE Path')
         ELSE
C           Process Elevation Units for Source Elevations   ---   CALL REELUN
            CALL REELUN
         END IF
      ELSE IF (KEYWRD .EQ. 'FINISHED') THEN
C        Set Status Switch
         IRSTAT(20) = IRSTAT(20) + 1
         IF (IRSTAT(20) .NE. 1) THEN
C           Error Message: Repeat Finished In Same Pathway
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
            GO TO 999
         END IF
C        Write Out The Error Message: Mandatory Keyword Missing
         IF (IRSTAT(1) .EQ. 0)THEN
            CALL ERRHDL(PATH,MODNAM,'E','130','STARTING')
         END IF

         IF (IRSTAT(2).EQ.0 .AND. IRSTAT(3).EQ.0 .AND.
     &       IRSTAT(4).EQ.0 .AND. IRSTAT(5).EQ.0 .AND.
     &       IRSTAT(6).EQ.0) THEN
C           WRITE Error Message:  No Receptor Keywords
            CALL ERRHDL(PATH,MODNAM,'E','185',' ')
         END IF

         IF (ISTA) THEN
C           WRITE Error Message:  Missing END Keyword for a Grid Network
            CALL ERRHDL(PATH,MODNAM,'E','175',PNETID)
         END IF

         IF (IRSTAT(6) .GT. 0) THEN
C           Check for Correct Number of Distances for Boundary Receptor
            IF (IBND .LT. 36) THEN
C              Error Message: No. Of Dist Not Enough
               CALL ERRHDL(PATH,MODNAM,'E','230','BOUNDARY')
            END IF
         END IF

         IF (ELEV .AND. IRSTAT(7) .GT. 0) THEN
C           Check for Correct Number of Elevations for Boundary Receptor
            IF (IBELEV .LT. 36) THEN
C              Error Message: No. Of Elev Not Enough
               CALL ERRHDL(PATH,MODNAM,'E','230','BOUNDELV')
            END IF
         END IF

C        Set Total Number of Receptors for This Run, NUMREC
         NUMREC = IRXR
         IF (NUMREC .EQ. 0) THEN
C           WRITE Error Message:  No Receptors Defined
            CALL ERRHDL(PATH,MODNAM,'E','227','NUMREC=0')
         END IF

C        Reinitialize ZELEV and ZFLAG arrays if needed
         IF (FLAT) THEN
            DO 100 IREC = 1, NUMREC
               AZELEV(IREC) = 0.0
 100        CONTINUE
         END IF
         IF (.NOT. FLGPOL) THEN
            DO 200 IREC = 1, NUMREC
               AZFLAG(IREC) = 0.0
 200        CONTINUE
         END IF

      ELSE
C        Write Error Message:  Invalid Keyword for This Pathway
         CALL ERRHDL(PATH,MODNAM,'E','110',KEYWRD)
      END IF

 999  RETURN
      END

      SUBROUTINE REELUN
C***********************************************************************
C                 REELUN Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Process Elevation Units Option for Receptors
C                 From Runstream Input Image
C
C        PROGRAMMER: Roger Brode
C
C        DATE:    November 22, 1994
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Receptor Elevation Units Switch
C
C        ERROR HANDLING:   Checks for Invalid Parameters;
C                          Checks for No Parameters;
C                          Checks for Too Many Parameters
C
C        CALLED FROM:   RECARD
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'

C     Variable Initializations
      MODNAM = 'REELUN'

      IF (IFC .EQ. 3) THEN
         IF (FIELD(3) .EQ. 'METERS') THEN
            REELEV = 'METERS'
         ELSE IF (FIELD(3) .EQ. 'FEET') THEN
            REELEV = 'FEET'
         ELSE
C           WRITE Error Message  ! Invalid Parameter
            CALL ERRHDL(PATH,MODNAM,'E','203','RE_ELEV')
         END IF
      ELSE IF (IFC .GT. 3) THEN
C        WRITE Error Message     ! Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
      ELSE
C        WRITE Error Message     ! No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200','ElevUnit')
      END IF

 999  RETURN
      END

      SUBROUTINE RECART
C***********************************************************************
C                 RECART Module of ISC2 Model
C
C        PURPOSE: Processes Cartesian Grid Receptor Network Inputs
C
C        PROGRAMMER:  Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        MODIFIED:   To Fix Error Checking - Compare NETIDT With
C                    Full Secondary Keywords - 9/29/92
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Cartesian Grid Receptor Network Inputs
C
C        CALLED FROM:   RECARD
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'

C     Variable Initializations
      MODNAM = 'RECART'

C     READ in the Netid and Nettype
      IF (IFC .LT. 3) THEN
C        Write Error Message: Missing Data Field
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      END IF
      NETIDT = FIELD(3)
      IF (.NOT.NEWID .AND. (NETIDT.EQ.'    ' .OR.
     &    NETIDT.EQ.'XYINC' .OR. NETIDT.EQ.'XPNTS' .OR.
     &    NETIDT.EQ.'YPNTS' .OR. NETIDT.EQ.'ELEV' .OR.
     &    NETIDT.EQ.'FLAG'  .OR. NETIDT.EQ.'END')) THEN
         NETIDT = PNETID
         KTYPE = FIELD(3)
      ELSE IF (.NOT.NEWID .AND. NETIDT.EQ.PNETID) THEN
         KTYPE = FIELD(4)
      ELSE IF (NEWID .AND. NETIDT.NE.' ') THEN
         NEWID = .FALSE.
         KTYPE = FIELD(4)
C        The Keyword Counter
         INNET = INNET + 1
         IF (INNET .GT. NNET) THEN
C           WRITE Error Message:  Too Many Networks
            WRITE(DUMMY,'(I8)') NNET
            CALL ERRHDL(PATH,MODNAM,'E','224',DUMMY)
            RECERR = .TRUE.
            GO TO 999
         END IF
         INCSET = 0
         IXYSET = 0
         IEVSET = 0
         IFGSET = 0
      ELSE
C        Error Message: Invalid Secondary Keyword
         CALL ERRHDL(PATH,MODNAM,'E','170',PNETID)
         RECERR = .TRUE.
         GO TO 999
      END IF

C     Start to Set Up the Network
      IF (KTYPE .EQ. 'STA') THEN
C        Initialize Logical Control Variables
         ISTA = .TRUE.
         IEND = .FALSE.
         NEWID = .FALSE.
         RECERR = .FALSE.
C        Set Counters of Calculation Field
         ICOUNT = 0
         JCOUNT = 0
         IZE = 0
         IZF = 0
         IDC1 = IRXR
C        Check for Previous Grid Network With Same ID
         DO 100 I = 1, INNET-1
            IF (FIELD(3) .EQ. NTID(I)) THEN
C              WRITE Warning Message:  Duplicate Network ID
               CALL ERRHDL(PATH,MODNAM,'W','252',NTID(I))
            END IF
 100     CONTINUE
      ELSE IF (KTYPE .EQ. 'XYINC') THEN
C        Error Message:Conflict Secondary Keyword
         IF (IXYSET .NE. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','180',NETIDT)
         END IF
C        Set the Uniform Spacing Receptor Network           ---   CALL GENCAR
         CALL GENCAR
         INCSET = INCSET + 1
      ELSE IF (KTYPE.EQ.'XPNTS' .OR. KTYPE.EQ.'YPNTS') THEN
C        Error Message:Conflict Secondary Keyword
         IF (INCSET .NE. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','180',NETIDT)
         END IF
C        Set the Non-uniform Spacing Receptor Network       ---   CALL XYPNTS
         CALL XYPNTS
         IXYSET = IXYSET + 1
      ELSE IF (KTYPE .EQ. 'ELEV') THEN
C        Read in and set the Terrain Elevation              ---   CALL TERHGT
         CALL TERHGT
         IEVSET = IEVSET + 1
      ELSE IF (KTYPE .EQ. 'FLAG') THEN
C        Read in and set the Flagpole Receptor              ---   CALL FLGHGT
         CALL FLGHGT
         IFGSET = IFGSET + 1
      ELSE IF (KTYPE .EQ. 'END') THEN
         IEND = .TRUE.
C        Get The Final Results
         IF (.NOT. ISTA) THEN
C           Write Error: MISSING STA OF THE BLOCK DATA
            CALL ERRHDL(PATH,MODNAM,'E','200','STA')
         ELSE IF (.NOT. RECERR) THEN
            CALL SETCAR
         END IF
         ISTA = .FALSE.
         NEWID = .TRUE.
C        Check If The Secondary Parameter Has Been Specified
         IF (IXYSET.EQ.0 .AND. INCSET.EQ.0) THEN
C           WRITE Error Message: Missing (X,Y) Point Setting
            CALL ERRHDL(PATH,MODNAM,'E','212',NETIDT)
         END IF

C        Warning: Elevated Terrain Inputs Inconsistent With Options
         IF (ELEV .AND. IEVSET.EQ.0) THEN
            CALL ERRHDL(PATH,MODNAM,'W','214',NETIDT)
            IRZE = IRXR
         ELSE IF (FLAT .AND. IEVSET.NE.0) THEN
            CALL ERRHDL(PATH,MODNAM,'W','213',NETIDT)
            IRZE = IRXR
         ELSE IF (FLAT .AND. IEVSET.EQ.0) THEN
            IRZE = IRXR
         END IF

C        Warning: Flagpole Receptor Inputs Inconsistent With Options
         IF (FLGPOL .AND. IFGSET.EQ.0) THEN
            CALL ERRHDL(PATH,MODNAM,'W','216',NETIDT)
            IRZF = IRXR
         ELSE IF (.NOT.FLGPOL .AND. IFGSET.NE.0) THEN
            CALL ERRHDL(PATH,MODNAM,'W','215',NETIDT)
            IRZF = IRXR
         ELSE IF (.NOT.FLGPOL .AND. IFGSET.EQ.0) THEN
            IRZF = IRXR
         END IF

C        Check If The Number of Elev & Flag Is Match
         IF (ELEV .AND. IEVSET.NE.0) THEN
            IF (ICOUNT*JCOUNT .NE. IZE) THEN
C              Write Out The Error Message: No. Of ELEV not match
               CALL ERRHDL(PATH,MODNAM,'E','218','ELEV')
            END IF
         END IF
         IF (FLGPOL .AND. IFGSET.NE.0) THEN
            IF (ICOUNT*JCOUNT .NE. IZF) THEN
C              Write Out The Error Message: No. Of FLAG not match
               CALL ERRHDL(PATH,MODNAM,'E','218','FLAG')
            END IF
         END IF

      ELSE
C        Error Message: Invalid Secondary Keyword
         CALL ERRHDL(PATH,MODNAM,'E','170',NETIDT)
         RECERR = .TRUE.
         GO TO 999

      END IF

      PNETID = NETIDT

 999  RETURN
      END

      SUBROUTINE GENCAR
C***********************************************************************
C                 GENCAR Module of ISC2 Model
C
C        PURPOSE: Generates Cartesian Grid Receptor Network With
C                 Uniform Spacing
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Cartesian Grid Receptor Network With Uniform
C                 Spacing
C
C        CALLED FROM:   RECART
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'
      DIMENSION TEMPP(6)
      LOGICAL ERROR

C     Variable Initializations
      MODNAM = 'GENCAR'
      ERROR = .FALSE.

C     Check for Location of Secondary Keyword, XYINC
      DO 41 I = 1, IFC
         IF (FIELD(I) .EQ. 'XYINC') THEN
            ISC = I + 1
         END IF
 41   CONTINUE

C     Determine Whether There Are Enough Parameter Fields
      IF (IFC .EQ. ISC-1) THEN
C        Error Message: Missing Parameter
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         RECERR = .TRUE.
         GO TO 999
      ELSE IF (IFC .GT. ISC+5) THEN
C        Error Message: Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KTYPE)
         RECERR = .TRUE.
         GO TO 999
      ELSE IF (IFC .LT. ISC+5) THEN
C        Error Message: Too Few Parameters
         CALL ERRHDL(PATH,MODNAM,'E','201',KTYPE)
         RECERR = .TRUE.
         GO TO 999
      END IF

C     Input The Numerical Values
      DO 21 K = 1,6
         CALL STONUM(FIELD(ISC + K-1),40,TEMPP(K),MITL)
C        Check The Numerical Field
         IF (MITL .EQ. -1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            ERROR = .TRUE.
            RECERR = .TRUE.
         END IF
 21   CONTINUE

      IF (ERROR) THEN
         ERROR = .FALSE.
         GO TO 999
      END IF

C     Assign Values to Appropriate Variables for Generated Network
      XINT   = TEMPP(1)
      ICOUNT = INT(TEMPP(2))
      XDELTA = TEMPP(3)
      YINT   = TEMPP(4)
      JCOUNT = INT(TEMPP(5))
      YDELTA = TEMPP(6)

C     Assign Them to the Coordinate Arrays
      IF (ICOUNT .LE. IXM) THEN
         DO 30 I = 1, ICOUNT
            XCOORD(I,INNET) = XINT + XDELTA*FLOAT(I-1)
 30      CONTINUE
      ELSE
C        WRITE Error Message:  Too Many X-Coordinates for This Network
         WRITE(DUMMY,'(I8)') IXM
         CALL ERRHDL(PATH,MODNAM,'E','225',DUMMY)
         RECERR = .TRUE.
      END IF
      IF (JCOUNT .LE. IYM) THEN
         DO 40 J = 1, JCOUNT
            YCOORD(J,INNET) = YINT + YDELTA*FLOAT(J-1)
 40      CONTINUE
      ELSE
C        WRITE Error Message:  Too Many Y-Coordinates for This Network
         WRITE(DUMMY,'(I8)') IYM
         CALL ERRHDL(PATH,MODNAM,'E','226',DUMMY)
         RECERR = .TRUE.
      END IF

 999  RETURN
      END

      SUBROUTINE XYPNTS
C***********************************************************************
C                 XYPNTS Module of ISC2 Model
C
C        PURPOSE: Processes Cartesian Grid x,y Input Value
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        MODIFIED:   To Fix Error Checking - Change Limit for DO 15
C                    To 'JSET -1' - 9/29/92
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Cartesian Grid x,y Input Value
C
C        CALLED FROM:   RECART
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'

C     Variable Initializations
      MODNAM = 'XYPNTS'

      IF (KTYPE .EQ. 'XPNTS') THEN
C        Check for Location of Secondary Keyword, XPNTS
         DO 41 I = 1, IFC
            IF (FIELD(I) .EQ. 'XPNTS') THEN
               ISC = I + 1
            END IF
 41      CONTINUE

C        Determine Whether There Are Enough Parameter Fields
         IF (IFC .EQ. ISC-1) THEN
C           Error Message: Missing Parameter
            CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
            RECERR = .TRUE.
            GO TO 999
         END IF

         ISET = ICOUNT
         DO 20 I = ISC, IFC
            CALL STONUM(FIELD(I),40,FNUM,IMIT)
C           Check The Numerical Field
            IF (IMIT .EQ. -1) THEN
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
               RECERR = .TRUE.
            END IF
            ISET = ISET + 1
            IF (ISET .LE. IXM) THEN
               XCOORD(ISET,INNET) = FNUM
               DO 10 J = 1, ISET-1
                  IF (FNUM .EQ. XCOORD(J,INNET)) THEN
C                    WRITE Warning Message:  X-Coord Specified More Than Once
                     CALL ERRHDL(PATH,MODNAM,'W','250',NETIDT)
                  END IF
 10            CONTINUE
            ELSE
C              WRITE Error Message:  Too Many X-Coordinates for This Network
               WRITE(DUMMY,'(I8)') IXM
               CALL ERRHDL(PATH,MODNAM,'E','225',DUMMY)
               RECERR = .TRUE.
            END IF
 20      CONTINUE
         ICOUNT = ISET

      ELSE IF (KTYPE .EQ. 'YPNTS') THEN
C        Check for Location of Secondary Keyword, YPNTS
         DO 42 I = 1, IFC
            IF (FIELD(I) .EQ. 'YPNTS') THEN
               ISC = I + 1
            END IF
 42      CONTINUE

C        Determine Whether There Are Enough Parameter Fields
         IF (IFC .EQ. ISC-1) THEN
C           Error Message: Missing Parameter
            CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
            RECERR = .TRUE.
            GO TO 999
         END IF

         JSET = JCOUNT

         DO 25 I = ISC, IFC
            CALL STONUM(FIELD(I),40,FNUM,IMIT)
C           Check The Numerical Field
            IF (IMIT .EQ. -1) THEN
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
               RECERR = .TRUE.
            END IF
            JSET = JSET + 1
            IF (JSET .LE. IYM) THEN
               YCOORD(JSET,INNET) = FNUM
               DO 15 J = 1, JSET-1
                  IF (FNUM .EQ. YCOORD(J,INNET)) THEN
C                    WRITE Warning Message:  Y-Coord Specified More Than Once
                     CALL ERRHDL(PATH,MODNAM,'W','250',NETIDT)
                  END IF
 15            CONTINUE
            ELSE
C              WRITE Error Message:  Too Many Y-Coordinates for This Network
               WRITE(DUMMY,'(I8)') IYM
               CALL ERRHDL(PATH,MODNAM,'E','226',DUMMY)
               RECERR = .TRUE.
            END IF
 25      CONTINUE
         JCOUNT = JSET
      END IF

 999  RETURN
      END

      SUBROUTINE SETCAR
C***********************************************************************
C                 SETCAR Module of ISC2 Model
C
C        PURPOSE: Setup the Final Cartesian Grid Receptor Network Inputs
C
C        PROGRAMMER:  Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        MODIFIED:  To Include TOXXFILE Option - 9/29/92
C
C        INPUTS:  The GRIDCART Sub-pathway Input Parameters
C
C        OUTPUTS: Cartesian Grid Receptor Network Inputs
C
C        CALLED FROM:   RECART
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'

C     Variable Initializations
      MODNAM = 'SETCAR'

      IF (ICOUNT.NE.0 .AND. JCOUNT.NE.0) THEN
C        Setup The Coordinate Of The Receptors
         NETSTA(INNET) = IRXR + 1
         ISET = IRXR
         JSET = IRYR
         DO 25 J = 1, JCOUNT
            DO 20 I = 1, ICOUNT
               ISET = ISET + 1
               JSET = JSET + 1
               IF (ISET .GT. NREC) THEN
C                 Error Msg: Maximum Number Of Receptor Exceeded
                  WRITE(DUMMY,'(I8)') NREC
                  CALL ERRHDL(PATH,MODNAM,'E','219',DUMMY)
                  GO TO 999
               END IF
               IF (ICOUNT .GT. IXM) THEN
C                 WRITE Error Message:  Too Many X-Coordinates for This Network
                  WRITE(DUMMY,'(I8)') IXM
                  CALL ERRHDL(PATH,MODNAM,'E','225',DUMMY)
                  GO TO 999
               END IF
               IF (JCOUNT .GT. IYM) THEN
C                 WRITE Error Message:  Too Many Y-Coordinates for This Network
                  WRITE(DUMMY,'(I8)') IYM
                  CALL ERRHDL(PATH,MODNAM,'E','226',DUMMY)
                  GO TO 999
               END IF
               AXR(ISET) = XCOORD(I,INNET)
               AYR(JSET) = YCOORD(J,INNET)
 20         CONTINUE
 25      CONTINUE
         IRXR = ISET
         IRYR = JSET
         NETEND(INNET) = IRXR
         NUMXPT(INNET) = ICOUNT
         NUMYPT(INNET) = JCOUNT
         NTID(INNET)   = NETIDT
         NTTYP(INNET)  = 'GRIDCART'
C        Define ITAB, NXTOX, NYTOX Variables for TOXXFILE Option, 9/29/92
         IF (ITAB .LT. 0) THEN
C           First Receptor Network Defined - Set Variables
            ITAB  = 2
            NXTOX = ICOUNT
            NYTOX = JCOUNT
         ELSE
C           Previous Receptors Have Been Defined - Reset ITAB = 0
            ITAB = 0
         END IF
      END IF

C     Setup The AZELEV Array
      CALL SBYVAL(ZETMP1,ZETMP2,IZE)
      ISET = IRZE
      DO 30 I = 1, IZE
         ISET = ISET + 1
         IF (ISET .GT. NREC) THEN
C           Error Msg: Maximum Number Of Receptor Exceeded
            WRITE(DUMMY,'(I8)') NREC
            CALL ERRHDL(PATH,MODNAM,'E','219',DUMMY)
            GO TO 999
         END IF
         AZELEV(ISET) = ZETMP2(I)
 30   CONTINUE
      IRZE = ISET

C     Setup The AZFLAG Aarry
      CALL SBYVAL(ZFTMP1,ZFTMP2,IZF)
      ISET = IRZF
      DO 35 I = 1, IZF
         ISET = ISET + 1
         IF (ISET .GT. NREC) THEN
C           Error Msg: Maximum Number Of Receptor Exceeded
            WRITE(DUMMY,'(I8)') NREC
            CALL ERRHDL(PATH,MODNAM,'E','219',DUMMY)
            GO TO 999
         END IF
         AZFLAG(ISET) = ZFTMP2(I)
 35   CONTINUE
      IRZF = ISET

      DO 40 I = IDC1+1, IRXR
         NETID(I) = NETIDT
         RECTYP(I) = 'GC'
 40   CONTINUE

 999  RETURN
      END

      SUBROUTINE REPOLR
C***********************************************************************
C                 REPOLR Module of ISC2 Model
C
C        PURPOSE: Processes Polar Grid Receptor Network Inputs
C
C        PROGRAMMER:  Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Polar Receptor Network Inputs
C
C        CALLED FROM:   RECARD
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'

C     Variable Initializations
      MODNAM = 'REPOLR'

      IF (IFC .LT. 3) THEN
C        Write Error Message: Missing Data Field
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      END IF

C     READ in the Netid and Nettype
      NETIDT = FIELD(3)
      IF (.NOT.NEWID .AND. (NETIDT.EQ.'    ' .OR.
     &    NETIDT.EQ.'ORIG' .OR. NETIDT.EQ.'DIST' .OR.
     &    NETIDT.EQ.'DDIR' .OR. NETIDT.EQ.'ELEV' .OR.
     &    NETIDT.EQ.'FLAG' .OR. NETIDT.EQ.'GDIR' .OR.
     &    NETIDT.EQ.'END')) THEN
         NETIDT = PNETID
         KTYPE = FIELD(3)
      ELSE IF (.NOT.NEWID .AND. NETIDT.EQ.PNETID) THEN
         KTYPE = FIELD(4)
      ELSE IF (NEWID .AND. NETIDT.NE.'    ') THEN
         NEWID = .FALSE.
         KTYPE = FIELD(4)
C        The Keyword Counter
         INNET = INNET + 1
         IF (INNET .GT. NNET) THEN
C           WRITE Error Message:  Too Many Networks
            WRITE(DUMMY,'(I8)') NNET
            CALL ERRHDL(PATH,MODNAM,'E','224',DUMMY)
            RECERR = .TRUE.
            GO TO 999
         END IF
         IORSET = 0
         IXRSET = 0
         IDRSET = 0
         IGRSET = 0
         IEVSET = 0
         IFGSET = 0
      ELSE
C        Error Message: Invalid Secondary Keyword
         CALL ERRHDL(PATH,MODNAM,'E','170',PNETID)
         RECERR = .TRUE.
         GO TO 999
      END IF

C     Start to Set Up the Network
      IF (KTYPE .EQ. 'STA') THEN
         ISTA = .TRUE.
         IEND = .FALSE.
         NEWID = .FALSE.
         RECERR = .FALSE.
         ICOUNT = 0
         JCOUNT = 0
         IZE = 0
         IZF = 0
         IDC1 = IRXR
C        Check for Previous Grid Network With Same ID
         DO 100 I = 1, INNET-1
            IF (FIELD(3) .EQ. NTID(I)) THEN
C              WRITE Warning Message:  Duplicate Network ID
               CALL ERRHDL(PATH,MODNAM,'W','252',NTID(I))
            END IF
 100     CONTINUE
      ELSE IF (KTYPE .EQ. 'ORIG') THEN
C        Error Message: Conflict Secondary Keyword
         IF (IORSET .NE. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','160',NETIDT)
         END IF
C        Read In XINT, YINT                                 ---   CALL POLORG
         CALL POLORG
         IORSET = IORSET + 1
      ELSE IF (KTYPE .EQ. 'DIST') THEN
C        Read in the Distance Set                           ---   CALL POLDST
         CALL POLDST
         IXRSET = IXRSET + 1
      ELSE IF (KTYPE .EQ. 'GDIR') THEN
C        Error Message: Conflict Secondary Keyword
         IF (IDRSET .NE. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','180',NETIDT)
         END IF
C        Set the Uniform Spacing Receptor Network           ---   CALL GENPOL
         CALL GENPOL
         IGRSET = IGRSET + 1
      ELSE IF (KTYPE .EQ. 'DDIR') THEN
C        Error Message: Conflict Secondary Keyword
         IF (IGRSET .NE. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','180',NETIDT)
         END IF
C        Set the Non-uniform Spacing Receptor Network       ---   CALL RADRNG
         CALL RADRNG
         IDRSET = IDRSET + 1
      ELSE IF (KTYPE .EQ. 'ELEV') THEN
C        Read in and set the Terrain Elevation              ---   CALL TERHGT
         CALL TERHGT
         IEVSET = IEVSET + 1
      ELSE IF (KTYPE .EQ. 'FLAG') THEN
C        Read in and set the Flagpole Receptor              ---   CALL FLGHGT
         CALL FLGHGT
         IFGSET = IFGSET + 1
      ELSE IF (KTYPE .EQ. 'END') THEN
         IEND = .TRUE.
C        Get the Final Result
         IF (.NOT. ISTA) THEN
C           Write Error: MISSING STA OF THE BLOCK DATA
            CALL ERRHDL(PATH,MODNAM,'E','200','STA')
         ELSE IF (.NOT. RECERR) THEN
            CALL SETPOL
         END IF
         ISTA = .FALSE.
         NEWID = .TRUE.
C        Check If The Secondary Parameter Has Been Specified
C        Warning Message: Missing (Xin,Yin) Point Setting
         IF (IORSET .EQ. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'W','220',NETIDT)
            XINT = 0.0
            YINT = 0.0
         END IF
C        Error Message: Missing Distance Point Setting
         IF (IXRSET .EQ. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','221',NETIDT)
         END IF
C        Error Message: Missing Degree Or Rad Setting
         IF (IGRSET.EQ.0 .AND. IDRSET.EQ.0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','222',NETIDT)
         END IF

C        Warning: Elevated Terrain Inputs Inconsistent With Options
         IF (ELEV .AND. IEVSET.EQ.0) THEN
            CALL ERRHDL(PATH,MODNAM,'W','214',NETIDT)
            IRZE = IRXR
         ELSE IF (FLAT .AND. IEVSET.NE.0) THEN
            CALL ERRHDL(PATH,MODNAM,'W','213',NETIDT)
            IRZE = IRXR
         ELSE IF (FLAT .AND. IEVSET.EQ.0) THEN
            IRZE = IRXR
         END IF

C        Warning: Flagpole Receptor Inputs Inconsistent With Options
         IF (FLGPOL .AND. IFGSET.EQ.0) THEN
            CALL ERRHDL(PATH,MODNAM,'W','216',NETIDT)
            IRZF = IRXR
         ELSE IF (.NOT.FLGPOL .AND. IFGSET.NE.0) THEN
            CALL ERRHDL(PATH,MODNAM,'W','215',NETIDT)
            IRZF = IRXR
         ELSE IF (.NOT.FLGPOL .AND. IFGSET.EQ.0) THEN
            IRZF = IRXR
         END IF

C        Check If The Number of Elev & Flag Is Match
         IF (ELEV .AND. IEVSET.NE.0) THEN
            IF (ICOUNT*JCOUNT .NE. IZE) THEN
C              Write Out The Error Message: No. Of ELEV not match
               CALL ERRHDL(PATH,MODNAM,'E','218','ELEV')
            END IF
         END IF
         IF (FLGPOL .AND. IFGSET.NE.0) THEN
            IF (ICOUNT*JCOUNT .NE. IZF) THEN
C              Write Out The Error Message: No. Of FLAG not match
               CALL ERRHDL(PATH,MODNAM,'E','218','FLAG')
            END IF
         END IF

      ELSE
C        Error Message: Invalid Secondary Keyword
         CALL ERRHDL(PATH,MODNAM,'E','170',NETIDT)
         RECERR = .TRUE.
         GO TO 999

      END IF

      PNETID = NETIDT

 999  RETURN
      END

      SUBROUTINE POLORG
C***********************************************************************
C                 POLORG Module of ISC2 Model
C
C        PURPOSE: Input The Original of The Polar Network
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Polar Network Origin  Coordinates
C
C        CALLED FROM:   REPOLR
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'
      CHARACTER SOID*8
      LOGICAL FIND

C     Variable Initializations
      MODNAM = 'POLORG'
      FIND = .FALSE.

C     Check for the Location of the Secondary Keyword, ORIG
      DO 41 I = 1, IFC
         IF (FIELD(I) .EQ. 'ORIG') THEN
            ISC = I + 1
         END IF
 41   CONTINUE

C     Determine Whether There Are Enough Parameter Fields
      IF (IFC .EQ. ISC-1) THEN
C        Error Message: Missing Parameter
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         RECERR = .TRUE.
         GO TO 999
      ELSE IF (IFC .GT. ISC+1) THEN
C        Error Message: Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KTYPE)
         RECERR = .TRUE.
         GO TO 999
      END IF

      IF (IFC .EQ. ISC) THEN
C        Identify Origin Associated With a Source ID
         SOID = FIELD(ISC)
         CALL SINDEX(SRCID,NSRC,SOID,ISDX,FIND)
         IF (.NOT. FIND) THEN
C           Error Message: Source ID Does Not Match Existing Sources
            CALL ERRHDL(PATH,MODNAM,'E','300',KEYWRD)
            RECERR = .TRUE.
         ELSE
            XINT = AXS(ISDX)
            YINT = AYS(ISDX)
         END IF

      ELSE
C        Input Numerical Values, XINT and YINT
         CALL STONUM(FIELD(ISC),40,XINT,IMUT)
C        Check The Numerical Field
         IF (IMUT .EQ. -1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            RECERR = .TRUE.
         END IF

         CALL STONUM(FIELD(ISC + 1),40,YINT,IMUT)
C        Check The Numerical Field
         IF (IMUT .EQ. -1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            RECERR = .TRUE.
         END IF
      END IF

 999  RETURN
      END

      SUBROUTINE POLDST
C***********************************************************************
C                 POLDST Module of ISC2 Model
C
C        PURPOSE: Gets Distances for the Polar Network
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Polar Network Distance Input Value
C
C        CALLED FROM:   REPOLR
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'

C     Variable Initializations
      MODNAM = 'POLDST'

C     Skip the Unrelated Fields
      DO 41 I = 1, IFC
         IF (FIELD(I) .EQ. 'DIST') THEN
            ISC = I + 1
         END IF
 41   CONTINUE

C     Determine Whether There Are Enough Parameter Fields
      IF (IFC .EQ. ISC-1) THEN
C        Error Message: Missing Parameter
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         RECERR = .TRUE.
         GO TO 999
      END IF

      ISET = ICOUNT

      DO 20 I = ISC, IFC
         CALL STONUM(FIELD(I),40,FNUM,IMIT)
C        Check The Numerical Field
         IF (IMIT .EQ. -1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            RECERR = .TRUE.
         END IF
         ISET = ISET + 1
         IF (ISET .LE. IXM) THEN
C           Store Distance to XCOORD Array and Check for Previous Occurrence
            XCOORD(ISET,INNET) = FNUM
            DO 10 J = 1, ISET-1
               IF (FNUM .EQ. XCOORD(J,INNET)) THEN
C                 WRITE Warning Message:  Distance Specified More Than Once
                  CALL ERRHDL(PATH,MODNAM,'W','250',NETIDT)
               END IF
 10         CONTINUE
         ELSE
C           WRITE Error Message:  Too Many X-Coordinates for This Network
            WRITE(DUMMY,'(I8)') IXM
            CALL ERRHDL(PATH,MODNAM,'E','225',DUMMY)
            RECERR = .TRUE.
         END IF
 20   CONTINUE

      ICOUNT = ISET

 999  RETURN
      END

      SUBROUTINE GENPOL
C***********************************************************************
C                 GENPOL Module of ISC2 Model
C
C        PURPOSE: Generates Polar Receptor Network With
C                 Uniform Spacing
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Polar Receptor Network With Uniform Direction Spacing
C
C        CALLED FROM:   REPOLR
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'
      DIMENSION TEMPP(3)
      LOGICAL ERROR

C     Variable Initializations
      MODNAM = 'GENPOL'
      ERROR = .FALSE.

C     Check for the Location of the Secondary Keyword, GDIR
      DO 41 I = 1, IFC
         IF (FIELD(I) .EQ. 'GDIR') THEN
            ISC = I + 1
         END IF
 41   CONTINUE

C     Determine Whether There Are Enough Parameter Fields
      IF (IFC .EQ. ISC-1) THEN
C        Error Message: Missing Parameter
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         RECERR = .TRUE.
         GO TO 999
      ELSE IF (IFC .LT. ISC+2) THEN
C        Error Message: Not Enough Parameters
         CALL ERRHDL(PATH,MODNAM,'E','201',KTYPE)
         RECERR = .TRUE.
         GO TO 999
      ELSE IF (IFC .GT. ISC+2) THEN
C        Error Message: Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KTYPE)
         RECERR = .TRUE.
         GO TO 999
      END IF

C     Input Numerical Values
      DO 21 K = 1, 3
         CALL STONUM(FIELD(ISC + K-1),40,TEMPP(K),MITL)
C        Check The Numerical Field
         IF (MITL .EQ. -1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            RECERR = .TRUE.
            ERROR = .TRUE.
         END IF
 21   CONTINUE

      IF (ERROR) THEN
         ERROR = .FALSE.
         GO TO 999
      END IF

      JCOUNT = INT(TEMPP(1))
      DIRINI = TEMPP(2)
      DIRINC = TEMPP(3)

C     Assign Them to the Coordinate Arrays
      IF (JCOUNT .LE. IYM) THEN
         DO 20 J = 1, JCOUNT
            YCOORD(J,INNET) = (DIRINI + DIRINC*FLOAT(J-1))
            IF (YCOORD(J,INNET) .GT. 360.) THEN
               YCOORD(J,INNET) = YCOORD(J,INNET) - 360.
            ELSE IF (YCOORD(J,INNET) .LE. 0.) THEN
               YCOORD(J,INNET) = YCOORD(J,INNET) + 360.
            END IF
 20      CONTINUE
      ELSE
C        WRITE Error Message:  Too Many Y-Coordinates for This Network
         WRITE(DUMMY,'(I8)') IYM
         CALL ERRHDL(PATH,MODNAM,'E','226',DUMMY)
         RECERR = .TRUE.
      END IF

 999  RETURN
      END

      SUBROUTINE RADRNG
C***********************************************************************
C                 RADRNG Module of ISC2 Model
C
C        PURPOSE: Processes Non-Uniform Polar Network Value
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Polar Network Directions in Non-Uniform Spacing
C
C        CALLED FROM:   REPOLR
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'

C     Variable Initializations
      MODNAM = 'RADRNG'

C     Skip the non-useful Fields
      DO 41 I = 1, IFC
         IF (FIELD(I) .EQ. 'DDIR') THEN
            ISC = I + 1
         END IF
 41   CONTINUE

C     Determine Whether There Are Enough Parameter Fields
      IF (IFC .EQ. ISC-1) THEN
C        Error Message: Missing Parameter
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         RECERR = .TRUE.
         GO TO 999
      END IF

      ISET = JCOUNT

      DO 20 I = ISC, IFC
         CALL STONUM(FIELD(I),40,FNUM,IMIT)
C        Check The Numerical Field
         IF (IMIT .EQ. -1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            RECERR = .TRUE.
         END IF
         ISET = ISET + 1
         IF (ISET .LE. IYM) THEN
C           Store Direction to YCOORD Array, Adjust to 0-360 Range if Needed,
C           and Check for Previous Occurrence
            YCOORD(ISET,INNET) = FNUM
            IF (YCOORD(ISET,INNET) .GT. 360.) THEN
               YCOORD(ISET,INNET) = YCOORD(ISET,INNET) - 360.
            ELSE IF (YCOORD(ISET,INNET) .LE. 0.) THEN
               YCOORD(ISET,INNET) = YCOORD(ISET,INNET) + 360.
            END IF
            DO 10 J = 1, ISET-1
               IF (FNUM .EQ. YCOORD(J,INNET)) THEN
C                 WRITE Warning Message:  Direction Specified More Than Once
                  CALL ERRHDL(PATH,MODNAM,'W','250',NETIDT)
               END IF
 10         CONTINUE
         ELSE
C           WRITE Error Message:  Too Many Y-Coordinates for This Network
            WRITE(DUMMY,'(I8)') IYM
            CALL ERRHDL(PATH,MODNAM,'E','226',DUMMY)
            RECERR = .TRUE.
         END IF
 20   CONTINUE

      JCOUNT = ISET

 999  RETURN
      END

      SUBROUTINE SETPOL
C***********************************************************************
C                 SETPOL Module of ISC2 Model
C
C        PURPOSE: Setup the Final Polar Receptor Network Inputs
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        MODIFIED:  To Include TOXXFILE Option - 9/29/92
C
C        INPUTS:  The GRIDPOLR Sub-pathway Input Parameters
C
C        OUTPUTS: Polar Receptor Network Arrays
C
C        CALLED FROM:   REPOLR
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'

C     Variable Initializations
      MODNAM = 'SETPOL'

      IF (ICOUNT.NE.0 .AND. JCOUNT.NE.0) THEN
C        Setup The Coordinate Of The Receptors
         NETSTA(INNET) = IRXR + 1
         ISET = IRXR
         JSET = IRYR
         DO 25 J = 1, JCOUNT
            DO 20 I = 1, ICOUNT
               ISET = ISET + 1
               JSET = JSET + 1
               IF (ISET .GT. NREC) THEN
C                 Error Msg: Maximum Number Of Receptor Exceeded
                  WRITE(DUMMY,'(I8)') NREC
                  CALL ERRHDL(PATH,MODNAM,'E','219',DUMMY)
                  GO TO 999
               END IF
               IF (ICOUNT .GT. IXM) THEN
C                 WRITE Error Message:  Too Many X-Coordinates for This Network
                  WRITE(DUMMY,'(I8)') IXM
                  CALL ERRHDL(PATH,MODNAM,'E','225',DUMMY)
                  GO TO 999
               END IF
               IF (JCOUNT .GT. IYM) THEN
C                 WRITE Error Message:  Too Many Y-Coordinates for This Network
                  WRITE(DUMMY,'(I8)') IYM
                  CALL ERRHDL(PATH,MODNAM,'E','226',DUMMY)
                  GO TO 999
               END IF
               YTEMP = YCOORD(J,INNET) * DTORAD
               AXR(ISET) = XINT + XCOORD(I,INNET)*SIN(YTEMP)
               AYR(JSET) = YINT + XCOORD(I,INNET)*COS(YTEMP)
 20         CONTINUE
 25      CONTINUE
         IRXR = ISET
         IRYR = JSET
         XORIG(INNET)  = XINT
         YORIG(INNET)  = YINT
         NETEND(INNET) = IRXR
         NUMXPT(INNET) = ICOUNT
         NUMYPT(INNET) = JCOUNT
         NTID(INNET)   = NETIDT
         NTTYP(INNET)  = 'GRIDPOLR'
C        Define ITAB, NXTOX, NYTOX Variables for TOXXFILE Option, 9/29/92
         IF (ITAB .LT. 0) THEN
C           First Receptor Network Defined - Set Variables
            ITAB  = 1
            NXTOX = ICOUNT
            NYTOX = JCOUNT
         ELSE
C           Previous Receptors Have Been Defined - Reset ITAB = 0
            ITAB = 0
         END IF
      END IF

C     Setup The AZELEV Array
      CALL SBYVAL(ZETMP1,ZETMP2,IZE)
      ISET = IRZE
      DO 30 I = 1, IZE
         ISET = ISET + 1
         IF (ISET .GT. NREC) THEN
C           Error Msg: Maximum Number Of Receptor Exceeded
            WRITE(DUMMY,'(I8)') NREC
            CALL ERRHDL(PATH,MODNAM,'E','219',DUMMY)
            GO TO 999
         END IF
         AZELEV(ISET) = ZETMP2(I)
 30   CONTINUE
      IRZE = ISET

C     Setup The AZFLAG Array
      CALL SBYVAL(ZFTMP1,ZFTMP2,IZF)
      ISET = IRZF
      DO 35 I = 1, IZF
         ISET = ISET + 1
         IF (ISET .GT. NREC) THEN
C           Error Msg: Maximum Number Of Receptor Exceeded
            WRITE(DUMMY,'(I8)') NREC
            CALL ERRHDL(PATH,MODNAM,'E','219',DUMMY)
            GO TO 999
         END IF
         AZFLAG(ISET) = ZFTMP2(I)
 35   CONTINUE
      IRZF = ISET

      DO 40 I = IDC1+1, IRXR
         NETID(I) = NETIDT
         RECTYP(I) = 'GP'
 40   CONTINUE

 999  RETURN
      END

      SUBROUTINE TERHGT
C***********************************************************************
C                 TERHGT Module of ISC2 Model
C
C        PURPOSE: Processes Elevated Terrain Inputs for Receptor Network
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Elevated Terrain Input for a Receptor Network
C
C        CALLED FROM:   RECART
C                       REPOLR
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'

C     Variable Initializations
      MODNAM = 'TERHGT'
      IZE1 = IZE + 1

C     Check for the Location of the Secondary Keyword, ELEV
      DO 10 I = 1, IFC
         IF (FIELD(I) .EQ. 'ELEV') THEN
            ISC = I + 1
         END IF
 10   CONTINUE

C     Determine Whether There Are Enough Parameter Fields
      IF (IFC .EQ. ISC-1) THEN
C        Error Message: Missing Parameter
         CALL ERRHDL(PATH,MODNAM,'E','223',KTYPE)
         RECERR = .TRUE.
         GO TO 999
      ELSE IF (IFC .EQ. ISC) THEN
C        Error Message: Missing Numerical Field
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         RECERR = .TRUE.
         GO TO 999
      END IF

      CALL STONUM(FIELD(ISC),40,FNUM,IMIT)
C     Check The Numerical Field
      IF (IMIT .EQ. -1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         RECERR = .TRUE.
      END IF
      ROW = FNUM

      ISET = IZE

      DO 25 I = ISC+1, IFC
         CALL STONUM(FIELD(I),40,FNUM,IMIT)
C        Check The Numerical Field
         IF (IMIT .EQ. -1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            RECERR = .TRUE.
         END IF
         DO 20 J = 1, IMIT
            ISET = ISET + 1
            ZETMP1(ISET) = ROW
            ZETMP2(ISET) = FNUM
 20      CONTINUE
 25   CONTINUE

      IZE = ISET

      IF (ELTYPE .EQ. 'FEET' .OR. REELEV .EQ. 'FEET') THEN
C        Convert ELEV to Metric System
         DO 50 I = IZE1, IZE
            ZETMP2(I) = 0.3048*ZETMP2(I)
 50      CONTINUE
      END IF

 999  RETURN
      END

      SUBROUTINE FLGHGT
C***********************************************************************
C                 FLGHGT Module of ISC2 Model
C
C        PURPOSE: Processes Flagpole Receptor Heights for Receptor Network
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Flagpole Receptor Heights for a Receptor Network
C
C        CALLED FROM:   RECART
C                       REPOLR
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'

C     Variable Initializations
      MODNAM = 'FLGHGT'

C     Check for the Location of the Secondary Keyword, FLAG
      DO 10 I = 1, IFC
         IF (FIELD(I) .EQ. 'FLAG') THEN
            ISC = I + 1
         END IF
 10   CONTINUE

C     Determine Whether There Are Enough Parameter Fields
      IF (IFC .EQ. ISC-1) THEN
C        Error Message: Missing Parameter
         CALL ERRHDL(PATH,MODNAM,'E','223',KTYPE)
         RECERR = .TRUE.
         GO TO 999
      ELSE IF (IFC .EQ. ISC) THEN
C        Error Message: Missing Numerical Field
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         RECERR = .TRUE.
         GO TO 999
      END IF

      CALL STONUM(FIELD(ISC),40,FNUM,IMIT)
C     Check The Numerical Field
      IF (IMIT .EQ. -1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         RECERR = .TRUE.
      END IF
      ROW = FNUM

      ISET = IZF

      DO 25 I = ISC+1, IFC
         CALL STONUM(FIELD(I),40,FNUM,IMIT)
C        Check The Numerical Field
         IF (IMIT .EQ. -1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            RECERR = .TRUE.
         END IF
         DO 20 J = 1, IMIT
            ISET = ISET + 1
            ZFTMP1(ISET) = ROW
            ZFTMP2(ISET) = FNUM
 20      CONTINUE
 25   CONTINUE

      IZF = ISET

 999  RETURN
      END

      SUBROUTINE DISCAR
C***********************************************************************
C                 DISCAR Module of ISC2 Model
C
C        PURPOSE: Processes Discrete Cartesian Receptor Location Inputs
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        MODIFIED:  To Include TOXXFILE Option - 9/29/92
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Discrete Cartesian Receptor Location Inputs
C
C        CALLED FROM:   RECARD
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'

C     Variable Initializations
      MODNAM = 'DISCAR'
      I1 = IRXR
      I2 = IRYR
      I3 = IRZE
      I4 = IRZF

C     Determine Whether There Are Too Few Or Too Many Parameter Fields
      IF (IFC .LT. 4) THEN
C        WRITE Error Message: Missing Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      ELSE IF (IFC .GT. 6) THEN
C        Error Message: Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
         GO TO 999
      ELSE IF (ELEV .AND. FLGPOL .AND. IFC.LT.6) THEN
C        WRITE Warning Message: Default(s) Used for Missing Parameter(s)
         CALL ERRHDL(PATH,MODNAM,'W','228',KEYWRD)
      ELSE IF ((ELEV .OR. FLGPOL) .AND. IFC.LT.5) THEN
C        WRITE Warning Message: Default(s) Used for Missing Parameter(s)
         CALL ERRHDL(PATH,MODNAM,'W','228',KEYWRD)
      ELSE IF (ELEV .AND. .NOT.FLGPOL .AND. IFC .GT. 5) THEN
C        WRITE Warning Message: Parameter Ignored, ZFLAG
         CALL ERRHDL(PATH,MODNAM,'W','229',KEYWRD)
      ELSE IF (FLGPOL .AND. .NOT.ELEV .AND. IFC .GT. 5) THEN
C        WRITE Warning Message: Parameter Ignored, ZELEV
         CALL ERRHDL(PATH,MODNAM,'W','229',KEYWRD)
      ELSE IF (.NOT.ELEV .AND. .NOT.FLGPOL .AND. IFC .GT. 4) THEN
C        WRITE Warning Message: Parameters Ignored, ZELEV & ZFLAG
         CALL ERRHDL(PATH,MODNAM,'W','229',KEYWRD)
      END IF

C     Check Whether The Maximum Number of Receptors is Exceeded
      IF (I1.EQ.NREC .OR. I2.EQ.NREC .OR. I3.EQ.NREC .OR.
     &                                    I4.EQ.NREC) THEN
C        Error Msg: Maximum Number Of Receptors Exceeded
         WRITE(DUMMY,'(I8)') NREC
         CALL ERRHDL(PATH,MODNAM,'E','219',DUMMY)
         GO TO 999
      END IF

C     READ XCOORD,YCOORD,ELEV,FLAG And Assign Them to Different Arrays

      CALL STONUM(FIELD(3),40,FNUM,INUM)
C     Check The Numerical Field
      IF (INUM .EQ. -1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
      ELSE
         AXR(I1 + 1) = FNUM
      END IF

      CALL STONUM(FIELD(4),40,FNUM,INUM)
C     Check The Numerical Field
      IF (INUM .EQ. -1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
      ELSE
         AYR(I2 + 1) = FNUM
      END IF

      IF (ELEV .AND. FLGPOL) THEN
         IF (IFC .GE. 5) THEN
            CALL STONUM(FIELD(5),40,FNUM,INUM)
C           Check The Numerical Field
            IF (INUM .EQ. -1) THEN
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            ELSE
               AZELEV(I3 + 1) = FNUM
            END IF
         END IF
         IF (IFC .EQ. 6) THEN
            CALL STONUM(FIELD(6),40,FNUM,INUM)
C           Check The Numerical Field
            IF (INUM .EQ. -1) THEN
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            ELSE
               AZFLAG(I4 + 1) = FNUM
            END IF
         END IF
      ELSE IF (ELEV .AND. .NOT.FLGPOL) THEN
         IF (IFC .GE. 5) THEN
            CALL STONUM(FIELD(5),40,FNUM,INUM)
C           Check The Numerical Field
            IF (INUM .EQ. -1) THEN
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            ELSE
               AZELEV(I3 + 1) = FNUM
            END IF
         END IF
      ELSE IF (FLGPOL .AND. .NOT.ELEV) THEN
         IF (IFC .EQ. 5) THEN
            CALL STONUM(FIELD(5),40,FNUM,INUM)
C           Check The Numerical Field
            IF (INUM .EQ. -1) THEN
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            ELSE
               AZFLAG(I4 + 1) = FNUM
            END IF
         ELSE IF (IFC .EQ. 6) THEN
            CALL STONUM(FIELD(6),40,FNUM,INUM)
C           Check The Numerical Field
            IF (INUM .EQ. -1) THEN
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            ELSE
               AZFLAG(I4 + 1) = FNUM
            END IF
         END IF
      END IF

      IF (ELTYPE .EQ. 'FEET' .OR. REELEV .EQ. 'FEET') THEN
C        Convert ELEV to Metric system
         AZELEV(I3 + 1) = 0.3048*AZELEV(I3 + 1)
      END IF

      IRXR = I1 + 1
      IRYR = I2 + 1
      IRZE = I3 + 1
      IRZF = I4 + 1
      NETID(IRXR) = '   NA   '
      RECTYP(IRXR) = 'DC'
C     Reset ITAB Variable for TOXXFILE Option, 9/29/92
      ITAB = 0

 999  RETURN
      END

      SUBROUTINE DISPOL
C***********************************************************************
C                 DISPOL Module of ISC2 Model
C
C        PURPOSE: Processes Discrete Polar Receptor Location Inputs
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        MODIFIED:  To Include TOXXFILE Option - 9/29/92
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Discrete Polar Receptor Location Inputs
C
C        CALLED FROM:   RECARD
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'
      CHARACTER SOID*8
      LOGICAL FIND

C     Variable Initializations
      MODNAM = 'DISPOL'
      I1 = IRXR
      I2 = IRYR
      I3 = IRZE
      I4 = IRZF

C     Determine Whether There Are Too Few Or Too Many Parameter Fields
      IF (IFC .LT. 5) THEN
C        WRITE Error Message: Missing Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      ELSE IF (IFC .GT. 7) THEN
C        Error Message: Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
         GO TO 999
      ELSE IF (ELEV .AND. FLGPOL .AND. IFC.LT.7) THEN
C        WRITE Warning Message: Default(s) Used for Missing Parameter(s)
         CALL ERRHDL(PATH,MODNAM,'W','228',KEYWRD)
      ELSE IF ((ELEV .OR. FLGPOL) .AND. IFC.LT.6) THEN
C        WRITE Warning Message: Default(s) Used for Missing Parameter(s)
         CALL ERRHDL(PATH,MODNAM,'W','228',KEYWRD)
      ELSE IF (ELEV .AND. .NOT.FLGPOL .AND. IFC .GT. 6) THEN
C        WRITE Warning Message: Parameter Ignored, ZFLAG
         CALL ERRHDL(PATH,MODNAM,'W','229',' ZFLAG ')
      ELSE IF (FLGPOL .AND. .NOT.ELEV .AND. IFC .GT. 6) THEN
C        WRITE Error Message: Parameter Ignored, ZELEV
         CALL ERRHDL(PATH,MODNAM,'W','229',KEYWRD)
      ELSE IF (.NOT.ELEV .AND. .NOT.FLGPOL .AND. IFC .GT. 5) THEN
C        WRITE Warning Message: Parameters Ignored, ZELEV & ZFLAG
         CALL ERRHDL(PATH,MODNAM,'W','229',KEYWRD)
      END IF

C     Check Whether The Maximum Number of Receptors is Exceeded
      IF (I1.EQ.NREC .OR. I2.EQ.NREC .OR. I3.EQ.NREC .OR.
     &                                    I4.EQ.NREC) THEN
C        Error Msg: Maximum Number Of Receptors Exceeded
         WRITE(DUMMY,'(I8)') NREC
         CALL ERRHDL(PATH,MODNAM,'E','219',DUMMY)
         GO TO 999
      END IF

C     READ SRCID,RANGE,DIRECT,ELEV,FLAG

      SOID = FIELD(3)

      CALL STONUM(FIELD(4),40,RANGE,IMIT)
C     Check The Numerical Field
      IF (IMIT .EQ. -1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
      END IF

      CALL STONUM(FIELD(5),40,DIRECT,IMIT)
C     Check The Numerical Field
      IF (IMIT .EQ. -1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
      ELSE IF (DIRECT .GT. 360.) THEN
         DIRECT = DIRECT - 360.
      ELSE IF (DIRECT .LE. 0.) THEN
         DIRECT = DIRECT + 360.
      END IF

      IF (ELEV .AND. FLGPOL) THEN
         IF (IFC .GE. 6) THEN
            CALL STONUM(FIELD(6),40,FNUM,INUM)
C           Check The Numerical Field
            IF (INUM .EQ. -1) THEN
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            ELSE
               AZELEV(I3 + 1) = FNUM
            END IF
         END IF
         IF (IFC .EQ. 7) THEN
            CALL STONUM(FIELD(7),40,FNUM,INUM)
C           Check The Numerical Field
            IF (INUM .EQ. -1) THEN
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            ELSE
               AZFLAG(I4 + 1) = FNUM
            END IF
         END IF
      ELSE IF (ELEV .AND. .NOT.FLGPOL) THEN
         IF (IFC .GE. 6) THEN
            CALL STONUM(FIELD(6),40,FNUM,INUM)
C           Check The Numerical Field
            IF (INUM .EQ. -1) THEN
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            ELSE
               AZELEV(I3 + 1) = FNUM
            END IF
         END IF
      ELSE IF (FLGPOL .AND. .NOT.ELEV) THEN
         IF (IFC .EQ. 6) THEN
            CALL STONUM(FIELD(6),40,FNUM,INUM)
C           Check The Numerical Field
            IF (INUM .EQ. -1) THEN
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            ELSE
               AZFLAG(I4 + 1) = FNUM
            END IF
         ELSE IF (IFC .EQ. 7) THEN
            CALL STONUM(FIELD(7),40,FNUM,INUM)
C           Check The Numerical Field
            IF (INUM .EQ. -1) THEN
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            ELSE
               AZFLAG(I4 + 1) = FNUM
            END IF
         END IF
      END IF

C     Assign Them to Different Arrays,
C     Retrieve The Origin From Source Coordinates

      CALL SINDEX(SRCID,NSRC,SOID,ISDX,FIND)
      IF (.NOT. FIND) THEN
C        Error Message: Source ID Not Match
         CALL ERRHDL(PATH,MODNAM,'E','300',KEYWRD)
      ELSE
         AXR(I1 + 1) = AXS(ISDX) + RANGE*SIN(DIRECT*DTORAD)
         AYR(I2 + 1) = AYS(ISDX) + RANGE*COS(DIRECT*DTORAD)
         IF (IFC.GE.6 .AND. (ELTYPE.EQ.'FEET' .OR.
     &                       REELEV.EQ.'FEET')) THEN
C           Convert ELEV to Metric system
            AZELEV(I3 + 1) = 0.3048*AZELEV(I3 + 1)
         END IF
         IRXR = I1 + 1
         IRYR = I2 + 1
         IRZE = I3 + 1
         IRZF = I4 + 1
C        Reset ITAB Variable for TOXXFILE Option, 9/29/92
         ITAB = 0
      END IF

      NETID(IRXR)  = '   NA   '
      RECTYP(IRXR) = 'DP'
      IREF(IRXR)   = ISDX

 999  RETURN
      END

      SUBROUTINE BOUNDR
C***********************************************************************
C                 BOUNDR Module of ISC2 Model
C
C        PURPOSE: Processes Plant Boundary Receptor Location Inputs
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        MODIFIED:   To Include TOXXFILE Option - 9/29/92
C                    To Correct Index Counter for BOUNDELV, and
C                    To Include Conversion of Elevations From
C                    Feet to Meters - 9/29/92
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Plant Boundary Receptor Location Inputs
C
C        CALLED FROM:   RECARD
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'
      CHARACTER SOID*8
      LOGICAL FIND

C     Variable Initializations
      MODNAM = 'BOUNDR'

C     Determine Whether There Are Enough Parameter Fields
      IF (IFC .EQ. 2) THEN
C        Error Message: Missing Parameter
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      ELSE IF (IFC .EQ. 3) THEN
C        Error Message: Missing Numerical Field
         CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
         GO TO 999
      END IF

      SOID = FIELD(3)

      CALL SINDEX(SRCID,NSRC,SOID,ISDX,FIND)

      IF (.NOT. FIND) THEN
C        Error Message: Source ID Not Match
         CALL ERRHDL(PATH,MODNAM,'E','300',KEYWRD)
         GO TO 999
      ELSE
         IF (KEYWRD .EQ. 'BOUNDARY') THEN
            ISET = IRXR
            JSET = IRYR
C           Update The Counter
            IF (SOID.NE.PXSOID .AND. PXSOID.NE.' ') THEN
C              Check If The Previous Boundary Points Number Right
               IF (IBND .LT. 36) THEN
C                 Error Message: No. Of Dist Not Enough
                  CALL ERRHDL(PATH,MODNAM,'E','230',PXSOID)
               END IF
C              Reset The Counter
               IBND = 0
            END IF
            DO 25 I = 4, IFC
               CALL STONUM(FIELD(I),40,FNUM,IMIT)
C              Check The Numerical Field
               IF (IMIT .EQ. -1) THEN
                  CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
               END IF
               DO 20 J = 1, IMIT
                  ISET = ISET + 1
                  JSET = JSET + 1
                  IF (ISET .GT. NREC) THEN
C                    Error Msg: Maximum Number Of Receptors Exceeded
                     WRITE(DUMMY,'(I8)') NREC
                     CALL ERRHDL(PATH,MODNAM,'E','219',DUMMY)
                     GO TO 999
                  END IF
                  IBND = IBND + 1
C                 Check If The Count Exceeds 36
                  IF (IBND .GT. 36) THEN
C                    Error Message: No. of Dist Over 36
                     CALL ERRHDL(PATH,MODNAM,'E','231',SOID)
                     GO TO 999
                  END IF
                  DEG  =  10.*IBND*DTORAD
                  AXR(ISET) = AXS(ISDX) + FNUM*SIN(DEG)
                  AYR(JSET) = AYS(ISDX) + FNUM*COS(DEG)
                  RECTYP(ISET) = 'BD'
                  NETID(ISET) = '   NA   '
                  IREF(ISET) = ISDX
 20            CONTINUE
 25         CONTINUE

            IRXR = ISET
            IRYR = JSET
            IRZE = ISET
            IRZF = ISET
            PXSOID = SOID
C           Reset ITAB Variable for TOXXFILE Option, 9/29/92
            ITAB = 0

         ELSE IF (KEYWRD .EQ. 'BOUNDELV') THEN

            IF (FLAT) THEN
C              WRITE Warning Message:  Terrain Elevations Ignored
               CALL ERRHDL(PATH,MODNAM,'W','213',KEYWRD)
               GO TO 999
            END IF
            IF (SOID .NE. PESOID) THEN
C              First BOUNDELV Card for This Source,
C              Check for Previous BOUNDARY Card and Set Index
               FIND = .FALSE.
               DO 100 IREC = 1, NREC
                  IF (RECTYP(IREC) .EQ. 'BD' .AND.
     &                  IREF(IREC) .EQ. ISDX) THEN
C                    Set Index Counter for Elevations and Exit Loop
                     FIND = .TRUE.
                     IRZE = IREC - 1
                     GO TO 200
                  END IF
 100           CONTINUE
               IF (.NOT. FIND) THEN
C                 WRITE Error Message:  Boundary Locations Not Defined
                  CALL ERRHDL(PATH,MODNAM,'E','255',SOID)
                  GO TO 999
               END IF
            END IF

 200        CONTINUE

            ISET = IRZE
            IF (SOID.NE.PESOID .AND. PESOID.NE.' ') THEN
C              Check If The Previous Boundary Points Number Right
               IF (IBELEV .LT. 36) THEN
C                 Error Message: No. Of Elev Not Enough
                  CALL ERRHDL(PATH,MODNAM,'E','230',PESOID)
               END IF
C              Reset The Counter
               IBELEV = 0
            END IF
            DO 35 I = 4, IFC
               CALL STONUM(FIELD(I),40,FNUM,IMIT)
C              Check The Numerical Field
               IF (IMIT .EQ. -1) THEN
                  CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
               END IF
               IF (ELTYPE .EQ. 'FEET' .OR. REELEV .EQ. 'FEET') THEN
C                 Convert ELEV to Metric system
                  FNUM = 0.3048 * FNUM
               END IF
               DO 30 J = 1, IMIT
                  ISET = ISET + 1
                  IF (ISET .GT. NREC) THEN
C                    Error Msg: Maximum Number Of Receptors Exceeded
                     WRITE(DUMMY,'(I8)') NREC
                     CALL ERRHDL(PATH,MODNAM,'E','219',DUMMY)
                     GO TO 999
                  END IF
                  IBELEV = IBELEV + 1
C                 Check If The Count Exceeds 36
                  IF (IBELEV .GT. 36) THEN
C                    Error Message: Elev Over 36
                     CALL ERRHDL(PATH,MODNAM,'E','231',SOID)
                     GO TO 300
                  END IF
                  AZELEV(ISET) = FNUM
 30            CONTINUE
 35         CONTINUE

 300        CONTINUE

            IRZE = ISET
            IRZF = ISET
            PESOID = SOID
         END IF
      END IF

 999  RETURN
      END

      SUBROUTINE SBYVAL(ARRIN1,ARRIN2,INX)
C***********************************************************************
C                 SBYVAL Module of ISC2 Model
C
C        PURPOSE: Sort Array By Its 'Index Value'
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        INPUTS:  ARRIN1: 'Index Array',  ARRIN2: 'Value Array'
C                 INX: Number of Values to Sort
C
C        OUTPUTS: Sorted Array
C
C        CALLED FROM: (This Is A Utility Program)
C***********************************************************************
C
C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'
C     Declare Input Arrays as Assumed-Size Arrays (Currently Dimensioned NREC
C     in Calling Routines)
      DIMENSION ARRIN1(*), ARRIN2(*)

C     Variable Initialization
      MODNAM = 'SBYVAL'
      JC = 1

      DO WHILE (JC .LE. INX)
C        Find out The First Minimum In the Array
         MIN = ARRIN1(JC)
         IMIN = JC
         DO 20 I = JC, INX
            IF (ARRIN1(I) .LT. MIN) THEN
               IMIN = I
               MIN = ARRIN1(I)
            END IF
  20     CONTINUE
C        Swap The Selected Array Elements
         TEMP1 = ARRIN1(JC)
         TEMP2 = ARRIN2(JC)
         ARRIN1(JC) = ARRIN1(IMIN)
         ARRIN2(JC) = ARRIN2(IMIN)
         ARRIN1(IMIN) = TEMP1
         ARRIN2(IMIN) = TEMP2
C        Increment The Counter
         JC = JC + 1
      END DO

      RETURN
      END
