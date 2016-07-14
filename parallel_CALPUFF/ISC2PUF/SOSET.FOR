      SUBROUTINE SOCARD
C***********************************************************************
C                 SOCARD Module of ISC2 Model
C
C        PURPOSE: To process SOurce Pathway card images
C
C        PROGRAMMER:  Roger Brode, Jeff Wang
C        MODIFIED BY  D. Strimaitis, SRC (for WET DEPOSITION)
C
C        DATE:    November  8, 1993
C
C        MODIFIED BY  D. Strimaitis, SRC (for DRY DEPOSITION)
C        (DATE:    February 15, 1993)
C
C        INPUTS:  Pathway (SO) and Keyword
C
C        OUTPUTS: Source Arrays
C                 Sourcer Setup Status Switches
C
C        CALLED FROM:   SETUP
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'

C     Variable Initializations
      MODNAM = 'SOCARD'

      IF (KEYWRD .EQ. 'STARTING') THEN
C        Initialize Counters and Set Status Switch
         ISRC = 0
         IGRP = 0
         NUMSRC = 0
         NUMGRP = 0
         ISSTAT(1) = ISSTAT(1) + 1
         IF (ISSTAT(1) .NE. 1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
            GO TO 999
         END IF
C        Flush The Working Area
         DO 20 I = 1, NSRC
            DO 10 J = 1, 12
               IWRK2(I,J) = 0
  10        CONTINUE
  20     CONTINUE
      ELSE IF (KEYWRD .EQ. 'LOCATION') THEN
C        Set Status Switch
         ISSTAT(2) = ISSTAT(2) + 1
C        Check for SRCGROUP Card Out Of Order
         IF (ISSTAT(19) .NE. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','140','SRCGROUP')
         END IF
C        Process Source Location                            ---   CALL SOLOCA
         CALL SOLOCA
      ELSE IF (KEYWRD .EQ. 'SRCPARAM') THEN
C        Set Status Switch
         ISSTAT(3) = ISSTAT(3) + 1
C        Check for SRCGROUP Card Out Of Order
         IF (ISSTAT(19) .NE. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','140','SRCGROUP')
         END IF
C        Process Source Parameters                          ---   CALL SOPARM
         CALL SOPARM
      ELSE IF (KEYWRD .EQ. 'BUILDHGT' .OR.
     &         KEYWRD .EQ. 'BUILDWID' .OR.
     &         KEYWRD .EQ. 'BUILDLEN' .OR.
     &         KEYWRD .EQ. 'XBADJ   ' .OR.
     &         KEYWRD .EQ. 'YBADJ') THEN
C        Check for SRCGROUP Card Out Of Order
         IF (ISSTAT(19) .NE. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','140','SRCGROUP')
         END IF
C        Set Status Switch
         IF (KEYWRD .EQ. 'BUILDHGT') THEN
            ISSTAT(4) = ISSTAT(4) + 1
         ELSE IF (KEYWRD .EQ. 'BUILDWID') THEN
            ISSTAT(5) = ISSTAT(5) + 1
         ELSE IF (KEYWRD .EQ. 'LOWBOUND') THEN
cvrt
             Write (*,1000) 
1000  format (' WARNING:  "LOWBOUND" Keyword not compatible with ',
     &      'CALPUFF')

            ISSTAT(6) = ISSTAT(6) + 1
            IF (DFAULT) THEN
C              WRITE Warning Message and Ignore Inputs
               CALL ERRHDL(PATH,MODNAM,'W','206',KEYWRD)
               GO TO 999
            END IF
         END IF
C        Process Direction-specific Building Dimensions     ---   CALL DSBLDG
         CALL DSBLDG
      ELSE IF (KEYWRD .EQ. 'EMISFACT') THEN
C        Set Status Switch
         ISSTAT(7) = ISSTAT(7) + 1
C        Check for SRCGROUP Card Out Of Order
         IF (ISSTAT(19) .NE. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','140','SRCGROUP')
         END IF
C        Process Variable Emission Rate Factors             ---   CALL EMVARY
         CALL EMVARY
      ELSE IF (KEYWRD .EQ. 'EMISUNIT') THEN
cvrt
             Write (*,1001) 
1001  format (' WARNING:  "EMISUNIT" Keyword not compatible with ',
     &      'CALPUFF')

C        Set Status Switch
         ISSTAT(8) = ISSTAT(8) + 1
C        Check for SRCGROUP Card Out Of Order
         IF (ISSTAT(19) .NE. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','140','SRCGROUP')
         END IF
         IF (ISSTAT(8) .NE. 1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE IF (NUMTYP .EQ. 1) THEN
C           Process Emission Rate Unit Conversion Factors   ---   CALL EMUNIT
            CALL EMUNIT
         ELSE
C           WRITE Error Message: EMISUNIT Keyword with more than 1 output type
            CALL ERRHDL(PATH,MODNAM,'E','157',' ')
         END IF
      ELSE IF (KEYWRD .EQ. 'PARTDIAM' .OR. KEYWRD .EQ. 'MASSFRAX' .OR.
     &         KEYWRD .EQ. 'PARTDENS' .OR. KEYWRD .EQ. 'PARTSLIQ' .OR.
     &         KEYWRD .EQ. 'PARTSICE') THEN
C        Set Status Switch
         IF (KEYWRD .EQ. 'PARTDIAM') THEN
cvrt
             Write (*,1002) 
1002  format (' WARNING:  "PARTDIAM" Keyword not compatible with ',
     &      'CALPUFF')

            ISSTAT(9) = ISSTAT(9) + 1
         ELSE IF (KEYWRD .EQ. 'MASSFRAX') THEN
cvrt
             Write (*,1003) 
1003  format (' WARNING:  "MASSFRAX" Keyword not compatible with ',
     &      'CALPUFF')

            ISSTAT(10) = ISSTAT(10) + 1
         ELSE IF (KEYWRD .EQ. 'PARTDENS') THEN
cvrt
             Write (*,1004) 
1004  format (' WARNING:  "PARTDENS" Keyword not compatible with ',
     &      'CALPUFF')

            ISSTAT(11) = ISSTAT(11) + 1
         ELSE IF (KEYWRD .EQ. 'PARTSLIQ') THEN
cvrt
             Write (*,1005) 
1005  format (' WARNING:  "PARTSLIQ" Keyword not compatible with ',
     &      'CALPUFF')

            ISSTAT(12) = ISSTAT(12) + 1
         ELSE IF (KEYWRD .EQ. 'PARTSICE') THEN
cvrt
             Write (*,1006) 
1006  format (' WARNING:  "PARTSICE" Keyword not compatible with ',
     &      'CALPUFF')

            ISSTAT(13) = ISSTAT(13) + 1
         END IF
C        Check for SRCGROUP Card Out Of Order
         IF (ISSTAT(19) .NE. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','140','SRCGROUP')
         END IF
C        Process Particle Deposition Parameters             ---   CALL PARTDEP
         CALL PARTDEP
      ELSE IF (KEYWRD .EQ. 'GAS-SCAV') THEN
cvrt
             Write (*,1007) 
1007  format (' WARNING:  "GAS-SCAV" Keyword not compatible with ',
     &      'CALPUFF')

C        Set Status Switch
         ISSTAT(14) = ISSTAT(14) + 1
C        Check for SRCGROUP Card Out Of Order
         IF (ISSTAT(19) .NE. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','140','SRCGROUP')
         END IF
C        Process Wet Deposition Parameters for gases        ---   CALL GASDEP
         CALL GASDEP

C*----   ISCSTM Modification: allow for hourly emissions - jah 11/3/94               
      ELSE IF (KEYWRD .EQ. 'HOUREMIS') THEN
cvrt
             Write (*,1008) 
1008  format (' WARNING:  "HOUREMIS" Keyword not compatible with ',
     &      'CALPUFF')

C*       Set Status Switch
         ISSTAT(16) = ISSTAT(16) + 1
C        Check for SRCGROUP Card Out Of Order
         IF (ISSTAT(19) .NE. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','140','SRCGROUP')
         END IF
C*       Process Hourly Emissions                           ---   CALL HREMIS
         CALL HREMIS
C*#      

      ELSE IF (KEYWRD .EQ. 'SRCGROUP') THEN
cvrt
             Write (*,1009) 
1009  format (' WARNING:  "SRCGROUP" Keyword not compatible with ',
     &      'CALPUFF')

C        Set Status Switch
         ISSTAT(19) = ISSTAT(19) + 1
C        Process Source Groups                              ---   CALL SOGRP
         CALL SOGRP
      ELSE IF (KEYWRD .EQ. 'ELEVUNIT') THEN
C        Set Status Switch
         ISSTAT(15) = ISSTAT(15) + 1
C        Check for SRCGROUP Card Out Of Order
         IF (ISSTAT(19) .NE. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','140','SRCGROUP')
         END IF
         IF (ISSTAT(15) .NE. 1) THEN
C           WRITE Error Message: Repeat Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE IF (NUMSRC .GT. 0) THEN
C           Write Error Message: ELEVUNIT must be first card after STARTING
            CALL ERRHDL(PATH,MODNAM,'E','152','  SO')
         ELSE IF (ICSTAT(10) .NE. 0) THEN
C           Write Error Message: Use of obsolescent CO ELEVUNIT card with
C           SO ELEVUNIT card
            CALL ERRHDL(PATH,MODNAM,'E','153',' SO Path')
         ELSE
C           Process Elevation Units for Source Elevations   ---   CALL SOELUN
            CALL SOELUN
         END IF
      ELSE IF (KEYWRD .EQ. 'CONCUNIT') THEN
cvrt
             Write (*,1010) 
1010  format (' WARNING:  "CONCUNIT" Keyword not compatible with ',
     &      'CALPUFF')

C        Set Status Switch
         ISSTAT(17) = ISSTAT(17) + 1
C        Check for SRCGROUP Card Out Of Order
         IF (ISSTAT(19) .NE. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','140','SRCGROUP')
         END IF
         IF (ISSTAT(17) .NE. 1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE IF (ISSTAT(8) .NE. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','158',KEYWRD)
         ELSE
C           Process Emission Rate Unit Conversion Factors   ---   CALL COUNIT
            CALL COUNIT
         END IF
      ELSE IF (KEYWRD .EQ. 'DEPOUNIT') THEN
cvrt
             Write (*,1011) 
1011  format (' WARNING:  "DEPOUNIT" Keyword not compatible with ',
     &      'CALPUFF')

C        Set Status Switch
         ISSTAT(18) = ISSTAT(18) + 1
C        Check for SRCGROUP Card Out Of Order
         IF (ISSTAT(19) .NE. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','140','SRCGROUP')
         END IF
         IF (ISSTAT(18) .NE. 1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE IF (ISSTAT(8) .NE. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','158',KEYWRD)
         ELSE
C           Process Emission Rate Unit Conversion Factors   ---   CALL DPUNIT
            CALL DPUNIT
         END IF
      ELSE IF (KEYWRD .EQ. 'FINISHED') THEN
C        Set Status Switch
         ISSTAT(20) = ISSTAT(20) + 1
         IF (ISSTAT(20) .NE. 1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         END IF

C        Check for Missing Mandatory Keywords
         IF (ISSTAT(1) .EQ. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','130','STARTING')
         END IF
         IF (ISSTAT(2) .EQ. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','130','LOCATION')
         END IF
         IF (ISSTAT(3) .EQ. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','130','SRCPARAM')
         END IF
         IF (ISSTAT(19) .EQ. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','130','SRCGROUP')
         END IF
         IF (ISSTAT(3) .LT. ISSTAT(2)) THEN
C           Must Be Missing a SRCPARAM Card for One or More Sources
            CALL ERRHDL(PATH,MODNAM,'E','130','SRCPARAM')
         END IF

C        Check to Insure That SRCGROUP Was The Last Functional Keyword
         IF (PKEYWD .NE. 'SRCGROUP') THEN
            CALL ERRHDL(PATH,MODNAM,'E','140','SRCGROUP')
         END IF

         IF (NUMSRC .EQ. 0) THEN
C           WRITE Error Message:  No Sources Input
            CALL ERRHDL(PATH,MODNAM,'E','248','NUMSRC=0')
         ELSE
C           Quality Assure Source Parameter Inputs          ---   CALL SRCQA
            CALL SRCQA
C           Calculate settling velocity and related time-invariant
C           deposition data                                 ---   CALL VDP1
            IF (LDPART) then
               CALL VDP1
            END IF
         END IF

      ELSE
C        Write Error Message: Invalid Keyword for This Pathway
         CALL ERRHDL(PATH,MODNAM,'E','110',KEYWRD)
      END IF

 999  RETURN
      END

      SUBROUTINE SRCQA
C***********************************************************************
C                 SRCQA Module of ISC2 Model
C
C        PURPOSE: Quality Assure Source Parameter Inputs
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C        MODIFIED BY D. Strimaitis, SRC (for WET & DRY DEPOSITION)
C
C        DATE:    November 8, 1993
C
C        MODIFIED BY D. Strimaitis, SRC (for DEPOSITION)
C        (DATE:    February 15, 1993)
C
C        INPUTS:  Source Parameters
C                 Source Parameters Array Limits, IWRK2(NSRC,12)
C
C        OUTPUTS: Source Parameter Error Messages
C
C        CALLED FROM:   SOCARD
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'

C     Variable Initializations
      MODNAM = 'SRCQA'

C     Begin Source LOOP
      DO 40 I = 1, NUMSRC

C        Check Source Array Limits for Too Few Values;
C        (Too Many Checked In DSFILL and EFFILL)
         IF (IWRK2(I,1).NE.0 .AND. IWRK2(I,1).LT.NSEC) THEN
C           WRITE Error Message:  Not Enough BUILDHGTs
            CALL ERRHDL(PATH,MODNAM,'E','236',SRCID(I))
         END IF
         IF (IWRK2(I,2).NE.0 .AND. IWRK2(I,2).LT.NSEC) THEN
C           WRITE Error Message:  Not Enough BUILDWIDs
            CALL ERRHDL(PATH,MODNAM,'E','237',SRCID(I))
         END IF
         IF (IWRK2(I,3).NE.0 .AND. IWRK2(I,3).LT.NSEC) THEN
C           WRITE Error Message:  Not Enough LOWBOUNDs
            CALL ERRHDL(PATH,MODNAM,'E','238',SRCID(I))
         END IF
         IF (QFLAG(I) .NE. ' ') THEN
            IF (QFLAG(I).EQ.'SEASON' .AND. IWRK2(I,4).LT.4) THEN
C              WRITE Error Message: Not Enough QFACTs
               CALL ERRHDL(PATH,MODNAM,'E','239',SRCID(I))
            ELSE IF (QFLAG(I).EQ.'MONTH' .AND. IWRK2(I,4).LT.12) THEN
C              WRITE Error Message: Not Enough QFACTs
               CALL ERRHDL(PATH,MODNAM,'E','239',SRCID(I))
            ELSE IF(QFLAG(I).EQ.'HROFDY' .AND. IWRK2(I,4).LT.24) THEN
C              WRITE Error Message: Not Enough QFACTs
               CALL ERRHDL(PATH,MODNAM,'E','239',SRCID(I))
            ELSE IF (QFLAG(I).EQ.'STAR' .AND. IWRK2(I,4).LT.36) THEN
C              WRITE Error Message: Not Enough QFACTs
               CALL ERRHDL(PATH,MODNAM,'E','239',SRCID(I))
            ELSE IF(QFLAG(I).EQ.'SEASHR' .AND. IWRK2(I,4).LT.96) THEN
C              WRITE Error Message: Not Enough QFACTs
               CALL ERRHDL(PATH,MODNAM,'E','239',SRCID(I))
            END IF
         END IF

C        Check Settling and Removal Parameters
         IF (IWRK2(I,5).NE.0 .OR. IWRK2(I,6).NE.0 .OR.
     &       IWRK2(I,7).NE.0 .OR. IWRK2(I,8).NE.0 .OR.
     &                            IWRK2(I,9).NE.0) THEN
C           Set Number of Particle Diameter Categories for This Source
            INPD(I) = IWRK2(I,5)
C           Check for Consistent Number of Categories for All Parameters
            IF (IWRK2(I,5).NE.IWRK2(I,6) .OR.
     &          IWRK2(I,5).NE.IWRK2(I,7)) THEN
C              WRITE Error Message: PartDiam Categories Don't Match
               CALL ERRHDL(PATH,MODNAM,'E','240',SRCID(I))
            ELSE IF (DEPOS .OR. WDEP .OR. WDPLETE) THEN
               IF (IWRK2(I,5).NE.IWRK2(I,8) .OR.
     &             IWRK2(I,5).NE.IWRK2(I,9)) THEN
C                 WRITE Error Message: PartDiam Categories Don't Match
                  CALL ERRHDL(PATH,MODNAM,'E','240',SRCID(I))
               END IF
            END IF
C           Check for Mass Fraction Summing to 1.0 (+/- 2%)
            ATOT = 0.0
            N = INPD(I)
            IF (N .LE. NPDMAX) THEN
               DO 30 J = 1, N
                  ATOT = ATOT + APHI(J,I)
 30            CONTINUE
               IF (ATOT .LT. 0.98 .OR. ATOT .GT. 1.02) THEN
C                 WRITE Error Message: Mass Fractions Don't Sum to 1.0
                  CALL ERRHDL(PATH,MODNAM,'W','330',SRCID(I))
               END IF
               DO 35 J = 1, N
                  IF ((DEPOS.OR.WDEP.OR.WDPLETE) .AND.
     &                      (APSLIQ(J,I).EQ.0.0)) THEN
C                    WRITE Warning Message: Missing or Invalid Scavenging Coef.
                     CALL ERRHDL(PATH,MODNAM,'W','243',SRCID(I))
                  END IF
 35            CONTINUE
            ELSE
C              WRITE Error Message:  Too Many Settling/Removal Categories
               CALL ERRHDL(PATH,MODNAM,'E','244',SRCID(I))
            END IF

C        Check for OPENPIT source type with no particle categories
         ELSE IF (SRCTYP(I) .EQ. 'OPENPIT') THEN
C           WRITE Error Message: Open Pit source with no particle categories
            CALL ERRHDL(PATH,MODNAM,'E','323',SRCID(I))
         END IF

C        Screen for Conflicts with the use of Particles
         IF (INPD(I) .EQ. 0) THEN
C           Check for NPD=0 with the DDEP option (DDEP is active only for
C           particles at this time, not for gases!)
            IF (DEPOS .OR. DDEP .OR. DDPLETE) THEN
C              WRITE Error Message for Lack of Settling/Removal Categories
               CALL ERRHDL(PATH,MODNAM,'W','242',SRCID(I))
            END IF
            IF (DEPOS .OR. WDEP .OR. WDPLETE) THEN
               IF (AGSCAV(1,I) .LE. 0.0) THEN
C                 WRITE Error Message:  Missing or Invalid Scavenging Coef.
                  CALL ERRHDL(PATH,MODNAM,'W','243',SRCID(I))
               END IF
            END IF
         ELSE
C           Check for NPD .NE. 0 and Gas Scavenging Coefficient .NE. 0 for
C           this source (A source may be either particles, or gas, but
C           not both !)
            IF((AGSCAV(1,I) .GT. 0.0) .OR. (AGSCAV(2,I) .GT. 0.0)) THEN
C              WRITE Error Message:  Too Many Settling/Removal Categories
               CALL ERRHDL(PATH,MODNAM,'E','244',SRCID(I))
            END IF
         END IF

 40   CONTINUE
C     End Source LOOP

      RETURN
      END

      SUBROUTINE SOELUN
C***********************************************************************
C                 SOELUN Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Process Elevation Units Option for Sources
C                 From Runstream Input Image
C
C        PROGRAMMER: Roger Brode
C
C        DATE:    November 22, 1994
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Source Elevation Units Switch
C
C        ERROR HANDLING:   Checks for Invalid Parameters;
C                          Checks for No Parameters;
C                          Checks for Too Many Parameters
C
C        CALLED FROM:   SOCARD
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'

C     Variable Initializations
      MODNAM = 'SOELUN'

      IF (IFC .EQ. 3) THEN
         IF (FIELD(3) .EQ. 'METERS') THEN
            SOELEV = 'METERS'
         ELSE IF (FIELD(3) .EQ. 'FEET') THEN
            SOELEV = 'FEET'
         ELSE
C           WRITE Error Message  ! Invalid Parameter
            CALL ERRHDL(PATH,MODNAM,'E','203','SO_ELEV')
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

      SUBROUTINE SOLOCA
C***********************************************************************
C                 SOLOCA Module of ISC2 Model
C
C        PURPOSE: Processes Source Location Card
C
C        PROGRAMMER:  Jeff Wang, Roger Brode
C
C*       MODIFIED BY: Jayant Hardikar (PES) 7/19/94 to incorporate
C*                    new "PIT" source type.
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Source Type and Location
C
C        CALLED FROM:   SOCARD
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'
      CHARACTER SOID*8
      LOGICAL FIND

C     Variable Initializations
      FIND = .FALSE.
      MODNAM = 'SOLOCA'

C     Check The Number Of The Fields
      IF (IFC .LE. 2) THEN
C        Error Message: No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      ELSE IF (IFC .LT. 6) THEN
C        Error Message: Not Enough Parameters
         CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
         GO TO 999
      ELSE IF (IFC .GT. 7) THEN
C        Error Message: Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
         GO TO 999
      END IF

C     Read In The Data Fields and Assign to Arrays
C     First Check for Previous Occurrence of This SRCID
      SOID = FIELD(3)
      CALL SINDEX(SRCID,NSRC,SOID,INDEXS,FIND)

      IF (.NOT. FIND) THEN
         ISRC = ISRC + 1
         IF (ISRC .LE. NSRC) THEN
            SRCID(ISRC)  = FIELD(3)
            SRCTYP(ISRC) = FIELD(4)

            IF (SRCTYP(ISRC) .EQ. 'OPENPIT'  .OR.
     &          SRCTYP(ISRC) .EQ. 'OPEN_PIT' .OR.
     &          SRCTYP(ISRC) .EQ. 'OPEN-PIT') THEN
                   SRCTYP(ISRC) = 'OPENPIT'
            ENDIF
            
            
C*          IF (SRCTYP(ISRC).EQ.'POINT' .OR. SRCTYP(ISRC).EQ.
C*   &             'VOLUME' .OR. SRCTYP(ISRC).EQ.'AREA') THEN
            IF (SRCTYP(ISRC).EQ.'POINT' .OR. SRCTYP(ISRC).EQ.
     &             'VOLUME' .OR. SRCTYP(ISRC).EQ.'AREA' .OR.
     &              SRCTYP(ISRC) .EQ. 'OPENPIT') THEN

               CALL STONUM(FIELD(5), 40, AXS(ISRC), IMIT)
C              Check The Numerical Field
               IF (IMIT .NE. 1) THEN
                  CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
               END IF
               CALL STONUM(FIELD(6), 40, AYS(ISRC), IMIT)
C              Check The Numerical Field
               IF (IMIT .NE. 1) THEN
                  CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
               END IF
               IF (IFC .EQ. 7) THEN
C                 Retrieve Source Elevation From Inputs
                  CALL STONUM(FIELD(7), 40, AZS(ISRC), IMIT)
C                 Check The Numerical Field
                  IF (IMIT .NE. 1) THEN
                     CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
                  END IF
C                 Check for units conversion from feet to meters
                  IF (SOELEV .EQ. 'FEET') THEN
                     AZS(ISRC) = AZS(ISRC) * 0.3048
                  END IF
               ELSE
C                 No Source Elevation Field - Default to 0.0
                  AZS(ISRC) = 0.0
                  IF (ELEV) THEN
C                    Write Warning Message for No Source Elevation with ELEV
                     CALL ERRHDL(PATH,MODNAM,'W','205','ZS = 0.0')
                  END IF
               END IF
            ELSE
C              Error Message: Invalid Source Type
               CALL ERRHDL(PATH,MODNAM,'E','203','SRCTYP')
               GO TO 999
            END IF
            ISET = ISRC
            NUMSRC = NUMSRC + 1
         ELSE
C           WRITE Error Message    ! Number of Sources Exceeds NSRC Parameter
            WRITE(DUMMY,'(I8)') NSRC
            CALL ERRHDL(PATH,MODNAM,'E','232',DUMMY)
            GO TO 999
         END IF
      ELSE
C        WRITE Error Message    ! Source Location Has Already Been Identified
         CALL ERRHDL(PATH,MODNAM,'E','310',SOID)
      END IF

 999  RETURN
      END

      SUBROUTINE SOPARM
C***********************************************************************
C                 SOPARM Module of ISC2 Model
C
C        PURPOSE: Processes Source parameter Card
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        MODIFIED:   To allow for additional parameters on area source
C                    parameter cards for new algorithm - 7/7/93
C
C*       MODIFIED BY: Jayant Hardikar (PES) 7/19/94 to incorporate
C*                    new "PIT" source type.
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Source Parameters
C
C        CALLED FROM:   SOCARD
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'
      LOGICAL FIND
      REAL TEMP(IFMAX)

C     Variable Initializations
      FIND = .FALSE.
      MODNAM = 'SOPARM'

C     Check The Number Of The Fields
      IF (IFC .LE. 2) THEN
C        Error Message: No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      ELSE IF (IFC .EQ. 3) THEN
C        Error Message: No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
         GO TO 999
      END IF

C     Search For The Source ID Index
      CALL SINDEX(SRCID,NSRC,FIELD(3),ISDX,FIND)

      IF (FIND) THEN
C        Check for Previous SRCPARAM Card for This Source
         IF (SOPCRD(ISDX) .EQ. 'Y') THEN
C           WRITE Error Message: Duplicate SRCPARAM Card
            CALL ERRHDL(PATH,MODNAM,'E','315',SRCID(ISDX))
            GO TO 999
         ELSE
            SOPCRD(ISDX) = 'Y'
         END IF
C        Assign The Parameter Arrays
         DO 50 I = 4, IFC
            CALL STONUM(FIELD(I),40,TEMP(I-3),IMIT)
C           Check The Numerical Field
            IF (IMIT .NE. 1) THEN
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
               GO TO 999
            END IF
 50      CONTINUE
         IF (SRCTYP(ISDX) .EQ. 'POINT') THEN
            IF (IFC .EQ. 3) THEN
C              Error Message: No Parameters
               CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
               GO TO 999
            ELSE IF (IFC .LT. 8) THEN
C              Error Message: Not Enough Parameters
               CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
               GO TO 999
            ELSE IF (IFC .GT. 8) THEN
C              Error Message: Too Many Parameters
               CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
               GO TO 999
            END IF
            CALL PPARM(ISDX,TEMP)
         ELSE IF (SRCTYP(ISDX) .EQ. 'VOLUME') THEN
            IF (IFC .EQ. 3) THEN
C              Error Message: No Parameters
               CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
               GO TO 999
            ELSE IF (IFC .LT. 7) THEN
C              Error Message: Not Enough Parameters
               CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
               GO TO 999
            ELSE IF (IFC .GT. 7) THEN
C              Error Message: Too Many Parameters
               CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
               GO TO 999
            END IF
            CALL VPARM(ISDX,TEMP)
         ELSE IF (SRCTYP(ISDX) .EQ. 'AREA') THEN
            IF (IFC .EQ. 3) THEN
C              Error Message: No Parameters
               CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
               GO TO 999
            ELSE IF (IFC .LT. 6) THEN
C              Error Message: Not Enough Parameters
               CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
               GO TO 999
            ELSE IF (IFC .GT. 9) THEN
C              Error Message: Too Many Parameters
               CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
               GO TO 999
            END IF
            CALL APARM(ISDX,TEMP)
C*       Get Source Parameters for the OPENPIT source
         ELSE IF (SRCTYP(ISDX) .EQ. 'OPENPIT') THEN
            IF (IFC .EQ. 3) THEN
C              Error Message: No Parameters
               CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
               GO TO 999
            ELSE IF (IFC .LT. 8) THEN
C              Error Message: Not Enough Parameters
               CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
               GO TO 999
            ELSE IF (IFC .GT. 9) THEN
C              Error Message: Too Many Parameters
               CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
               GO TO 999
            END IF
            CALL OPARM(ISDX,TEMP)
            
         END IF
      ELSE
C        WRITE Error Message    ! Source Location Has Not Been Identified Yet
         CALL ERRHDL(PATH,MODNAM,'E','300',KEYWRD)
      END IF

 999  RETURN
      END

      SUBROUTINE PPARM(ISDX,TEMP)
C***********************************************************************
C                 PPARM Module of ISC2 Model
C
C        PURPOSE: Processes Source Parameters for POINT Sources
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Source Parameter Card
C
C        CALLED FROM:   SOPARM
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'
      REAL TEMP(IFMAX)

C     Variable Initializations
      MODNAM = 'PPARM'

      AQS(ISDX) = TEMP(1)
      AHS(ISDX) = TEMP(2)
      ATS(ISDX) = TEMP(3)
      AVS(ISDX) = TEMP(4)
      ADS(ISDX) = TEMP(5)

C     Perform QA Error Checking on Source Parameters

      IF (AQS(ISDX) .EQ. 0.0) THEN
C        WRITE Warning Message:  Emission Rate Equals 0.0
         CALL ERRHDL(PATH,MODNAM,'W','320',' QS ')
      END IF

      IF (AHS(ISDX) .LT. 0.0) THEN
C        WRITE Error Message:  Negative Release Height
         CALL ERRHDL(PATH,MODNAM,'E','209',' HS ')
      ELSE IF (AHS(ISDX) .GT. 600.0) THEN
C        WRITE Warning Message:  Large Release Height (> 600M)
         CALL ERRHDL(PATH,MODNAM,'W','320',' HS ')
      END IF

      IF (ATS(ISDX) .EQ. 0.0) THEN
C        Set Temperature to Small Negative Value for Ambient Releases
         ATS(ISDX) = -1.0E-5
      ELSE IF (ATS(ISDX) .GT. 2000.0) THEN
C        WRITE Warning Message:  Exit Temp. > 2000K
         CALL ERRHDL(PATH,MODNAM,'W','320',' TS ')
      END IF

      IF (AVS(ISDX) .LT. 0.0) THEN
C        WRITE Warning Message:  Negative or Zero Exit Velocity
         CALL ERRHDL(PATH,MODNAM,'W','325',' VS ')
C        Set to Small Value to Avoid Zero-divide and Underflow
         AVS(ISDX) = 1.0E-5
      ELSE IF (AVS(ISDX) .LT. 1.0E-5) THEN
C        Set to Small Value to Avoid Zero-divide and Underflow
         AVS(ISDX) = 1.0E-5
      ELSE IF (AVS(ISDX) .GT. 50.0) THEN
C        WRITE Warning Message:  Exit Velocity > 50.0 m/s
         CALL ERRHDL(PATH,MODNAM,'W','320',' VS ')
      END IF

      IF (ADS(ISDX) .LT. 0.0) THEN
C        WRITE Warning Message:  Negative Stack Diameter
         CALL ERRHDL(PATH,MODNAM,'E','209',' DS ')
      ELSE IF (ADS(ISDX) .LT. 1.0E-5) THEN
C        Set to Small Value to Avoid Zero-divide and Underflow
         ADS(ISDX) = 1.0E-5
      ELSE IF (ADS(ISDX) .GT. 20.0) THEN
C        WRITE Warning Message:  Large Stack Diameter (> 20m)
         CALL ERRHDL(PATH,MODNAM,'W','320',' DS ')
      END IF

      RETURN
      END

      SUBROUTINE VPARM(ISDX,TEMP)
C***********************************************************************
C                 VPARM Module of ISC2 Model
C
C        PURPOSE: Processes Source Parameters for VOLUME Sources
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Source Parameter Card
C
C        CALLED FROM:   SOPARM
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'
      REAL TEMP(IFMAX)

C     Variable Initializations
      MODNAM = 'VPARM'

      AQS(ISDX) = TEMP(1)
      AHS(ISDX) = TEMP(2)
      ASYINI(ISDX) = TEMP(3)
      ASZINI(ISDX) = TEMP(4)

C     Perform QA Error Checking on Source Parameters

      IF (AQS(ISDX) .EQ. 0.0) THEN
C        WRITE Warning Message:  Emission Rate Equals 0.0
         CALL ERRHDL(PATH,MODNAM,'W','320',' QS ')
      END IF

      IF (AHS(ISDX) .LT. 0.0) THEN
C        WRITE Error Message:  Negative Release Height
         CALL ERRHDL(PATH,MODNAM,'E','209',' HS ')
      ELSE IF (AHS(ISDX) .GT. 100.0) THEN
C        WRITE Warning Message:  Large Release Height (> 100M)
         CALL ERRHDL(PATH,MODNAM,'W','320',' HS')
      END IF

      IF (ASYINI(ISDX) .LT. 0.0) THEN
C        WRITE Warning Message:  Negative Initial Lateral Parameter
         CALL ERRHDL(PATH,MODNAM,'E','209',' SYINIT ')
      ELSE IF (ASYINI(ISDX) .LT. 1.0E-5) THEN
C        WRITE Warning Message:  Small Initial Lateral Parameter
         CALL ERRHDL(PATH,MODNAM,'W','320',' SYINIT ')
C        Set to Small Value to Avoid Zero-divide and Underflow
         ASYINI(ISDX) = 1.0E-5
      ELSE IF (ASYINI(ISDX) .GT. 200.0) THEN
C        WRITE Warning Message:  Large Initial Lateral Parameter (> 200m)
         CALL ERRHDL(PATH,MODNAM,'W','320',' SYINIT ')
      END IF

      IF (ASZINI(ISDX) .LT. 0.0) THEN
C        WRITE Warning Message:  Negative Initial Vertical Parameter
         CALL ERRHDL(PATH,MODNAM,'E','209',' SZINIT ')
      ELSE IF (ASZINI(ISDX) .LT. 1.0E-5) THEN
C        WRITE Warning Message:  Small Initial Lateral Parameter
         CALL ERRHDL(PATH,MODNAM,'W','320',' SZINIT ')
C        Set to Small Value to Avoid Zero-divide and Underflow
         ASZINI(ISDX) = 1.0E-5
      ELSE IF (ASZINI(ISDX) .GT. 200.0) THEN
C        WRITE Warning Message:  Large Initial Vertical Parameter (> 200m)
         CALL ERRHDL(PATH,MODNAM,'W','320',' SZINIT ')
      END IF

      RETURN
      END

      SUBROUTINE APARM(ISDX,TEMP)
C***********************************************************************
C                 APARM Module of ISC2 Model
C
C        PURPOSE: Processes Source Parameters for AREA Sources
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        MODIFIED:   To allow for additional parameters on area source
C                    parameter cards for new algorithm - 7/7/93
C
C        MODIFIED:   Corrected IF-BLOCK for error checking - 7/21/94
C
C        MODIFIED BY Roger Brode, PES (modified data structure for
C                    AXVERT and AYVERT for consistency with other
C                    2-D source arrays) - 8/15/95
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Source Parameter Card
C
C        CALLED FROM:   SOPARM
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'
      REAL TEMP(IFMAX)

C     Variable Initializations
      MODNAM = 'APARM'

      AQS(ISDX) = TEMP(1)
      AHS(ISDX) = TEMP(2)
      IF (IFC .EQ. 6) THEN
         AXINIT(ISDX) = TEMP(3)
         AYINIT(ISDX) = AXINIT(ISDX)
         AANGLE(ISDX) = 0.
         ASZINI(ISDX) = 0.
      ELSE IF (IFC .EQ. 7) THEN
         AXINIT(ISDX) = TEMP(3)
         AYINIT(ISDX) = TEMP(4)
         AANGLE(ISDX) = 0.
         ASZINI(ISDX) = 0.
      ELSE IF (IFC .EQ. 8) THEN
         AXINIT(ISDX) = TEMP(3)
         AYINIT(ISDX) = TEMP(4)
         AANGLE(ISDX) = TEMP(5)
         ASZINI(ISDX) = 0.

C*----   ISCSTM Modification: allow for initial sigma-Z - jah 11/2/94                        
      ELSE IF (IFC .EQ. 9) THEN
         AXINIT(ISDX) = TEMP(3)
         AYINIT(ISDX) = TEMP(4)
         AANGLE(ISDX) = TEMP(5)
         ASZINI(ISDX) = TEMP(6)
C*#
      END IF

C     Perform QA Error Checking on Source Parameters

      IF (AQS(ISDX) .EQ. 0.0) THEN
C        WRITE Warning Message:  Emission Rate Equals 0.0
         CALL ERRHDL(PATH,MODNAM,'W','320',' QS ')
      END IF

      IF (AHS(ISDX) .LT. 0.0) THEN
C        WRITE Error Message:  Negative Release Height
         CALL ERRHDL(PATH,MODNAM,'E','209',' HS ')
      ELSE IF (AHS(ISDX) .GT. 100.0) THEN
C        WRITE Warning Message:  Large Release Height (> 100M)
         CALL ERRHDL(PATH,MODNAM,'W','320',' HS ')
      END IF

      IF (AXINIT(ISDX) .LT. 0.0) THEN
C        WRITE Error Message:  Negative Area Size
         CALL ERRHDL(PATH,MODNAM,'E','209',' XINIT ')
      ENDIF
      IF (AYINIT(ISDX) .LT. 0.0) THEN
C        WRITE Error Message:  Negative Area Size
         CALL ERRHDL(PATH,MODNAM,'E','209',' YINIT ')
      ENDIF
      IF (AXINIT(ISDX) .LT. 1.0E-5) THEN
C        WRITE Warning Message:  Small Source Area
         CALL ERRHDL(PATH,MODNAM,'W','320',' XINIT ')
C        Set to Small Value to Avoid Zero-divide and Underflow
         AXINIT(ISDX) = 1.0E-5
      ENDIF
      IF (AYINIT(ISDX) .LT. 1.0E-5) THEN
C        WRITE Warning Message:  Small Source Area
         CALL ERRHDL(PATH,MODNAM,'W','320',' YINIT ')
C        Set to Small Value to Avoid Zero-divide and Underflow
         AYINIT(ISDX) = 1.0E-5
      ENDIF
      IF (AXINIT(ISDX) .GT. 2000.0) THEN
C        WRITE Warning Message:  Large Source Area (> 2000m)
         CALL ERRHDL(PATH,MODNAM,'W','320',' XINIT ')
      ENDIF
      IF (AYINIT(ISDX) .GT. 2000.0) THEN
C        WRITE Warning Message:  Large Source Area (> 2000m)
         CALL ERRHDL(PATH,MODNAM,'W','320',' YINIT ')
      ENDIF
      IF (ABS(AANGLE(ISDX)) .GT. 180. ) THEN
C        WRITE Warning Message:  Rotation Angle Larger Than 180 Degrees
         CALL ERRHDL(PATH,MODNAM,'W','320',' ANGLE ')
      ENDIF
      
C*----   ISCSTM Modification: allow for initial sigma-Z - jah 11/2/94                        
      IF (ASZINI(ISDX) .LT. 0.0) THEN
C*       WRITE Warning Message:  Negative Initial Vertical Parameter
         CALL ERRHDL(PATH,MODNAM,'E','209',' SZINIT ')
      ELSE IF (ASZINI(ISDX) .LT. 1.0E-5) THEN
C*       Set to Small Value to Avoid Zero-divide and Underflow
         ASZINI(ISDX) = 1.0E-5
      ELSE IF (ASZINI(ISDX) .GT. 200.0) THEN
C*       WRITE Warning Message:  Large Initial Vertical Parameter (> 200m)
         CALL ERRHDL(PATH,MODNAM,'W','320',' SZINIT ')
      END IF
C*----

C     Check for aspect ratio (length/width) > 10
      IF (AYINIT(ISDX)/AXINIT(ISDX) .GT. 10.0 .OR.
     &    AXINIT(ISDX)/AYINIT(ISDX) .GT. 10.0) THEN
C        WRITE Warning Message: Aspect ratio > 10 for area source
         CALL ERRHDL(PATH,MODNAM,'W','391',SRCID(ISDX))
      END IF

C     Set Number of Vertices (4 for Rectangular Source)
      NVERT = 4

C     Set Coordinates of Vertices for Rectangular Area (in Kilometers).
C     Vertices Start with the "Southwest" Corner and Are Defined
C     Clockwise.  The First Vertex is Repeated as the Last Vertex.

      AXVERT(1,ISDX) = AXS(ISDX)/1000.
      AYVERT(1,ISDX) = AYS(ISDX)/1000.

      AXVERT(2,ISDX) = AXVERT(1,ISDX) +
     &                (AYINIT(ISDX)*SIN(AANGLE(ISDX)*DTORAD))/1000.
      AYVERT(2,ISDX) = AYVERT(1,ISDX) +
     &                (AYINIT(ISDX)*COS(AANGLE(ISDX)*DTORAD))/1000.

      AXVERT(3,ISDX) = AXVERT(2,ISDX) +
     &                (AXINIT(ISDX)*COS(AANGLE(ISDX)*DTORAD))/1000.
      AYVERT(3,ISDX) = AYVERT(2,ISDX) -
     &                (AXINIT(ISDX)*SIN(AANGLE(ISDX)*DTORAD))/1000.

      AXVERT(4,ISDX) = AXVERT(3,ISDX) -
     &                (AYINIT(ISDX)*SIN(AANGLE(ISDX)*DTORAD))/1000.
      AYVERT(4,ISDX) = AYVERT(3,ISDX) -
     &                (AYINIT(ISDX)*COS(AANGLE(ISDX)*DTORAD))/1000.

      AXVERT(5,ISDX) = AXS(ISDX)/1000.
      AYVERT(5,ISDX) = AYS(ISDX)/1000.

      RETURN
      END

      SUBROUTINE OPARM(ISDX,TEMP)
C***********************************************************************
C                 OPARM Module of ISC2 Model
C
C        PURPOSE: Processes Source Parameters for OPENPIT Sources
C
C        PROGRAMMER: Jayant Hardikar, Roger Brode
C                    (based on APARM - Jeff Wang/Roger Brode)
C
C        DATE:       July 19, 1994
C
C        MODIFIED BY Roger Brode, PES (modified data structure for
C                    AXVERT and AYVERT for consistency with other
C                    2-D source arrays) - 8/15/95
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Source Parameter Card
C
C        CALLED FROM:   SOPARM
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'
      REAL TEMP(IFMAX)

C     Variable Initializations
      MODNAM = 'OPARM'

      AQS(ISDX) = TEMP(1)
      AHS(ISDX) = TEMP(2)
      AXINIT(ISDX) = TEMP(3)
      AYINIT(ISDX) = TEMP(4)
      AVOLUM(ISDX) = TEMP(5)
      AANGLE(ISDX) = 0.      
      IF (IFC .EQ. 9) THEN
         AANGLE(ISDX) = TEMP(6)
      END IF

C     Perform QA Error Checking on Source Parameters

      IF (AQS(ISDX) .EQ. 0.0) THEN
C        WRITE Warning Message:  Emission Rate Equals 0.0
         CALL ERRHDL(PATH,MODNAM,'W','320',' QS ')
      END IF

      IF (AHS(ISDX) .LT. 0.0) THEN
C        WRITE Error Message:  Negative Release Height
         CALL ERRHDL(PATH,MODNAM,'E','209',' HS ')
      ELSE IF (AHS(ISDX) .GT. 200.0) THEN
C        WRITE Warning Message:  Large Release Height (> 200M)
         CALL ERRHDL(PATH,MODNAM,'W','320',' HS ')
      END IF

      IF (AXINIT(ISDX) .LT. 0.0) THEN
C        WRITE Error Message:  Negative Area Size
         CALL ERRHDL(PATH,MODNAM,'E','209',' XINIT ')
      ENDIF
      IF (AYINIT(ISDX) .LT. 0.0) THEN
C        WRITE Error Message:  Negative Area Size
         CALL ERRHDL(PATH,MODNAM,'E','209',' YINIT ')
      ENDIF
      IF (AXINIT(ISDX) .LT. 1.0E-5) THEN
C        WRITE Warning Message:  Small Source Area
         CALL ERRHDL(PATH,MODNAM,'W','320',' XINIT ')
C        Set to Small Value to Avoid Zero-divide and Underflow
         AXINIT(ISDX) = 1.0E-5
      ENDIF
      IF (AYINIT(ISDX) .LT. 1.0E-5) THEN
C        WRITE Warning Message:  Small Source Area
         CALL ERRHDL(PATH,MODNAM,'W','320',' YINIT ')
C        Set to Small Value to Avoid Zero-divide and Underflow
         AYINIT(ISDX) = 1.0E-5
      ENDIF
      IF (AXINIT(ISDX) .GT. 2000.0) THEN
C        WRITE Warning Message:  Large Source Area (> 2000m)
         CALL ERRHDL(PATH,MODNAM,'W','320',' XINIT ')
      ENDIF
      IF (AYINIT(ISDX) .GT. 2000.0) THEN
C        WRITE Warning Message:  Large Source Area (> 2000m)
         CALL ERRHDL(PATH,MODNAM,'W','320',' YINIT ')
      ENDIF
      IF (ABS(AANGLE(ISDX)) .GT. 180. ) THEN
C        WRITE Warning Message:  Rotation Angle Larger Than 180 Degrees
         CALL ERRHDL(PATH,MODNAM,'W','320',' ANGLE ')
      ENDIF
      IF (AVOLUM(ISDX) .LE. 0.0) THEN
C        WRITE Error Message: Open-Pit Volume is less than
C        or equal to zero
         CALL ERRHDL(PATH,MODNAM,'E','209',' AVOLUM ')
      ENDIF

C     Check for aspect ratio (length/width) > 10
      IF (AYINIT(ISDX)/AXINIT(ISDX) .GT. 10.0 .OR.
     &    AXINIT(ISDX)/AYINIT(ISDX) .GT. 10.0) THEN
C        WRITE Warning Message: Aspect ratio > 10 for pit source
         CALL ERRHDL(PATH,MODNAM,'W','392',SRCID(ISDX))
      END IF

C     Check for Release Height > Effective Depth
      EFFDEP = AVOLUM(ISDX)/(AXINIT(ISDX)*AYINIT(ISDX))
      IF (AHS(ISDX) .GT. EFFDEP) THEN
C        WRITE Error Message: Release Height is greater than Effective Depth
         CALL ERRHDL(PATH,MODNAM,'E','322',SRCID(ISDX))
      END IF

C     Set Number of Vertices (4 for Rectangular Source)
      NVERT = 4

C     Set Coordinates of Vertices for Rectangular Area (in Kilometers).
C     Vertices Start with the "Southwest" Corner and Are Defined
C     Clockwise.  The First Vertex is Repeated as the Last Vertex.

      AXVERT(1,ISDX) = AXS(ISDX)/1000.
      AYVERT(1,ISDX) = AYS(ISDX)/1000.

      AXVERT(2,ISDX) = AXVERT(1,ISDX) +
     &                (AYINIT(ISDX)*SIN(AANGLE(ISDX)*DTORAD))/1000.
      AYVERT(2,ISDX) = AYVERT(1,ISDX) +
     &                (AYINIT(ISDX)*COS(AANGLE(ISDX)*DTORAD))/1000.

      AXVERT(3,ISDX) = AXVERT(2,ISDX) +
     &                (AXINIT(ISDX)*COS(AANGLE(ISDX)*DTORAD))/1000.
      AYVERT(3,ISDX) = AYVERT(2,ISDX) -
     &                (AXINIT(ISDX)*SIN(AANGLE(ISDX)*DTORAD))/1000.

      AXVERT(4,ISDX) = AXVERT(3,ISDX) -
     &                (AYINIT(ISDX)*SIN(AANGLE(ISDX)*DTORAD))/1000.
      AYVERT(4,ISDX) = AYVERT(3,ISDX) -
     &                (AYINIT(ISDX)*COS(AANGLE(ISDX)*DTORAD))/1000.

      AXVERT(5,ISDX) = AXS(ISDX)/1000.
      AYVERT(5,ISDX) = AYS(ISDX)/1000.

C*    Determine the angle of long pit dimension with North
      IF (AYINIT(ISDX) .GE. AXINIT(ISDX)) THEN
         AALPHA(ISDX) = AANGLE(ISDX)
      ELSE IF (AXINIT(ISDX) .GT. AYINIT(ISDX)) THEN
         AALPHA(ISDX) = AANGLE(ISDX) + 90.0
      ENDIF
      
C*    Calculate the effective pit depth
      APDEFF(ISDX) = AVOLUM(ISDX) / (AXINIT(ISDX) * AYINIT(ISDX))

C*    Calculate Initial Sigma-Z
      ASZINI(ISDX) = APDEFF(ISDX) / 4.3      

      RETURN
      END


      SUBROUTINE DSBLDG
C***********************************************************************
C                 DSBLDG Module of ISC2 Model
C
C        PURPOSE: Processes Direction-specific Building Directions
C
C        PROGRAMMER:  Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Direction Specific Building Directions
C
C        CALLED FROM:   SOCARD
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'
      CHARACTER LID*8, HID*8, LID1*8, LID2*8, HID1*8, HID2*8, SOID*40
      LOGICAL FIND, INGRP, RMARK

C     Variable Initializations
      FIND = .FALSE.
      INGRP =  .FALSE.
      MODNAM = 'DSBLDG'

C     Check The Number Of The Fields
      IF (IFC .LE. 2) THEN
C        Error Message: No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      ELSE IF (IFC .EQ. 3) THEN
C        Error Message: Not Enough Parameters
         CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
         GO TO 999
      END IF

C     Get The Source ID(s)
      SOID = FIELD(3)
      CALL FSPLIT(PATH,KEYWRD,SOID,40,'-',RMARK,LID,HID)

C     Verify The Effective Srcid
      IF (LID .EQ. HID) THEN
C        Search For The Index
         CALL SINDEX(SRCID,NSRC,SOID,ISDX,FIND)
         IF (FIND) THEN
            IF (SRCTYP(ISDX) .EQ. 'POINT') THEN
C              Fill Array
               CALL DSFILL(ISDX)
            ELSE
C              WRITE Warning Message: Building Inputs for Non-POINT Source
               CALL ERRHDL(PATH,MODNAM,'W','233',SRCID(ISDX))
            END IF
         ELSE
C           WRITE Error Message     ! Source Location Has Not Been Identified
            CALL ERRHDL(PATH,MODNAM,'E','300',KEYWRD)
         END IF
      ELSE
C        First Check Range for Upper Value < Lower Value
         CALL SETIDG(LID,LID1,IL,LID2)
         CALL SETIDG(HID,HID1,IH,HID2)
         IF ((HID1.LT.LID1) .OR. (IH.LT.IL) .OR. (HID2.LT.LID2)) THEN
C           WRITE Error Message:  Invalid Range,  Upper < Lower
            CALL ERRHDL(PATH,MODNAM,'E','203','SRCRANGE')
            GO TO 999
         END IF
         DO 20 I = 1, NUMSRC
C           See Whether It's In The Group
            CALL ASNGRP(SRCID(I),LID,HID,INGRP)
            IF (INGRP .AND. SRCTYP(I).EQ.'POINT') THEN
               ISDX = I
C              Fill DS Array
               CALL DSFILL(ISDX)
            END IF
 20      CONTINUE
      END IF

 999  RETURN
      END

      SUBROUTINE DSFILL(ISDX)
C***********************************************************************
C                 DSFILL Module of ISC2 Model
C
C        PURPOSE: Fill Direction-specific Building Dimension Arrays
C
C        PROGRAMMER:  Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Direction Specific Building Directions
C
C        CALLED FROM:   DSBLDG
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'

C     Variable Initializations
      MODNAM = 'DSFILL'

      IF (KEYWRD .EQ. 'BUILDHGT') THEN
         ISET = IWRK2(ISDX,1)
         DO 200 K = 4, IFC
C           Change Fields To Numbers
            CALL STONUM(FIELD(K),40,FNUM,IMIT)
C           Check The Numerical Field
            IF (IMIT .EQ. -1) THEN
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
               GO TO 200
            END IF
            DO 100 J = 1, IMIT
               ISET = ISET + 1
C              Assign The Field
               IF (ISET .LE. NSEC) THEN
                  ADSBH(ISET,ISDX) = FNUM
                  IF (FNUM .LT. 0.0) THEN
C                    WRITE Error Message:  Negative Value for ADSBH
                     CALL ERRHDL(PATH,MODNAM,'E','209',KEYWRD)
                  END IF
               ELSE
C                 WRITE Error Message    ! Too Many Sectors Input
                  CALL ERRHDL(PATH,MODNAM,'E','234',KEYWRD)
               END IF
 100        CONTINUE
 200     CONTINUE
         IWRK2(ISDX,1) = ISET
      ELSE IF (KEYWRD .EQ. 'BUILDWID') THEN
         ISET = IWRK2(ISDX,2)
         DO 400 K = 4, IFC
C           Change Fields To Numbers
            CALL STONUM(FIELD(K),40,FNUM,IMIT)
C           Check The Numerical Field
            IF (IMIT .EQ. -1) THEN
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
               GO TO 400
            END IF
            DO 300 J = 1, IMIT
               ISET = ISET + 1
C              Assign The Field
               IF (ISET .LE. NSEC) THEN
                  ADSBW(ISET,ISDX) = FNUM
                  IF (FNUM .LT. 0.0) THEN
C                    WRITE Error Message:  Negative Value for ADSBW
                     CALL ERRHDL(PATH,MODNAM,'E','209',KEYWRD)
                  END IF
               ELSE
C                 WRITE Error Message    ! Too Many Sectors Input
                  CALL ERRHDL(PATH,MODNAM,'E','234',KEYWRD)
               END IF
 300        CONTINUE
 400     CONTINUE
         IWRK2(ISDX,2) = ISET
c
c --- Additional PRIME building variables
      ELSE IF (KEYWRD .EQ. 'BUILDLEN') THEN
         ISET = IWRK2(ISDX,10)
         DO K = 4, IFC
C           Change Fields To Numbers
            CALL STONUM(FIELD(K),40,FNUM,IMIT)
C           Check The Numerical Field
            IF (IMIT .EQ. -1) THEN
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            ELSE
               DO J = 1, IMIT
                  ISET = ISET + 1
C                 Assign The Field
                  IF (ISET .LE. NSEC) THEN
                     ADSBL(ISET,ISDX) = FNUM
                     IF (FNUM .LT. 0.0) THEN
C                       WRITE Error Message:  Negative Value for ADSBL
                        CALL ERRHDL(PATH,MODNAM,'E','209',KEYWRD)
                     END IF
                  ELSE
C                    WRITE Error Message    ! Too Many Sectors Input
                     CALL ERRHDL(PATH,MODNAM,'E','234',KEYWRD)
                  END IF
               ENDDO
            END IF
         ENDDO
         IWRK2(ISDX,10) = ISET
      ELSE IF (KEYWRD .EQ. 'XBADJ   ') THEN
         ISET = IWRK2(ISDX,11)
         DO K = 4, IFC
C           Change Fields To Numbers
            CALL STONUM(FIELD(K),40,FNUM,IMIT)
C           Check The Numerical Field
            IF (IMIT .EQ. -1) THEN
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            ELSE
               DO J = 1, IMIT
                  ISET = ISET + 1
C                 Assign The Field
                  IF (ISET .LE. NSEC) THEN
                     ADSBX(ISET,ISDX) = FNUM
                  ELSE
C                    WRITE Error Message    ! Too Many Sectors Input
                     CALL ERRHDL(PATH,MODNAM,'E','234',KEYWRD)
                  END IF
               ENDDO
            END IF
         ENDDO
         IWRK2(ISDX,11) = ISET
      ELSE IF (KEYWRD .EQ. 'YBADJ   ') THEN
         ISET = IWRK2(ISDX,12)
         DO K = 4, IFC
C           Change Fields To Numbers
            CALL STONUM(FIELD(K),40,FNUM,IMIT)
C           Check The Numerical Field
            IF (IMIT .EQ. -1) THEN
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            ELSE
               DO J = 1, IMIT
                  ISET = ISET + 1
C                 Assign The Field
                  IF (ISET .LE. NSEC) THEN
                     ADSBY(ISET,ISDX) = FNUM
                  ELSE
C                    WRITE Error Message    ! Too Many Sectors Input
                     CALL ERRHDL(PATH,MODNAM,'E','234',KEYWRD)
                  END IF
               ENDDO
            END IF
         ENDDO
         IWRK2(ISDX,12) = ISET
c
      ELSE IF (KEYWRD .EQ. 'LOWBOUND') THEN
         ISET = IWRK2(ISDX,3)
         DO 600 K = 4, IFC
C           Change Fields To Numbers
            CALL STONUM(FIELD(K),40,FNUM,IMIT)
C           Check The Numerical Field
            IF (IMIT .EQ. -1) THEN
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
               GO TO 600
            END IF
            DO 500 J = 1, IMIT
               ISET = ISET + 1
C              Assign The Field
               IF (ISET .LE. NSEC) THEN
                  IDSWAK(ISET,ISDX) = INT(FNUM)
                  IF (INT(FNUM) .NE. 0 .AND. INT(FNUM) .NE. 1) THEN
C                    WRITE Error Message:  Invalid Parameter for IDSWAK
                     CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
                  END IF
               ELSE
C                 WRITE Error Message    ! Too Many Sectors Input
                  CALL ERRHDL(PATH,MODNAM,'E','234',KEYWRD)
               END IF
 500        CONTINUE
 600     CONTINUE
         IWRK2(ISDX,3) = ISET
      END IF

 999  RETURN
      END

      SUBROUTINE EMVARY
C***********************************************************************
C                 EMVARY Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Processes Variable Emission Rate Factors
C
C        PROGRAMMER:  Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Variable Emmission Rate Factors
C
C        CALLED FROM:   SOCARD
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'
      CHARACTER LID*8, HID*8, LID1*8, LID2*8, HID1*8, HID2*8, SOID*40
      LOGICAL FIND, INGRP, RMARK

C     Variable Initializations
      FIND = .FALSE.
      INGRP = .FALSE.
      MODNAM = 'EMVARY'

C     Check The Number Of The Fields
      IF (IFC .LE. 2) THEN
C        Error Message: No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      ELSE IF (IFC .EQ. 3) THEN
C        Error Message: No Numerical Parameters
         CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
         GO TO 999
      ELSE IF (IFC .LT. 5) THEN
C        Error Message: Not Enough Parameters
         CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
         GO TO 999
      END IF

C     Get The Source ID(s)
      SOID = FIELD(3)
      CALL FSPLIT(PATH,KEYWRD,SOID,40,'-',RMARK,LID,HID)

C     Verify The Effective Srcid
      IF (LID .EQ. HID) THEN
C        Search For The Index
         CALL SINDEX(SRCID,NSRC,SOID,ISDX,FIND)
         IF (FIND) THEN
            QFLAG(ISDX) = FIELD(4)
            IF (QFLAG(ISDX) .EQ. 'SEASON') THEN
               IQMAX = 4
            ELSE IF (QFLAG(ISDX) .EQ. 'MONTH') THEN
               IQMAX = 12
            ELSE IF (QFLAG(ISDX) .EQ. 'HROFDY') THEN
               IQMAX = 24
            ELSE IF (QFLAG(ISDX) .EQ. 'STAR') THEN
               IQMAX = 36
            ELSE IF (QFLAG(ISDX) .EQ. 'SEASHR') THEN
               IQMAX = 96
            ELSE
C              WRITE Error Message    ! Invalid QFLAG Field Entered
               CALL ERRHDL(PATH,MODNAM,'E','203','QFLAG')
            END IF
            IF (IQMAX .LE. NQF) THEN
               CALL EFFILL(ISDX,IQMAX)
            ELSE
C              WRITE Error Message     ! NQF Parameter Not Large Enough
               WRITE(DUMMY,'(I8)') NQF
               CALL ERRHDL(PATH,MODNAM,'E','260',DUMMY)
            END IF
         ELSE
C           WRITE Error Message     ! Source Location Has Not Been Identified
            CALL ERRHDL(PATH,MODNAM,'E','300',KEYWRD)
         END IF
      ELSE
C        First Check Range for Upper Value < Lower Value
         CALL SETIDG(LID,LID1,IL,LID2)
         CALL SETIDG(HID,HID1,IH,HID2)
         IF ((HID1.LT.LID1) .OR. (IH.LT.IL) .OR. (HID2.LT.LID2)) THEN
C           WRITE Error Message:  Invalid Range,  Upper < Lower
            CALL ERRHDL(PATH,MODNAM,'E','203','SRCRANGE')
            GO TO 999
         END IF
         DO 20 I = 1, NUMSRC
C           See Whether It's In The Group
            CALL ASNGRP(SRCID(I),LID,HID,INGRP)
            IF (INGRP) THEN
               ISDX = I
               QFLAG(ISDX) = FIELD(4)
               IF (QFLAG(ISDX) .EQ. 'SEASON') THEN
                  IQMAX = 4
               ELSE IF (QFLAG(ISDX) .EQ. 'MONTH') THEN
                  IQMAX = 12
               ELSE IF (QFLAG(ISDX) .EQ. 'HROFDY') THEN
                  IQMAX = 24
               ELSE IF (QFLAG(ISDX) .EQ. 'STAR') THEN
                  IQMAX = 36
               ELSE IF (QFLAG(ISDX) .EQ. 'SEASHR') THEN
                  IQMAX = 96
               ELSE
C                 WRITE Error Message    ! Invalid QFLAG Field Entered
                  CALL ERRHDL(PATH,MODNAM,'E','203','QFLAG')
               END IF
               IF (IQMAX .LE. NQF) THEN
                  CALL EFFILL(ISDX,IQMAX)
               ELSE
C                 WRITE Error Message    ! NQF Parameter Not Large Enough
                  WRITE(DUMMY,'(I8)') NQF
                  CALL ERRHDL(PATH,MODNAM,'E','260',DUMMY)
               END IF
            END IF
 20      CONTINUE
      END IF

 999  RETURN
      END

      SUBROUTINE EFFILL(ISDX,IQMAX)
C***********************************************************************
C                 EFFILL Module of ISC2 Model
C
C        PURPOSE: Fill Variable Emission Rate Array
C
C        PROGRAMMER:  Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Direction Specific Building Directions
C
C        CALLED FROM:   EMVARY
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'

C     Variable Initializations
      MODNAM = 'EFFILL'

      ISET = IWRK2(ISDX,4)

      DO 200 K = 5, IFC
C        Change Fields To Numbers
         CALL STONUM(FIELD(K),40,FNUM,IMIT)
C        Check The Numerical Field
         IF (IMIT.EQ.-1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            GO TO 200
         END IF
         DO 100 J = 1, IMIT
            ISET = ISET + 1
C           Assign The Field
            IF (ISET .LE. IQMAX) THEN
               QFACT(ISET,ISDX) = FNUM
               IF (FNUM .LT. 0.0) THEN
C                 WRITE Error Message:  Negative Value for QFACT
                  CALL ERRHDL(PATH,MODNAM,'E','209',KEYWRD)
               END IF
            ELSE
C              WRITE Error Message    ! Too Many QFACT Values Input
               CALL ERRHDL(PATH,MODNAM,'E','231','QFACT')
            END IF
 100     CONTINUE
 200  CONTINUE

      IWRK2(ISDX,4) = ISET

      RETURN
      END

      SUBROUTINE EMUNIT
C***********************************************************************
C                 EMUNIT Module of ISC2 Model
C
C        PURPOSE: Processes Emission Rate Unit Conversion Factors
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Emission Rate Unit Conversion Factors
C
C        CALLED FROM:   SOCARD
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'

C     Variable Initializations
      MODNAM = 'EMUNIT'

C     Check The Number Of The Fields
      IF (IFC .LE. 2) THEN
C        Error Message: No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      ELSE IF (IFC .LT. 5) THEN
C        Error Message: Not Enough Parameters
         CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
         GO TO 999
      ELSE IF (IFC .GT. 5) THEN
C        Error Message: Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
         GO TO 999
      END IF

C     Fetch Each Field
      CALL STONUM(FIELD(3),40,FNUM,IMIT)
C     Check The Numerical Field
      IF (IMIT .NE. 1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         GO TO 999
      END IF

      EMIFAC(1) = FNUM
      EMILBL(1) = FIELD(4)
      OUTLBL(1) = FIELD(5)

 999  RETURN
      END

      SUBROUTINE COUNIT
C***********************************************************************
C                 COUNIT Module of ISC2 Model
C
C        PURPOSE: Processes Emission Rate Unit Conversion Factors
C                 for CONCentration Values
C
C        PROGRAMMER: Roger Brode
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Emission Rate Unit Conversion Factors
C
C        CALLED FROM:   SOCARD
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'

C     Variable Initializations
      MODNAM = 'COUNIT'

C     Check The Number Of The Fields
      IF (IFC .LE. 2) THEN
C        Error Message: No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      ELSE IF (IFC .LT. 5) THEN
C        Error Message: Not Enough Parameters
         CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
         GO TO 999
      ELSE IF (IFC .GT. 5) THEN
C        Error Message: Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
         GO TO 999
      END IF

C     Fetch Each Field
      CALL STONUM(FIELD(3),40,FNUM,IMIT)
C     Check The Numerical Field
      IF (IMIT .NE. 1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         GO TO 999
      END IF

      EMIFAC(1) = FNUM
      EMILBL(1) = FIELD(4)
      OUTLBL(1) = FIELD(5)

 999  RETURN
      END

      SUBROUTINE DPUNIT
C***********************************************************************
C                 DPUNIT Module of ISC2 Model
C
C        PURPOSE: Processes Emission Rate Unit Conversion Factors
C                 for Deposition Values
C
C        PROGRAMMER: Roger Brode
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Emission Rate Unit Conversion Factors
C
C        CALLED FROM:   SOCARD
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'

C     Variable Initializations
      MODNAM = 'DPUNIT'

C     Check The Number Of The Fields
      IF (IFC .LE. 2) THEN
C        Error Message: No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      ELSE IF (IFC .LT. 5) THEN
C        Error Message: Not Enough Parameters
         CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
         GO TO 999
      ELSE IF (IFC .GT. 5) THEN
C        Error Message: Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
         GO TO 999
      END IF

C     Fetch Each Field
      CALL STONUM(FIELD(3),40,FNUM,IMIT)
C     Check The Numerical Field
      IF (IMIT .NE. 1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         GO TO 999
      END IF

      IF (.NOT. CONC) THEN
         DO I = 1, NTYP
            EMIFAC(I) = FNUM
            EMILBL(I) = FIELD(4)
            OUTLBL(I) = FIELD(5)
         END DO
      ELSE
         DO I = 2, NTYP
            EMIFAC(I) = FNUM
            EMILBL(I) = FIELD(4)
            OUTLBL(I) = FIELD(5)
         END DO
      END IF

 999  RETURN
      END

      SUBROUTINE PARTDEP
C***********************************************************************
C                 PARTDEP Module of ISC2 Model
C
C        ADAPTED from  DRYDEP Module of ISC2 Model
C        PROGRAMMER:  Jeff Wang, Roger Brode
C
C        PURPOSE: Processes Inputs for Wet & Dry PARTicle DEPosition
C
C        DRYDEP ADAPTED BY: D. Strimaitis, SRC (for Wet & Dry Deposition)
C        DATE:    November 8, 1993
C
C        DRYDEP MODIFIED BY: D. Strimaitis, SRC (for Dry Deposition)
C        (DATE:    February 15, 1993)
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Input For Setting and Removal Variables
C
C        CALLED FROM:   SOCARD
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'

C     Variable Initializations
      MODNAM = 'PARTDE'

C     Check The Number Of The Fields
      IF (IFC .LE. 2) THEN
C        Error Message: No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      ELSE IF (IFC .EQ. 3) THEN
C        Error Message: No Numerical Parameters
         CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
         GO TO 999
      END IF

C     Process The Appropriate Settling & Removal Parameter
      IF (KEYWRD .EQ. 'PARTDIAM') THEN
C        Process Particle Diameter Categories (PDIAM)       ---   CALL INPPDM
         CALL INPPDM
      ELSE IF (KEYWRD .EQ. 'MASSFRAX') THEN
C        Process Mass Fractions (PHI)                       ---   CALL INPPHI
         CALL INPPHI
      ELSE IF (KEYWRD .EQ. 'PARTDENS') THEN
C        Process Particle Density (PDENS)                   ---   CALL INPPDN
         CALL INPPDN
      ELSE IF (KEYWRD .EQ. 'PARTSLIQ') THEN
C        Process Wet(liquid) Scavenging Coefficient (PSLIQ) ---   CALL INPLSC
         CALL INPLSC
C        Set logical LWPART to indicate processing of wet deposition
C        of particles
         LWPART=.TRUE.
      ELSE IF (KEYWRD .EQ. 'PARTSICE') THEN
C        Process Wet(frozen) Scavenging Coefficient (PSICE) ---   CALL INPFSC
         CALL INPFSC
C        Set logical LWPART to indicate processing of wet deposition
C        of particles
         LWPART=.TRUE.
      END IF
C     Set logical LDPART to indicate processing of dry particle deposition
      LDPART=.TRUE.

 999  RETURN
      END

      SUBROUTINE INPPDM
C***********************************************************************
C                 INPPDM Module of ISC2 Model
C
C        PURPOSE: Processes Particle Diameter Categories
C
C        PROGRAMMER: D. Strimaitis, SRC
C
C        ADAPTED FROM "INPVSN"
C        PROGRAMMER: Jeff Wang, Roger Brode
C
C        DATE:    February 15, 1993
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Particle Diameter Categories
C
C        CALLED FROM:   PARTDEP
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'
      CHARACTER LID*8, HID*8, LID1*8, LID2*8, HID1*8, HID2*8, SOID*40
      LOGICAL FIND, INGRP, RMARK

C     Variable Initializations
      FIND   = .FALSE.
      INGRP  = .FALSE.
      MODNAM = 'INPPDM'

C     Get The Source ID(s)
      SOID = FIELD(3)
      CALL FSPLIT(PATH,KEYWRD,SOID,40,'-',RMARK,LID,HID)

      IF (LID .EQ. HID) THEN
C        Search For The Index
         CALL SINDEX(SRCID,NSRC,SOID,ISDX,FIND)
         IF (FIND) THEN
            ISET = IWRK2(ISDX,5)
            DO 36 K = 4, IFC
C              Change It To Numbers
               CALL STONUM(FIELD(K),40,FNUM,IMIT)
C              Check The Numerical Field
               IF (IMIT .EQ. -1) THEN
                  CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
                  GO TO 36
               END IF
               DO 35 J = 1, IMIT
                  ISET = ISET + 1
                  IF (ISET .LE. NPDMAX) THEN
C                    Assign The Field
                     APDIAM(ISET,ISDX) = FNUM
                  ELSE
C                    WRITE Error Message: Too Many PartDiam Categories
                     WRITE(DUMMY,'(I8)') NPDMAX
                     CALL ERRHDL(PATH,MODNAM,'E','245',DUMMY)
                  END IF
 35            CONTINUE
 36         CONTINUE
            IWRK2(ISDX,5) = ISET
         ELSE
C           WRITE Error Message     ! Source Location Has Not Been Identified
            CALL ERRHDL(PATH,MODNAM,'E','300',KEYWRD)
         END IF
      ELSE
C        First Check Range for Upper Value < Lower Value
         CALL SETIDG(LID,LID1,IL,LID2)
         CALL SETIDG(HID,HID1,IH,HID2)
         IF ((HID1.LT.LID1) .OR. (IH.LT.IL) .OR. (HID2.LT.LID2)) THEN
C           WRITE Error Message:  Invalid Range,  Upper < Lower
            CALL ERRHDL(PATH,MODNAM,'E','203','SRCRANGE')
            GO TO 999
         END IF
         DO 26 I = 1, NUMSRC
C           See Whether It's In The Group
            CALL ASNGRP(SRCID(I),LID,HID,INGRP)
            IF (INGRP) THEN
               IING = I
               ISET = IWRK2(IING,5)
               DO 25 K = 4, IFC
C                 Get Numbers
                  CALL STONUM(FIELD(K),40,FNUM,IMIT)
C                 Check The Numerical Field
                  IF (IMIT .EQ. -1) THEN
                     CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
                     GO TO 26
                  END IF
                  DO 20 J = 1, IMIT
                     ISET = ISET + 1
                     IF (ISET .LE. NPDMAX) THEN
                        APDIAM(ISET,I) = FNUM
                     ELSE
C                       WRITE Error Message: Too Many PartDiam Categories
                        WRITE(DUMMY,'(I8)') NPDMAX
                        CALL ERRHDL(PATH,MODNAM,'E','245',DUMMY)
                     END IF
 20               CONTINUE
 25            CONTINUE
               IWRK2(IING,5) = ISET
            END IF
 26      CONTINUE
      END IF

 999  RETURN
      END

      SUBROUTINE INPPHI
C***********************************************************************
C                 INPPHI Module of ISC2 Model
C
C        PURPOSE: Processes Mass Fraction (PHI) Input Values
C
C        PROGRAMMER:  Jeff Wang, Roger Brode
C        MODIFIED BY: D. Strimaitis, SRC
C
C        DATE:    February 15, 1993
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Mass Fraction Input Values
C
C        CALLED FROM:   PARTDEP
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'
      CHARACTER LID*8, HID*8, LID1*8, LID2*8, HID1*8, HID2*8, SOID*40
      LOGICAL FIND, INGRP, RMARK

C     Variable Initializations
      FIND = .FALSE.
      INGRP =  .FALSE.
      MODNAM = 'INPPHI'

C     Get The Source ID(s)
      SOID = FIELD(3)
      CALL FSPLIT(PATH,KEYWRD,SOID,40,'-',RMARK,LID,HID)

      IF (LID .EQ. HID) THEN
C        Search For The Index
         CALL SINDEX(SRCID,NSRC,SOID,ISDX,FIND)
         IF (FIND) THEN
            ISET = IWRK2(ISDX,6)
            DO 36 K = 4, IFC
C              Change It To Numbers
               CALL STONUM(FIELD(K),40,FNUM,IMIT)
C              Check The Numerical Field
               IF (IMIT .EQ. -1) THEN
                  CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
                  GO TO 36
               END IF
               IF (FNUM .LT. 0.0 .OR. FNUM .GT. 1.0) THEN
C                 WRITE Error Message: Mass Fraction Out-of-Range
                  CALL ERRHDL(PATH,MODNAM,'E','332',SRCID(ISDX))
               END IF
               DO 35 J = 1, IMIT
                  ISET = ISET + 1
                  IF (ISET .LE. NPDMAX) THEN
C                    Assign The Field
                     APHI(ISET,ISDX) = FNUM
                  ELSE
C                    WRITE Error Message: Too Many PartDiam Categories
                     WRITE(DUMMY,'(I8)') NPDMAX
                     CALL ERRHDL(PATH,MODNAM,'E','245',DUMMY)
                  END IF
 35            CONTINUE
 36         CONTINUE
            IWRK2(ISDX,6) = ISET
         ELSE
C           WRITE Error Message     ! Source Location Has Not Been Identified
            CALL ERRHDL(PATH,MODNAM,'E','300',KEYWRD)
         END IF
      ELSE
C        First Check Range for Upper Value < Lower Value
         CALL SETIDG(LID,LID1,IL,LID2)
         CALL SETIDG(HID,HID1,IH,HID2)
         IF ((HID1.LT.LID1) .OR. (IH.LT.IL) .OR. (HID2.LT.LID2)) THEN
C           WRITE Error Message:  Invalid Range,  Upper < Lower
            CALL ERRHDL(PATH,MODNAM,'E','203','SRCRANGE')
            GO TO 999
         END IF
         DO 26 I = 1, NUMSRC
C           See Whether It's In The Group
            CALL ASNGRP(SRCID(I),LID,HID,INGRP)
            IF (INGRP) THEN
               IING = I
               ISET = IWRK2(IING,6)
               DO 25 K = 4, IFC
C                 Get Numbers
                  CALL STONUM(FIELD(K),40,FNUM,IMIT)
C                 Check The Numerical Field
                  IF (IMIT .EQ. -1) THEN
                     CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
                     GO TO 26
                  END IF
                  IF (FNUM .LT. 0.0 .OR. FNUM .GT. 1.0) THEN
C                    WRITE Error Message: Mass Fraction Out-of-Range
                     CALL ERRHDL(PATH,MODNAM,'E','332',SRCID(ISDX))
                  END IF
                  DO 20 J = 1, IMIT
                     ISET = ISET + 1
                     IF (ISET .LE. NPDMAX) THEN
                        APHI(ISET,I) = FNUM
                     ELSE
C                       WRITE Error Message: Too Many PartDiam Categories
                        WRITE(DUMMY,'(I8)') NPDMAX
                        CALL ERRHDL(PATH,MODNAM,'E','245',DUMMY)
                     END IF
 20               CONTINUE
 25            CONTINUE
               IWRK2(IING,6) = ISET
            END IF
 26      CONTINUE
      END IF

 999  RETURN
      END

      SUBROUTINE INPPDN
C***********************************************************************
C                 INPPDN Module of ISC2 Model
C
C        PURPOSE: Processes Particle Density Input Values
C
C        PROGRAMMER:  D. Strimaitis, SRC
C
C        ADAPTED FROM "INPGAM"
C        PROGRAMMER:  Jeff Wang, Roger Brode
C
C        DATE:    February 15, 1993
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Particle Density Input Values
C
C        CALLED FROM:   PARTDEP
C***********************************************************************
C
C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'
      CHARACTER LID*8, HID*8, LID1*8, LID2*8, HID1*8, HID2*8, SOID*40
      LOGICAL FIND, INGRP, RMARK

C     Variable Initializations
      FIND = .FALSE.
      INGRP =  .FALSE.
      MODNAM = 'INPPDN'

C     Get The Source ID(s)
      SOID = FIELD(3)
      CALL FSPLIT(PATH,KEYWRD,SOID,40,'-',RMARK,LID,HID)

      IF (LID .EQ. HID) THEN
C        Search For The Index
         CALL SINDEX(SRCID,NSRC,SOID,ISDX,FIND)
         IF (FIND) THEN
            ISET = IWRK2(ISDX,7)
            DO 36 K = 4, IFC
C              Change It To Numbers
               CALL STONUM(FIELD(K),40,FNUM,IMIT)
C              Check The Numerical Field
               IF (IMIT .EQ. -1) THEN
                  CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
                  GO TO 36
               END IF
               IF (FNUM .LE. 0.0) THEN
C                 WRITE Error Message: Particle Density Out-of-Range
                  CALL ERRHDL(PATH,MODNAM,'E','334',SRCID(ISDX))
               END IF
               DO 35 J = 1, IMIT
                  ISET = ISET + 1
                  IF (ISET .LE. NPDMAX) THEN
C                    Assign The Field
                     APDENS(ISET,ISDX) = FNUM
                  ELSE
C                    WRITE Error Message: Too Many PartDiam Categories
                     WRITE(DUMMY,'(I8)') NPDMAX
                     CALL ERRHDL(PATH,MODNAM,'E','245',DUMMY)
                  END IF
 35            CONTINUE
 36         CONTINUE
            IWRK2(ISDX,7) = ISET
         ELSE
C           WRITE Error Message     ! Source Location Has Not Been Identified
            CALL ERRHDL(PATH,MODNAM,'E','300',KEYWRD)
         END IF
      ELSE
C        First Check Range for Upper Value < Lower Value
         CALL SETIDG(LID,LID1,IL,LID2)
         CALL SETIDG(HID,HID1,IH,HID2)
         IF ((HID1.LT.LID1) .OR. (IH.LT.IL) .OR. (HID2.LT.LID2)) THEN
C           WRITE Error Message:  Invalid Range,  Upper < Lower
            CALL ERRHDL(PATH,MODNAM,'E','203','SRCRANGE')
            GO TO 999
         END IF
         DO 26 I = 1, NUMSRC
C           See Whether It's In The Group
            CALL ASNGRP(SRCID(I),LID,HID,INGRP)
            IF (INGRP) THEN
               IING = I
               ISET = IWRK2(IING,7)
               DO 25 K = 4, IFC
C                 Get Numbers
                  CALL STONUM(FIELD(K),40,FNUM,IMIT)
C                 Check The Numerical Field
                  IF (IMIT .NE. 1) THEN
                     CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
                     GO TO 26
                  END IF
                  IF (FNUM .LE. 0.0) THEN
C                    WRITE Error Message: Particle Density Out-of-Range
                     CALL ERRHDL(PATH,MODNAM,'E','334',SRCID(ISDX))
                  END IF
                  DO 20 J = 1, IMIT
                     ISET = ISET + 1
                     IF (ISET .LE. NPDMAX) THEN
                        APDENS(ISET,I) = FNUM
                     ELSE
C                       WRITE Error Message: Too Many PartDiam Categories
                        WRITE(DUMMY,'(I8)') NPDMAX
                        CALL ERRHDL(PATH,MODNAM,'E','245',DUMMY)
                     END IF
 20               CONTINUE
 25            CONTINUE
               IWRK2(IING,7) = ISET
            END IF
 26      CONTINUE
      END IF

 999  RETURN
      END

      SUBROUTINE INPLSC
C***********************************************************************
C                 INPLSC Module of ISC2 Model
C
C        PURPOSE: Processes Wet Scavenging Coefficients for Particles
C                 -- Liquid Precipitation --
C
C        PROGRAMMER: D. Strimaitis, SRC
C
C        ADAPTED FROM "INPVSN"
C        PROGRAMMER: Jeff Wang, Roger Brode
C
C        DATE:    November 8, 1993
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Wet Scavenging Coefficient for each Particle Category
C
C        CALLED FROM:   PARTDEP
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'
      CHARACTER LID*8, HID*8, LID1*8, LID2*8, HID1*8, HID2*8, SOID*40
      LOGICAL FIND, INGRP, RMARK

C     Variable Initializations
      FIND   = .FALSE.
      INGRP  = .FALSE.
      MODNAM = 'INPLSC'

C     Get The Source ID(s)
      SOID = FIELD(3)
      CALL FSPLIT(PATH,KEYWRD,SOID,40,'-',RMARK,LID,HID)

      IF (LID .EQ. HID) THEN
C        Search For The Index
         CALL SINDEX(SRCID,NSRC,SOID,ISDX,FIND)
         IF (FIND) THEN
            ISET = IWRK2(ISDX,8)
            DO 36 K = 4, IFC
C              Change It To Numbers
               CALL STONUM(FIELD(K),40,FNUM,IMIT)
C              Check The Numerical Field
               IF (IMIT .EQ. -1) THEN
                  CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
                  GO TO 36
               END IF
               DO 35 J = 1, IMIT
                  ISET = ISET + 1
                  IF (ISET .LE. NPDMAX) THEN
C                    Assign The Field
                     APSLIQ(ISET,ISDX) = FNUM
                  ELSE
C                    WRITE Error Message: Too Many PartDiam Categories
                     WRITE(DUMMY,'(I8)') NPDMAX
                     CALL ERRHDL(PATH,MODNAM,'E','245',DUMMY)
                  END IF
 35            CONTINUE
 36         CONTINUE
            IWRK2(ISDX,8) = ISET
         ELSE
C           WRITE Error Message     ! Source Location Has Not Been Identified
            CALL ERRHDL(PATH,MODNAM,'E','300',KEYWRD)
         END IF
      ELSE
C        First Check Range for Upper Value < Lower Value
         CALL SETIDG(LID,LID1,IL,LID2)
         CALL SETIDG(HID,HID1,IH,HID2)
         IF ((HID1.LT.LID1) .OR. (IH.LT.IL) .OR. (HID2.LT.LID2)) THEN
C           WRITE Error Message:  Invalid Range,  Upper < Lower
            CALL ERRHDL(PATH,MODNAM,'E','203','SRCRANGE')
            GO TO 999
         END IF
         DO 26 I = 1, NUMSRC
C           See Whether It's In The Group
            CALL ASNGRP(SRCID(I),LID,HID,INGRP)
            IF (INGRP) THEN
               IING = I
               ISET = IWRK2(IING,8)
               DO 25 K = 4, IFC
C                 Get Numbers
                  CALL STONUM(FIELD(K),40,FNUM,IMIT)
C                 Check The Numerical Field
                  IF (IMIT .EQ. -1) THEN
                     CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
                     GO TO 26
                  END IF
                  DO 20 J = 1, IMIT
                     ISET = ISET + 1
                     IF (ISET .LE. NPDMAX) THEN
                        APSLIQ(ISET,I) = FNUM
                     ELSE
C                       WRITE Error Message: Too Many PartDiam Categories
                        WRITE(DUMMY,'(I8)') NPDMAX
                        CALL ERRHDL(PATH,MODNAM,'E','245',DUMMY)
                     END IF
 20               CONTINUE
 25            CONTINUE
               IWRK2(IING,8) = ISET
            END IF
 26      CONTINUE
      END IF

 999  RETURN
      END

      SUBROUTINE INPFSC
C***********************************************************************
C                 INPFSC Module of ISC2 Model
C
C        PURPOSE: Processes Wet Scavenging Coefficients for Particles
C                 -- Frozen Precipitation --
C
C        PROGRAMMER: D. Strimaitis, SRC
C
C        ADAPTED FROM "INPVSN"
C        PROGRAMMER: Jeff Wang, Roger Brode
C
C        DATE:    November 8, 1993
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Wet Scavenging Coefficient for each Particle Category
C
C        CALLED FROM:   PARTDEP
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'
      CHARACTER LID*8, HID*8, LID1*8, LID2*8, HID1*8, HID2*8, SOID*40
      LOGICAL FIND, INGRP, RMARK

C     Variable Initializations
      FIND   = .FALSE.
      INGRP  = .FALSE.
      MODNAM = 'INPFSC'

C     Get The Source ID(s)
      SOID = FIELD(3)
      CALL FSPLIT(PATH,KEYWRD,SOID,40,'-',RMARK,LID,HID)

      IF (LID .EQ. HID) THEN
C        Search For The Index
         CALL SINDEX(SRCID,NSRC,SOID,ISDX,FIND)
         IF (FIND) THEN
            ISET = IWRK2(ISDX,9)
            DO 36 K = 4, IFC
C              Change It To Numbers
               CALL STONUM(FIELD(K),40,FNUM,IMIT)
C              Check The Numerical Field
               IF (IMIT .EQ. -1) THEN
                  CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
                  GO TO 36
               END IF
               DO 35 J = 1, IMIT
                  ISET = ISET + 1
                  IF (ISET .LE. NPDMAX) THEN
C                    Assign The Field
                     APSICE(ISET,ISDX) = FNUM
                  ELSE
C                    WRITE Error Message: Too Many PartDiam Categories
                     WRITE(DUMMY,'(I8)') NPDMAX
                     CALL ERRHDL(PATH,MODNAM,'E','245',DUMMY)
                  END IF
 35            CONTINUE
 36         CONTINUE
            IWRK2(ISDX,9) = ISET
         ELSE
C           WRITE Error Message     ! Source Location Has Not Been Identified
            CALL ERRHDL(PATH,MODNAM,'E','300',KEYWRD)
         END IF
      ELSE
C        First Check Range for Upper Value < Lower Value
         CALL SETIDG(LID,LID1,IL,LID2)
         CALL SETIDG(HID,HID1,IH,HID2)
         IF ((HID1.LT.LID1) .OR. (IH.LT.IL) .OR. (HID2.LT.LID2)) THEN
C           WRITE Error Message:  Invalid Range,  Upper < Lower
            CALL ERRHDL(PATH,MODNAM,'E','203','SRCRANGE')
            GO TO 999
         END IF
         DO 26 I = 1, NUMSRC
C           See Whether It's In The Group
            CALL ASNGRP(SRCID(I),LID,HID,INGRP)
            IF (INGRP) THEN
               IING = I
               ISET = IWRK2(IING,9)
               DO 25 K = 4, IFC
C                 Get Numbers
                  CALL STONUM(FIELD(K),40,FNUM,IMIT)
C                 Check The Numerical Field
                  IF (IMIT .EQ. -1) THEN
                     CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
                     GO TO 26
                  END IF
                  DO 20 J = 1, IMIT
                     ISET = ISET + 1
                     IF (ISET .LE. NPDMAX) THEN
                        APSICE(ISET,I) = FNUM
                     ELSE
C                       WRITE Error Message: Too Many PartDiam Categories
                        WRITE(DUMMY,'(I8)') NPDMAX
                        CALL ERRHDL(PATH,MODNAM,'E','245',DUMMY)
                     END IF
 20               CONTINUE
 25            CONTINUE
               IWRK2(IING,9) = ISET
            END IF
 26      CONTINUE
      END IF

 999  RETURN
      END

      SUBROUTINE GASDEP
C***********************************************************************
C                 GASDEP Module of ISC2 Model
C
C        PROGRAMMER: D. Strimaitis, SRC
C
C        PURPOSE: Processes Inputs for Wet & Dry GAS DEPosition
C
C        DATE:    November 8, 1993
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Input For Gaseous Removal Variables
C
C        CALLED FROM:   SOCARD
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'

C     Variable Initializations
      MODNAM = 'GASDEP'

C     Process Keyword
      IF (KEYWRD .EQ. 'GAS-SCAV') THEN
C        Process Wet Scavending Coefficients (AGSCAV)       ---   CALL INPGSC
         CALL INPGSC
      END IF
C     Set logical LWGAS to indicate processing of Wet removal of gases
      LWGAS=.TRUE.

 999  RETURN
      END

      SUBROUTINE INPGSC
C***********************************************************************
C                 INPGSC Module of ISC2 Model
C
C        PURPOSE: Processes Scavenging Parameters for Gases
C
C        PROGRAMMER: D. Strimaitis, SRC
C
C        DATE:    November 8, 1993
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Wet Scavenging Coefficients for Gases
C
C        CALLED FROM:   GASDEP
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'
      CHARACTER LID*8, HID*8, LID1*8, LID2*8, HID1*8, HID2*8, SOID*40
      LOGICAL FIND, INGRP, RMARK

C     Variable Initializations
      FIND   = .FALSE.
      INGRP  = .FALSE.
      MODNAM = 'INPGSC'

C     Check the Number of Fields
      IF (IFC .LE. 2) THEN
C        Error Message: No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      ELSE IF (IFC .LT. 5) THEN
C        Error Message: Not Enough Parameters
         CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
         GO TO 999
      ELSE IF (IFC .GT. 5) THEN
C        Error Message: Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
         GO TO 999
      END IF

C     Get The Source ID(s)
      SOID = FIELD(3)
      CALL FSPLIT(PATH,KEYWRD,SOID,40,'-',RMARK,LID,HID)

      IF (LID .EQ. HID) THEN
C        Search For The Index
         CALL SINDEX(SRCID,NSRC,SOID,ISDX,FIND)
         IF (FIND) THEN
C           Read Secondary Parameter for Either LIQ (liquid) or
C           ICE (frozen) Precipitation, and assign index accordingly
            IF (FIELD(4) .EQ. 'LIQ') THEN
               ipindex=1
            ELSE IF (FIELD(4) .EQ. 'ICE') THEN
               ipindex=2
            ELSE
C              Error Message: Invalid Precipitation Type
               CALL ERRHDL(PATH,MODNAM,'E','203','PRECIP')
               GO TO 999
            END IF
C           Read Scavenging Coef.
C           Change it to Numbers
            CALL STONUM(FIELD(5),40,FNUM,IMIT)
C           Check The Numerical Field
            IF (IMIT .EQ. -1) THEN
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
               GO TO 999
            END IF
C           Assign The Field
            AGSCAV(IPINDEX,ISDX) = FNUM
         ELSE
C           WRITE Error Message     ! Source Location Has Not Been Identified
            CALL ERRHDL(PATH,MODNAM,'E','300',KEYWRD)
         END IF
      ELSE
C        First Check Range for Upper Value < Lower Value
         CALL SETIDG(LID,LID1,IL,LID2)
         CALL SETIDG(HID,HID1,IH,HID2)
         IF ((HID1.LT.LID1) .OR. (IH.LT.IL) .OR. (HID2.LT.LID2)) THEN
C           WRITE Error Message:  Invalid Range,  Upper < Lower
            CALL ERRHDL(PATH,MODNAM,'E','203','SRCRANGE')
            GO TO 999
         END IF
         DO 26 I = 1, NUMSRC
C           See Whether It's In The Group
            CALL ASNGRP(SRCID(I),LID,HID,INGRP)
            IF (INGRP) THEN
               IING = I
C              Read Secondary Parameter for Either LIQ (liquid) or
C              ICE (frozen) Precipitation, and assign index accordingly
               IF (FIELD(4) .EQ. 'LIQ') THEN
                  ipindex=1
               ELSE IF (FIELD(4) .EQ. 'ICE') THEN
                  ipindex=2
               ELSE
C                 Error Message: Invalid Precipitation Type
                  CALL ERRHDL(PATH,MODNAM,'E','203','PRECIP')
                  GO TO 999
               END IF
C              Read Scavenging Coef.
C              Change it to Numbers
               CALL STONUM(FIELD(5),40,FNUM,IMIT)
C              Check The Numerical Field
               IF (IMIT .EQ. -1) THEN
                  CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
                  GO TO 999
               END IF
C              Assign The Field
               AGSCAV(IPINDEX,IING) = FNUM
            END IF
 26      CONTINUE
      END IF

 999  RETURN
      END

      SUBROUTINE SOGRP
C***********************************************************************
C                 SOGRP Module of ISC2 Model
C
C        PURPOSE: Processes Source Group Inputs for Pass One
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Source Group Input For Pass One
C
C        CALLED FROM: SOCARD
C***********************************************************************
C

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'
      CHARACTER*8 LOWID, HIGID, LID1, LID2, HID1, HID2, TEMPID
      LOGICAL CONT, INGRP, RMARK

C     Variable Initializations
      CONT   = .FALSE.
      MODNAM = 'SOGRP'

C     Check The Number Of The Fields
      IF (IFC .LE. 2) THEN
C        Error Message: No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      ELSE IF (IFC .LE. 3 .AND. FIELD(3) .NE. 'ALL') THEN
C        Error Message: Not Enough Parameters
         CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
         GO TO 999
      END IF

C     READ in the Group ID and Check for Continuation Card
      TEMPID = FIELD(3)
      DO 20 I = 1, NUMGRP
         IF (TEMPID .EQ. GRPID(I)) THEN
            CONT = .TRUE.
         END IF
 20   CONTINUE

C     Increment Counters and Assign Group ID If Not a Continuation Card
      IF (.NOT. CONT) THEN
         IGRP = IGRP + 1
         IF (IGRP .GT. NGRP) THEN
C           WRITE Error Message    ! Too Many Source Groups Specified
            WRITE(DUMMY,'(I8)') NGRP
            CALL ERRHDL(PATH,MODNAM,'E','235',DUMMY)
C           Exit to END
            GO TO 999
         END IF
         NUMGRP = NUMGRP + 1
         GRPID(IGRP) = TEMPID
      END IF

C     Set Up The Source Group Array
      IF (GRPID(IGRP) .EQ. 'ALL' .AND. .NOT.CONT) THEN
         DO 30 I = 1, NUMSRC
            IGROUP(I,IGRP) = 1
 30      CONTINUE
      ELSE
C        Loop Through Fields
         DO 50 I = 4, IFC
            CALL FSPLIT(PATH,KEYWRD,FIELD(I),40,'-',RMARK,LOWID,HIGID)
C           First Check Range for Upper Value < Lower Value
            CALL SETIDG(LOWID,LID1,IL,LID2)
            CALL SETIDG(HIGID,HID1,IH,HID2)
            IF ((HID1.LT.LID1) .OR. (IH.LT.IL) .OR. (HID2.LT.LID2)) THEN
C              WRITE Error Message:  Invalid Range,  Upper < Lower
               CALL ERRHDL(PATH,MODNAM,'E','203','SRCRANGE')
               GO TO 50
            END IF
            DO 40 K = 1, NUMSRC
               CALL ASNGRP(SRCID(K),LOWID,HIGID,INGRP)
               IF (INGRP) THEN
                  IGROUP(K,IGRP) = 1
               END IF
 40         CONTINUE
 50      CONTINUE
      END IF

 999  RETURN
      END

      SUBROUTINE ASNGRP(INID,LOWID,HIGID,INGRP)
C***********************************************************************
C                 ASNGRP Module of ISC2 Model
C
C        PURPOSE: Find Whether A Source ID is In The Specific Group
C
C        PROGRAMMER: Roger Brode, Jeff Wang, Kevin Stroupe
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Field Parameters
C
C        OUTPUTS: Indicator for Source ID in The Group
C
C        CALLED FROM: (This is An Utility Program)
C***********************************************************************
C
C     Variable Declarations
      CHARACTER LOWID*8, HIGID*8, INID*8, IID1*8, LID1*8, HID1*8,
     &          PATH*2, MODNAM*6, IID2*8, LID2*8, HID2*8
      INTEGER IN, IL, IH
      LOGICAL INGRP

C     Variable Initializations
      MODNAM = 'ASNGRP'
      PATH = 'SO'
      INGRP = .FALSE.

C     Extract The Character Field And Numerical Field
      CALL SETIDG(INID,IID1,IN,IID2)
      CALL SETIDG(LOWID,LID1,IL,LID2)
      CALL SETIDG(HIGID,HID1,IH,HID2)

C     Do Comparisons of Character and Numeric Fields, All Must Satisfy Ranges
      IF ((IID1.GE.LID1 .AND. IID1.LE.HID1) .AND.
     &        (IN.GE.IL .AND. IN.LE.IH) .AND.
     &    (IID2.GE.LID2 .AND. IID2.LE.HID2)) THEN
         INGRP = .TRUE.
      END IF

      RETURN
      END

      SUBROUTINE SETIDG(INID,IDCHR1,IDNUM,IDCHR2)
C***********************************************************************
C                 SETIDG Module of ISC2 Model
C
C        PURPOSE: Find A Source ID's Character Part and
C                 Numerical Part
C
C        PROGRAMMER: Jeff Wang, Roger Brode, Kevin Stroupe
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Field Parameters
C
C        OUTPUTS: An Initial Character String, a Number, and
C                 a Second Character String
C
C        CALLED FROM: (This is An Utility Program)
C***********************************************************************
C
C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'
      CHARACTER INID*8, IDCHR1*8, IDCHR2*8, CHKI, NUMID*40, ZERO, BLNK
      LOGICAL HIT

C     Variable Initializations
      MODNAM = 'SETIDG'
      I = 8
      NUMID  = ' '
      IDCHR1 = ' '
      IDCHR2 = ' '
      IDNUM  = 0
      HIT    = .FALSE.
      ZERO   = '0'
      BLNK   = ' '

C     Find The Length of the Input Field, II (<= 8)
      DO WHILE (.NOT.HIT .AND. I.GE.1)
         CHKI = INID(I:I)
         IF (CHKI .NE. ' ') THEN
            II = I
            HIT = .TRUE.
         END IF
         I = I - 1
      END DO

C     Divide the Input Id into 3 parts (char1, int, and char2)
      I = 1
      ISTR = I
      CHKI = INID(I:I)
C     Get first character part
      DO WHILE (CHKI .LT. '0' .OR. CHKI .GT. '9')
         IDCHR1 = INID(ISTR:I)
         I = I + 1
         IF (I .GT. II) THEN
            GO TO 20
         ELSE
            CHKI = INID(I:I)
         END IF
      END DO

C     Get integer part
      ISTR = I
      DO WHILE (CHKI .GE. '0' .AND. CHKI .LE. '9')
         NUMID = INID(ISTR:I)
         I = I + 1
         IF (I .GT. II) THEN
            GO TO 20
         ELSE
            CHKI = INID(I:I)
         END IF
      END DO

C     Get second character part
      ISTR = I
      DO WHILE (I .LE. II)
         IDCHR2 = INID(ISTR:I)
         I = I + 1
         IF (I .GT. II) THEN
            GO TO 20
         ELSE
            CHKI = INID(I:I)
         END IF
      END DO

 20   CONTINUE

C     Convert Numeric Part to Integer Variable
      CALL STONUM(NUMID,40,FNUM,IMIT)
      IDNUM = INT(FNUM)

 991  FORMAT(A40)
 992  FORMAT(A8)

      RETURN
      END

c----------------------------------------------------------------------
      subroutine vdp1
c----------------------------------------------------------------------
c
c --- ISC2ST     Version:  1.0     Level:  930215                  VDP1
c                J. Scire, SRC
c
c --- PURPOSE:  Setup routine for PARTICLE dry deposition.
c               Completes particle common block /SOURC4/.  Performs
c               initialization and time-invariant calculations.
c
c --- INPUTS:
c     Common block /SOURC4/ variables:
c              INPD - integer    - Number of particle size categories
c            APDIAM - real array - Mean diameter (microns) of each
c                                  particle size category
c              APHI - real array - Mass fraction in each size category
c            APDENS - real       - Particle density (g/cm**3)
c
c --- OUTPUT:
c     Common block /SOURC4/ variables:
c               ASC - real array - Schmidt number
c            AVGRAV - real array - Gravitational settling velocity (m/s)
c            ATSTOP - real array - Stopping time (s)
c            VAIRMS - real       - Viscosity of air (m**2/s)
c             ZRDEP - real       - Reference height (m) for Deposition
c            VDPHOR - real       - Phoretic effects term (m/s)
c
c --- VDP1 called by:  SOCARD
c --- VDP1 calls:      none
c----------------------------------------------------------------------
c
      INCLUDE 'MAIN1.INC'

      data a1/1.257/,a2/0.4/,a3/0.55/,xmfp/6.5e-6/
      data vcon/1.81e-4/,xk/1.38e-16/
      data vair/0.15/,gcgs/981./,rhoair/1.2e-3/,tair/293.15/
c
      io6=iounit
c ***
      if(DEBUG)then
         write(io6,*)
         write(io6,*)'SUBR. VDP1 -- INPUTS'
         write(io6,*)
         do 5 i=1,numsrc
         write(io6,*)'SOURCE          = ',i
         write(io6,*)'INPD            = ',inpd(i)
         write(io6,*)'APDIAM (um)     = ',(apdiam(n,i),n=1,inpd(i))
         write(io6,*)'APDIAM (um)     = ',(apdiam(n,i),n=1,inpd(i))
         write(io6,*)'APHI            = ',(aphi(n,i),n=1,inpd(i))
         write(io6,*)'APDENS(g/cm**3) = ',(apdens(n,i),n=1,inpd(i))
         write(io6,*)
5        continue
      endif
c ***
c
c --- Convert viscosity of air (at 20 deg C) from cm**2/s to m**2/s
      vairms=1.e-4*vair

CRWB  Reference Height is now a function of Z0M, which may change hourly.
CRWB  It is now calculated in SUB. VDP of CALC1.FOR.
CRWBc
CRWBc --- Set reference height for aerodynamic resistance calculation
CRWB      zrdep=1.0
c
c --- Define phoretic effects term (m/s)
      vdphor=0.0001
c
c --  LOOP over sources
      do 25 j=1,numsrc
c
c --- LOOP over "INPD" size intervals if non-zero
         if(inpd(j) .LE. 0) goto 25
         do 20 i=1,inpd(j)
c
c ---       Slip correction factor
            diamcm=1.e-4*apdiam(i,j)
            scf=1.+2.0*xmfp*(a1+a2*exp(-a3*diamcm/xmfp))/diamcm
c
c ---       Stokes friction coefficient
            sfc=3.*pi*vcon*diamcm/scf
c
c ---       Diffusivity (cm**2/s)
            diff=xk*tair/sfc
c ***
            if(DEBUG)then
               write(io6,*)'i = ',i,' diamcm = ',diamcm,' scf = ',scf,
     1         ' sfc = ',sfc,' diff = ',diff
            endif
c ***
c
c ---       Schmidt number
c ---       (vair = viscosity of air at 20 deg. c = 0.15 cm**2/s)
            asc(i,j)=vair/diff
c
c ---       Gravitational settling velocity (m/s)
c ---       (rhoair is approx. density of air -- 1.2e-3 g/cm**3)
            avgrav(i,j)=0.01*(apdens(i,j)-rhoair)*gcgs*diamcm**2
     1                     *scf/(18.*vcon)
c
c ---       Stopping times
            atstop(i,j)=avgrav(i,j)/(0.01*gcgs)
20       continue
25    continue
c ***
      if(DEBUG)then
         write(io6,*)
         write(io6,*)'SUBR. VDP1 -- Outputs'
         write(io6,*)
         do 30 i=1,numsrc
         write(io6,*)'SOURCE          = ',i
         write(io6,*)'ASC             = ',(asc(n,i),n=1,inpd(i))
         write(io6,*)'AVGRAV (m/s)    = ',(avgrav(n,i),n=1,inpd(i))
         write(io6,*)'ATSTOP (s)      = ',(atstop(n,i),n=1,inpd(i))
         write(io6,*)'VAIRMS (m**2/s) = ',vairms
C         write(io6,*)'ZRDEP (m)       = ',zrdep
         write(io6,*)'VDPHOR (m/s)    = ',vdphor
         write(io6,*)
30       continue
      endif
c ***
c
      return
      end

      SUBROUTINE HREMIS
C***********************************************************************
C                 HREMIS Module of AERMOD
C
C        PURPOSE: To process Hourly Emissions Data 
C
C        PROGRAMMER: Jayant Hardikar, Roger Brode
C  
C        DATE:    September 15, 1993
C
C        INPUTS:  Pathway (SO) and Keyword (HOURLY)
C
C        OUTPUTS: Source QFLAG Array
C
C        CALLED FROM:   SOCARD
C***********************************************************************
   
C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'

      LOGICAL FOPEN, INGRP
      LOGICAL RMARK

      CHARACTER*8 LOWID, HIGID, LID1, LID2, HID1, HID2, TEMPID

C     Variable Initializations
      MODNAM = 'HREMIS'

      FOPEN  = .FALSE.

      IF (IFC .GE. 4) THEN
C        Retrieve Hourly Emissions Data Filename as Character Substring to
C        Maintain Case
         HRFILE = RUNST1(LOCB(3):LOCE(3))

C        Open Hourly Emissions Data File If Not Already Open
         INQUIRE (FILE=HRFILE,OPENED=FOPEN)
         
         IF (.NOT. FOPEN) THEN
C           Open Hourly Emissions Data File If Not Already Open
            INQUIRE (UNIT=IHREMI,OPENED=FOPEN)
            IF (.NOT. FOPEN) THEN
               OPEN (UNIT=IHREMI,ERR=998,FILE=HRFILE,IOSTAT=IOERRN,
     &               STATUS='OLD')
            ELSE
C              Hourly Emissions File is Already Opened With Different Filename
               CALL ERRHDL(PATH,MODNAM,'E','500',KEYWRD)
               GO TO 999
            ENDIF
         ENDIF

      ELSE
C        WRITE Error Message         ! Not Enough Parameters Specified
         CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
         GO TO 999
      END IF

      TEMPID = FIELD(4)

C     Set Up The Source Group Array
      IF (TEMPID .EQ. 'ALL') THEN
         DO 30 I = 1, NUMSRC
            QFLAG(I) = 'HOURLY'
 30      CONTINUE
      ELSE
C        Loop Through Fields
         DO 50 I = 4, IFC
            CALL FSPLIT(PATH,KEYWRD,FIELD(I),40,'-',RMARK,LOWID,HIGID)
C           First Check Range for Upper Value < Lower Value
            CALL SETIDG(LOWID,LID1,IL,LID2)
            CALL SETIDG(HIGID,HID1,IH,HID2)
            IF ((HID1.LT.LID1) .OR. (IH.LT.IL) .OR. (HID2.LT.LID2)) THEN
C              WRITE Error Message:  Invalid Range,  Upper < Lower
               CALL ERRHDL(PATH,MODNAM,'E','203','SRCRANGE')
               GO TO 50
            END IF
            DO 40 K = 1, NUMSRC
               CALL ASNGRP(SRCID(K),LOWID,HIGID,INGRP)
               IF (INGRP) THEN
                  QFLAG(K) = 'HOURLY'                  
               END IF
 40         CONTINUE
 50      CONTINUE
      END IF

      GO TO 999

C     Process Error Messages
998   CALL ERRHDL(PATH,MODNAM,'E','500',KEYWRD)

999   RETURN
      END 
