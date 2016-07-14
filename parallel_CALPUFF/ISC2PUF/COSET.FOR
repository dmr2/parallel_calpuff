      SUBROUTINE COCARD
C***********************************************************************
C                 COCARD Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: To process COntrol Pathway card images
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        MODIFIED:   To add error check for use of NOSMPL option with
C                    FLAT terrain.
C                    R. W. Brode, PES - 9/30/94
C
C        MODIFIED:   To add DDEP and WDEP parameters to CONC/DEPOS options
C                    to allow just the wet or just the dry deposition flux
C                    to be reported.  DEPOS now reports the sum of wet and
C                    dry fluxes.  Also, a new option parameter is provided
C                    to force the Intermeiate Terrain procedure to ignore
C                    either the simple terrain model or the complex terrain
C                    model (NOSMPL/NOCMPL).
C                    D. Strimaitis, SRC - 11/8/93
C
C        MODIFIED:   To add DEPLETE parameter for plume depletion option
C                    and to allow flagpole receptors with DEPOS option.
C                    D. Strimaitis, SRC - 2/15/93
C
C        INPUTS:  Pathway (CO) and Keyword
C
C        OUTPUTS: Processing Option Switches
C                 Option Setup Status Switches
C
C        CALLED FROM:   SETUP
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'

C     Variable Initializations
      MODNAM = 'COCARD'

      IF (KEYWRD .EQ. 'STARTING') THEN
C        Set Status Switch
         ISTART = .TRUE.
         ICSTAT(1) = ICSTAT(1) + 1
         IF (ICSTAT(1) .NE. 1) THEN
C           WRITE Error Message: Repeat Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'W','135',KEYWRD)
         END IF
      ELSE IF (KEYWRD .EQ. 'TITLEONE') THEN
C        Set Status Switch
         ICSTAT(2) = ICSTAT(2) + 1
         IF (ICSTAT(2) .NE. 1) THEN
C           WRITE Error Message: Repeat Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'W','135',KEYWRD)
         ELSE
C           Process Titles                                  ---   CALL TITLES
            CALL TITLES
         END IF
      ELSE IF (KEYWRD .EQ. 'TITLETWO') THEN
C        Set Status Switch
         ICSTAT(3) = ICSTAT(3) + 1
         IF (ICSTAT(3) .NE. 1) THEN
C           WRITE Error Message: Repeat Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'W','135',KEYWRD)
         ELSE
C           Process Titles                                  ---   CALL TITLES
            CALL TITLES
         END IF
      ELSE IF (KEYWRD .EQ. 'MODELOPT') THEN
C        Set Status Switch
         ICSTAT(4) = ICSTAT(4) + 1
         IF (ICSTAT(4) .NE. 1) THEN
C           WRITE Error Message: Repeat Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE
C           Process Modeling Options                        ---   CALL MODOPT
            CALL MODOPT
         END IF
      ELSE IF (KEYWRD .EQ. 'AVERTIME') THEN
cvrt
             Write (*,1001) 
1001  format (' WARNING:  "AVERTIME" Keyword not compatible with ',
     &      'CALPUFF')

C        Set Status Switch
         ICSTAT(5) = ICSTAT(5) + 1
         IF (ICSTAT(5) .NE. 1) THEN
C           WRITE Error Message: Repeat Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE
C           Process Averaging Time Options                  ---   CALL AVETIM
            CALL AVETIM
         END IF
      ELSE IF (KEYWRD .EQ. 'POLLUTID') THEN
C        Set Status Switch
         ICSTAT(6) = ICSTAT(6) + 1
         IF (ICSTAT(6) .NE. 1) THEN
C           WRITE Error Message: Repeat Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE IF (ICSTAT(4) .NE. 1) THEN
C           WRITE Error Message: Keyword Out of Order (Must Follow MODELOPT)
            CALL ERRHDL(PATH,MODNAM,'E','140',KEYWRD)
         ELSE
C           Process Pollutant ID Option                     ---   CALL POLLID
            CALL POLLID
         END IF
      ELSE IF (KEYWRD .EQ. 'HALFLIFE' .OR.
     &         KEYWRD .EQ. 'DCAYCOEF') THEN
         IF (KEYWRD .EQ. 'HALFLIFE') THEN
cvrt
             Write (*,1002) 
1002  format (' WARNING:  "HALFLIFE" Keyword not compatible with ',
     &      'CALPUFF')

C           Check for Previous DCAYCOEF Keyword in Runstream File
            IF (ICSTAT(8) .NE. 0) THEN
               CALL ERRHDL(PATH,MODNAM,'W','155',KEYWRD)
               GO TO 999
            ELSE
C              Set Status Switch and Check for Duplicate Keyword
               ICSTAT(7) = ICSTAT(7) + 1
               IF (ICSTAT(7) .NE. 1) THEN
C                 WRITE Error Message: Repeat Non-repeatable Keyword
                  CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
                  GO TO 999
               END IF
            END IF
         ELSE IF (KEYWRD .EQ. 'DCAYCOEF') THEN
cvrt
             Write (*,1003) 
1003  format (' WARNING:  "DCAYCOEF" Keyword not compatible with ',
     &      'CALPUFF')

C           Check for Previous HALFLIFE Keyword in Runstream File
            IF (ICSTAT(7) .NE. 0) THEN
               CALL ERRHDL(PATH,MODNAM,'W','155',KEYWRD)
               GO TO 999
            ELSE
C              Set Status Switch and Check for Duplicate Keyword
               ICSTAT(8) = ICSTAT(8) + 1
               IF (ICSTAT(8) .NE. 1) THEN
C                 WRITE Error Message: Repeat Non-repeatable Keyword
                  CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
                  GO TO 999
               END IF
            END IF
         END IF
C        Check for Keyword Out of Order
         IF (ICSTAT(4) .NE. 1) THEN
C           WRITE Error Message: Keyword Out of Order (Must Follow MODELOPT)
            CALL ERRHDL(PATH,MODNAM,'E','140',KEYWRD)
         ELSE IF (ICSTAT(6) .NE. 1) THEN
C           WRITE Error Message: Keyword Out of Order (Must Follow POLLUTID)
            CALL ERRHDL(PATH,MODNAM,'E','140',KEYWRD)
         END IF
C        Process Exponential Decay Option                   ---   CALL EDECAY
         CALL EDECAY
      ELSE IF (KEYWRD .EQ. 'TERRHGTS') THEN
C        Set Status Switch
         ICSTAT(9) = ICSTAT(9) + 1
         IF (ICSTAT(9) .NE. 1) THEN
C           WRITE Error Message: Repeat Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE
C           Process Terrain Heights Option                  ---   CALL TERRHT
            CALL TERRHT
         END IF
      ELSE IF (KEYWRD .EQ. 'ELEVUNIT') THEN
C        Set Status Switch
         ICSTAT(10) = ICSTAT(10) + 1
         IF (ICSTAT(10) .NE. 1) THEN
C           WRITE Error Message: Repeat Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE
C           Process Elevation Units Option                  ---   CALL ELUNIT
            CALL ELUNIT
C           WRITE Warning Message: CO ELEVUNIT is obsolete, should place
C           ELEVUNIT on SO, RE and/or TG pathway
            CALL ERRHDL(PATH,MODNAM,'W','151','instead ')
         END IF
      ELSE IF (KEYWRD .EQ. 'FLAGPOLE') THEN
C        Set Status Switch
         ICSTAT(11) = ICSTAT(11) + 1
         IF (ICSTAT(11) .NE. 1) THEN
C           WRITE Error Message: Repeat Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE
C           Process Flagpole Receptor Height Option         ---   CALL FLAGDF
            CALL FLAGDF
         END IF
      ELSE IF (KEYWRD .EQ. 'RUNORNOT') THEN
C        Set Status Switch
         ICSTAT(12) = ICSTAT(12) + 1
         IF (ICSTAT(12) .NE. 1) THEN
C           WRITE Error Message: Repeat Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE
C           Process Option to Run Model or Not              ---   CALL RUNNOT
            CALL RUNNOT
         END IF
      ELSE IF (KEYWRD .EQ. 'EVENTFIL') THEN
cvrt
             Write (*,1004) 
1004  format (' WARNING:  "EVENTFIL" Keyword not compatible with ',
     &      'CALPUFF')

C        Set Status Switch
         ICSTAT(13) = ICSTAT(13) + 1
         IF (ICSTAT(13) .NE. 1) THEN
C           WRITE Error Message: Repeat Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE
C           Process EVENT File Option                       ---   CALL EVNTFL
            CALL EVNTFL
         END IF
      ELSE IF (KEYWRD .EQ. 'SAVEFILE') THEN
cvrt
             Write (*,1005) 
1005  format (' WARNING:  "SAVEFILE" Keyword not compatible with ',
     &      'CALPUFF')

C        Set Status Switch
         ICSTAT(14) = ICSTAT(14) + 1
         IF (ICSTAT(14) .NE. 1) THEN
C           WRITE Error Message: Repeat Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE
C           Process Model Re-start Save File Option         ---   CALL SAVEFL
            CALL SAVEFL
         END IF
      ELSE IF (KEYWRD .EQ. 'INITFILE') THEN
cvrt
             Write (*,1006) 
1006  format (' WARNING:  "INITFILE" Keyword not compatible with ',
     &      'CALPUFF')

C        Set Status Switch
         ICSTAT(15) = ICSTAT(15) + 1
         IF (ICSTAT(15) .NE. 1) THEN
C           WRITE Error Message: Repeat Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE
C           Process Re-start Initialization File Option     ---   CALL INITFL
            CALL INITFL
         END IF
      ELSE IF (KEYWRD .EQ. 'MULTYEAR') THEN
cvrt
             Write (*,1007) 
1007  format (' WARNING:  "MULTYEAR" Keyword not compatible with ',
     &      'CALPUFF')

C        Set Status Switch
         ICSTAT(16) = ICSTAT(16) + 1
         IF (ICSTAT(16) .NE. 1) THEN
C           WRITE Error Message: Repeat Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE
C           Process Multiple-Year Run Option                ---   CALL MYEAR
            CALL MYEAR
         END IF
      ELSE IF (KEYWRD .EQ. 'ERRORFIL') THEN
cvrt
             Write (*,1008) 
1008  format (' WARNING:  "ERRORFIL" Keyword not compatible with ',
     &      'CALPUFF')

C        Set Status Switch
         ICSTAT(17) = ICSTAT(17) + 1
         IF (ICSTAT(17) .NE. 1) THEN
C           WRITE Error Message: Repeat Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE
C           Process Error File Option                       ---   CALL ERRFIL
            CALL ERRFIL
         END IF
      ELSE IF (KEYWRD .EQ. 'FINISHED') THEN
C        Set Status Switch
         IFINIS = .TRUE.
C        Set Status Switch
         ICSTAT(20) = ICSTAT(20) + 1
         IF (ICSTAT(20) .NE. 1) THEN
C           WRITE Error Message: Repeat Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
            GO TO 999
         END IF

C        Check for Missing Mandatory Keywords
         IF (ICSTAT(1) .EQ. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','130','STARTING')
         END IF
         IF (ICSTAT(2) .EQ. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','130','TITLEONE')
         END IF
         IF (ICSTAT(4) .EQ. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','130','MODELOPT')
         END IF
         IF (ICSTAT(5) .EQ. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','130','AVERTIME')
         END IF
         IF (ICSTAT(6) .EQ. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','130','POLLUTID')
         END IF
         IF (ICSTAT(12) .EQ. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','130','RUNORNOT')
         END IF

C        Check for Conflicting Options
         IF (NOSMPL .AND. FLAT) THEN
C           WRITE Error Message for Use of NOSMPL with FLAT terrain
            CALL ERRHDL(PATH,MODNAM,'E','144','       ')
         END IF

C *** Elevated terrain option is allowed in ISCOMDEP -- remove check
C        IF (DEPOS .AND. ELEV) THEN
C           WRITE Warning Message for Use of ELEV Terrain with DEPOS,
C           Reset to FLAT Terrain
C           CALL ERRHDL(PATH,MODNAM,'W','213',' DEPOS ')
c           ELEV = .FALSE.
C           FLAT = .TRUE.
C        END IF
C *** Flagpole option is OK with new deposition algorithm --remove check
C       IF (DEPOS .AND. FLGPOL) THEN
C           WRITE Warning Message for Use of FLGPOL Option with DEPOS,
C           Reset FLGPOL Option to FALSE
C           CALL ERRHDL(PATH,MODNAM,'W','215',' DEPOS ')
C           FLGPOL = .FALSE.
C        END IF

C        OPEN Restart Save and Initialization Files
         IF (RSTSAV) THEN
            DUMMY = 'SAVEFILE'
            OPEN(UNIT=IDPUNT,ERR=99,FILE=SAVFIL,FORM='UNFORMATTED',
     &           IOSTAT=IOERRN,STATUS='UNKNOWN')
            IF (SAVFL2 .NE. SAVFIL) THEN
               OPEN(UNIT=IDPUN2,ERR=99,FILE=SAVFL2,FORM='UNFORMATTED',
     &              IOSTAT=IOERRN,STATUS='UNKNOWN')
            END IF
         END IF
         IF (RSTINP) THEN
            DUMMY = 'INITFILE'
            OPEN(UNIT=IRSUNT,ERR=99,FILE=INIFIL,FORM='UNFORMATTED',
     &           IOSTAT=IOERRN,STATUS='OLD')
         END IF

C        Generate MODOPS Character Array to Summarize Modeling Options
         IF (CONC) THEN
            MODOPS(1) = 'CONC '
         END IF
         IF (DEPOS) THEN
            MODOPS(2) = 'DEPOS'
         END IF
         IF (DDEP) THEN
            MODOPS(3) = 'DDEP'
         END IF
         IF (WDEP) THEN
            MODOPS(4) = 'WDEP'
         END IF
         IF (RURAL) THEN
            MODOPS(5) = 'RURAL'
         ELSE
            MODOPS(5) = 'URBAN'
         END IF
         IF (FLAT) THEN
            MODOPS(6) = 'FLAT'
         ELSE
            MODOPS(6) = 'ELEV'
         END IF
         IF (FLGPOL) MODOPS(7) = 'FLGPOL'
         IF (DFAULT) MODOPS(8) = 'DFAULT'
         IF (GRDRIS) MODOPS(9) = 'GRDRIS'
         IF (NOSTD)  MODOPS(10) = 'NOSTD'
         IF (NOBID)  MODOPS(11) = 'NOBID'
         IF (NOCALM) MODOPS(12) = 'NOCALM'
         IF (MSGPRO) MODOPS(13) = 'MSGPRO'
         IF (MULTYR) MODOPS(14) = 'MULTYR'
         IF (DDPLETE) MODOPS(15) = 'DRYDPL'
         IF (WDPLETE) MODOPS(16) = 'WETDPL'
         IF (NOSMPL) MODOPS(17) = 'NOSMPL'
         IF (NOCMPL) MODOPS(17) = 'NOCMPL'

         GO TO 1000

C        WRITE Error Message for Error Opening File
 99      CALL ERRHDL(PATH,MODNAM,'E','500',DUMMY)
         IF (DUMMY .EQ. 'SAVEFILE') THEN
C           Reset Logical Flag for SAVEFILE Option Due to Error Opening File
            RSTSAV = .FALSE.
         ELSE IF (DUMMY .EQ. 'INITFILE') THEN
C           Reset Logical Flag for INITFILE Option Due to Error Opening File
            RSTINP = .FALSE.
         END IF

 1000    CONTINUE

      ELSE
C        Write Error Message: Invalid Keyword for This Pathway
         CALL ERRHDL(PATH,MODNAM,'E','110',KEYWRD)
      END IF

 999  RETURN
      END

      SUBROUTINE TITLES
C***********************************************************************
C                 TITLES Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Process Title Information From Runstream Input Image
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Title Strings for Model Outputs
C
C        CALLED FROM:   COCARD
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'

C     Variable Initializations
      MODNAM = 'TITLES'

      IF (KEYWRD .EQ. 'TITLEONE') THEN
         TITLE1 = RUNST1(LOCE(2)+2:80)
         IF (RUNST1(LOCE(2)+2:80) .EQ. ' ') THEN
C           Write Error Message: Missing Parameter Title
            CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         END IF
      ELSE IF (KEYWRD .EQ. 'TITLETWO') THEN
         TITLE2 = RUNST1(LOCE(2)+2:80)
         IF (RUNST1(LOCE(2)+2:80) .EQ. ' ') THEN
C           Write Warning Message
            CALL ERRHDL(PATH,MODNAM,'W','200',KEYWRD)
         END IF
      END IF

 999  RETURN
      END

      SUBROUTINE MODOPT
C***********************************************************************
C                 MODOPT Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Process Modeling Options From Runstream Input Image
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        MODIFIED:   To allow for calculating CONC/DEPOS/DDEP/WDEP in
C                    a single model run.
C                    R. W. Brode, PES - 4/17/95
C
C        MODIFIED:   To add DDEP and WDEP parameters to CONC/DEPOS options
C                    to allow just the wet or just the dry deposition flux
C                    to be reported.  DEPOS now reports the sum of wet and
C                    dry fluxes.  Also, a new option parameter is provided
C                    to force the Intermeiate Terrain procedure to ignore
C                    either the simple terrain model or the complex terrain
C                    model (NOSMPL/NOCMPL).
C                    D. Strimaitis, SRC - 11/8/93
C
C        MODIFIED:   To add DEPLETE parameter for plume depletion option
C                    D. Strimaitis, SRC - 2/15/93
C
C        MODIFIED:   To Output Warning Message '206' For Overriding
C                    Non-DEFAULT Option - 9/29/92
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Modeling Option Logical Switch Settings
C
C        ERROR HANDLING:   Checks for Too Few or Too Many Option Keywords;
C                          Checks for Invalid Option Keywords;
C                          Checks for Conflicting or Missing Option Keywords
C
C        CALLED FROM:   COCARD
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      CHARACTER KOPT*8

C     Variable Initializations - Initialize All Logical Switches to FALSE
      MODNAM = 'MODOPT'

C     Check for Too Few or Too Many Parameters
      IF (IFC .LT. 3) THEN
C        WRITE Error Message     ! No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
      ELSE IF (IFC .LT. 4) THEN
C        WRITE Warning Message   ! Not Enough Parameters
         CALL ERRHDL(PATH,MODNAM,'W','201',KEYWRD)
      ELSE IF (IFC .GT. 11) THEN
C        WRITE Warning Message   ! Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'W','202',KEYWRD)
      END IF

C     First Check for Presence of DFAULT Switch
      DO 10 I = 3, IFC
         KOPT = FIELD(I)
         IF (KOPT .EQ. 'DFAULT' .OR. KOPT .EQ. 'DEFAULT') THEN
            DFAULT = .TRUE.
         END IF
 10   CONTINUE

      NUMTYP = 0
C     Loop Through Fields Again Setting All Swithes
      DO 20 I = 3, IFC
         KOPT = FIELD(I)
         IF (KOPT .EQ. 'DFAULT') THEN
            DFAULT = .TRUE.
         ELSE IF (KOPT .EQ. 'CONC') THEN
            IF (.NOT. CONC) THEN
               CONC   = .TRUE.
               NUMTYP = NUMTYP + 1
            END IF
c           EMIFAC = 1.0E06
c           EMILBL = 'GRAMS/SEC'
c           OUTLBL = 'MICROGRAMS/M**3'
c           CHIDEP(1) = 'AVER'
c           CHIDEP(2) = 'AGE '
c           CHIDEP(3) = 'CONC'
c           CHIDEP(4) = 'ENTR'
c           CHIDEP(5) = 'ATIO'
c           CHIDEP(6) = 'N   '
         ELSE IF (KOPT .EQ. 'DEPOS') THEN
            IF (.NOT. DEPOS) THEN
               DEPOS  = .TRUE.
               NUMTYP = NUMTYP + 1
            END IF
c           EMIFAC = 3600.
c           EMILBL = 'GRAMS/SEC'
c           OUTLBL = 'GRAMS/M**2'
c           CHIDEP(1) = '  TO'
c           CHIDEP(2) = 'TAL '
c           CHIDEP(3) = 'DEPO'
c           CHIDEP(4) = 'SITI'
c           CHIDEP(5) = 'ON  '
c           CHIDEP(6) = '    '
         ELSE IF (KOPT .EQ. 'DDEP') THEN
            IF (.NOT. DDEP) THEN
               DDEP   = .TRUE.
               NUMTYP = NUMTYP + 1
            END IF
c           EMIFAC = 3600.
c           EMILBL = 'GRAMS/SEC'
c           OUTLBL = 'GRAMS/M**2'
c           CHIDEP(1) = '    '
c           CHIDEP(2) = 'DRY '
c           CHIDEP(3) = 'DEPO'
c           CHIDEP(4) = 'SITI'
c           CHIDEP(5) = 'ON  '
c           CHIDEP(6) = '    '
         ELSE IF (KOPT .EQ. 'WDEP') THEN
            IF (.NOT. WDEP) THEN
               WDEP   = .TRUE.
               NUMTYP = NUMTYP + 1
            END IF
c           EMIFAC = 3600.
c           EMILBL = 'GRAMS/SEC'
c           OUTLBL = 'GRAMS/M**2'
c           CHIDEP(1) = '    '
c           CHIDEP(2) = 'WET '
c           CHIDEP(3) = 'DEPO'
c           CHIDEP(4) = 'SITI'
c           CHIDEP(5) = 'ON  '
c           CHIDEP(6) = '    '
         ELSE IF (KOPT .EQ. 'RURAL') THEN
            RURAL = .TRUE.
         ELSE IF (KOPT .EQ. 'URBAN') THEN
            URBAN = .TRUE.
         ELSE IF (KOPT .EQ. 'DRYDPLT') THEN
            DDPLETE = .TRUE.
         ELSE IF (KOPT .EQ. 'WETDPLT') THEN
            WDPLETE = .TRUE.
         ELSE IF (KOPT .EQ. 'NOSMPL') THEN
            NOSMPL = .TRUE.
         ELSE IF (KOPT .EQ. 'NOCMPL') THEN
            NOCMPL = .TRUE.
         ELSE IF (KOPT .EQ. 'GRDRIS') THEN
            IF (.NOT. DFAULT) THEN
               GRDRIS = .TRUE.
            ELSE
C              WRITE Warning Message     ! Non-DEFAULT Option Overridden
               CALL ERRHDL(PATH,MODNAM,'W','206',KOPT)
            END IF
         ELSE IF (KOPT .EQ. 'NOSTD') THEN
            IF (.NOT. DFAULT) THEN
               NOSTD = .TRUE.
            ELSE
C              WRITE Warning Message     ! Non-DEFAULT Option Overridden
               CALL ERRHDL(PATH,MODNAM,'W','206',KOPT)
            END IF
         ELSE IF (KOPT .EQ. 'NOBID') THEN
            IF (.NOT. DFAULT) THEN
               NOBID = .TRUE.
            ELSE
C              WRITE Warning Message     ! Non-DEFAULT Option Overridden
               CALL ERRHDL(PATH,MODNAM,'W','206',KOPT)
            END IF
         ELSE IF (KOPT .EQ. 'NOCALM') THEN
            IF (.NOT. DFAULT) THEN
               NOCALM = .TRUE.
               CLMPRO = .FALSE.
            ELSE
C              WRITE Warning Message     ! Non-DEFAULT Option Overridden
               CALL ERRHDL(PATH,MODNAM,'W','206',KOPT)
            END IF
         ELSE IF (KOPT .EQ. 'MSGPRO') THEN
            IF (.NOT. DFAULT) THEN
               MSGPRO = .TRUE.
            ELSE
C              WRITE Warning Message     ! Non-DEFAULT Option Overridden
               CALL ERRHDL(PATH,MODNAM,'W','206',KOPT)
            END IF
         ELSE
C           WRITE Error Message     ! Invalid Parameter
            CALL ERRHDL(PATH,MODNAM,'E','203',KOPT)
         END IF
 20   CONTINUE

C     Setup Label Array for Concentration and Depositions
      IF (NUMTYP .GT. NTYP) THEN
C        WRITE Error Message: Number of output types exceeds maximum
         WRITE(DUMMY,'(I4)') NTYP
         CALL ERRHDL(PATH,MODNAM,'E','290',DUMMY)
      ELSE IF (NUMTYP .EQ. 0) THEN
C        WRITE Warning Message: No Output Types Selected, Assume CONC Only
         CALL ERRHDL(PATH,MODNAM,'W','205','CONC')
         NUMTYP = 1
         ITYP   = 1
         CONC   = .TRUE.
         CHIDEP(1,ITYP) = 'AVER'
         CHIDEP(2,ITYP) = 'AGE '
         CHIDEP(3,ITYP) = 'CONC'
         CHIDEP(4,ITYP) = 'ENTR'
         CHIDEP(5,ITYP) = 'ATIO'
         CHIDEP(6,ITYP) = 'N   '
         EMIFAC(ITYP) = 1.0E06
         EMILBL(ITYP) = 'GRAMS/SEC'
         OUTLBL(ITYP) = 'MICROGRAMS/M**3'
      ELSE IF (CONC) THEN
         ITYP = 1
         CHIDEP(1,ITYP) = 'AVER'
         CHIDEP(2,ITYP) = 'AGE '
         CHIDEP(3,ITYP) = 'CONC'
         CHIDEP(4,ITYP) = 'ENTR'
         CHIDEP(5,ITYP) = 'ATIO'
         CHIDEP(6,ITYP) = 'N   '
         EMIFAC(ITYP) = 1.0E06
         EMILBL(ITYP) = 'GRAMS/SEC'
         OUTLBL(ITYP) = 'MICROGRAMS/M**3'
         IF (DEPOS) THEN
            ITYP = 2
            CHIDEP(1,ITYP) = '  TO'
            CHIDEP(2,ITYP) = 'TAL '
            CHIDEP(3,ITYP) = 'DEPO'
            CHIDEP(4,ITYP) = 'SITI'
            CHIDEP(5,ITYP) = 'ON  '
            CHIDEP(6,ITYP) = '    '
            EMIFAC(ITYP) = 3600.
            EMILBL(ITYP) = 'GRAMS/SEC'
            OUTLBL(ITYP) = 'GRAMS/M**2'
            IF (DDEP) THEN
               ITYP = 3
               CHIDEP(1,ITYP) = '    '
               CHIDEP(2,ITYP) = 'DRY '
               CHIDEP(3,ITYP) = 'DEPO'
               CHIDEP(4,ITYP) = 'SITI'
               CHIDEP(5,ITYP) = 'ON  '
               CHIDEP(6,ITYP) = '    '
               EMIFAC(ITYP) = 3600.
               EMILBL(ITYP) = 'GRAMS/SEC'
               OUTLBL(ITYP) = 'GRAMS/M**2'
               IF (WDEP) THEN
                  ITYP = 4
                  CHIDEP(1,ITYP) = '    '
                  CHIDEP(2,ITYP) = 'WET '
                  CHIDEP(3,ITYP) = 'DEPO'
                  CHIDEP(4,ITYP) = 'SITI'
                  CHIDEP(5,ITYP) = 'ON  '
                  CHIDEP(6,ITYP) = '    '
                  EMIFAC(ITYP) = 3600.
                  EMILBL(ITYP) = 'GRAMS/SEC'
                  OUTLBL(ITYP) = 'GRAMS/M**2'
               END IF
            ELSE IF (WDEP) THEN
               ITYP = 3
               CHIDEP(1,ITYP) = '    '
               CHIDEP(2,ITYP) = 'WET '
               CHIDEP(3,ITYP) = 'DEPO'
               CHIDEP(4,ITYP) = 'SITI'
               CHIDEP(5,ITYP) = 'ON  '
               CHIDEP(6,ITYP) = '    '
               EMIFAC(ITYP) = 3600.
               EMILBL(ITYP) = 'GRAMS/SEC'
               OUTLBL(ITYP) = 'GRAMS/M**2'
            END IF
         ELSE IF (DDEP) THEN
            ITYP = 2
            CHIDEP(1,ITYP) = '    '
            CHIDEP(2,ITYP) = 'DRY '
            CHIDEP(3,ITYP) = 'DEPO'
            CHIDEP(4,ITYP) = 'SITI'
            CHIDEP(5,ITYP) = 'ON  '
            CHIDEP(6,ITYP) = '    '
            EMIFAC(ITYP) = 3600.
            EMILBL(ITYP) = 'GRAMS/SEC'
            OUTLBL(ITYP) = 'GRAMS/M**2'
            IF (WDEP) THEN
               ITYP = 3
               CHIDEP(1,ITYP) = '    '
               CHIDEP(2,ITYP) = 'WET '
               CHIDEP(3,ITYP) = 'DEPO'
               CHIDEP(4,ITYP) = 'SITI'
               CHIDEP(5,ITYP) = 'ON  '
               CHIDEP(6,ITYP) = '    '
               EMIFAC(ITYP) = 3600.
               EMILBL(ITYP) = 'GRAMS/SEC'
               OUTLBL(ITYP) = 'GRAMS/M**2'
            END IF
         ELSE IF (WDEP) THEN
            ITYP = 2
            CHIDEP(1,ITYP) = '    '
            CHIDEP(2,ITYP) = 'WET '
            CHIDEP(3,ITYP) = 'DEPO'
            CHIDEP(4,ITYP) = 'SITI'
            CHIDEP(5,ITYP) = 'ON  '
            CHIDEP(6,ITYP) = '    '
            EMIFAC(ITYP) = 3600.
            EMILBL(ITYP) = 'GRAMS/SEC'
            OUTLBL(ITYP) = 'GRAMS/M**2'
         END IF
      ELSE IF (DEPOS) THEN
         ITYP = 1
         CHIDEP(1,ITYP) = '  TO'
         CHIDEP(2,ITYP) = 'TAL '
         CHIDEP(3,ITYP) = 'DEPO'
         CHIDEP(4,ITYP) = 'SITI'
         CHIDEP(5,ITYP) = 'ON  '
         CHIDEP(6,ITYP) = '    '
         EMIFAC(ITYP) = 3600.
         EMILBL(ITYP) = 'GRAMS/SEC'
         OUTLBL(ITYP) = 'GRAMS/M**2'
         IF (DDEP) THEN
            ITYP = 2
            CHIDEP(1,ITYP) = '    '
            CHIDEP(2,ITYP) = 'DRY '
            CHIDEP(3,ITYP) = 'DEPO'
            CHIDEP(4,ITYP) = 'SITI'
            CHIDEP(5,ITYP) = 'ON  '
            CHIDEP(6,ITYP) = '    '
            EMIFAC(ITYP) = 3600.
            EMILBL(ITYP) = 'GRAMS/SEC'
            OUTLBL(ITYP) = 'GRAMS/M**2'
            IF (WDEP) THEN
               ITYP = 3
               CHIDEP(1,ITYP) = '    '
               CHIDEP(2,ITYP) = 'WET '
               CHIDEP(3,ITYP) = 'DEPO'
               CHIDEP(4,ITYP) = 'SITI'
               CHIDEP(5,ITYP) = 'ON  '
               CHIDEP(6,ITYP) = '    '
               EMIFAC(ITYP) = 3600.
               EMILBL(ITYP) = 'GRAMS/SEC'
               OUTLBL(ITYP) = 'GRAMS/M**2'
            END IF
         ELSE IF (WDEP) THEN
            ITYP = 2
            CHIDEP(1,ITYP) = '    '
            CHIDEP(2,ITYP) = 'WET '
            CHIDEP(3,ITYP) = 'DEPO'
            CHIDEP(4,ITYP) = 'SITI'
            CHIDEP(5,ITYP) = 'ON  '
            CHIDEP(6,ITYP) = '    '
            EMIFAC(ITYP) = 3600.
            EMILBL(ITYP) = 'GRAMS/SEC'
            OUTLBL(ITYP) = 'GRAMS/M**2'
         END IF
      ELSE IF (DDEP) THEN
         ITYP = 1
         CHIDEP(1,ITYP) = '    '
         CHIDEP(2,ITYP) = 'DRY '
         CHIDEP(3,ITYP) = 'DEPO'
         CHIDEP(4,ITYP) = 'SITI'
         CHIDEP(5,ITYP) = 'ON  '
         CHIDEP(6,ITYP) = '    '
         EMIFAC(ITYP) = 3600.
         EMILBL(ITYP) = 'GRAMS/SEC'
         OUTLBL(ITYP) = 'GRAMS/M**2'
         IF (WDEP) THEN
            ITYP = 2
            CHIDEP(1,ITYP) = '    '
            CHIDEP(2,ITYP) = 'WET '
            CHIDEP(3,ITYP) = 'DEPO'
            CHIDEP(4,ITYP) = 'SITI'
            CHIDEP(5,ITYP) = 'ON  '
            CHIDEP(6,ITYP) = '    '
            EMIFAC(ITYP) = 3600.
            EMILBL(ITYP) = 'GRAMS/SEC'
            OUTLBL(ITYP) = 'GRAMS/M**2'
         END IF
      ELSE IF (WDEP) THEN
         ITYP = 1
         CHIDEP(1,ITYP) = '    '
         CHIDEP(2,ITYP) = 'WET '
         CHIDEP(3,ITYP) = 'DEPO'
         CHIDEP(4,ITYP) = 'SITI'
         CHIDEP(5,ITYP) = 'ON  '
         CHIDEP(6,ITYP) = '    '
         EMIFAC(ITYP) = 3600.
         EMILBL(ITYP) = 'GRAMS/SEC'
         OUTLBL(ITYP) = 'GRAMS/M**2'
      END IF

      EMICON = 1.0E06

C     Modify PLTFRM and PSTFRM if needed for more than one output type
      IF (NUMTYP .GT. 1) THEN
         WRITE(PLTFRM,1009) NUMTYP+2
 1009    FORMAT('(',I1,'(1X,F13.5),1X,F8.2,3X,A5,2X,A8,2X,A4,6X,A8)')
         WRITE(PSTFRM,1019) NUMTYP+2
 1019    FORMAT('(',I1,'(1X,F13.5),1X,F8.2,2X,A6,2X,A8,2X,I8,2X,A8)')
      END IF

C     Check for Conflicting Options
      IF (RURAL .AND. URBAN) THEN
C        WRITE Warning Message:  Using RURAL Setting
         CALL ERRHDL(PATH,MODNAM,'W','204','RURAL')
         URBAN = .FALSE.
      ELSE IF (.NOT.RURAL .AND. .NOT.URBAN) THEN
C        WRITE Warning Message   ! Use RURAL as Default
         CALL ERRHDL(PATH,MODNAM,'W','205','RURAL')
         RURAL = .TRUE.
      END IF
      IF (NOSMPL .AND. NOCMPL) THEN
C        WRITE ERROR Message:  Using NOCMPL Setting
         CALL ERRHDL(PATH,MODNAM,'E','204','NOCMPL')
         NOSMPL = .FALSE.
      END IF

 999  RETURN
      END

      SUBROUTINE AVETIM
C***********************************************************************
C                 AVETIM Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Process Averaging Time Options From Runstream Input Image
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Averaging Period Array and PERIOD Logical Switch
C
C        ERROR HANDLING:   Checks for Too Many Short Term Averages (>4);
C                          Checks for Invalid Averaging Periods, MOD(24,X) NE 0;
C                          Checks for Duplicate Short Term Averaging Periods
C
C        CALLED FROM:   COCARD
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      CHARACTER*8 KOPT

C     Variable Initializations
      MODNAM = 'AVETIM'

C     Check for No Parameters
      IF (IFC .LT. 3) THEN
C        WRITE Error Message     ! No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
      END IF

C     First Check for Presence of PERIOD or ANNUAL Switch
      DO 10 I = 3, IFC
         KOPT = FIELD(I)
         IF (KOPT .EQ. 'PERIOD') THEN
            PERIOD = .TRUE.
         ELSE IF (KOPT .EQ. 'ANNUAL') THEN
            ANNUAL = .TRUE.
         END IF
 10   CONTINUE

C     Check for Both PERIOD and ANNUAL
      IF (PERIOD .AND. ANNUAL) THEN
         CALL ERRHDL(PATH,MODNAM,'E','295',KEYWRD)
      END IF

C     Check for Too Many Averaging Periods
      IF (PERIOD .OR. ANNUAL) THEN
         IF (IFC .GT. NAVE+3) THEN
C           WRITE Error Message: Too Many Period Or Time Fields
            CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
          END IF
      ELSE
         IF (IFC .GT. NAVE+2) THEN
C           WRITE Error Message: Too Many Period Or Time Fields
            CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
         END IF
      END IF

C     Loop Through Fields Again, Filling KAVE Array for Short Term Averages
      J = 0
      DO 20 I = 3, IFC
         KOPT = FIELD(I)
         IF (KOPT .NE. 'PERIOD' .AND. KOPT .NE. 'ANNUAL') THEN
            IF (KOPT .NE. 'MONTH') THEN
               CALL STONUM(KOPT,8,AVENUM,IDUM)
               IF (IDUM .EQ. -1) THEN
C                 Write Error Message:Invalid Numerical Field
                  CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
               END IF
            END IF
C           Check for Valid Averaging Period
            IF (KOPT.EQ.'MONTH' .OR. (MOD(24,INT(AVENUM)).EQ.0 .AND.
     &                                IDUM.EQ.1)) THEN
               J = J + 1
               IF (J .LE. NAVE) THEN
                  IF (KOPT .EQ. 'MONTH') THEN
                     KAVE(J) = 720
                     MONTH = .TRUE.
                     CHRAVE(J) = 'MONTH'
                  ELSE
                     KAVE(J) = AVENUM
                     WRITE(CHRAVE(J),'(I2,3H-HR)') KAVE(J)
                  END IF
                  NUMAVE = J
C                 Check for Duplicate Averaging Periods
                  DO 15 K = J-1, 1, -1
                     IF (KAVE(J) .EQ. KAVE(K)) THEN
C                       WRITE Error Message    ! Duplicate Averaging Period
                        CALL ERRHDL(PATH,MODNAM,'E','211',KEYWRD)
                     END IF
 15               CONTINUE
               ELSE
C                 WRITE Error Message   ! Too Many Short Term Averaging Periods
                  WRITE(DUMMY,'(I8)') NAVE
                  CALL ERRHDL(PATH,MODNAM,'E','210',DUMMY)
               END IF
            ELSE
C              WRITE Error Message      ! Invalid Averaging Period
               CALL ERRHDL(PATH,MODNAM,'E','203','AVEPER')
            END IF
         END IF
 20   CONTINUE

 999  RETURN
      END

      SUBROUTINE POLLID
C***********************************************************************
C                 POLLID Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Processes Pollutant Identification Option
C
C        PROGRAMMER: Jeff Wang
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Pollutant Identification Option
C
C        CALLED FROM:   COCARD
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'

C     Variable Initializations
      MODNAM = 'POLLID'

C     Check The Number Of The Fields
      IF (IFC .LE. 2) THEN
C        Error Message: No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      ELSE IF (IFC .GT. 3) THEN
C        Error Message: Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
         GO TO 999
      END IF

      POLLUT = FIELD(3)

C     Check for Urban Regulatory Default for SO2
      IF (DFAULT .AND. URBAN .AND. POLLUT.EQ.'SO2') THEN
C        Set Default Decay Coefficient Corresponding to 4-Hour Half Life
         DECOEF = 4.81E-5
      END IF

 999  RETURN
      END

      SUBROUTINE EDECAY
C***********************************************************************
C                 EDECAY Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Processes Exponential Decay Options
C
C        PROGRAMMER: Jeff Wang
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Exponental Decay Options
C
C        CALLED FROM:   COCARD
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'

C     Variable Initializations
      MODNAM = 'EDECAY'

C     Check The Number Of The Fields
      IF (IFC .LE. 2) THEN
C        Error Message: No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      ELSE IF (IFC .GT. 3) THEN
C        Error Message: Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
         GO TO 999
      END IF

C     Start To Get Decay Coef.
      CALL STONUM(FIELD(3),40,FNUM,IMIT)
C     Check The Numerical Field
      IF (IMIT .NE. 1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         GO TO 999
      END IF

      IF (KEYWRD .EQ. 'HALFLIFE') THEN
         HAFLIF = FNUM
C        Calculate Decay Coef. by Halflife
         DECOEF = 0.693/HAFLIF
      ELSE IF (KEYWRD .EQ. 'DCAYCOEF') THEN
         DECOEF = FNUM
      END IF

C     Check for Urban Regulatory Default for SO2
      IF (DFAULT .AND. URBAN .AND. POLLUT.EQ.'SO2') THEN
         IF (DECOEF .NE. 4.81E-5) THEN
C           WRITE Warning Message: Attempt to Override Regulatory Default
            CALL ERRHDL(PATH,MODNAM,'W','206',KEYWRD)
         END IF
         DECOEF = 4.81E-5
      ELSE IF (DFAULT) THEN
         IF (DECOEF .NE. 0.0) THEN
C           WRITE Warning Message: Attempt to Override Regulatory Default
            CALL ERRHDL(PATH,MODNAM,'W','206',KEYWRD)
         END IF
         DECOEF = 0.0
      END IF

 999  RETURN
      END

      SUBROUTINE ELUNIT
C***********************************************************************
C                 ELUNIT Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Process Terrain Elevation Units Option
C                 From Runstream Input Image
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Terrain Elevation Units Switch
C
C        ERROR HANDLING:   Checks for Invalid Parameters;
C                          Checks for No Parameters;
C                          Checks for Too Many Parameters
C
C        CALLED FROM:   COCARD
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'

C     Variable Initializations
      MODNAM = 'ELUNIT'

      IF (IFC .EQ. 3) THEN
         IF (FIELD(3) .EQ. 'METERS') THEN
            ELTYPE = 'METERS'
         ELSE IF (FIELD(3) .EQ. 'FEET') THEN
            ELTYPE = 'FEET'
         ELSE
C           WRITE Error Message  ! Invalid Parameter
            CALL ERRHDL(PATH,MODNAM,'E','203','ELTYPE')
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

      SUBROUTINE TERRHT
C***********************************************************************
C                 TERRHT Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Process Terrain Height Option From Runstream Input Image
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Terrain Height Option Logical Switch
C
C        ERROR HANDLING:   Checks for Invalid Parameters;
C                          Checks for No Parameters;
C                          Checks for Too Many Parameters
C
C        CALLED FROM:   COCARD
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'

C     Variable Initializations
      MODNAM = 'TERRHT'

      IF (IFC .EQ. 3) THEN
         IF (FIELD(3) .EQ. 'FLAT') THEN
            FLAT = .TRUE.
            ELEV = .FALSE.
         ELSE IF (FIELD(3) .EQ. 'ELEV') THEN
            ELEV = .TRUE.
            FLAT = .FALSE.
         ELSE
C           WRITE Error Message  ! Invalid Parameter
            CALL ERRHDL(PATH,MODNAM,'E','203',KEYWRD)
         END IF
      ELSE IF (IFC .GT. 3) THEN
C        WRITE Error Message     ! Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
      ELSE
C        WRITE Error Message     ! No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
      END IF

 999  RETURN
      END

      SUBROUTINE RUNNOT
C***********************************************************************
C                 RUNNOT Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Process Option To RUN Or NOT From Runstream Input Image
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Model RUN Logical Switch
C
C        ERROR HANDLING:   Checks for Invalid Parameters;
C                          Checks for No Parameters;
C                          Checks for Too Many Parameters
C
C        CALLED FROM:   COCARD
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'

C     Variable Initializations
      MODNAM = 'RUNNOT'

      IF (IFC .EQ. 3) THEN
         IF (FIELD(3) .EQ. 'RUN') THEN
            RUN = .TRUE.
         ELSE IF (FIELD(3) .EQ. 'NOT') THEN
            RUN = .FALSE.
         ELSE
C           WRITE Error Message  ! Invalid Parameter
            CALL ERRHDL(PATH,MODNAM,'E','203',KEYWRD)
         END IF
      ELSE IF (IFC .GT. 3) THEN
C        WRITE Error Message     ! Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
      ELSE
C        WRITE Error Message     ! No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
      END IF

 999  RETURN
      END

      SUBROUTINE FLAGDF
C***********************************************************************
C                 FLAGDF Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Process Default Flagpole Receptor Height Option
C                 From Runstream Input Image
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Default Flagpole Receptor Heights
C
C        ERROR HANDLING:   Checks for Invalid Parameters;
C                          Checks for No Parameters;
C                          Checks for Too Many Parameters
C
C        CALLED FROM:   COCARD
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'

C     Variable Initializations
      MODNAM = 'FLAGDF'
      FLGPOL = .TRUE.

      IF (IFC .EQ. 3) THEN
         CALL STONUM(FIELD(3),40,ZFLG,IDUM)
         IF (IDUM .EQ. -1) THEN
C           Write Error Message:Invalid Numerical Field
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         END IF
         IF (ZFLG .GE. 0.0 .AND. IDUM .EQ. 1) THEN
            DO 10 I = 1, NREC
               AZFLAG(I) = ZFLG
 10         CONTINUE
         ELSE IF (ZFLG .LT. 0.0) THEN
C            WRITE Error Message: Invalid Data. Positive Value Turns Negative
             CALL ERRHDL(PATH,MODNAM,'E','209','ZFLAG')
         ELSE IF (IDUM .NE. 1) THEN
C            WRITE Error Message: Field Number Not Meet Requirement
             CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         ELSE
C            WRITE Error Message: Invalid Parameter
             CALL ERRHDL(PATH,MODNAM,'E','203',KEYWRD)
         END IF
      ELSE IF (IFC .GT. 3) THEN
C        WRITE Error Message: Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
      ELSE
C        WRITE Error Message: No Parameters
         CALL ERRHDL(PATH,MODNAM,'W','205','ZFLAG=0.')
      END IF

 999  RETURN
      END

      SUBROUTINE EVNTFL
C***********************************************************************
C                 EVNTFL Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Process EVENT File Option
C                 From Runstream Input Image
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: EVENT File Logical Switch and EVENT Filename
C
C        ERROR HANDLING:   Checks for No Parametes;
C                          Checks for Too Many Parameters
C
C        CALLED FROM:   COCARD
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'

C     Variable Initializations
      MODNAM = 'EVNTFL'

      IF (IFC .EQ. 3) THEN
         EVENTS = .TRUE.
         EVFILE = RUNST1(LOCB(3):LOCE(3))
         EVPARM = 'DETAIL'
      ELSE IF (IFC .EQ. 4) THEN
         EVENTS = .TRUE.
         EVFILE = RUNST1(LOCB(3):LOCE(3))
         EVPARM = FIELD(4)
      ELSE IF (IFC .GT. 4) THEN
C        WRITE Error Message           ! Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
      ELSE
C        WRITE Warning Message         ! No Parameters - Use Default Name
         CALL ERRHDL(PATH,MODNAM,'W','207',KEYWRD)
         EVENTS = .TRUE.
         EVFILE = 'EVENTS.INP'
         EVPARM = 'DETAIL'
      END IF

C     Check for Invalid EVPARM
      IF (EVPARM .NE. 'SOCONT' .AND. EVPARM .NE. 'DETAIL') THEN
C        WRITE Warning Message         ! Invalid Parameter - Use Default
         CALL ERRHDL(PATH,MODNAM,'W','203','EVPARM')
      END IF

C     Open The EVENT Input File
      OPEN(UNIT=IEVUNT,FILE=EVFILE,STATUS='UNKNOWN',
     &     FORM='FORMATTED')

 999  RETURN
      END

      SUBROUTINE SAVEFL
C***********************************************************************
C                 SAVEFL Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Process RESTART File Save Option
C                 From Runstream Input Image
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: RSTSAV File Logical Switch and RESTART Filename
C
C        ERROR HANDLING:   Checks for No Parametes (uses default name);
C                          Checks for Too Many Parameters
C
C        CALLED FROM:   COCARD
C***********************************************************************
C
C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'

C     Variable Initializations
      MODNAM = 'SAVEFL'

      IF (MULTYR) THEN
C        WRITE Error Message:  Conflicting Options RE-START and MULTYEAR
         CALL ERRHDL(PATH,MODNAM,'E','145',KEYWRD)
      ELSE IF (IFC .EQ. 3) THEN
         RSTSAV = .TRUE.
         SAVFIL = RUNST1(LOCB(3):LOCE(3))
         SAVFL2 = SAVFIL
         INCRST = 1
      ELSE IF (IFC .EQ. 4) THEN
         RSTSAV = .TRUE.
         SAVFIL = RUNST1(LOCB(3):LOCE(3))
         SAVFL2 = SAVFIL
         CALL STONUM(FIELD(4),40,FNUM,IDUM)
         INCRST = INT(FNUM)
         IF (IDUM .EQ. -1) THEN
C           Write Error Message:Invalid Numerical Field
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         END IF
      ELSE IF (IFC .EQ. 5) THEN
         RSTSAV = .TRUE.
         SAVFIL = RUNST1(LOCB(3):LOCE(3))
         CALL STONUM(FIELD(4),40,FNUM,IDUM)
         INCRST = INT(FNUM)
         IF (IDUM .EQ. -1) THEN
C           Write Error Message:Invalid Numerical Field
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         END IF
         SAVFL2 = RUNST1(LOCB(5):LOCE(5))
      ELSE IF (IFC .GT. 5) THEN
C        WRITE Error Message           ! Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
      ELSE
C        WRITE Warning Message          ! No Parameters - Use Default Name
         CALL ERRHDL(PATH,MODNAM,'W','207',KEYWRD)
         RSTSAV = .TRUE.
         SAVFIL = 'SAVE.FIL'
         SAVFL2 = SAVFIL
         INCRST = 1
      END IF

 999  RETURN
      END

      SUBROUTINE INITFL
C***********************************************************************
C                 INITFL Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Process RESTART Initialization Input File Option
C                 From Runstream Input Image
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        MODIFIED:   To change default filename to SAVE.FIL to match
C                    default name for SAVEFILE card.
C                    R.W. Brode, PES, Inc. - 6/20/95
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: RSTINP Logical Switch and Re-start Input Filename
C
C        ERROR HANDLING:   Checks for No Parametes (uses default name);
C                          Checks for Too Many Parameters
C
C        CALLED FROM:   COCARD
C***********************************************************************
C
C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'

C     Variable Initializations
      MODNAM = 'INITFL'

      IF (MULTYR) THEN
C        WRITE Error Message:  Conflicting Options RE-START and MULTYEAR
         CALL ERRHDL(PATH,MODNAM,'E','145',KEYWRD)
      ELSE IF (IFC .EQ. 3) THEN
         RSTINP = .TRUE.
         INIFIL = RUNST1(LOCB(3):LOCE(3))
      ELSE IF (IFC .GT. 3) THEN
C        WRITE Error Message           ! Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
      ELSE
C        WRITE Warning Message          ! No Parameters - Use Default Name
         CALL ERRHDL(PATH,MODNAM,'W','207',KEYWRD)
         RSTINP = .TRUE.
         INIFIL = 'SAVE.FIL'
      END IF

 999  RETURN
      END

      SUBROUTINE ERRFIL
C***********************************************************************
C                 ERRFIL Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Process Error Message File Option
C                 From Runstream Input Image
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Error Message File Logical Switch and ERRMSG Filename
C
C        ERROR HANDLING:   Checks for No Parametes (uses default name);
C                          Checks for Too Many Parameters
C
C        CALLED FROM:   COCARD
C***********************************************************************
C
C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'

C     Variable Initializations
      MODNAM = 'ERRFIL'

      IF (IFC .EQ. 3) THEN
         ERRLST = .TRUE.
         MSGFIL = RUNST1(LOCB(3):LOCE(3))
      ELSE IF (IFC .EQ. 4) THEN
C*----   ISCSTM Modification: allow for NOCHKD option - jah 11/2/94
         IF (FIELD(4) .EQ. 'DEBUG') THEN
C*          DEBUG Option Selected
            ERRLST = .TRUE.
            MSGFIL = RUNST1(LOCB(3):LOCE(3))
            DEBUG  = .TRUE.

         ELSE IF (FIELD(4) .EQ. 'NOCHKD') THEN
C*          NOCHKD Option Selected - Bypass Checking of Date Sequence
            ERRLST = .TRUE.
            MSGFIL = RUNST1(LOCB(3):LOCE(3))
            NOCHKD = .TRUE.
         END IF

      ELSE IF (IFC .EQ. 5) THEN
         IF (FIELD(4) .EQ. 'DEBUG' .OR. FIELD(5) .EQ. 'DEBUG') THEN
C*          DEBUG Option Selected
            ERRLST = .TRUE.
            MSGFIL = RUNST1(LOCB(3):LOCE(3))
            DEBUG  = .TRUE.
         END IF
         IF (FIELD(4) .EQ. 'NOCHKD' .OR. FIELD(5) .EQ. 'NOCHKD') THEN
C*          NOCHKD Option Selected - Bypass Checking of Date Sequence
            ERRLST = .TRUE.
            MSGFIL = RUNST1(LOCB(3):LOCE(3))
            NOCHKD = .TRUE.
         END IF

      ELSE IF (IFC .GT. 5) THEN
C*----

C*       WRITE Error Message                      ! Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
      ELSE
C*       WRITE Warning Message              ! No Parameters - Use Default Name
         CALL ERRHDL(PATH,MODNAM,'W','207',KEYWRD)
         ERRLST = .TRUE.
         MSGFIL = 'ERRORS.LST'
      END IF
C*#

 999  RETURN
      END

      SUBROUTINE MYEAR
C***********************************************************************
C                 MYEAR Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Process RESTART File Save Option
C                 From Runstream Input Image
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: RSTSAV File Logical Switch and RESTART Filename
C
C        ERROR HANDLING:   Checks for No Parametes (uses default name);
C                          Checks for Too Many Parameters
C
C        CALLED FROM:   COCARD
C***********************************************************************
C
C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'

C     Variable Initializations
      MODNAM = 'MYEAR'

      IF (RSTSAV .OR. RSTINP) THEN
C        WRITE Error Message:  Conflicting Options RE-START and MULTYEAR
         CALL ERRHDL(PATH,MODNAM,'E','145',KEYWRD)
      ELSE IF (POLLUT .NE. 'PM10' .AND. POLLUT .NE. 'PM-10' .AND.
     &         POLLUT .NE. 'LEAD' .AND. POLLUT .NE. 'OTHER') THEN
C        WRITE Error Message:  Conflicting Options MULTYEAR For Wrong POLLUT
         CALL ERRHDL(PATH,MODNAM,'E','150',KEYWRD)
      ELSE IF (IFC .EQ. 3) THEN
         MULTYR = .TRUE.
         RSTSAV = .TRUE.
C        Use Character Substring to Retrieve Filenames to Maintain Case
         SAVFIL = RUNST1(LOCB(3):LOCE(3))
         SAVFL2 = SAVFIL
C        Value of INCRST is Set to 365 or 366 in SUB. MECARD
      ELSE IF (IFC .EQ. 4) THEN
         MULTYR = .TRUE.
         RSTSAV = .TRUE.
C        Use Character Substring to Retrieve Filenames to Maintain Case
         SAVFIL = RUNST1(LOCB(3):LOCE(3))
         SAVFL2 = SAVFIL
         RSTINP = .TRUE.
         INIFIL = RUNST1(LOCB(4):LOCE(4))
C        Value of INCRST is Set to 365 or 366 in SUB. MECARD
      ELSE IF (IFC .GT. 4) THEN
C        WRITE Error Message           ! Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
      ELSE
C        WRITE Warning Message          ! No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
      END IF

 999  RETURN
      END
