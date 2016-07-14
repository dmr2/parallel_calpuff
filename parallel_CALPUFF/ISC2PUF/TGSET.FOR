      SUBROUTINE TGCARD
C***********************************************************************
C                 TGCARD Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: To process Terrain Grid Pathway Card Images
C
C        PROGRAMMER: D. Strimaitis, SRC
C
C        DATE:    December 15, 1993
C
C        INPUTS:  Pathway (TG) and Keyword
C
C        OUTPUTS: Terrain Grid Filename
C                 Origin to use with Terrain Grid (shift in UTM coord.,
C                   must be same as that used for sources/receptors,
C                   but this is not checked!)
C                 Gridded Terrain Data
C
C        CALLED FROM:   SETUP
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'

C     Variable Initializations
      MODNAM = 'TGCARD'

      IF (KEYWRD .EQ. 'STARTING') THEN
C        Set Status Switch
         ITSTAT(1) = ITSTAT(1) + 1
         IF (ITSTAT(1) .NE. 1) THEN
C           WRITE Error Message: Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         END IF
      ELSE IF (KEYWRD .EQ. 'INPUTFIL') THEN
C        Set Status Switch
         ITSTAT(2) = ITSTAT(2) + 1
         IF (ITSTAT(2) .NE. 1) THEN
C           WRITE Error Message: Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE
C           Process Gridded Terrain File Information         ---   CALL TERFIL
            CALL TERFIL
         END IF
      ELSE IF (KEYWRD .EQ. 'LOCATION') THEN
C        Set Status Switch
         ITSTAT(3) = ITSTAT(3) + 1
         IF (ITSTAT(3) .NE. 1) THEN
C           WRITE Error Message: Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE
C           Process Location (origin) Information           ---   CALL TERLOC
            CALL TERLOC
         END IF
      ELSE IF (KEYWRD .EQ. 'ELEVUNIT') THEN
C        Set Status Switch
         ITSTAT(4) = ITSTAT(4) + 1
         IF (ICSTAT(10) .NE. 0) THEN
C           Write Error Message: Use of obsolescent CO ELEVUNIT card with
C           TG ELEVUNIT card
            CALL ERRHDL(PATH,MODNAM,'E','153',' TG Path')
         ELSE IF (ITSTAT(4) .NE. 1) THEN
C           WRITE Error Message: Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE
C           Process Elevation Units for Source Elevations   ---   CALL TGELUN
            CALL TGELUN
         END IF
      ELSE IF (KEYWRD .EQ. 'FINISHED') THEN
C        Set Status Switch
         ITSTAT(20) = ITSTAT(20) + 1
         IF (ITSTAT(20) .NE. 1) THEN
C           WRITE Error Message: Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
            GO TO 999
         END IF

C        Open Terrain Data File (Free-Format ASCII) and Process Data
         IF (ITSTAT(2) .NE. 0) THEN
            OPEN(UNIT=IZUNIT,ERR=99,FILE=TERINP,IOSTAT=IOERRN,
     &           STATUS='OLD')
            GOTO 100
C           Write Out Error Message for File OPEN Error
 99         CALL ERRHDL(PATH,MODNAM,'E','500',' TER-INP')
            GOTO 999
C                                                           ---   CALL TGDATA
100         CALL TGDATA(TGX0,TGY0,IZUNIT,TGELEV,IERRTG,
     &                  GRDXLL,GRDXUR,GRDYLL,GRDYUR,XYINT)
            IF (IERRTG .NE. 0) THEN
C              Write Out Error Message for File Grid Error
               CALL ERRHDL(PATH,MODNAM,'E','510',' TER-INP')
            ELSE
               LTGRID=.TRUE.
            END IF

C           Check to make sure that terrain grid covers all source/receptor
C           locations and check consistency of elevations   ---   CALL TGQA
            IF (LTGRID) THEN
               CALL TGQA
            END IF

         END IF

      ELSE
C        Write Error Message: Invalid Keyword for This Pathway
         CALL ERRHDL(PATH,MODNAM,'E','110',KEYWRD)
      END IF

 999  RETURN
      END

      SUBROUTINE TERFIL
C***********************************************************************
C                 TERFIL Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Process Meteorology Input File Options
C                 From Runstream Input Image
C
C        PROGRAMMER: D. Strimaitis, SRC
C
C        DATE:    December 15, 1993
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Gridded Terrain Data Filename
C
C        ERROR HANDLING:   Checks for No Parameters;
C                          Checks for Too Many Parameters
C
C        CALLED FROM:   TGCARD
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'

C     Variable Initializations
      MODNAM = 'TERFIL'

      IF (IFC .EQ. 3) THEN
C        Retrieve Data Filename as Character Substring to Maintain Case
         TERINP = RUNST1(LOCB(3):LOCE(3))
      ELSE IF (IFC .GT. 3) THEN
C        WRITE Error Message           ! Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
      ELSE
C        WRITE Warning Message         ! No Parameters Specified
         CALL ERRHDL(PATH,MODNAM,'W','200',KEYWRD)
      END IF

      RETURN
      END

      SUBROUTINE TERLOC
C***********************************************************************
C                 TERLOC Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Process Location to be used as Origin of Grid Coord.
C                 From Runstream Input Image
C
C        PROGRAMMER: D. Strimaitis, SRC
C
C        DATE:    December 15, 1993
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Origin of Cartesian System, TGX0,TGY0 (UTM m)
C
C        ERROR HANDLING:   Checks for No Parameters;
C                          Checks for No Units (uses default of m);
C                          Checks for Too Many Parameters
C
C        CALLED FROM:   TGCARD
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'

C     Variable Initializations
      MODNAM = 'TERLOC'

      IF (IFC .EQ. 4 .OR. IFC .EQ. 5) THEN
         CALL STONUM(FIELD(3),40,TGX0,IDUM1)
C        Check The Numerical Field
         IF (IDUM1.EQ.-1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            GO TO 999
         END IF
         CALL STONUM(FIELD(4),40,TGY0,IDUM2)
C        Check The Numerical Field
         IF (IDUM2.EQ.-1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            GO TO 999
         END IF
         IF (IFC .EQ. 5) THEN
            IF (FIELD(5) .EQ. 'FEET') THEN
               TGX0 = 0.3048 * TGX0
               TGY0 = 0.3048 * TGY0
            ELSE IF (FIELD(5) .EQ. 'KM') THEN
               TGX0 = 1000. * TGX0
               TGY0 = 1000. * TGY0
            ELSE IF (FIELD(5) .NE. 'METERS') THEN
C              WRITE Warning Message - Invalid TGUNIT Parameter
               CALL ERRHDL(PATH,MODNAM,'W','203','TGUNIT')
            END IF
         ELSE IF (IDUM1*IDUM2 .NE. 1) THEN
C           WRITE Error Message - Invalid Numeric Field
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         END IF
      ELSE IF (IFC .GT. 5) THEN
C        WRITE Error Message           ! Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
      ELSE
C        WRITE Error Message           ! No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
      END IF

 999  RETURN
      END

      SUBROUTINE TGELUN
C***********************************************************************
C                 TGELUN Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Process Elevation Units Option for Terrain Grid
C                 From Runstream Input Image
C
C        PROGRAMMER: Roger Brode
C
C        DATE:    November 22, 1994
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Terrain Grid Elevation Units Switch
C
C        ERROR HANDLING:   Checks for Invalid Parameters;
C                          Checks for No Parameters;
C                          Checks for Too Many Parameters
C
C        CALLED FROM:   TGCARD
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'

C     Variable Initializations
      MODNAM = 'TGELUN'

      IF (IFC .EQ. 3) THEN
         IF (FIELD(3) .EQ. 'METERS') THEN
            TGELEV = 'METERS'
         ELSE IF (FIELD(3) .EQ. 'FEET') THEN
            TGELEV = 'FEET'
         ELSE
C           WRITE Error Message  ! Invalid Parameter
            CALL ERRHDL(PATH,MODNAM,'E','203','TG_ELEV')
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

c-----------------------------------------------------------------------
      subroutine tgdata(tgx0,tgy0,io,tgelev,ierr,grdxll,grdxur,grdyll,
     &                  grdyur,xyint)
c-----------------------------------------------------------------------
c
c --- ISCST2    Version: 1.0            Level: 931215           TGDATA
c               D. Strimaitis, SRC
c
c PURPOSE:     Subroutine reads terrain elevation data (m MSL) from
c              file, and places it in array for use by ZTERR.
c
c MODIFIED:    To elevation units from feet to meters if
c              TGELEV = 'FEET'.  Roger W. Brode, PES, Inc. - 11/22/94
c
c ARGUMENTS:
c    PASSED:  tgx0      x-UTM offset of modeling coord. system (m)   [r]
c             tgy0      y-UTM offset of modeling coord. system (m)   [r]
c             io        FORTRAN unit number for gridded terrain data [i]
c             tgelev    TG elevation units option ('FEET', 'METERS') [c]
c
c  RETURNED:  ierr      error condition indicator (no error = 0)     [i]
c             grdxllm   x-coord. of lower-left corner of grid  (m)   [r]
c             grdxurm   x-coord. of upper-right corner of grid (m)   [r]
c             grdyllm   y-coord. of lower-left corner of grid  (m)   [r]
c             grdyurm   y-coord. of upper-right corner of grid (m)   [r]
c             xyint     spacing between points in grid (m)           [r]
c
c To /TGRID/
c             xllm      x-coord. of lower-left corner of grid  (m)   [r]
c             xurm      x-coord. of upper-right corner of grid (m)   [r]
c             yllm      y-coord. of lower-left corner of grid  (m)   [r]
c             yurm      y-coord. of upper-right corner of grid (m)   [r]
c             sizem     spacing between points in grid (m)           [r]
c             izarray   array of terrain data  (whole m MSL)         [i]
c
c     (NOTE:  The coordinates of the lower-left and upper-right corners
c             of the grid are returned as arguments so that they can be
c             placed in MAIN1.INC for QA checks against source and
c             receptor locations.)
c
c
c CALLING ROUTINES:   SETUP
c
c EXTERNAL ROUTINES:  none
c-----------------------------------------------------------------------
      include 'DEPVAR.INC'
      character tgelev*6

c --- Initialize error flag
      ierr=0

c --- Read header
      read(io,*) ntx,nty,xllm,yllm,xurm,yurm,sizem

c --- Check number of points against the allocated limits
      if(ntx .GT. mxtx .OR. nty .GT. mxty) then
         write(*,*) 'TGDATA:  Fatal Error -- too many points!'
         write(*,*) 'ntx,nty     =',ntx,nty
         write(*,*) 'mxtx,mxty   =',mxtx,mxty
         write(*,*) 'Increase the parameters in DEPVAR.INC'
         ierr=1

      else

c ---    Reset coordinates of corners of grid to align with origin used
c ---    to specify souce/receptor locations.
         xllm=xllm-tgx0
         yllm=yllm-tgy0
         xurm=xurm-tgx0
         yurm=yurm-tgy0

c ---    Assign corners to variables passed back to calling subroutine
         grdxll=xllm
         grdyll=yllm
         grdxur=xurm
         grdyur=yurm
c ---    Assign interval size to variable passed back to calling subroutine
         xyint = sizem

c ---    Read data into array
         do jy=1,nty
            read(io,*) (izarray(ix,jy),ix=1,ntx)
         enddo

      endif

c --- Check for units conversion from feet to meters - R. Brode 11/22/94
      if (tgelev .eq. 'FEET') THEN
         do jy=1,nty
            do ix=1,ntx
               izarray(ix,jy) = izarray(ix,jy) * 0.3048
            enddo
         enddo
      endif

      return
      end

c-----------------------------------------------------------------------
      subroutine tgqa
c-----------------------------------------------------------------------
c
c --- ISCST2    Version: 1.0            Level: 931215           TGQA
c               D. Strimaitis, SRC
c
c PURPOSE:     Subroutine checks source/receptor locations against
c              the corners of the terrain grid to assure that all lie
c              within the grid.
c
c MODIFIED:    To compare interpolated elevations from grid file against
c              source elevations and receptor elevations.
c              Roger W. Brode, PES, Inc. - 11/29/94
c
c CALLING ROUTINES:   SETUP
c
c EXTERNAL ROUTINES:  none
c-----------------------------------------------------------------------

c --- Variable Declarations
      include 'MAIN1.INC'
      include 'MAIN2.INC'
      real params(4), xval(4), yval(4)

c --- Define a test logical
      logical lfail
      data lfail/.FALSE./

c --- Variable Initializations
      modnam = 'TGQA'

c --- Loop over sources to find max/min x and y coordinates
c --- (Does NOT treat Area Sources !!)
      xsmin=axs(1)
      ysmin=ays(1)
      xsmax=axs(1)
      ysmax=ays(1)
      do i=2,numsrc
         if(axs(i) .GT. xsmax) then
            xsmax=axs(i)
         elseif(axs(i) .LT. xsmin) then
            xsmin=axs(i)
         endif
         if(ays(i) .GT. ysmax) then
            ysmax=ays(i)
         elseif(ays(i) .LT. ysmin) then
            ysmin=ays(i)
         endif
      enddo

c --- Loop over receptors to find max/min x and y coordinates
      xrmin=axr(1)
      yrmin=ayr(1)
      xrmax=axr(1)
      yrmax=ayr(1)
      do i=2,numrec
         if(axr(i) .GT. xrmax) then
            xrmax=axr(i)
         elseif(axr(i) .LT. xrmin) then
            xrmin=axr(i)
         endif
         if(ayr(i) .GT. yrmax) then
            yrmax=ayr(i)
         elseif(ayr(i) .LT. yrmin) then
            yrmin=ayr(i)
         endif
      enddo

c --- Test max/min against corners of terrain grid
      xlltest=AMIN1(xsmin,xrmin)
      ylltest=AMIN1(ysmin,yrmin)
      xurtest=AMAX1(xsmax,xrmax)
      yurtest=AMAX1(ysmax,yrmax)
      if(xlltest .LT. grdxll) lfail=.TRUE.
      if(ylltest .LT. grdyll) lfail=.TRUE.
      if(xurtest .GT. grdxur) lfail=.TRUE.
      if(yurtest .GT. grdyur) lfail=.TRUE.

      if(LFAIL) then
C        Write Error Message: Invalid Keyword for This Pathway
         call ERRHDL(PATH,MODNAM,'E','305','GRID')
         write(iounit,*) 'Lower Left of Source Range   : ',xsmin,ysmin
         write(iounit,*) 'Upper Right of Source Range  : ',xsmax,ysmax
         write(iounit,*) 'Lower Left of Receptor Range : ',xrmin,yrmin
         write(iounit,*) 'Upper Right of Receptor Range: ',xrmax,yrmax
         write(iounit,*) 'Lower Left of Terrain Grid   : ',grdxll,grdyll
         write(iounit,*) 'Upper Right of Terrain Grid  : ',grdxur,grdyur
      endif

c     Loop through sources to compare source elevations to terrain grid
      do i = 1, numsrc
c        Set indices for 4-corners to interpolate elevation to source location
         indx1 = INT((axs(i) - grdxll)/xyint)
         indx2 = indx1 + 1
         indy1 = INT((ays(i) - grdyll)/xyint)
         indy2 = indy1 + 1
c        Extract elevations from Terrain Grid for the 4 corners
         call TGEXT(indx1,indx2,indy1,indy2,params)
         xval(1) = indx1
         xval(2) = indx2
         xval(3) = indx1
         xval(4) = indx2
         yval(1) = indy1
         yval(2) = indy1
         yval(3) = indy2
         yval(4) = indy2

c        Interpolate to obtain elevation at source location
         call INTERP(params,xval,yval,axs(i),ays(i),zint)

c        Compare interpolated height from terrain grid to source elevation
         if (azs(i) .lt. 0.5*zint .or. azs(i) .gt. 1.5*zint) then
            call ERRHDL(path,modnam,'W','393',srcid(i))
         end if

      enddo

c     Loop through receptors to compare receptor elevations to terrain grid
      do i = 1, numrec
c        Set indices for 4-corners to interpolate elevation to receptor location
         indx1 = INT((axr(i) - grdxll)/xyint)
         indx2 = indx1 + 1
         indy1 = INT((ayr(i) - grdyll)/xyint)
         indy2 = indy1 + 1
c        Extract elevations from Terrain Grid for the 4 corners
         call TGEXT(indx1,indx2,indy1,indy2,params)
         xval(1) = indx1
         xval(2) = indx2
         xval(3) = indx1
         xval(4) = indx2
         yval(1) = indy1
         yval(2) = indy1
         yval(3) = indy2
         yval(4) = indy2

c        Interpolate to obtain elevation at receptor location
         call INTERP(params,xval,yval,axr(i),ayr(i),zint)

c        Compare interpolated height from terrain grid to receptor elevation
         if (azelev(i) .lt. 0.5*zint .or. azelev(i) .gt. 1.5*zint) then
            write(dummy,'(3hRE#,i5.5)') i
            call ERRHDL(path,modnam,'W','394',dummy)
         end if

      enddo

      return
      end

      subroutine tgext(ix1,ix2,iy1,iy2,params)
C------------------------------------------------------------
C     ROUTINE: tgext
C     
C     PURPOSE: Extract terrain elevations from terrain grid array
C     for four points
C     
C     ARGUMENTS PASSED/RETURNED:
C     
C     PASSED:  
C     ix1       i4  First x-coordinate
C     ix2       i4  Second x-coordinate
C     iy1       i4  First y-coordinate
C     iy2       i4  Second y-coordinate
C     
C     RETURNED: 
C     params   r4  Array of grid values at (x1,y1), (x1,y2), (x2,y1) & (x2,y2)
C     
C     I/O:           NONE
C     
C     COMMON BLOCKS: DEPVAR.INC
C     
C     
C     EXTERNAL ROUTINES: NONE
C     
C------------------------------------------------------------

      include 'DEPVAR.INC'
      real params(*)

      params(1) = izarray(ix1,iy1)
      params(2) = izarray(ix2,iy1)
      params(3) = izarray(ix1,iy2)
      params(4) = izarray(ix2,iy2)

      return
      end


      SUBROUTINE INTERP(PARAMS,X1,Y1,XVALUE,YVALUE,VALUE)
C------------------------------------------------------------
C     ROUTINE: INTERP
C     
C     PURPOSE: LINEARLY INTERPOLATES BETWEEN FOUR VALUES AT FOUR POINTS
C     ON A PLANE TO GET A VALUE AT ONE POINT
C     
C     ARGUMENTS PASSED/RETURNED:
C     
C     PASSED:  
C     PARAMS   R4  KNOWN VALUES AT THE FOUR VERTICES OF A RECTANGLE
C     X1       R4  X COORDINATE LOCATION OF THE FOUR POINTS
C     Y1       R4  Y COORDINATE LOCATION OF THE FOUR POINTS
C     XVALUE   R4  X COORDINATE AT WHERE THE VALUE IS DESIRED
C     YVALUE   R4  Y COORDINATE AT WHERE THE VALUE IS DESIRED
C     
C     RETURNED: 
C     VALUE    R4  VALUE AT THE POINT DESIRED
C     
C     I/O:           NONE
C     
C     COMMON BLOCKS: NONE
C     
C     
C     EXTERNAL ROUTINES: NONE
C     
C------------------------------------------------------------
      DIMENSION PARAMS(4), X1(4), Y1(4)
C     
      XVALUM = XVALUE
      IF(XVALUM .GT. X1(2)) XVALUM = X1(2)
      IF(XVALUM .LT. X1(1)) XVALUM = X1(1)
      YVALUM = YVALUE
      IF(YVALUM .GT. Y1(3)) YVALUM = Y1(3)
      IF(YVALUM .LT. Y1(1)) YVALUM = Y1(1)
C     
      IF (X1(2) - X1(1) .LE. 0.) THEN
         X2MX = 1.0
         XMX1 = 0.0
      ELSE
         XBLKI = 1./(X1(2) - X1(1))
         X2MX  = (X1(2) - XVALUM)*XBLKI
         XMX1  = (XVALUM - X1(1))*XBLKI
      ENDIF
C     
      PINT1 = PARAMS(1)*X2MX + PARAMS(2)*XMX1
      PINT2 = PARAMS(3)*X2MX + PARAMS(4)*XMX1
C     
      IF (Y1(3) - Y1(1) .LE. 0.) THEN
         Y2MY = 1.0
         YMY1 = 0.0
      ELSE
         YBLKI = 1./(Y1(3) - Y1(1))
         Y2MY  = (Y1(3) - YVALUM)*YBLKI
         YMY1  = (YVALUM - Y1(1))*YBLKI
      ENDIF
C     
      VALUE = PINT1*Y2MY + PINT2*YMY1
C     
      RETURN
      END
