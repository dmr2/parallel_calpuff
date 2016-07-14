c-----------------------------------------------------------------------
c --- ISCST2    Version: 1.0            Level: 931215        DEPVAR.INC
c               D. Strimaitis, SRC
c
c PURPOSE:     Include-file of parameters and commons for deposition
c
c MODIFIED:    Split terrain grid common blocks to avoid alignment warning.
c              R. Brode, PES, Inc. - 5/24/95
c
c MODIFIED:    Added variable kurdat to common.
c              R. Brode, PES, Inc. - 9/30/94
c
c-----------------------------------------------------------------------
C     The following commented line provides the array limits for the
C     Microsoft executable file.
CMICRO      parameter (mxtx=101,mxty=101)
c***  parameter (mxtx=601,mxty=601)
      parameter (mxtx=1101,mxty=1101)

c-----------------------------------------------------------------------
c --- COMMON BLOCK /DEPVAR/ --- Source Depletion Variables
c-----------------------------------------------------------------------
      LOGICAL rural,urban,lterr,debug
      CHARACTER srctyp*8
      common/depvar/vd,vs,zd,ap,bp,cp,ar,br,cr,
     &              igrav,rural,urban,kst,hmix,onebyu,
     &              er,ep,xsrc,ysrc,xrec,yrec,
     &              xr,xv,h,sgz,sgz0,xtd,sztd,szmn,
     &              rtpiby2,rt2,rtpi,lterr,debug,iounit, kurdat
      common/depchr/srctyp

c-----------------------------------------------------------------------
c --- COMMON BLOCK /TGRID/ --- Gridded Terrain Variables
c-----------------------------------------------------------------------
      LOGICAL ltgrid
      INTEGER*2 izarray(mxtx,mxty)
      common /TGRID1/ xllm,yllm,sizem,xurm,yurm
      common /TGRID2/ izarray
      common /TGRIDL/ ltgrid

c-----------------------------------------------------------------------
c     DEFINITIONS       [i]=integer     [r]=real       [l]=logical
c-----------------------------------------------------------------------
c                      - PARAMETERS -
c mxtx,mxty      maximum number of terrain points in x,y direction   [i]
c
c                         /DEPVAR/
c vd (m/s)       deposition velocity                                 [r]
c vs (m/s)       gravitational settling velocity                     [r]
c zd (m)         reference height for deposition calculation         [r]
c ap,bp,cp       coefficients for analytical form of profile fcn     [r]
c ar,br,cr       coefficients for resistance function                [r]
c igrav          flag for treatment of gravitational settling        [i]
c                  0:use analytical approximation to profile integral
c                  1:use numerical solution to profile integral
c rural,urban    logical indicators for dispersion site character    [l]
c kst            P-G stability class                                 [i]
c hmix (m)       mixing height                                       [r]
c onebyu (s/m)   reciprocal of wind speed                            [r]
c er (m)         elevation (MSL) of receptor location                [r]
c ep (m)         elevation (MSL) of point-source location            [r]
c (x,y)src (m)   source location                                     [r]
c (x,y)rec (m)   receptor location                                   [r]
c xr (m)         distance from source to receptor                    [r]
c xv (m)         virtual source distance upwind of actual source     [r]
c h (m)          plume centerline height (no gravitational settling) [r]
c sgz (m)        sigma-z                                             [r]
c sgz0 (m)       initial sigma-z (e.g. for BID)                      [r]
c xtd (m)        distance to centerline "touchdown" (settling)       [r]
c sgtd (m)       sigma-z at xtd                                      [r]
c szmn (m)       minimum allowed for "settling" sigma-z              [r]
c rtpiby2        square root of pi/2                                 [r]
c rt2            square root of 2                                    [r]
c rtpi           square root of pi                                   [r]
c lterr          logical controlling terrain adjustments             [l]
c debug          logical indicator for debug option                  [l]
c iounit         unit number for debug output                        [i]
c srctyp         source type (8 characters)                          [c]
c
c                         /TGRID/
c (x,y)llm   (m) coordinates of SW corner of cell1 (lower left
c                  corner) of terrain grid                           [r]
c sizem      (m) length of side of terrain grid cell                 [r]
c (x,y)urm   (m) coordinates of upper right corner of terrain grid   [r]
c izarray (m MSL) gridded terrain elevation data                     [i]
c
c  NOTE:  This grid contains terrain elevations at the corner of each
c         cell.  Therefore there are nx+1 points in x, and ny+1 points
c         in y.  The coordinates are assumed to be referenced to the
c         same origin used to specify source and receptor locations.
c-----------------------------------------------------------------------
