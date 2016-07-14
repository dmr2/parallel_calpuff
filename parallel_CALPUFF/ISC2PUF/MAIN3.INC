C***********************************************************************
C     MAIN3.INC
C     ISCST2 Model Data - Array Names, Array Limits, Named Common Blocks
C                         Necessary for Model Results
C     MODIFIED - 4/17/95   Output CONC/DEPOS in same model run
C***********************************************************************


C***********************************************************************
C     This is The Global Variable Definition Block For The Maximum
C     Value, Highest Value, Average Value, Annual Average Value and
C     Model Result Arrays.  Also Included are Calm/Missing Flag Arrays.
C***********************************************************************

      INTEGER HMLOC, HMDATE
      CHARACTER HCLMSG, MCLMSG, HMCLM

      COMMON /RESULT/ HRVAL(NTYP), SIMPL(NTYP), COMPL(NTYP),
     &                AVEVAL(NREC,NGRP,NAVE,NTYP)
      COMMON /HIGRES/ HIVALU(NREC,NVAL,NGRP,NAVE,NTYP),
     &                NHIDAT(NREC,NVAL,NGRP,NAVE,NTYP),
     &                HMAX(NVAL,NGRP,NAVE,NTYP),
     &                HMLOC(NVAL,NGRP,NAVE,NTYP),
     &                HMDATE(NVAL,NGRP,NAVE,NTYP)

      COMMON /ANURES/ ANNVAL(NREC,NGRP,NTYP), AMXVAL(NVAL,NGRP,NTYP),
     &                IMXLOC(NVAL,NGRP,NTYP), IANHRS, IANCLM, IANMSG
      COMMON /MAXRES/ RMXVAL(NMAX,NGRP,NAVE,NTYP),
     &                MXDATE(NMAX,NGRP,NAVE,NTYP),
     &                MXLOCA(NMAX,NGRP,NAVE,NTYP)
      COMMON /NUMRES/ NUMHRS(NAVE), NUMCLM(NAVE), NUMMSG(NAVE)
      COMMON /CLMFLG/ HCLMSG(NREC,NVAL,NGRP,NAVE,NTYP),
     &                MCLMSG(NMAX,NGRP,NAVE,NTYP),
     &                HMCLM(NVAL,NGRP,NAVE,NTYP)

C     Declare Temporary Work Arrays for ZELEV and ZFLAG Receptor Data
      DIMENSION ZETMP1(NREC), ZETMP2(NREC)
      DIMENSION ZFTMP1(NREC), ZFTMP2(NREC)

C     EQUIVALENCE Temporary Work Arrays to Results Data Arrays to Save
C     On Storage Requirements.  Amount Saved = NREC*16 Bytes.
C     Results Arrays Are Reinitialized In SUB. RESINI.
      EQUIVALENCE  (ZETMP1, AVEVAL)
      EQUIVALENCE  (ZETMP2, HIVALU)
      EQUIVALENCE  (ZFTMP1, NHIDAT)
      EQUIVALENCE  (ZFTMP2, ANNVAL)
