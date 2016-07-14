C****************************************************************
C     Ausplume and ISC3 Meteorological Data Checking Utility
C
C     Vincent Tino
C     Earth Tech, Inc.
C     196 Baker Ave.
C     Concord, MA 01742
C     (508) 371-4200
C
C****************************************************************

      integer unit0,unit1,unit2

      character*70 datafile,listfile
      character title*132              
      character ctmpc*3,cwspd*5,ciwd*4,stab*2,
     &      czmix*5,csigtha*5,cpfield*5,ctfield*5,cdfield*10,
     &      cpcfield*3,cprfield*5,cswfield*5,crhfield*3
      character cflow*9,cws*9,ctmp*6,cstab*2,crural*7,curban*7,
     &      cpexp*8,cdthdz*8,custar*9,cmonin*10,czknot*8,cpcode*4,
     &      cprate*7,csw*9,crh*3

      Parameter (unit0=9,unit1=10, unit2=11)   

      character cmodel*3,chktype*3
      integer ibegyr,ibegmo,ibegdy,ibeghr,iendyr,iendmo,ienddy,
     &      iendhr
      namelist /opts/ cmodel,chktype,ibegyr,ibegmo,ibegdy,ibeghr,
     &      iendyr,iendmo,ienddy,iendhr 

      logical ltempaus,lwsaus,lwdaus,lstabaus,lziaus,
     &      lsigtaus,lpexpaus,ldthdzaus,ldecayaus,lpcodeaus,
     &      lprateaus,lswradaus,lrhaus
      namelist /ausopts/ ltempaus,lwsaus,lwdaus,lstabaus,lziaus,
     &      lsigtaus,lpexpaus,ldthdzaus,ldecayaus,lpcodeaus,
     &      lprateaus,lswradaus,lrhaus

      logical lflowisc,lwsisc,ltempisc,lstabisc,lruralzisc,
     &      lurbanzisc,lpexpisc,ldthdzisc,lustarisc,lmoninisc,
     &      lzknotisc,lpcodeisc,lprateisc,lswisc,lrhisc
      namelist /iscopts/ lflowisc,lwsisc,ltempisc,lstabisc,lruralzisc,
     &      lurbanzisc,lpexpisc,ldthdzisc,lustarisc,lmoninisc,
     &      lzknotisc,lpcodeisc,lprateisc,lswisc,lrhisc

      real mintempaus,maxtempaus,minwsaus,maxwsaus,minziaus,maxziaus,
     &      minsigaus,maxsigaus,minpaus,maxpaus,mindthdzaus,maxdthdzaus,
     &      mindecayaus,maxdecayaus,minpraus,maxpraus,minswaus,
     &      maxswaus,minrhaus,maxrhaus
      integer minwdaus,maxwdaus,minpcaus,maxpcaus
      namelist /auscheck/ mintempaus,maxtempaus,minwsaus,maxwsaus,
     &      minwdaus,maxwdaus,minziaus,maxziaus,minsigaus,maxsigaus,
     &      minpaus,maxpaus,mindthdzaus,maxdthdzaus,mindecayaus,
     &      maxdecayaus,minpcaus,maxpcaus,minpraus,maxpraus,
     &      minswaus,maxswaus,minrhaus,maxrhaus

      real minwsisc,maxwsisc,
     &      mintempisc,maxtempisc,minruralzisc,maxruralzisc,
     &      minurbanzisc,maxurbanzisc,minpexpisc,maxpexpisc, 
     &      mindthdzisc,maxdthdzisc,minustarisc,maxustarisc,
     &      minmoninisc,maxmoninisc,minzknotisc,maxzknotisc,
     &      minprateisc,maxprateisc,minswisc,maxswisc,minrhisc,
     &      maxrhisc
      integer minflowisc,maxflowisc,minpcodeisc,maxpcodeisc
      namelist /isccheck/ minflowisc,maxflowisc,minwsisc,maxwsisc,
     &      mintempisc,maxtempisc,minruralzisc,maxruralzisc,
     &      minurbanzisc,maxurbanzisc,minpexpisc,maxpexpisc, 
     &      mindthdzisc,maxdthdzisc,minustarisc,maxustarisc,
     &      minmoninisc,maxmoninisc,minzknotisc,maxzknotisc,
     &      minpcodeisc,maxpcodeisc,minprateisc,maxprateisc,
     &      minswisc,maxswisc,minrhisc,maxrhisc


C****************************************************************
C
C     Variables and parameters for specific checks of data.
C
C****************************************************************

      logical lerror
      integer errcode(20,20),count(20,20),iday(12),misscount

c --- Set number of days for each month
      data iday /31,28,31,30,31,30,31,31,30,31,30,31/




