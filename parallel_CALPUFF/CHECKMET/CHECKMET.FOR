      Program CheckAusplumeAndISCMet

c******************************************************************************
C     Ausplume and ISC3 Meteorological Data Checking Utility
C
C     Vincent Tino
C     Earth Tech, Inc.
C     196 Baker Ave.
C     Concord, MA 01742
C     (508) 371-4200
C
c******************************************************************************

      include 'checkmet.inc'
      include 'defaults.inc'
c
      open (unit0,Err=1,File="input.tmp",status='old')

c --- Set flags to check specific data and override default ranges
c --- (Namelist format)
      read (unit0,opts)
      read (unit0,ausopts)
      read (unit0,iscopts)
      read (unit0,auscheck)
      read (unit0,isccheck)

c --- Write Banner
1     write (*,1000)

c --- get command line input data and output list file
      call getcom('CHECKMET',datafile,listfile,cmodel,chktype)

      write (*,1003) datafile,listfile,cmodel,chktype
1003  format (' Data file: ',a70,/,' List file: ',a70,/,
     &      ' Model ID (AUS or ISC): ',a3,/,
     &      ' Record Type (REG or EXT): ',a3,//)

c --- Open files depending on model
      if (cmodel.eq.'AUS'.or.cmodel.eq.'aus') then
        open (unit1,File=datafile,status='old')
        open (unit2,File=listfile,status='unknown')
        write (unit2,1000)
        Write (unit2,1001) ' AUSPLUME MET FILE'
      else if (cmodel.eq.'ISC'.or.cmodel.eq.'isc') then
        open (unit1,File=datafile,status='old')
        open (unit2,File=listfile,status='unknown')
        write (unit2,1000)
        write (unit2,1001) ' ISC MET FILE'
      else
        write (*,1002)
        stop
      end if

c --- Write options to output list file
      write (unit2,1005)
      write (unit2,opts)

1000  format (/15x,'***********************************************',/
     &        ,15x,'*                                             *',/
     &        ,15x,'*                  CHECKMET                   *',/
     &        ,15x,'*                                             *',/
     &        ,15x,'*             METEOROLOGICAL FILE             *',/
     &        ,15x,'*               CHECKING UTILITY              *',/
     &        ,15x,'*                                             *',/
     &        ,15x,'*         Level: 1.0    Version: 960628       *',/
     &        ,15x,'*                                             *',/
     &        ,15x,'***********************************************',//)
1001  format (' Processing:  ',a20)
1002  format (' Error:  Model Type Error!  Use "AUS" or "ISC" only')
1005  format (' Options Selected: ')

c******************************************************************************

C --- Read Header Line

      if (cmodel.eq.'AUS'.or.cmodel.eq.'aus') then
c --- Read 1 PLMMET header record
        read(unit1,1010,err=9999) title
        write (unit2,1020) title

      else if (cmodel.eq.'ISC'.or.cmodel.eq.'isc') then
c --- Read 1 ISC MET header record
        read(unit1,'(a130)',err=9999) title
        write (unit2,1120) title
      end if


      do numhr=1,8790

C*** IF AUSPLUME METEOROLOGICAL DATA

      if (cmodel.eq.'AUS'.or.cmodel.eq.'aus') then

C --- Read one line of hourly data as character to compare to blanks
      if (chktype.eq.'REG'.or.chktype.eq.'reg') then
        read(unit1,1030,end=999,err=9999)iy,im,id,ih,ctmpc,cwspd,ciwd,
     &            stab,czmix,csigtha,cpfield,ctfield,cdfield

c --- Extended data section
      else if (chktype.eq.'EXT'.or.chktype.eq.'ext') then
        read(unit1,1040,end=999,err=9999)iy,im,id,ih,ctmpc,cwspd,ciwd,
     &            stab,czmix,csigtha,cpfield,ctfield,cdfield,
     &            cpcfield,cprfield,cswfield,crhfield
      else
        write(unit2,1050)
        write(*,1050)
        stop
      end if

C***  ELSE IF ISC3 METEOROLOGICAL DATA

      else if (cmodel.eq.'ISC'.or.cmodel.eq.'isc') then

C --- Read one line of hourly data as character to compare to blanks
      if (chktype.eq.'REG'.or.chktype.eq.'reg') then
        read(unit1,1130,end=999,err=9999)iy,im,id,ih,cflow,cws,ctmp,
     &            cstab,crural,curban

c --- Extended data section
      else if (chktype.eq.'EXT'.or.chktype.eq.'ext') then
        read(unit1,1140,end=999,err=9999)iy,im,id,ih,cflow,cws,ctmp,
     &            cstab,crural,curban,custar,cmonin,czknot,cpcode,
     &            cprate,cdthdz,cpexp,csw,crh
      else
        write(unit2,1150)
        write(*,1150)
        stop
      end if

1010  format (A132)
1020  format (/' Title of Met file: ',/,4x,a132,//)
1030  format(BN,4i2,a3,a5,a4,a2,a5,a5,2a5,a10)
1040  format(BN,4i2,a3,a5,a4,a2,a5,a5,2a5,a10,
     &            a3,a9,a9,a3)
1050  format (' Fatal Error: Data file type misdefined!' )
1120  format (/' ID and Year of Station Data: ',/,4x,a132,//)
1130  format(BN,4i2,a9,a9,a6,a2,a7,a7)
1140  format(BN,4i2,a9,a9,a6,a2,a7,a7,a9,a10,a8,a4,a7,a10,a5,a9,a3)
1150  format (' Fatal Error: Data file type misdefined!' )

C***  END READING 1 HOUR OF DATA
      end if

c******************************************************************************

C** Check to see if record is between selected beginning and ending time period
      ibegin = (ibegyr*1000000)+(ibegmo*10000)+(ibegdy*100)+ibeghr
      iend = (iendyr*1000000)+(iendmo*10000)+(ienddy*100)+iendhr
      inow = (iy*1000000)+(im*10000)+(id*100)+ih
      if(ibegin.ne.0) then
        if (inow.lt.ibegin.or.inow.gt.iend) goto 900
      endif

C --- Write status message to screen
      write (*,1060) im,id,iy,ih
      ihrcount = ihrcount + 1
1060  format ('+',' Processing Date - Month: ',i2,' Day: ',i2,' Year: ',
     &i2,' Hour: ',i2)

C** CHECK FOR DATE OUT OF ORDER
      if(iexpmo.ne.0) then
        if (iy .ne. iexpyr .or. im .ne. iexpmo .or. id .ne. iexpdy
     &        .or. ih .ne. iexphr) then
            write(unit2,1070) im,id,iy,ih,iexpmo,iexpdy,iexpyr,iexphr
            misscount=misscount+1
            goto 900
        end if
      endif
1070  format(/' Current Date Not Matching Expected Date!  ',
     &   'Possible Period Missing! ',/,5x,
     &   'Month: ',i2,' Day: ',i2,' Year: ',i2,' Hour: ',i2,' <-> ',
     &   'Month: ',i2,' Day: ',i2,' Year: ',i2,' Hour: ',i2)

C** Convert Character Data to Numeric

200   if (cmodel.eq.'AUS'.or.cmodel.eq.'aus') then
        read (ctmpc,1200) tmpc
        read (cwspd,1201) wspd
        read (ciwd,1202) iwd
        read (czmix,1203) zmix
        read (csigtha,1204) sigtha
        read (cpfield,1205) pfield
        read (ctfield,1205) tfield
        read (cdfield,1206) dfield
        read (cpcfield,1207) pcfield
        read (cprfield,1205) prfield
        read (cswfield,1203) swfield
        read (crhfield,1207) rhfield
      else if (cmodel.eq.'ISC'.or.cmodel.eq.'isc') then
        read (cflow,1208) flow
        read (cws,1208) ws
        read (ctmp,1209) tmp
        read (crural,1211) rural
        read (curban,1211) urban
        read (cpexp,1213) pexp
        read (cdthdz,1213) dthdz
        read (custar,1208) ustar
        read (cmonin,1214) monin
        read (czknot,1213) zknot
        read (cpcode,1202) pcode
        read (cprate,1215) prate
        read (csw,1208) sw
        read (crh,1200) rh
      end if

1200  format (f3.0)
1201  format (f5.1)
1202  format (i4)
1203  format (f5.0)
1204  format (f5.1)
1205  format (f5.2)
1206  format (e10.1)
1207  format (i3)
1208  format (f9.4)
1209  format (f6.1)
1210  format (i2)
1211  format (f7.1)
1213  format (f8.4)
1214  format (f10.1)
1215  format (f7.2)


C** RESET ERROR CODES FOR NEW HOUR
      do i=1,20
        do j=1,20
          errcode(i,j)=0
        end do
      end do
      lerror=.false.

C******************************************************************************
C** BEGIN DATA CHECKING FOR AUSPLUME MET FILE

      if (cmodel.eq.'AUS'.or.cmodel.eq.'aus') then

c --- Check Temperature in Celsius
      if (ltempaus) then
        if (ctmpc.eq.'   ') then
            errcode(1,1)=1
            count(1,1) = count(1,1) + 1
        else if (tmpc.lt.mintempaus.or.tmpc.gt.maxtempaus) then
            errcode(1,2)=1
            count(1,2) = count(1,2) + 1
        end if
      end if

c --- Check Wind Speed
      if (lwsaus) then
        if (cwspd.eq.'     ') then
            errcode(2,1)=1
            count(2,1) = count(2,1) + 1
        else if (wspd.lt.minwsaus.or.wspd.gt.maxwsaus) then
            errcode(2,2)=1
            count(2,2) = count(2,2) + 1
        end if
      end if

c --- Check Wind Direction
      if (lwdaus) then
        if (ciwd.eq.'    ') then
            errcode(3,1)=1
            count(3,1) = count(3,1) + 1
        else if (iwd.lt.minwdaus.or.iwd.gt.maxwdaus) then
            errcode(3,2)=1
            count(3,2) = count(3,2) + 1
        end if
      end if

c --- Check Stability Class (A-F)
      if (lstabaus) then
        if (stab.eq.'  ') then
          errcode(4,1)=1
          count(4,1) = count(4,1) + 1
          goto 10
        else if (stab.eq.' A'.or.stab.eq.' B'.or.stab.eq.' C'.or.
     &           stab.eq.' D'.or.stab.eq.' E'.or.stab.eq.' F')then
          errcode(4,2)=0
          goto 10
        else
          errcode(4,2)=1
            count(4,2) = count(4,2) + 1
        end if
      end if

c --- Check Mixing Height
10    if (lziaus) then
        if (czmix.eq.'     ') then
            errcode(5,1)=1
            count(5,1) = count(5,1) + 1
        else if (zmix.lt.minziaus.or.zmix.gt.maxziaus) then
            errcode(5,2)=1
            count(5,2) = count(5,2) + 1
        end if
      end if

c --- Check Sigma Theta
      if (lsigtaus) then
        if (csigtha.eq.'     ') then
            errcode(6,1)=1
            count(6,1) = count(6,1) + 1
        else if (sigtha.lt.minsigaus.or.sigtha.gt.maxsigaus) then
            errcode(6,2)=1
            count(6,2) = count(6,2) + 1
        end if
      end if

c --- Check Wind Speed Power Law Exponent
      if (lpexpaus) then
        if (cpfield.eq.'     ') then
            errcode(7,1)=1
            count(7,1) = count(7,1) + 1
        else if (pfield.lt.minpaus.or.pfield.gt.maxpaus) then
            errcode(7,2)=1
            count(7,2) = count(7,2) + 1
        end if
      end if

c --- Check Potential Temperature Gradient
      if (ldthdzaus) then
        if (ctfield.eq.'     ') then
            errcode(8,1)=1
            count(8,1) = count(8,1) + 1
        else if (tfield.lt.mindthdzaus.or.tfield.gt.maxdthdzaus) then
            errcode(8,2)=1
            count(8,2) = count(8,2) + 1
        end if
      end if

c --- Check Decay Constant
      if (ldecayaus) then
        if (cdfield.eq.'          ') then
            errcode(9,1)=1
            count(9,1) = count(9,1) + 1
        else if (dfield.lt.mindecayaus.or.dfield.gt.maxdecayaus) then
            errcode(9,2)=1
            count(9,2) = count(9,2) + 1
        end if
      end if

C** CONTINUE DATA CHECKING FOR AUSPLUME EXTENDED FILE

      if (chktype.eq.'EXT'.or.chktype.eq.'ext') then

c --- Check PCfield
      if (lpcodeaus) then
        if (cpcfield.eq.'   ') then
            errcode(10,1)=1
            count(10,1) = count(10,1) + 1
        else if (pcfield.lt.minpcaus.or.pcfield.gt.maxpcaus) then
            errcode(10,2)=1
            count(10,2) = count(10,2) + 1
        end if
      end if

c --- Check PRfield
      if (lprateaus) then
        if (cprfield.eq.'     ') then
            errcode(11,1)=1
            count(11,1) = count(11,1) + 1
        else if (prfield.lt.minpraus.or.prfield.gt.maxpraus) then
            errcode(11,2)=1
            count(11,2) = count(11,2) + 1
        end if
      end if

c --- Check SWfield
      if (lswradaus) then
        if (cswfield.eq.'     ') then
            errcode(12,1)=1
            count(12,1) = count(12,1) + 1
        else if (swfield.lt.minswaus.or.swfield.gt.maxswaus) then
            errcode(12,2)=1
            count(12,2) = count(12,2) + 1
        end if
      end if

c --- Check RHfield
      if (lrhaus) then
        if (crhfield.eq.'   ') then
            errcode(13,1)=1
            count(13,1) = count(13,1) + 1
        else if (rhfield.lt.minrhaus.or.rhfield.gt.maxrhaus) then
            errcode(13,2)=1
            count(13,2) = count(13,2) + 1
        end if
      end if

c --- End extended check
      end if

c --- End AUSPLUME MET check
c******************************************************************************
C******************************************************************************
C** BEGIN DATA CHECKING FOR ISC MET FILE

      else if (cmodel.eq.'ISC'.or.cmodel.eq.'isc') then

c --- Check Flow Vector
      if (lflowisc) then
        if (cflow.eq.'         ') then
            errcode(1,1)=1
            count(1,1) = count(1,1) + 1
        else if (flow.lt.minflowisc.or.flow.gt.maxflowisc) then
            errcode(1,2)=1
            count(1,2) = count(1,2) + 1
        end if
      end if

c --- Check Wind Speed
      if (lwsisc) then
        if (cws.eq.'         ') then
            errcode(2,1)=1
            count(2,1) = count(2,1) + 1
        else if (ws.lt.minwsisc.or.ws.gt.maxwsisc) then
            errcode(2,2)=1
            count(2,2) = count(2,2) + 1
        end if
      end if

c --- Check Temperature in Kelvin
      if (ltempisc) then
        if (ctmp.eq.'      ') then
            errcode(3,1)=1
            count(3,1) = count(3,1) + 1
        else if (tmp.lt.mintempisc.or.tmp.gt.maxtempisc) then
            errcode(3,2)=1
            count(3,2) = count(3,2) + 1
        end if
      end if

c --- Check Stability Class (1-6)
      if (lstabisc) then
        if (cstab.eq.'  ') then
          errcode(4,1)=1
          count(4,1) = count(4,1) + 1
          goto 50
        else if (cstab.eq.' 1'.or.cstab.eq.' 2'.or.cstab.eq.' 3'.or.
     &           cstab.eq.' 4'.or.cstab.eq.' 5'.or.cstab.eq.' 6'.or.
     &           cstab.eq.' 7')then
          errcode(4,2)=0
          goto 50
        else
          errcode(4,2)=1
          count(4,2) = count(4,2) + 1
        end if
      end if

c --- Check Rural Mixing Height
50    if (lruralzisc) then
        if (crural.eq.'       ') then
            errcode(5,1)=1
            count(5,1) = count(5,1) + 1
        else if (rural.lt.minruralzisc.or.rural.gt.maxruralzisc) then
            errcode(5,2)=1
            count(5,2) = count(5,2) + 1
        end if
      end if

c --- Check Urban Mixing Height
      if (lurbanzisc) then
        if (curban.eq.'       ') then
            errcode(6,1)=1
            count(6,1) = count(6,1) + 1
        else if (urban.lt.minurbanzisc.or.urban.gt.maxurbanzisc) then
            errcode(6,2)=1
            count(6,2) = count(6,2) + 1
        end if
      end if

C** CONTINUE DATA CHECKING FOR ISC EXTENDED FILE

      if (chktype.eq.'EXT'.or.chktype.eq.'ext') then

c --- Check Friction Velocity
      if (lustarisc) then
        if (custar.eq.'         ') then
            errcode(7,1)=1
            count(7,1) = count(7,1) + 1
        else if (ustar.lt.minustarisc.or.ustar.gt.maxustarisc) then
            errcode(7,2)=1
            count(7,2) = count(7,2) + 1
        end if
      end if

c --- Check Monin-Obukhov Length
      if (lmoninisc) then
        if (cmonin.eq.'          ') then
            errcode(8,1)=1
            count(8,1) = count(8,1) + 1
        else if (monin.lt.minmoninisc.or.monin.gt.maxmoninisc) then
            errcode(8,2)=1
            count(8,2) = count(8,2) + 1
        end if
      end if

c --- Check Surface Roughness Length
      if (lzknotisc) then
        if (czknot.eq.'        ') then
            errcode(9,1)=1
            count(9,1) = count(9,1) + 1
        else if (zknot.lt.minzknotisc.or.zknot.gt.maxzknotisc) then
            errcode(9,2)=1
            count(9,2) = count(9,2) + 1
        end if
      end if

c --- Check Precip Code
      if (lpcodeisc) then
        if (cpcode.eq.'    ') then
            errcode(10,1)=1
            count(10,1) = count(10,1) + 1
        else if (pcode.lt.minpcodeisc.or.pcode.gt.maxpcodeisc) then
            errcode(10,2)=1
            count(10,2) = count(10,2) + 1
        end if
      end if

c --- Check Precip Rate
      if (lprateisc) then
        if (cprate.eq.'       ') then
            errcode(11,1)=1
            count(11,1) = count(11,1) + 1
        else if (prate.lt.minprateisc.or.prate.gt.maxprateisc) then
            errcode(11,2)=1
            count(11,2) = count(11,2) + 1
        end if
      end if

c --- Check Potential Temp. Gradient
      if (ldthdzisc) then
        if (cdthdz.eq.'        ') then
            errcode(12,1)=1
            count(12,1) = count(12,1) + 1
        else if (dthdz.lt.mindthdzisc.or.dthdz.gt.maxdthdzisc) then
            errcode(12,2)=1
            count(12,2) = count(12,2) + 1
        end if
      end if

c --- Check Power law Exponent
      if (lpexpisc) then
        if (cpexp.eq.'        ') then
            errcode(13,1)=1
            count(13,1) = count(13,1) + 1
        else if (pexp.lt.minpexpisc.or.pexp.gt.maxpexpisc) then
            errcode(13,2)=1
            count(13,2) = count(13,2) + 1
        end if
      end if

c --- Check Shortwave Radiation Field
      if (lswisc) then
        if (csw.eq.'         ') then
            errcode(14,1)=1
            count(14,1) = count(14,1) + 1
        else if (sw.lt.minswisc.or.sw.gt.maxswisc) then
            errcode(14,2)=1
            count(14,2) = count(14,2) + 1
        end if
      end if

c --- Check Relative Humidity
      if (lrhisc) then
        if (crh.eq.'   ') then
            errcode(15,1)=1
            count(15,1) = count(15,1) + 1
        else if (rh.lt.minrhisc.or.rh.gt.maxrhisc) then
            errcode(15,2)=1
            count(15,2) = count(15,2) + 1
        end if
      end if

c --- End extended check
      end if

c --- End ISC MET check
      end if
c***************************************************************************



c --- Write Errors
      do i=1,20
        do j=1,20
          if (errcode(i,j).eq.1) lerror=.true.
        end do
      end do

      if (lerror.and.(cmodel.eq.'AUS'.or.cmodel.eq.'aus')) then
        write (unit2,1500) im,id,iy,ih
        if (errcode(1,1).eq.1) write (unit2,1511)
        if (errcode(1,2).eq.1) write (unit2,1521) tmpc
        if (errcode(2,1).eq.1) write (unit2,1512)
        if (errcode(2,2).eq.1) write (unit2,1522) wspd
        if (errcode(3,1).eq.1) write (unit2,1513)
        if (errcode(3,2).eq.1) write (unit2,1523) iwd
        if (errcode(4,1).eq.1) write (unit2,1514)
        if (errcode(4,2).eq.1) write (unit2,1524) stab
        if (errcode(5,1).eq.1) write (unit2,1515)
        if (errcode(5,2).eq.1) write (unit2,1525) zmix
        if (errcode(6,1).eq.1) write (unit2,1516)
        if (errcode(6,2).eq.1) write (unit2,1526) sigtha
        if (errcode(7,1).eq.1) write (unit2,1517)
        if (errcode(7,2).eq.1) write (unit2,1527) pfield
        if (errcode(8,1).eq.1) write (unit2,1518)
        if (errcode(8,2).eq.1) write (unit2,1528) tfield
        if (errcode(9,1).eq.1) write (unit2,1519)
        if (errcode(9,2).eq.1) write (unit2,1529) dfield
        if (errcode(10,1).eq.1) write (unit2,15101)
        if (errcode(10,2).eq.1) write (unit2,15102) pcfield
        if (errcode(11,1).eq.1) write (unit2,15111)
        if (errcode(11,2).eq.1) write (unit2,15112) prfield
        if (errcode(12,1).eq.1) write (unit2,15121)
        if (errcode(12,2).eq.1) write (unit2,15122) swfield
        if (errcode(13,1).eq.1) write (unit2,15131)
        if (errcode(13,2).eq.1) write (unit2,15132) rhfield
      else if (lerror.and.(cmodel.eq.'ISC'.or.cmodel.eq.'isc')) then
        write (unit2,1500) im,id,iy,ih
        if (errcode(1,1).eq.1) write (unit2,1611)
        if (errcode(1,2).eq.1) write (unit2,1621) flow
        if (errcode(2,1).eq.1) write (unit2,1612)
        if (errcode(2,2).eq.1) write (unit2,1622) ws
        if (errcode(3,1).eq.1) write (unit2,1613)
        if (errcode(3,2).eq.1) write (unit2,1623) tmp
        if (errcode(4,1).eq.1) write (unit2,1614)
        if (errcode(4,2).eq.1) write (unit2,1624) cstab
        if (errcode(5,1).eq.1) write (unit2,1615)
        if (errcode(5,2).eq.1) write (unit2,1625) rural
        if (errcode(6,1).eq.1) write (unit2,1616)
        if (errcode(6,2).eq.1) write (unit2,1626) urban
        if (errcode(7,1).eq.1) write (unit2,1617)
        if (errcode(7,2).eq.1) write (unit2,1627) ustar
        if (errcode(8,1).eq.1) write (unit2,1618)
        if (errcode(8,2).eq.1) write (unit2,1628) monin
        if (errcode(9,1).eq.1) write (unit2,1619)
        if (errcode(9,2).eq.1) write (unit2,1629) zknot
        if (errcode(10,1).eq.1) write (unit2,16101)
        if (errcode(10,2).eq.1) write (unit2,16102) pcode
        if (errcode(11,1).eq.1) write (unit2,16111)
        if (errcode(11,2).eq.1) write (unit2,16112) prate
        if (errcode(12,1).eq.1) write (unit2,16121)
        if (errcode(12,2).eq.1) write (unit2,16122) dthdz
        if (errcode(13,1).eq.1) write (unit2,16131)
        if (errcode(13,2).eq.1) write (unit2,16132) pexp
        if (errcode(14,1).eq.1) write (unit2,16141)
        if (errcode(14,2).eq.1) write (unit2,16142) sw
        if (errcode(15,1).eq.1) write (unit2,16151)
        if (errcode(15,2).eq.1) write (unit2,16152) rh
      end if

1500  format (/' Date - Month:',i2,' Day: ',i2,' Year: ',i2,
     &      ' Hour: ',i2)
1511  format('     Warning:  Temperature Missing ')
1521  format('     Warning:  Temperature Value: ',i4)
1512  format('     Warning:  Wind Speed Missing ')
1522  format('     Warning:  Wind Speed Value: ',f5.1)
1513  format('     Warning:  Wind Direction Missing ')
1523  format('     Warning:  Wind Direction Value: ',i3)
1514  format('     Warning:  Stability Class Missing ')
1524  format('     Warning:  Stability Class Value: ',a2)
1515  format('     Warning:  Mixing Height Missing ')
1525  format('     Warning:  Mixing Height Value: ',f6.1)
1516  format('     Warning:  Sigma Theta Missing ')
1526  format('     Warning:  Sigma Theta Value: ',f5.0)
1517  format('     Warning:  Power Law Exponent Missing ')
1527  format('     Warning:  Power Law Exponent Value: ',f5.2)
1518  format('     Warning:  Potential Temp. Gradient Missing ')
1528  format('     Warning:  Potential Temp. Gradient Value: ',f5.3)
1519  format('     Warning:  Decay Constant Missing ')
1529  format('     Warning:  Decay Constant Value: ',f4.0)
15101 format('     Warning:  Precip Code Missing ')
15102 format('     Warning:  Precip Code Value: ',f4.0)
15111 format('     Warning:  Precip Rate Missing ')
15112 format('     Warning:  Precip Rate Value: ',f4.0)
15121 format('     Warning:  Short Wave Radiation Missing ')
15122 format('     Warning:  Short Wave Radiation Value: ',f4.0)
15131 format('     Warning:  Relative Humidity Missing ')
15132 format('     Warning:  Relative Humidity Value: ',f4.0)

1611  format('     Warning:  Flow Vector Missing ')
1621  format('     Warning:  Flow Vector Value: ',f9.4)
1612  format('     Warning:  Wind Speed Missing ')
1622  format('     Warning:  Wind Speed Value: ',f9.4)
1613  format('     Warning:  Temperature Missing ')
1623  format('     Warning:  Temperature Value: ',f6.1)
1614  format('     Warning:  Stability Class Missing ')
1624  format('     Warning:  Stability Class Value: ',a2)
1615  format('     Warning:  Rural Mixing Height Missing ')
1625  format('     Warning:  Rural Mixing Height Value: ',f7.1)
1616  format('     Warning:  Urban Mixing Height Missing ')
1626  format('     Warning:  Urban Mixing Height Value: ',f7.1)
1617  format('     Warning:  Friction Velocity Missing ')
1627  format('     Warning:  Friction Velocity Value: ',f9.4)
1618  format('     Warning:  Monin-Obukhov Length Missing ')
1628  format('     Warning:  Monin-Obukhov Length Value: ',f10.1)
1619  format('     Warning:  Surface Roughness Length Missing ')
1629  format('     Warning:  Surface Roughness Length Value: ',f8.4)
16101 format('     Warning:  Precipitation Code Missing ')
16102 format('     Warning:  Precipitation Code Value: ',i4)
16111 format('     Warning:  Precipitation Rate Missing ')
16112 format('     Warning:  Precipitation Rate Value: ',f7.2)
16121 format('     Warning:  Potential Temp. Gradient Missing ')
16122 format('     Warning:  Potential Temp. Gradient Value: ',f8.4)
16131 format('     Warning:  Power Law Exponent Missing ')
16132 format('     Warning:  Power Law Exponent Value: ',f8.4)
16141 format('     Warning:  ShortWave Radiation Missing ')
16142 format('     Warning:  ShortWave Radiation Value: ',f9.2)
16151 format('     Warning:  Relative Humidity Missing ')
16152 format('     Warning:  Relative Humidity Value: ',f3.0)

C --- Calculate Next Expected Hour
c --- check for leap year
900   if (mod(iy,4) .eq. 0) iday(2) = 29

      iexpyr = iy
      iexpmo = im
      iexpdy = id
      iexphr = ih
c --- check for expected hour
      iexphr = iexphr+1
      if (iexphr .gt. 24) then
        iexphr = 1
        iexpdy = iexpdy + 1
        if (iexpdy .gt. iday(iexpmo)) then
          iexpdy = 1
          iexpmo = iexpmo + 1
          if (iexpmo .gt. 12) goto 999
        end if
      end if

999   end do

      write (unit2,1600) im,id,iy,ih

c --- Write Summary Table At end
      Write (unit2,1700)

      if (cmodel.eq.'AUS'.or.cmodel.eq.'aus') then

      Write (unit2,1701) '  Missing Periods .......... ',misscount
      if (ltempaus)
     &  Write (unit2,1701) '  Temperature .............. ',
     &       count(1,1),count(1,2)
      if (lwsaus)
     &  Write (unit2,1701) '  Wind Speed ............... ',
     &       count(2,1),count(2,2)
      if (lwdaus)
     &  Write (unit2,1701) '  Wind Direction ........... ',
     &       count(3,1),count(3,2)
      if (lstabaus)
     &  Write (unit2,1701) '  Stability Class .......... ',
     &       count(4,1),count(4,2)
      if (lziaus)
     &  Write (unit2,1701) '  Mixing Height ............ ',
     &       count(5,1),count(5,2)
      if (lsigtaus)
     &  Write (unit2,1701) '  Sigma Theta .............. ',
     &       count(6,1),count(6,2)
      if (lpexpaus)
     &  Write (unit2,1701) '  Power Law Exponent ....... ',
     &       count(7,1),count(7,2)
      if (ldthdzaus)
     &  Write (unit2,1701) '  Potential Temp. Gradient . ',
     &       count(8,1),count(8,2)
      if (ldecayaus)
     &  Write (unit2,1701) '  Decay Constant ........... ',
     &       count(9,1),count(9,2)

      if (chktype.eq.'EXT'.or.chktype.eq.'ext') then
        if (lpcodeaus)
     &  Write (unit2,1701) '  Precip. Code ............. ',
     &       count(10,1),count(10,2)
        if (lprateaus)
     &  Write (unit2,1701) '  Precip. Rate ............. ',
     &       count(11,1),count(11,2)
        if (lswradaus)
     &  Write (unit2,1701) '  Short Wave Radiation ..... ',
     &       count(12,1),count(12,2)
        if (lrhaus)
     &  Write (unit2,1701) '  Relative Humidity ........ ',
     &       count(13,1),count(13,2)
      end if

      else if (cmodel.eq.'ISC'.or.cmodel.eq.'isc') then

      Write (unit2,1701) '  Missing Periods .......... ',misscount
      if (lflowisc)
     &  Write (unit2,1701) '  Flow Vector............... ',
     &       count(1,1),count(1,2)
      if (lwsisc)
     &  Write (unit2,1701) '  Wind Speed ............... ',
     &       count(2,1),count(2,2)
      if (ltempisc)
     &  Write (unit2,1701) '  Temperature .............. ',
     &       count(3,1),count(3,2)
      if (lstabisc)
     &  Write (unit2,1701) '  Stability Class .......... ',
     &       count(4,1),count(4,2)
      if (lruralzisc)
     &  Write (unit2,1701) '  Rural Mixing Height ...... ',
     &       count(5,1),count(5,2)
      if (lurbanzisc)
     &  Write (unit2,1701) '  Urban Mixing Height ...... ',
     &       count(6,1),count(6,2)

      if (chktype.eq.'EXT'.or.chktype.eq.'ext') then
        if (lustarisc)
     &  Write (unit2,1701) '  Friction Velocity ........ ',
     &       count(7,1),count(7,2)
        if (lmoninisc)
     &  Write (unit2,1701) '  Monin-Obukhov Length ..... ',
     &       count(8,1),count(8,2)
        if (lzknotisc)
     &  Write (unit2,1701) '  Surface Roughness ........ ',
     &       count(9,1),count(9,2)
        if (lpcodeisc)
     &  Write (unit2,1701) '  Precip. Code ............. ',
     &       count(10,1),count(10,2)
        if (lprateisc)
     &  Write (unit2,1701) '  Precip. Rate ............. ',
     &       count(11,1),count(11,2)
        if (ldthdzisc)
     &  Write (unit2,1701) '  Potential Temp. Gradient . ',
     &       count(12,1),count(12,2)
        if (lpexpisc)
     &  Write (unit2,1701) '  Wind Profile Exponent .... ',
     &       count(13,1),count(13,2)
        if (lswisc)
     &  Write (unit2,1701) '  Short Wave Radiation ..... ',
     &       count(14,1),count(14,2)
        if (lrhisc)
     &  Write (unit2,1701) '  Relative Humidity ........ ',
     &       count(15,1),count(15,2)
      end if

      end if

      write (unit2,1702) ihrcount

1600  format (//' Last Date Processed - Month: ',i2,' Day: ',i2,
     &      ' Year: ',i2,' Hour: ',i2)
1700  format(20('*'),' Warning Message Summary ',20('*'),/,
     &    '       Variable                 Missing     Out of Range ',/,
     &    65('-'))
1701  format(1x,a30,2x,i5,5x,2x,i5)
1702  format(65('-'),/,
     &    '       Total Number of Hours Processed: ',i5)

      goto 10000

9999  Write (*,9000)
9000  format (' ERROR:  File does not match file type!')

c --- Close files
10000 close (unit0)
      close (unit1)
      close (unit2)


      stop
      end



      SUBROUTINE GETCOM (MODEL,INPFIL,OUTFIL,cmodel,chktype)
C***********************************************************************
C
C        ADAPTED FROM PCCODE Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Controls Retrieving Input and Output File Names From
C                 the Command Line for PCs
C
C        PROGRAMMER: Roger Brode
C
C        DATE:    March 2, 1992
C
C        MODIFIED:   Jayant Hardikar, PES, Inc.
C                    - Length of command line for Lahey version changed
C                      from 80 to 120 characters - 4/19/93
C                    - Adapted for DEPMET/PMERGE - 7/29/94
C        MODIFIED:   D.G. Strimaitis, EARTH TECH
C                    - Length of command line increased to 130, although
C                      Lahey function takes only 127 characters,
C                      and pathnames increased to 70    - 5/96
C        MODIFIED:   V. R. Tino, EARTH TECH
C                    - Added 2 extra variables to command line - 6/96
C        MODIFIED:   D.G. Strimaitis, EARTH TECH
C                    - Remove MACRO constructs for compiler  - 8/02
C
C        INPUTS:  Command Line
C
C        OUTPUTS: Input Runstream File Name
C                 Output Print File Name
C
C        CALLED FROM:   MAIN
C***********************************************************************
C
C     Variable Declarations
c-DGS- Increase size of in/out filenames to match MENU limits (70)
      CHARACTER*70 INPFIL,OUTFIL
c     CHARACTER*40 INPFIL,OUTFIL
      CHARACTER*8 MODEL

      character*3 cmodel,chktype

c$IF DEFINED (MICRO)
cC     Declare 2-Byte Integer for Field Number of Command Line Argument
c      INTEGER*2 IARG
c$ELSEIF DEFINED (LAHEY)
C     Declare the COMLIN Variable to Hold Contents of Command Line for Lahey
      CHARACTER*130 COMLIN
      INTEGER*2 LOCB(10),LOCE(10)
      LOGICAL INFLD
c$ENDIF

c$IF DEFINED (MICRO)
cC************************************************************MICRO START
cC     Use Microsoft Functions NARGS and GETARG To Retrieve
cC     Contents of Command Line
c      IFCNT = NARGS()
cC     IFCNT Is The Number Of Arguments on Command Line Including Program
ccvrt  IF (IFCNT .NE. 3) THEN
c      IF (IFCNT .NE. 5) THEN
cC        Error on Command Line.  Write Error Message and STOP
c         WRITE(*,660) MODEL
c         STOP
c      ELSE
cC        Retrieve First Argument as Input File Name
c         IARG = 1
c         CALL GETARG(IARG,INPFIL,ISTAT)
cC        Retrieve Second Argument as Output File Name
c         IARG = 2
c         CALL GETARG(IARG,OUTFIL,ISTAT)
c
cC        Retrieve third Argument as Model type
c         IARG = 3
c         CALL GETARG(IARG,cmodel,ISTAT)
cC        Retrieve Fourth Argument as Model record type
c         IARG = 4
c         CALL GETARG(IARG,chktype,ISTAT)
c      END IF
cC************************************************************MICRO STOP

c$ELSEIF DEFINED (LAHEY)
C************************************************************LAHEY START
C     Use Lahey Function GETCL To Retrieve Contents of Command Line.
C     Retrieve Input and Output File Names From the COMLIN Variable.
      CALL GETCL(COMLIN)

      INFLD = .FALSE.
      IFCNT = 0
      DO 100 I = 1, 130
         IF (.NOT.INFLD .AND. COMLIN(I:I) .NE. ' ') THEN
            INFLD = .TRUE.
            IFCNT = IFCNT + 1
            LOCB(IFCNT) = I
         ELSE IF (INFLD .AND. COMLIN(I:I) .EQ. ' ') THEN
            INFLD = .FALSE.
            LOCE(IFCNT) = I - 1
         END IF
 100  CONTINUE
cvrt  IF (IFCNT .NE. 2) THEN
      IF (IFCNT .NE. 4) THEN
C        Error on Command Line.  Write Error Message and STOP
         WRITE(*,660) MODEL
         STOP
      END IF

      INPFIL = COMLIN(LOCB(1):LOCE(1))
      OUTFIL = COMLIN(LOCB(2):LOCE(2))

      cmodel = COMLIN(LOCB(3):LOCE(3))
      chktype = COMLIN(LOCB(4):LOCE(4))

C************************************************************LAHEY STOP

c$ENDIF

  660 FORMAT (' COMMAND LINE ERROR: ',A8,' input_file output_file ',
     &      'model_type check_type')

      RETURN
      END
