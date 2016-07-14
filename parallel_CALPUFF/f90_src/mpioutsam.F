c----------------------------------------------------------------------
      subroutine outsam(rarray,iarray,ityp,nsigd,ldate,messag,nx,ny)
c----------------------------------------------------------------------
c
c --- PARALLEL CALPUFF (V5.8)      Level: 000602                 OUTSAM
c     Original code writted by J. Scire, SRC
c
c     Modified by D.J. Rasmussen
c                 Department of Civil and Environmental Engineering
c                 University of California, Davis
c                
c --- PURPOSE:  Write a gridded field of real or integer numbers
c ---           Array dimensions sized for SAMPLING grid
c
c --- UPDATE
c --- PARALLEL(V5.8)120710  (DMR): Modified for PARALLEL CALPUFF        
c --- V5.2-V5.4     000602  (DGS): YYYY format for year
c --- V5.0-V5.2     991104  (DGS): YYYY format for year
c
c --- INPUTS:
c   RARRAY(MXNXG,MXNYG) - Real array  - Array of real numbers to print
c                                       (used only if ITYP = 1)
c   IARRAY(MXNXG,MXNYG) - Int. array  - Array of integer numbers to
c                                       print (used only if ITYP = 2)
c                  ITYP - Integer     - Array type (1=real, 2=integer)
c                 NSIGD - Integer     - No. digits to print (valid range
c                                       for NSIGD is 1 to 5)
c                 LDATE - Logical     - Control variable for printing
c                                       of date (.true. = print date in
c                                       common /GEN/, .false. = do not
c                                       print date)
c                MESSAG - Char.*70    - Label of table
c                    NX - Integer     - No. X grid cells being used in
c                                       array
c                    NY - Integer     - No. Y grid cells being used in
c                                       array
c       Use /MPIRANKTASKS/ variables:
c          MPINUMTASKS, MPIRANK
c       Use /MPIDECOMP/ variables:
c          MYSAMB, MYSAMT        
c       Common block /DATEHR/ variables:
c          NYR, NMO, NDAY, NJUL, NHR  - (Used only if LDATE=.true.)
c       Parameters: MXNXG, MXNYG, IO6
c
c --- OUTPUT:  none
c
c --- OUTSAM called by:  OUTPUT
c --- OUTSAM calls:      WRT, WRT2
c----------------------------------------------------------------------
c dmr
c --- Use statements
      use mpif
      use mpiranktasks
      use mpidecomp
c dmr
c --- Include parameters
      include 'params.puf'
c
c --- Include date/hour common block
      include 'datehr.puf'
c
      real rarray(mxnxg,mxnyg)
c dmr
      real mpirarray(mxnxg,mxnyg)
c dmr
c
      integer iarray(mxnxg,mxnyg),icol(5)
c dmr
      integer mpiiarray(mxnxg,mxnyg),asize,cnt
      integer alloffset(mpinumtasks),recvcnts(mpinumtasks)
      integer myendpts(2),allendpts(mpinumtasks*2)
c dmr
      integer iout(mxnxg)
c
      logical ldate
c
      character*70 messag
      character*1 sign(mxnxg),plus,minus
      character*24 form1(5)
      character*21 form2(5)
      character*18 form3(5)
c
      data icol /40,40,30,25,20/
      data plus,minus /'+','-'/
c MJK DEBUG
      data form1 /'(1x,i2,1x,1hI,40(i2,1x))',
     1            '(1x,i2,1x,1hI,40(i2,1x))',
     2            '(1x,i2,1x,1hI,40(i3,1x))',
     3            '(1x,i2,1x,1hI,40(i4,1x))',
     4            '(1x,i2,1x,1hI,40(i5,1x))'/
c      data form1 /'(1x,i3,1x,1hI,40(i2,1x))',
c     1            '(1x,i3,1x,1hI,40(i2,1x))',
c     2            '(1x,i3,1x,1hI,40(i3,1x))',
c     3            '(1x,i3,1x,1hI,40(i4,1x))',
c     4            '(1x,i3,1x,1hI,40(i5,1x))'/
c MJK DEBUG
      data form2 /'(4x,1hI,40(1x,a1,1x))',
     1            '(4x,1hI,40(1x,a1,1x))',
     2            '(4x,1hI,40(2x,a1,1x))',
     3            '(4x,1hI,40(3x,a1,1x))',
     4            '(4x,1hI,40(4x,a1,1x))'/
      data form3 /'(5x,40(i2,1x))',
     1            '(5x,40(i2,1x))',
     2            '(5x,40(i3,1x))',
     3            '(5x,40(i4,1x))',
     4            '(5x,40(i5,1x))'/
c
c --- check that valid values of array type (ityp) and print digits
c --- (nsigd) have been passed to routine
      if(ityp.ne.1.and.ityp.ne.2)then
         write(io6,*)'ERROR in SUBR. OUTSAM -- invalid value of ITYP',
     1   ' -- ITYP = ',ityp
         write(*,*)
         stop 'Halted in OUTSAM -- see list file.'
      endif
      if(nsigd.lt.1.or.nsigd.gt.5)then
         write(io6,*)'ERROR in SUBR. OUTSAM -- invalid value of NSIGD',
     1   ' -- NSIGD = ',nsigd
         write(*,*)
         stop 'Halted in OUTSAM -- see list file.'
      endif
c
      icr=2
      if(nsigd.eq.1)icr=1
      if(mod(nx,icol(nsigd)).eq.0)then
         npass=nx/icol(nsigd)
      else
         npass=nx/icol(nsigd)+1
      endif
c
c --- real array -- find min. & max. values
c --- stitch together receptor decomposition for list file output
c       
        myendpts(1) = mysamb
        myendpts(2) = mysamt
c
          call MPI_ALLGATHER(myendpts,2,
     1                        MPI_INT,allendpts,
     2                        2,MPI_INT,MPI_COMM_WORLD,ierr)

c --- each process calculates all matrix offsets 
c
        cnt=1
        do i=1, mpinumtasks*2,2
          alloffset(cnt) = (allendpts(i)-1)*mxnxg
          cnt = cnt +1
        enddo
c        
        cnt=1
        do i=1, mpinumtasks*2,2
          recvcnts(cnt)=((allendpts(i+1)-allendpts(i))+1)*mxnxg
          cnt=cnt+1
        enddo
c
       asize = recvcnts(mpirank+1)
c
      call MPI_ALLGATHERV(rarray(1:mxnxg,mysamb:mysamt),asize,MPI_REAL,
     1                    mpirarray,recvcnts,alloffset,MPI_REAL,
     2                    MPI_COMM_WORLD,ierr)
c        
      if(ityp.ne.1)go to 50
      xmax=-1.e-25
      xmin=1.e25
      do 10 i=1,nx
      do 10 j=1,ny
      if(mpirarray(i,j).gt.xmax)xmax=mpirarray(i,j)
      if(mpirarray(i,j).lt.xmin)xmin=mpirarray(i,j)
10    continue
      if(xmin.ne.0.0.or.xmax.ne.0.0)go to 12
      if(ldate)write(io6,94)messag,nyr,nmo,nday,njul,nhr
      if(.not.ldate)write(io6,95)messag
      write(io6,11)
11    format(1x,'GRID NOT PRINTED -- all values zero')
      return
c
12    continue
      xexp=xmax
      if(abs(xmin).gt.xmax)xexp=abs(xmin)
      iexp=alog10(xexp)
      if(xexp.lt.1.0)iexp=iexp-1
      nexp=iexp-(nsigd-icr)
      xscale=10.**(-nexp)
c
      ic2=0
      do 30 ipass=1,npass
c
      if(ldate)write(io6,94)messag,nyr,nmo,nday,njul,nhr
94    format(/1x,a70,2x,'year: ',i4,2x,'month: ',i2,2x,'day: ',i2,2x,
     1 'Julian day: ',i3,2x,'hour(0-23): ',i2/)
      if(.not.ldate)write(io6,95)messag
95    format(/1x,a70/)
      write(io6,109)nexp
109   format(1x,'Multiply all values by 10 ** ',i3/)
c
      ic1=ic2+1
      ic2=ic2+icol(nsigd)
      if(ic2.gt.nx)ic2=nx
c
         do 20 jj=ny,1,-1
         icnt=0
c
            do 18 i=ic1,ic2
            icnt=icnt+1
            if(mpirarray(i,jj).lt.0)then
               iout(icnt)=-(mpirarray(i,jj)*xscale-0.5)
               sign(icnt)=minus
            else
               iout(icnt)=mpirarray(i,jj)*xscale+0.5
               sign(icnt)=plus
            endif
18          continue
            call wrt(form1(nsigd),form2(nsigd),jj,iout,sign,icnt,io6)
20       continue
      nund=(nsigd+1)*icnt-1
      if(nsigd.eq.1)nund=(nsigd+2)*icnt-1
      write(io6,101)(minus,n=1,nund)
101   format(5x,128a1)
      call wrt2(form3(nsigd),ic1,ic2,io6)
30    continue
      return
c
c --- integer array -- find min. & max. values
50    continue
c --- stitch together receptor decomposition for list file output
c       
        myendpts(1) = mysamb
        myendpts(2) = mysamt
c
      call MPI_ALLGATHER(myendpts,2,MPI_INT,allendpts,2,MPI_INT,
     1                   MPI_COMM_WORLD,ierr)
c
c --- each process calculates all matrix offsets 
c
        cnt=1
        do i=1, mpinumtasks*2,2
          alloffset(cnt) = (allendpts(i)-1)*mxnxg
          cnt = cnt +1
        enddo
c        
        cnt=1
        do i=1, mpinumtasks*2,2
          recvcnts(cnt)=((allendpts(i+1)-allendpts(i))+1)*mxnxg
          cnt=cnt+1
        enddo
c
       asize = recvcnts(mpirank+1)
c
      call MPI_ALLGATHERV(iarray(1:mxnxg,mysamb:mysamt),asize,MPI_INT,
     1                    mpiiarray,recvcnts,alloffset,MPI_INT,
     2                    MPI_COMM_WORLD,ierr)
c        
      kmax=-9999999
      kmin=9999999
      do 110 i=1,nx
      do 110 j=1,ny
      if(mpiiarray(i,j).gt.kmax)kmax=mpiiarray(i,j)
      if(mpiiarray(i,j).lt.kmin)kmin=mpiiarray(i,j)
110   continue
      if(kmin.ne.0.or.kmax.ne.0)go to 102
      if(ldate)write(io6,94)messag,nyr,nmo,nday,njul,nhr
      if(.not.ldate)write(io6,95)messag
      write(io6,11)
      return
c
102   continue
      xexp=kmax
      if(iabs(kmin).gt.kmax)xexp=iabs(kmin)
      iexp=alog10(xexp)
      if(xexp.lt.1.0)iexp=iexp-1
      nexp=iexp-(nsigd-icr)
      xscale=10.**(-nexp)
c
      ic2=0
      do 130 ipass=1,npass
c
      if(ldate)write(io6,94)messag,nyr,nmo,nday,njul,nhr
      if(.not.ldate)write(io6,95)messag
      write(io6,109)nexp
c
      ic1=ic2+1
      ic2=ic2+icol(nsigd)
      if(ic2.gt.nx)ic2=nx
c
         do 120 jj=ny,1,-1
         icnt=0
c
            do 118 i=ic1,ic2
            icnt=icnt+1
            if(mpiiarray(i,jj).lt.0)then
               iout(icnt)=-(mpiiarray(i,jj)*xscale-0.5)
               sign(icnt)=minus
            else
               iout(icnt)=mpiiarray(i,jj)*xscale+0.5
               sign(icnt)=plus
            endif
118         continue
         call wrt(form1(nsigd),form2(nsigd),jj,iout,sign,icnt,io6)
120      continue
      nund=(nsigd+1)*icnt-1
      if(nsigd.eq.1)nund=(nsigd+2)*icnt-1
      write(io6,101)(minus,n=1,nund)
      call wrt2(form3(nsigd),ic1,ic2,io6)
130   continue
c
      return
      end
