c----------------------------------------------------------------------
      subroutine mpidecomp1D
c----------------------------------------------------------------------
c
c --- PARALLEL CALPUFF(V5.8)                                MPIDECOMP1D
c     written by: D.J. Rasmussen (July 2012)
c                 Department of Civil and Environmental Engineering
c                 University of California, Davis
c
c --- PURPOSE:  Decompose gridded receptors, non-gridded discrete  
c               receptors, non-gridded discrete complex terrain
c               receptors 1-dimensionally
c
c --- INPUTS:
c     Use /MPIDECOMP/ variables:
c        MYSAMB, MYSAMT, MPIFIRSTREC, MPILASTREC,
c        MPIFIRSTCTREC, MPILASTCTREC
c     Use /MPIRANKTASKS/ variables:
c       MPIRANK, MPINUMTASKS        
c
c --- OUTPUT:  none
c
c --- MPIDECOMP1D called by: SETUP
c --- MPIDECOMP1D calls:     MPE_DECOMP1D
c----------------------------------------------------------------------
c
       use mpidecomp
       use mpiranktasks
c
c --- Include parameters
       include 'params.puf'
c
c --- Include common blocks
       include 'nongrd.puf'
       include 'ctsgdat.puf'
       include 'grid.puf'
c
       integer excess, maxid
c
c --- 1D domain decomposition for gridded sampling receptors
c
c     This MPI process exits if the number of y- receptor elements
c     is less than the number of running MPI tasks
c 
      if (mpinumtasks.GT.mxnyg) then
        excess = mpinumtasks - mxnyg
        maxid = (mpinumtasks-1)-excess
             write(io6,*), 'NOTE: **** mpinumtasks.GT.mxnyg ****'
        if (mpirank.GT.maxid) stop
      endif 
c
      call MPE_DECOMP1D(nysam,mpinumtasks,mpirank,
     1                   mysamb,mysamt)
c
c --- 1D domain decomposition for non-gridded discrete receptors
c
      if(nrec.GT.0) then
        if (mpinumtasks.GT.nrec) then
          excess = mpinumtasks - nrec
          maxid = (mpinumtasks-1) - excess
            if (mpirank.GT.maxid) then
              mpifirstrec = 0
              mpilastrec = 0
            else
        call MPE_DECOMP1D(nrec,mpinumtasks,mpirank,
     1                    mpifirstrec,mpilastrec)
            endif ! mpirank.GT.maxid
        else
        call MPE_DECOMP1D(nrec,mpinumtasks,mpirank,
     1                    mpifirstrec,mpilastrec)
        endif ! mpinumtasks.GT.nrec
      endif ! nrec.GT.0
c
c --- 1D domain decomposition for non-gridded discrete CTSG receptors
c
      if(nctrec.GT.0) then
        if (mpinumtasks.GT.nctrec) then
          excess = mpinumtasks - nctrec
          maxid = (mpinumtasks-1) - excess
            if (mpirank.GT.maxid) then
              mpifirstctrec = 0
              mpilastctrec = 0
            else
        call MPE_DECOMP1D(nrec,mpinumtasks,mpirank,
     1                    mpifirstctrec,mpilastctrec)
            endif ! mpirank.GT.maxid
        else
        call MPE_DECOMP1D(nrec,mpinumtasks,mpirank,
     1                    mpifirstctrec,mpilastctrec)
        endif ! mpinumtasks.GT.nctrec
      endif ! nctrec.GT.0
c
       return 
       end
!**********************************************************************
      subroutine MPE_DECOMP1D( n, numprocs, myid, s, e )
!**********************************************************************
! Written by: DJ Rasmussen 
! UCalifornia Davis (April 2012) 
! Reference: Gropp, Lusk, and Skjellum (1999) MIT Press
!
! Purpose: this file contains a routine for producing a decomposition
! of a 1D array when given a number of processors.
!
! Input:
!   n - integer - the length of the array to be decomposed
!   numprocs - integer - the number of MPI processes
!   myid - integer - the rank ID of the current process
!    
! Output: 
!   s - the start of the decomposition
!   e - the end of the decomposition
! 
! Other variables:
!   nlocal - integer - a local counter to go through n 
!   deficit - integer - the remaing elements after the
!                       n has been divided up among
!                       MPI proceses
!**********************************************************************

      integer n, numprocs, myid, s, e
      integer nlocal, excess, maxid
      integer deficit

      nlocal  = n / numprocs

      s = myid * nlocal + 1
      deficit = mod(n,numprocs)
      s = s + min(myid,deficit)

      if (myid .lt. deficit) then
        nlocal = nlocal + 1
      endif

      e = s + nlocal - 1
      if (e .gt. n .or. myid .eq. numprocs-1) e = n

      return

      end
