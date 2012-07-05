!$Author$
!$Date$
!$Revision$
!$HeadURL$
!**********************************************************************
!     subroutine sbemit
!**********************************************************************
      subroutine sbemit (ounit, ws, hhr)

!     To calc the emissions for each time step of the input wind speed
!     The emissions for EPA are the suspension component
!      with units kg m-2 s-1.
!     To write out a file in the format:
!      12 blank col, yr, mo, day, hr, soucename, emissionrate
!
!     Instructions & logic:
!     To get ntstep period emissions output on erosion days:
!       user sets am0efl = 3 in WEPS configuration screen
!          subroutine openfils creates output file emit.out
!          EROSION calls sbemit to write heading in emit.out file,
!          & sets am0efl to 98, then calls sbemit
!          to print (hourly) Weps emissions on erosion days.
!       or
!       user sets ae0efl (print flg)=4 in stand_alone input file
!           EROSION opens emit.out file, calls sbemit to write headings
!           & sets  ae0efl to 99, then calls sbemit
!            to print period emissions for an erosion day.
!
!
!     +++ ARGUMENT DECLARATIONS +++
      integer        ounit   !Unit number for detail grid erosion
      real           ws, hhr
!     +++ ARGUMENT DEFINITIONS +++
!
!
!     + + + GLOBAL COMMON BLOCKS + + +
!
      include 'p1werm.inc'
      include 'm1sim.inc'
      include 'm1flag.inc'
      include 'file.inc'
!
!     + + + LOCAL COMMON BLOCKS + + +
      include  'erosion/m2geo.inc'
      include  'erosion/e2erod.inc'
!
!
!     +++ PARAMETERS +++
!
!     +++ LOCAL VARIABLES +++
      integer        initflg
      save           initflg
      integer        prev_erosion_jday
      save           prev_erosion_jday

      integer j,i
      integer yr, mo, da
      save    yr, mo, da
      real    tims, aegtp, aegtssp, aegt10p
      save    tims, aegtp, aegtssp, aegt10p
 !     real    hr
 !     save    hr
      real    aegt, aegtss, aegt10
      real    emittot, emitss, emit10, tt

      integer :: dt(8)
      character(len=3) :: mstring
      common / datetime / dt, mstring

!
!     +++ LOCAL VARIABLE DEFINITIONS +++
!
!
!     +++ OUTPUT FORMATS +++
!
  100 format (1x,'  yr  mo  day     hr  ws  emission (kg m-2 s-1)')
  110 format (22x,'        total    salt/creep    susp      PM10')
  120 format (1x,3(i4),F7.3,F6.2, 1x,4(F11.8))
!
!     +++ END SPECIFICATIONS +++
!
!     set initial conditions

      if (initflg .eq. 0) then
          initflg = initflg + 1

          prev_erosion_jday = am0jd - 1  !init to previous day

       !  aegtp   = 0.0
       !  aegtssp = 0.0
       !  aegt10p = 0.0
          tims = 3600*24/ntstep !seconds in each emission period
          call caldatw (da, mo, yr) !Set day, month and year
      write(0,*) 'First ntstep is: ', ntstep, tims, tims/3600
!          hr = 0

          write (ounit,*) 'SBEMIT output'
!          write (ounit,*) 'Suspended emissions < 0.10 mm dia.'
          write (ounit,*)

          ! Print date of Run
212       format(1x,'Date of run: ',a3,' ',i2.2,', ',i4,' ',            &
     &          i2.2,':',i2.2,':',i2.2)
          write (ounit,212) mstring, dt(3), dt(1), dt(5), dt(6), dt(7) 
          write (ounit,*)

          write (ounit,100)
          write (ounit,110) 
          write (ounit,*)
      endif
    ! else

         !init prev erosion hr values to zero if this is new erosion day
      if (prev_erosion_jday .ne. am0jd) then
          prev_erosion_jday = am0jd 
          aegtp   = 0.0
          aegtssp = 0.0
          aegt10p = 0.0
      endif   

      write(0,*) 'Subsequent ntstep is: ', ntstep, tims, tims/3600
!          if (hr .ge. 24) then
!             hr = tims/3600
             call caldatw (da, mo, yr)
!          else
!             hr = hr + tims/3600
!          endif
!     calculate averages of inner grid points
             aegt   = 0.0
             aegtss = 0.0
             aegt10 = 0.0
!
         do  j=1,jmax-1
           do  i= 1, imax-1
             aegt= aegt + egt(i,j)
             aegtss = aegtss + egtss(i,j)
             aegt10 = aegt10 + egt10(i,j)
           enddo
         enddo
             tt     = (imax-1)*(jmax-1)
             aegt   = - aegt/tt     ! change signs to positive=emission
             aegtss = - aegtss/tt
             aegt10 = - aegt10/tt

!  Commented out so that emission results don't get summed from hr to hr
!  when erosion stops.  - LEW 5/26/06
!     Hourly (or peroid) emission rate (kg m-2 s-1)
   !      if (aegtssp .gt. aegtss) then  !main set egt arrays to zero
   !          aegtp   = 0.0
   !          aegtssp = 0.0
   !          aegt10p = 0.0
   !      endif
!
         emittot = (aegt - aegtp)/tims
         emitss  = (aegtss - aegtssp)/tims
         emit10  = (aegt10 - aegt10p)/tims
!
!     Save prior hour average emission
         aegtp   =  aegt
         aegtssp =  aegtss
         aegt10p =  aegt10
!
!     Write to emit.out file
         write (ounit,120) yr, mo, da, hhr, ws,                         &
     &                     emittot, emittot-emitss, emitss, emit10
!
   !   endif
      return
      end





