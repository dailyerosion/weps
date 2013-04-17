!$Author$
!$Date$
!$Revision$
!$HeadURL$
!**********************************************************************
!     subroutine sbemit
!**********************************************************************
      subroutine sbemit (ounit, ws, hhr, cellstate, first_emit)

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

      use weps_interface_defs
      use datetime_mod, only: get_systime_string
      use erosion_data_struct_defs, only: cellsurfacestate, ntstep
      use grid_geo_def, only: imax, jmax

!     +++ ARGUMENT DECLARATIONS +++
      integer        ounit   !Unit number for detail grid erosion
      real           ws, hhr
      type(cellsurfacestate), dimension(0:,0:), intent(inout) :: cellstate     ! initialized grid cell state values
      logical, intent(inout) :: first_emit   ! indicates entry of emit from erosion for the first time this day

!     +++ LOCAL VARIABLES +++
      integer        initflg
      save           initflg

      integer j,i
      integer yr, mo, da
      save    yr, mo, da
      real    tims, aegtp, aegtssp, aegt10p
      save    tims, aegtp, aegtssp, aegt10p
 !     real    hr
 !     save    hr
      real    aegt, aegtss, aegt10
      real    emittot, emitss, emit10, tt

!     +++ OUTPUT FORMATS +++
!
  100 format (1x,'  yr  mo  day     hr  ws  emission (kg m-2 s-1)')
  110 format (22x,'        total    salt/creep    susp      PM10')
  120 format (1x,3(i4),F7.3,F6.2, 1x,4(F11.8))

!     +++ END SPECIFICATIONS +++

!     set initial conditions

      if (initflg .eq. 0) then
          initflg = initflg + 1

          tims = 3600*24/ntstep !seconds in each emission period

          call caldatw (da, mo, yr) !Set day, month and year

          write(0,*) 'First ntstep is: ', ntstep, tims, tims/3600

          write (ounit,*) 'SBEMIT output'
!          write (ounit,*) 'Suspended emissions < 0.10 mm dia.'
          write (ounit,*)

          ! Print date of Run
          write(ounit,"(1x,'Date of run: ',a21)") get_systime_string()
          write (ounit,*)

          write (ounit,100)
          write (ounit,110) 
          write (ounit,*)
      endif

      ! init prev erosion hr values to zero if this is new erosion day
      if( first_emit ) then
          first_emit = .false.
          aegtp   = 0.0
          aegtssp = 0.0
          aegt10p = 0.0
      endif   

      write(0,*) 'Subsequent ntstep is: ', ntstep, tims, tims/3600

      call caldatw (da, mo, yr)

      aegt   = 0.0
      aegtss = 0.0
      aegt10 = 0.0

      do  j=1,jmax-1
         do  i= 1, imax-1
            aegt= aegt + cellstate(i,j)%egt
            aegtss = aegtss + cellstate(i,j)%egtss
            aegt10 = aegt10 + cellstate(i,j)%egt10
         enddo
      enddo

      tt     = (imax-1)*(jmax-1)
      aegt   = - aegt/tt     ! change signs to positive=emission
      aegtss = - aegtss/tt
      aegt10 = - aegt10/tt

      emittot = (aegt - aegtp)/tims
      emitss  = (aegtss - aegtssp)/tims
      emit10  = (aegt10 - aegt10p)/tims

!     Save prior hour average emission
      aegtp   =  aegt
      aegtssp =  aegtss
      aegt10p =  aegt10

!     Write to emit.out file
      write (ounit,120) yr, mo, da, hhr, ws,                            &
     &                  emittot, emittot-emitss, emitss, emit10

      return
      end





