!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine erosion( min_erosion_awu, SURF_UPD_FLG, subrsurf, noerod, cellstate )

!     +++ PURPOSE +++
!     subroutine erosion is the control subroutine and calls other
!     subroutines in the EROSION submodel.
!     overall the EROSION submodel:
!     - calculates ridge/dike spacing parallel the wind 
!     - determines if friction velocity exceeds threshold.
!     - calculates soil loss/deposition, suspension, PM-10 on grid
!     - updates soil  variables changed by erosion.

      use weps_interface_defs
      use file_io_mod, only: luo_sgrd, luo_emit
      use erosion_data_struct_defs
      use saeinp_mod, only: mksaeinp, saeinp

!     +++ ARGUMENT DECLARATIONS +++
      real min_erosion_awu       !Minimum erosive wind speed (m/s) to evaluate for erosion loss
      integer :: SURF_UPD_FLG    ! erosion surface updating (0 - disabled, 1 - enabled)
      type(subregionsurfacestate), dimension(:) :: subrsurf  ! subregion surface conditions (erosion specific set)
      type(threshold), dimension(:), intent(out) :: noerod                 ! report values to show which factors prevented erosion
      type(cellsurfacestate), dimension(0:,0:), intent(out) :: cellstate     ! initialized grid cell state values

!     +++ PARAMETER +++
      real SNODEP                !Minimum snow depth to prevent erosion
      parameter (SNODEP = 20.0)  !No erosion when snow depth >= 20mm
      real  PID180
      parameter(PID180 = 3.14159/180.)

!     + + + GLOBAL COMMON BLOCKS + + +
      include  'p1werm.inc'
      include  'm1subr.inc'  ! nsubr
      include  'p1const.inc' ! SEC_PER_DAY, anemht, awzzo, wzoflg
      include  'w1wind.inc'  ! awadir, awudmx, awdir, awu
      include  'm1flag.inc'  ! am0efl
      include  'm1sim.inc'   ! erod_interval, ntstep
      include  'timer.inc'   ! TIMEROS, TIMSBEROD, TIMSBWIND, TIMSTART, TIMSTOP

!     +++ LOCAL VARIABLES +++
      integer i,j,wustfl, icsr
!      integer nhill
      integer n
      integer hidx
      real wuref, rusust, rut
!      real rusust_preros(ntstep)
      real wzorg, wzorr, wzzo, wzzov
      real wus, wust, wusp, brcd, wusto  ! these are just used to screen subregions for erosion. Global cellstate values are computed elsewhere.
      real wr, time
      real sina, prev_dir
      real hr, sub_ntstep, hrs
      real wuse, wuste, enge
      real wus_anemom, wus_random, wus_ridge, wus_biodrag
      real wubsts, wucsts, wucwts, wucdts, sfcv
      real :: dmlos_tmp, sf84mn_tmp, smaglos_tmp, smaglosmx_tmp

!     + + + LOCAL VARIABLE DEFINITIONS + + +
!     i,j      - index
!     wustfl   - flag to update threshold friction velocity
!     icsr     - index of current subregion (now only one)
!     nhill    - number of hills
!     hidx     - hour index (for referencing surface water content)
!     wuref    - reference wind speed (m/s) 
!     rusust   - ratio of friction vel. to threshold friction vel.
!     rut      - ratio for this timestep
!     rusust_preros - ratio of friction vel. to threshold friction vel.
!                     by timestep (max all subregions) before erosion starts
!     wzorg  - aerodynamic roughness of ridge (mm)
!     wzorr  - aerodynamic roughness of random roughness (mm)
!     wzzo     - soil surface aerodynamic roughness (mm)
!     wzzov    - above canopy aerodynamic roughness (mm)
!     wus      - friction velocity (m/s)
!     wust     - threshold friction velocity (m/s)
!     wusp     - threshold friction velocity for trapping (m/s)
!     wr       -  ratio to be exceeded fric. vel. to thresh. fric. vel.
!                 for erosion
!     sina     - sin of the acute angle between ridge and wind angles
!     prev_dir - prior direction
!     rut      - updated ratio of rusust
!     time     - time (s)
!     wuse     - est. of max. wus on grid at ntstep
!     wuste    - est. of min wust on grid at ntstep 
!     enge     - est. of relative max erosive engergy at ntstep
!     wus_anemom - Friction velocity at the anemometer location
!     wus_random - Friction velocity adjusted for site random roughness only
!     wus_ridge - Friction velocity adjusted for site ridge roughness only
!     wus_biodrag - Friction velocity adjusted for site biodrag only
!     wubsts - bare soil threshold friction velocity
!     wucsts - surface cover addition to bare soil threshold friction velocity
!     wucwts - surface wetness addition to bare soil threshold friction velocity
!     wucdts - aggregate density addition to bare soil threshold friction velocity
!     cellstate - structure containing the cell state variables

!     +++ SUBROUTINES CALLED +++
!     sbzo
!     sbwus
!     sbwust
!     sbinit
!     sbdirini
!     sbwind
!     sberod
!     sbsfdi

!     +++ FUNCTION DECLARATIONS +++

!     +++ END SPECIFICATIONS +++

!     start general erosion timer
      call timer(TIMEROS,TIMSTART)

      hr = 0.0
      sub_ntstep = 0.0
      hrs = 0.0

   ! We need to ensure that sbemit only gets called once here to write the header
   ! multiple days in WEPS will mess this up.
  !    if (btest(am0efl,2)) then
  !       write(0,*) 'i is:', i, 'hr is:',hr,'should be header only here'
  !       call sbemit (luo_emit, awu(i), hr)  !Should only write the header one time
  !    endif

!     initialize wind direction array for subhourly values
!     presently, there is only one direction for each day
!     erosion stand alone may want to input wind speed
!     and direction as subhourly pairs. If so, this can be disabled.
      do i =1, ntstep
          awdir(i) = awadir
!          rusust_preros(i) = 0.0
      end do

      ! set ratio: defined as the ratio wus/wust
      rusust = 0

!*****this check cannot be done if subhourly wind is used  FAF
! for subhourly, surface water content values should be interpolated. see hidx calculation
      do 20 icsr=1, nsubr
       ! If snow depth > 20 mm in all subregions, then no erosion
       if (subrsurf(icsr)%ahzsnd .le. SNODEP) then
        ! Have insufficient snow depth
        noerod(icsr)%snowdepth = 0

        ! calc if daily max friction vel. exceeds threshold in any 
        ! subregions without hill and barrier effects

        ! calc. ridge spacing parallel the wind
        if (subrsurf(icsr)%aszrgh > 5.0) then
          sina = abs(sin(PID180*abs(awdir(icsr) - subrsurf(icsr)%asargo)))
          sina = max(0.10, sina)
          subrsurf(icsr)%sxprg = subrsurf(icsr)%asxrgs/sina
            if (subrsurf(icsr)%asxdks > subrsurf(icsr)%asxrgs/3.) then
             subrsurf(icsr)%sxprg = amin1(subrsurf(icsr)%sxprg, subrsurf(icsr)%asxdks)
            endif
        else
            subrsurf(icsr)%sxprg = 1000
        endif

        ! Compute Zo (wzzo) of surface
        call sbzo( subrsurf(icsr)%sxprg, subrsurf(icsr)%aszrgh, subrsurf(icsr)%aslrr, &
          wzoflg, subrsurf(icsr)%adrlaitot, subrsurf(icsr)%adrsaitot, subrsurf(icsr)%abzht, &
          subrsurf(icsr)%acrlai, subrsurf(icsr)%acrsai, subrsurf(icsr)%aczht, &
          subrsurf(icsr)%acxrow, subrsurf(icsr)%ac0rg, wzorg, wzorr, &
          wzzo, wzzov, awzzo, brcd )

        ! Calculate soil clod fraction less than 0.84 mm diameter
        ! calc soil mass < 0.84 mm
        call sbsfdi( subrsurf(icsr)%bsl(1)%aslagm, subrsurf(icsr)%bsl(1)%as0ags, subrsurf(icsr)%bsl(1)%aslagn, &
             subrsurf(icsr)%bsl(1)%aslagx, 0.84, subrsurf(icsr)%sfd84 )

        ! save the initial sf84 value
        subrsurf(icsr)%sf84ic = subrsurf(icsr)%sfd84
        subrsurf(icsr)%sf84ic = min (0.9999, max(subrsurf(icsr)%sf84ic,0.0001))    ! edit ljh 1-23-05
     
        do i=1, ntstep
          ! find hour index (1-24)
          hidx = int(i*23.75/ntstep) + 1

          ! (comparison) anemometer location surface friction velocity
          call sbwus( anemht, awzzo, awu(i), awzzo, 0.0, wus_anemom )

          ! (comparison) site random roughness surface friction velocity
          call sbwus( anemht, awzzo, awu(i), wzorr, 0.0, wus_random )

          ! (comparison) site ridge (pattern) roughness surface friction velocity
          call sbwus( anemht, awzzo, awu(i), wzorg, 0.0, wus_ridge )

          ! (comparison) site biodrag surface friction velocity
          call sbwus( anemht, awzzo, awu(i), awzzo, brcd, wus_biodrag )

          ! Compute soil surface friction velocity (wus)
          call sbwus( anemht, awzzo, awu(i), wzzov, brcd, wus )

          ! Compute friction velocity threshold for entrainment (wust) and
          ! transport friction velocity threshold (wusp)
          dmlos_tmp = 0.0
          smaglos_tmp = 0.0
          smaglosmx_tmp = 0.0
          call sbwust( subrsurf(icsr)%sfd84, subrsurf(icsr)%bsl(1)%asdagd, subrsurf(icsr)%asfcr, &
     &                 subrsurf(icsr)%bsl(1)%asvroc, subrsurf(icsr)%asflos, subrsurf(icsr)%abffcv, wzzo, &
     &                 subrsurf(icsr)%ahrwc0(hidx), subrsurf(icsr)%bsl(1)%ahrwcw, wus, subrsurf(icsr)%sf84ic,  &
     &                 subrsurf(icsr)%bsl(1)%asvroc, dmlos_tmp, wust, wusp, &
     &                 wusto, sf84mn_tmp, smaglos_tmp, smaglosmx_tmp, &
     &                 wubsts, wucsts, wucwts, wucdts, sfcv)

          ! Checks to find maximum ratio between surface friction velocity
          ! and friction velocity threshold among all time steps and subregion surfaces.
          ! We have erosion if (wus/wust .gt. 1.0) - for flat fields only
!          rusust_preros(i) = max( rusust_preros(i), wus/wust )
!          rusust = max( rusust, rusust_preros(i) )
           if( wus/wust .gt. rusust ) then
             ! set new maximum
             rusust = wus/wust
             ! set reporting values for the new maximum (this is as close to erosion as we will get)
             noerod(icsr)%wus_anemom = wus_anemom
             noerod(icsr)%wus_random = wus_random
             noerod(icsr)%wus_ridge = wus_ridge
             noerod(icsr)%wus_biodrag = wus_biodrag
             noerod(icsr)%wus = wus
             noerod(icsr)%bare = wubsts
             noerod(icsr)%flat_cov = wucsts
             noerod(icsr)%surf_wet = wucwts
             noerod(icsr)%ag_den = wucdts
             noerod(icsr)%wust = wust
             noerod(icsr)%sfd84 = subrsurf(icsr)%sfd84
             noerod(icsr)%asvroc = subrsurf(icsr)%bsl(1)%asvroc
             noerod(icsr)%wzzo = wzzo
             noerod(icsr)%sfcv = sfcv
           end if
        end do

        if( (noerod(icsr)%wus/noerod(icsr)%wust) .le. 1.0 ) then
           ! non-erodable subregion, set plot.out flag
           noerod(icsr)%erosion = 0
        else
           ! erodable subregion, set plot.out flag
           noerod(icsr)%erosion = 1
        endif

       else
        ! snow prevented erosion in this subregion
        noerod(icsr)%erosion = 0
        noerod(icsr)%snowdepth = 1
        noerod(icsr)%wus_anemom = 0
        noerod(icsr)%wus_random = 0
        noerod(icsr)%wus_ridge = 0
        noerod(icsr)%wus_biodrag = 0
        noerod(icsr)%wus = 0
        noerod(icsr)%bare = 1
        noerod(icsr)%flat_cov = 1
        noerod(icsr)%surf_wet = 1
        noerod(icsr)%ag_den = 1
        noerod(icsr)%wust = 1
        noerod(icsr)%sfd84 = 0
        noerod(icsr)%asvroc = 0
        noerod(icsr)%wzzo = 0
        noerod(icsr)%sfcv = 0
       endif
   20 continue

!     Some placeholder code for hills
!     (it adjusts the threshold ratio cutoff for erosion)
!     following st. is tmp. until sbhill is available
!      nhill = 0

!     if (nhill .eq. 0) then
        wr = 1.0
!      else
!        wr = 0.7
!      endif
      
      ! Check wind ratio
      if (rusust .le. wr) then
          ! exit out of erosion submodel
          go to 100
      endif

      ! entering erosion submodel

!     code to output standalone erosion input file on specified date
!     check for day of simulation for which you want a file created
      if( mksaeinp%simday .gt. 0 ) then
          call saeinp( subrsurf )    ! output daily erosion stuff
      end if

!     sbinit calls sbsdfi to get sf< 0.01,0.1,0.84,2.0 mm
!     and writes to grid, writes other var. to grid and
!     zeros eros output arrays.
      call sbinit( subrsurf, cellstate )

!     calc. sweep direction based on wind direction for sberod
      prev_dir = awdir(1)+ 1.0   !make different to force calculation
      call sbdirini( awdir(1), prev_dir )

!     set flag on to update threshold fric. vel. on grid
      wustfl = 1

!     set ref wind speed to daily max
      wuref = awudmx

!     step thru each periodic wind speed
      do 41 i =1, ntstep
          ! check for erodible wind speed
         if (awu(i) .lt. min_erosion_awu) then
             hr = hr + (24.0/ntstep)  !No sub_ntstep's (no erosion calculated for this 'ntstep'
            go to 40
         endif
         ! calc. sweep direction based on wind direction for sberod
         ! only needed if one reads hourly wind directions for input
         ! call sbdirini( awdir(i), prev_dir )

          !rut = rusust_preros(i)  This change sabotaged code logic
          
           rut = rusust*awu(i)/wuref
          !if (wustfl < 1) then
            ! no erosion aka surface updating has occurred yet
            !if( rusust_preros(i) .gt. wr ) then
               ! erosion will occur, updated surface requires full grid calculation
             !  wustfl = 1
            

            if (rut .le. wr) then
               hr = hr + (24.0/ntstep)  !No sub_ntstep's (no erosion calculated for this 'ntstep'
               go to 40    ! skip since we do no erosion (still print sbemit) 
            endif
          !endif

! The following code determines how often the surface updating code
! is executed.  The internal loop interval appears to be determined based
! upon the value of "ntstep".  "ntstep" determines the base updating interval
! value used within WEPS and is currently defaulting to 24 (ie, updating
! once an hour).  The internal loop then can be set to update the surface
! on a smaller interval, depending upon the value of "ntstep" and some
! other variables (probably to reduce runtime).

! "ntstep" is used in the standalone erosion submodel to determine the
! default reporting and updating interval as well as the number of wind
! speed values for the day when a daily Weibull wind speed distribution
! is provided as input.  Currently this value can have a maximum of 96
! in the code).  Thus, we cannot just change the value of "ntstep" as
! an input to the standalone erosion submodel to modify the frequency
! of the surface updating without expanding array dimensions throughout
! entire WEPS code and changing the wind speed input information.

! Therefore, a commandline option has been added to the standalone erosion
! submodel code to allow us to completely overide the current code
! that determines the frequency of surface updating, if desired.  If the
! value of the "surface update interval" variable is set to a value
! greater than 59 and the product of "ntstep" and "erod_interval" is
! evenly divisble into the number of seconds in a day, the surface updating
! will occur at the computed interval of: 

!       update_interval = SEC_PER_DAY/(ntstep * n)   ! (seconds)

! if update interval is 15 minutes (900 seconds):
!       ntstep = 24, then n = 4
!       ntstep = 96, then n = 1
! if update interval is 1 minute:
!       ntstep = 24, then n = 60
!       ntstep = 96, then n = 15
! if update interval is 10 seconds:
!       ntstep = 24, then n = 360
!       ntstep = 96, then n = 90
! if update interval is 6 seconds:
!       ntstep = 24, then n = 600
!       ntstep = 96, then n = 150
! if update interval is 1 second:
!       ntstep = 24, then n = 3600
!       ntstep = 96, then n = 900

! NOTE: "n" is the number of steps within a single "ntstep" to obtain
!       the desired update interval.
 
         if (erod_interval > 0) then   !overide the default update interval
            ! check for even divisibility done in tsterode main program
            n = SEC_PER_DAY/(erod_interval*ntstep)
!            write(6,*) 'erod_interval and n',erod_interval, n
         else  !default surface updating behavior

            ! force calculation to 15 minute time steps:
            ! useful to allow enough updates of surface
            n = max(1,96/ntstep)
            ! modify time step to more or less than 15 minutes
!           if (awu(i) .lt. 15.0) then
!            if (rut < 1.1 ) then
!               n = n - 2
!            elseif (rut > 1.4) then
!               n = n*2
!            endif
!           elseif (rut >1.4) then
!             n = n*4
!           else
!             n = n*2
!           endif
!
!         est. n as fn. of approx max. erosive energy 9-6-06 LH

            wuse = 0.06*awu(i)
            wuste = wuse/rut
            enge  = wuse*wuse*(wuse-wuste)
            n = nint((0.5 + 4.6*enge)*n)

        ! insure at least 1 time step
            n = max(1,n)
         endif

         ! calculate the time step in seconds
         time = SEC_PER_DAY/(n*ntstep)

         sub_ntstep = (24.0/ntstep)/n  !fraction of hr == sub_ntstep

         ! start the inner loop time step
         j = 1
         hrs = hr    !Let hrs be sub_ntstep interval hr and keep current ntstep hr
         do while (j <= n)

            ! prepare to update rusust and wus.
            ! note: when rusust= <0.1, sbaglos does not calculate.
            rusust = 0.2

            ! stop general timer and start sbwind timer
            call timer(TIMEROS,TIMSTOP)
            call timer(TIMSBWIND,TIMSTART)

            ! updates the fric. vel and threshold fric. vel on grid
            ! and calc. max. for  rusust = wus/wust
            ! this subroutine calls sbzo and sbwus
            call sbwind( wustfl, awu(i), ntstep, i, rusust, subrsurf, cellstate )
            wuref = awu(i)
            wr = 1

            ! stop sbwind timer and start general timer
            call timer(TIMSBWIND,TIMSTOP)
            call timer(TIMEROS,TIMSTART)

            if (rusust .gt. wr) then
               ! erosion will occur this time step
               ! wustfl = 1
               if (btest(am0efl,3)) then
                  call sb1out (j, n, hrs, awu(i), awdir(i), luo_sgrd, subrsurf(1), cellstate)
               endif

               ! stop gneral timer and start sberod timer
               call timer(TIMEROS,TIMSTOP)
               call timer(TIMSBEROD,TIMSTART)

               call sberod (time, SURF_UPD_FLG, subrsurf, cellstate)

               call timer(TIMSBEROD,TIMSTOP)
               call timer(TIMEROS,TIMSTART)

               hrs = hrs + sub_ntstep !Compute end-of-period time in fraction of hours

               if (btest(am0efl,3)) then
                  call sb2out (j, n, hrs, luo_sgrd, cellstate)
               endif

               j = j + 1
            else
               ! print out initial state, even if we never call sberode()
               if (btest(am0efl,3).and.(j .eq. 1).and.(i .eq. 1)) then
                  call sbwind( wustfl, awu(i), ntstep, i, rusust, subrsurf, cellstate )
                  wuref = awu(i)
                  call sb1out (j, n, hrs, awu(i), awdir(i), luo_sgrd, subrsurf(1), cellstate)
               endif

               ! set to get out of inner loop and go to next wind speed - wustfl = 0
               j = n + 1
            endif

            ! If we are ready to leave the sub_ntstep loop, update "hr" and go
            if (j .eq. (n+1)) then
               hr = hr + (24.0/ntstep)  ! skip to next "nstep" hr - don't care if all sub_ntsteps don't equal one ntstep
            end if             
         enddo

   40 continue

         if (btest(am0efl,2)) then
            ! write(0,*) 'i is:', i, 'hr is:', hr
            ! Note that we use "hr" not "hrs" here so we report the end of the "ntstep" hr period
            call sbemit (luo_emit, awu(i), hr, cellstate)  !Should only write data here
         endif

   41 continue

!     Average surface conditions and update

!     Purpose: update global variables changed by erosion at end of day
!     not implemented partly because windgen does not correlate days.

  100 continue

      call timer(TIMEROS,TIMSTOP)

      return
      end

