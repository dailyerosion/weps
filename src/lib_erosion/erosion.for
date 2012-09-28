!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine erosion (min_erosion_awu, icsr)

!     +++ PURPOSE +++
!     subroutine erosion is the control subroutine and calls other
!     subroutines in the EROSION submodel.
!     overall the EROSION submodel:
!     - calculates ridge/dike spacing parallel the wind 
!     - determines if friction velocity exceeds threshold.
!     - calculates soil loss/deposition, suspension, PM-10 on grid
!     - updates soil  variables changed by erosion.

! ****See user modifications at start of code to enable test
!      output subroutines sb1out and sb2out

!     +++ ARGUMENT DECLARATIONS +++

      real min_erosion_awu       !Minimum erosive wind speed (m/s)
                                 !to evaluate for erosion loss

!     +++ ARGUMENT DEFINITIONS +++
! added icsr for subregion by JG 
      integer icsr    
!     +++ PARAMETER +++

      real SNODEP                !Minimum snow depth to prevent erosion
      parameter (SNODEP = 20.0)  !No erosion when snow depth >= 20mm
      real  PID180
      parameter(PID180 = 3.14159/180.)

!     + + + GLOBAL COMMON BLOCKS + + +
      include  'p1werm.inc'
      include  'c1gen.inc'
      include  'm1subr.inc'
      include  'p1const.inc'
      include  'b1glob.inc'
      include  'c1glob.inc'
      include  'd1glob.inc'
!      include  'm1geo.inc'
      include  'w1wind.inc'
      include  's1dbh.inc'
      include  's1phys.inc'
      include  's1agg.inc'
      include  's1surf.inc'
      include  's1sgeo.inc'
      include  'h1db1.inc'
      include  'm1flag.inc'
      include  'm1sim.inc'
!      include  'wpath.inc'
      include  'file.inc'
      include  'timer.inc'
      include  'command.inc'
      include  'main/main.inc'

!     + + + LOCAL COMMON BLOCKS + + +
      include 'erosion/p1erode.inc'  !Needs the SURF_UPD_FLG variable
!      include 'erosion/e2grid.inc'
!      include 'erosion/e3grid.inc'
!      include 'erosion/m2geo.inc'
      include 'erosion/s2sgeo.inc'
      include 'erosion/s2agg.inc'
      include 'erosion/s2surf.inc'
      include 'erosion/threshold.inc'


!     +++ LOCAL VARIABLES +++
      integer i,j,wustfl, outfl
      integer nhill, n
      integer day, mon, yr, hidx
      real wuref, rusust, rut
!      real rusust_preros(ntstep)
      real wzorg, wzorr, wzzo, wzzov
      real wus, wust, wusp, brcd, wusto
      real wr, sfd84(mnsub), time
      real sina, prev_dir
      real hr, sub_ntstep, hrs
      real wuse, wuste, enge
      real wus_anemom, wus_random, wus_ridge, wus_biodrag
      real wubsts, wucsts, wucwts, wucdts, sfcv

!     + + + LOCAL VARIABLE DEFINITIONS + + +
!     i,j      - index
!     wustfl   - flag to update threshold friction velocity
!     icsr     - index of current subregion (now only one)
!     outfl    - flag to call sb1out & sb2out for subdaily info
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
!     sfd84(mnsub)- soil fraction with diameter < 0.84 mm
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

!     +++ SUBROUTINES CALLED +++
!     sbzo
!     sbwus
!     sbwust
!     sbinit
!     sbdirini
!     sbigrd
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

!     code to output standalone erosion input file on specified date
!     check for day of simulation for which you want a file created
      if( saeinp_daysim.gt.0 ) then
          if ((am0jd-ijday+1).eq.saeinp_daysim) then
              call caldat (am0jd,day,mon,yr)
              write(*,*) 'Stand alone erosion input file created D/M/Y',&
     &                day,'/',mon,'/',yr,'simulation day',saeinp_daysim
              call saeinp    ! output daily erosion stuff
          end if
      else if( saeinp_jday.gt.0 ) then
          if ((am0jd).eq.saeinp_jday) then
              call caldat (am0jd,day,mon,yr)
              write(*,*) 'Stand alone erosion input file created D/M/Y',&
     &                day,'/',mon,'/',yr,'simulation day',am0jd-ijday+1
              call saeinp    ! output daily erosion stuff
          end if
      end if
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

! ****Important notes to users of this submodel
! ****flag to enable subdaily output for validation
!      turn off, i.e. set = 0 in WEPS
      outfl = 0

      ! set ratio: defined as the ratio wus/wust
      rusust = 0

!*****this check cannot be done if subhourly wind is used  FAF
! Remove the do-loop from erosion by JG
   !   do 20 icsr=1, nsubr
       ! If snow depth > 20 mm in all subregions, then no erosion
       if (ahzsnd(icsr) .le. SNODEP) then
        ! Have insufficient snow depth
        ne_snowdepth(icsr) = 0

        ! calc if daily max friction vel. exceeds threshold in any 
        ! subregions without hill and barrier effects
        ! calc. ridge spacing parallel the wind
        if (aszrgh(icsr) > 5.0) then
          sina = abs(sin(PID180*abs(awdir(icsr) - asargo(icsr))))
          sina = max(0.10, sina)
          sxprg(icsr) = asxrgs(icsr)/sina
            if (asxdks(icsr) > asxrgs(icsr)/3.) then
             sxprg(icsr) = amin1(sxprg(icsr), asxdks(icsr))
             endif
        else
	  sxprg(icsr) = 1000
        endif
      !compute Zo (wzzo) of surface
        call sbzo                                                       &
     &   (sxprg(icsr), aszrgh(icsr), aslrr(icsr),                       &
     &    wzoflg, adrlaitot(icsr), adrsaitot(icsr), abzht(icsr),        &
     &    acrlai(icsr), acrsai(icsr), aczht(icsr),                      &
     &    acxrow(icsr), ac0rg(icsr), wzorg, wzorr,                      &
     &    wzzo, wzzov, awzzo, brcd)

        ! Calculate soil clod fraction less than 0.84 mm diameter
        ! calc soil mass < 0.84 mm
        call sbsfdi( aslagm(1,icsr), as0ags(1,icsr), aslagn(1,icsr),    &
     &       aslagx(1,icsr), 0.84, sfd84(icsr) )

        ! save the initial sf84 value
        sf84ic = sfd84(icsr)
        sf84ic = min (0.9999, max(sf84ic,0.0001))    ! edit ljh 1-23-05
     
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
          dmlos(1,1) = 0.0
          smaglos(1,1) = 0.0
          smaglosmx(1,1)= 0.0
          call sbwust( sfd84(icsr), asdagd(1,icsr), asfcr(icsr),        &
     &                 asvroc(1,icsr), asflos(icsr),abffcv(icsr), wzzo, &
     &                 ahrwc0(hidx,icsr), ahrwcw(1,icsr), wus, sf84ic,  &
     &                 asvroc(1,icsr), dmlos(1,1), wust, wusp,          &
     &                 wusto, sf84mn(1,1), smaglos(1,1),smaglosmx(1,1), &
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
             ne_wus_anemom(icsr) = wus_anemom
             ne_wus_random(icsr) = wus_random
             ne_wus_ridge(icsr) = wus_ridge
             ne_wus_biodrag(icsr) = wus_biodrag
             ne_wus(icsr) = wus
             ne_bare(icsr) = wubsts
             ne_flat_cov(icsr) = wucsts
             ne_surf_wet(icsr) = wucwts
             ne_ag_den(icsr) = wucdts
             ne_wust(icsr) = wust
             ne_sfd84(icsr) = sfd84(icsr)
             ne_asvroc(icsr) = asvroc(1,icsr)
             ne_wzzo(icsr) = wzzo
             ne_sfcv(icsr) = sfcv
           end if
        end do
       else
        ! snow prevented erosion in this subregion
        ne_snowdepth(icsr) = 1
        ne_wus_anemom(icsr) = 0
        ne_wus_random(icsr) = 0
        ne_wus_ridge(icsr) = 0
        ne_wus_biodrag(icsr) = 0
        ne_wus(icsr) = 0
        ne_bare(icsr) = 1
        ne_flat_cov(icsr) = 1
        ne_surf_wet(icsr) = 1
        ne_ag_den(icsr) = 1
        ne_wust(icsr) = 1
        ne_sfd84(icsr) = 0
        ne_asvroc(icsr) = 0
        ne_wzzo(icsr) = 0
        ne_sfcv(icsr) = 0
       endif
 !  20 continue
    ! remove the do-loop by JG

!     Some placeholder code for hills
!     (it adjusts the threshold ratio cutoff for erosion)
!     following st. is tmp. until sbhill is available
!      nhill = 0

!     if (nhill .eq. 0) then
        wr = 1.0
!      else
!        wr = 0.7
!      endif
      
      ! Check wind ration
      if (rusust .le. wr) then
          ! no erosion, set plot.out flag
          ne_erosion = 0
          ! exit out of erosion submodel
          go to 100
      endif

      ! entering erosion submodel, set plot.out flag
      ne_erosion = 1

!     sbinit calls sbsdfi to get sf< 0.01,0.1,0.84,2.0 mm
!     and writes to grid, writes other var. to grid and
!     zeros eros output arrays.
      call sbinit(icsr)
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
            call sbwind (wustfl, awu(i), awdir(i), ntstep, i, rusust)
            wuref = awu(i)
            wr = 1

            ! stop sbwind timer and start general timer
            call timer(TIMSBWIND,TIMSTOP)
            call timer(TIMEROS,TIMSTART)

            if (rusust .gt. wr) then
               ! erosion will occur this time step
               ! wustfl = 1
               if (btest(am0efl,3)) then
                  call sb1out (j, n, hrs, awu(i), awdir(i), luo_sgrd)
               endif

               ! stop gneral timer and start sberod timer
               call timer(TIMEROS,TIMSTOP)
               call timer(TIMSBEROD,TIMSTART)

               call sberod (time,SURF_UPD_FLG, icsr)

               call timer(TIMSBEROD,TIMSTOP)
               call timer(TIMEROS,TIMSTART)

               hrs = hrs + sub_ntstep !Compute end-of-period time in fraction of hours

               if (btest(am0efl,3)) then
                  call sb2out (j, n, hrs, awu(i), awdir(i), luo_sgrd)
               endif

               j = j + 1
            else
               ! print out initial state, even if we never call sberode()
               if (btest(am0efl,3).and.(j .eq. 1).and.(i .eq. 1)) then
                  call sbwind (wustfl,awu(i),awdir(i),ntstep,i,rusust)
                  wuref = awu(i)
                  call sb1out (j, n, hrs, awu(i), awdir(i), luo_sgrd)
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
            call sbemit (luo_emit, awu(i), hr,icsr)  !Should only write data here
         endif

   41 continue

!     Average surface conditions and update
!     Purpose: update global variables changed by erosion at end of day
!     not implemented partly because windgen does not correlate days.

  100 continue

      call timer(TIMEROS,TIMSTOP)

      return
      end

