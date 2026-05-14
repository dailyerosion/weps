!$Author$
!$Date$
!$Revision$
!$HeadURL$

module erosion_mod

  contains

    subroutine erosion( min_erosion_awu, SURF_UPD_FLG, julday, noerod, cellstate )

!     +++ PURPOSE +++
!     subroutine erosion is the control subroutine and calls other
!     subroutines in the EROSION submodel.
!     overall the EROSION submodel:
!     - calculates ridge/dike spacing parallel the wind 
!     - determines if friction velocity exceeds threshold.
!     - calculates soil loss/deposition, suspension, PM-10 on grid
!     - updates soil  variables changed by erosion.

      use file_io_mod, only: fopenk, makenamnum, luo_erod, luo_egrd, luo_emit, luo_sgrd, makedir
      use erosion_data_struct_defs, only: in_sweep, cellsurfacestate, threshold, &
                                          ntstep, erod_interval, anemht, awzzo, &
                                          wzoflg, awadir, awudmx, subday, am0efl, subrsurf
      use erosion_data_struct_defs, only: initflag
      use sae_in_out_mod, only: mksaeinp, mksaeout, saeinp_forceday, daily_erodout, sb1out, sb2out, sbemit, saeinp, sweepfile
      use p1unconv_mod, only: SEC_PER_DAY, degtorad
      use barriers_mod, only: sbbr
      use grid_mod, only: sbdirini, gridfile
      use wind_mod, only: sbzo, sbwus, biodrag
      use sberod_mod, only: sberod, sbinit, sbwind
      use process_mod, only: sbwust, sbsfdi
      use sberod_mod, only: SNODEP

!     +++ ARGUMENT DECLARATIONS +++
      real min_erosion_awu       ! Minimum erosive wind speed (m/s) to evaluate for erosion loss
      integer :: SURF_UPD_FLG    ! erosion surface updating (0 - disabled, 1 - enabled)
      integer, intent (in) :: julday ! current julian day (index into subrsurf array)
      type(threshold), dimension(:), intent(out) :: noerod                 ! report values to show which factors prevented erosion
      type(cellsurfacestate), dimension(0:,0:), intent(inout) :: cellstate     ! initialized grid cell state values

      ! +++ LOCAL VARIABLES +++
      integer :: nsubr    ! number of subregions
      logical :: first_emit  ! pass to sbemit on first entry to zero out daily accumulators
      integer :: idx      ! time step loop index    
      integer :: ndx      ! sub_ntstep loop index
      integer :: wustfl   ! flag to update threshold friction velocity
      integer :: icsr     ! index of current subregion
      integer :: ipool    ! index of brcdInput pool
!      integer :: nhill    ! number of hills
      integer :: ntsub     ! number of steps within a single "ntstep" to obtain the desired update interval
      integer :: hidx     ! hour index (for referencing surface water content)
      real :: wuref      ! reference wind speed (m/s) 
      real :: rusust_max     ! ratio of friction vel. to threshold friction vel. simulation region maximum
      real :: rusust_sub     ! ratio of friction vel. to threshold friction vel. subregion maximum
      real :: rut        ! ratio of rusust for this timestep
!      real :: rusust_preros ! ratio of friction vel. to threshold friction vel. by timestep (max all subregions) before erosion starts
      real :: wzorg      ! aerodynamic roughness of ridge (mm)
      real :: wzorr      ! aerodynamic roughness of random roughness (mm)
      real :: wzzo       ! soil surface aerodynamic roughness (mm)
      real :: wzzov      ! above canopy aerodynamic roughness (mm)
      real :: wus        ! friction velocity (m/s)
      real :: wust       ! threshold friction velocity (m/s)
      real :: wusp       ! threshold friction velocity for trapping (m/s)
      real :: brcd       ! biomass drag coefficient (or "effective" biomass silhouette area index)
      real :: wusto      ! threshold friction velocity for emission (smooth surface with sf84ic, wus minus flat biomass and wetness effects) (m/s)
      real :: wr         !  ratio to be exceeded fric. vel. to thresh. fric. vel. for erosion
      real :: sina       ! sin of the acute angle between ridge and wind angles
      real :: prev_dir   ! prior direction
      real :: time       ! time (s)
      real :: hr         ! time interval for ntsteps in day (s)
      real :: hrs        ! time interval for sub_ntsteps in one ntstep (s)
      real :: sub_ntstep ! number of time steps in one ntstep
      real :: wuse       ! est. of max. wus on grid at ntstep
      real :: wuste      ! est. of min wust on grid at ntstep 
      real :: enge       ! est. of relative max erosive engergy at ntstep
      real :: wus_anemom ! Friction velocity at the anemometer location
      real :: wus_random ! Friction velocity adjusted for site random roughness only
      real :: wus_ridge  ! Friction velocity adjusted for site ridge roughness only
      real :: wus_biodrag ! Friction velocity adjusted for site biodrag only
      real :: wubsts     ! bare soil threshold friction velocity
      real :: wucsts     ! surface cover addition to bare soil threshold friction velocity
      real :: wucwts     ! surface wetness addition to bare soil threshold friction velocity
      real :: wucdts     ! aggregate density addition to bare soil threshold friction velocity
      real :: sfcv       ! soil fraction clod & crust cover
      character*512 :: daypath  ! the root path plus subdirectory plus specific day directory for sweep output files

      ! +++ END SPECIFICATIONS +++

      first_emit = .true.
      hr = 0.0
      sub_ntstep = 0.0
      hrs = 0.0

      ! We need to ensure that sbemit only gets called once here to write the header
      ! multiple days in WEPS will mess this up.
      ! if (btest(am0efl,2)) then
      !   write(0,*) 'i is:', i, 'hr is:',hr,'should be header only here'
      !   call sbemit (luo_emit, subday(i)%awu, hr)  !Should only write the header one time
      ! endif

      ! initialize wind direction array for subhourly values
      ! presently, there is only one direction for each day
      ! erosion stand alone may want to input wind speed
      ! and direction as subhourly pairs. If so, this can be disabled.
      do idx = 1, ntstep
          subday(idx)%awdir = awadir
         !  rusust_preros(idx) = 0.0
      end do

      ! set ratio: defined as the ratio wus/wust
      rusust_max = 0.0

      ! set number of suberegions
      nsubr = size(noerod)
      
      ! for subhourly, surface water content values should be interpolated. see hidx calculation
      do icsr = 1, nsubr
        ! initialize the subregion ratio
        rusust_sub = 0.0

        ! calc if daily max friction vel. exceeds threshold in any 
        ! subregions without hill and barrier effects

        ! Calculate soil clod fraction less than 0.84 mm diameter
        ! calc soil mass < 0.84 mm
        call sbsfdi( subrsurf(julday,icsr)%bsl(1)%aslagm, subrsurf(julday,icsr)%bsl(1)%as0ags, &
                     subrsurf(julday,icsr)%bsl(1)%aslagn, &
                     subrsurf(julday,icsr)%bsl(1)%aslagx, 0.84, subrsurf(julday,icsr)%sfd84 )

        ! save the initial sf84 value
        subrsurf(julday,icsr)%sf84ic = subrsurf(julday,icsr)%sfd84
        subrsurf(julday,icsr)%sf84ic = min (0.9999, max(subrsurf(julday,icsr)%sf84ic,0.0001))    ! edit ljh 1-23-05
     
        do idx = 1, ntstep

          ! calc. ridge spacing parallel the wind
          if (subrsurf(julday,icsr)%aszrgh > 5.0) then
            sina = abs(sin(dble(degtorad)*abs(subday(idx)%awdir - subrsurf(julday,icsr)%asargo)))
            sina = max(0.10, sina)
            subrsurf(julday,icsr)%sxprg = subrsurf(julday,icsr)%asxrgs/sina
              if (subrsurf(julday,icsr)%asxdks > subrsurf(julday,icsr)%asxrgs/3.) then
                subrsurf(julday,icsr)%sxprg = min(subrsurf(julday,icsr)%sxprg, subrsurf(julday,icsr)%asxdks)
              endif
          else
              subrsurf(julday,icsr)%sxprg = 1000
          endif

          ! accumulate biodrag components
          brcd = 0.0
          do ipool = 1, subrsurf(julday,icsr)%npools
            ! calculate "effective" biomass drag coefficient
            brcd = brcd + biodrag( 0.0, 0.0, subrsurf(julday,icsr)%brcdInput(ipool)%rlai, &
                                   subrsurf(julday,icsr)%brcdInput(ipool)%rsai, &
                                   subrsurf(julday,icsr)%brcdInput(ipool)%rg, subrsurf(julday,icsr)%brcdInput(ipool)%xrow, &
                                   subrsurf(julday,icsr)%brcdInput(ipool)%zht, subrsurf(julday,icsr)%aszrgh )
          end do

          ! Compute Zo (wzzo) of surface
          call sbzo( subrsurf(julday,icsr)%sxprg, subrsurf(julday,icsr)%aszrgh, subrsurf(julday,icsr)%aslrr, &
                     subrsurf(julday,icsr)%abzht, brcd, wzoflg, wzorg, wzorr, wzzo, wzzov, awzzo )

          ! find hour index (1-24)
          hidx = int(idx*23.75/ntstep) + 1

          ! (comparison) anemometer location surface friction velocity
          wus_anemom = sbwus( anemht, awzzo, subday(idx)%awu, awzzo, 0.0 )

          ! (comparison) site random roughness surface friction velocity
          wus_random = sbwus( anemht, awzzo, subday(idx)%awu, wzorr, 0.0 )

          ! (comparison) site ridge (pattern) roughness surface friction velocity
          wus_ridge = sbwus( anemht, awzzo, subday(idx)%awu, wzorg, 0.0 )

          ! (comparison) site biodrag surface friction velocity
          wus_biodrag = sbwus( anemht, awzzo, subday(idx)%awu, awzzo, brcd )

          ! Compute soil surface friction velocity (wus)
          wus = sbwus( anemht, awzzo, subday(idx)%awu, wzzov, brcd )

          ! Compute friction velocity threshold for entrainment (wust) and
          ! transport friction velocity threshold (wusp)
          call sbwust( subrsurf(julday,icsr)%sfd84, subrsurf(julday,icsr)%bsl(1)%asdagd, subrsurf(julday,icsr)%asfcr, &
                       subrsurf(julday,icsr)%bsl(1)%asvroc, subrsurf(julday,icsr)%asflos, subrsurf(julday,icsr)%abffcv, wzzo, &
                       subrsurf(julday,icsr)%ahrwc0(hidx), subrsurf(julday,icsr)%bsl(1)%ahrwcw, subrsurf(julday,icsr)%sf84ic,  &
                       subrsurf(julday,icsr)%bsl(1)%asvroc, wust, wusp, wusto, wubsts, wucsts, wucwts, wucdts, sfcv)

          ! Checks to find maximum ratio between surface friction velocity
          ! and friction velocity threshold among all time steps and subregion surfaces.
          ! We have erosion if (wus/wust .gt. 1.0) - for flat fields only
          !  rusust_preros(i) = max( rusust_preros(i), wus/wust )
          !  rusust = max( rusust, rusust_preros(i) )
          if( wus/wust .gt. rusust_sub ) then
            ! set new maximum
            rusust_sub = wus/wust
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
            noerod(icsr)%sfd84 = subrsurf(julday,icsr)%sfd84
            noerod(icsr)%asvroc = subrsurf(julday,icsr)%bsl(1)%asvroc
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

        ! If snow depth > 20 mm in all subregions, then no erosion
        if (subrsurf(julday,icsr)%ahzsnd .gt. SNODEP) then
          ! snow prevented erosion in this subregion
          noerod(icsr)%snowdepth = 1
          ! set this so all subregions checked for snow depth suppression
          ! see mod to wust below and in sbwind cell by cell for when
          ! only some cells have sufficient depth to supress erosion
          rusust_sub = 0.99
        else
          ! Have insufficient snow depth
          noerod(icsr)%snowdepth = 0
        end if

        ! set global maximum
        rusust_max = max( rusust_sub, rusust_max )

      end do

      ! Some placeholder code for hills
      ! (it adjusts the threshold ratio cutoff for erosion)
      ! following st. is tmp. until sbhill is available
      !  nhill = 0

      ! if (nhill .eq. 0) then
        wr = 1.0
      !  else
      !    wr = 0.7
      !  endif

         ! For -O/-o requests, force stand-alone erosion input output even when
         ! thresholds prevent erosion from occurring this day.
           if( (mksaeinp%simday .gt. 0) .and. saeinp_forceday ) then
               sweepfile = 'erod.sweep'
               gridfile = '../../erod.grdx'
               call saeinp( julday, nsubr )
         end if
      
      ! Check wind ratio
      if( .not. in_sweep ) then
        if (rusust_max .le. wr) then
          return
        endif
      end if

      ! entering erosion submodel

         ! For -e1 requests, emit stand-alone input only on actual erosion days.
           if( (mksaeinp%simday .gt. 0) .and. (.not. saeinp_forceday) ) then
               sweepfile = 'erod.sweep'
               gridfile = '../../erod.grdx'
               call saeinp( julday, nsubr )
         end if

      if (btest(am0efl,2)) then
         if( luo_emit .eq. 0 ) then
            daypath = trim(mksaeout%fullpath) // makenamnum('saeros', mksaeout%simday, mksaeout%maxday,'/')
            call makedir(daypath)
            call fopenk(luo_emit, trim(daypath) // 'erod.emit','unknown')
         end if
      end if

      if (btest(am0efl,3)) then
         if( luo_sgrd .eq. 0 ) then
            daypath = trim(mksaeout%fullpath) // makenamnum('saeros', mksaeout%simday, mksaeout%maxday,'/')
            call makedir(daypath)
            call fopenk(luo_sgrd, trim(daypath) // 'erod.sgrd','unknown')
         end if
      end if

      ! sbinit calls sbsdfi to get sf< 0.01,0.1,0.84,2.0 mm
      ! and writes to grid, writes other var. to grid and
      ! zeros eros output arrays.
      call sbinit( julday, nsubr, cellstate )

      ! calc. sweep direction based on wind direction for sberod
      prev_dir = subday(1)%awdir+ 1.0   !make different to force calculation
      ! NOTE: this would be moved into subday loop if daily direction array is populated and surface updating can handle changing directions
      if( subday(1)%awdir .ne. prev_dir ) then
         call sbdirini( subday(1)%awdir )
         ! determine barrier influence
         call sbbr( cellstate )
         prev_dir = subday(1)%awdir
      end if

      ! set flag on to update threshold fric. vel. on grid
      wustfl = 1

      ! set ref wind speed to daily max
      wuref = awudmx

      ! controls daily detailed output, printing daily header
      initflag = 0

      ! step thru each periodic wind speed
      do idx = 1, ntstep
        ! check for erosive wind speed
        if (subday(idx)%awu .lt. min_erosion_awu) then
          hr = hr + (24.0/ntstep)  !No sub_ntstep's (no erosion calculated for this 'ntstep'
        else

          ! calc. sweep direction based on wind direction for sberod
          ! only needed if one reads hourly wind directions for input
          !if( subday(1)%awdir .ne. prev_dir ) then
          !   call sbdirini( subday(1)%awdir, cellstate )
          !   ! determine barrier influence
          !   call sbbr( cellstate )
          !   prev_dir = subday(1)%awdir
          !end if

          !rut = rusust_preros(idx)  This change sabotaged code logic
          
          rut = rusust_max*subday(idx)%awu/wuref
          !if (wustfl < 1) then
          ! no erosion aka surface updating has occurred yet
          !if( rusust_preros(idx) .gt. wr ) then
             ! erosion will occur, updated surface requires full grid calculation
          !  wustfl = 1


          if (rut .le. wr) then
            ! skip since we do no erosion (still print sbemit)
            hr = hr + (24.0/ntstep)  !No sub_ntstep's (no erosion calculated for this 'ntstep'
          else   

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

            !       update_interval = SEC_PER_DAY/(ntstep * ntsub)   ! (seconds)

            ! if update interval is 15 minutes (900 seconds):
            !       ntstep = 24, then ntsub = 4
            !       ntstep = 96, then ntsub = 1
            ! if update interval is 1 minute:
            !       ntstep = 24, then ntsub = 60
            !       ntstep = 96, then ntsub = 15
            ! if update interval is 10 seconds:
            !       ntstep = 24, then ntsub = 360
            !       ntstep = 96, then ntsub = 90
            ! if update interval is 6 seconds:
            !       ntstep = 24, then ntsub = 600
            !       ntstep = 96, then ntsub = 150
            ! if update interval is 1 second:
            !       ntstep = 24, then ntsub = 3600
            !       ntstep = 96, then ntsub = 900

            ! NOTE: "ntsub" is the number of steps within a single "ntstep" to obtain the desired update interval.
            if (erod_interval > 0) then   !overide the default update interval
               ! check for even divisibility done in tsterode main program
               ntsub = SEC_PER_DAY/(erod_interval*ntstep)
               !write(6,*) 'erod_interval and ntsub',erod_interval, ntsub
            else  ! default surface updating behavior

               ! force calculation to 15 minute time steps:
               ! useful to allow enough updates of surface
               ntsub = max(1,96/ntstep)
               ! modify time step to more or less than 15 minutes
               !if (subday(idx)%awu .lt. 15.0) then
               ! if (rut < 1.1 ) then
               !    ntsub = ntsub - 2
               ! elseif (rut > 1.4) then
               !    ntsub = ntsub * 2
               ! endif
               !elseif (rut > 1.4) then
               !  ntsub = ntsub * 4
               !else
               !  ntsub = ntsub * 2
               !endif

               ! est. ntsub as fn. of approx max. erosive energy 9-6-06 LH
               wuse = 0.06*subday(idx)%awu
               wuste = wuse/rut
               enge  = wuse*wuse*(wuse-wuste)
               ntsub = nint((0.5 + 4.6*enge) * ntsub)
   
               ! insure at least 1 time step
               ntsub = max(1, ntsub)
            endif

            ! calculate the time step in seconds
            time = SEC_PER_DAY/(ntsub * ntstep)

            sub_ntstep = (24.0/ntstep)/ntsub  !fraction of hr == sub_ntstep

            ! start the inner loop time step
            ndx = 1
            ! Let hrs be sub_ntstep interval hr and keep current ntstep hr
            hrs = hr
            do while (ndx <= ntsub)

               ! prepare to update rusust and wus.
               ! note: when rusust= <0.1, sbaglos does not calculate.
               rusust_max = 0.2

               ! updates the fric. vel and threshold fric. vel on grid
               ! and calc. max. for  rusust = wus/wust
               ! this subroutine calls sbzo and sbwus
               call sbwind( julday, wustfl, subday(idx)%awu, ntstep, idx, rusust_max, cellstate )
               wuref = subday(idx)%awu
               wr = 1

               if (rusust_max .gt. wr) then
                  ! erosion will occur this time step
                  ! wustfl = 1
                  if (btest(am0efl,3)) then
                     call sb1out (ndx, ntsub, hrs, subday(idx)%awu, subday(idx)%awdir, luo_sgrd, subrsurf(julday, 1), cellstate)
                  endif

                  call sberod (julday, time, SURF_UPD_FLG, cellstate)

                  ! Compute end-of-period time in fraction of hours
                  hrs = hrs + sub_ntstep

                  if (btest(am0efl,3)) then
                     call sb2out (ndx, ntsub, hrs, luo_sgrd, cellstate)
                  endif

                  ndx = ndx + 1
               else
                  ! print out initial state, even if we never call sberode()
                  if (btest(am0efl,3).and.(ndx .eq. 1).and.(idx .eq. 1)) then
                     call sbwind( julday, wustfl, subday(idx)%awu, ntstep, idx, rusust_max, cellstate )
                     wuref = subday(idx)%awu
                     call sb1out (ndx, ntsub, hrs, subday(idx)%awu, subday(idx)%awdir, luo_sgrd, subrsurf(julday, 1), cellstate)
                  endif

                  ! set to get out of inner loop and go to next wind speed - wustfl = 0
                  ndx = ntsub + 1
               endif
   
               ! If we are ready to leave the sub_ntstep loop, update "hr" and go
               if (ndx .eq. (ntsub + 1)) then
                  ! skip to next "nstep" hr - don't care if all sub_ntsteps don't equal one ntstep
                  hr = hr + (24.0/ntstep)
               end if             
            end do
          end if
        end if

        if (btest(am0efl,2)) then
           ! write(0,*) 'idx is:', idx, 'hr is:', hr
           ! Note that we use "hr" not "hrs" here so we report the end of the "ntstep" hr period
           call sbemit (luo_emit, subday(idx)%awu, hr, cellstate, first_emit)  !Should only write data here
        end if

      end do

      ! Average surface conditions and update

      ! Purpose: update global variables changed by erosion at end of day
      ! not implemented partly because windgen does not correlate days.

      if (btest(am0efl,2)) then
         close(luo_emit)
      end if

      ! output end of day erosion results
      call daily_erodout(luo_egrd, luo_erod, luo_sgrd, mksaeout%fullpath, cellstate )

      if (btest(am0efl,3)) then
         close(luo_sgrd)
      end if

    end subroutine erosion

    subroutine erodinit( noerod )
      ! Controls calls to subroutines that:
      ! calculate normalized effect of hills on friction velocity 
      ! on grid for each wind direction (not activated)
      ! initialize reporting variables that need to have a value even
      ! when erosion is not being called.

      ! + + + Modules Used + + +
      use erosion_data_struct_defs, only: threshold, cellsurfacestate
      use grid_mod, only: sbigrd

      ! +++ ARGUMENT DECLARATIONS +++
      type(threshold), dimension(:), intent(inout) :: noerod                 ! report values to show which factors prevented erosion

      ! +++ LOCAL VARIABLES +++
      integer :: sr    ! subregion do loop index
      integer :: nsubr ! total number of subregions

      ! +++ END SPECIFICATIONS +++

      nsubr = size(noerod)

      do sr = 1, nsubr
           ! initalize erosion threshold trigger variables
           noerod(sr)%erosion = 0
           noerod(sr)%snowdepth = 0

           noerod(sr)%wus_anemom = 0
           noerod(sr)%wus_random = 0
           noerod(sr)%wus_ridge = 0
           noerod(sr)%wus_biodrag = 0
           noerod(sr)%wus = 0

           noerod(sr)%bare = 0
           noerod(sr)%flat_cov = 0
           noerod(sr)%surf_wet = 0
           noerod(sr)%ag_den = 0
           noerod(sr)%wust = 1

           noerod(sr)%sfd84 = 0
           noerod(sr)%asvroc = 0
           noerod(sr)%wzzo = 0
           noerod(sr)%sfcv = 0
      end do

      ! zero out gridcell erosion totaling arrays
      call sbigrd()

    end subroutine erodinit

end module erosion_mod
