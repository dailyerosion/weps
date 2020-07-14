!$Author$
!$Date$
!$Revision$
!$HeadURL$

module crop_mod

  integer, dimension(:), allocatable :: cprevseasonrotation ! rotation count number previously printed in crop season report

  contains

    subroutine callcrop(daysim, sr, soil, plant, croptot, restot, biotot, h1et)
! ***************************************************************** wjr
! Wrapper to call crop

      use soil_data_struct_defs, only: soil_def
      use biomaterial, only: plant_pointer, residue_pointer, biototal, residueAdd
      use timer_mod, only: timer, TIMCROP, TIMSTART, TIMSTOP
      use crop_data_struct_defs, only: am0cdb, crop_residue, create_crop_residue, destroy_crop_residue
      use hydro_data_struct_defs, only: hydro_derived_et
      use crop_growth_mod, only: cropgrow
      use update_mod, only: plantupdate
      use WEPS_UPGM_mod, only: run_UPGM

      ! + + +   ARGUMENT DECLARATIONS + + +
      integer daysim
      integer sr
      type(soil_def), intent(in) :: soil  ! soil for this subregion
      type(plant_pointer), pointer :: plant     ! pointer to youngest plant data, which chains to older plant data
      type(biototal), intent(inout) :: croptot
      type(biototal), intent(inout) :: restot
      type(biototal), intent(inout) :: biotot  ! structure array containing summary amounts for all biomass
      type(hydro_derived_et), intent(in) :: h1et

! Local Variables
      integer lay
      type(crop_residue) :: cropres
      logical :: crop_growing
      type(plant_pointer), pointer :: thisPlant       ! pointer used to interate plant pointer chain

      ! + + + END OF SPECIFICATIONS + + +

      call timer(TIMCROP,TIMSTART)

! Note that crop "may" really require (admbgz + admrtz) in place of admbgz
! because crop wants to know the amount of biomass in each soil layer
! for nutrient cycling.  However, since the nutrient cycling is supposed
! to be disabled, we won't worry about it right now.  LEW - 04/23/99


      thisPlant => plant
      do while ( associated(thisPlant) )
         ! plant exists
         ! check for a valid growing crop
         if(      (thisPlant%database%shoot .le. 0.0) &
           .or. (thisPlant%geometry%dpop .le. 0.0) &
           .or. (thisPlant%database%idc .le. 0) ) then
           ! this is not a valid growing crop
           thisPlant%growth%growing = .false.
           crop_growing = .false.
         else
           crop_growing = thisPlant%growth%growing
         end if

         if( crop_growing ) then

           if( associated(thisPlant%upgm_grow%plant) ) then

            call run_UPGM( sr, soil, thisPlant )

           else

            cropres = create_crop_residue(soil%nslay)

            if (am0cdb(sr).eq.1) call cdbug(sr, soil, thisPlant, restot, h1et)

            call cropgrow(sr, soil%nslay, soil%aszlyd, &
              thisPlant%database%ck, thisPlant%database%grf, thisPlant%database%ehu0, thisPlant%database%zmxc, &
              thisPlant%bname, thisPlant%database%idc, thisPlant%geometry%xrow, &
              thisPlant%database%zmrt, thisPlant%database%tmin, thisPlant%database%topt, &
              thisPlant%database%fd1(1), thisPlant%database%fd2(1), thisPlant%database%fd1(2), thisPlant%database%fd2(2), &
              thisPlant%database%bceff, &
              thisPlant%database%alf, thisPlant%database%blf, thisPlant%database%clf, &
              thisPlant%database%dlf, thisPlant%database%arp, thisPlant%database%brp, thisPlant%database%crp, &
              thisPlant%database%drp, thisPlant%database%aht, thisPlant%database%bht, &
              thisPlant%database%sla, thisPlant%database%hue, thisPlant%database%tverndel, &
              soil%tsmx, soil%tsmn, &
              thisPlant%growth%fwsf, &
              thisPlant%growth%am0cif, &
              thisPlant%database%baf, &
              thisPlant%geometry%hyfg, thisPlant%database%thum, thisPlant%geometry%dpop, thisPlant%database%dmaxshoot, &
              thisPlant%database%storeinit, thisPlant%database%fshoot, &
              thisPlant%database%growdepth, thisPlant%database%fleafstem, thisPlant%database%shoot, &
              thisPlant%database%diammax, thisPlant%database%ssa, thisPlant%database%ssb, &
              thisPlant%database%fleaf2stor, thisPlant%database%fstem2stor, thisPlant%database%fstor2stor, &
              thisPlant%database%yld_coef, thisPlant%database%resid_int, thisPlant%database%xstm, &
              thisPlant%mass%standstem, thisPlant%mass%standleaf, thisPlant%mass%standstore, &
              thisPlant%mass%flatstem, thisPlant%mass%flatleaf, thisPlant%mass%flatstore, &
              thisPlant%growth%mshoot, thisPlant%growth%mtotshoot, thisPlant%mass%stemz, &
              thisPlant%mass%rootstorez, thisPlant%mass%rootfiberz, &
              thisPlant%geometry%zht, thisPlant%geometry%zshoot, thisPlant%geometry%dstm, thisPlant%geometry%zrtd, &
              thisPlant%growth%dayap, thisPlant%growth%dayam, &
              thisPlant%growth%thucum, thisPlant%growth%trthucum, &
              thisPlant%geometry%grainf, thisPlant%growth%zgrowpt, thisPlant%growth%fliveleaf, &
              thisPlant%growth%leafareatrend, thisPlant%growth%stemmasstrend, &
              thisPlant%growth%twarmdays, thisPlant%growth%tcolddays, &
              thisPlant%growth%tchillucum, thisPlant%growth%thardnx, thisPlant%growth%thu_shoot_beg, &
              thisPlant%growth%thu_shoot_end, thisPlant%growth%mtotleaf, thisPlant%growth%thu_leaf_beg, &
              thisPlant%growth%thu_leaf_end, thisPlant%geometry%xstmrep, &
              thisPlant%prev%standstem, thisPlant%prev%standleaf, thisPlant%prev%standstore, &
              thisPlant%prev%flatstem, thisPlant%prev%flatleaf, thisPlant%prev%flatstore, &
              thisPlant%prev%mshoot, thisPlant%prev%stemz, &
              thisPlant%prev%rootstorez, thisPlant%prev%rootfiberz, &
              thisPlant%prev%ht, thisPlant%prev%zshoot, thisPlant%prev%stm, thisPlant%prev%rtd, &
              thisPlant%prev%dayap, thisPlant%prev%hucum, thisPlant%prev%rthucum, &
              thisPlant%prev%grainf, thisPlant%prev%chillucum, thisPlant%prev%liveleaf, &
              thisPlant%prev%dayspring, thisPlant%prev%dayfall, daysim, &
              thisPlant%growth%dayspring, thisPlant%growth%dayfall, thisPlant%database%zloc_regrow, &
              cropres%standstem, cropres%standleaf, cropres%standstore, &
              cropres%flatstem, cropres%flatleaf, cropres%flatstore, &
              cropres%stemz, &
              cropres%zht, cropres%dstm, cropres%xstmrep, cropres%grainf, thisPlant%database%plant_doy )

            ! check for abandoned stems in crop regrowth
            if( ( cropres%standstem + cropres%standleaf + cropres%standstore &
               + cropres%flatstem + cropres%flatleaf + cropres%flatstore ) &
                 .gt. 0.0 ) then
              ! create new residue pool and transfer cropres into it
              thisPlant%residue => residueAdd( thisPlant%residue, thisPlant%residueIndex, soil%nslay ) 

              thisPlant%residue%standstem = cropres%standstem
              thisPlant%residue%standleaf = cropres%standleaf
              thisPlant%residue%standstore = cropres%standstore
              thisPlant%residue%flatstem = cropres%flatstem
              thisPlant%residue%flatleaf = cropres%flatleaf
              thisPlant%residue%flatstore = cropres%flatstore
              do lay = 1, soil%nslay
                thisPlant%residue%stemz(lay) = cropres%stemz(lay)
              end do
              thisPlant%residue%zht = cropres%zht
              thisPlant%residue%dstm = cropres%dstm
              thisPlant%residue%xstmrep = cropres%xstmrep
              thisPlant%residue%grainf = cropres%grainf

            end if

            call destroy_crop_residue(cropres)

           end if

           if (am0cdb(sr).eq.1) call cdbug(sr, soil, thisPlant, restot, h1et)

           ! update all derived globals for thisPlant global variables
           call plantupdate( soil, thisPlant, croptot, restot, biotot )

           ! set prevday derived variable for later reference in end_season
           thisPlant%prev%cancov = thisPlant%deriv%fcancov

         end if

         ! point to next older thisPlant
         thisPlant => thisPlant%olderPlant

      end do

      call timer(TIMCROP,TIMSTOP)

    end subroutine callcrop

    subroutine plant_endseason ( isr, bmrotation, bmperod, bm0cfl, &
                                bnslay, mature_warn_flg, plant )

      ! + + + PURPOSE + + +
      ! Prints out crop status variables that are of interest at the end of the season

      ! + + + KEYWORDS + + +
      ! crop model status

      use weps_cmdline_parms, only: report_info
      use weps_main_mod, only: init_loop, calib_loop
      use datetime_mod, only: get_simdate, julday
      use file_io_mod, only: luoseason
      use manage_data_struct_defs, only: lastoper
      use biomaterial, only: plant_pointer

      ! + + + ARGUMENT DECLARATIONS + + +
      integer, intent(in) :: isr   ! subregion number
      integer, intent(in) :: bmrotation ! rotation count updated in manage.for
      integer, intent(in) :: bmperod ! number of years for a management cycle
      integer, intent(in) :: bm0cfl  ! flag to print CROP output
                                     ! 0 = no output
                                     ! 1 = detailed output file created
      integer, intent(in) :: bnslay  ! number of soil layers
      integer, intent(in) :: mature_warn_flg  ! flag to indicate use of crop maturity warning
                                              ! 0  - no crop maturity warning given for any crop
                                              ! 1  - Warnings generated for any crop unless supressed by crop type
      type(plant_pointer), pointer :: plant   ! pointer to youngest plant data, which chains to older plant data

      ! + + + LOCAL VARIABLES + + +
      integer lay, dd, mm, yy
      real :: hui
      real bg_stem_sum, root_store_sum, root_fiber_sum
      integer adj_plant_yr
      type(plant_pointer), pointer :: thisPlant

      ! + + + LOCAL VARIABLE DEFINITIONS + + +
      ! lay - index used to loop through layers
      ! dd,mm,yy - the current day, month, and year
      ! hui - heat unit index
      ! bg_stem_sum - sum of below ground stem
      ! root_store_sum - sum of root storage
      ! root_fiber_sum - sum of root fiber
      ! adj_plant_yr - planting year adjusted to be less than the operation year that triggered this report

      ! + + + OUTPUT FORMATS + + +
 2010 format(1x,i2,'/',i2,'/',i3,'|',1x,i2,'/',i2,'/',i2,'|',a40,'|',   &
             10(f7.3,'|'),f7.2,'|',2(f7.3,'|'),f7.5,'|',f7.3,'|',i6,'|',&
             3(f8.1,'|'),f5.3,'|',i4,'|',i6,'|')

      ! + + + END OF SPECIFICATIONS + + +

      if( init_loop .or. calib_loop ) then  !initilizing or calibrating cycle

        ! set to the beginning of simulation
        ! to eliminate newline at beginning of file
        cprevseasonrotation(isr) = 1

      else  !done when initializing and calibrating cycle(s) are completed

        if( bmrotation .gt. cprevseasonrotation(isr) ) then
          ! write newline
          write(unit=luoseason(isr),fmt="(a)") ''
        end if

        ! day of year
        call get_simdate(dd, mm, yy)

        ! end of season print statements when crop submodel output flag set
        ! added initialization flag to prevent printing if crop not yet initialized

        ! loop to find plant for report
        thisPlant => plant
        do while( associated(thisPlant) )
          ! plant exists
          if( thisPlant%database%idc .gt. 0 ) then
            ! this is a plant, not some added residue

            if( thisPlant%database%thum .gt. 0.0 ) then
              hui = thisPlant%prev%hucum / thisPlant%database%thum
            else
              hui = 0.0
            end if

            ! print end-of-season (before harvest) crop state
            if( (bm0cfl .ge. 0) ) then ! Always print this one now - LEW
              bg_stem_sum = 0.0
              root_store_sum = 0.0
              root_fiber_sum = 0.0
              do lay = 1, bnslay
                bg_stem_sum = bg_stem_sum + thisPlant%prev%stemz(lay)
                root_store_sum = root_store_sum + thisPlant%prev%rootstorez(lay)
                root_fiber_sum = root_fiber_sum + thisPlant%prev%rootfiberz(lay)
              end do

              ! adjust planting year to be less than the operation year that triggered this report
              if(     julday(thisPlant%database%plant_day, thisPlant%database%plant_month, thisPlant%database%plant_rotyr) &
                 .gt. julday(lastoper(isr)%day,lastoper(isr)%mon,lastoper(isr)%yr) ) then
                adj_plant_yr = thisPlant%database%plant_rotyr - bmperod
              else
                adj_plant_yr = thisPlant%database%plant_rotyr
              end if

              write(UNIT=luoseason(isr),FMT=2010,advance='NO') &
                thisPlant%database%plant_day, thisPlant%database%plant_month, adj_plant_yr, &
                lastoper(isr)%day, lastoper(isr)%mon, lastoper(isr)%yr, thisPlant%bname, &
                thisPlant%prev%standstem, thisPlant%prev%standleaf, thisPlant%prev%standstore, &
                thisPlant%prev%flatstem, thisPlant%prev%flatleaf, thisPlant%prev%flatstore, &
                bg_stem_sum, root_store_sum, root_fiber_sum, &
                thisPlant%prev%ht, thisPlant%prev%stm, thisPlant%prev%rtd, thisPlant%prev%grainf, &
                thisPlant%geometry%xstmrep, thisPlant%prev%cancov, thisPlant%prev%dayap, thisPlant%prev%chillucum, &
                thisPlant%prev%hucum, thisPlant%database%thum, hui, thisPlant%growth%dayam, thisPlant%prev%dayspring
            end if

            ! for annual crops, ALWAYS write out warning message
            ! if harvested before maturity
            ! Note that this is reported back to the WEPS GUI
            ! So, 'report_info' should be set to 1 (default) or greater under normal run conditions
            if( (hui < 1.0) .and. (mature_warn_flg .gt. 0) &
              .and. ( (thisPlant%database%idc.eq.1) .or. (thisPlant%database%idc.eq.2) &
                 .or. (thisPlant%database%idc.eq.4) .or. (thisPlant%database%idc.eq.5) ) ) then
              if (report_info >= 1) then
                write(UNIT=6,FMT="(1x,3(a),i0,'/',i0,'/',i0,a,f5.1,a,a)") &
                 'Warning: ', &
                 thisPlant%bname(1:len_trim(thisPlant%bname)), &
                 ' harvested ', &
                 dd, mm, yy, &
                 ' only reached ', hui*100.0, '% of maturity', &
                 ' (Check crop selection, planting, harvest dates)'
              end if
            end if

            ! updated every call to get newline in right place
            cprevseasonrotation(isr) = bmrotation

            ! only dealing with one plant in this report, exit do loop on first valid plant
            exit

          end if

          ! go to next older plant
          thisPlant => thisPlant%olderPlant
        end do

      end if

      return
    end subroutine plant_endseason

    subroutine cpout( isr )
      ! Author : A. Retta - 11/19/96
      ! + + + PURPOSE + + +
      ! Prints headers for the CROP submodel output files

      use file_io_mod, only: luoseason, luocrop, luoshoot, luoinpt
      use crop_data_struct_defs, only: am0cfl

      integer, intent(in) :: isr   ! subregion number

      ! + + + OUTPUT FORMATS + + +
 2131 format ('#    -    -   -    -    -   stand   stand   stand   flat &
     &   flat    flat    root    root  bel.grnd  total   total')
 2132 format ('# daysim doy year dap heatui stem    leaf    store   stem&
     &    leaf    store   store   fiber   stem    leaf    stem    height&
     &   stem   lai     eff_lai rootd  grainf tempst watstf  frost  ffa &
     &   ffw   par     apar     massinc    p_rw   p_st   p_lf   p_rp  st&
     &dflt pdiam  parea  fpdiam fparea hu_del frzhrd sai repstmd rgflg f&
     &livelf crop')
 2133 format ('#    -    -   -    -    -   kg/m^2  kg/m^2  kg/m^2  kg/m^&
     &2  kg/m^2  kg/m^2  kg/m^2  kg/m^2  kg/m^2  kg/m^2  kg/m^2  meters &
     &    -    m^2/m^2 m^2/m^2 meters     -      -     -       -     -  &
     &  #/m^2 MJ/m^2  MJ/m^2   kg/plnt      -      -      -       -     &
     & -  meters m^2')

 2231 format ('# daysim doy year dap heatui ',                          &
     &        's_root_sum f_root_sum tot_mass_req end_shoot_mass ',     &
     &        'end_root_mass d_root_mass d_shoot_mass d_s_root_mass ',  &
     &        'end_stem_mass end_stem_area end_shoot_len bczshoot ',    &
     &        'bcmshoot bcdstm crop%bname')
 2232 format ('# (dy) (dy) (yr) (dy) (C)    ',                          &
     &        '(kg/m^2)   (kg/m^2)   (mg/shoot)   (mg/shoot)     ',     &
     &        '(mg/shoot)    (mg/shoot)  (mg/shoot)   (mg/shoot)    ',  &
     &        '(mg/shoot)    (m^2/shoot)   (m)           (m)      ',    &
     &        '(kg/m^2) (#/m^2)')

 2043 format('# Planting |Harv/Term  |')
 2044 format('#dy mo year|dy mo year |')
 2045 format('#          |           |')

 2053 format('                                        |')
 2054 format('crop_name                               |')
 2055 format('                                        |')

 2063 format('standing|      |       |flat   |       |       |root  |')
 2064 format('stem    |leaf  |store  |stem   |leaf   |store  |stem  |')
 2065 format('kg/m^2  |------|-------|-------|-------|-------|------|')

 2073 format('       |       |      |         |root  |')
 2074 format('store  |fiber  |height|stemcount|depth |')
 2075 format('-------|-------|meters|#/m^2    |meters|')

 2084 format('grainf |stmrepd|cancov|dapl    |chill |hucum  |mxhu |')
 2085 format('-------|meters |----- |days    |deg_C |deg_C  |deg_C|')

 2094 format('huind|dafm|spring')
 2095 format('-----|days|------')

 6000 format('#plant harvest 0=days_mat calc_d_mat db_d_mat calc_heatu d&
     &b_heatu')
 6001 format('# doy    doy   1=heatunit    days      days    degree_C  d&
     &egree_C')



      ! season.out headers

      write(luoseason(isr),2043,ADVANCE="NO")
      write(luoseason(isr),2053,ADVANCE="NO")
      write(luoseason(isr),2063,ADVANCE="NO")
      write(luoseason(isr),2073,ADVANCE="YES")

      write(luoseason(isr),2044,ADVANCE="NO")
      write(luoseason(isr),2054,ADVANCE="NO")
      write(luoseason(isr),2064,ADVANCE="NO")
      write(luoseason(isr),2074,ADVANCE="NO")
      write(luoseason(isr),2084,ADVANCE="NO")
      write(luoseason(isr),2094,ADVANCE="YES")

      write(luoseason(isr),2045,ADVANCE="NO")
      write(luoseason(isr),2055,ADVANCE="NO")
      write(luoseason(isr),2065,ADVANCE="NO")
      write(luoseason(isr),2075,ADVANCE="NO")
      write(luoseason(isr),2085,ADVANCE="NO")
      write(luoseason(isr),2095,ADVANCE="YES")

      if (am0cfl(isr).gt.0) then

         ! crop.out headers
         write(luocrop(isr), 2131)
         write(luocrop(isr), 2132)
         write(luocrop(isr), 2133)

         ! shoot.out headers
         write(luoshoot(isr), 2231)
         write(luoshoot(isr), 2232)

         ! inpt.out headers
         write(luoinpt(isr), 6000)
         write(luoinpt(isr), 6001)

      endif
      return
    end subroutine cpout

    subroutine cdbug(isr, soil, plant, restot, h1et)

      ! + + + PURPOSE + + +
!    This program prints out many of the global variables before
!    and after the call to CROP provide a comparison of values
!    which may be changed by CROP

!    author: John Tatarko
!    version: 09/01/92

      ! + + + KEY WORDS + + +
      ! wind, erosion, hydrology, tillage, soil, crop, decomposition
      ! management

      use datetime_mod, only: get_simdate
      use file_io_mod, only: luocdb
      use soil_data_struct_defs, only: soil_def
      use biomaterial, only: plant_pointer, biototal
      use erosion_data_struct_defs, only: awadir, awhrmx, awudmx, awudmn
      use hydro_data_struct_defs, only: hydro_derived_et
      use crop_data_struct_defs, only: tisr, tday, tmo, tyr
      use climate_input_mod, only: cli_today

      ! + + +   ARGUMENT DECLARATIONS + + +
      integer, intent(in) :: isr    ! subregion index
      type(soil_def), intent(in) :: soil  ! soil for this subregion
      type(plant_pointer), pointer :: plant     ! pointer to youngest plant data, which chains to older plant data
      type(biototal), intent(in) :: restot   ! structure containing residue totals
      type(hydro_derived_et), intent(in) :: h1et

      ! + + + LOCAL VARIABLES + + +
      integer cd, cm, cy
      integer        l

      ! + + + LOCAL DEFINITIONS + + +

!   cd        - The current day of simulation month.
!   cm        - The current month of simulation year.
!   cy        - The current year of simulation run.
!   l         - This variable is an index on soil layers.

      ! + + + SUBROUTINES CALLED + + +

      ! + + + FUNCTIONS CALLED + + +

      ! + + + UNIT NUMBERS FOR INPUT/OUTPUT DEVICES + + +
      ! * = screen and keyboard
!    27 = debug CROP

      ! + + + DATA INITIALIZATIONS + + +

      if (plant%growth%am0cif .eqv. .true.) then
          tday = -1
          tmo = -1
          tyr = -1
          tisr = -1
      end if
      call get_simdate (cd, cm, cy)

      ! + + + INPUT FORMATS + + +

      ! + + + OUTPUT FORMATS + + +
 2030 format ('**',1x,2(i2,'/'),i4,'    After  call to CROP         Subr&
     &egion No. ',i3)
 2031 format ('**',1x,2(i2,'/'),i4,'    Before call to CROP         Subr&
     &egion No. ',i3)
 2032 format (' awzdpt  awtdmx  awtdmn  aweirr  awudmx  awudmn ',       &
     &        ' awtdpt  awadir  awhrmx')
 2038 format (f7.2,9f8.2)
! 2045 format ('Subregion Number',i3)
 2050 format ('amrslp(',i2,') acftcv(',i2,') acrlai(',i2,')',           &
     &        ' aczrtd(',i2,') admftot(',i2,') fwsf(',i2,')',         &
     &        ' ac0nam(',i2,')')
 2051 format (2f10.2,2f10.5,2x,f10.2,f10.2,3x,a12)
 2052 format ('actdtm(',i2,') sum-phu(',i2,') acmst(',i2,')',  &
              '  acmrt(',i2,')  h1et%zeta h1et%zetp', &
              ' h1et%zpta ')
 2053 format (i10, 4f10.2,2f12.2)
 2054 format (' h1et%zea  h1et%zep h1et%zptp ', &
              ' actmin(',i2,') actopt(',i2,') aslrr(',i2,')')
 2055 format (2f10.2,2f10.3,3f12.2)
 2056 format('layer aszlyt  ahrsk ahrwc ahrwcs ahrwca',                 &
     &       ' ahrwcf ahrwcw ah0cb aheaep ahtsmx ahtsmn')
 2060 format (i4,1x,f7.2,1x,e7.1,f6.2,4f7.2,f6.2,3f7.2)
 2065 format(' layer  asfsan asfsil asfcla asfom asdblk aslagm  as0ags',&
     &       ' aslagn  aslagx  aseags')
 2070 format (i4,2x,3f7.2,f7.3,2f7.2,f8.2,f7.3,2f8.2)

      ! + + + END SPECIFICATIONS + + +

      ! write weather cligen and windgen variables
      if ((cd .eq. tday) .and. (cm .eq. tmo) .and. (cy .eq. tyr) .and.  &
     &   (isr .eq. tisr)) then
         write(luocdb(isr),2030) cd,cm,cy,isr
      else
         write(luocdb(isr),2031) cd,cm,cy,isr
      end if
      write(luocdb(isr),2032)
      write(luocdb(isr),2038) cli_today%zdpt, cli_today%tdmx, cli_today%tdmn, cli_today%eirr, awudmx, &
     &                        awudmn, cli_today%tdpt, awadir, awhrmx

      ! write(luocdb(isr),2045) isr

      write(luocdb(isr),2050) isr, isr, isr, isr, isr, isr, isr
      write(luocdb(isr),2051) soil%amrslp, plant%deriv%ftcv, plant%deriv%rlai,    &
     &               plant%geometry%zrtd, restot%mftot, plant%growth%fwsf, plant%bname
      write(luocdb(isr),2052) isr, isr, isr, isr
      write(luocdb(isr),2053)                                           &
     &               plant%database%tdtm, plant%growth%thucum, plant%deriv%mst, plant%deriv%mrt, &
     &               h1et%zeta, h1et%zetp, h1et%zpta
      write(luocdb(isr),2054) isr, isr, isr, isr
      write(luocdb(isr),2055) h1et%zea, h1et%zep, h1et%zptp, plant%database%tmin, &
     &               plant%database%topt, soil%aslrr
      write(luocdb(isr),2056)

      do 200 l = 1,soil%nslay
         write(luocdb(isr),2060) l,soil%aszlyt(l), soil%ahrsk(l), soil%ahrwc(l), &
     &                  soil%ahrwcs(l), soil%ahrwca(l), soil%ahrwcf(l), &
     &                  soil%ahrwcw(l), soil%ah0cb(l), soil%aheaep(l), &
     &                  soil%tsmx(l), soil%tsmn(l)
  200 continue
         write(luocdb(isr),2065)

      do 300 l=1,soil%nslay
         write(luocdb(isr),2070) l,soil%asfsan(l),soil%asfsil(l), &
     &                  soil%asfcla(l), soil%asfom(l), soil%asdblk(l), &
     &                  soil%aslagm(l), soil%as0ags(l), soil%aslagn(l), &
     &                  soil%aslagx(l), soil%aseags(l)
  300 continue

      tisr = isr
      tday = cd
      tmo = cm
      tyr = cy

      return
    end subroutine cdbug

end module crop_mod
