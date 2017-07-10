!$Author$
!$Date$
!$Revision$
!$HeadURL$

module crop_mod

  integer, dimension(:), allocatable :: cprevseasonrotation ! rotation count number previously printed in crop season report

  contains

    subroutine cropinit(isr, crop)

      use biomaterial, only: biomatter

!     + + + ARGUMENT DECLARATIONS + + +
      integer isr
      type(biomatter), intent(inout) :: crop    ! structure containing full crop description

!     + + + PARAMETERS AND COMMON BLOCKS + + +
      include 'p1werm.inc'

!     + + + LOCAL VARIABLE DECLARATIONS + + +
      integer idx

      ! no crop growing at start of simulation
      crop%growth%am0cgf = .false.
      crop%growth%am0cif = .false.

      crop%mass%standstem = 0.0
      crop%mass%standleaf = 0.0
      crop%mass%standstore = 0.0
      crop%mass%flatstem = 0.0
      crop%mass%flatleaf = 0.0
      crop%mass%flatstore = 0.0
      crop%mass%flatrootstore = 0.0
      crop%mass%flatrootfiber = 0.0
      do idx = 1, size(crop%mass%rootstorez)
          crop%mass%stemz(idx) = 0.0
          crop%mass%leafz(idx) = 0.0
          crop%mass%storez(idx) = 0.0
          crop%mass%rootstorez(idx) = 0.0
          crop%mass%rootfiberz(idx) = 0.0
      end do

      crop%geometry%xrow = 0.0
      crop%geometry%zht = 0.0
      crop%geometry%dstm = 0.0
      crop%geometry%zrtd = 0.0
      crop%growth%dayap = 0
      crop%growth%thucum = 0.0
      crop%growth%trthucum = 0.0
      crop%geometry%grainf = 0.0
      crop%deriv%mbgrootstore = 0.0
      crop%deriv%mbgrootfiber = 0.0
      crop%geometry%xstmrep = 0.0
      crop%growth%fliveleaf = 0.0

      crop%deriv%m = 0.0
      crop%deriv%mst = 0.0
      crop%deriv%mf = 0.0
      crop%deriv%mrt = 0.0

      do idx = 1, size(crop%deriv%mrtz)
          crop%deriv%mrtz(idx) = 0.0
      end do

      crop%deriv%rsai = 0.0
      crop%deriv%rlai = 0.0

      do idx = 1, size(crop%deriv%rsaz)
          crop%deriv%rsaz(idx) = 0.0
          crop%deriv%rlaz(idx) = 0.0
      end do

      crop%deriv%ffcv = 0.0
      crop%deriv%fscv = 0.0
      crop%deriv%ftcv = 0.0

      crop%database%xstm = 0.0
      crop%database%rbc = 1
      crop%database%covfact = 0.0
      crop%database%ck = 0.0

      ! initialize some derived globals for crop global variables
      crop%deriv%fcancov = 0.0
      crop%deriv%rcd = 0.0

!     initialize crop yield reporting parameters in case harvest call before planting
      crop%bname = ''
      crop%database%ynmu = ''
      crop%database%ycon = 1.0
      crop%database%ywct = 0.0

!     initialize crop type id to 0 indicating no crop type is growing
      crop%database%idc = 0
      crop%database%sla = 0.0
      crop%geometry%dpop = 0.0

!     initialize row placement to be on the ridge
      crop%geometry%rg = 1
!     initialize harvestable yield fraction flag
      crop%geometry%hyfg = 0

      ! initialize decomp parameters since they are used before a crop is growing
      do idx = 1, size(crop%database%dkrate)
          crop%database%dkrate(idx) = 0.0
      end do
      crop%database%ddsthrsh = 0.0

      ! values that need initialization for cdbug calls (before initial crop entry)
      crop%database%tdtm = 0

      crop%database%shoot = 0.0

      return
    end subroutine cropinit

    subroutine callcrop(daysim, sr, soil, crop, cropprev, residue, restot, croptot, h1et)
! ***************************************************************** wjr
! Wrapper to call crop

      use weps_interface_defs
      use soil_data_struct_defs, only: soil_def
      use biomaterial, only: biomatter, biototal, bio_prevday
      use timer_mod, only: timer, TIMCROP, TIMSTART, TIMSTOP
      use crop_data_struct_defs, only: am0cdb, crop_residue, create_crop_residue, destroy_crop_residue
      use hydro_data_struct_defs, only: hydro_derived_et
      use crop_growth_mod, only: cropgrow

!     + + +   ARGUMENT DECLARATIONS + + +
      integer daysim
      integer sr
      type(soil_def), intent(in) :: soil  ! soil for this subregion
      type(biomatter), intent(inout) :: crop    ! structure containing full crop description
      type(bio_prevday), intent(inout) :: cropprev    ! structure containing crop previous day values
      type(biomatter), dimension(:), intent(inout) :: residue  ! structure containing full residue pool description
      type(biototal), intent(in) :: restot
      type(biototal), intent(inout) :: croptot
      type(hydro_derived_et), intent(in) :: h1et

! Includes
      include 'p1werm.inc'
      include 'h1hydro.inc'
      include 'h1temp.inc'

! Local Variables
      integer lay
      type(crop_residue) :: cropres

!     + + + END OF SPECIFICATIONS + + +

      call timer(TIMCROP,TIMSTART)

! Note that crop "may" really require (admbgz + admrtz) in place of admbgz
! because crop wants to know the amount of biomass in each soil layer
! for nutrient cycling.  However, since the nutrient cycling is supposed
! to be disabled, we won't worry about it right now.  LEW - 04/23/99

      ! check for a valid growing crop
      if(      (crop%database%shoot .le. 0.0) &
          .or. (crop%geometry%dpop .le. 0.0) &
          .or. (crop%database%idc .le. 0) ) then
          ! this is not a valid growing crop
          crop%growth%am0cgf = .false.
      end if

      ! allocate and zero out crop residue structure
      cropres = create_crop_residue(soil%nslay)

!     only continue if crop is growing
      if( crop%growth%am0cgf ) then

         if (am0cdb(sr).eq.1) call cdbug(sr, soil, crop, restot, h1et)

         call cropgrow(sr, soil%nslay, soil%aszlyd, &
     &   crop%database%ck, crop%database%grf, crop%database%ehu0, crop%database%zmxc, &
     &   crop%bname, crop%database%idc, crop%geometry%xrow, &
     &   crop%database%tdtm, crop%database%zmrt, crop%database%tmin, crop%database%topt, &
     &   crop%database%fd1(1), crop%database%fd2(1), crop%database%fd1(2), crop%database%fd2(2), &
     &   crop%database%bceff, &
     &   crop%database%alf, crop%database%blf, crop%database%clf, &
     &   crop%database%dlf, crop%database%arp, crop%database%brp, crop%database%crp, &
     &   crop%database%drp, crop%database%aht, crop%database%bht, &
     &   crop%database%sla, crop%database%hue, crop%database%tverndel, &
     &   ahtsmx(1,sr), ahtsmn(1,sr),                                    &
     &   ahfwsf(sr),                                                    &
     &   crop%growth%am0cif, &
     &   crop%database%thudf, crop%database%baf, &
     &   crop%geometry%hyfg, crop%database%thum, crop%geometry%dpop, crop%database%dmaxshoot, &
     &   crop%database%storeinit, crop%database%fshoot, &
     &   crop%database%growdepth, crop%database%fleafstem, crop%database%shoot, &
     &   crop%database%diammax, crop%database%ssa, crop%database%ssb, &
     &   crop%database%fleaf2stor, crop%database%fstem2stor, crop%database%fstor2stor, &
     &   crop%database%yld_coef, crop%database%resid_int, crop%database%xstm, &
     &   crop%mass%standstem, crop%mass%standleaf, crop%mass%standstore, &
     &   crop%mass%flatstem, crop%mass%flatleaf, crop%mass%flatstore, &
     &   crop%growth%mshoot, crop%growth%mtotshoot, crop%mass%stemz, &
     &   crop%mass%rootstorez, crop%mass%rootfiberz, &
     &   crop%geometry%zht, crop%geometry%zshoot, crop%geometry%dstm, crop%geometry%zrtd, &
     &   crop%growth%dayap, crop%growth%dayam, &
     &   crop%growth%thucum, crop%growth%trthucum, &
     &   crop%geometry%grainf, crop%growth%zgrowpt, crop%growth%fliveleaf, &
     &   crop%growth%leafareatrend, crop%growth%stemmasstrend, crop%growth%twarmdays, &
     &   crop%growth%tchillucum, crop%growth%thardnx, crop%growth%thu_shoot_beg, &
     &   crop%growth%thu_shoot_end, crop%geometry%xstmrep, &
     &   cropprev%standstem, cropprev%standleaf, cropprev%standstore, &
     &   cropprev%flatstem, cropprev%flatleaf, cropprev%flatstore, &
     &   cropprev%mshoot, cropprev%bgstemz, &
     &   cropprev%rootstorez, cropprev%rootfiberz, &
     &   cropprev%ht, cropprev%zshoot, cropprev%stm, cropprev%rtd, &
     &   cropprev%dayap, cropprev%hucum, cropprev%rthucum, &
     &   cropprev%grainf, cropprev%chillucum, cropprev%liveleaf, &
     &   cropprev%dayspring, daysim, crop%growth%dayspring, crop%database%zloc_regrow, &
     &   cropres%standstem, cropres%standleaf, cropres%standstore, &
     &   cropres%flatstem, cropres%flatleaf, cropres%flatstore, &
     &   cropres%bgstemz, &
     &   cropres%zht, cropres%dstm, cropres%xstmrep, cropres%grainf )

         if (am0cdb(sr).eq.1) call cdbug(sr, soil, crop, restot, h1et)
      end if

      ! check for abandoned stems in crop regrowth
      if( ( cropres%standstem + cropres%standleaf + cropres%standstore &
     &     + cropres%flatstem + cropres%flatleaf + cropres%flatstore ) &
     &    .gt. 0.0 ) then
          call trans( &
     &      cropres%standstem, cropres%standleaf, cropres%standstore, &
     &      cropres%flatstem, cropres%flatleaf, cropres%flatstore, &
     &      cropres%flatrootstore, cropres%flatrootfiber, &
     &      cropres%bgstemz, cropres%bgleafz, cropres%bgstorez, &
     &      cropres%bgrootstorez, cropres%bgrootfiberz, &
     &      cropres%zht, cropres%dstm, cropres%xstmrep, cropres%grainf, &
     &      crop%bname, crop%database%xstm, crop%database%rbc, crop%database%sla, crop%database%ck, &
     &      crop%database%dkrate, crop%database%covfact, crop%database%ddsthrsh, crop%geometry%hyfg, &
     &      crop%database%resevapa, crop%database%resevapb, &
     &      soil%nslay, residue)
      end if

      ! deallocate crop residue structure
      call destroy_crop_residue(cropres)

      ! update all derived globals for crop global variables
      call cropupdate(                                                  &
            soil%aszrgh, soil%aszlyd, &
     &      crop%geometry%rg, crop%geometry%xrow, &
     &      soil%nslay, crop%database%ssa, crop%database%ssb, &
     &      crop%geometry%dpop, &
     &      ahztranspdepth(sr), ahzfurcut(sr),                          &
     &      ahztransprtmin(sr), ahztransprtmax(sr), crop, croptot  )

      ! set prevday derived variable for later reference in end_season
      cropprev%cancov = crop%deriv%fcancov

      call timer(TIMCROP,TIMSTOP)

    end subroutine callcrop

    subroutine crop_endseason ( isr, bmrotation, bmperod,             &
     &                 bc0nam, bm0cfl,                                  &
     &                 bnslay, bc0idc, bcdayam,                         &
     &                 bplant_day, bplant_month, bplant_rotyr,          &
     &                 bcthum, bcxstmrep,                               &
     &                 bprevstandstem, bprevstandleaf, bprevstandstore, &
     &                 bprevflatstem, bprevflatleaf, bprevflatstore,    &
     &                 bprevbgstemz,                                    &
     &                 bprevrootstorez, bprevrootfiberz,                &
     &                 bprevht, bprevstm, bprevrtd,                     &
     &                 bprevdayap, bprevhucum, bprevrthucum,            &
     &                 bprevgrainf, bprevchillucum, bprevliveleaf,      &
     &                 bprevcancov, bprevdayspring, mature_warn_flg )

!     + + + PURPOSE + + +
!     Prints out crop status variables that are of interest at the end of the season

!     + + + KEYWORDS + + +
!     crop model status

      use weps_main_mod, only: init_loop, calib_loop
      use datetime_mod, only: get_simdate, julday
      use file_io_mod, only: luoseason
      use manage_data_struct_defs, only: lastoper

!     + + + ARGUMENT DECLARATIONS + + +
      integer, intent(in) :: isr   ! subregion number
      integer, intent(in) :: bmrotation ! rotation count updated in manage.for
      integer, intent(in) :: bmperod ! number of years for a management cycle
      character*(80) bc0nam
      integer bm0cfl, bnslay, bc0idc, bcdayam
      integer bplant_day, bplant_month, bplant_rotyr
      real bcthum, bcxstmrep
      real bprevstandstem, bprevstandleaf, bprevstandstore
      real bprevflatstem, bprevflatleaf, bprevflatstore
      real bprevbgstemz(*)
      real bprevrootstorez(*), bprevrootfiberz(*)
      real bprevht, bprevstm, bprevrtd
      integer bprevdayap
      real bprevhucum, bprevrthucum
      real bprevgrainf, bprevchillucum, bprevliveleaf
      real bprevcancov
      integer bprevdayspring, mature_warn_flg

!     + + + ARGUMENT DEFINITIONS + + +

!     bc0nam - crop name
!     bnslay - number of soil layers
!     bc0idc - crop type:annual,perennial,etc
!     bcdayam - number of days since crop matured
!     bplant_day - day on month crop was planted
!     bplant_month - month of year crop was planted
!     bplant_rotyr - rotation year crop was planted
!     bcthum - potential heat units for crop maturity (deg. C)
!     bcxstmrep - a representative diameter so that acdstm*acxstmrep*aczht=acrsai
!     bcmstandstem - crop standing stem mass (kg/m^2)
!     bcmstandleaf - crop standing leaf mass (kg/m^2)
!     bcmstandstore - crop standing storage mass (kg/m^2)
!                    (head with seed, or vegetative head (cabbage, pineapple))
!     bcmflatstem  - crop flat stem mass (kg/m^2)
!     bcmflatleaf  - crop flat leaf mass (kg/m^2)
!     bcmflatstore - crop flat storage mass (kg/m^2)
!     bcmbgstemz - crop stem mass below soil surface by soil layer (kg/m^2)
!     bcmrootstorez - crop root storage mass by soil layer (kg/m^2)
!                   (tubers (potatoes, carrots), extended leaf (onion), seeds (peanuts))
!     bcmrootfiberz - crop root fibrous mass by soil layer (kg/m^2)
!     bczht  - Crop height (m)
!     bcdstm - Number of crop stems per unit area (#/m^2)
!     bczrtd  - Crop root depth (m)
!     bprevdayap - number of days of growth completed since crop planted
!     bcthucum - crop accumulated heat units
!     bctrthucum - accumulated root growth heat units (degree-days)
!     bcgrainf - internally computed reproductive grain fraction
!     bctchillucum - accumulated chilling units (days)
!     bcfliveleaf - fraction of standing plant leaf which is living (transpiring)
!     bcfcancov - crop canopy cover (fraction)
!     bprevdayspring - day of year in which a winter annual releases stored growth
!     mature_warn_flg - flag to indicate use of crop maturity warning
!                0  - no crop maturity warning given for any crop
!                1  - Warnings generated for any crop unless supressed by crop type

!     + + + LOCAL VARIABLES + + +
      integer lay, dd, mm, yy
      real hui
      real bg_stem_sum, root_store_sum, root_fiber_sum
      integer adj_plant_yr

!     + + + LOCAL VARIABLE DEFINITIONS + + +
!     lay - index used to loop through layers
!     dd,mm,yy - the current day, month, and year
!     hui - heat unit index
!     bg_stem_sum - sum of below ground stem
!     root_store_sum - sum of root storage
!     root_fiber_sum - sum of root fiber
!     adj_plant_yr - planting year adjusted to be less than the operation year that triggered this report

!     + + + OUTPUT FORMATS + + +
 2010 format(1x,i2,'/',i2,'/',i3,'|',1x,i2,'/',i2,'/',i2,'|',a40,'|',   &
     &       10(f7.3,'|'),f7.2,'|',2(f7.3,'|'),f7.5,'|',f7.3,'|',i6,'|',&
     &       3(f6.1,'|'),f5.3,'|',i4,'|',i6,'|')
 2020 format(a)

!     + + + END OF SPECIFICATIONS + + +

      if( init_loop .or. calib_loop ) then  !initilizing or calibrating cycle

        ! set to the beginning of simulation
        ! to eliminate newline at beginning of file
        cprevseasonrotation(isr) = 1

      else  !done when initializing and calibrating cycle(s) are completed

        if( bmrotation .gt. cprevseasonrotation(isr) ) then
          ! write newline
          write(unit=luoseason(isr),fmt=2020) ''
        end if

!     day of year
      call get_simdate(dd, mm, yy)

      ! end of season print statements when crop submodel output flag set
      ! added initialization flag to prevent printing if crop not yet initialized

      if( bcthum .gt. 0.0 ) then
          hui = bprevhucum / bcthum
      else
          hui = 0.0
      end if

      ! print end-of-season (before harvest) crop state
      if( (bm0cfl .ge. 0) ) then ! Always print this one now - LEW
        bg_stem_sum = 0.0
        root_store_sum = 0.0
        root_fiber_sum = 0.0
        do lay = 1, bnslay
            bg_stem_sum = bg_stem_sum + bprevbgstemz(lay)
            root_store_sum = root_store_sum + bprevrootstorez(lay)
            root_fiber_sum = root_fiber_sum + bprevrootfiberz(lay)
        end do

        ! adjust planting year to be less than the operation year that triggered this report
        if(     julday(bplant_day, bplant_month, bplant_rotyr)          &
     &  .gt.julday(lastoper(isr)%day,lastoper(isr)%mon,lastoper(isr)%yr)&
     &  ) then
            adj_plant_yr = bplant_rotyr - bmperod
        else
            adj_plant_yr = bplant_rotyr
        end if

        write(UNIT=luoseason(isr),FMT=2010,advance='NO')                &
     &    bplant_day, bplant_month, adj_plant_yr,                       &
     &   lastoper(isr)%day, lastoper(isr)%mon, lastoper(isr)%yr, bc0nam,&
     &    bprevstandstem, bprevstandleaf, bprevstandstore,              &
     &    bprevflatstem, bprevflatleaf, bprevflatstore,                 &
     &    bg_stem_sum, root_store_sum, root_fiber_sum,                  &
     &    bprevht, bprevstm, bprevrtd, bprevgrainf,                     &
     &    bcxstmrep, bprevcancov, bprevdayap, bprevchillucum,           &
     &    bprevhucum, bcthum, hui, bcdayam, bprevdayspring
      end if

      ! for annual crops, ALWAYS write out warning message
      ! if harvested before maturity
      if( (hui < 1.0) .and. (mature_warn_flg .gt. 0)                    &
     &    .and. ( (bc0idc.eq.1) .or. (bc0idc.eq.2)                      &
     &       .or. (bc0idc.eq.4) .or. (bc0idc.eq.5) ) ) then
         write(UNIT=6,FMT="(1x,3(a),i0,'/',i0,'/',i0,a,f5.1,a,a)")      &
     &       'Warning: ',                                               &
     &       bc0nam(1:len_trim(bc0nam)),                                &
     &       ' harvested ',                                             &
     &       dd, mm, yy,                                                &
     &       ' only reached ', hui*100.0, '% of maturity',              &
     &       ' (Check crop selection, planting, harvest dates)'
      end if

        ! updated every call to get newline in right place
        cprevseasonrotation(isr) = bmrotation

      end if

      return
    end subroutine crop_endseason

    subroutine cpout( isr )
!     Author : A. Retta - 11/19/96
!     + + + PURPOSE + + +
!     Prints headers for the CROP submodel output files

      use file_io_mod, only: luoseason, luocrop, luoshoot, luoinpt
      use crop_data_struct_defs, only: am0cfl

      integer, intent(in) :: isr   ! subregion number

!     + + + OUTPUT FORMATS + + +
 2131 format ('#                           stand   stand   stand   flat &
     &   flat    flat    root    root  bel.grnd  total   total')
 2132 format ('#daysim doy year dap heatui stem    leaf    store   stem &
     &   leaf    store   store   fiber   stem    leaf    stem    height &
     & #stem   lai     eff_lai rootd  grainf tempst watstf  frost  ffa  &
     &  ffw   par     apar     massinc    p_rw   p_st   p_lf   p_rp  std&
     &flt pdiam  parea  fpdiam fparea hu_del frzhrd sai repstmd crop')
 2133 format ('#                           kg/m^2  kg/m^2  kg/m^2  kg/m^&
     &2  kg/m^2  kg/m^2  kg/m^2  kg/m^2  kg/m^2  kg/m^2  kg/m^2  meters &
     &         m^2/m^2 m^2/m^2 meters                                   &
     &        MJ/m^2  MJ/m^2   kg/plnt                                  &
     &    meters m^2')

 2231 format ('#daysim doy year dap heatui ',                           &
     &        's_root_sum f_root_sum tot_mass_req end_shoot_mass ',     &
     &        'end_root_mass d_root_mass d_shoot_mass d_s_root_mass ',  &
     &        'end_stem_mass end_stem_area end_shoot_len bczshoot ',    &
     &        'bcmshoot bcdstm bc0nam')
 2232 format ('#(dy) (dy) (yr) (dy) (C)    ',                           &
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

    subroutine cdbug(isr, soil, crop, restot, h1et)

!     + + + PURPOSE + + +
!    This program prints out many of the global variables before
!    and after the call to CROP provide a comparison of values
!    which may be changed by CROP

!    author: John Tatarko
!    version: 09/01/92

!     + + + KEY WORDS + + +
!     wind, erosion, hydrology, tillage, soil, crop, decomposition
!     management

      use datetime_mod, only: get_simdate
      use file_io_mod, only: luocdb
      use soil_data_struct_defs, only: soil_def
      use biomaterial, only: biomatter, biototal
      use erosion_data_struct_defs, only: awadir, awhrmx, awudmx, awudmn
      use hydro_data_struct_defs, only: hydro_derived_et
      use crop_data_struct_defs, only: tisr, tday, tmo, tyr
      use climate_input_mod, only: cli_today

!     + + +   ARGUMENT DECLARATIONS + + +
      integer, intent(in) :: isr    ! subregion index
      type(soil_def), intent(in) :: soil  ! soil for this subregion
      type(biomatter), intent(in) :: crop    ! structure containing full crop description
      type(biototal), intent(in) :: restot   ! structure containing residue totals
      type(hydro_derived_et), intent(in) :: h1et

!     + + + GLOBAL COMMON BLOCKS + + +
      include 'p1werm.inc'
      include 'h1hydro.inc'
      include 'h1db1.inc'
      include 'h1temp.inc'

!     + + + LOCAL VARIABLES + + +

      integer cd, cm, cy
      integer        l

!     + + + LOCAL DEFINITIONS + + +

!   cd        - The current day of simulation month.
!   cm        - The current month of simulation year.
!   cy        - The current year of simulation run.
!   l         - This variable is an index on soil layers.

!     + + + SUBROUTINES CALLED + + +

!     + + + FUNCTIONS CALLED + + +

!     + + + UNIT NUMBERS FOR INPUT/OUTPUT DEVICES + + +
!     * = screen and keyboard
!    27 = debug CROP

!     + + + DATA INITIALIZATIONS + + +

      if (crop%growth%am0cif .eqv. .true.) then
          tday = -1
          tmo = -1
          tyr = -1
          tisr = -1
      end if
      call get_simdate (cd, cm, cy)

!     + + + INPUT FORMATS + + +

!     + + + OUTPUT FORMATS + + +
 2030 format ('**',1x,2(i2,'/'),i4,'    After  call to CROP         Subr&
     &egion No. ',i3)
 2031 format ('**',1x,2(i2,'/'),i4,'    Before call to CROP         Subr&
     &egion No. ',i3)
 2032 format (' awzdpt  awtdmx  awtdmn  aweirr  awudmx  awudmn ',       &
     &        ' awtdpt  awadir  awhrmx')
 2038 format (f7.2,9f8.2)
! 2045 format ('Subregion Number',i3)
 2050 format ('amrslp(',i2,') acftcv(',i2,') acrlai(',i2,')',           &
     &        ' aczrtd(',i2,') admftot(',i2,') ahfwsf(',i2,')',         &
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

!     + + + END SPECIFICATIONS + + +

!          write weather cligen and windgen variables
      if ((cd .eq. tday) .and. (cm .eq. tmo) .and. (cy .eq. tyr) .and.  &
     &   (isr .eq. tisr)) then
         write(luocdb(isr),2030) cd,cm,cy,isr
      else
         write(luocdb(isr),2031) cd,cm,cy,isr
      end if
      write(luocdb(isr),2032)
      write(luocdb(isr),2038) cli_today%zdpt, cli_today%tdmx, cli_today%tdmn, cli_today%eirr, awudmx, &
     &                        awudmn, cli_today%tdpt, awadir, awhrmx

!      write(luocdb(isr),2045) isr

      write(luocdb(isr),2050) isr, isr, isr, isr, isr, isr, isr
      write(luocdb(isr),2051) soil%amrslp, crop%deriv%ftcv, crop%deriv%rlai,    &
     &               crop%geometry%zrtd, restot%mftot, ahfwsf(isr), crop%bname
      write(luocdb(isr),2052) isr, isr, isr, isr
      write(luocdb(isr),2053)                                           &
     &               crop%database%tdtm, crop%growth%thucum, crop%deriv%mst, crop%deriv%mrt, &
     &               h1et%zeta, h1et%zetp, h1et%zpta
      write(luocdb(isr),2054) isr, isr, isr, isr
      write(luocdb(isr),2055) h1et%zea, h1et%zep, h1et%zptp, crop%database%tmin, &
     &               crop%database%topt, soil%aslrr
      write(luocdb(isr),2056)

      do 200 l = 1,soil%nslay
         write(luocdb(isr),2060) l,soil%aszlyt(l), soil%ahrsk(l), soil%ahrwc(l), &
     &                  soil%ahrwcs(l), soil%ahrwca(l), soil%ahrwcf(l), &
     &                  soil%ahrwcw(l), soil%ah0cb(l), soil%aheaep(l), &
     &                  ahtsmx(l,isr), ahtsmn(l,isr)
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
