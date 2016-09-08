!$Author$
!$Date$
!$Revision$
!$HeadURL$

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

!     + + + GLOBAL COMMON BLOCKS + + +
      include 'p1werm.inc'
      include 'c1report.inc'
      include 'm1flag.inc'

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

        if( init_loop ) then  !initilizing cycle
          ! attach a crop name and id as harvest/termination operation in stir report
          call stir_crop(isr, bc0nam, 2)
        end if

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
      end
