!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine crop_endseason ( bc0nam, bm0cfl,                       &
     &                 bnslay, bc0idc, bcdayam,                         &
     &                 bcthum, bcxstmrep,                               &
     &                 bprevstandstem, bprevstandleaf, bprevstandstore, &
     &                 bprevflatstem, bprevflatleaf, bprevflatstore,    &
     &                 bprevbgstemz,                                    &
     &                 bprevrootstorez, bprevrootfiberz,                &
     &                 bprevht, bprevstm, bprevrtd,                     &
     &                 bprevdayap, bprevhucum, bprevrthucum,            &
     &                 bprevgrainf, bprevchillucum, bprevliveleaf,      &
     &                 bprevdayspring, mature_warn_flg )

!     + + + PURPOSE + + +
!     Prints out crop status variables that are of interest at the end of the season

!     + + + KEYWORDS + + +
!     crop model status

!     + + + ARGUMENT DECLARATIONS + + +
      character*(80) bc0nam
      integer bm0cfl, bnslay, bc0idc, bcdayam
      real bcthum, bcxstmrep
      real bprevstandstem, bprevstandleaf, bprevstandstore
      real bprevflatstem, bprevflatleaf, bprevflatstore
      real bprevbgstemz(*)
      real bprevrootstorez(*), bprevrootfiberz(*)
      real bprevht, bprevstm, bprevrtd
      integer bprevdayap
      real bprevhucum, bprevrthucum
      real bprevgrainf, bprevchillucum, bprevliveleaf
      integer bprevdayspring, mature_warn_flg

!     + + + ARGUMENT DEFINITIONS + + +

!     bc0nam - crop name
!     bnslay - number of soil layers
!     bc0idc - crop type:annual,perennial,etc
!     bcdayam - number of days since crop matured
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
!     bprevdayspring - day of year in which a winter annual releases stored growth
!     mature_warn_flg - flag to indicate use of crop maturity warning
!                0  - no crop maturity warning given for any crop
!                1  - Warnings generated for any crop unless supressed by crop type

!     + + + GLOBAL COMMON BLOCKS + + +
      include 'file.inc'

!     + + + LOCAL VARIABLES + + +
      integer lay, dd, mm, yy
      real hui
      real bg_stem_sum, root_store_sum, root_fiber_sum

!     + + + LOCAL VARIABLE DEFINITIONS + + +
!     dd,mm,yy - the current day, month, and year
!     bg_stem_sum - sum of below ground stem
!     root_store_sum - sum of root storage
!     root_fiber_sum - sum of root fiber 

!     + + + OUTPUT FORMATS + + +
 2013 format(1x,i4,13(1x,f7.3),1x,f7.5,1x,i4,3(1x,f6.1),1x,f5.3,1x,i4,  &
     &       1x,i6,1x,a40)

!     + + + END OF SPECIFICATIONS + + +

!     day of year
      call caldatw(dd, mm, yy)

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

        write(luoseason,2013) yy,                                       &
     &    bprevstandstem, bprevstandleaf, bprevstandstore,              &
     &    bprevflatstem, bprevflatleaf, bprevflatstore,                 &
     &    bg_stem_sum, root_store_sum, root_fiber_sum,                  &
     &    bprevht, bprevstm, bprevrtd, bprevgrainf,                     &
     &    bcxstmrep, bprevdayap, bprevchillucum, bprevhucum, bcthum,    &
     &    hui, bcdayam, bprevdayspring, bc0nam
      end if

      ! for annual crops, ALWAYS write out warning message
      ! if harvested before maturity
      if( (hui < 1.0) .and. (mature_warn_flg .gt. 0)                    &
     &    .and. ( (bc0idc.eq.1) .or. (bc0idc.eq.2)                      &
     &       .or. (bc0idc.eq.4) .or. (bc0idc.eq.5) ) ) then
         write(UNIT=6,FMT="(1x,3(a),i2,'/',i2,'/',i2,a,f5.1,a,a)")      &
     &       'Warning: ',                                               &
     &       bc0nam(1:len_trim(bc0nam)),                                &
     &       ' harvested ',                                             &
     &       dd, mm, yy,                                                &
     &       ' only reached ', hui*100.0, '% of maturity',              &
     &       ' (Check crop selection, planting, harvest dates)'
      end if

      return
      end
