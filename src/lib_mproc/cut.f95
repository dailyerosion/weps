!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine cut (                                                  &
     &           cutflg, cutht, grainf, cropf, standf,                  &
     &           bcmstandstem, bcmstandleaf, bcmstandstore,             &
     &           bcmflatstem, bcmflatleaf, bcmflatstore,                &
     &           bczht, bcgrainf, bchyfg,                               &
     &           btmstandstem, btmstandleaf, btmstandstore,             &
     &           btmflatstem, btmflatleaf, btmflatstore,                &
     &           btzht, btgrainf, residue,                              &
     &           tot_mass_rem, sel_mass_left)


!     + + + PURPOSE + + +
!     Process # 32 called from doproc.for

!     This subroutine performs the biomass manipulation of cutting
!     biomass. Any biomass that is cut is considered killed and moved
!     to the temporary pool to become residue. The component (either
!     crop or a biomass pool) removed is determined by flag which is
!     set before the call to this subroutine.

!     0  - cut height is measured from ground up
!     1  - cut height is measured from plant top down
!     2  - cut height is fraction of plant height from top down 
!          ie 0.7 means 70% of plant is cut off

!     Note that biomass for any of these pools that are cut is
!     either transferred to the coresponding flat pool or removed
!     depending on the three removal fraction values input

!     + + + KEYWORDS + + +
!     cut, transfer, biomass manipulation

      use weps_interface_defs
      use biomaterial, only: biomatter
      use p1unconv_mod, only: mmtom

!     + + + ARGUMENT DECLARATIONS + + +
      integer cutflg
      real    cutht, grainf, cropf, standf

      real bcmstandstem, bcmstandleaf, bcmstandstore
      real bcmflatstem, bcmflatleaf, bcmflatstore
      real bczht, bcgrainf
      integer bchyfg

      real btmstandstem, btmstandleaf, btmstandstore
      real btmflatstem, btmflatleaf, btmflatstore
      real btzht, btgrainf
      type(biomatter), dimension(:), intent(inout) :: residue

      real tot_mass_rem, sel_mass_left

!     + + + ARGUMENT DEFINITIONS + + +
!     cutflg    - cut height definition flag
!     cutht     - above ground height standing crop and/or
!                 residue is cut to (mm) or fraction

!     grainf    - fraction of cut grain mass removed from field
!     cropf     - fraction of cut growing crop mass removed from field
!                 (stems, leaves and any part of grain not removed above)
!     standf    - fraction of cut standing residue removed from field 
!                 (stems, leaves and any part of grain not removed above)

!     bcmstandstem - crop standing stem mass (kg/m^2)
!     bcmstandleaf - crop standing leaf mass (kg/m^2)
!     bcmstandstore - crop standing storage mass (kg/m^2)
!                    (head with seed, or vegetative head (cabbage, pineapple))

!     bcmflatstem  - crop flat stem mass (kg/m^2)
!     bcmflatleaf  - crop flat leaf mass (kg/m^2)
!     bcmflatstore - crop flat storage mass (kg/m^2)

!     bczht  - Crop height (m)
!     bcgrainf - internally computed grain fraction of reproductive mass
!     bchyfg - flag indicating the part of plant to apply the "grain fraction",
!              GRF, to when removing that plant part for yield
!         0     GRF applied to above ground storage (seeds, reproductive)
!         1     GRF times growth stage factor (see growth.for) applied to above ground storage (seeds, reproductive)
!         2     GRF applied to all aboveground biomass (forage)
!         3     GRF applied to leaf mass (tobacco)
!         4     GRF applied to stem mass (sugarcane)
!         5     GRF applied to below ground storage mass (potatoes, peanuts)

!     btmstandstem - temporary crop standing stem mass (kg/m^2)
!     btmstandleaf - temporary crop standing leaf mass (kg/m^2)
!     btmstandstore - temporarycrop standing storage mass (kg/m^2)
!                    (head with seed, or vegetative head (cabbage, pineapple))

!     btmflatstem  - temporary crop flat stem mass (kg/m^2)
!     btmflatleaf  - temporary crop flat leaf mass (kg/m^2)
!     btmflatstore - temporary crop flat storage mass (kg/m^2)

!     btzht  - Temporary Crop height (m)
!     btgrainf - internally computed grain fraction of reproductive mass
!     NOTE: harvestable yield flag for crop pool used also for temporary pool

!     residue - structure containing residue state variables to be modified
!     tot_mass_rem - total of all mass removed from the field
!     sel_mass_left - mass of material left in pools from which mass is removed
!                      by this harvest operation (kg/m^2)

!     + + + ACCESSED COMMON BLOCK VARIABLE DEFINITIONS + + +
!     mnbpls        - max number of decomposition pools (currently=3)

!     + + + PARAMETERS + + +

!     + + + LOCAL VARIABLES + + +
      integer  idy

!     + + + LOCAL VARIABLE DEFINITIONS + + +
!     idy      - loop variable for decomp pools (3 pools total)

!     + + + END SPECIFICATIONS + + +

!     assign crop grain fraction values to temporary pool since 
!     material may be transferred to the temporary pool without
!     a specific kill operation (really, how is that?)
      btgrainf = bcgrainf

!     convert cut height based on cutflg and also change mm to meters
!     in this conversion, make it always from the ground up, and using
!     the max of either crop or temporary crop pools to make sure a 
!     height greater than zero exists
      select case(cutflg)
      case(0)
          cutht = cutht*mmtom
      case(1)
          cutht = cutht*mmtom
          cutht = max(bczht,btzht) - cutht
          if(cutht.lt.0.0) cutht = 0.0
      case(2)
          cutht = (1.0-cutht) * max(bczht,btzht)
      case default
          write(*,*) 'Invalid cutht flag, nothing cut'
      end select

!***  print *, 'cut tflg: ', tflg
!***  print *, 'tflat before cutting: ', tflat
!***  print *, 'cutht/cstemht/tstemht: ', cutht,cstemht,tstemht

!!!!!!!!!!!!!!!!!!!!
!    For now, until the crop database can be updated to include some
!    indication of yield location, all yield will be available for removal
!    if the cut height gets at least the top quarter of the plant, otherwise
!    the amount will be linearly reduced until it is zero when cut height
!    equals crop height.

      tot_mass_rem = 0.0
      sel_mass_left = 0.0

      ! cut the living crop pool. Note that cut material left on the
      ! field ends up in as flat in the same pool. The transfer from crop
      ! to temporary is then done after all pool accounting is complete.
      ! this accomplishes the assumption that all cut becomes killed flat.
      call cut_pool (                                                   &
     &           cutht, grainf, cropf,                                  &
     &           bcmstandstem, bcmstandleaf, bcmstandstore,             &
     &           bczht, bcgrainf, bchyfg,                               &
     &           bcmflatstem, bcmflatleaf, bcmflatstore,                &
     &           tot_mass_rem, sel_mass_left )

      ! cut the temporary crop pool
      call cut_pool (                                                   &
     &           cutht, grainf, cropf,                                  &
     &           btmstandstem, btmstandleaf, btmstandstore,             &
     &           btzht, btgrainf, bchyfg,                               &
     &           btmflatstem, btmflatleaf, btmflatstore,                &
     &           tot_mass_rem, sel_mass_left )

      do idy = 1, size(residue)
          ! cut the individual decomposition crop pools. Note that standf
          ! is used instead of cropf, keeping plant material removal
          ! separate for living and dead crop. Grain is harvested out of both
          call cut_pool (                                               &
     &           cutht, grainf, standf,                                 &
     &           residue(idy)%mass%standstem, residue(idy)%mass%standleaf, residue(idy)%mass%standstore, &
     &           residue(idy)%geometry%zht, residue(idy)%geometry%grainf, residue(idy)%geometry%hyfg, &
     &           residue(idy)%mass%flatstem, residue(idy)%mass%flatleaf, residue(idy)%mass%flatstore, &
     &           tot_mass_rem, sel_mass_left )
      end do

      ! Transfer all crop flat material to temporary. This will end up in
      ! a decomp pool.
      btmflatstem = btmflatstem + bcmflatstem
      btmflatleaf = btmflatleaf + bcmflatleaf
      btmflatstore = btmflatstore + bcmflatstore
      bcmflatstem = 0.0
      bcmflatleaf = 0.0
      bcmflatstore = 0.0

      ! check that complete crop failure shows remaining biomass
      if( tot_mass_rem + sel_mass_left .le. 0.0 ) then
          sel_mass_left = bcmstandstem + bcmstandleaf + bcmstandstore   &
     &                  + btmstandstem + btmstandleaf + btmstandstore   &
     &                  + btmflatstem + btmflatleaf + btmflatstore
      end if

      return
      end

! ------------------------------------------------------------------------
      ! generalized routine for cutting biomass from each pool. Grain will
      ! now even be harvested from decomposition pools, so it is possible
      ! to kill a crop, transfer it to a decomposition pool, harvest
      ! the grain sucessfully, and get harvest index
      subroutine cut_pool (                                             &
     &           poolcutht, grainf, cropf,                                  &
     &           poolmstandstem, poolmstandleaf, poolmstandstore,       &
     &           poolzht, poolgrainf, poolhyfg,                         &
     &           poolmflatstem, poolmflatleaf, poolmflatstore,          &
     &           tot_mass_rem, sel_mass_left )

!     + + + ARGUMENT DECLARATIONS + + +
      real    poolcutht
      real    grainf
      real    cropf

      real    poolmstandstem
      real    poolmstandleaf
      real    poolmstandstore

      real    poolzht
      real    poolgrainf
      integer poolhyfg

      real    poolmflatstem
      real    poolmflatleaf
      real    poolmflatstore

      real    tot_mass_rem
      real    sel_mass_left

!     + + + LOCAL VARIABLES + + +
      integer pool_flag
      real    mass_cut
      real    mass_rem
      real    rem_frac

!     + + + LOCAL VARIABLE DEFINITIONS + + +
!     pool_flag - if mass is removed, set true, so remaining biomass is 
!                 summed so harvest index can be computed.
!     mass_cut - mass cut by cutting operation
!     mass_rem - mass removed from field by harvest operation
!     rem_frac - actual removal fraction calculated from combination of
!                grain fraction (GRF) and removal fraction

      pool_flag = 0
      if (poolcutht.lt.poolzht) then        ! cut crop pool
          ! above ground storage, reproductive fraction
          ! find amount cut 

          ! disabled partial storage fraction removal due to cutting too high
          ! we now get all the storage regardless of cut height.
!          if( poolcutht.gt.0.75*poolzht ) then
!              ! yield assumed uniformly distributed in top 25% of plant
!              mass_cut = poolmstandstore                                &
!     &                  * ((poolzht-poolcutht)/(0.25*poolzht))
!          else
!              mass_cut = poolmstandstore
!          end if

          ! we get all of the standing storage material regardless of cut height
          mass_cut = poolmstandstore

          poolmstandstore = poolmstandstore - mass_cut
          ! find amount removed
          rem_frac = grainf
          if( poolhyfg .le. 2 ) then
              rem_frac = rem_frac * poolgrainf
          end if
          mass_rem = mass_cut * rem_frac
          mass_cut = mass_cut - mass_rem
          ! cut crop material left on field placed in temporary pool
          poolmflatstore = poolmflatstore + mass_cut
          if( mass_rem.gt.0.0 ) then 
              pool_flag = 1
              tot_mass_rem = tot_mass_rem + mass_rem
          end if

          ! leaf fraction removal amounts
          ! find amount cut 
          if( poolcutht.gt.0.5*poolzht ) then
              ! leaves assumed uniformly distributed in top 50% of plant
              mass_cut = poolmstandleaf                                 &
     &                 * ((poolzht-poolcutht)/(0.5*poolzht))
          else
              mass_cut = poolmstandleaf
          end if
          poolmstandleaf = poolmstandleaf - mass_cut
          ! find amount removed
          rem_frac = cropf
          if( poolhyfg .eq. 3 ) then
              rem_frac = rem_frac * poolgrainf
          end if
          mass_rem = mass_cut * rem_frac
          mass_cut = mass_cut - mass_rem
          ! cut crop material left on field placed in temporary pool
          poolmflatleaf = poolmflatleaf + mass_cut
          if( mass_rem.gt.0.0 ) then 
              pool_flag = 1
              tot_mass_rem = tot_mass_rem + mass_rem
          end if

          ! stem fraction removal amounts
          ! find amount cut 
          mass_cut = poolmstandstem * (1.0 - (poolcutht/poolzht))
          poolmstandstem = poolmstandstem - mass_cut
          ! find amount removed
          rem_frac = cropf
          if( poolhyfg .eq. 4 ) then
              rem_frac = rem_frac * poolgrainf
          end if
          mass_rem = mass_cut * rem_frac
          mass_cut = mass_cut - mass_rem
          ! cut crop material left on field placed in temporary pool
          poolmflatstem = poolmflatstem + mass_cut
          if( mass_rem.gt.0.0 ) then 
              pool_flag = 1
              tot_mass_rem = tot_mass_rem + mass_rem
          end if

!         stem height
          poolzht = poolcutht
      endif

!     add biomass to selected mass if biomass was removed from pool
      if( pool_flag.eq.1 ) then
          sel_mass_left = sel_mass_left + poolmstandstore               &
     &                  + poolmstandleaf + poolmstandstem               &
     &                  + poolmflatstem + poolmflatleaf + poolmflatstore

      end if

      return
      end
