!$Author$
!$Date$
!$Revision$
!$HeadURL$

module mproc_cut_mod

  contains

    subroutine cut ( cutflg, cutht, grainf, cropf, standf, nslay, plant, &
                     tot_mass_rem, sel_mass_left)

      ! This subroutine performs the biomass manipulation of cutting
      ! biomass. Any biomass that is cut is considered killed and moved
      ! to the temporary pool to become residue. The component (either
      ! crop or a biomass pool) removed is determined by flag which is
      ! set before the call to this subroutine.

      ! Note that biomass for any of these pools that are cut is
      ! either transferred to the coresponding flat pool or removed
      ! depending on the three removal fraction values input

      use p1unconv_mod, only: mmtom
      use biomaterial, only: plant_pointer, residue_pointer, residueAdd

      ! + + + ARGUMENT DECLARATIONS + + +
      integer, intent(in) :: cutflg ! cut height definition flag
                        ! 0  - cut height is measured from ground up
                        ! 1  - cut height is measured from plant top down
                        ! 2  - cut height is fraction of plant height from top down 
                        !      ie cutht = 0.7 means 70% of plant is cut off

      real, intent(in) :: cutht  ! above ground height standing crop and/or
                     ! residue is cut to (mm) or fraction

      real, intent(in) :: grainf ! fraction of cut grain mass removed from field
      real, intent(in) :: cropf  ! fraction of cut growing crop mass removed from field
                     ! (stems, leaves and any part of grain not removed above)
      real :: standf ! fraction of cut standing residue removed from field 
                     ! (stems, leaves and any part of grain not removed above)
      integer, intent(in) :: nslay  ! number of soil layers for this subregion
      type(plant_pointer), pointer :: plant     ! pointer to youngest plant data, which chains to older plant data

      real, intent(out) :: tot_mass_rem  ! total of all mass removed from the field
      real, intent(out) :: sel_mass_left ! mass of material left in pools from which mass is removed
                            ! by this harvest operation (kg/m^2)

      ! + + + ACCESSED COMMON BLOCK VARIABLE DEFINITIONS + + +
      ! mnbpls        - max number of decomposition pools (currently=3)

      ! + + + LOCAL VARIABLES + + +
      real :: mod_cutht  ! cut height modified locally for units and sense (from top or ground)
      real :: mass_zht   ! standing mass weighted height for all plants (and residue)
      real :: mass_sum   ! standing mass sum for all plants (and residue)
      real :: standmass  ! standing mass components
      type(plant_pointer), pointer :: thisPlant
      type(residue_pointer), pointer :: thisResidue
      real :: tflatstem  ! flat stem mass returned from cut
      real :: tflatleaf  ! flat leaf mass returned from cut
      real :: tflatstore ! flat storage mass returned from cut

      ! + + + END SPECIFICATIONS + + +

      ! find mass weighted height of biomass in all plants
      mass_zht = 0.0
      mass_sum = 0.0
      ! check all plants for height (including all residue)
      thisPlant => plant
      do while( associated(thisPlant) )
        ! standing mass
        standmass = thisPlant%mass%standstem + thisPlant%mass%standleaf + thisPlant%mass%standstore
        ! mass time living height
        mass_zht = mass_zht + standmass * thisPlant%geometry%zht
        ! standing mass sum
        mass_sum = mass_sum + standmass
        ! check all residue pools for this plant
        thisResidue => thisPlant%residue
        do while( associated(thisResidue) )
          ! standing mass
          standmass = thisResidue%standstem + thisResidue%standleaf + thisResidue%standstore
          ! residue height
          mass_zht = mass_zht + standmass * thisResidue%zht
          ! standing mass sum
          mass_sum = mass_sum + standmass
          ! go to next older residue in thisPlant
          thisResidue => thisResidue%olderResidue
        end do
        ! go to next older plant
        thisPlant => thisPlant%olderPlant
      end do
      ! mass weighted height
      mass_zht = mass_zht / mass_sum
      if( mass_zht .le. 0.0) then
        ! nothing to cut
        return
      end if

      ! convert cut height based on cutflg and also change mm to meters
      ! in this conversion, make it always from the ground up, and using
      ! the max of either living plant or residue pools to make sure a 
      ! height greater than zero exists
      select case(cutflg)
      case(0)
          mod_cutht = cutht*mmtom
      case(1)
          mod_cutht = cutht*mmtom
          mod_cutht = mass_zht - mod_cutht
          if(mod_cutht.lt.0.0) mod_cutht = 0.0
      case(2)
          mod_cutht = (1.0-cutht) * mass_zht
      case default
          write(*,*) 'Invalid cutht flag, nothing cut'
          return
      end select

     ! NOTE: PRESENTLY COMMENTED OUT. GETS ALL YIELD REGARDLESS!!
     ! For now, until the crop database can be updated to include some
     ! indication of yield location, all yield will be available for removal
     ! if the cut height gets at least the top quarter of the plant, otherwise
     ! the amount will be linearly reduced until it is zero when cut height
     ! equals crop height.

      tot_mass_rem = 0.0
      sel_mass_left = 0.0

      ! begin with provided plant then loop to older plants
      thisPlant => plant
      do while( associated(thisPlant) )

        ! cut the living crop pool. Note that cut material left on the
        ! field ends up as flat in a new residue pool. The transfer from living 
        ! to residue is done upon return from cut_pool. The assumption is 
        ! that all cut not removed becomes killed flat.
        tflatstem = 0.0
        tflatleaf = 0.0
        tflatstore = 0.0
        call cut_pool ( mod_cutht, grainf, cropf, &
                 thisPlant%mass%standstem, thisPlant%mass%standleaf, thisPlant%mass%standstore, &
                 thisPlant%geometry%zht, thisPlant%geometry%grainf, thisPlant%geometry%hyfg, &
                 tflatstem, tflatleaf, tflatstore, &
                 tot_mass_rem, sel_mass_left )

        if( (tflatstem + tflatleaf + tflatstore) > 0.0 ) then
          ! add residue pool for cut material left behind (iniitalizes all variables)
          thisPlant%residue => residueAdd( thisPlant%residue, thisplant%residueIndex, nslay )
          thisResidue => thisPlant%residue
          thisResidue%flatstem = tflatstem
          thisResidue%flatleaf = tflatleaf
          thisResidue%flatstore = tflatstore
          ! store affected, set residue grain fraction from plant value 
          thisResidue%grainf = thisPlant%geometry%grainf
          ! nothing to cut in this pool, move to next
          thisResidue => thisResidue%olderResidue
        else
          ! cut first residue pool
          thisResidue => thisPlant%residue
        end if

        do while( associated(thisResidue) )
          ! cut the individual residue pools. Note that standf
          ! is used instead of cropf, keeping plant material removal
          ! separate for living and dead crop. Grain is harvested out of both.
          ! Cut material left behind is added to flat mass pools.
          call cut_pool ( mod_cutht, grainf, standf, &
                thisResidue%standstem, thisResidue%standleaf, thisResidue%standstore, &
                thisResidue%zht, thisResidue%grainf, thisPlant%geometry%hyfg, &
                thisResidue%flatstem, thisResidue%flatleaf, thisResidue%flatstore, &
                tot_mass_rem, sel_mass_left )

          ! go to next older residue in thisPlant
          thisResidue => thisResidue%olderResidue

        end do

        ! go to next older plant
        thisPlant => thisPlant%olderPlant
      end do

      ! check that complete crop failure shows remaining biomass
      if( tot_mass_rem + sel_mass_left .le. 0.0 ) then
        if( associated(plant) ) then
          sel_mass_left = plant%mass%standstem + plant%mass%standleaf + plant%mass%standstore   &
                        + plant%mass%flatstem + plant%mass%flatleaf + plant%mass%flatstore
        else
          sel_mass_left = 0.0
        end if
      end if

      return

    end subroutine cut

    ! generalized routine for cutting biomass from each pool. Grain will
    ! now even be harvested from decomposition pools, so it is possible
    ! to kill a crop, transfer it to a decomposition pool, harvest
    ! the grain sucessfully, and get harvest index
    subroutine cut_pool ( poolcutht, grainf, cropf, &
                 poolmstandstem, poolmstandleaf, poolmstandstore, &
                 poolzht, poolgrainf, poolhyfg, &
                 poolmflatstem, poolmflatleaf, poolmflatstore, &
                 tot_mass_rem, sel_mass_left )

      ! + + + ARGUMENT DECLARATIONS + + +
      real, intent(in) :: poolcutht
      real, intent(in) :: grainf
      real, intent(in) :: cropf

      real, intent(inout) :: poolmstandstem
      real, intent(inout) :: poolmstandleaf
      real, intent(inout) :: poolmstandstore

      real, intent(inout) :: poolzht
      real, intent(in) :: poolgrainf
      integer, intent(in) :: poolhyfg

      real, intent(inout) :: poolmflatstem
      real, intent(inout) :: poolmflatleaf
      real, intent(inout) :: poolmflatstore

      real, intent(inout) :: tot_mass_rem
      real, intent(inout) :: sel_mass_left

      ! + + + LOCAL VARIABLES + + +
      integer :: pool_flag  ! if mass is removed, set true, so remaining biomass is 
                            ! summed so harvest index can be computed.
      real :: mass_cut   ! mass cut by cutting operation
      real :: mass_rem   ! mass removed from field by harvest operation
      real :: rem_frac   ! actual removal fraction calculated from combination of
                         ! grain fraction (GRF) and removal fraction

      pool_flag = 0
      if (poolcutht.lt.poolzht) then        ! cut crop pool
          ! above ground storage, reproductive fraction
          ! find amount cut 

          ! disabled partial storage fraction removal due to cutting too high
          ! we now get all the storage regardless of cut height.
      !      if( poolcutht.gt.0.75*poolzht ) then
      !          ! yield assumed uniformly distributed in top 25% of plant
      !          mass_cut = poolmstandstore                                &
      ! &                  * ((poolzht-poolcutht)/(0.25*poolzht))
      !      else
      !          mass_cut = poolmstandstore
      !      end if

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
              mass_cut = poolmstandleaf * ((poolzht-poolcutht)/(0.5*poolzht))
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

          ! stem height
          poolzht = poolcutht
      endif

      ! add biomass to selected mass if biomass was removed from pool
      if( pool_flag.eq.1 ) then
          sel_mass_left = sel_mass_left + poolmstandstore &
                        + poolmstandleaf + poolmstandstem &
                        + poolmflatstem + poolmflatleaf + poolmflatstore
      end if

      return
    end subroutine cut_pool

end module mproc_cut_mod
