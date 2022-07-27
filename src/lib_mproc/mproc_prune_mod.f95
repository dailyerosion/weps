!$Author$
!$Date$
!$Revision$
!$HeadURL$

module mproc_prune_mod

  contains

    subroutine prune ( stemf, leaff, storef, rootstoref, rootfiberf, &
                       nslay, plant)

      ! This subroutine performs the biomass manipulation of pruning
      ! biomass. The amount of each component pruned is determined by
      ! the fraction passed into this subroutine for each component.
      ! Only the crop pool passed to routine is pruned first reproductive
      ! then leaf, then stem (branches), then roots below ground.
      ! Since this is for tree pruning, no consideration is given to how
      ! much reproductive, leaf or stem mass will be removed if supporting
      ! stem or root mass is pruned. This proportioning is the responsibilty
      ! of the user. All mass removed from the plant becomes flat residue, 
      ! or if underground, underground residue. Root pruning is applied
      ! equally to all layers.

      use biomaterial, only: plant_pointer, residue_pointer, residueAdd

      ! + + + ARGUMENT DECLARATIONS + + +
      real, intent(in) :: stemf       ! fraction of plant stems trimmed (kg/kg)
      real, intent(in) :: leaff       ! fraction of plant leaves trimmed (kg/kg)
      real, intent(in) :: storef      ! fraction of storage (reproductive components) trimmed (kg/kg)
      real, intent(in) :: rootstoref  ! fraction of plant storage root trimmed (kg/kg)
      real, intent(in) :: rootfiberf  ! fraction of plant fibrous root trimmed (kg/kg)
      integer, intent(in) :: nslay    ! number of soil layers
      type(plant_pointer), pointer :: plant     ! pointer to youngest plant data, which chains to older plant data

      ! + + + LOCAL VARIABLES + + +
      integer idx
      real :: temp_stem  ! temporary stem mass pool
      real :: temp_leaf  ! temporary leaf mass pool
      real :: temp_store ! temporary store mass pool
      real :: tot_froot  ! sum of fibrous root mass pruned
      real :: tot_sroot  ! sum of storage root mass pruned
      real :: temp_froot(nslay) ! temporary fibrous root mass pool
      real :: temp_sroot(nslay) ! temporary storage root mass pool
      logical :: store_flag = .false.
      logical :: leaf_flag = .false.
      logical :: stem_flag = .false.
      logical :: sroot_flag = .false.
      logical :: froot_flag = .false.
      type(plant_pointer), pointer :: thisPlant

      integer :: cntr

      ! + + + END SPECIFICATIONS + + +

      cntr = 0

      thisPlant => plant
      do while( associated(thisPlant) )
        ! this plant is allocated

        cntr = cntr + 1

        if( thisPlant%growth%living ) then

          ! this is a living plant
          temp_store = prune_pool(thisPlant%mass%standstore, storef, store_flag )
          temp_leaf = prune_pool(thisPlant%mass%standleaflive, leaff, leaf_flag ) &
                    + prune_pool(thisPlant%mass%standleafdead, leaff, leaf_flag )
          temp_stem = prune_pool(thisPlant%mass%standstem, stemf, stem_flag )

          tot_froot = 0.0
          tot_sroot = 0.0
          do idx = 1, nslay
            temp_froot(idx) = prune_pool(thisPlant%mass%rootfiberz(idx), rootfiberf, froot_flag )
            tot_froot = tot_froot + temp_froot(idx)
            temp_sroot(idx) = prune_pool(thisPlant%mass%rootstorez(idx), rootstoref, sroot_flag )
            tot_sroot = tot_froot + temp_sroot(idx)
          end do

          ! any mass pruned now becomes flat or below ground residue
          if( (temp_stem + temp_leaf + temp_store + tot_froot + tot_sroot) .gt. 0.0 ) then
            ! create new residue pool
            thisPlant%residue => residueAdd(thisPlant%residue, thisPlant%residueIndex, nslay)
            ! Add pruned mass to flat residue
            thisPlant%residue%flatstem = thisPlant%residue%flatstem + temp_stem
            thisPlant%residue%flatleaf = thisPlant%residue%flatleaf + temp_leaf
            thisPlant%residue%flatstore = thisPlant%residue%flatstore + temp_store

            do idx = 1, nslay
              thisPlant%residue%rootfiberz(idx) = thisPlant%residue%rootfiberz(idx) + temp_froot(idx)
              thisPlant%residue%rootstorez(idx) = thisPlant%residue%rootstorez(idx) + temp_sroot(idx)
            end do
          end if

          !this is the youngest growing plant. Only prune it.
          exit
        end if

        ! go to next older plant
        thisPlant => thisPlant%olderPlant
      end do

      return

    end subroutine prune

    function prune_pool( pool_mass, pool_frac, pool_flag ) result(mass_prune)

      ! + + + ARGUMENT DECLARATIONS + + +
      real, intent(inout) :: pool_mass
      real, intent(in) :: pool_frac
      logical, intent(inout) :: pool_flag
      real :: mass_prune

      mass_prune = pool_mass * pool_frac
      if( mass_prune.gt.0.0 ) then 
          pool_flag = .true.
          pool_mass = pool_mass - mass_prune
      end if

      return
    end function prune_pool

end module mproc_prune_mod
