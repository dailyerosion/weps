!$Author$
!$Date$
!$Revision$
!$HeadURL$

module mproc_thin_mod

  contains

    subroutine thin ( thinflg, thinval, grainf, cropf, standf, nslay, plant, &
                     tot_mass_rem, sel_mass_left)

      ! This subroutine performs thinning on the plant that is passed, not the
      ! whole plant chain. The component (either plant or it's residue) removed
      ! is determined by flag which is set before the call to this
      ! subroutine.

      ! Note that thinned biomass not removed for living plant is put into
      ! a flat residue pool. Any thinned residue that is not removed is
      ! transferred to the corresponding flat pool.

      use biomaterial, only: plant_pointer, residue_pointer, residueAdd

      ! + + + ARGUMENT DECLARATIONS + + +
      integer, intent(in) :: thinflg ! thinning definition flag
                                     ! 0  - Remove fraction of Plants, thinval = fraction
                                     ! 1  - Thin to Plant Population, thinval = population
      real, intent(in) :: thinval  ! fraction of plants removed or final population
      real, intent(in) :: grainf   ! fraction of thinned grain mass removed from field
      real, intent(in) :: cropf    ! fraction of thinned growing crop mass removed from field
                                   ! (stems, leaves and any part of grain not removed above)
      real, intent(in) :: standf   ! fraction of thinned standing residue removed from field 
                                   ! (stems, leaves and any part of grain not removed above)
      integer, intent(in) :: nslay      ! total number of layers in soil
      type(plant_pointer), pointer :: plant     ! pointer to youngest plant data, which chains to older plant data

      real, intent(out) :: tot_mass_rem  ! total of all mass removed from the field
      real, intent(out) :: sel_mass_left ! mass of material left in pools from which mass is removed
                            ! by this harvest operation (kg/m^2)

      ! + + + ACCESSED COMMON BLOCK VARIABLE DEFINITIONS + + +
      ! mnbpls        - max number of decomposition pools (currently=3)

      ! + + + LOCAL VARIABLES + + +
      real :: mod_thinval  ! thinval modified locally for units and sense (fraction or population)
      type(residue_pointer), pointer :: thisResidue
      real :: tflatstem  ! flat stem mass returned from thin
      real :: tflatleaf  ! flat leaf mass returned from thin
      real :: tflatstore ! flat storage mass returned from thin

      ! + + + END SPECIFICATIONS + + +

      if( associated(plant) ) then
        ! a plant exists, continue

        ! convert thinning value for all cases to fraction of plant
        ! population to remain
        select case(thinflg)
        case(0)
          mod_thinval = 1.0-thinval
        case(1)
          if( plant%geometry%dstm .gt. 0.0 ) then
              mod_thinval = min(1.0, thinval/plant%geometry%dstm)
          else
              mod_thinval = 0.0
          end if
        case default
          write(*,*) 'Invalid thinning flag, nothing thinned'
        end select

        tot_mass_rem = 0.0
        sel_mass_left = 0.0

        ! thinning live plants. Note that thinned material left on the
        ! field ends up as flat in a new residue pool. The transfer from living 
        ! to residue is done upon return from thin_pool. The assumption is 
        ! that all thinnings not removed become killed flat.
        tflatstem = 0.0
        tflatleaf = 0.0
        tflatstore = 0.0
        call thin_pool_stand_plant ( mod_thinval, grainf, cropf, &
           plant%mass%standstem, plant%mass%standleaflive, &
           plant%mass%standleafdead, plant%mass%standstore, &
           tflatstem, tflatleaf, tflatstore, &
           plant%geometry%grainf, plant%geometry%hyfg, tot_mass_rem, sel_mass_left)

        ! if living crop flat pool has biomass, also transfer the correct
        ! proportions into temporary flat pool
        call thin_pool_residue ( mod_thinval, grainf, cropf, &
           plant%mass%flatstem, plant%mass%flatleaf, plant%mass%flatstore, &
           tflatstem, tflatleaf, tflatstore, &
           plant%geometry%grainf, plant%geometry%hyfg, tot_mass_rem, sel_mass_left)

        ! modify stem count to reflect change
        plant%geometry%dstm = plant%geometry%dstm * mod_thinval

        if( (tflatstem + tflatleaf + tflatstore) > 0.0 ) then
          ! add residue pool for thinned material left behind
          plant%residue => residueAdd( plant%residue, plant%residueIndex, nslay )
          thisResidue => plant%residue
          thisResidue%flatstem = tflatstem
          thisResidue%flatleaf = tflatleaf
          thisResidue%flatstore = tflatstore
          ! store affected, set residue grain fraction from plant value 
          thisResidue%grainf = plant%geometry%grainf
          ! nothing to cut in this pool, move to next
          thisResidue => thisResidue%olderResidue
        else
          ! thin first residue pool
          thisResidue => plant%residue
        end if

        do while( associated(thisResidue) )
          ! thin residue decomposition pools (standing to flat if not removed)
          call thin_pool_residue ( mod_thinval, grainf, standf, &
             thisResidue%standstem, thisResidue%standleaf, thisResidue%standstore,&
             thisResidue%flatstem, thisResidue%flatleaf, thisResidue%flatstore,   &
             thisResidue%grainf, plant%geometry%hyfg, tot_mass_rem, sel_mass_left)

          ! modify stem count to reflect change
          thisResidue%dstm = thisResidue%dstm * mod_thinval

          ! go to next older residue in thisPlant
          thisResidue => thisResidue%olderResidue

        end do

      end if

      ! check that complete crop failure shows remaining biomass
      if( tot_mass_rem + sel_mass_left .le. 0.0 ) then
        if( associated(plant) ) then
          sel_mass_left = plant%mass%standstem + plant%mass%standleaflive &
                        + plant%mass%standleafdead + plant%mass%standstore   &
                        + plant%mass%flatstem + plant%mass%flatleaf + plant%mass%flatstore
        else
          sel_mass_left = 0.0
        end if
      end if

      return

    end subroutine thin

      ! local subroutine to apply thinning fractions and biomass removal 
      ! fractions to each pool. Stem number reduction is done outside the
      ! the subroutine to allow the flat mass pool for a living crop to be
      ! handled the same as a standing pool for thinning purposes.

    pure subroutine thin_pool_stand_plant ( thinval, grainf, cropf, &
                poolmstandstem, poolmstandleaflive, poolmstandleafdead, poolmstandstore, &
                poolmflatstem, poolmflatleaf, poolmflatstore, &
                poolgrainf, poolhyfg, tot_mass_rem, sel_mass_left)

      ! + + + ARGUMENT DECLARATIONS + + +
      real, intent(in) :: thinval
      real, intent(in) :: grainf
      real, intent(in) :: cropf

      real, intent(inout) :: poolmstandstem
      real, intent(inout) :: poolmstandleaflive
      real, intent(inout) :: poolmstandleafdead
      real, intent(inout) :: poolmstandstore

      real, intent(inout) :: poolmflatstem
      real, intent(inout) :: poolmflatleaf
      real, intent(inout) :: poolmflatstore

      integer, intent(in) :: poolhyfg
      real, intent(in) :: poolgrainf
      real, intent(inout) :: tot_mass_rem
      real, intent(inout) :: sel_mass_left

      ! + + + LOCAL VARIABLE DEFINITIONS + + +
      integer pool_flag
      real mass_thin
      real mass_rem
      real rem_frac

      ! + + + END SPECIFICATIONS + + +

      pool_flag = 0

      ! yield mass
      mass_thin = poolmstandstore * (1.0-thinval)
      rem_frac = grainf
      if( poolhyfg .le. 2 ) then
          rem_frac = rem_frac * poolgrainf
      end if
      mass_rem = mass_thin * rem_frac
      if( mass_rem.gt.0.0 ) then 
          pool_flag = 1
          tot_mass_rem = tot_mass_rem + mass_rem
      end if
      poolmstandstore = poolmstandstore - mass_thin
      poolmflatstore = poolmflatstore + mass_thin - mass_rem

      ! live standing leaf mass
      mass_thin = poolmstandleaflive * (1.0-thinval) 
      rem_frac = cropf
      if( poolhyfg .eq. 3 ) then
          rem_frac = rem_frac * poolgrainf
      end if
      mass_rem = mass_thin * rem_frac
      if( mass_rem.gt.0.0 ) then 
          pool_flag = 1
          tot_mass_rem = tot_mass_rem + mass_rem
      end if
      poolmstandleaflive = poolmstandleaflive - mass_thin
      poolmflatleaf = poolmflatleaf + mass_thin - mass_rem

      ! dead standing leaf mass
      mass_thin = poolmstandleafdead * (1.0-thinval) 
      rem_frac = cropf
      if( poolhyfg .eq. 3 ) then
          rem_frac = rem_frac * poolgrainf
      end if
      mass_rem = mass_thin * rem_frac
      if( mass_rem.gt.0.0 ) then 
          pool_flag = 1
          tot_mass_rem = tot_mass_rem + mass_rem
      end if
      poolmstandleafdead = poolmstandleafdead - mass_thin
      poolmflatleaf = poolmflatleaf + mass_thin - mass_rem

      ! standing stem mass
      mass_thin = poolmstandstem * (1.0-thinval) 
      rem_frac = cropf
      if( poolhyfg .eq. 4 ) then
          rem_frac = rem_frac * poolgrainf
      end if
      mass_rem = mass_thin * rem_frac
      if( mass_rem.gt.0.0 ) then 
          pool_flag = 1
          tot_mass_rem = tot_mass_rem + mass_rem
      end if
      poolmstandstem = poolmstandstem - mass_thin
      poolmflatstem = poolmflatstem + mass_thin - mass_rem

      ! add biomass to selected mass if biomass was removed from pool
      if( pool_flag.eq.1 ) then
          sel_mass_left = sel_mass_left + poolmstandstem + poolmstandleaflive &
                        + poolmstandleafdead + poolmstandstore
      end if

      return
    end subroutine thin_pool_stand_plant

    pure subroutine thin_pool_residue ( thinval, grainf, cropf, &
                poolmstandstem, poolmstandleaf, poolmstandstore, &
                poolmflatstem, poolmflatleaf, poolmflatstore, &
                poolgrainf, poolhyfg, tot_mass_rem, sel_mass_left)

      ! + + + ARGUMENT DECLARATIONS + + +
      real, intent(in) :: thinval
      real, intent(in) :: grainf
      real, intent(in) :: cropf

      real, intent(inout) :: poolmstandstem
      real, intent(inout) :: poolmstandleaf
      real, intent(inout) :: poolmstandstore

      real, intent(inout) :: poolmflatstem
      real, intent(inout) :: poolmflatleaf
      real, intent(inout) :: poolmflatstore

      integer, intent(in) :: poolhyfg
      real, intent(in) :: poolgrainf
      real, intent(inout) :: tot_mass_rem
      real, intent(inout) :: sel_mass_left

      ! + + + LOCAL VARIABLE DEFINITIONS + + +
      integer pool_flag
      real mass_thin
      real mass_rem
      real rem_frac

      ! + + + END SPECIFICATIONS + + +

      pool_flag = 0

      ! yield mass
      mass_thin = poolmstandstore * (1.0-thinval)
      rem_frac = grainf
      if( poolhyfg .le. 2 ) then
          rem_frac = rem_frac * poolgrainf
      end if
      mass_rem = mass_thin * rem_frac
      if( mass_rem.gt.0.0 ) then 
          pool_flag = 1
          tot_mass_rem = tot_mass_rem + mass_rem
      end if
      poolmstandstore = poolmstandstore - mass_thin
      poolmflatstore = poolmflatstore + mass_thin - mass_rem

      ! standing leaf mass
      mass_thin = poolmstandleaf * (1.0-thinval) 
      rem_frac = cropf
      if( poolhyfg .eq. 3 ) then
          rem_frac = rem_frac * poolgrainf
      end if
      mass_rem = mass_thin * rem_frac
      if( mass_rem.gt.0.0 ) then 
          pool_flag = 1
          tot_mass_rem = tot_mass_rem + mass_rem
      end if
      poolmstandleaf = poolmstandleaf - mass_thin
      poolmflatleaf = poolmflatleaf + mass_thin - mass_rem

      ! standing stem mass
      mass_thin = poolmstandstem * (1.0-thinval) 
      rem_frac = cropf
      if( poolhyfg .eq. 4 ) then
          rem_frac = rem_frac * poolgrainf
      end if
      mass_rem = mass_thin * rem_frac
      if( mass_rem.gt.0.0 ) then 
          pool_flag = 1
          tot_mass_rem = tot_mass_rem + mass_rem
      end if
      poolmstandstem = poolmstandstem - mass_thin
      poolmflatstem = poolmflatstem + mass_thin - mass_rem

      ! add biomass to selected mass if biomass was removed from pool
      if( pool_flag.eq.1 ) then
          sel_mass_left = sel_mass_left + poolmstandstem                  &
     &                  + poolmstandleaf + poolmstandstore
      end if

      return
    end subroutine thin_pool_residue

end module mproc_thin_mod
