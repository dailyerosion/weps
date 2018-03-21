!$Author$
!$Date$
!$Revision$
!$HeadURL$

module mproc_remove_mod

  contains

    subroutine remove ( sel_position, sel_pool, bflg, &
                        stemf, leaff, storef, rootstoref, rootfiberf, &
                        nslay, plant, tot_mass_rem, sel_mass_left)

      ! This subroutine performs the biomass manipulation of removing
      ! biomass. The amount of each component removed is determined by
      ! the fraction passed into this subroutine for each component.
      ! Pools are changed in the order: first plant and residue, second
      ! plant and residue, ..., nth plant and residue and
      ! locations in the order: stand with roots, flat, below ground.
      ! Consideration is given that if root mass or stem mass is removed,
      ! then the leaves and storage portion must become flat in the same
      ! pool. Removal of stem mass also results in a reduction in stem
      ! count. In order to avoid double accounting, proportions of standing
      ! are tracked, but the final adjustment and movement is not done
      ! until all removals are completed in that pool.

      ! Possible future enhancements
      ! a)  bioflg - selects which age pools will be processed.  Probably the
      ! same definition as other biomass manipulation process effects use.
      ! b)  xxlocflg - selects the individual mass component pools that are
      ! being effected (material being removed in this case).  There would
      ! likely need to be more than one of these flags, possibly one for each
      ! "age" pool.  Example settings could be:

      ! crlocflg (st,yld,flt,bg,rt)	decomp1locflg (st,flt,bg,rt)

      ! bit             val                    bit              val
      ! x    st+yld+flt  0                      x    st+flt      0
      ! 0    yld*fract   1                      0    -           1
      ! 1    st*fract    2                      1    st*fract    2
      ! 2    fl*fract    4                      2    fl*fract    4
      ! 3    bg*fract    8                      3    bg*fract    8
      ! 4    rt*fract    16                     4    rt*fract    16
      ! 5    st*cutht    32                     5    st*cutht    32

      use biomaterial, only: plant_pointer, residue_pointer, residueAdd

      ! + + + ARGUMENT DECLARATIONS + + +
      integer :: sel_position ! position to which percentages will be applied
                              ! 0 - don't apply to anything
                              ! 1 - apply to standing (and attached roots)
                              ! 2 - apply to flat
                              ! 3 - apply to standing (and attached roots) and flat
                              ! 4 - apply to buried
                              ! 5 - apply to standing (and attached roots) and buried
                              ! 6 - apply to flat and buried
                              ! 7 - apply to standing (and attached roots), flat and buried
                              ! this corresponds to the bit pattern:
                              ! msb(buried, flat, standing)lsb
      integer :: sel_pool  ! pool to which percentages will be applied
                           ! 0 - don't apply to anything
                           ! 1 - apply to crop pool
                           ! 2 - apply to temporary pool
                           ! 3 - apply to crop and temporary pools
                           ! 4 - apply to residue pools
                           ! 5 - apply to crop and residue pools
                           ! 6 - apply to temporary and residue pools
                           ! 7 - apply to crop, temporary and residue pools
                           !     this corresponds to the bit pattern:
                           !     msb(residue, temporary, crop)lsb
      integer :: bflg   ! flag values (binary #'s actually)
                        ! bit no.                                    decimal value
                        ! x  - remove standing material in all pools      (0)
                        ! 0  - remove first plant (crop and residue)      (1)
                        ! 1  - remove second plant (crop and residue)     (2)
                        ! 2  - remove third plant (crop and residue)      (4)
                        ! 3  - remove fourth plant (crop and residue)     (8)
                        ! ....
                        ! n-1 - remove (n-1)th plant (crop and residue)  (2**n)

      real :: stemf       ! fraction of plant stems removed (kg/kg)
      real :: leaff       ! fraction of plant leaves removed (kg/kg)
      real :: storef      ! fraction of storage (reproductive components) removed (kg/kg)
      real :: rootstoref  ! fraction of plant storage root removed (kg/kg)
      real :: rootfiberf  ! fraction of plant fibrous root removed (kg/kg)
      integer :: nslay    ! number of soil layers
      type(plant_pointer), pointer :: plant     ! pointer to youngest plant data, which chains to older plant data
      real :: tot_mass_rem   ! mass of material removed by this harvest operation (kg/m^2)
      real :: sel_mass_left  ! mass of material left in pools from which mass is removed
                             ! by this harvest operation (kg/m^2)

      ! + + + LOCAL VARIABLES + + +
      real pool_temp1, pool_temp2
      real pool_temp1z(nslay), pool_temp2z(nslay), pool_temp3z(nslay)
      integer :: idx
      integer :: idy
      integer :: tflg    ! temporary flag to carry bioflag value if changes to all pools
      real :: temp_stem  ! temporary stem mass pool
      real :: temp_leaf  ! temporary leaf mass pool
      real :: temp_store ! temporary store mass pool

      real start_store, start_leaf, start_stem
      real start_rootstore(nslay), start_rootfiber(nslay)
      type(plant_pointer), pointer :: thisPlant
      type(residue_pointer), pointer :: thisResidue

      ! + + + LOCAL VARIABLE DEFINITIONS + + +
      ! pool_temp1 - used to substitute for non existent pools in crop,
      !              where there are no flatrootstore and flatrootfiber pools
      ! pool_temp2 -  see pool_temp1
      ! idx       - loop variable for soil layers
      ! idy       - loop variable for decomp pools

      ! + + + END SPECIFICATIONS + + +

      ! set tflg bits correctly for "all" pools if bflg=0
      if (bflg .eq. 0) then
        tflg = 0
        do idy = 0, (bit_size(tflg) - 1)
           tflg = ibset(tflg, idy)
        end do
      else
          tflg = bflg
      endif

      tot_mass_rem = 0.0
      sel_mass_left = 0.0

      pool_temp1 = 0.0
      pool_temp2 = 0.0
      do idx = 1, nslay
          pool_temp1z(idx) = 0.0
          pool_temp2z(idx) = 0.0
          pool_temp3z(idx) = 0.0
      end do

      ! begin with provided plant then loop to older plants
      thisPlant => plant
      do while( associated(thisPlant) )
        ! living plant pool
        if( BTEST(sel_pool,0) ) then
          ! standing and rooted biomass
          ! set starting values
          start_store = thisPlant%mass%standstore
          start_leaf = thisPlant%mass%standleaf
          start_stem = thisPlant%mass%standstem
          do idx = 1, nslay
            start_rootstore(idx) = thisPlant%mass%rootstorez(idx)
            start_rootfiber(idx) = thisPlant%mass%rootfiberz(idx)
          end do
          if( BTEST(sel_position,0) ) then
            call rem_stand_pool( &
            stemf, leaff, storef, rootstoref, rootfiberf, &
            thisPlant%mass%standstem, thisPlant%mass%standleaf, thisPlant%mass%standstore, &
            thisPlant%mass%rootstorez, thisPlant%mass%rootfiberz, &
            nslay, thisPlant%geometry%hyfg, thisPlant%geometry%grainf, thisPlant%geometry%dstm, &
            tot_mass_rem, sel_mass_left )
          end if
          ! flat biomass
          if( BTEST(sel_position,1) ) then
            call rem_flat_pool( &
            stemf, leaff, storef, rootstoref, rootfiberf, &
            thisPlant%mass%flatstem, thisPlant%mass%flatleaf, thisPlant%mass%flatstore, &
            pool_temp1, pool_temp2, &
            thisPlant%geometry%hyfg, thisPlant%geometry%grainf, tot_mass_rem, sel_mass_left )
          end if
          ! buried biomass
          if(             BTEST(sel_position,2) &
            .and. .not. BTEST(sel_position,0) ) then
            ! standing not done so root removal done here
            call rem_bg_pool( &
            stemf, leaff, storef, rootstoref, rootfiberf, &
            thisPlant%mass%stemz, pool_temp2z, pool_temp3z, &
            thisPlant%mass%rootstorez, thisPlant%mass%rootfiberz, &
            nslay, thisPlant%geometry%hyfg, thisPlant%geometry%grainf, tot_mass_rem, sel_mass_left )
          end if
          ! adjust standing pools if supporting stems or roots removed
          call adj_stand_pool( &
             start_stem, start_leaf, start_store, &
             start_rootstore, start_rootfiber, &
             thisPlant%mass%standstem, thisPlant%mass%standleaf, thisPlant%mass%standstore, &
             thisPlant%mass%rootstorez, thisPlant%mass%rootfiberz, &
             thisPlant%mass%flatstem, thisPlant%mass%flatleaf, thisPlant%mass%flatstore, &
             thisPlant%geometry%dstm, temp_stem, temp_leaf, temp_store, nslay)

          ! any standing mass moved to flat is now dead becoming residue
          if( (temp_stem + temp_leaf + temp_store) .gt. 0.0 ) then
            ! create new residue pool
            thisPlant%residue => residueAdd(thisPlant%residue, thisPlant%residueIndex, nslay)
            ! move mass from living to residue
            thisPlant%mass%flatstem = thisPlant%mass%flatstem - temp_stem
            thisPlant%mass%flatleaf = thisPlant%mass%flatleaf - temp_leaf
            thisPlant%mass%flatstore = thisPlant%mass%flatstore - temp_store
            thisPlant%residue%flatstem = temp_stem
            thisPlant%residue%flatleaf = temp_leaf
            thisPlant%residue%flatstore = temp_store
          end if

        end if

        ! residue pools
        idy = 0
        thisResidue => thisPlant%residue
        do while( associated(thisResidue) )
          idy = idy + 1
          if (BTEST(tflg,idy)) then
            ! flag indicates to remove biomass from this plant

            ! standing and rooted biomass
            ! set starting values
            start_store = thisResidue%standstore
            start_leaf = thisResidue%standleaf
            start_stem = thisResidue%standstem
            do idx = 1, nslay
              start_rootstore(idx) = thisResidue%rootstorez(idx)
              start_rootfiber(idx) = thisResidue%rootfiberz(idx)
            end do
            if( BTEST(sel_position,0) ) then
              call rem_stand_pool( &
              stemf, leaff, storef, rootstoref, rootfiberf, &
              thisResidue%standstem, thisResidue%standleaf, thisResidue%standstore, &
              thisResidue%rootstorez, thisResidue%rootfiberz, &
              nslay, thisPlant%geometry%hyfg, thisResidue%grainf, thisResidue%dstm, &
              tot_mass_rem, sel_mass_left )
            end if
            ! flat biomass
            if( BTEST(sel_position,1) ) then
              call rem_flat_pool( &
              stemf, leaff, storef, rootstoref, rootfiberf, &
              thisResidue%flatstem, thisResidue%flatleaf, thisResidue%flatstore, &
              thisResidue%flatrootstore, thisResidue%flatrootfiber, &
              thisPlant%geometry%hyfg, thisResidue%grainf, tot_mass_rem, sel_mass_left )
            end if
            ! buried biomass
            if( BTEST(sel_position,2) ) then
              if( BTEST(sel_position,0) ) then
                ! root removal already done in standing
                call rem_bg_pool( &
                stemf, leaff, storef, rootstoref, rootfiberf, &
                thisResidue%stemz, thisResidue%leafz, thisResidue%storez, &
                pool_temp1z, pool_temp2z, &
                nslay, thisPlant%geometry%hyfg, thisResidue%grainf, &
                tot_mass_rem, sel_mass_left )
              else
                ! standing not done so do root removal here
                call rem_bg_pool( &
                stemf, leaff, storef, rootstoref, rootfiberf, &
                thisResidue%stemz, thisResidue%leafz, thisResidue%storez, &
                thisResidue%rootstorez, thisResidue%rootfiberz, &
                nslay, thisPlant%geometry%hyfg, thisResidue%grainf, &
                tot_mass_rem, sel_mass_left )
              end if
            end if
            ! adjust standing pools if supporting stems or roots removed
            call adj_stand_pool( &
               start_stem, start_leaf, start_store, &
               start_rootstore, start_rootfiber, &
               thisResidue%standstem, thisResidue%standleaf, thisResidue%standstore, &
               thisResidue%rootstorez, thisResidue%rootfiberz, &
               thisResidue%flatstem, thisResidue%flatleaf, thisResidue%flatstore, &
               thisResidue%dstm, temp_stem, temp_leaf, temp_store, nslay)

          end if

          ! go to next older residue in thisPlant
          thisResidue => thisResidue%olderResidue
        end do

        ! go to next older plant
        thisPlant => thisPlant%olderPlant
      end do

      ! check that complete crop failure shows remaining biomass
      if( tot_mass_rem + sel_mass_left .le. 0.0 ) then
        if( associated(plant) ) then
          sel_mass_left = plant%mass%standstem + plant%mass%standleaf + plant%mass%standstore &
                        + plant%mass%flatstem + plant%mass%flatleaf + plant%mass%flatstore
        else
          sel_mass_left = 0.0
        end if
      end if

      return

    end subroutine remove

    subroutine rem_stand_pool( &
           stemf, leaff, storef, rootstoref, rootfiberf, &
           pool_stem, pool_leaf, pool_store, &
           pool_rootstore, pool_rootfiber, &
           nslay, pool_hyfg, pool_grainf, pool_dstm, &
           tot_mass_rem, sel_mass_left )
      
      ! + + + ARGUMENT DECLARATIONS + + +
      real stemf, leaff, storef, rootstoref, rootfiberf
      real pool_store, pool_leaf, pool_stem
      real pool_rootstore(*), pool_rootfiber(*)
      integer nslay, pool_hyfg
      real pool_grainf, pool_dstm, tot_mass_rem, sel_mass_left

      ! + + + LOCAL VARIABLES + + +
      integer idx, pool_flag
      real rem_frac

      pool_flag = 0

      rem_frac = storef
      if( pool_hyfg .le. 2 ) then
          rem_frac = rem_frac * pool_grainf
      end if
      call rem_pool(pool_store, rem_frac, pool_flag, tot_mass_rem)

      rem_frac = leaff
      if( pool_hyfg .eq. 3 ) then
          rem_frac = rem_frac * pool_grainf
      end if
      call rem_pool(pool_leaf, rem_frac, pool_flag, tot_mass_rem)

      rem_frac = stemf
      if( pool_hyfg .eq. 4 ) then
          rem_frac = rem_frac * pool_grainf
      end if
      call rem_pool(pool_stem, rem_frac, pool_flag, tot_mass_rem)
      ! also reduce stem count
      pool_dstm = pool_dstm * (1.0 - rem_frac)

      ! all above ground biomass remaining included in harvest index
      if( pool_flag .eq. 1 ) then
          sel_mass_left = sel_mass_left + pool_store + pool_leaf &
                        + pool_stem
          pool_flag = 0
      end if

      rem_frac = rootstoref
      if( pool_hyfg .eq. 5 ) then
          rem_frac = rem_frac * pool_grainf
      end if
      do idx = 1, nslay
          call rem_pool(pool_rootstore(idx), rem_frac, pool_flag, &
                        tot_mass_rem)
      end do

      ! If storage root harvested, then remaining mass included in harvest index
      if( pool_flag .eq. 1 ) then
          do idx = 1, nslay
              sel_mass_left = sel_mass_left + pool_rootstore(idx)
          end do
      end if

      rem_frac = rootfiberf
      do idx = 1, nslay
          call rem_pool(pool_rootfiber(idx), rem_frac, pool_flag, &
                        tot_mass_rem)
      end do

      return
    end subroutine rem_stand_pool


    subroutine rem_flat_pool( &
                 stemf, leaff, storef, rootstoref, rootfiberf, &
                 pool_stem, pool_leaf, pool_store, &
                 pool_rootstore, pool_rootfiber, &
                 pool_hyfg, pool_grainf, tot_mass_rem, sel_mass_left )

      ! + + + ARGUMENT DECLARATIONS + + +
      real stemf, leaff, storef, rootstoref, rootfiberf
      real pool_store, pool_leaf, pool_stem
      real pool_rootstore, pool_rootfiber
      integer pool_hyfg
      real pool_grainf, tot_mass_rem, sel_mass_left

      ! + + + LOCAL VARIABLES + + +
      integer pool_flag
      real rem_frac

      pool_flag = 0

      rem_frac = storef
      if( pool_hyfg .le. 2 ) then
          rem_frac = rem_frac * pool_grainf
      end if
      call rem_pool(pool_store, rem_frac, pool_flag, tot_mass_rem)

      rem_frac = leaff
      if( pool_hyfg .eq. 3 ) then
          rem_frac = rem_frac * pool_grainf
      end if
      call rem_pool(pool_leaf, rem_frac, pool_flag, tot_mass_rem)

      rem_frac = stemf
      if( pool_hyfg .eq. 4 ) then
          rem_frac = rem_frac * pool_grainf
      end if
      call rem_pool(pool_stem, rem_frac, pool_flag, tot_mass_rem)

      rem_frac = rootstoref
      if( pool_hyfg .eq. 5 ) then
          rem_frac = rem_frac * pool_grainf
      end if
      call rem_pool(pool_rootstore, rem_frac, pool_flag, tot_mass_rem)

      rem_frac = rootfiberf
      call rem_pool(pool_rootfiber, rem_frac, pool_flag, tot_mass_rem)

      ! all but fibrous root included in harvest index
      if( pool_flag .eq. 1 ) then
          sel_mass_left = sel_mass_left + pool_store + pool_leaf &
                        + pool_stem + pool_rootstore
      end if

      return
    end subroutine rem_flat_pool

    subroutine rem_bg_pool( &
           stemf, leaff, storef, rootstoref, rootfiberf, &
           pool_stem, pool_leaf, pool_store, &
           pool_rootstore, pool_rootfiber, &
           nslay, pool_hyfg, pool_grainf, tot_mass_rem, sel_mass_left )

      ! + + + ARGUMENT DECLARATIONS + + +
      real stemf, leaff, storef, rootstoref, rootfiberf
      real pool_store(*), pool_leaf(*), pool_stem(*)
      real pool_rootstore(*), pool_rootfiber(*)
      integer nslay, pool_hyfg
      real pool_grainf, tot_mass_rem, sel_mass_left

      ! + + + LOCAL VARIABLES + + +
      integer idx, pool_flag
      real rem_frac

      pool_flag = 0

      rem_frac = storef
      if( pool_hyfg .le. 2 ) then
          rem_frac = rem_frac * pool_grainf
      end if
      do idx = 1, nslay
          call rem_pool(pool_store(idx),rem_frac,pool_flag,tot_mass_rem)
      end do

      rem_frac = leaff
      if( pool_hyfg .eq. 3 ) then
          rem_frac = rem_frac * pool_grainf
      end if
      do idx = 1, nslay
          call rem_pool(pool_leaf(idx),rem_frac,pool_flag,tot_mass_rem)
      end do

      rem_frac = stemf
      if( pool_hyfg .eq. 4 ) then
          rem_frac = rem_frac * pool_grainf
      end if
      do idx = 1, nslay
          call rem_pool(pool_stem(idx),rem_frac,pool_flag,tot_mass_rem)
      end do

      rem_frac = rootstoref
      if( pool_hyfg .eq. 5 ) then
          rem_frac = rem_frac * pool_grainf
      end if
      do idx = 1, nslay
          call rem_pool(pool_rootstore(idx), rem_frac, pool_flag, &
                        tot_mass_rem)
      end do

      rem_frac = rootfiberf
      do idx = 1, nslay
          call rem_pool(pool_rootfiber(idx), rem_frac, pool_flag, &
                        tot_mass_rem)
      end do

      ! all but fibrous root included in harvest index
      if( pool_flag .eq. 1 ) then
          do idx = 1, nslay
              sel_mass_left = sel_mass_left + pool_store(idx)
              sel_mass_left = sel_mass_left + pool_leaf(idx)
              sel_mass_left = sel_mass_left + pool_stem(idx)
              sel_mass_left = sel_mass_left + pool_rootstore(idx)
          end do
      end if

      return
    end subroutine rem_bg_pool

    subroutine rem_pool(pool_mass, pool_frac, pool_flag, tot_mass_rem)

      ! + + + ARGUMENT DECLARATIONS + + +
      real pool_mass, pool_frac
      integer pool_flag
      real tot_mass_rem

      ! + + + LOCAL VARIABLES + + +
      real mass_rem

      mass_rem = pool_mass * pool_frac
      if( mass_rem.gt.0.0 ) then 
          pool_flag = 1
          pool_mass = pool_mass - mass_rem
          tot_mass_rem = tot_mass_rem + mass_rem
      end if

      return
    end subroutine rem_pool

    subroutine adj_stand_pool( &
           start_standstem, start_standleaf, start_standstore, &
           start_rootstore, start_rootfiber, &
           pool_standstem, pool_standleaf, pool_standstore, &
           pool_rootstore, pool_rootfiber, &
           pool_flatstem, pool_flatleaf, pool_flatstore, &
           pool_dstm, mov_stem, mov_leaf, mov_store, nslay)

      ! + + + PURPOSE + + +
      ! this subroutine checks to see if a greater proportion of roots
      ! (storage and fiber) have been removed than stems, and if so turns
      ! the now unsupported stems into flat biomass. The same check is
      ! then done for stems supoorting leaves and storage biomass.

      ! + + + ARGUMENT DECLARATIONS + + +
      real start_standstem, start_standleaf, start_standstore
      real start_rootstore(*), start_rootfiber(*)
      real pool_standstem, pool_standleaf, pool_standstore
      real pool_rootstore(*), pool_rootfiber(*)
      real pool_flatstem, pool_flatleaf, pool_flatstore
      real pool_dstm
      real mov_stem, mov_leaf, mov_store
      integer nslay

      ! + + + ARGUMENT DEFINITIONS + + +
      ! start_standstem - before biomass removal, crop standing stem mass (kg/m^2)
      ! start_standleaf - before biomass removal, crop standing leaf mass (kg/m^2)
      ! start_standstore - before biomass removal, crop standing storage mass (kg/m^2)
      !                (head with seed, or vegetative head (cabbage, pineapple))
      ! start_rootstorez - before biomass removal, crop root storage mass by soil layer (kg/m^2)
      !               (tubers (potatoes, carrots), extended leaf (onion), seeds (peanuts))
      ! start_rootfiberz - before biomass removal, crop root fibrous mass by soil layer (kg/m^2)

      ! pool_flatstem  - pool flat stem mass (kg/m^2)
      ! pool_flatleaf  - pool flat leaf mass (kg/m^2)
      ! pool_flatstore - pool flat storage mass (kg/m^2)

      ! pool_rootstore - pool flat root storage mass (kg/m^2)
      !               (tubers (potatoes, carrots), extended leaf (onion), seeds (peanuts))
      ! pool_rootfiber - pool flat root fibrous mass (kg/m^2)

      ! pool_flatstem  - pool flat stem mass (kg/m^2)
      ! pool_flatleaf  - pool flat leaf mass (kg/m^2)
      ! pool_flatstore - pool flat storage mass (kg/m^2)

      ! pool_dstm - Number of crop stems per unit area (#/m^2)
      !           - It is computed by taking the tillering factor
      !             times the plant population density.

      ! mov_stem - amount of stem biomass moved from standing to flat
      ! mov_leaf - amount of leaf biomass moved from standing to flat
      ! mov_store - amount of store biomass moved from standing to flat

      ! nslay - number of soil layers used

      ! + + + LOCAL VARIABLES + + +
      integer idx
      real rat_store, rat_leaf, rat_stem
      real rat_rootstore, rat_rootfiber
      real rat_root, mov_mass

      ! rat_root - the fraction of material remaining after removal

      ! adjust store, leaf and stem for rootstore or stem removal
      if( start_standstore .gt. 0.0 ) then
          rat_store = pool_standstore / start_standstore
      else
          rat_store = 1.0
      end if
      if( start_standleaf .gt. 0.0 ) then
          rat_leaf = pool_standleaf / start_standleaf
      else
          rat_leaf = 1.0
      end if
      if( start_standstem .gt. 0.0 ) then
          rat_stem = pool_standstem / start_standstem
      else
          rat_stem = 1.0
      end if
      rat_rootstore = 1.0
      do idx = 1, nslay
          if( start_rootstore(idx) .gt. 0.0 ) then
              rat_rootstore = min(rat_rootstore, &
                           pool_rootstore(idx) / start_rootstore(idx))
          end if
      end do
      rat_rootfiber = 1.0
      do idx = 1, nslay
          if( start_rootfiber(idx) .gt. 0.0 ) then
              rat_rootfiber = min(rat_rootfiber, &
                           pool_rootfiber(idx) / start_rootfiber(idx))
          end if
      end do
      ! check if supporting roots removed
      rat_root = min( rat_rootstore, rat_rootfiber )
      if( rat_root .lt. rat_stem ) then
          ! reduce stem count proportionally as well
          pool_dstm = pool_dstm * (rat_root/rat_stem)
          ! move standing mass
          mov_stem = pool_standstem * (1.0  - (rat_root/rat_stem))
          pool_flatstem = pool_flatstem + mov_stem
          pool_standstem = pool_standstem - mov_stem
          rat_stem = rat_root
      else
          mov_stem = 0.0
      end if
      ! check if supporting stems removed
      if( rat_stem .lt. rat_leaf ) then
          mov_leaf = pool_standleaf * (1.0  - (rat_stem/rat_leaf))
          pool_flatleaf = pool_flatleaf + mov_leaf
          pool_standleaf = pool_standleaf - mov_leaf
      else
          mov_leaf = 0.0
      end if
      if( rat_stem .lt. rat_store ) then
          mov_store = pool_standstore * (1.0  - (rat_stem/rat_store))
          pool_flatstore = pool_flatstore + mov_store
          pool_standstore = pool_standstore - mov_store
      else
          mov_store = 0.0
      end if

      return

    end subroutine adj_stand_pool

end module mproc_remove_mod
