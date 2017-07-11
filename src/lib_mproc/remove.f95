!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine remove (                                               &
     &           sel_position, sel_pool, bflg,                          &
     &           stemf, leaff, storef, rootstoref, rootfiberf,          &
     &           bcmstandstem, bcmstandleaf, bcmstandstore,             &
     &           bcmflatstem, bcmflatleaf, bcmflatstore,                &
     &           bcmrootstorez, bcmrootfiberz,                          &
     &           bcmbgstemz,                                            &
     &           bczht, bcdstm, bcgrainf, bchyfg,                       &
     &           btmstandstem, btmstandleaf, btmstandstore,             &
     &           btmflatstem, btmflatleaf, btmflatstore,                &
     &           btmflatrootstore, btmflatrootfiber,                    &
     &           btmbgstemz, btmbgleafz, btmbgstorez,                   &
     &           btmbgrootstorez, btmbgrootfiberz,                      &
     &           btzht, btdstm, btgrainf, residue,                      &
     &           nslay, tot_mass_rem, sel_mass_left)

!     + + + PURPOSE + + +
!     This subroutine performs the biomass manipulation of removing
!     biomass. The amount of each component removed is determined by
!     the fraction passed into this subroutine for each component.
!     Pools are changed in the order: crop, temporary, residue and 
!     locations in the order: stand with roots, flat, below ground.
!     Consideration is given that if root mass or stem mass is removed,
!     then the leaves and storage portion must become flat in the same
!     pool. Removal of stem mass also results in a reduction in stem
!     count. In order to avoid double accounting, proportions of standing
!     are tracked, but the final adjustment and movement is not done
!     until all removals are completed in that pool.

!     Possible future enhancements
!     a)  bioflg - selects which age pools will be processed.  Probably the
!     same definition as other biomass manipulation process effects use.
!     b)  xxlocflg - selects the individual mass component pools that are
!     being effected (material being removed in this case).  There would
!     likely need to be more than one of these flags, possibly one for each
!     "age" pool.  Example settings could be:
!   
!     crlocflg (st,yld,flt,bg,rt)	decomp1locflg (st,flt,bg,rt)
!   
!     bit             val                    bit              val
!     x    st+yld+flt  0                      x    st+flt      0
!     0    yld*fract   1                      0    -           1
!     1    st*fract    2                      1    st*fract    2
!     2    fl*fract    4                      2    fl*fract    4
!     3    bg*fract    8                      3    bg*fract    8
!     4    rt*fract    16                     4    rt*fract    16
!     5    st*cutht    32                     5    st*cutht    32

!     + + + KEYWORDS + + +
!     remove, biomass manipulation

      use weps_interface_defs, ignore_me=>remove
      use biomaterial, only: biomatter

!     + + + ARGUMENT DECLARATIONS + + +
      integer sel_position, sel_pool, bflg
      real stemf, leaff, storef, rootstoref, rootfiberf
      real bcmstandstem, bcmstandleaf, bcmstandstore
      real bcmflatstem, bcmflatleaf, bcmflatstore
      real bcmrootstorez(*), bcmrootfiberz(*)
      real bczht, bcdstm, bcgrainf
      integer bchyfg

      real btmstandstem, btmstandleaf, btmstandstore
      real btmflatstem, btmflatleaf, btmflatstore
      real btmflatrootstore, btmflatrootfiber
      real btmbgstemz(*), btmbgleafz(*), btmbgstorez(*)
      real btmbgrootstorez(*), btmbgrootfiberz(*)
      real bcmbgstemz(*)

      real btzht, btdstm, btgrainf
      type(biomatter), dimension(:), intent(inout) :: residue

      integer nslay
      real   tot_mass_rem, sel_mass_left

!     + + + ARGUMENT DEFINITIONS + + +
!     sel_position - position to which percentages will be applied
!                0 - don't apply to anything
!                1 - apply to standing (and attached roots)
!                2 - apply to flat
!                3 - apply to standing (and attached roots) and flat
!                4 - apply to buried
!                5 - apply to standing (and attached roots) and buried
!                6 - apply to flat and buried
!                7 - apply to standing (and attached roots), flat and buried
!                this corresponds to the bit pattern:
!                msb(buried, flat, standing)lsb
                
!     sel_pool - pool to which percentages will be applied
!            0 - don't apply to anything
!            1 - apply to crop pool
!            2 - apply to temporary pool
!            3 - apply to crop and temporary pools
!            4 - apply to residue pools
!            5 - apply to crop and residue pools
!            6 - apply to temporary and residue pools
!            7 - apply to crop, temporary and residue pools
!                this corresponds to the bit pattern:
!                msb(residue, temporary, crop)lsb

!     bflg      - flag indicating what to manipulate
!       0 - All standing material is manipulated (both crop and residue)
!       1 - Crop
!       2 - 1'st residue pool
!       4 - 2'nd residue pool
!       ....
!       2**n - nth residue pool

!     storef   - fraction of storage (reproductive components) removed (kg/kg)
!     leaff    - fraction of plant leaves removed (kg/kg)
!     stemf    - fraction of plant stems removed (kg/kg)
!     rootstoref - fraction of plant storage root removed (kg/kg)
!     rootfiberf - fraction of plant fibrous root removed (kg/kg)

!     bcmstandstem - crop standing stem mass (kg/m^2)
!     bcmstandleaf - crop standing leaf mass (kg/m^2)
!     bcmstandstore - crop standing storage mass (kg/m^2)
!                    (head with seed, or vegetative head (cabbage, pineapple))

!     bcmflatstem  - crop flat stem mass (kg/m^2)
!     bcmflatleaf  - crop flat leaf mass (kg/m^2)
!     bcmflatstore - crop flat storage mass (kg/m^2)

!     bcmrootstorez - crop root storage mass by soil layer (kg/m^2)
!                   (tubers (potatoes, carrots), extended leaf (onion), seeds (peanuts))
!     bcmrootfiberz - crop root fibrous mass by soil layer (kg/m^2)

!     bcmbgstemz  - crop buried stem mass by layer (kg/m^2)

!     bczht  - Crop height (m)
!     bcdstm - Number of crop stems per unit area (#/m^2)
!            - It is computed by taking the tillering factor
!              times the plant population density.
!     bcgrainf - internally computed grain fraction of reproductive mass

!     btmstandstem - crop standing stem mass (kg/m^2)
!     btmstandleaf - crop standing leaf mass (kg/m^2)
!     btmstandstore - crop standing storage mass (kg/m^2)
!                    (head with seed, or vegetative head (cabbage, pineapple))

!     btmflatstem  - crop flat stem mass (kg/m^2)
!     btmflatleaf  - crop flat leaf mass (kg/m^2)
!     btmflatstore - crop flat storage mass (kg/m^2)

!     btmflatrootstore - crop flat root storage mass (kg/m^2)
!                   (tubers (potatoes, carrots), extended leaf (onion), seeds (peanuts))
!     btmflatrootfiber - crop flat root fibrous mass (kg/m^2)

!     btmbgflatstemz  - crop buried stem mass by layer (kg/m^2)
!     btmbgflatleafz  - crop buried leaf mass by layer (kg/m^2)
!     btmbgflatstorez - crop buried storage mass by layer (kg/m^2)

!     btmbgrootstorez - crop root storage mass by layer (kg/m^2)
!                   (tubers (potatoes, carrots), extended leaf (onion), seeds (peanuts))
!     btmbgrootfiberz - crop root fibrous mass by layer (kg/m^2)

!     btzht  - Crop height (m)
!     btdstm - Number of crop stems per unit area (#/m^2)
!            - It is computed by taking the tillering factor
!              times the plant population density.
!     btgrainf - internally computed grain fraction of reproductive mass
!     residue - structure containing residue state variables to be modified

!     nlay      - number of layer from which below ground biomass is removed
!     tot_mass_rem - mass of material removed by this harvest operation (kg/m^2)
!     sel_mass_left - mass of material left in pools from which mass is removed
!                     by this harvest operation (kg/m^2)

!     + + + LOCAL VARIABLES + + +
      real pool_temp1, pool_temp2
      real pool_temp1z(nslay), pool_temp2z(nslay), pool_temp3z(nslay)
      integer idx, idy, tflg

      real start_store, start_leaf, start_stem
      real start_rootstore(nslay), start_rootfiber(nslay)
      integer :: npools

!     + + + LOCAL VARIABLE DEFINITIONS + + +
!     pool_temp1 - used to substitute for non existent pools in crop,
!                  where there are no flatrootstore and flatrootfiber pools
!     pool_temp2 -  see pool_temp1
!     idx       - loop variable for soil layers
!     idy       - loop variable for decomp pools
!     tflg      - temporary flag to carry bioflag value if changes to all pools

!     + + + END SPECIFICATIONS + + +

      npools = size(residue)

      !set tflg bits correctly for "all" pools if bflg=0
      if (bflg .eq. 0) then
         tflg = 1                   ! crop pool
         do idy = 1, npools
            tflg = tflg + 2**idy    ! decomp pools
         end do
      else
        tflg = bflg
      endif

      tot_mass_rem = 0.0
      sel_mass_left = 0.0

!     assign crop grain fraction and representaive stem diameter values
!     to temporary pool since material may be transferred to the temporary
!     pool without a specific kill operation
      btgrainf = bcgrainf

      pool_temp1 = 0.0
      pool_temp2 = 0.0
      do idx = 1, nslay
          pool_temp1z(idx) = 0.0
          pool_temp2z(idx) = 0.0
          pool_temp3z(idx) = 0.0
      end do

      ! crop pool
      if( BTEST(sel_pool,0) ) then
          ! standing and rooted biomass
          ! set starting values
          start_store = bcmstandstore
          start_leaf = bcmstandleaf
          start_stem = bcmstandstem
          do idx = 1, nslay
              start_rootstore(idx) = bcmrootstorez(idx)
              start_rootfiber(idx) = bcmrootfiberz(idx)
          end do
          if( BTEST(sel_position,0) ) then
              call rem_stand_pool(                                      &
     &        stemf, leaff, storef, rootstoref, rootfiberf,             &
     &        bcmstandstem, bcmstandleaf, bcmstandstore,                &
     &        bcmrootstorez, bcmrootfiberz,                             &
     &        nslay, bchyfg, bcgrainf, bcdstm,                          &
     &        tot_mass_rem, sel_mass_left )
          end if
          ! flat biomass
          if( BTEST(sel_position,1) ) then
              call rem_flat_pool(                                       &
     &        stemf, leaff, storef, rootstoref, rootfiberf,             &
     &        bcmflatstem, bcmflatleaf, bcmflatstore,                   &
     &        pool_temp1, pool_temp2,                                   &
     &        bchyfg, bcgrainf, tot_mass_rem, sel_mass_left )
          end if
          ! buried biomass
          if(             BTEST(sel_position,2)                         &
     &        .and. .not. BTEST(sel_position,0) ) then
              ! standing not done so root removal done here
              call rem_bg_pool(                                         &
     &        stemf, leaff, storef, rootstoref, rootfiberf,             &
     &        bcmbgstemz, pool_temp2z, pool_temp3z,                     &
     &        bcmrootstorez, bcmrootfiberz,                             &
     &        nslay, bchyfg, bcgrainf, tot_mass_rem, sel_mass_left )
          end if
          ! adjust standing pools if supporting stems or roots removed
          call adj_stand_pool(                                          &
     &         start_stem, start_leaf, start_store,                     &
     &         start_rootstore, start_rootfiber,                        &
     &         bcmstandstem, bcmstandleaf, bcmstandstore,               &
     &         bcmrootstorez, bcmrootfiberz,                            &
     &         bcmflatstem, bcmflatleaf, bcmflatstore,                  &
     &         bcdstm, nslay)
      end if
      ! temporary pool
      if( BTEST(sel_pool,1) ) then
          ! standing and rooted biomass
          ! set starting values
          start_store = btmstandstore
          start_leaf = btmstandleaf
          start_stem = btmstandstem
          do idx = 1, nslay
              start_rootstore(idx) = btmbgrootstorez(idx)
              start_rootfiber(idx) = btmbgrootfiberz(idx)
          end do
          if( BTEST(sel_position,0) ) then
              call rem_stand_pool(                                      &
     &        stemf, leaff, storef, rootstoref, rootfiberf,             &
     &        btmstandstem, btmstandleaf, btmstandstore,                &
     &        btmbgrootstorez, btmbgrootfiberz,                         &
     &        nslay, bchyfg, btgrainf, btdstm,                          &
     &        tot_mass_rem, sel_mass_left )
          end if
          ! flat biomass
          if( BTEST(sel_position,1) ) then
              call rem_flat_pool(                                       &
     &        stemf, leaff, storef, rootstoref, rootfiberf,             &
     &        btmflatstem, btmflatleaf, btmflatstore,                   &
     &        btmflatrootstore, btmflatrootfiber,                       &
     &        bchyfg, btgrainf, tot_mass_rem, sel_mass_left )
          end if
          ! buried biomass
          if( BTEST(sel_position,2) ) then
              if( BTEST(sel_position,0) ) then
                  ! root removal already done in standing
                  call rem_bg_pool(                                     &
     &            stemf, leaff, storef, rootstoref, rootfiberf,         &
     &            btmbgstemz, btmbgleafz, btmbgstorez,                  &
     &            pool_temp1z, pool_temp2z,                             &
     &            nslay, bchyfg, btgrainf, tot_mass_rem, sel_mass_left )
              else
                  ! standing not done so do root removal here
                  call rem_bg_pool(                                     &
     &            stemf, leaff, storef, rootstoref, rootfiberf,         &
     &            btmbgstemz, btmbgleafz, btmbgstorez,                  &
     &            btmbgrootstorez, btmbgrootfiberz,                     &
     &            nslay, bchyfg, btgrainf, tot_mass_rem, sel_mass_left )
              end if
          end if
          ! adjust standing pools if supporting stems or roots removed
          call adj_stand_pool(                                          &
     &         start_stem, start_leaf, start_store,                     &
     &         start_rootstore, start_rootfiber,                        &
     &         btmstandstem, btmstandleaf, btmstandstore,               &
     &         btmbgrootstorez, btmbgrootfiberz,                        &
     &         btmflatstem, btmflatleaf, btmflatstore,                  &
     &         btdstm, nslay)
      end if
      ! residue pools
      if( BTEST(sel_pool,2) ) then
        do idy = 1, npools
         if (BTEST(tflg,idy)) then
          ! standing and rooted biomass
          ! set starting values
          start_store = residue(idy)%mass%standstore
          start_leaf = residue(idy)%mass%standleaf
          start_stem = residue(idy)%mass%standstem
          do idx = 1, nslay
              start_rootstore(idx) = residue(idy)%mass%rootstorez(idx)
              start_rootfiber(idx) = residue(idy)%mass%rootfiberz(idx)
          end do
          if( BTEST(sel_position,0) ) then
              call rem_stand_pool(                                      &
     &        stemf, leaff, storef, rootstoref, rootfiberf,             &
     &        residue(idy)%mass%standstem, residue(idy)%mass%standleaf, residue(idy)%mass%standstore, &
     &        residue(idy)%mass%rootstorez, residue(idy)%mass%rootfiberz, &
     &        nslay, residue(idy)%geometry%hyfg, residue(idy)%geometry%grainf, residue(idy)%geometry%dstm, &
     &        tot_mass_rem, sel_mass_left )
          end if
          ! flat biomass
          if( BTEST(sel_position,1) ) then
              call rem_flat_pool(                                       &
     &        stemf, leaff, storef, rootstoref, rootfiberf,             &
     &        residue(idy)%mass%flatstem, residue(idy)%mass%flatleaf, residue(idy)%mass%flatstore, &
     &        residue(idy)%mass%flatrootstore, residue(idy)%mass%flatrootfiber, &
     &        residue(idy)%geometry%hyfg, residue(idy)%geometry%grainf, tot_mass_rem, sel_mass_left )
          end if
          ! buried biomass
          if( BTEST(sel_position,2) ) then
            if( BTEST(sel_position,0) ) then
              ! root removal already done in standing
              call rem_bg_pool(                                         &
     &        stemf, leaff, storef, rootstoref, rootfiberf,             &
     &        residue(idy)%mass%stemz, residue(idy)%mass%leafz, residue(idy)%mass%storez, &
     &        pool_temp1z, pool_temp2z,                                 &
     &        nslay, residue(idy)%geometry%hyfg, residue(idy)%geometry%grainf, &
     &        tot_mass_rem, sel_mass_left )
            else
              ! standing not done so do root removal here
              call rem_bg_pool(                                         &
     &        stemf, leaff, storef, rootstoref, rootfiberf,             &
     &        residue(idy)%mass%stemz, residue(idy)%mass%leafz, residue(idy)%mass%storez, &
     &        residue(idy)%mass%rootstorez, residue(idy)%mass%rootfiberz, &
     &        nslay, residue(idy)%geometry%hyfg, residue(idy)%geometry%grainf, &
     &        tot_mass_rem, sel_mass_left )
            end if
          end if
          ! adjust standing pools if supporting stems or roots removed
          call adj_stand_pool(                                          &
     &         start_stem, start_leaf, start_store,                     &
     &         start_rootstore, start_rootfiber,                        &
     &         residue(idy)%mass%standstem, residue(idy)%mass%standleaf, residue(idy)%mass%standstore, &
     &         residue(idy)%mass%rootstorez, residue(idy)%mass%rootfiberz, &
     &         residue(idy)%mass%flatstem, residue(idy)%mass%flatleaf, residue(idy)%mass%flatstore, &
     &         residue(idy)%geometry%dstm, nslay)
         end if
        end do  
      end if

      ! check that complete crop failure shows remaining biomass
      if( tot_mass_rem + sel_mass_left .le. 0.0 ) then
          sel_mass_left = bcmstandstem + bcmstandleaf + bcmstandstore   &
     &                  + btmstandstem + btmstandleaf + btmstandstore   &
     &                  + btmflatstem + btmflatleaf + btmflatstore
      end if

      return
      end

! -----------------------------------------------------------------
      subroutine rem_stand_pool(                                        &
     &      stemf, leaff, storef, rootstoref, rootfiberf,               &
     &      pool_stem, pool_leaf, pool_store,                           &
     &      pool_rootstore, pool_rootfiber,                             &
     &      nslay, pool_hyfg, pool_grainf, pool_dstm,                   &
     &      tot_mass_rem, sel_mass_left )
      
!     + + + ARGUMENT DECLARATIONS + + +
      real stemf, leaff, storef, rootstoref, rootfiberf
      real pool_store, pool_leaf, pool_stem
      real pool_rootstore(*), pool_rootfiber(*)
      integer nslay, pool_hyfg
      real pool_grainf, pool_dstm, tot_mass_rem, sel_mass_left

!     + + + LOCAL VARIABLES + + +
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
          sel_mass_left = sel_mass_left + pool_store + pool_leaf        &
     &                  + pool_stem
          pool_flag = 0
      end if

      rem_frac = rootstoref
      if( pool_hyfg .eq. 5 ) then
          rem_frac = rem_frac * pool_grainf
      end if
      do idx = 1, nslay
          call rem_pool(pool_rootstore(idx), rem_frac, pool_flag,       &
     &                  tot_mass_rem)
      end do

      ! If storage root harvested, then remaining mass included in harvest index
      if( pool_flag .eq. 1 ) then
          do idx = 1, nslay
              sel_mass_left = sel_mass_left + pool_rootstore(idx)
          end do
      end if

      rem_frac = rootfiberf
      do idx = 1, nslay
          call rem_pool(pool_rootfiber(idx), rem_frac, pool_flag,       &
     &                  tot_mass_rem)
      end do

      return
      end


! -----------------------------------------------------------------
      subroutine rem_flat_pool(                                         &
     &           stemf, leaff, storef, rootstoref, rootfiberf,          &
     &           pool_stem, pool_leaf, pool_store,                      &
     &           pool_rootstore, pool_rootfiber,                        &
     &           pool_hyfg, pool_grainf, tot_mass_rem, sel_mass_left )

!     + + + ARGUMENT DECLARATIONS + + +
      real stemf, leaff, storef, rootstoref, rootfiberf
      real pool_store, pool_leaf, pool_stem
      real pool_rootstore, pool_rootfiber
      integer pool_hyfg
      real pool_grainf, tot_mass_rem, sel_mass_left

!     + + + LOCAL VARIABLES + + +
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
          sel_mass_left = sel_mass_left + pool_store + pool_leaf        &
     &                  + pool_stem + pool_rootstore
      end if

      return
      end


! -----------------------------------------------------------------
      subroutine rem_bg_pool(                                           &
     &      stemf, leaff, storef, rootstoref, rootfiberf,               &
     &      pool_stem, pool_leaf, pool_store,                           &
     &      pool_rootstore, pool_rootfiber,                             &
     &      nslay, pool_hyfg, pool_grainf, tot_mass_rem, sel_mass_left )

!     + + + ARGUMENT DECLARATIONS + + +
      real stemf, leaff, storef, rootstoref, rootfiberf
      real pool_store(*), pool_leaf(*), pool_stem(*)
      real pool_rootstore(*), pool_rootfiber(*)
      integer nslay, pool_hyfg
      real pool_grainf, tot_mass_rem, sel_mass_left

!     + + + LOCAL VARIABLES + + +
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
          call rem_pool(pool_rootstore(idx), rem_frac, pool_flag,       &
     &                  tot_mass_rem)
      end do

      rem_frac = rootfiberf
      do idx = 1, nslay
          call rem_pool(pool_rootfiber(idx), rem_frac, pool_flag,       &
     &                  tot_mass_rem)
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
      end

! -----------------------------------------------------------------
      subroutine rem_pool(pool_mass, pool_frac, pool_flag, tot_mass_rem)

!     + + + ARGUMENT DECLARATIONS + + +
      real pool_mass, pool_frac
      integer pool_flag
      real tot_mass_rem

!     + + + LOCAL VARIABLES + + +
      real mass_rem

      mass_rem = pool_mass * pool_frac
      if( mass_rem.gt.0.0 ) then 
          pool_flag = 1
          pool_mass = pool_mass - mass_rem
          tot_mass_rem = tot_mass_rem + mass_rem
      end if

      return
      end


! -----------------------------------------------------------------
      subroutine adj_stand_pool(                                        &
     &      start_standstem, start_standleaf, start_standstore,         &
     &      start_rootstore, start_rootfiber,                           &
     &      pool_standstem, pool_standleaf, pool_standstore,            &
     &      pool_rootstore, pool_rootfiber,                             &
     &      pool_flatstem, pool_flatleaf, pool_flatstore,               &
     &      pool_dstm, nslay)

!     + + + PURPOSE + + +
!     this subroutine checks to see if a greater proportion of roots
!    (storage and fiber) have been removed than stems, and if so turns
!     the now unsupoorted stems into flat biomass. The same check is
!     then done for stems supoorting leaves and storage biomass.

!     + + + ARGUMENT DECLARATIONS + + +
      real start_standstem, start_standleaf, start_standstore
      real start_rootstore(*), start_rootfiber(*)
      real pool_standstem, pool_standleaf, pool_standstore
      real pool_rootstore(*), pool_rootfiber(*)
      real pool_flatstem, pool_flatleaf, pool_flatstore
      real pool_dstm
      integer nslay

!     + + + ARGUMENT DEFINITIONS + + +
!     start_standstem - before biomass removal, crop standing stem mass (kg/m^2)
!     start_standleaf - before biomass removal, crop standing leaf mass (kg/m^2)
!     start_standstore - before biomass removal, crop standing storage mass (kg/m^2)
!                    (head with seed, or vegetative head (cabbage, pineapple))
!     start_rootstorez - before biomass removal, crop root storage mass by soil layer (kg/m^2)
!                   (tubers (potatoes, carrots), extended leaf (onion), seeds (peanuts))
!     start_rootfiberz - before biomass removal, crop root fibrous mass by soil layer (kg/m^2)

!     pool_flatstem  - pool flat stem mass (kg/m^2)
!     pool_flatleaf  - pool flat leaf mass (kg/m^2)
!     pool_flatstore - pool flat storage mass (kg/m^2)

!     pool_rootstore - pool flat root storage mass (kg/m^2)
!                   (tubers (potatoes, carrots), extended leaf (onion), seeds (peanuts))
!     pool_rootfiber - pool flat root fibrous mass (kg/m^2)

!     pool_flatstem  - pool flat stem mass (kg/m^2)
!     pool_flatleaf  - pool flat leaf mass (kg/m^2)
!     pool_flatstore - pool flat storage mass (kg/m^2)

!     pool_dstm - Number of crop stems per unit area (#/m^2)
!               - It is computed by taking the tillering factor
!                 times the plant population density.

!     nslay - number of soil layers used

!     + + + LOCAL VARIABLES + + +
      integer idx
      real rat_store, rat_leaf, rat_stem
      real rat_rootstore, rat_rootfiber
      real rat_root, mov_mass

!     rat_root - the fraction of material remaining after removal

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
              rat_rootstore = min(rat_rootstore,                        &
     &                     pool_rootstore(idx) / start_rootstore(idx))
          end if
      end do
      rat_rootfiber = 1.0
      do idx = 1, nslay
          if( start_rootfiber(idx) .gt. 0.0 ) then
              rat_rootfiber = min(rat_rootfiber,                        &
     &                     pool_rootfiber(idx) / start_rootfiber(idx))
          end if
      end do
      ! check if supporting roots removed
      rat_root = min( rat_rootstore, rat_rootfiber )
      if( rat_root .lt. rat_stem ) then
          ! reduce stem count proportionally as well
          pool_dstm = pool_dstm * (rat_root/rat_stem)
          ! move standing mass
          mov_mass = pool_standstem * (1.0  - (rat_root/rat_stem))
          pool_flatstem = pool_flatstem + mov_mass
          pool_standstem = pool_standstem - mov_mass
          rat_stem = rat_root
      end if
      ! check if supporting stems removed
      if( rat_stem .lt. rat_leaf ) then
          mov_mass = pool_standleaf * (1.0  - (rat_stem/rat_leaf))
          pool_flatleaf = pool_flatleaf + mov_mass
          pool_standleaf = pool_standleaf - mov_mass
      end if
      if( rat_stem .lt. rat_store ) then
          mov_mass = pool_standstore * (1.0  - (rat_stem/rat_store))
          pool_flatstore = pool_flatstore + mov_mass
          pool_standstore = pool_standstore - mov_mass
      end if

      return
      end
