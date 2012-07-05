!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine thin (                                                 &
     &           thinflg, thinval, grainf, cropf, standf,               &
     &           bcmstandstem, bcmstandleaf, bcmstandstore,             &
     &           bcmflatstem, bcmflatleaf, bcmflatstore,                &
     &           bcdstm, bcgrainf, bchyfg,                              &
     &           btmstandstem, btmstandleaf, btmstandstore,             &
     &           btmflatstem, btmflatleaf, btmflatstore,                &
     &           btdstm, btgrainf,                                      &
     &           bdmstandstem, bdmstandleaf, bdmstandstore,             &
     &           bdmflatstem, bdmflatleaf, bdmflatstore,                &
     &           bddstm, bdgrainf, bdhyfg,                              &
     &           tot_mass_rem, sel_mass_left)

!     + + + PURPOSE + + +
!     Process # 37 called from doproc.for

!     This subroutine performs the biomass manipulation of thinning
!     biomass.  The component (either crop or a biomass pool) removed
!     is determined by flag which is set before the call to this
!     subroutine.

!     thinflg
!     0  - Remove fraction of Plants, thinval = fraction
!     1  - Thin to Plant Population, thinval = population

!     Note that biomass for any of these pools that are thinned is
!     either transferred to the coresponding flat pool or removed
!     depending on the three removal fraction values input

!     + + + KEYWORDS + + +
!     thin, transfer, biomass manipulation

      include 'p1werm.inc'

!     + + + ARGUMENT DECLARATIONS + + +

      integer thinflg
      real    thinval, grainf, cropf, standf

      real    bcmstandstem
      real    bcmstandleaf
      real    bcmstandstore

      real    bcmflatstem
      real    bcmflatleaf
      real    bcmflatstore

      real    bcdstm
      real    bcgrainf
      integer bchyfg

      real    btmstandstem
      real    btmstandleaf
      real    btmstandstore

      real    btmflatstem
      real    btmflatleaf
      real    btmflatstore

      real    btdstm
      real    btgrainf

      real    bdmstandstem(mnbpls)
      real    bdmstandleaf(mnbpls)
      real    bdmstandstore(mnbpls)

      real    bdmflatstem(mnbpls)
      real    bdmflatleaf(mnbpls)
      real    bdmflatstore(mnbpls)

      real    bddstm(mnbpls)
      real    bdgrainf(mnbpls)
      integer bdhyfg(mnbpls)

      real    tot_mass_rem, sel_mass_left

!     + + + ARGUMENT DEFINITIONS + + +

!     thinflg   - thinning value definition flag
!     thinval   - above ground height standing crop and/or
!                 residue is cut to (mm) or fraction

!     grainf    - of thinned material, fraction of reproductive mass removed
!     cropf     - of thinned material, fraction of standing crop plants removed
!     standf    - of thinned material, fraction of standing residue removed

!     bcmstandstem - crop standing stem mass (kg/m^2)
!     bcmstandleaf - crop standing leaf mass (kg/m^2)
!     bcmstandstore - crop standing storage mass (kg/m^2)
!                    (head with seed, or vegetative head (cabbage, pineapple))

!     bcdstm   - crop stem count (# stems/m^2)
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

!     btdstm   - temporary crop stem count (# stems/m^2)
!     btgrainf - internally computed grain fraction of reproductive mass

!     bdmstandstem  - standing stem mass (kg/m^2)
!     bdmstandleaf  - standing leaf mass (kg/m^2)
!     bdmstandstore - standing storage mass (kg/m^2)

!     bdmflatstem  - flat stem mass (kg/m^2)
!     bdmflatleaf  - flat leaf mass (kg/m^2)
!     bdmflatstore - flat storage mass (kg/m^2)

!     bddstm   - residue pool stem count (# stems/m^2)
!     bdgrainf - internally computed grain fraction of reproductive mass
!     bdhyfg - flag indicating the part of plant to apply the "grain fraction",
!              GRF, to when removing that plant part for yield
!         0     GRF applied to above ground storage (seeds, reproductive)
!         1     GRF times growth stage factor (see growth.for) applied to above ground storage (seeds, reproductive)
!         2     GRF applied to all aboveground biomass (forage)
!         3     GRF applied to leaf mass (tobacco)
!         4     GRF applied to stem mass (sugarcane)
!         5     GRF applied to below ground storage mass (potatoes, peanuts)

!     tot_mass_rem - mass of material removed by this harvest operation (kg/m^2)
!     sel_mass_left - mass of material left in pools from which mass is removed
!                     by this harvest operation (kg/m^2)

!     + + + ACCESSED COMMON BLOCK VARIABLE DEFINITIONS + + +
!     mnbpls        - max number of decomposition pools (currently=3)

!     + + + PARAMETERS + + +

!     + + + LOCAL VARIABLES + + +
      integer  idy

!     + + + LOCAL VARIABLE DEFINITIONS + + +
!     idy     - loop variable for decomp pools (3 pools total)

!     + + + END SPECIFICATIONS + + +

!     assign crop grain fraction values to temporary pool since 
!     material may be transferred to the temporary pool without
!     a specific kill operation
      btgrainf = bcgrainf

!     convert thinning value for all cases to fraction of plant
!     population to remain
      select case(thinflg)
      case(0)
          thinval = 1.0-thinval
      case(1)
          if(bcdstm.gt.0.0) then
              thinval = min(1.0, thinval/bcdstm)
          else
              thinval = 0.0
          end if
      case default
          write(*,*) 'Invalid thinning flag, nothing thinned'
      end select

!     This thinning is applied to all standing pools,
!     like a cutting device, it is not discriminate in any way

      tot_mass_rem = 0.0
      sel_mass_left = 0.0

      ! based on the structure used here, it is assumed that thinning
      ! is a live crop (standing and flat) and a temporary pool (standing only,
      ! since thinning dead flat biomass is meaningless)
      ! and multiple residue decomposition pools (standing only). Removal
      ! is applied to the pools in the same manner.

      ! thin crop standing pool
      call thin_pool (                                                  &
     &     thinval, grainf, cropf,                                      &
     &     bcmstandstem, bcmstandleaf, bcmstandstore,                   &
     &     btmflatstem, btmflatleaf, btmflatstore,                      &
     &     bcgrainf, bchyfg, tot_mass_rem, sel_mass_left)

      ! if living crop flat pool has biomass, also transfer the correct
      ! proportions into temporary pool
      ! thin crop flat pool
      call thin_pool (                                                  &
     &     thinval, grainf, cropf,                                      &
     &     bcmflatstem, bcmflatleaf, bcmflatstore,                      &
     &     btmflatstem, btmflatleaf, btmflatstore,                      &
     &     bcgrainf, bchyfg, tot_mass_rem, sel_mass_left)

      ! modify stem count to reflect change
      bcdstm = bcdstm * thinval

      ! thin temporary crop pool
      call thin_pool (                                                  &
     &     thinval, grainf, cropf,                                      &
     &     btmstandstem, btmstandleaf, btmstandstore,                   &
     &     btmflatstem, btmflatleaf, btmflatstore,                      &
     &     btgrainf, bchyfg, tot_mass_rem, sel_mass_left)

      ! modify stem count to reflect change
      btdstm = btdstm * thinval

      do idy = 1, mnbpls
          ! thin residue decomposition crop pools
          call thin_pool (                                              &
     &         thinval, grainf, standf,                                 &
     &         bdmstandstem(idy), bdmstandleaf(idy), bdmstandstore(idy),&
     &         bdmflatstem(idy), bdmflatleaf(idy), bdmflatstore(idy),   &
     &         bdgrainf(idy), bdhyfg(idy), tot_mass_rem, sel_mass_left)

          ! modify stem count to reflect change
          bddstm(idy) = bddstm(idy) * thinval
      end do

      return
      end

! ------------------------------------------------------------------
      ! local subroutine to apply thinning fractions and biomass removal 
      ! fractions to each pool. Stem number reduction is done outside the
      ! the subroutine to allow the flat mass pool for a living crop to be
      ! handled the same as a standing pool for thinning purposes.

      subroutine thin_pool (                                            &
     &           thinval, grainf, cropf,                                &
     &           poolmstandstem, poolmstandleaf, poolmstandstore,       &
     &           poolmflatstem, poolmflatleaf, poolmflatstore,          &
     &           poolgrainf, poolhyfg, tot_mass_rem, sel_mass_left)

!     + + + ARGUMENT DECLARATIONS + + +
      real    thinval, grainf, cropf
      real    poolmstandstem, poolmstandleaf, poolmstandstore
      real    poolmflatstem, poolmflatleaf, poolmflatstore
      integer poolhyfg
      real    poolgrainf, tot_mass_rem, sel_mass_left

!     + + + LOCAL VARIABLE DEFINITIONS + + +
      integer pool_flag
      real mass_thin
      real mass_rem
      real rem_frac

!     + + + LOCAL VARIABLE DEFINITIONS + + +

!     + + + END SPECIFICATIONS + + +

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
      end
