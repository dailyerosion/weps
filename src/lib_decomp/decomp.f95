!$Author$
!$Date$
!$Revision$
!$HeadURL$
      subroutine decomp(isr, crop, residue, decompfac, h1et)

      use weps_interface_defs, ignore_me=>decomp
      use biomaterial, only: biomatter, decomp_factors
      use decomp_data_struct_defs, only: am0dfl, am0ddb
      use climate_input_mod, only: cli_today
      use hydro_data_struct_defs, only: hydro_derived_et

!     +++ PURPOSE + + +

!     decomp.for calculates change in standing, flat and belowground
!     biomass. It carries three age pools of residues most recent, previous and
!     combined old material. Decomp also estimates the number of standing
!     stems and soil surface cover provided by the surface residues.
!     Data for each subregion that is needed following a harvest is
!     maintained within local variables on a daily basis.

!     Authors: Harry Schomberg and Jean Steiner
!              USDA-ARS Bushland, TX
!              USDA-ARS Watkinsville, GA
!
!     + + +   KEYWORDS + + +
!     decompdays, standing residue, surface residue, buried residue,
!     soil cover, residue cover, decomposition day

!       + + +  PARAMTERS AND COMMON BLOCKS +++

      include 'p1werm.inc'

!   These hydrology common blocks provide soil temp, moisture and irrigation

      include 'h1temp.inc'
      include 'h1db1.inc'
      include 'h1hydro.inc'

!     + + +   ARGUMENT DECLARATIONS + + +
      integer, intent(in) :: isr                               ! current subregion
      type(biomatter), intent(inout) :: crop  ! structure containing biomatter state and parameters
      type(biomatter), dimension(:), intent(inout) :: residue  ! structure containing biomatter state and parameters
      type(decomp_factors), intent(inout) :: decompfac
      type(hydro_derived_et), intent(in) :: h1et

!     + + +  VARIABLES MAINTAINED BY SUBREGION + + +
!
!         dweti(mnsub)
!         diwcsy(mnsub)
!         cumdds(mnbpls,mnsub)
!         cumddf(mnbpls,mnsub)
!         cumddg(mnsz,mnbpls,mnsub)

!     + + + VARIABLE DECLARATIONS + + +

      integer :: iage    ! residue pool age index
      integer :: isz     ! soil layer indexing variable
      integer :: nslay   ! maximum number of soil layers

! + + +  ADDITIONAL LOCAL VARIABLES NOT IN DECOMP.KOM + + +
!     These are used in tc function.
!     tavgsq - average temperature squared  (C)
!     temp    - average air or soil temp    (C)
!     toptsq  - optimum temperature for residue decomposition (32C)

      real dec_fac, prev_mass
      real leaf_fac, store_fac
      parameter (leaf_fac = 3.0)
      parameter (store_fac = 1.5)

!     leaf_fac  - leaf decomposition rate = leaf_fac * stem decomposition rate
!     store_fac - store decomposition rate = store_fac * stem decomposition rate

!     + + +   FUNCTION CALLS +++
!
!     tc - Calculates temperature based scaling factor
      real  tc
      
      logical dbgflg
!
!     + + +   DATA INITIALIZATIONS + + +
!  These data initializations are being done every day.  Need to make
!  sure that when a harvest takes place that all the decomp pools are
!  updated correctly.
!
      data dbgflg /.false./

      ! set the number of soil layers from previously allocated structure
      nslay = size(decompfac%iwcg)

      if (am0ddb(isr) .eq. 1) call ddbug(isr, nslay, residue)

      if (dbgflg) write(*,*) 'decomp 1'

!     + + +  END SPECIFICATIONS + + +
      if (dbgflg) write(*,*) 'decomp 1a'

!   call initilization
!     if (am0dif .eqv. .true.) then
!         call decini (isr)
!     end if
!     am0dif = .false.
!  Calculation of water coefficent for decomp days
! Standing residues water factor  ( 0. to 1. )
! Steiner et al. 1994 Agronomy Journal  Jan-Feb issue

! sum rain, irr, snow melt

      if (dbgflg) write(*,*) 'decomp 2'
      decompfac%aqua = cli_today%zdpt + h1et%zirr + ahzsmt(isr)

! Test for water input day.

      if (decompfac%aqua .gt. 0.) then

         decompfac%weti = 4 !set # of days for antecedent
         decompfac%iwcs = decompfac%aqua / 4.0                !4 mm or more is optimum for decomp.
         decompfac%iwcs = decompfac%iwcs + decompfac%iwcsy * 0.4 !add previous antecdent moisture

         if (decompfac%iwcs.gt.1.) decompfac%iwcs = 1.0      !Limit no greater than 1.

      else if (decompfac%weti .gt. 0) then      !No precip but recent water input
         decompfac%weti = decompfac%weti - 1     !decrement days since precip
         decompfac%iwcs = decompfac%iwcsy*0.4           !set decompfac%iwcs to decremented value

      else                                 
         decompfac%iwcs = 0.0                       !no decomp after 4 or more days without precip.
      endif

      decompfac%iwcsy = decompfac%iwcs !save decompfac%iwcs for calc. of tomorrows water factor

!  Surface water factor same as standing  (0. to 1.)
!     Need to set up better test of water factor  (12-8-1993)


!     code changed to use hydrology global variables  HHS 1- 4- 1994
!     old code >    decompfac%iwcf = theta(1)/thetaf(1)

      decompfac%iwcf = ahrwc0(12,isr) / ahrwcf(1,isr) !use water content at surface at 12 noon
      !decompfac%iwcf = ahrwc(1,isr) / ahrwcf(1,isr) !use water content of soil layer 1

!     water factor = water content of top soil layer / optimum water content of top soil layer
      if (decompfac%iwcf.gt.1.0) decompfac%iwcf = 1.0
      decompfac%iwcf = max(decompfac%iwcf,decompfac%iwcs) !for flat residue, use the greatest of flat and standing water factor



      !decompfac%iwcf = decompfac%iwcs !flat: precip based (like standing) --> underestimation of decomposition
      !decompfac%iwcf = 1.0 !flat: optimum moisture for decomp --> overestimation of decomposition




! Belowground water factor             (0. to 1.)
! Stanford and Epstien 1974, SSSAJ 34:103-107 theta/thetaopt


! code changed to use global hydrology variables HHS 1-4-1994
!
!      do 30 isz = 1 , nslay
!          decompfac%iwcg(isz) = theta(isz)/ thetaf(isz)
!          if (decompfac%iwcg(isz) .gt. 1.) decompfac%iwcg(isz) = 1.
!   30 continue

      if (dbgflg) write(*,*) 'decomp 3'

      do isz = 1 , nslay
         decompfac%iwcg(isz) = ahrwc(isz,isr)/ ahrwcf(isz,isr)
!        water factor = water content of soil layer / optimum water content of soil layer
         if (decompfac%iwcg(isz) .gt. 1.0) decompfac%iwcg(isz) = 1.0
      end do

! Calculate temperature coefficient    (0. to 1.)
! Stroo et al., 1989, SSSAJ 53:91-99 used in the tc function.
! Above ground (standing and flat) biomass tc use air temp.
! Compute TC(Tmax) and TC(Tmin)and then average the two results.
! This is the way it was intended to be (Harry Schomberg, 
! phone call with Simon van Donk, July 2002)

      ! replaced itca with itcs (standing) and itcf (flat) so different methods could be used. (like under snow or w/ thick mulch)
      decompfac%itcs =  (tc(cli_today%tdmn) + tc(cli_today%tdmx))/2
      decompfac%itcf =  (tc(cli_today%tdmn) + tc(cli_today%tdmx))/2

! Below ground biomass tc calculated for each soil layer
!!use average of max and min for calculation

      do isz = 1, nslay

! Code changed to use global hydrology soil temp variable
!              tsavg= (tsmax(isz) + tsmin(isz))/2.
!              decompfac%itcg(isz) = tc(tsavg)

         decompfac%itcg(isz) =  tc(ahtsav(isz,isr))
      end do

! Select minimum of temperature or water functions for
! the quantity (fraction) of a decomposition day accumulated
! during the current 24 hr period.

!  for standing, flat and buried residues
      decompfac%idds = min(decompfac%iwcs,decompfac%itcs) !standing
      decompfac%iddf = min(decompfac%iwcf,decompfac%itcf) !flat
      do isz = 1, nslay
         decompfac%iddg(isz) = min(decompfac%iwcg(isz),decompfac%itcg(isz)) !buried
      end do

! Summation of DECOMPOSITION days for graphing
! this is indexed based on the number of residue age pools

! all, standing, flat and below ground
      do iage = 1,mnbpls
         ! calendar days
         residue(iage)%decomp%resday = residue(iage)%decomp%resday + 1
         ! decomposition days
         residue(iage)%decomp%cumdds = residue(iage)%decomp%cumdds + decompfac%idds
         residue(iage)%decomp%cumddf = residue(iage)%decomp%cumddf + decompfac%iddf
         do isz = 1, nslay
            residue(iage)%decomp%cumddg(isz) = residue(iage)%decomp%cumddg(isz) + decompfac%iddg(isz)
         end do
      end do

      if (dbgflg) write(*,*) 'decomp 4'

! Decompose each age pool of residue based on decomp days accumulated in
! the present 24 hr using the numerical formula for exponential decay
!      Mass(t) = mass(t-1) * (1 - k * dday)

      ! crop flat leaves are dead and assumed to start decomposing
      if( crop%mass%flatleaf .gt. 0.0 ) then
          dec_fac = max(0.0, 1.0 - leaf_fac*crop%database%dkrate(2)*decompfac%iddf)
          crop%mass%flatleaf = crop%mass%flatleaf * dec_fac
      end if

      do iage = 1,mnbpls
        !standing residue mass
        dec_fac = max(0.0, 1.0 - residue(iage)%database%dkrate(1)*decompfac%idds)
        residue(iage)%mass%standstem = residue(iage)%mass%standstem * dec_fac
        dec_fac = max(0.0, 1.0 - leaf_fac*residue(iage)%database%dkrate(1)*decompfac%idds)
        residue(iage)%mass%standleaf = residue(iage)%mass%standleaf * dec_fac
        dec_fac = max(0.0, 1.0 - store_fac*residue(iage)%database%dkrate(1)*decompfac%idds)
        residue(iage)%mass%standstore = residue(iage)%mass%standstore * dec_fac

        !flat residue mass
        dec_fac = max(0.0, 1.0 - residue(iage)%database%dkrate(2)*decompfac%iddf)
        residue(iage)%mass%flatstem = residue(iage)%mass%flatstem * dec_fac
        dec_fac = max(0.0, 1.0 - leaf_fac*residue(iage)%database%dkrate(2)*decompfac%iddf)
        residue(iage)%mass%flatleaf = residue(iage)%mass%flatleaf * dec_fac
        dec_fac = max(0.0, 1.0 - store_fac*residue(iage)%database%dkrate(2)*decompfac%iddf)
        residue(iage)%mass%flatstore = residue(iage)%mass%flatstore * dec_fac

        ! unburied root mass
        dec_fac = max(0.0, 1.0 - residue(iage)%database%dkrate(2)*decompfac%iddf)
        residue(iage)%mass%flatrootstore = residue(iage)%mass%flatrootstore * dec_fac
        residue(iage)%mass%flatrootfiber = residue(iage)%mass%flatrootfiber * dec_fac

        do isz = 1, nslay
          ! buried surface biomass
          dec_fac = max(0.0, 1.0-residue(iage)%database%dkrate(3)*decompfac%iddg(isz))
          residue(iage)%mass%stemz(isz) = residue(iage)%mass%stemz(isz) * dec_fac
          dec_fac = max(0.0, 1.0-leaf_fac*residue(iage)%database%dkrate(3)*decompfac%iddg(isz))
          residue(iage)%mass%leafz(isz) = residue(iage)%mass%leafz(isz) * dec_fac
          dec_fac = max(0.0,1.0-store_fac*residue(iage)%database%dkrate(3)*decompfac%iddg(isz))
          residue(iage)%mass%storez(isz) = residue(iage)%mass%storez(isz) * dec_fac

          ! buried root biomass
          dec_fac = max(0.0, 1.0 - residue(iage)%database%dkrate(4)*decompfac%iddg(isz))
          residue(iage)%mass%rootstorez(isz) = residue(iage)%mass%rootstorez(isz) * dec_fac
          residue(iage)%mass%rootfiberz(isz) = residue(iage)%mass%rootfiberz(isz) * dec_fac
        end do
      end do

! Change standing stem number and adjust the mass for standing
! and surface biomass Steiner et al., 1994 Agronomy Journal

      if (dbgflg) write(*,*) 'decomp 5'

      do iage = 1,mnbpls
         ! check for threshold ddays value before allowing stems to decline
         if (residue(iage)%decomp%cumdds .gt. residue(iage)%database%ddsthrsh) then
            if (residue(iage)%geometry%dstm .gt. 0.0) then
               ! Calculate stem fall and new stem number. This stem fall
               ! ratio is then applied to the standing pools since their 
               ! mass is transferred to flat in the same proportion
               dec_fac = max(0.0, 1.0 - residue(iage)%database%dkrate(5)* decompfac%idds)
               residue(iage)%geometry%dstm = residue(iage)%geometry%dstm * dec_fac

               ! Move corresponding standing stem mass to flat stem mass
               prev_mass = residue(iage)%mass%standstem
               residue(iage)%mass%standstem = residue(iage)%mass%standstem * dec_fac
               residue(iage)%mass%flatstem = residue(iage)%mass%flatstem + (prev_mass - residue(iage)%mass%standstem)

               ! Move corresponding standing leaf mass to flat leaf mass
               prev_mass = residue(iage)%mass%standleaf
               residue(iage)%mass%standleaf = residue(iage)%mass%standleaf * dec_fac
               residue(iage)%mass%flatleaf = residue(iage)%mass%flatleaf + (prev_mass - residue(iage)%mass%standleaf)

               ! Move corresponding standing store mass to flat store mass
               prev_mass = residue(iage)%mass%standstore
               residue(iage)%mass%standstore = residue(iage)%mass%standstore * dec_fac
               residue(iage)%mass%flatstore = residue(iage)%mass%flatstore + (prev_mass - residue(iage)%mass%standstore)
            end if
         end if
      end do

      if (dbgflg) write(*,*) 'decomp 10'
      if (am0ddb(isr) .eq. 1) call ddbug(isr, nslay, residue)
      if ((am0dfl(isr) .eq. 1).or.(am0dfl(isr) .eq. 2).or.(am0dfl(isr) .eq.3)) call decout(isr, residue)

      return
      end

! + + +  Function tc

      real function tc (temp)

! + + +  PURPOSE + + +
!
!     Calculate temperature coefficients for estimation of decompsition days
!     using the temperature of the environment the residues are in.
!
!     Equation form is from Stroo et. al, 1989. SSSAJ 53:91-99
!     we used a different optimum temperature and set the "a" value
!     to zero to make the minimum microbial activity corespond to 0 C
!     In their equation the entire value was multiplied by 1.32 to
!     broaden the temperature range where temperature was optimum.
!     We felt that this parameter should be dropped
!     to allow greater interacting effects of water and moisture.
!
!  + + +  DECLARATION OF ARGUMENT + + +

      real temp

! + + +  DECLARATION OF VARIABLES  + + +

      real toptsq, tavgsq

! + + +  DEFINITION OF VARIABLEES AND ARGUMENTS + + +
!     all in degrees C

!     temp    - temperature of air or soil layer
!     toptsq  - optimum temperature squared
!     tavgsq  - temp variable squared

! + + +  END OF SPECIFICATION + + +

      if (temp .lt. 0.0) then
         tc = 0.0
      else
         toptsq = 32.0 * 32.0
         tavgsq = temp * temp
         tc = (2.0*tavgsq*toptsq-tavgsq*tavgsq) / (toptsq*toptsq)
      endif

      if (tc .gt. 1.0) tc = 1.0
      if (tc .lt. 0.0) tc = 0.0 !this prevents tc from becoming less than 0 at high temperatures! SVD

      return
      end

