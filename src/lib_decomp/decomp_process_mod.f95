!$Author$
!$Date$
!$Revision$
!$HeadURL$

module decomp_process_mod

  contains

    subroutine decomp(isr, soil, plant, decompfac, hstate, h1et)

      use soil_data_struct_defs, only: soil_def
      use biomaterial, only: plant_pointer, residue_pointer, decomp_factors
      use decomp_data_struct_defs, only: am0dfl, am0ddb
      use climate_input_mod, only: cli_day
      use hydro_data_struct_defs, only: hydro_derived_et, hydro_state, hhrs
      use decomp_out_mod, only: ddbug, decout
      use datetime_mod, only: get_psim_juld

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

!     + + +   ARGUMENT DECLARATIONS + + +
      integer, intent(in) :: isr                               ! current subregion
      type(soil_def), intent(in) :: soil  ! soil for this subregion
      type(plant_pointer), pointer :: plant     ! pointer to youngest plant data, which chains to older plant data
      type(decomp_factors), intent(inout) :: decompfac
      type(hydro_state), intent(in) :: hstate
      type(hydro_derived_et), intent(in) :: h1et

!     + + + VARIABLE DECLARATIONS + + +

      integer :: isz     ! soil layer indexing variable
      integer :: nslay   ! maximum number of soil layers
      integer :: pjuld   ! present julian day

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

      type(plant_pointer), pointer :: thisPlant
      type(residue_pointer), pointer :: thisResidue

!     + + +   DATA INITIALIZATIONS + + +
!  These data initializations are being done every day.  Need to make
!  sure that when a harvest takes place that all the decomp pools are
!  updated correctly.
!
      pjuld = get_psim_juld(isr)

      ! set the number of soil layers from previously allocated structure
      nslay = size(decompfac%iwcg)

      if (am0ddb(isr) .eq. 1) call ddbug(isr, nslay, plant)

      ! + + +  END SPECIFICATIONS + + +

      !  Calculation of water coefficent for decomp days
      ! Standing residues water factor  ( 0. to 1. )
      ! Steiner et al. 1994 Agronomy Journal  Jan-Feb issue

      ! sum rain, irr, snow melt
      decompfac%aqua = cli_day(pjuld)%zdpt + h1et%zirr + hstate%zsmt

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

      ! Surface water factor same as standing  (0. to 1.)
      ! Need to set up better test of water factor  (12-8-1993)

      ! code changed to use hydrology global variables  HHS 1- 4- 1994
      ! old code >    decompfac%iwcf = theta(1)/thetaf(1)

      decompfac%iwcf = hstate%rwc0(hhrs/2) / soil%ahrwcf(1) !use water content at surface at midday
      ! decompfac%iwcf = ahrwc(1,isr) / soil%ahrwcf(1) !use water content of soil layer 1

!     water factor = water content of top soil layer / optimum water content of top soil layer
      if (decompfac%iwcf.gt.1.0) decompfac%iwcf = 1.0
      decompfac%iwcf = max(decompfac%iwcf,decompfac%iwcs) !for flat residue, use the greatest of flat and standing water factor

      ! decompfac%iwcf = decompfac%iwcs !flat: precip based (like standing) --> underestimation of decomposition
      ! decompfac%iwcf = 1.0 !flat: optimum moisture for decomp --> overestimation of decomposition

      ! Belowground water factor             (0. to 1.)
      ! Stanford and Epstien 1974, SSSAJ 34:103-107 theta/thetaopt

      ! code changed to use global hydrology variables HHS 1-4-1994

      ! do isz = 1 , nslay
      !     decompfac%iwcg(isz) = theta(isz)/ thetaf(isz)
      !     if (decompfac%iwcg(isz) .gt. 1.) decompfac%iwcg(isz) = 1.
      ! end do

      do isz = 1 , nslay
         decompfac%iwcg(isz) = soil%ahrwc(isz)/ soil%ahrwcf(isz)
         ! water factor = water content of soil layer / optimum water content of soil layer
         if (decompfac%iwcg(isz) .gt. 1.0) decompfac%iwcg(isz) = 1.0
      end do

      ! Calculate temperature coefficient    (0. to 1.)
      ! Stroo et al., 1989, SSSAJ 53:91-99 used in the tc function.
      ! Above ground (standing and flat) biomass tc use air temp.
      ! Compute TC(Tmax) and TC(Tmin)and then average the two results.
      ! This is the way it was intended to be (Harry Schomberg, 
      ! phone call with Simon van Donk, July 2002)

      ! replaced itca with itcs (standing) and itcf (flat) so different methods could be used. (like under snow or w/ thick mulch)
      decompfac%itcs =  (tc(cli_day(pjuld)%tdmn) + tc(cli_day(pjuld)%tdmx))/2
      decompfac%itcf =  (tc(cli_day(pjuld)%tdmn) + tc(cli_day(pjuld)%tdmx))/2

      ! Below ground biomass tc calculated for each soil layer
      !!use average of max and min for calculation

      do isz = 1, nslay
         ! Code changed to use global hydrology soil temp variable
         ! tsavg= (tsmax(isz) + tsmin(isz))/2.
         ! decompfac%itcg(isz) = tc(tsavg)
         decompfac%itcg(isz) =  tc(soil%tsav(isz))
      end do

      ! Select minimum of temperature or water functions for
      ! the quantity (fraction) of a decomposition day accumulated
      ! during the current 24 hr period.

      ! for standing, flat and buried residues
      decompfac%idds = min(decompfac%iwcs,decompfac%itcs) !standing
      decompfac%iddf = min(decompfac%iwcf,decompfac%itcf) !flat
      do isz = 1, nslay
         decompfac%iddg(isz) = min(decompfac%iwcg(isz),decompfac%itcg(isz)) !buried
      end do

      ! Decompose each age pool of residue based on decomp days accumulated in
      ! the present 24 hr using the numerical formula for exponential decay
      ! Mass(t) = mass(t-1) * (1 - k * dday)

      thisPlant => plant
      do while( associated(thisPlant) )
        ! plant exists
        ! all plant biomass is living, not decomposing.

        ! decompose all residues in this plant
        thisResidue => thisPlant%residue
        do while( associated(thisResidue) )

          ! update decomposition ages for this pool
          ! calendar days
          thisResidue%resday = thisResidue%resday + 1
          ! decomposition days
          thisResidue%cumdds = thisResidue%cumdds + decompfac%idds
          thisResidue%cumddf = thisResidue%cumddf + decompfac%iddf
          do isz = 1, nslay
            thisResidue%cumddg(isz) = thisResidue%cumddg(isz) + decompfac%iddg(isz)
          end do

          ! decompose
          !standing residue mass
          dec_fac = max(0.0, 1.0 - thisPlant%database%dkrate(1)*decompfac%idds)
          thisResidue%standstem = thisResidue%standstem * dec_fac
          dec_fac = max(0.0, 1.0 - leaf_fac*thisPlant%database%dkrate(1)*decompfac%idds)
          thisResidue%standleaf = thisResidue%standleaf * dec_fac
          dec_fac = max(0.0, 1.0 - store_fac*thisPlant%database%dkrate(1)*decompfac%idds)
          thisResidue%standstore = thisResidue%standstore * dec_fac

          !flat residue mass
          dec_fac = max(0.0, 1.0 - thisPlant%database%dkrate(2)*decompfac%iddf)
          thisResidue%flatstem = thisResidue%flatstem * dec_fac
          dec_fac = max(0.0, 1.0 - leaf_fac*thisPlant%database%dkrate(2)*decompfac%iddf)
          thisResidue%flatleaf = thisResidue%flatleaf * dec_fac
          dec_fac = max(0.0, 1.0 - store_fac*thisPlant%database%dkrate(2)*decompfac%iddf)
          thisResidue%flatstore = thisResidue%flatstore * dec_fac

          ! unburied root mass
          dec_fac = max(0.0, 1.0 - thisPlant%database%dkrate(2)*decompfac%iddf)
          thisResidue%flatrootstore = thisResidue%flatrootstore * dec_fac
          thisResidue%flatrootfiber = thisResidue%flatrootfiber * dec_fac

          do isz = 1, nslay
            ! buried surface biomass
            dec_fac = max(0.0, 1.0-thisPlant%database%dkrate(3)*decompfac%iddg(isz))
            thisResidue%stemz(isz) = thisResidue%stemz(isz) * dec_fac
            dec_fac = max(0.0, 1.0-leaf_fac*thisPlant%database%dkrate(3)*decompfac%iddg(isz))
            thisResidue%leafz(isz) = thisResidue%leafz(isz) * dec_fac
            dec_fac = max(0.0,1.0-store_fac*thisPlant%database%dkrate(3)*decompfac%iddg(isz))
            thisResidue%storez(isz) = thisResidue%storez(isz) * dec_fac

            ! buried root biomass
            dec_fac = max(0.0, 1.0 - thisPlant%database%dkrate(4)*decompfac%iddg(isz))
            thisResidue%rootstorez(isz) = thisResidue%rootstorez(isz) * dec_fac
            thisResidue%rootfiberz(isz) = thisResidue%rootfiberz(isz) * dec_fac
          end do


          ! Change standing stem number and adjust the mass for standing
          ! and surface biomass Steiner et al., 1994 Agronomy Journal

          ! check for threshold ddays value before allowing stems to decline
          if (thisResidue%cumdds .gt. thisPlant%database%ddsthrsh) then
            if (thisResidue%dstm .gt. 0.0) then
               ! Calculate stem fall and new stem number. This stem fall
               ! ratio is then applied to the standing pools since their 
               ! mass is transferred to flat in the same proportion
               dec_fac = max(0.0, 1.0 - thisPlant%database%dkrate(5)* decompfac%idds)
               thisResidue%dstm = thisResidue%dstm * dec_fac

               ! Move corresponding standing stem mass to flat stem mass
               prev_mass = thisResidue%standstem
               thisResidue%standstem = thisResidue%standstem * dec_fac
               thisResidue%flatstem = thisResidue%flatstem + (prev_mass - thisResidue%standstem)

               ! Move corresponding standing leaf mass to flat leaf mass
               prev_mass = thisResidue%standleaf
               thisResidue%standleaf = thisResidue%standleaf * dec_fac
               thisResidue%flatleaf = thisResidue%flatleaf + (prev_mass - thisResidue%standleaf)

               ! Move corresponding standing store mass to flat store mass
               prev_mass = thisResidue%standstore
               thisResidue%standstore = thisResidue%standstore * dec_fac
               thisResidue%flatstore = thisResidue%flatstore + (prev_mass - thisResidue%standstore)
            end if
          end if

          ! go to next older residue in thisPlant
          thisResidue => thisResidue%olderResidue
        end do

        ! go to next older plant
        thisPlant => thisPlant%olderPlant
      end do

      ! for debug and out, no derived values are updated at this point
      if (am0ddb(isr) .eq. 1) call ddbug(isr, nslay, plant)
      if ((am0dfl(isr) .eq. 1).or.(am0dfl(isr) .eq. 2).or.(am0dfl(isr) .eq.3)) call decout(isr, plant)

      return
    end subroutine decomp

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
    end function tc

end module decomp_process_mod

