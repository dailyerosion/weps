!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine sumbio(isr, crop, residue, restot, croptot, biotot, subrsurf)

      use biomaterial, only: biomatter, biototal
      use wind_mod, only: biodrag
      use erosion_data_struct_defs, only: subregionsurfacestate

!     + + +   ARGUMENT DECLARATIONS + + +
      integer, intent(in) :: isr
      type(biomatter), intent(in) :: crop
      type(biomatter), dimension(:), intent(in) :: residue
      type(biototal), intent(in) :: croptot
      type(biototal), intent(inout) :: restot, biotot
      type(subregionsurfacestate), intent(inout) :: subrsurf  ! subregion surface conditions

!     Update geometric properties of all biomass pools

      include 'p1werm.inc'

! local variables

      integer idx,jdx
      real atotal

!     + + + FUNCTIONS CALLED + + +
      real    resevapredu

! *****************************************************************
!     Compute total number of stems

      biotot%dstmtot = croptot%dstmtot + restot%dstmtot

! *****************************************************************
!     compute the weighted average residue height

!     determine weighting factors (stem area index)
      atotal = croptot%rsaitot + restot%rsaitot

!     linearly weight height and representative diameter from crops and residue pools  based on stem area index
      if( atotal .gt. 0.0 ) then
         biotot%zht_ave = (croptot%zht_ave * croptot%rsaitot + restot%zht_ave * restot%rsaitot) / atotal
         biotot%xstmrep = (croptot%xstmrep * croptot%rsaitot + restot%xstmrep * restot%rsaitot) / atotal
      else
         biotot%zht_ave = 0.0
         biotot%xstmrep = 0.0
      end if

! *****************************************************************
!     set the tallest biomass height
      biotot%zmht = max( croptot%zmht, restot%zmht )

! *****************************************************************
!     sum the flat biomass from each pool
!     sum the standing biomass from each pool
!     sum the buried biomass from each pool
!     sum the root biomass from each pool

      biotot%mftot = croptot%mftot + restot%mftot    !flat
      biotot%msttot = croptot%msttot + restot%msttot !standing 

      biotot%mbgtot = croptot%mbgtot + restot%mbgtot !below ground
      biotot%mrttot = croptot%mrttot + restot%mrttot !roots
! *****************************************************************
!     determine the total mass of biomass (above, flat and below ground)
      biotot%mtot = croptot%mtot + restot%mtot
! *****************************************************************
!     sum the buried biomass by layer
!     sum the root mass by layer
      do jdx=1, size(biotot%mbgz)
        biotot%mbgz(jdx) = croptot%mbgz(jdx) + restot%mbgz(jdx)
        biotot%mrtz(jdx) = croptot%mrtz(jdx) + restot%mrtz(jdx)
      end do
! *****************************************************************
!     sum the stem area index and leaf area index values
      biotot%rsaitot = croptot%rsaitot + restot%rsaitot
      biotot%rlaitot = croptot%rlaitot + restot%rlaitot

!     compute "effective biomass (live and dead) drag coefficient
!     from SAI and LAI values
      biotot%rcdtot = biodrag( restot%rlaitot, restot%rsaitot, croptot%rlaitot,&
                  croptot%rsaitot, croptot%c0rg, croptot%xrow, croptot%zht_ave, &
                  subrsurf%aszrgh )
! *****************************************************************
!     sum the stem area index and leaf area index values by height
!     this is based upon the "tallest" biomass pool height value
!     (abzmht) determined previously.

      ! This divides the biomass equally into the height increments
      ! it isn't used yet and !really!!! is not right!!! since each
      ! pool should have it's own height, and hence divisions. This
      ! should at least stay within the arrays.
      do jdx = 1, size(biotot%rsaz)
          biotot%rsaz(jdx) = croptot%rsaz(jdx) + restot%rsaz(jdx)
          biotot%rlaz(jdx) = croptot%rlaz(jdx) + restot%rlaz(jdx)
      end do


! *****************************************************************
!     Combine residue cover from crop and decomp. pools.
!     Overlap only applies when adding flat and flat, not flat and standing,
!     or standing and standing.
!     Note that these values shouldn't ever exceed 1.0 or be less than zero

      ! flat and flat, with overlap
      biotot%ffcvtot = croptot%ffcvtot + restot%ffcvtot * (1.0-croptot%ffcvtot)

      ! standing and standing, no overlap
      biotot%fscvtot = croptot%fscvtot + restot%fscvtot
      if (biotot%fscvtot > 1.0) biotot%fscvtot = 1.0

      ! flat and standing, no overlap
      biotot%ftcvtot =  biotot%ffcvtot + biotot%fscvtot
      if (biotot%ftcvtot > 1.0) biotot%ftcvtot = 1.0

      ! canopy cover for all biomass (overlaps)
      biotot%ftcancov = croptot%ftcancov + restot%ftcancov*(1.0-croptot%ftcancov)

!     find composite evaporation supression for total flat residue
      ! set initial value to no residue condition
      biotot%evapredu = 1.0
      ! start with older flat residue layers
      do idx = size(residue),1,-1
          if( residue(idx)%deriv%mf .gt. 0.0 ) then
              biotot%evapredu = resevapredu( biotot%evapredu, residue(idx)%deriv%mf, &
                                            residue(idx)%database%resevapa, residue(idx)%database%resevapb )
          end if
      end do
      ! add any flat crop residue to the reduction
      if( croptot%mftot .gt. 0.0 ) then
          biotot%evapredu = resevapredu( biotot%evapredu, croptot%mftot,    &
                       crop%database%resevapa, crop%database%resevapb )
      end if

      return
      end
