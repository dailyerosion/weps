!$Author$
!$Date$
!$Revision$
!$HeadURL$
      real function resevapredu(                                        &
     &           prev_redu_ratio, biomass, coeff_a, coeff_b)

!     + + + VARIABLE DECLARATIONS + + +
      real prev_redu_ratio
      real biomass
      real coeff_a
      real coeff_b

!     + + + PURPOSE + + +
      ! calculates the evaporation reduction ratio given an accumulated
      ! evaporation reduction ratio from lower layers of residue. This
      ! accumulated ratio moves the effect of the additional biomass
      ! to a lower point on the evaporation reduction - biomass function
      ! curve and returns a ratio effecting the additional effect of the
      ! additional biomass.

!     + + + VARIABLE DEFINITIONS + + +
!     prev_redu_ratio - Accumulated evaporation reduction ratio from lower layers
!     biomass - additional biomass being added to evaporation reduction effect
!     coeff_a - coefficient a of ratio = exp( a * biomass ** b ) for this biomass
!     coeff_b - coefficient b of ratio = exp( a * biomass ** b ) for this biomass

!     LOCAL VARIABLES
      real pseudo_biomass

!     LOCAL VARIABLE DEFINITIONS
!     pseudo_biomass - an amount of biomass that would result in the prev_redu_ratio

      if( prev_redu_ratio .gt. 0.0 ) then
        ! zero value results in error taking log  below. This only happens
        ! with very large amounts of residue in multiple residue pools.
        if( (coeff_a .ne. 0.0) .and. (coeff_b .ne. 0.0) ) then
          pseudo_biomass =(log(prev_redu_ratio)/coeff_a )**(1.0/coeff_b)
        else
          pseudo_biomass = 0.0
        end if

        resevapredu = exp(coeff_a * (pseudo_biomass + biomass)**coeff_b)
      else
        resevapredu = 0.0
      end if

      return
      end