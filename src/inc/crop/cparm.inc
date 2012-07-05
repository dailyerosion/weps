!$Author$
!$Date$
!$Revision$
!$HeadURL$

!     file: cparm.inc

!     This file contains the declaration of the common block of variables
!     used in s-curve calculations of lai, co2, frost & harvest index.

!     + + + VARIABLE DECLARATIONS + + +
      real a_la,b_la,a_co,b_co,a_fr,b_fr,a_hi
      real b_hi,a_s11,b_s11,a_s8,b_s8,parm

      common / par /                                                    &
     & a_la,b_la,a_co,b_co,a_fr,b_fr,a_hi,                              &
     & b_hi,a_s11,b_s11,a_s8,b_s8,parm(12)

      save :: /par/

!     + + + VARIABLE DEFINITIONS + + +
! dlax1 - fraction of grwing season (1st x-coordinate in lai s-curve)
! dlay1 - fraction of maximum lai (1st y-coordinate in the lai s-curve)
! dlax2 - fraction of grwing season (2nd x-coordinate in lai s-curve)
! dlay2 - fraction of maximum lai (2nd y-coordinate in the lai s-curve)
! wacx1 - co2 concentration(ppm) (1st x-coordinate in the biomass conversion efficiency s-curve)
! wacy1 - biomass conversion efficiency (kg/ha/mj) (1st y-coordinate in the biomass efficiency s-curve)
! wacx2 - co2 concentration(ppm) (2nd x-coordinate in the biomass conversion efficiency s-curve)
! wacy2 - biomass conversion efficiency (kg/ha/mj) (2nd y-coordinate in the biomass efficiency s-curve)
! frsx1 - minimum temperature below zero (c) (1st x-coordinate in the frost damage s-curve)
! frsy1 - fraction of biomass lost each day due to frost (1st y-coordinate in the frost damge s-curve)
! frsx2 - minimum temperature below zero (c) (2nd x-coordinate in the frost damage s-curve)
! frsy2 - fraction of biomass lost each day due to frost (2nd y-coordinate in the frost damge s-curve)
! a_la - parameter in the lai development s-curve
! b_la - parameter in the lai development s-curve
! a_co - parameter in the biomass conversion efficiency s-curve
! b_co - parameter in the biomass conversion efficiency s-curve
! a_fr - parameter in the frost damage s-curve
! b_fr - parameter in the frost damage s-curve
! a_hi - parameter in the harvest index s-curve
! b_hi - parameter in the harvest index s-curve
! a_s11 - parameter in P uptake eqn. (corresponding to scrp(11,1) in epic)
! b_s11 - parameter in P uptake eqn. (corresponding to scrp(11,2) in epic)
! a_s8 - parameter in N or P stress eqn. (corresponds to scrp(8,1) in epic)
! b_s8 - parameter in N or P stress eqn. (corresponds to scrp(8,2) in epic)
