!$Author$
!$Date$
!$Revision$
!$HeadURL$

!     This block common contains local variables used to store
!     variables needed to calculate the soil conditioning index (sci).

!     Parameter include files:  p1werm.inc

!     + + + VARIABLE DECLARATIONS + + +

      real             allbiomass_sum(mnsub)
      real             allerosion_sum(mnsub)
      integer          days_sum(mnsub)
      real             stir_sum(mnsub)
      real             energy_sum(mnsub)

      common / sci_report_var /                                         &
     &      allbiomass_sum, allerosion_sum, days_sum,                   &
     &      stir_sum, energy_sum

      save :: /sci_report_var/

!     + + + VARIABLE DEFINITIONS + + +

!    allbiomass_sum - sum of residue biomass from all pools over all days (kg/m^2)
!    allerosion_sum - sum of all erosion loss from field over all days (kg/m^2)
!    days_sum       - number of days summed

