!$Author$
!$Date$
!$Revision$
!$HeadURL$

      real function heatcap(bsdblk, theta, bhfice,                      &
     &                      bsfsan, bsfsil, bsfcla, bsfom)

!     + + + PURPOSE + + +
!     This function returns the volumetric heat capacity of the soil
!     given mass fractions of the soil constituents. (J/m^3 C)

!     + + + KEYWORDS + + +
!     soil heat capacity

!     + + + ARGUMENT DECLARATIONS + + +
      real bsdblk
      real theta
      real bhfice
      real bsfsan
      real bsfsil
      real bsfcla
      real bsfom

!     + + + ARGUMENT DEFINITIONS + + +
!     bsdblk  - Soil bulk density (Mg/m^3)
!     theta   - volumetric Soil water content (m water/m soil)
!     bhfice  - mass fraction of soil water which is ice (kg ice/kg water)
!     bsfsan  - Sand mass fractions (kg clay/kg soil mineral)
!     bsfsil  - Silt mass fractions (kg clay/kg soil mineral)
!     bsfcla  - Clay mass fractions (kg clay/kg soil mineral)
!     bsfom   - Organic matter fraction (kg organic matter/kg soil)

!     + + + LOCAL COMMON BLOCKS + + +
      include 'hydro/heatcap.inc'

!     + + + LOCAL VARIABLES + + +
      real grav_wat

!     + + + LOCAL VARIABLE DEFINITION + + +
!     grav_wat - gravimetric Soil water content (Mg water/Mg soil)

!     + + + END SPECIFICATIONS + + +

      ! mass fraction weighted volumetric heat capacity based on the 
      ! method by De Vries as defined in:
      ! Kluitenberg, G.J. 2002. Heat Capacity and Specific Heat. in Dane, J.H. and
      ! Topp, G.C. eds. Methods of Soil Analysis, Part 4, Physical Methods. 
      ! Soil Science Society of America, Inc. Madison, Wisconsin, USA

      ! NOTE: (1-bsfom) gives (kg mineral soil/kg soil)
      ! air is not included

      ! convert volumetric to gravimetric
      grav_wat = theta / bsdblk

      ! units: Mg/m^3 * 1000kg/Mg * (J/(kg C)) = J/(m^3 C))
      heatcap = bsdblk * 1000.0 * ( bsfsan * (1.0-bsfom) * sandheatcap  &
     &        + bsfsil * (1.0-bsfom) * siltheatcap                      &
     &        + bsfcla * (1.0-bsfom) * clayheatcap                      &
     &        + bsfom * organheatcap                                    &
     &        + grav_wat * (1.0 - bhfice) * waterheatcap                &
     &        + grav_wat * bhfice * iceheatcap )

      return
      end