!$Author$
!$Date$
!$Revision$
!$HeadURL$

       real function depstore( ranrough, soilslope, bhzoutflow )

!     + + + PURPOSE + + +
!     returns the maximum depression storage depth (m)
!     equation from WEPP 4.3.4 reference to 

!     + + + KEY WORDS + + +
!     hydrology

!     + + + ARGUMENT DECLARATIONS + + +
      real ranrough, soilslope, bhzoutflow

!     + + +  ARGUMENT DEFINITIONS + + +
!     ranrough   - random roughness of soil surface (m)
!     soilslope  - slope of soil surface (m/m)
!     bhzoutflow - height of runoff outlet above field surface (m)

!     + + + LOCAL VARIABLES + + +
      real coef_a, coef_b, coef_c
      parameter( coef_a = 0.112 )
      parameter( coef_b = 3.1 )
      parameter( coef_c = -1.2 )

      depstore = max( bhzoutflow, ranrough                              &
     &         * (coef_a + coef_b * ranrough + coef_c * soilslope))

      return
      end
