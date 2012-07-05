!$Author$
!$Date$
!$Revision$
!$HeadURL$

!     These parameter variables are used in place of typical constant
!     values within WERM to make the coding more readable and
!     maintainable.  Generally, only common constants likely to be used
!     throughout the WERM coding should reside here.  Constants used
!     locally in the WERM coding should be defined in separate parameter
!     included files.
!
!     These parameter values may be consulted from within any sections
!     of the WERM code if this file has been included.

      integer   SEC_PER_DAY
      real      pi
      real      anemht, awzzo, awzdisp
      integer   wzoflg
      real      KG_per_M2_to_LBS_per_ACRE

!     + + + VARIABLE DECLARATIONS + + +

      parameter (SEC_PER_DAY = 3600*24)  !number of seconds per day

      parameter (pi      = 3.1415927)

      parameter (KG_per_M2_to_LBS_per_ACRE = 8921.791)

      common /p1const/ anemht, awzzo, awzdisp, wzoflg

      save :: /p1const/

!     + + + VARIABLE DEFINITIONS + + +

!     pi      - The constant PI (radians)
!     SEC_PER_DAY - number of seconds per day
!     KG_per_M2_to_LBS_per_ACRE - unit conversion factor
!     anemht  - Standardized anemometer height (m)
!     awzzo   - Weather station aerodynamic roughness height (mm)
!     awzdisp - Weather station zero plane displacement height (mm)
!     wzoflg  - Flag = 0 for anem. and  constant awwzo at wx. stations
!               Flag = 1 for anem. and variable awwzo at field.
