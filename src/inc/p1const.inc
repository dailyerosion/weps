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

      real      anemht, awzzo, awzdisp
      integer   wzoflg

      common /p1const/ anemht, awzzo, awzdisp, wzoflg

      save :: /p1const/

!     + + + VARIABLE DEFINITIONS + + +
!     anemht  - Standardized anemometer height (m)
!     awzzo   - Weather station aerodynamic roughness height (mm)
!     awzdisp - Weather station zero plane displacement height (mm)
!     wzoflg  - Flag = 0 for anem. and  constant awwzo at wx. stations
!               Flag = 1 for anem. and variable awwzo at field.
