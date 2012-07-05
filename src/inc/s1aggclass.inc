!$Author$
!$Date$
!$Revision$
!$HeadURL$
!     This common block contains the variables representing the definitions
!     of soil aggregate distributions for each soil layer.

!     Parameter include files:  p1werm.inc

!     + + + VARIABLE DECLARATIONS + + +

      integer    nclass
      parameter (nclass = 30)

      real aggclass_size(0:nclass)
      real aggclass_mdia(nclass)
      real aggclass_frac(nclass, mnsz)
      real aggclass_cumul(0:nclass, mnsz)

      common / s1aggclass /                                             &
     & aggclass_size, aggclass_mdia, aggclass_frac, aggclass_cumul

      save :: /s1aggclass/

!     + + + VARIABLE DEFINITIONS + + +
!     nclass         - number of size classes
!     aggclass_size  - size of class divisions (mm)
!     aggclass_mdia  - geometric mean diameter of size classes (mm)
!     aggclass_frac  - mass fraction by size class and soil layer
!     aggclass_cumul - cummulative mass fraction by class and soil layer
