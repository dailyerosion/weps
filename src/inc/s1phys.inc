!$Author$
!$Date$
!$Revision$
!$HeadURL$
!     ------------------------------------------------------------------
!     file 's1phys.inc'

!     This common block contains the state variables relating to
!     some physical soil and layer properties.

!     Parameter include files:  p1werm.inc

!     + + + VARIABLE DECLARATIONS + + +

      real             asdblk(0:mnsz, mnsub)
      real             asdsblk(mnsz, mnsub)
      real             asdwblk(mnsz, mnsub)
      real             asdblk0(mnsz, mnsub)
      real             asdpart(mnsz, mnsub)
      real             restrict_depth(mnsub)
      real             bedrock_depth(mnsub)

      common / s1phys /                                                 &
     &    asdblk, asdsblk, asdwblk, asdblk0, asdpart,                   &
     &    restrict_depth, bedrock_depth 

      save :: /s1phys/

!     + + + VARIABLE DEFINITIONS + + +

!     asdblk  - Soil layer bulk density for each subregion (Mg/m^3)

!     asdsblk - Soil layer settled bulk density (Mg/m^3)

!     asdwblk - Soil layer bulk density at 1/3 bar for each
!               subregion (Mg/m^3)

!     asdblk0 - Soil layer bulk density from previous day
!               for use in hydro to update parameters based on bulk density changes

!     asdpart - Soil layer particle density adjusted from mineral only
!               to include organic matter content

!     restrict_depth - depth to impermeable layer/restricting zone (mm)

!     bedrock_depth - depth to bedrock (mm)
