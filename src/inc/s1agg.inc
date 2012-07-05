!$Author$
!$Date$
!$Revision$
!$HeadURL$
!     This common block contains the variables representing the soil
!     aggregate properties for each soil layer.

!     Parameter include files:  p1werm.inc

!     + + + VARIABLE DECLARATIONS + + +

      real             asdagd(0:mnsz, mnsub)
      real             aseags(0:mnsz, mnsub)
      real             aslagm(0:mnsz, mnsub)
      real             aslagn(0:mnsz, mnsub)
      real             aslagx(0:mnsz, mnsub)
      real             as0ags(0:mnsz, mnsub)
      real             aseagm(mnsz, mnsub)
      real             aseagmn(mnsz, mnsub)
      real             aseagmx(mnsz, mnsub)
      real             ask4d(mnsz, mnsub)
      real             aslmin(mnsz, mnsub)
      real             aslmax(mnsz, mnsub)

      common / s1agg /                                                  &
     & asdagd, aseags, aslagm, aslagn, aslagx, as0ags,                  &
     & aseagm, aseagmn, aseagmx, ask4d, aslmin, aslmax

      save :: /s1agg/

!     + + + VARIABLE DEFINITIONS + + +
!     for each subregion, the following variables are defined as:

!     asdagd - Soil layer aggregate density (Mg/m^3)
!     aseags - Soil layer dry aggregate stability (J/m^2)
!     aslagm - Soil layer aggregate size geometric mean dia. (mm)
!     aslagn - Minimum aggregate size (mm)
!     aslagx - Maximum aggregate size (mm)
!     as0ags - Soil layer aggregate size geometric standard dev. (mm)
!     aseagm - soil layer mean aggregate stabillity (J/m^2)
!     aseagmn - soil layer minimum aggregate stability
!     aseagmx - soil layer maximum aggregate stability
!     k4d    - soil layer drying stability coefficient
!     aslmin - min values of geom. mean agg. diameter (eq. S-45, S-46)
!     aslmax - max values of geom. mean agg. diameter (eq. S-45, S-46)
