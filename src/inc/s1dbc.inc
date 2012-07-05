!$Author$
!$Date$
!$Revision$
!$HeadURL$
!     ------------------------------------------------------------------
!     file 's1dbc.inc'

!     This common block contains the variables representing the soil
!     chemical properties in WERM for each soil layer.

!     Parameter include files:  p1werm.inc

!     + + + VARIABLE DECLARATIONS + + +

      real             as0ph(mnsz, mnsub)
      real             ascmg(mnsz, mnsub)
      real             ascna(mnsz, mnsub)
      real             asfcce(mnsz, mnsub)
      real             asfcec(mnsz, mnsub)
      real             asfesp(mnsz, mnsub)
      real             asfom(0:mnsz, mnsub)
      real             asfnoh(mnsz,mnsub)
      real             asfpoh(mnsz,mnsub)
      real             asfpsp(mnsz,mnsub)
      real             asfsmb(mnsz,mnsub)
      real             asftap(mnsz,mnsub)
      real             asftan(mnsz,mnsub)
      real             asmno3(mnsub)
      real             asrsar(mnsz,mnsub)
      real             as0ec(mnsz,mnsub)         ! Not used - not read in Version 1.0 IFC file


      common / s1dbc /                                                  &
     &                 as0ph, ascmg, ascna, asfcce, asfcec, asfesp,     &
     &                 asfom, asfnoh, asfpsp, asfpoh, asfsmb,           &
     &                 asftan, asftap, asmno3, asrsar, as0ec

      save :: /s1dbc/

!     + + + VARIABLE DEFINITIONS + + +


!     as0ph  - Soil layer pH

!              range values       min        max
!              ------------   ---------  ---------
!              fixed            0.0       14.0
!              typical        tmin       tmax
!              ------------   ---------  ---------

!            - This variable contains the soil pH for each of the
!              subregions.

!     ascmg  - Soil layer magnesium ion concentration (cmol/kg)

!              range values       min        max
!              ------------   ---------  ---------
!              fixed          fmin       fmax
!              typical        tmin       tmax
!              ------------   ---------  ---------

!            - This variable contains the soil layer magnesium ion
!              concentration for each of the subregions.

!     ascna  - Soil layer sodium ion concentration (cmol/kg)

!              range values       min        max
!              ------------   ---------  ---------
!              fixed          fmin       fmax
!              typical        tmin       tmax
!              ------------   ---------  ---------

!            - This variable contains the soil layer sodium ion
!              concentration for each of the subregions.

!     asfcec - Soil layer cation exchange capacity (cmol/kg) (meq/100g)

!              range values       min        max
!              ------------   ---------  ---------
!              fixed          fmin       fmax
!              typical        tmin       tmax
!              ------------   ---------  ---------

!            - This variable contains the soil layer cation
!              exchange capacity for each of the subregions.

!     asfcce - Soil layer calcium carbonate equivalent (dec %)

!              range values       min        max
!              ------------   ---------  ---------
!              fixed          fmin       fmax
!              typical        tmin       tmax
!              ------------   ---------  ---------

!            - This variable contains the soil layer calcium
!              carbonate equivalent for each of the subregions.

!     asfesp - Soil layer exchangeable sodium percentage (dec %)

!              range values       min        max
!              ------------   ---------  ---------
!              fixed          fmin       fmax
!              typical        tmin       tmax
!              ------------   ---------  ---------

!            - This variable contains the soil layer exchangeable
!              sodium percentage for each of the subregions.

!     asfom  - Soil layer organic matter content (Mg/Mg)

!              range values       min        max
!              ------------   ---------  ---------
!              fixed          fmin       fmax
!              typical        tmin       tmax
!              ------------   ---------  ---------

!            - This variable contains the soil layer organic matter
!              content for each of the subregions.

!     asfnoh - Soil layer organic N content of the humus (mg/kg)

!              range values       min        max
!              ------------   ---------  ---------
!              fixed          fmin       fmax
!              typical        tmin       tmax
!              ------------   ---------  ---------

!            - This variable contains the organic N content of the
!              humus for each of the subregions.

!     asfpoh - Soil layer organic P content of the humus (mg/kg)

!              range values       min        max
!              ------------   ---------  ---------
!              fixed          fmin       fmax
!              typical        tmin       tmax
!              ------------   ---------  ---------

!            - This variable contains the organic P content of the
!              humus for each of the subregions.

!     asfpsp - Fraction of fertilizer P that is labile (dec.)

!              range values       min        max
!              ------------   ---------  ---------
!              fixed          fmin       fmax
!              typical        tmin       tmax
!              ------------   ---------  ---------

!            - This variable contains the fraction of fertilizer P that
!              is labile by layer for each of the subregions.

!     asfsmb - Sum of bases (cmol/kg)

!              range values       min        max
!              ------------   ---------  ---------
!              fixed          fmin       fmax
!              typical        tmin       tmax
!              ------------   ---------  ---------

!            - This variable contains the sum of bases by layer
!              for each of the subregions.

!     asftap - Soil layer available Phosphorus (kg/ha)

!              range values       min        max
!              ------------   ---------  ---------
!              fixed          fmin       fmax
!              typical        tmin       tmax
!              ------------   ---------  ---------

!            - This variable contains the soil layer available Phosphorus
!              content from all sources for each of the subregions.

!     asftan - Soil layer available Nitrogen (kg/ha)

!              range values       min        max
!              ------------   ---------  ---------
!              fixed          fmin       fmax
!              typical        tmin       tmax
!              ------------   ---------  ---------

!            - This variable contains the soil layer available Nitrogen
!              content from all sources for each of the subregions.

!     asmno3  - Amount of N03 Nitrogen applied as fertilizer (kg/ha)

!              range values       min        max
!              ------------   ---------  ---------
!              fixed          fmin       fmax
!              typical        tmin       tmax
!              ------------   ---------  ---------

!            - This variable contains the NO3 Nitrogen applied as
!              fertilizer for each of the subregions.

!     asrsar - Soil layer sodium adsorption ratio (cmol/kg)

!              range values       min        max
!              ------------   ---------  ---------
!              fixed          fmin       fmax
!              typical        tmin       tmax
!              ------------   ---------  ---------

!            - This variable contains the soil layer sodium
!              adsorption ratio for each of the subregions.

!     as0ec  - Soil layer electrical conductivity (S/m)

!              range values       min        max
!              ------------   ---------  ---------
!              fixed          fmin       fmax
!              typical        tmin       tmax
!              ------------   ---------  ---------

!            - This variable contains the soil layer sodium
!              adsorption ratio for each of the subregions.
!     ------------------------------------------------------------------
