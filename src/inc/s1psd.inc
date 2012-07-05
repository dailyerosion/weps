!$Author$
!$Date$
!$Revision$
!$HeadURL$
!     ------------------------------------------------------------------
!     file 's1psd.inc'
!
!     This common block contains the variables representing the soil
!     particle distribution parameters for each soil layer.
!
!     Parameter include files:  p1werm.inc
!
!     + + + VARIABLE DECLARATIONS + + +
!
      real             aslsgm(mnsz, mnsub)
      real             as0sgs(mnsz, mnsub)
!
!     + + + VARIABLE DEFINITIONS + + +
!
      common / s1psd /                                                  &
     &                 aslsgm, as0sgs

      save :: /s1psd/
!
!     aslsgm - Soil layer particle geometric mean dia. (mm)
!
!              range values       min        max
!              ------------   ---------  ---------
!              fixed          fmin       fmax
!              typical        tmin       tmax
!              ------------   ---------  ---------
!
!            - This variable contains the value of the geometric mean
!              diameter parameter describing the particle size
!              distribution in the soil layers for each of the
!              subregions.
!
!     as0sgs - Soil layer particle size geometric standard dev. (mm)
!
!              range values       min        max
!              ------------   ---------  ---------
!              fixed          fmin       fmax
!              typical        tmin       tmax
!              ------------   ---------  ---------
!
!            - This variable contains the value of the geometric mean
!              standard deviation parameter describing the particle
!              size distribution in the soil layers for each of the
!              subregions.
!
!     ------------------------------------------------------------------

