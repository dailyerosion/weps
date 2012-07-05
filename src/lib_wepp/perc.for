!$Author$
!$Date$
!$Revision$
!$HeadURL$

!**********************************************************************
!  Returns percolation; ie, seepage (SEP) from the bottom of the
!     current soil layer (into the layer below) when field capacity
!     in the current layer is exceeded.  Correction is made for
!     saturation of the layer below.
!**********************************************************************

      subroutine perc(vv, k1, nsl, st, ul, hk, ssc, sep)

      implicit none

      integer, intent(in) :: k1, nsl
      real, intent(in) :: vv, st(*), ul(*), hk, ssc

      real, intent(inout) :: sep

!**********************************************************************
!   vv -  Water excess beyond field capacity for the current soil layer (m)
!   k1 - counter in main loop.
!   nsl - number of soil layers
!   st - current available water content per soil layer (m)
!   ul - upper limit of water content per soil layer (m)
!   hk -  a parameter that causes SC approach zero as soil water approaches FC
!   ssc - saturated hydraulic conductivity (m/s)
!   sep -  seepage (m)
!**********************************************************************

!     + + + LOCAL VARIABLES + + +
      real stz, fx, stu, cr, zz, funzz

!     + + + LOCAL DEFINITIONS + + +
!     stz    - percent saturation (expressed as a fraction)
!     fx     - correction factor for sat. hyd. cond. for unsat. soil
!              (equation 7.4.3)
!     stu    - percent saturation (fraction) of lower layer
!     cr     - correction factor for lower layer saturation
!              (equation 7.4.5)
!     zz     - travel time of water through the layer (days)
!              (a part of equation 7.4.19 -delta t/ti)
!     funzz  - a part of equation 7.4.1, 1-exp(-delta t/ti) but
!              linear form
!     sscz   - (not needed)


!      Compute percent saturation (fraction) in the current layer.
      stz = st(k1) / ul(k1)
      if (stz.lt.0.95) then
        fx = stz ** hk
        if (fx.lt.0.002) fx = 0.002
      else
        fx = 1.
      end if

!     Adjust the percolation rate for the saturation of the soil
!     layer below the current one.  (Chapter 7, equation 7.4.3)

!     Compute percent saturation (fraction) in the layer below.
      if (k1.lt.nsl) then
        stu = st(k1+1) / ul(k1+1)
        if (stu.ge.0.95) stu = 0.95
      else
        stu = 0.
      end if

      if (stu.lt.1.0) then
!       Correct for lower level saturation.  (Chapter 7, eq. 7.4.5)
        cr = sqrt(1.-stu)
!       Travel time of water (days) through the layer
        zz = 86400. * fx * ssc / vv

        if (zz.le.10.) then
!         Note: For positive values of ZZ, FUNZZ starts at 1.0, and
!         approaches a lower limit of zero at positive infinity.
!         (Chapter 7, equation 7.4.1)
          funzz = exp(-zz)
          sep = vv * (1.0-funzz) * cr
        else
!         If time > 10 days, FUNZZ approaches zero.
          sep = vv * cr
        end if
      else
        ! If lower level is saturated, there is no seepage
        ! from the current level....
        sep = 0.0
      end if

      return
      end subroutine
