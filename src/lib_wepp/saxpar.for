!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine saxpar(sand,clay,orgmat,nsl,saxwp,saxfc,saxenp,saxpor, &
     &                  saxA,saxB,saxks)
!
!     + + + PURPOSE + + +
!
!     Estimate Saxton&Rawl equation parameters for a soil
!
!     Called from: SR WINIT
!     Author(s): Shuhui Dun, WSU
!     Reference in User Guide: Saxton K.E. and Rawls W.J., 2006. 
!     Soil water characteristics estimates by texture and organic matter for hydraologic solution.
!     Soil SCI. SOC. AM. J., 70, 1569--1578

!     After viewing the web page by Saxton http://hrsl.ba.ars.usda.gov/soilwater/Index.htm
!     the minimum clay content modeled is around 5%, making the maximum sand content around 95%
!     It also shows the maximum clay content to be around 60%. Clamping the input values to
!     remain within these ranges, and adjusting the other components accordingly would prevent
!     out of range errors such as wilting point becoming less than zero, but does not answer the
!     question of what the values really should be for these extremes. For now, the wilting point
!     value is prevented from going to zero.

!     Version: 2008.
!     Date recoded: Febuary 19, 2008
!     Verified by: Joan Wu, WSU 
!
      real, intent(in) :: sand(*),clay(*),orgmat(*)
      integer, intent(in) :: nsl
      real, intent(out) :: saxwp(*),saxfc(*),saxenp(*)
      real, intent(out) :: saxpor(*),saxA(*),saxB(*),saxks(*)
      
!     Saxton K.E. and Rawls W.J., 2006. Soil water characteristics estimates 
!     by texture and organic matter for hydraologic solution.
!     Soil SCI. SOC. AM. J., 70, 1569--1578
!
!     saxwp: 1500 kpa soil water content (wilting point)
!     saxfc: 33 kpa soil water content (field capacity)
!     saxpor: saturated water content
!     saxenp: air entry pressure (kpa) 
!     saxA, saxB : moisture tension equition coefficients
!     saxks: saturated hydraulic conductivity (m/s) 
      
!
!
!     + + + LOCAL VARIABLES + + +
!
      integer i
      real sw1500, sw33, sws33, s33, spaen
!
!     + + + LOCAL DEFINITIONS + + +
!
!     sw1500: first solution 1500 kpa soil moisture
!     sw33: first solution 1500 kpa soil moisture
!     sws33:first solution SAT-33 kpa soil moisture
!     s33:  moisture SAT-33 kpa, normal density
!      spaen: first solution air entry tension, kpa
!
!     + + + SAVES + + +
!
!     + + + SUBROUTINES CALLED + + +
!
!
!     + + + DATA INITIALIZATIONS + + +
!      
!     + + + END SPECIFICATIONS + + +
!
      do 10 i = 1, nsl
!      eqation 1 
         sw1500 = - 0.024*sand(i)                                       &
     &            + 0.487*clay(i)                                       &
     &            + 0.006*orgmat(i)                                     &
     &            + 0.005*sand(i)*orgmat(i)                             &
     &            - 0.013*clay(i)*orgmat(i)                             &
     &            + 0.068*sand(i)*clay(i)                               &
     &            + 0.031
!
          saxwp(i) = max( 1.0e-5, (sw1500 + 0.14*sw1500 - 0.02) )
!
!      equation 2
         sw33 =   - 0.251*sand(i)                                       &
     &            + 0.195*clay(i)                                       &
     &            + 0.011*orgmat(i)                                     &
     &            + 0.006*sand(i)*orgmat(i)                             &
     &            - 0.027*clay(i)*orgmat(i)                             &
     &            + 0.452*sand(i)*clay(i)                               &
     &            + 0.299
!
          saxfc(i) = sw33 + 1.283*sw33**2 - 0.374*sw33 - 0.015
!
!      equation 3
         sws33 =  + 0.278*sand(i)                                       &
     &            + 0.034*clay(i)                                       &
     &            + 0.022*orgmat(i)                                     &
     &            - 0.018*sand(i)*orgmat(i)                             &
     &            - 0.027*clay(i)*orgmat(i)                             &
     &            - 0.584*sand(i)*clay(i)                               &
     &            + 0.078
!
          s33 = sws33 + 0.636*sws33 - 0.107
!
!      eqation 4
          spaen =  - 21.67*sand(i)                                      &
     &            - 27.93*clay(i)                                       &
     &            - 81.97*s33                                           &
     &            + 71.12*sand(i)*s33                                   &
     &            +  8.29*clay(i)*s33                                   &
     &            + 14.05*sand(i)*clay(i)                               &
     &            + 27.16
!
          saxenp(i) = spaen + 0.02*spaen**2 - 0.113*spaen - 0.70
!
!     equation 5
          saxpor(i) = saxfc(i) + s33                                    &
     &                       - 0.097*sand(i) + 0.043
!
!     eqation 14 and 15
            saxB(i) = (log(1500.) - log(33.))/                          &
     &                     (log(saxfc(i)) - log(saxwp(i)))
            saxA(i) = exp (log(33.) +                                   &
     &                          saxB(i)*log(saxfc(i)))
!
!     equation 16
!     The unit of the original saxton ans Rawls is mm/hr.
!     The factor 1./3.6e+6 converts mm/hr to m/s
          saxks(i) = 1930.*(saxpor(i) - saxfc(i))                       &
     &                      **(3. - 1./saxB(i))*1.0/3.6E+6
10    continue
!
      return
      end
