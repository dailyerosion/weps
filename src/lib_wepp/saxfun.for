!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine saxfun(lysoil,varsm,varwtp,varkus,                     &
     &  saxfc,saxwp,saxA,saxB,saxpor,saxenp,saxks)
!
!     + + + PURPOSE + + +
!
!     Estimate soil water potential and unsaturated hydraulic conductivity usingSaxton&Rawl equation
!
!     Called from: 
!     Author(s): Shuhui Dun, WSU
!     Reference in User Guide: Saxton K.E. and Rawls W.J., 2006. 
!     Soil water characteristics estimates by texture and organic matter for hydraologic solution.
!     Soil SCI. SOC. AM. J., 70, 1569--1578
!
!     Version: 2008.
!     Date recoded: Febuary 26, 2008
!     Verified by: Joan Wu, WSU
!

!
!
!     + + + KEYWORDS + + +
!
!
!     + + + ARGUMENT DECLARATIONS + + +
!
      integer, intent(in) :: lysoil
      real, intent(out) :: varwtp,varkus
      real, intent(in) :: varsm
      real, intent(in) :: saxfc(*),saxwp(*),saxA(*),saxB(*),saxpor(*)
      real, intent(in) :: saxenp(*),saxks(*)
      
!
!     + + + ARGUMENT DEFINITIONS + + +
!
!      varsm - soil water content
!      varwtp - soil water potital in meter 
!      varkus - unsaturated hydraulic conductivity (m/s)
!      lysoil - soil layer number
!
!
!
!     + + + LOCAL VARIABLES + + +
      real wtpkpa
!
!     + + + LOCAL DEFINITIONS + + +
!
!     sw1500: first solution 1500 kpa soil moisture
!     sw33: first solution 1500 kpa soil moisture
!     sws33:first solution SAT-33 kpa soil moisture
!     s33:  moisture SAT-33 kpa, normal density
!     spaen: first solution air entry tension, kpa
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
!
!
!    Estimate the water potential of the soil layer below the frozen front
!    using Saxton and Rawls, 2006
!
!
      if (varsm.lt.saxfc(lysoil)) then
!     water potential between 1500kpa and 33kpa
          if (varsm.lt.saxwp(lysoil)) then
              wtpkpa = 1500.
          else
          wtpkpa = saxA(lysoil) * varsm**(-saxB(lysoil))
          endif
!
          if (wtpkpa .gt. 1500) wtpkpa = 1500.
!
      elseif (varsm.ge.saxpor(lysoil)) then
!     Saturation
          wtpkpa = 0
      else
!     water potential between 33kpa and 0 kpa
          wtpkpa = 33.0 - (33.0 - saxenp(lysoil))*                      &
     &           (varsm - saxfc(lysoil))/                               &
     &           (saxpor(lysoil) - saxfc(lysoil))
          if (wtpkpa .lt. saxenp(lysoil)) wtpkpa = 0
      endif
!
!     Convert Kpa to meter of water
      varwtp = -wtpkpa / 10.
!
      if(varsm.lt.saxpor(lysoil)) then
          varkus = saxks(lysoil) * (varsm/saxpor(lysoil))               &
     &             **(3. + 2.0*saxB(lysoil))
      else
          varkus = saxks(lysoil)
      endif
!
!
      return
      end
