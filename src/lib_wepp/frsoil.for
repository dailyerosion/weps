!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine frsoil(nsl,sscunf,LNfrst,ssc,sscv,dg,kfactor,slsic,    &
     &     saxfc,saxwp,saxA,saxB,saxpor,saxenp,saxks)
!
!     +++PURPOSE+++
!
!     The purpose of this program is to estimate saturated hydraulic conductivity
!     when frost exists.
!     We treat ice as air for saturated hydralic conductivity calculation.
!     Then the saturated K with ice would be as if the unsaturated K with 
!     liqiud water content = porosity - ice water content 
!     
!     Author(s):  Shuhui Dun, WSU
!     Date: 02/28/2008
!     Verified by: Joan Wu, WSU
!
!     +++PARAMETERS+++
 
!     +++ARGUMENT DECLARATIONS+++
      real, intent(in) ::  sscunf(*),dg(*)
      real, intent(in) :: kfactor
      integer, intent(in) :: nsl,LNfrst
      real, intent(in) :: saxfc(*),saxwp(*),saxA(*),saxB(*),saxpor(*)
      real, intent(in) :: saxenp(*),saxks(*),slsic(*)
      real, intent(out) :: ssc(*), sscv(*)
!
!     +++ARGUMENT DEFINITIONS+++
!    
!     sscunf - unfrozen saturated hydraulic conductivity (SSC)
!
!     +++COMMON BLOCKS+++
!
!
!     +++LOCAL VARIABLES+++
!
      integer  frstn,i,j,jend
      real     slks(100)
      real     varsm,varwtp,varkus,vardp,tmpvr1,tmpvr2
!
!     +++LOCAL DEFINITIONS+++
!
!     frstn - frost existing fine layer number in a soil layer
!     slks -  satuareated hydraulic conductivity of a fine layer m/s.
!
!     varsm  - soil moisture variable
!     varwtp - water potential variable
!     varkus - unsaturated K varible
!
!     tmpvr1 - variable for mathmatic mean
!     tmpvr2 - variable for harmonic mean
!
!     +++DATA INITIALIZATIONS+++
!
!     +++END SPECIFICATIONS+++
!
      Do 10 i = 1, nsl
!
      if (i.gt.LNfrst) then
!        deeper than the frost bottom
         ssc(i) = sscunf(i)
         sscv(i) = sscunf(i)
      else
!        in frost zone
         tmpvr1 = 0.
         tmpvr2 = 0.
         vardp = dg(i)

!        Estimate unsaturated hydraulic conductivity of a soil
!        using Saxton and Rawls, 2006
!
         if( slsic(i) .gt. 0.001) then
!            frost exists
!
!                as if soil water content at     
             varsm = saxpor(i) - slsic(i)/vardp
!
!                kfactor = 1E-5
!                 kfactor = 0.5
!
             if (varsm .le. 0.01) then
!                forst heave
                  slks(i) = kfactor*sscunf(i)
             else
                  call saxfun(i,varsm,varwtp, varkus,                   &
     &                 saxfc,saxwp,saxA,saxB,saxpor,saxenp,saxks)
                  if ((varkus/sscunf(i)).lt.kfactor) then
                     slks(i) = kfactor*sscunf(i)
                  else
                     slks(i) = varkus
                  endif
             endif
!
          else
!             no frost
             slks(i) = sscunf(i)
          endif
!              
          tmpvr1 = tmpvr1 + vardp*slks(i)
          tmpvr2 = tmpvr2 + vardp/slks(i)

!
         ssc(i) = tmpvr1/dg(i)
         sscv(i) = dg(i)/tmpvr2
!
      endif
10    continue
!
      return
      end
