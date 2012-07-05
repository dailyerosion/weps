!$Author$
!$Date$
!$Revision$
!$HeadURL$

!     This file contains the declaration of the common block of variables
!     related to conversion of biomass.

!     Parameter include files:  p1werm.inc

!     + + + VARIABLE DECLARATIONS + + +
      integer ir, je, daye,dtm , iprint
      real dm,ddm,slai0,                                                &
     & rw,rwt,u,su,yld,                                                 &
     & pdm,strs,prw,pdmag,pclfwt,pcstwt,pcrpwt,pclfarea,pcstarea,pslai, &
     & pssai,pcht,pchty,clfwt,cstwt,crpwt,clfarea,cstarea,ssai,         &
     & parea,pclfwtx,clfwtx,slai, dmag,                                 &
     & clfwtb,cstwtb,crpwtb,dmagb,clfwty,cstwty,crpwty,dmagy,clfwts,    &
     & cstwts,crpwts,dmags, prd, prdy, slaix, ssaix

      common / grow / dm,ddm,slai0,iprint,                              &
     & ir,rw,rwt(mnsz),u(mnsz),su,yld,je,                               &
     & pdm,strs,prw,pdmag,pclfwt,pcstwt,pcrpwt,pclfarea,pcstarea,pslai, &
     & pssai,pcht,pchty,clfwt,cstwt,crpwt,clfarea,cstarea,ssai,         &
     & parea, pclfwtx,clfwtx, slai, dmag,                               &
     & clfwtb,cstwtb,crpwtb,dmagb,clfwty,cstwty,crpwty,dmagy,clfwts,    &
     & cstwts,crpwts,dmags, prd, prdy, slaix, ssaix, daye, dtm

      save :: /grow/

!     + + + VARIABLE DEFINITIONS + + +
!     clfarea - green leaf area per plant
!     clfwt - leaf mass (t/ha)
!     clfwtb - periodic leaf mass (t/ha)
!     clfwts - seasonal leaf mass (t/ha)
!     clfwty - periodic leaf mass (t/ha)
!     cstwt - stem mass (t/ha)
!     cstwtb - periodic stem mass (t/ha)
!     cstwts - seasonal stem mass (t/ha)
!     cstwty - periodic stem mass (t/ha)
!     crpwt - rprd (reproductive) mass (t/ha)
!     crpwtb - periodic rprd mass (t/ha)
!     crpwts - seasonal rprd mass (t/ha)
!     crpwty - periodic rprd mass (t/ha)
!     daye - days after emergence
!     dm - stress adjusted cummulated biomass (t/ha)
!     ddm - stress adjusted daily biomass production (t/ha)
!     dmag - stress adjusted cumulated aboveground biomass (t/ha)
!     dmags - seaonal above-ground biomass (t/ha)
!     dmagy - periodic above-ground biomass (t/ha)
!     drwt - daily root weight (t/ha)
!     dtm - days from planting to maturity
!     iprint - flag to print end-of-season values: 10/6/99
!     ir - deepest layer # to which roots have grown
!     parea - ground area per plant (cm^2)
!     pcht - potentail plant height (m)
!     pchty - potentail plant height on a previous day (m)
!     pclfarea - potential leaf area per plant (cm^2)
!     pclfwt - potential leaf mass (t/ha)
!     pclfwtx - potential leaf mass (t/ha)
!     pcrpwt - potential rprd (reproductive) mass (t/ha)
!     pddm - potential daily biomass production (t/ha)
!     pdm - potential cummulated biomass production (t/ha)
!     prd - potential root depth (m)
!     prdy - potential root depth on day i-1 (m)
!     qslai - same as slai (dummy variable)
!     qdmag - same as dmag (dummy variable)
!     rd - root depth (m)
!     rprwt  - reproductive weight (t/ha)
!     rw - total root weright (t/ha)
!     rwt - total root weight by layer (t/ha)
!     slai0 - leaf area index when leaf senescence starts
!     slaix - maximum actual leaf area index attained
!     ssaix - maximum actual stem area index
!     yld - crop yield (t/ha)

!     +++ No longer used +++
!     wavp - parameter relating vapor pressure deficit to be
