!$Author$
!$Date$
!$Revision$
!$HeadURL$
!**********************************************************************
!     subroutine sbwus
!**********************************************************************
      subroutine sbwus (anemht, awzzo, awu, wzzov, brcd, wus)
!
!     +++ PURPOSE +++
!     To calculate subregion, friction velocity, given station
!     anemometer height, surface roughness, wind speed; and subregion
!     aerodynamic roughness.
!
!     if standing biomass present, then calculate friction velocity
!     at surface below the canopy (wus).
!
!     +++ ARGUMENT DECLARATIONS +++
      real  anemht, awzzo, awu, wzzov
      real  brcd, wus
!
!     +++ ARGUMENT DEFINITIONS +++
!
!     anemht - parameter, anemometer height of input wind speed (m).
!     awzzo - parameter, surface aerodynamic roughness at input wind
!             speed location (mm).
!     awu - input wind speed driving EROSION submodel (m/s).
!     wzzov - subregion aerodynamic roughness (mm).
!     brcd - biomass drag coefficient
!     wus - subregion soil surface friction velocity (m/s)
!           i.e. below canopy, if one exists.
!
!     +++ LOCAL VARIABLES +++
      real wusst, wusv
!
!     +++ END SPECIFICATIONS +++
!     note:  in BLOCK.FOR wzoflg should be set to 1 and anemomht
!             set to correct height if anemometer is at field site
!             to obtain correct values from SBWUS or read as
!             input data in stand-alone EROSION.
!
!     Calc station (input wind speed location) friction velocity
      wusst = awu*0.4/alog(anemht*1000./awzzo)
!
!     calc subregion friction velocity
      wus = wusst * (wzzov/awzzo)**0.067
!
!     if standing biomass, calculate wus below canopy
      if (brcd .gt. 0.0001 ) then
         wusv = wus
!
!        calculate friction velocity below canopy

        if( brcd.gt.2.56) then       !check to avoid underflow
            wus = wusv * 0.25*exp(-brcd/0.356)
        else
            wus = wusv*(0.86*exp(-brcd/0.0298)+0.25*exp(-brcd/0.356))
        endif
         wus = amin1(wus,wusv)
      endif
!
      return
      end
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

