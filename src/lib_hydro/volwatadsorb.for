!$Author$
!$Date$
!$Revision$
!$HeadURL$

      real function volwatadsorb(bulkden, clayfrac, orgfrac,            &
     &                             claygrav80rh, orggrav80rh )
!     computes the volumetric water content of the soil at 80 percent
!     relative humidity based on basic soil properties and the clay
!     adsorption isotherms on clay minerals by Berge, H.F.M. ten, 1990
!     with the addition of the organic matter isotherm from Rutherford
!     and Chlou, 1992

!*** Argument declarations ***
      real bulkden, clayfrac, orgfrac, claygrav80rh, orggrav80rh
!     bulkden        - bulk density of the soil (kg/m^3)
!     clayfrac       - fraction of the mineral soil which is clay (kg/kg)
!     orgfrac        - fraction of the total soil which is organic (kg/kg)
!     claygrav80rh   - Gravimetric water content of clay at 0.8 relative
!                      humidity (parameter A in reference)
!     orggrav80rh    - Gravimetric water content of organics at 0.8 relative
!                      humidity (parameter A in reference)
!*** Include files ***
      include 'hydro/vapprop.inc'

      volwatadsorb = (bulkden  / denwat)                                &
     &             * ( clayfrac * (1-orgfrac)*claygrav80rh              &
     &             + orgfrac * orggrav80rh )
      return
      end

!     not used but retained if needed

!      real function volwat_rh( relhum, theta80rh, thetaw, soiltemp )
!     returns the volumetric water content of the soil based on
!     the relative humidity using the approximation of water adsorption
!     isotherms on clay minerals by Berge, H.F.M. ten, 1990

!*** Argument declarations ***
!      real relhum, theta80rh, thetaw, soiltemp
!     theta      - present volumetric water content
!     thetaw     - volumetric water content at wilt (15 bar or 1.5 MPa)
!     theta80rh  - volumetric water content of soil at 0.8 relative humidity
!     soiltemp   - soil temperature (C)

!*** local declarations ***
!      real relhumwilt

!      if( relhum .le. 0.8 ) then
!          volwat_rh = theta80rh*relhum/0.8
!      else if( relhum .lt. 1.0 ) then
!          relhumwilt = exp( (potwilt * molewater * gravconst)
!     &               / (rgas * (soiltemp + zerokelvin)) )
!          if( relhum.lt.relhumwilt ) then
!              volwat_rh = theta80rh+(thetaw-theta80rh)
!     &                  * (relhum-0.8)/(relhumwilt-0.8)
!          else
!              volwat_rh = thetaw
!          endif
!      endif
!      return
!     end
