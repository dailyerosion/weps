!$Author$
!$Date$
!$Revision$
!$HeadURL$
      real function soilrelhum(theta, thetaw, theta80rh, soiltemp,      &
     &                           matricpot)
!     returns the soil relative humidity using approximation of water
!     adsorption isotherms on clay minerals by Berge, H.F.M. ten, 1990

!*** Argument declarations ***
      real*4 theta, thetaw, theta80rh, soiltemp, matricpot
!     theta      - present volumetric water content
!     thetaw     - volumetric water content at wilt (15 bar or 1.5 MPa)
!     theta80rh  - volumetric water content at %80 relative humidity (300 bar or 30 MPa)
!     soiltemp   - soil temperature (C)
!     matricpot  - matric potential (meters of water) corresponding to theta
!                  only used if theta greater than thetaw

!*** Include files ***
      include 'hydro/vapprop.inc'

!*** local declarations ***
      real relhumwilt, mintheta
      parameter(mintheta = 1.0e-37)

      if( theta .le. mintheta ) then
          soilrelhum = 0.8*mintheta/theta80rh
      else if( theta .lt. theta80rh ) then
          soilrelhum = 0.8*theta/theta80rh
      else if( theta .le. thetaw ) then
!         find the relative humidity corresponding to thetaw (15 bar)
          relhumwilt = exp( (potwilt * molewater * gravconst)           &
     &               / (rgas * (soiltemp + zerokelvin)) )
          soilrelhum = 0.8+(relhumwilt - 0.8)                           &
     &               * ( (theta-theta80rh)/(thetaw-theta80rh) )
      else if( matricpot.le.0.0) then
          soilrelhum = exp( (matricpot * molewater * gravconst)         &
     &               / (rgas * (soiltemp + zerokelvin)) )
      else
          soilrelhum = 1.0
      endif
      return
      end
