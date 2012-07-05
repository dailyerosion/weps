!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!
! ***************************************************************

      real function calctht0( bszlyd, theta, thetaw, eratio )

!     + + + PURPOSE + + +

!     calctht0 - calculate surface water content based on extrapolation

!     + + + ARGUMENT DECLARATIONS + + +

      real bszlyd(*)
      real theta(0:*)
      real thetaw(*)
      real eratio

!     + + + ARGUMENT DEFINITIONS + + +

!     bszlyd - depth of layers
!     theta  - water content (m^3/m^3)
!     thetaw - wilting point (m^3/m^3)
!     eratio - actual surface evap / potential surface evap

!     + + + LOCAL VARIABLES + + +
      real thetax
      real thetae
      real theter

!     + + + LOCAL VARIABLE DEFINITIONS + + +
!     thetax - extrapolated surface soil water content
!     thetae - equivalent water content (theta/thetaw)
!     theter - evaporation ratio volumetric water content

!     + + + CALLED FUNCTIONS + + +
      real extra

!     + + + END SPECIFICATIONS + + +

      thetax = extra(bszlyd, theta)                             !h-64,65,66

      ! constrain  extrapolation
      ! - uppper limit assumes that if surface is wet, all effect will be 
      ! reflected in surface layer values
      ! - lower limit is arbitrary, but is well below the lower limit used in erosion
      ! to indicate no more erosion prevention effect of surface moisture
      if( thetax .gt. theta(1) ) then
          thetax = theta(1)
      else if( thetax .lt. 0.1 * thetaw(1) ) then
          thetax = 0.1 * thetaw(1)
      end if

      thetae = 0.24308 + 1.37918 / (1.0+EXP(-(eratio-0.44882)/0.081))        !h-85
      theter = thetae * thetaw(1)

      calctht0 = min( thetax, theter )
!      calctht0 = theter

      return
      end 

!     eratio - Ratio of actual to potential bare soil evaporation
!     thetae - Equivalent surface soil water content (m^3/m^3)
!     theter - Surface soil water content based on relationship
!              between evaporation ratio & equivalent soil water
!              content (m^3/m^3)
! ***      if (theta(1) .gt. ( thetaw(1) + awct*.70 )) then
! ***         if ( ephc .eq. 0.0 ) then
! ***            theta(0)= max(thetax , theta(0))
! ***            go to 290
! ***         else
! ***            theta(0)= thetax
! ***            go to 290
! ***         end if
! ***      end if
! ***
! ***      if ( ephc .gt. 0.0 ) then
! ***         eratio = eahc/ephc									!text after h-85
! ***
! ***c     This function estimates soil wetness at the soil-atmosphere
! ***c     interface based on a sigmoid curve that describes the relationship
! ***c     between evaporation ratio and surface soil wetness expressed as
! ***c     equivalent water content.
! ***
! ***         thetae = 0.24308+1.37918/
! ***     *            (1.0+EXP(-(eratio-0.44882)/0.081))					!h-85
! ***         theter = thetae*thetaw(1)
! ***         theta(0) = min(theter,thetax,theta(0))
! ***      else
! ***         theta(0) = max(thetax/2.0,theta(0))
! ***      end if
! ***
