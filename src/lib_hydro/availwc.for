!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!
! ***************************************************************

      real function availwc (theta, thetaw, thetaf)

!     + + + PURPOSE + + +

!     availwc - Available water content ratio (mm/mm)
!     returns a linear function from thetaw=0 to thetaf=1

!     + + + ARGUMENT DECLARATIONS + + +

      real theta, thetaw, thetaf

!     + + + ARGUMENT DEFINITIONS + + +

!     theta  - actual water content (mm/mm)
!     thetaw - water content at wilting point (mm/mm)
!     thetaf - water content at field capacity (mm/mm)

!     + + + END SPECIFICATIONS + + +

      if (theta .le. thetaw) then
         ! can't be negative value
         availwc = 0.0
      else if (theta .ge. thetaf) then
         ! can't be greater than 1
         availwc = 1.0
      else
         availwc = (theta - thetaw) / (thetaf - thetaw)
      endif

      end

