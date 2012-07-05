!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!
! This routine adjusts the burial coefficients for operation speed
! and tillage depth

      subroutine buryadj( burycoef,mnrbc,                               &
     &                    speed,stdspeed,minspeed,maxspeed,             &
     &                    depth,stddepth,mindepth,maxdepth)

!     argument declarations
      real    burycoef(mnrbc)
      integer mnrbc
      real    speed,stdspeed,minspeed,maxspeed
      real    depth,stddepth,mindepth,maxdepth

!     argument definitions
!     burycoef - burial fraction coefficient to be adjusted
!     mnrbc    - number of burial coefficients (residue burial classes)
!     speed    - actual
!     stdspeed - standard, where coefficient remains unchanged
!     minspeed - minimum
!     maxspeed - maximum
!     depth    - actual
!     stddepth - standard, where coefficient remains unchanged
!     mindepth - minimum
!     maxdepth - maximum

!     local variable declarations
      integer index
      real    rspeed, rdepth
      real    expspeed, s1speed, s2speed, expdepth

      parameter (expspeed = 0.5)
      parameter (s1speed = 0.6)
      parameter (s2speed = 0.4)
      parameter (expdepth = 2.7)

!     find speed adjustment parameter
      speed = max( min(speed, maxspeed), minspeed )
      rspeed = (s1speed+s2speed*(speed/maxspeed)**expspeed)/            &
     &         (s1speed+s2speed*(stdspeed/maxspeed)**expspeed)

!     find depth adjustment parameter
      depth = max(min(depth, maxdepth), mindepth )
      rdepth = (1.0-(1.0-depth/maxdepth)**expdepth)/                    &
     &         (1.0-(1.0-stddepth/maxdepth)**expdepth)

!     adjust burial coefficients and keep within range 0 to 1
      do 100 index=1,mnrbc
          burycoef(index) = burycoef(index)*rspeed*rdepth
          burycoef(index) = min( 1.0, max( 0.0, burycoef(index)))
 100  continue
      return
      end
