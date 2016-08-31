!$Author$
!$Date$
!$Revision$
!$HeadURL$

      real function transpdepth ( bczrtd, bhzfurcut,                    &
     &                            bhztransprtmin, bhztransprtmax )
!     + + + PURPOSE + + +
!     This function estimates a depth of transpiration for crops that are
!     planted using a "deep furrow" type drill or planter, where dry soil
!     is pushed aside, and the seed is placed in a wetter part of the soil.
!     As the root zone expands, this effect is reduced, with the transpiration
!     depth equal to the root depth as calculated from a flat soil surface
!     when the root depth exceeds bhztransprtmax.

!     RETURNS: depth in soil from which transpiration is extracted (m)
!              when crop is furrow planted, this is deeper than root depth
!              and is used in place of root depth when calling transp subroutine

!     + + + KEYWORDS + + +
!     ridges, furrow, seeding, transpiration

!     + + + ARGUMENT DECLARATIONS + + +
      real bczrtd, bhzfurcut
      real bhztransprtmin, bhztransprtmax

!     + + + ARGUMENT DEFINITIONS + + +
!     bczrtd  - Crop root depth (m)
!     bhzfurcut - estimated furrow bottom depth below flat soil surface (m)
!     bhztransprtmin - root depth where transpiration depth reduction begins (m)
!     bhztransprtmax - root depth where transpiration depth equals root depth (m)

!     + + + END SPECIFICATIONS + + +

      if( bczrtd .eq. 0.0 ) then
          ! no plant growing, no adjustment
          transpdepth = 0.0
      else if( bczrtd .le. bhztransprtmin ) then
          transpdepth = bczrtd + bhzfurcut
      else if( (bczrtd .lt. bhztransprtmax)                             &
     &    .and. (bhztransprtmax .gt. bhztransprtmin) ) then
          transpdepth = bczrtd + bhzfurcut * ( (bhztransprtmax - bczrtd)&
     &            / (bhztransprtmax - bhztransprtmin) )
      else
          transpdepth = bczrtd
      end if

      return
      end
