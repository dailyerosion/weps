!$Author$
!$Date$
!$Revision$
!$HeadURL$

      real function furrowcut ( bszrgh, bsxrgw, bsxrgs )
!     + + + PURPOSE + + +
!     This function estimates the depth of soil cut from a flat surface
!     to form a ridge and furrow. It is used to find a transpiration depth
!     where a newly planted seed is placed in a deeper, wetter soil layer.

!     + + + KEYWORDS + + +
!     ridges, furrow, seeding, transpiration

!     + + + ARGUMENT DECLARATIONS + + +
      real bszrgh, bsxrgw, bsxrgs

!     + + + ARGUMENT DEFINITIONS + + +
!     bszrgh - Ridge height (mm)
!     bsxrgw - Ridge width (mm)
!     bsxrgs - Ridge spacing (mm)

!     + + + LOCAL VARIABLES + + +
      real furrowdepth

!     + + + LOCAL DEFINITIONS + + +
!     furrowdepth - the furrow depth that the combination of spacing and
!     top width will give if the furrow side slope is limited to 1:1

!     + + + END SPECIFICATIONS + + +

      if ( bszrgh .ge. (bsxrgs - bsxrgw) ) then
          ! ridge height is greater than furrow width
          ! ie. side slope is steeper than 1:1 then limit to 1:1
         furrowdepth = bsxrgs - bsxrgw
      else
         furrowdepth = bszrgh
      endif

      furrowcut = 0.5 * furrowdepth * (1.0 + bsxrgw/bsxrgs)

      return
      end
