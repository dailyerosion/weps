!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!
      subroutine crust (crustf_rm,tillf,crustf,lmosf, lmosm)


!     + + + PURPOSE + + +
!
!     This subroutine destroys the surface crust after a tillage event.
!
!     + + + KEYWORDS + + +
!     crust, tillage (primary/secondary)

      include 'p1werm.inc'
!
!     + + + ARGUMENT DECLARATIONS + + +
!
      real tillf, crustf, crustf_rm, lmosf, lmosm
!
!     + + + ARGUMENT DEFINITIONS + + +
!
!     lmosf - fraction of crusted surface containing loose erodible material
!     lmosm - mass of loose erodible material on crusted portion of surface
!     crustf - Current fraction of surface crusted (before & after operation)
!     crustf_rm - Fraction of crust removed (0 <= crustf_rm <= 1)
!     tillf - Fraction of the surface tilled (0 <= tillf <= 1)
!
!     + + + ACCESSED COMMON BLOCK VARIABLE DEFINITIONS + + +
!
!
!     + + + PARAMETERS + + +
!
!     + + + LOCAL VARIABLES + + +
!
!     + + + LOCAL VARIABLE DEFINITIONS + + +
!
!     + + + END SPECIFICATIONS + + +
!
!
!     crf = cri * ( (1.0 - tillf) + (tillf * (1.0-crustf_rm)))

      ! determine fraction of surface that remains crusted
      crustf = crustf * (1.0 - tillf * crustf_rm)

! Currently the crust function doesn't modify the loose erodible
! material variables on the crusted surface.  That could be changed
! in the future if it was deemed necessary.

! The following should be removed.  Need to check SOIL and EROSION
! first to make sure they aren't adversely affected. - LEW
! 8/25/1999

!     check to see if the loose material on the surface is still there
!     if enough of the crust is removed set lmosf to zero (loose material)
!     This was done according to L. Hagen

      ! just clear them out if it close to zero 
      ! (LH shouldn't have erosion or soil submodels this sensitive)
      if (crustf .lt. 0.01) then
         lmosf = 0.0
         lmosm = 0.0
      endif

      return
      end
