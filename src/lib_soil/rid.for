!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!
      subroutine rid(cf2cov, bbfscv, bbffcv, bszrgh,                    &
     &  bsxrgs, bszrho, cumpa, dcump, bsvroc)

!     + + + ARGUMENT DECLARATIONS + + +
      real cf2cov, bbfscv, bbffcv, bszrgh, bsxrgs, bszrho
      real cumpa, dcump, bsvroc(*)

!     + + + LOCAL VARIABLES + + +
      real cf1rg

!     + + + LOCAL DEFINITIONS + + +
!   cf1rg     - correction factor for ridge scale

!  RIDGE SECTION:
!     calculate biomass cover sheltering factor (eq. S-9 & S-10 combined)
      cf2cov = 1.0 - 0.6 * (bbfscv + (1.0 - bbfscv)*bbffcv)

!     if ridge height is zero, skip ridge update
      if (bszrgh .ne. 0.0) then
         ! calc. ridge scale factor (eq. S-8)
         cf1rg = (348.0 / bsxrgs)**0.3
         ! calculate apparent cum. precip. (eq. S-5)
         cumpa = ((1. - bszrgh/bszrho)/(0.034*cf1rg))**2.
         ! update ridge height (eq. S-6)
         bszrgh = bszrho * (1.0 - 0.034 * sqrt(cumpa + dcump * cf2cov   &
     &          * (1.0 - bsvroc(1)))*cf1rg)

         ! check to see that minimum bszrgh/bszrho > 0.05 if not then set
         ! the ratio to 0.05.
         if ((bszrgh/bszrho) .lt. 0.05) bszrgh = 0.05 * bszrho
      endif
      end
