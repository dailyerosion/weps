!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!
! ***************************************************************

      real function acplwu (awcr, awcr_crit, wup)

!     + + + PURPOSE + + +

!     acplwu - Actual water use rate from soil layer 
!              same units as wup

!     + + + ARGUMENT DECLARATIONS + + +

      real awcr
      real awcr_crit
      real wup

!     + + + ARGUMENT DEFINITIONS + + +

!     awcr   - Relative soil water availability, fraction (0-1.0)
!     awcr_crit	- soil water availability ratio below which
!                 plant transpiration is reduced 
!     wup    - Potential water use rate from soil layer (mm/day)

      real str_fac
      parameter( str_fac = 100 )

!     + + + END SPECIFICATIONS + + +

!      if (awcr .ge. awcr_crit) then
!         acplwu = wup
!      else if (awcr .gt. 0.0) then
!         acplwu = wup * awcr/awcr_crit
!      else
!         acplwu = 0.0
!      endif

      if (awcr .ge. 1.0) then
         acplwu = wup
      else if (awcr .gt. 0.0) then
         acplwu = wup * log10( str_fac + 1.0 - str_fac*(1.0-awcr) )     &
     &          / log10( str_fac + 1.0 )
      else
         acplwu = 0.0
      endif

      end 
