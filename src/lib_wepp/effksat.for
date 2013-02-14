!$Author$
!$Date$
!$Revision$
!$HeadURL$

      real function effksat(uselan, clay, sand, cec, orgmat, rooty,     &
     &              rilcov, bascov, rescov, rrough, fbasr, fbasi, fresi)

! clay(i,iplane)
! sand(i,iplane)
! orgmat(1,iplane)
! cec(1,iplane)
! rooty(1,iplane)
! rilcov(iplane)
! fbasr(iplane)
! bascov(iplane)
! fresi(iplane)
! rescov(iplane))
! rrough(iplane)
! fbasi(iplane)

!     +++PURPOSE+++

!     The purpose of this program is to estimate the effective saturated
!     hydraulic conductivity in the surface layers. This is based on
!     routines copied from WEPP SCON.FOR

!     +++PARAMETERS+++
 
!     +++ARGUMENT DECLARATIONS+++
      integer, intent(in) :: uselan
      real, intent(in) ::  clay, sand, cec, orgmat, rooty
      real, intent(in) ::  rilcov, bascov, rescov, rrough
      real, intent(in) ::  fbasr, fbasi, fresi

!     +++ARGUMENT DEFINITIONS+++
    
!     clay - clay content of soil (fraction)
!     sand - sand content of soil (fraction)
!     orgmat - orgmat content of soil (fraction)
!     cec - cation exchange capacity of soil milliequivalent of hydrogen per 100 g (meq+/100g)
!                                    or numerically equal, the SI unit centi-mol per kg (cmol+/kg)
!     rooty - total root mass in a soil layer on day of simulation kg/m^2
!     rilcov - rill cover (0-1, unitless)
!     bascov - fraction of ground surface covered with basal vegetation (0-1)
!     rescov - residue cover (0-1)
!     rrough - 
!     fbasr - fraction of total basal cover located in rills (0-1)
!     fbasi - fraction of total basal cover located in interrills (0-1) 
!     fresi - fraction of total litter cover located in interrills (0-1)

!     +++COMMON BLOCKS+++

!     +++LOCAL VARIABLES+++

!     +++LOCAL DEFINITIONS+++

!     +++DATA INITIALIZATIONS+++

!     +++END SPECIFICATIONS+++

!           ELSE if in the top 2 soil layers and the input value of
!           conductivity has been set to zero, estimate a value.
!           (2/7/94 dcf from nearing)


      if(uselan.ne.2)then

          ! CONDUCTIVITY ESTIMATION FOR ALL LAND USES EXCEPT RANGE
          if (clay .le. 0.4) then
              if (cec .gt. 1.0) then
                  effksat = -0.265 + 0.0086 * (sand*100.0)**1.80        &
     &                    + 11.46 * (cec**(-0.75))
              else
                  effksat = 11.195 + 0.0086 * (sand*100.0)**1.80
              end if
          else
              effksat = 0.0066 * exp(244.0 / (clay*100.0))
          end if
      else

          ! RANGELAND CONDUCTIVITY ESTIMATION
          ! NEW KIDWELL EQUATION AS OF June 7, 1995   dcf
          if(rilcov .lt. 0.45)then
                effksat = 57.99                                         &
     &             - (14.05 * alog(cec))                                &
     &             + (6.20 * alog(rooty))                               &
     &             - (473.39 * (fbasr*bascov)**2)                       &
     &             + (4.78 * fresi*rescov)
          else
                effksat = -14.29                                        &
     &             - (3.40 * alog(rooty))                               &
     &             + (37.83 * sand)                                     &
     &             + (208.86 * orgmat)                                  &
     &             + (398.64 * rrough)                                  &
     &             - (27.39 * fresi*rescov)                             &
     &             + (64.14 * fbasi*bascov)
          endif

      endif

      ! Limit EFFECTIVE baseline conductivity value to 0.2 mm/hr minimum.
      if (effksat .lt. 0.2) effksat = 0.2

      ! Convert from mm/hr to meters/second
      effksat = effksat / 3.6e6


      return
      end
