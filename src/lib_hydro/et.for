!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine  et(rn, g_soil, vel_wind, bmzele, bwtdmx, bwtdmn,      &
     &            bwtdav, bwtdpt, bwrrh, bhzetp, loc_za, loc_zo, loc_zd)

!     + + + PURPOSE + + +
!     This subroutine calculates daily potential evapotranspiration
!     using Van Bavel's (1966) revised combination method.
!     DATE:  09/17/93
!     MODIFIED:  10/06/93

!     + + + KEY WORDS + + +
!     evapotranspiration

!     + + + ARGUMENT DECLARATIONS + + +
      real rn
      real g_soil
      real vel_wind
      real bmzele
      real bwtdmn
      real bwtdmx
      real bwtdav
      real bwtdpt
      real bwrrh
      real bhzetp
      real loc_za, loc_zo, loc_zd

!     + + + ARGUMENT DEFINITIONS + + +
!     rn     - Net radiation (Mj/m^2/day)
!     g_soil - ground heat flux (Mj/m^2/day)
!     vel_wind - Wind speed (m/s) at meteorological height (loc_za)
!     bmzele   - Elevation of the site (m)
!     bwtdmx - daily maximum temperature (C)
!     bwtdmn - daily minimum temperature (C)
!     bwtdpt - daily average dew point temperature (C)
!     bwrrh  - relative humidity ratio
!     bhzetp - potential evaporation depth (mm)
!        the following must be in consistent units (length)
!     loc_za - height of meteorological measurement 
!     loc_zo - aerodynamic roughness length
!     loc_zd - zero plane displacement

!     + + + PARAMETERS + + +

      real b1, b2, b3
      parameter   (b1 = 67.5242, b2 = 149.531, b3 = -4859.0665)
!     b1,b2,b3 - constants used in eqn for (svpg0)

      real d1, d2
      parameter   (d1 = 2.5002773719, d2 = 0.0023644939)
!     d1,d2 - constants used to compute latent heat of vaporization

      real e
      parameter   (e = 0.622)
!     e - water to air molecular weights ratio

      real vk
      parameter   (vk = 0.41)
!     vk - Von Karman's constant

      real dt_cli
      parameter (dt_cli = 2.0)
      ! dt_cli  - minimum dew point depression for no adjustment

      real k_arid
      parameter (k_arid = 0.5)
      ! k_arid  - emperical aridity proportioning coefficient

!     + + + LOCAL VARIABLES + + +
      real term1, term2, term3
      real bp
      real svpg
      real svpg0
      real vlh
      real vpa
      real vpd
      real vps
      real vpsmn
      real vpsmx
      real arho
      real ttc
      real zo_v

      real deltat, tmaxadj, tminadj, tdavadj, tdewadj

      real etpr
      real etpw

!     + + + LOCAL DEFINITIONS + + +
!     term1  - Temporary local variables
!     bp     - Barometric pressure (kpa)
!     svpg   - Ratio of saturation vapor pressure curve slope
!              to the pychrometric constant, adjusted to
!              ambient barometric pressure (unitless)
!     svpg0  - Unadjusted ratio of the saturation vapor pressure
!              curve slope to the pychrometric constant (unitless)
!     vlh    - Latent heat of vaporization (Mj/kg)
!     vpa    - Actual vapor pressure (kpa)
!     vpd    - Saturation vapor pressure deficit (kpa)
!     vps    - Saturated vapor pressure (kpa)
!     vpsmn  - Saturated vapor pressure at min air temp (kpa)
!     vpsmx  - Saturated vapor pressure at max air temp (kpa)
!     arho   - Air density (kg/m^3)
!     ttc    - Turbulent transfer coefficient (kg/m^2/kpa/day)
!     zo_v   - estimate of aerodynamic roughness length for vapor transfer
!     deltat - dew point depression below minimum temperature with climate adjustment
!     tmaxadj - maximum temperature adjusted to field site for potential ET conditions
!     tminadj - minimum temperature adjusted to field site for potential ET conditions
!     tdavadj - daily average temperature adjusted to field site for potential ET conditions
!     tdewadj - dew point temperature adjusted to field site for potential ET conditions
!     etpr   - Potential evapotransp due to radiation (mm/day)
!     etpw   - Potential evapotranspiration due to wind (mm/day)

!     + + + FUNCTIONS CALLED + + +
      real preslaps
      real satvappres

!     + + + END SPECIFICATIONS + + +

      ! check dew point and minimum temperature for variance from a freely
      ! transpiring surface. In calculating potential ET, Tmax and Tmin will 
      ! tend to be lower than over a non-transpiring surface as indicated by
      ! Tmin being significantly greater than Tdew. This adjustment process
      ! is described in Allen, R.G. 1996. Assessing Integrity of Weather
      ! Data for Reference Evapotranspiration Estimation. Journal of Irrigation 
      ! and Drainage Engineering, vol 122(2)

      deltat = bwtdmn - bwtdpt - dt_cli
      if( deltat .gt. 0.0 ) then
          tmaxadj = bwtdmx - k_arid * deltat
          tminadj = bwtdmn - k_arid * deltat
          tdavadj = 0.5 * (tmaxadj + tminadj)
          tdewadj = bwtdpt + (1.0 - k_arid) * deltat
      else
          tmaxadj = bwtdmx
          tminadj = bwtdmn
          tdavadj = bwtdav
          tdewadj = bwtdpt
      end if

      bp = preslaps( bmzele )
      svpg0 = b1 * exp(((tdavadj-b2)**2.) / b3)           !h-14
      svpg = svpg0*(101.325/bp)                          !h-13
      vlh = d1 - (d2*tdavadj)                             !h-24
      term1 = svpg * ((rn-g_soil)/vlh)                   !h-12(a)(modified)

      vpa = satvappres( tdewadj )
      vpsmn = satvappres( tminadj )
      vpsmx = satvappres( tmaxadj )
      vps = 0.5 * (vpsmn + vpsmx)                        !h-26
      vpd = vps - vpa                                    !h-25
      arho = 1000.*((bp/101.325)*((0.001293)/(1+(0.00367*tdavadj))))  !h-32

      ! Jensen, M.E., R.D. Burman, R.G.Allen. 1989. Evapotranspiration and
      ! Irrigation Water Requirements. ASCE Manuals and Reports on Engineering
      ! Practice No. 70, Page 91 states that the Bussinger-Van Bavel method does
      ! not work using aerodynamic roughness for momentum. Their estimate is that
      ! using 1/10th of zo gives more reasonable answers. This is consistent
      ! with discussion on page 94 that the aerodynamic roughness length for
      ! vapor transfer is at least 2/10th and maybe even less than 1/10th
      ! that of the aerodynamic roughness for momentum.
      zo_v = 0.1 * loc_zo

      ! van Bavel gives za as the height of the measurement instruments above
      ! the surface. In his example, he clarifies this as a height above the crop 
      ! surface, not the ground surface. This is probably best approximated here
      ! as height above the ground minus zero plane displacement.
      ! (meteorological height adjusted to account for crop (residue) height in
      ! hydro before call)
      ttc = (arho * e * (vk**2.) * vel_wind * 86400.0)                  &
     &    / (bp * (log( (loc_za - loc_zd) / zo_v) )**2.) !h-30

      term2 = vpd*ttc                                    !h-12(b)
      term3 = svpg + 1.0                                 !h-12(c)
      bhzetp = (term1+term2)/term3                       !h-12
      if ( bhzetp  .le. 0.0 )  bhzetp = 0.0
      bwrrh = vpa / vps

! DEBUGGING statements
!      etpr = term1 / term3
!      etpw = term2 / term3
!      write(*,*) 'et:',rn,tmaxadj,tminadj,tdavadj,tdewadj,bwrrh,        &
!     &           vel_wind, loc_zd,zo_v,etpr,etpw
!
! END DEBUGGING
      return
      end
