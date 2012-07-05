!$Author$
!$Date$
!$Revision$
!$HeadURL$

      real function internode_wt_bc(cond_up, cond_low,                  &
     &              ksat_up, ksat_low, lambda_up, lambda_low,           &
     &              thick_up, thick_low, airentry_up, airentry_low )

!     + + + PURPOSE + + +
!     Gastó, J.M., J. Grifoll, and Y. Cohen. 2002. Estimation of Internodal
!     Permeabilities for Numerical Simulation of Unsaturated Flows. Water
!     Resources Research, vol. 38, no. 12. Variable names correspond to
!     the convention used in the article.

!     Examples in the article suggest that uniform layer properties be
!     used for all internodal calculations, and that a node be placed at
!     the intersection of the two layers. The weakness of this requirement
!     shows up when considering soil with gradually varying properties.
!     This routine uses semi-layer thickness weighted values of lambda
!     and air entry potential to estimate the internodal soil properties.

!     + + + KEYWORDS + + +
!     darcy, layering, hydraulic conductivity

!     + + + ARGUMENT DECLARATIONS + + +
      real cond_up, cond_low
      real ksat_up, ksat_low, lambda_up, lambda_low
      real thick_up, thick_low, airentry_up, airentry_low

!     + + + ARGUMENT DEFINITIONS + + +
!     cond_up - unsaturated hydraulic conductivity of upper soil layer
!     cond_low - unsaturated hydraulic conductivity of lower soil layer
!     ksat_up - saturated hydraulic conductivity of upper soil layer
!     ksat_low - saturated hydraulic conductivity of lower soil layer
!     lambda_up - pore size interaction factor of upper soil layer
!     lambda_low - pore size interaction factor of lower soil layer
!     thick_up - layer thickness of upper soil layer
!     thick_low - layer thickness of lower soil layer
!     airentry_up - air entry potential of upper soil layer
!     airentry_low - air entry potential of lower soil layer

!     + + + PARAMETERS + + +
      real   a10, a11, a2, b01, b02, b1, c0, beta
      parameter( a10 = 0.208 )
      parameter( a11 = 0.634 )
      parameter( a2 = 0.191 )
      parameter( b01 = 0.690 )
      parameter( b02 = 2.294 )
      parameter( b1 = 0.049 )
      parameter( c0 = 0.020 )
      parameter( beta = 0.0080 )

!     + + + COMMON BLOCKS + + +

!     + + + LOCAL COMMON BLOCKS + + +

!     + + + LOCAL VARIABLES + + +
      real n, beta0, dist, lambda, b0
      real airentry, delta_z_star, c, b, a1, a
      real k_up, k_low, r

!     + + + LOCAL DEFINITIONS + + +
!     n - 
!     beta0 - 
!     dist - internodal distance (m)
!     lambda - pore size interaction factor (internodal average)
!     airentry - air entry potential (internodal average)
!     delta_z_star - nondimensional distance between nodes

!     + + + SUBROUTINES CALLED + + +

!     + + + FUNCTION DECLARATIONS + + +

!     + + + DATA INITIALIZATIONS + + +

!     + + + OUTPUT FORMATS + + +

!     + + + END SPECIFICATIONS + + +

      ! distance between layer nodes
      dist = 0.5 * ( thick_up + thick_low )

      ! using an internodal weighted average lambda
      lambda = ( lambda_up * thick_up + lambda_low * thick_low )        &
     &       / ( 2 * dist )
      n = lambda + 1.0

      ! equation 14
      beta0 = beta * n

      ! equation 13
      b0 = b01 * n / ( b02 * n - 1.0 )

      ! Nondimensional distance, using average of layer thickness for
      ! internadal distance and an internodal weighted average air entry
      ! potential
      airentry = ( airentry_up * thick_up + airentry_low * thick_low )  &
     &       / ( 2 * dist )
      delta_z_star = abs(dist / airentry)

      ! equation 12
      c = b0 + c0 * (n - 1.0) * delta_z_star

      ! equation 11
      b = b0 - b1 * delta_z_star
      ! keep b from going negative for large delta_z_star
      b = max( 1.0e-6, b )

      ! equation 10
      a1 = a10 + a11 * log10(n)

      ! equation 9
      a = (1.0 - a1 * delta_z_star) / (1.0 + a2 * n * n * delta_z_star)
      ! keep a from going negative for large delta_z_star
      a = max( 1.0e-6, a )

      ! equation 8b
      k_up = cond_up / ksat_up
      k_low = cond_low / ksat_low

      ! check for drying below the residual moisture content in denominator
      if( k_low .le. 0.0 ) then
          r = 1.0e38
      else
          r = k_up**b / k_low**c
      end if

      ! equation 8a
      internode_wt_bc = 1.0 /( 1.0 + ( a*r/(1.0 + beta0*r)))

      return
      end