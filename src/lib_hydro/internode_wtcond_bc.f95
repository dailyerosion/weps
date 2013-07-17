!$Author: joelevin $
!$Date: 2011-03-24 11:33:26 -0500 (Thu, 24 Mar 2011) $
!$Revision: 11724 $
!$HeadURL: https://svn.weru.ksu.edu/weru/weps1/trunk/weps.src/src/lib_hydro/internode_wt_bc.for $

  function internode_wtcond_bc( theta_up, theta_low, &
           thetar_up, thetar_low, thetas_up, thetas_low, &
           thetaw_up, thetaw_low, theta80rh_up, &
           theta80rh_low, soiltemp_up, soiltemp_low, &
           ksat_up, ksat_low, lambda_up, lambda_low, &
           thick_up, thick_low, airentry_up, airentry_low ) result( k_int )

    ! + + + PURPOSE + + +
    ! Szymkiewicz, A. (2009), Approximation of internodal conductivities in numerical
    ! simulation of one-dimensional infiltration, drainage, and capillary rise in
    ! unsaturated soils, Water Resour. Res., 45, W10403, doi:10.1029/2008WR007654.

    ! Using the method with uniform soil properties by averageing the 
    ! layer properties of the two adjoining layers.
    ! This routine uses semi-layer thickness weighted values of lambda,
    ! air entry potential and saturated hydraulic conductivity to estimate
    ! the internodal soil properties.

    ! + + + KEYWORDS + + +
    ! darcy, layering, hydraulic conductivity

    ! + + + ARGUMENT DECLARATIONS + + +
    real, intent(in) :: theta_up
    real, intent(in) :: theta_low
    real, intent(in) :: thetar_up
    real, intent(in) :: thetar_low
    real, intent(in) :: thetas_up
    real, intent(in) :: thetas_low
    real, intent(in) :: thetaw_up
    real, intent(in) :: thetaw_low
    real, intent(in) :: theta80rh_up
    real, intent(in) :: theta80rh_low
    real, intent(in) :: soiltemp_up
    real, intent(in) :: soiltemp_low
    real, intent(in) :: ksat_up      ! saturated hydraulic conductivity of upper soil layer
    real, intent(in) :: ksat_low     ! saturated hydraulic conductivity of lower soil layer
    real, intent(in) :: lambda_up    ! pore size interaction factor of upper soil layer
    real, intent(in) :: lambda_low   ! pore size interaction factor of lower soil layer
    real, intent(in) :: thick_up     ! layer thickness of upper soil layer
    real, intent(in) :: thick_low    ! layer thickness of lower soil layer
    real, intent(in) :: airentry_up  ! air entry potential of upper soil layer
    real, intent(in) :: airentry_low ! air entry potential of lower soil layer
    real :: k_int                    ! hydraulic conductivity return value

    ! + + + LOCAL VARIABLES + + +
    real :: dist         ! distance between layer nodes
    real :: potm_up      ! matric potential of upper soil layer
    real :: potm_low     ! matric potential of lower soil layer
    real :: thetar       ! layer thickness weighted average
    real :: thetas       ! layer thickness weighted average
    real :: airentry     ! layer thickness weighted average airentry value
    real :: lambda       ! layer thickness weighted average lambda value
    real :: thetaw       ! layer thickness weighted average
    real :: theta80rh    ! layer thickness weighted average
    real :: soiltemp     ! layer thickness weighted average
    real :: ksat         ! layer thickness weighted average ksat value
    real :: soilrh       ! return value from subroutine, not used

    ! + + + FUNCTION DECLARATIONS + + +
    real int_cond_uni_bc

    ! + + + END SPECIFICATIONS + + +

    ! distance between layer nodes
    dist = 0.5 * ( thick_up + thick_low )

    ! using an internodal thickness weighted average
    thetar = ( thetar_up * thick_up + thetar_low * thick_low ) / ( 2 * dist )
    thetas = ( thetas_up * thick_up + thetas_low * thick_low ) / ( 2 * dist )
    lambda = ( lambda_up * thick_up + lambda_low * thick_low ) / ( 2 * dist )
    airentry = ( airentry_up * thick_up + airentry_low * thick_low ) / ( 2 * dist )
    thetaw = ( thetaw_up * thick_up + thetaw_low * thick_low ) / ( 2 * dist )
    theta80rh = ( theta80rh_up * thick_up + theta80rh_low * thick_low ) / ( 2 * dist )
    soiltemp = ( soiltemp_up * thick_up + soiltemp_low * thick_low ) / ( 2 * dist )
    ksat = ( ksat_up * thick_up + ksat_low * thick_low ) / ( 2 * dist )

    call matricpot_bc(theta_up, thetar, thetas, airentry, lambda, thetaw, &
                      theta80rh, soiltemp, potm_up, soilrh )
    call matricpot_bc(theta_low, thetar, thetas, airentry, lambda, thetaw, &
                      theta80rh, soiltemp, potm_low, soilrh )

    k_int = int_cond_uni_bc( potm_up, potm_low, dist, ksat, lambda, airentry )

    return
  end function internode_wtcond_bc