!$Author: joelevin $
!$Date: 2011-03-24 11:33:26 -0500 (Thu, 24 Mar 2011) $
!$Revision: 11724 $
!$HeadURL: https://svn.weru.ksu.edu/weru/weps1/trunk/weps.src/src/lib_hydro/internode_wt_bc.for $

  function internode_cond_bc( potm_up, potm_low, &
           ksat_up, ksat_low, lambda_up, lambda_low, &
           thick_up, thick_low, airentry_up, airentry_low ) result( k_int )

    ! + + + PURPOSE + + +
    ! Szymkiewicz, A. (2009), Approximation of internodal conductivities in numerical
    ! simulation of one-dimensional infiltration, drainage, and capillary rise in
    ! unsaturated soils, Water Resour. Res., 45, W10403, doi:10.1029/2008WR007654.

    ! Szymkiewicz, A., R. Helmig. 2010. Comparison of conductivity averaging methods
    ! for one-dimensional unsaturated flow in layered soils. SRC Simtech No. 2010-80

    ! Szymkiewicz, A., R. Helmig show how to deal with different properties between
    ! layers. THe method designated CC-SZYM is implemented here, which involves
    ! interatively matching the interface flux value with material transitions.
    ! All the layers in WEPS have different properties as they are tilled and resettle,
    ! and many soil profiles have texture changes with depth.

    ! + + + KEYWORDS + + +
    ! darcy, layering, hydraulic conductivity

    ! + + + ARGUMENT DECLARATIONS + + +
    real, intent(in) :: potm_up      ! matric potential of upper soil layer
    real, intent(in) :: potm_low     ! matric potential of lower soil layer
    real, intent(in) :: ksat_up      ! saturated hydraulic conductivity of upper soil layer
    real, intent(in) :: ksat_low     ! saturated hydraulic conductivity of lower soil layer
    real, intent(in) :: lambda_up    ! pore size interaction factor of upper soil layer
    real, intent(in) :: lambda_low   ! pore size interaction factor of lower soil layer
    real, intent(in) :: thick_up     ! layer thickness of upper soil layer
    real, intent(in) :: thick_low    ! layer thickness of lower soil layer
    real, intent(in) :: airentry_up  ! air entry potential of upper soil layer
    real, intent(in) :: airentry_low ! air entry potential of lower soil layer
    real :: k_int                    ! hydraulic conductivity return value

    ! + + + LOCAL DEFINITIONS + + +
    real :: potm_c      ! interfacial matric potential
    real :: flux_delta  ! difference in interfacial flux between uppper an lower layer
    real :: kunsat_up   ! hydraulic conductivity value between upper layer node and interlayer node
    real :: kunsat_low  ! hydraulic conductivity value between lower layer node and interlayer node
    real :: dz_up       ! distance from node to layer interface for upper layer
    real :: dz_low      ! distance from node to layer interface for lower layer
    integer :: iter_cnt ! counter, iterations required to converge to value of potm_c giving internode flux equality

    real, parameter :: test_delta = 0.00001       ! accuracy level desired for flux_delta (m/s)

    ! + + + FUNCTION DECLARATIONS + + +
    real int_cond_uni_bc

    ! + + + END SPECIFICATIONS + + +

    ! Equation 21 - equal flux at layer internodal interface, solve interatively
    ! initial guess is arithmetic mean
    potm_c = 0.5 * ( potm_up + potm_low )

    dz_up = 0.5*thick_up
    dz_low = 0.5*thick_low
    kunsat_up = int_cond_uni_bc( potm_up, potm_c, dz_up, ksat_up, lambda_up, airentry_up )
    kunsat_low = int_cond_uni_bc( potm_c, potm_low, dz_low, ksat_low, lambda_low, airentry_low )
    flux_delta = kunsat_up * ( (potm_c - potm_up) / (dz_up) - 1.0 ) &
               - kunsat_low * ( (potm_low - potm_c) / (dz_low) - 1.0 )

    iter_cnt = 0
    do while( (abs(flux_delta) .gt. test_delta) .and. (iter_cnt .le. 10) )
      potm_c = ( kunsat_up * ( potm_up / dz_up - 1.0 ) + kunsat_low * ( potm_low / dz_low - 1.0 ) ) &
             / ( kunsat_up / dz_up + kunsat_low / dz_low )
      kunsat_up = int_cond_uni_bc( potm_up, potm_c, dz_up, ksat_up, lambda_up, airentry_up )
      kunsat_low = int_cond_uni_bc( potm_c, potm_low, dz_low, ksat_low, lambda_low, airentry_low )
      flux_delta = kunsat_up * ( (potm_c - potm_up) / (dz_up) - 1.0 ) &
                 - kunsat_low * ( (potm_low - potm_c) / (dz_low) - 1.0 )
      iter_cnt = iter_cnt + 1
      if( iter_cnt .gt. 10 ) then
        !write(*,*) 'INTERNODE COND: potm_c convergence not obtained in 10 interations'
        write(*,*) 'INTERNODE COND: ', potm_up, potm_c, potm_low, kunsat_up, kunsat_low, flux_delta
      end if
    end do

    ! Equation 22 - 
    ! after convergence kunsat_up and kunsat_low are defined
    k_int = ( dz_up + dz_low ) / ( (dz_up/kunsat_up) + (dz_low/kunsat_low) )

    return
  end function internode_cond_bc
