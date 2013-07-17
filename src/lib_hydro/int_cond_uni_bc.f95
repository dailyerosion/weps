!$Author: joelevin $
!$Date: 2011-03-24 11:33:26 -0500 (Thu, 24 Mar 2011) $
!$Revision: 11724 $
!$HeadURL: https://svn.weru.ksu.edu/weru/weps1/trunk/weps.src/src/lib_hydro/internode_wt_bc.for $

  function int_cond_uni_bc( potm_up, potm_low, dist, ksat, lambda, airentry ) result( k_int )
    ! This is the method used for internodal conductivity when soil properties are
    ! uniform between layers.  It is used within the method for layered soils.

    ! Szymkiewicz, A. (2009), Approximation of internodal conductivities in numerical
    ! simulation of one-dimensional infiltration, drainage, and capillary rise in
    ! unsaturated soils, Water Resour. Res., 45, W10403, doi:10.1029/2008WR007654.

    ! + + + KEYWORDS + + +
    ! darcy, layering, hydraulic conductivity

    ! + + + ARGUMENT DECLARATIONS + + +
    real, intent(in) :: potm_up  ! matric potential of upper soil layer (m)
    real, intent(in) :: potm_low ! matric potential of lower soil layer (m)
    real, intent(in) :: dist     ! distance between the two vertical nodes (m)
    real, intent(in) :: ksat     ! saturated hydraulic conductivity of soil layer (m/s)
    real, intent(in) :: lambda   ! pore size interaction factor of soil layer
    real, intent(in) :: airentry ! air entry potential of soil layer (m)
    real :: k_int                ! hydraulic conductivity return value

    ! + + + LOCAL VARIABLES + + +
    real :: d_potm        ! the difference in potential between uppper and lower "point"
    real :: cond_up       ! conductivity of the upper layer
    real :: cond_1        ! temporary conductivity value 1
    real :: cond_2        ! temporary conductivity value 2
    real :: potm_2        ! temporary potential value 2
    real :: temp_a        ! temporary value a
    real :: temp_b        ! temporary value b
    real :: temp_dzl      ! temporary value of ratio a/b

    ! + + + FUNCTION DECLARATIONS + + +
    real :: unsatcond_pot_bc
    real :: unsatcond_int_bc

    ! + + + END SPECIFICATIONS + + +

    d_potm = potm_low - potm_up

    cond_up = unsatcond_pot_bc(potm_up, ksat, airentry, lambda)

    if( (d_potm .eq. 0.0) .or. (d_potm .eq. dist) ) then
       ! uniform or hydrostatic matric potential distribution
       k_int = cond_up
    else if( d_potm/dist .lt. 0.0 ) then
       ! infiltration
       cond_1 = unsatcond_int_bc(potm_up, potm_low, ksat, airentry, lambda)
       cond_2 = cond_up / (1 - d_potm / dist)
       k_int = max( cond_1, cond_2 )
    else if( d_potm/dist .lt. 1.0 ) then
       ! drainage
       cond_1 = cond_up / (1 - d_potm / dist)
       potm_2 = potm_low - d_potm * d_potm / dist
       cond_2 = unsatcond_pot_bc(potm_2, ksat, airentry, lambda)
       k_int = min( cond_1, cond_2 )
    else
       ! capillary rise
       cond_1 = unsatcond_int_bc(potm_up, potm_low-dist, ksat, airentry, lambda)
       potm_2 = potm_low - dist
       cond_2 = unsatcond_pot_bc(potm_2, ksat, airentry, lambda)
       if( cond_1 .ne. cond_2 ) then
           temp_a = (d_potm * d_potm + 4.0*(cond_2 / cond_1 - 1.0)*(d_potm - dist)*dist)**0.5 - d_potm
           temp_b = 2.0 * (cond_2 / cond_1 - 1.0)
           temp_dzl = temp_a / temp_b
           k_int = dist * cond_1 * cond_2 / ((dist - temp_dzl)*cond_1 + temp_dzl*cond_2)
       else
           k_int = cond_1
       end if
    end if

    return
  end function int_cond_uni_bc