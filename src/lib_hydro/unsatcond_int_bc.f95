!$Author: joelevin $
!$Date: 2011-03-24 11:33:26 -0500 (Thu, 24 Mar 2011) $
!$Revision: 11724 $
!$HeadURL: https://svn.weru.ksu.edu/weru/weps1/trunk/weps.src/src/lib_hydro/unsatcond_bc.for $

  function unsatcond_int_bc( potm_1, potm_2, ksat, airentry, lambda ) result( k_int )
    ! returns the integrated unsaturated hydraulic conductivity as defined by
    ! Szymkiewicz, A. (2009), Approximation of internodal conductivities in numerical
    ! simulation of one-dimensional infiltration, drainage, and capillary rise in
    ! unsaturated soils, Water Resour. Res., 45, W10403, doi:10.1029/2008WR007654.

    ! *** Argument declarations ***
    real, intent(in) :: potm_1     ! matric potential one (meters of water)
    real, intent(in) :: potm_2     ! matric potential two (meters of water)
    real, intent(in) :: ksat       ! Saturated hydraulic conductivity (m/s)
    real, intent(in) :: airentry   ! Brooks and Corey air entry matric potential (m)
    real, intent(in) :: lambda     ! Brooks and Corey pore size interaction parameter 
    real :: k_int                  ! unsaturated hydraulic conductivity (m/s)

    ! *** functions ***
    real matricfluxpot_bc
    real unsatcond_pot_bc
      
    ! *** Local Variables ***
    real fluxpot_1, fluxpot_2

    fluxpot_1 = matricfluxpot_bc( potm_1, airentry, ksat, lambda )
    fluxpot_2 = matricfluxpot_bc( potm_2, airentry, ksat, lambda )

    if( (fluxpot_2 .eq. fluxpot_1) .or. (potm_2 .eq. potm_1) ) then
      ! equal potentials so use one
      k_int = unsatcond_pot_bc(potm_1, ksat, airentry, lambda)
    else
      ! denominator not equal to zero
      k_int = (fluxpot_2 - fluxpot_1) / (potm_2 - potm_1)
    end if

    return
  end function unsatcond_int_bc

