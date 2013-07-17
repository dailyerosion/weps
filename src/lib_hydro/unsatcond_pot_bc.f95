!$Author: joelevin $
!$Date: 2011-03-24 11:33:26 -0500 (Thu, 24 Mar 2011) $
!$Revision: 11724 $
!$HeadURL: https://svn.weru.ksu.edu/weru/weps1/trunk/weps.src/src/lib_hydro/unsatcond_bc.for $

  function unsatcond_pot_bc(potm, ksat, airentry, lambda) result( kunsat )
    ! returns the unsaturated hydraulic conductivity in same units as ksat as 
    ! defined by the Books and Corey function and the Mualem conductivity model
    ! as a function of the matric potential

    ! *** Argument declarations ***
    real, intent(in) :: potm       ! matric potential  (meters of water)
    real, intent(in) :: ksat       ! Saturated hydraulic conductivity (m/s)
    real, intent(in) :: airentry   ! Brooks and Corey air entry matric potential (m)
    real, intent(in) :: lambda     ! Brooks and Corey pore size interaction parameter 
    real :: kunsat                 ! unsaturated hydraulic conductivity (m/s)

    ! *** Local Variables ***
    real nu      ! Brooks and Corey term (modified)

    nu = 2.5 + 2.0/lambda

    if( potm .lt. airentry ) then
      ! unsaturated condition
      kunsat = ksat*(potm/airentry)**(-lambda*nu)
    else
      ! saturated condition
      kunsat = ksat
    end if

    return
  end function unsatcond_pot_bc

