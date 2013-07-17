!$Author: joelevin $
!$Date: 2011-03-24 11:33:26 -0500 (Thu, 24 Mar 2011) $
!$Revision: 11724 $
!$HeadURL: https://svn.weru.ksu.edu/weru/weps1/trunk/weps.src/src/lib_hydro/matricpot_bc.for $

  function matricfluxpot_bc( potm, airentry, ksat, lambda ) result( fluxpot )
    ! returns: matric flux potential using the Brooks and Corey relationship (m^2/s))
    ! as shown in Ross, P.J. 2003. Modeling Soil Water and Solute Transport - Fast,
    ! Simplified Numerical Solutions. Agron. J. 95:1352-1361

    ! *** Argument declarations ***
    real, intent(in) :: potm       ! matric potential (meters of water)
    real, intent(in) :: airentry   ! Brooks/Corey air entry potential (m)
    real, intent(in) :: ksat       ! saturated hydraulic conductivity (m/s)
    real, intent(in) :: lambda     ! Brooks/Corey pore size interaction parameter
    real :: fluxpot                ! matric flux potential using the Brooks and Corey relationship (m^2/s))

    ! *** Local Variables ***
    real :: nu      ! Brooks and Corey term (modified)
    real :: potme   ! flux potential coefficient of integration

    nu = 2.5 + 2.0/lambda

    potme = ksat * airentry / (1.0-lambda*nu)

    if( potm .lt. airentry ) then
      ! potential is less than air entry potential
      fluxpot = potme * (potm/airentry)**(1.0-lambda*nu)
    else
      ! potential is greater than or equal to air entry potential
      fluxpot = potme + ksat*(potm-airentry)
    end if
    return
  end function matricfluxpot_bc

