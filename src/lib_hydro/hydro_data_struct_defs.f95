!$Author$
!$Date$
!$Revision$
!$HeadURL$

module hydro_data_struct_defs
    use Polygons_Mod, only: polygon
    implicit none

    integer, dimension(:), allocatable :: am0hfl    ! flag to print HYDROlogy output
                                                    ! 0 = no output
                                                    ! 1 = daily
                                                    ! 2 = hourly
                                                    ! 3 = daily and hourly
                                                    ! 4 = soil temperature
                                                    ! 5 = daily and soil temperature
                                                    ! 6 = hourly and soil temperature
                                                    ! 7 = daily, hourly, and soil temperature
    integer, dimension(:), allocatable :: am0hdb    ! flag to print HYDROlogy variables before and after the call to HYDRO
                                                    ! 0 = no output
                                                    ! 1 = output


    type hydro_derived_et
       real :: zea  ! Actual bare soil evaporation (mm/day)
       real :: zep  ! Potential bare soil evaporation (mm/day)
       real :: zeta ! Actual evapotranspiration (mm/day)
       real :: zetp ! potential evapotranspiration (mm/day)
       real :: zpta ! Actual plant transpiration (mm/day)
       real :: zptp ! potential plant transpiration (mm/day)
       real :: drat ! dryness ratio
       real :: zsnd ! snow depth (mm)
       real :: snow_protect ! snow cover greater than snow_depth_thresh
    end type hydro_derived_et

  contains

    subroutine sim_area_ave_h1et( subr_poly, h1et )
       type(polygon), dimension(:), intent(in) :: subr_poly
       type(hydro_derived_et), dimension(0:), intent(inout) :: h1et

       integer :: isr, nsubr
       real :: tot_area
       REAL, PARAMETER :: snow_depth_thresh = 20.0

       nsubr = size(subr_poly)

       ! sum up all subregion areas
       tot_area = 0.0
       do isr = 1, nsubr
          tot_area = tot_area + subr_poly(isr)%area
       end do

       h1et(0)%zea  = 0.0
       h1et(0)%zep  = 0.0
       h1et(0)%zeta = 0.0
       h1et(0)%zetp = 0.0
       h1et(0)%zpta = 0.0
       h1et(0)%zptp = 0.0
       h1et(0)%drat = 0.0
       h1et(0)%zsnd = 0.0
       h1et(0)%snow_protect = 0.0

       do isr = 1, nsubr
          h1et(0)%zea  = h1et(0)%zea  + h1et(isr)%zea  * subr_poly(isr)%area / tot_area
          h1et(0)%zep  = h1et(0)%zep  + h1et(isr)%zep  * subr_poly(isr)%area / tot_area
          h1et(0)%zeta = h1et(0)%zeta + h1et(isr)%zeta * subr_poly(isr)%area / tot_area
          h1et(0)%zetp = h1et(0)%zetp + h1et(isr)%zetp * subr_poly(isr)%area / tot_area
          h1et(0)%zpta = h1et(0)%zpta + h1et(isr)%zpta * subr_poly(isr)%area / tot_area
          h1et(0)%zptp = h1et(0)%zptp + h1et(isr)%zptp * subr_poly(isr)%area / tot_area
          h1et(0)%drat = h1et(0)%drat + h1et(isr)%drat * subr_poly(isr)%area / tot_area
          h1et(0)%zsnd = h1et(0)%zsnd + h1et(isr)%zsnd * subr_poly(isr)%area / tot_area
          ! Note that the 20mm depth should be a global parameter
          ! It is currently stuck in erosion.for as a local parameter there
          ! this makes the 0 element of the snow cover array the fraction of the total area 
          ! which is protected from erosion by snow cover (the intent of the reporting code?)
          IF (h1et(isr)%zsnd > snow_depth_thresh) THEN
             h1et(isr)%snow_protect = 1.0
          else
             h1et(isr)%snow_protect = 0.0
          end if
          h1et(0)%snow_protect = h1et(0)%snow_protect + h1et(isr)%snow_protect * subr_poly(isr)%area / tot_area

       end do

    end subroutine sim_area_ave_h1et

end module hydro_data_struct_defs

