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

    integer, parameter :: hhrs = 24  ! number of surface water content values stored for one day (equally spaced)

    real, parameter :: claygrav80rh = 0.3 ! gravimetric water content of soil clay at 80% rh
                                          ! Montmorillonite from ten Berge, 1990
    real, parameter :: orggrav80rh = 0.27 ! gravimetric water content of soil organics at 80% rh
                                          ! peat and muck from Rutherford and Chlou, 1992

    real, parameter :: rgas = 8.3143        ! universal gas constant (joules/(mole degree K)
    real, parameter :: molewater = 0.018    ! molecular weight of water (kg/mole)
    real, parameter :: zerokelvin = 273.16  ! kelvin equivalent of zero degree centigrade
    real, parameter :: denwat = 1000.0      ! density of the liquid (water) (kg/m^3)
    real, parameter :: gravconst = 9.807    ! acceleration due to gravity (m/s^2)
    real, parameter :: potwilt = -152.95    ! matric potential for wilting point (15 bar) in meters of water at max density
    real, parameter :: potfc = -3.3989      ! matric potential for field capacity (1/3 bar) in meters of water at max density
    real, parameter :: potfcs = -1.01967    ! matric potential for field capacity (1/10 bar) in meters of water at max density
    real, parameter :: diffuntp = 2.12e-5   ! Binary diffusion coefficient for water vapor in air at
                                            ! normal temperature and pressure (0 C, 1 standard atmosphere) (m^2/s)
    real, parameter :: atmstand = 101.3     ! standard atmosphere (kilopascals)
    real, parameter :: templapse = 0.0065   ! temperature lapse rate for troposphere (degree K/meter)
    real, parameter :: tempstand = 288.0    ! standard temperature used to find standard atmosphere (degree K)
    real, parameter :: rair = 287.0         ! gas constant for air (J/kg/K or m^2/s^2/K)

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
       real :: zirr ! Single day irrigation water applied (mm)
       real :: zper ! daily deep percolation (mm/day)
       real :: zrun ! daily surface runoff (mm/day)
    end type hydro_derived_et

    type hydro_state
       real, dimension(hhrs) :: rwc0 ! Surface soil water content (kg/kg)
       real :: zsno     ! Water content of snow (mm)
       real :: tsno     ! temperature of snow layer (C)
       real :: fsnfrz   ! fraction of snow layer water content which is frozen
       real :: zdmaxirr ! characteristic maximum irrigation system application depth (mm)
       real :: ratirr   ! characteristic irrigation system application rate (mm/hour)
       real :: durirr   ! duration of irrigation water application (hours) 
                        ! corresponding to the characteristic maximum irrigation
                        ! system application depth. This is used to set the rate (depth / duration)
       real :: locirr   ! emitter location point (mm)
                        ! positive is above the soil surface
                        ! negative is below the soil surface
       real :: minirr   ! minimum irrigation application amount (mm)
       integer :: monirr ! flag setting monitoring for irrigation need
                         !  0 - do not monitor irrigation need
                         !  1 - monitor irrigation need
       real :: madirr   ! management allowed depletion used in monitoring irrigation
                        ! 0.0 sets up replacing yesterdays water loss today
                        ! 1.0 schedules next application at wilting point
       integer :: ndayirr ! the next simulation day on which an irrigation can occur (day)
       integer :: mintirr ! minimum interval for irrigation application (days)
       real :: zoutflow ! height of runoff outlet above field surface (m)
       real :: zinf   ! daily surface infiltration (mm)
       real :: zsmt     ! Snow melt (mm)
       real :: zwid     ! Water infiltration depth (mm)
       real :: zeasurf  ! accumulated surface evaporation since last complete rewetting (mm)

    end type hydro_state

end module hydro_data_struct_defs

