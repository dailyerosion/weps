!$Author:$
!$Date:$
!$Revision:$
!$HeadURL:$

module stir_soil_texture_mod
    implicit none
    private
    
    !soil texture multiplier calculated after the soil is read in
    real, dimension(:), allocatable :: soil_texture_multiplier
    save soil_texture_multiplier

    public :: update_stir_soil_multiplier
    public :: get_stir_soil_multiplier
    public :: create_stir_soil_multiplier
    public :: destroy_stir_soil_multiplier

contains

    !store the stir texture multipler for the given sand and clay values.
    !called at the end of input_ifc.for
    subroutine update_stir_soil_multiplier(sub, sand, clay)

        use hydro_wepp_util_mod, only: usdatx

        integer, intent(in) :: sub       ! subregion number
        real, intent(in) :: sand, clay

        integer :: texclass

        !get the texture class
        call usdatx(sand, clay, texclass)

        !store the multiplier for the given texture class
        !The texture multipliers provided by the RUSLE2 technical documentation.
        !The RUSLE2 Enegery Model (2011-03-08)
        
        select case (texclass)
        case(1) ! SAND
            soil_texture_multiplier(sub) = 0.85
        case(2) ! LOAMY SAND
            soil_texture_multiplier(sub) = 0.875
        case(3) ! SANDY LOAM
            soil_texture_multiplier(sub) = 0.9
        case(4) ! LOAM
            soil_texture_multiplier(sub) = 1.0
        case(5) ! SILT LOAM
            soil_texture_multiplier(sub) = 1.0
        case(6) ! SILT
            soil_texture_multiplier(sub) = 1.0
        case(7) ! SANDY CLAY LOAM
            soil_texture_multiplier(sub) = 1.1
        case(8) ! CLAY LOAM
            soil_texture_multiplier(sub) = 1.20
        case(9) ! SILTY CLAY LOAM
            soil_texture_multiplier(sub) = 1.25
        case(10) ! SANDY CLAY
            soil_texture_multiplier(sub) = 1.225
        case(11) ! SILTY CLAY
            soil_texture_multiplier(sub) = 1.275
        case(12) ! CLAY
            soil_texture_multiplier(sub) = 1.30
        case default
            soil_texture_multiplier(sub) = 1.0
        end select
        
    end subroutine

    ! return the stored texture multiplier.  
    function get_stir_soil_multiplier(sub) result(multiplier)
        real :: multiplier
        integer, intent(in) :: sub

        multiplier = soil_texture_multiplier(sub)
    end function

    ! allocate space for the stored texture multiplier.  
    subroutine create_stir_soil_multiplier(sub)
        integer, intent(in) :: sub
        integer :: alloc_stat

        allocate( soil_texture_multiplier(sub), stat=alloc_stat )

        if( alloc_stat .gt. 0 ) then
           Write(*,*) 'ERROR: unable to allocate enough memory for stir_soil_multiplier array'
        end if
    end subroutine create_stir_soil_multiplier

    subroutine destroy_stir_soil_multiplier
        integer :: dealloc_stat

        deallocate( soil_texture_multiplier, stat=dealloc_stat )

        if( dealloc_stat .gt. 0 ) then
           Write(*,*) 'ERROR: unable to deallocate memory for stir_soil_multiplier array'
        end if
    end subroutine destroy_stir_soil_multiplier

end module stir_soil_texture_mod
