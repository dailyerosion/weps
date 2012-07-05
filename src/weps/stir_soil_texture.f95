module stir_soil_texture
    implicit none
    private
    
    include "p1werm.inc"

    !soil texture multiplier calculated after the soil is read in
    real :: soil_texture_multiplier (mnsub)
    save soil_texture_multiplier

    public :: update_stir_soil_multiplier
    public :: get_stir_soil_multiplier

    

contains

    !store the stir texture multipler for the given sand and clay values.
    !called at the end of input_ifc.for
    subroutine update_stir_soil_multiplier(sub, sand, clay)
        integer, intent(in) :: sub
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

    !return the stored texture multiplier.  
    function get_stir_soil_multiplier(sub) result(multiplier)
        real :: multiplier
        integer, intent(in) :: sub

        multiplier = soil_texture_multiplier(sub)
    end function


end module