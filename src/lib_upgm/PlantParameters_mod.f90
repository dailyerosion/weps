    module plant_parameters_mod
    use constants, only : dp, int32
    implicit none
    private
    type, public :: geometry
        real(dp) :: max_canopy_height = 1.0_dp
    end type geometry
    
    interface geometry
        module procedure geometry_init
    end interface geometry
    
    
    type, public :: plant_parameters
        type(geometry) :: geo
    end type plant_parameters
    
    interface plant_parameters
        module procedure :: newplantpars
    end interface plant_parameters
    
    contains
    
    function geometry_init() result (geo)
    implicit none
    type(geometry) :: geo
    
    geo%max_canopy_height = 0.0_dp
    end function geometry_init
    
    function newplantpars() result (plantpars)
    type(plant_parameters) :: plantpars
    plantpars%geo = geometry()
    end function newplantpars 
        
    end module plant_parameters_mod