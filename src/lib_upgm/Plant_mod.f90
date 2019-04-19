    module plant_mod
    use UPGM_state
    use plant_parameters_mod
    implicit none
    
    type, public :: plant
        type(hash_state) :: pars
        type(hash_state) :: state
        logical, private :: initialized = .false.
    contains
        !final :: destroy_plant
        procedure, pass(self) :: init => initPlant
    end type plant
    
    interface plant
        module procedure :: newplant
    end interface plant
        
    contains
    
    function newplant() result(plt)
    implicit none
    type(plant) :: plt
    plt%pars = hash_state()
    plt%state = hash_state()
    end function newplant
    
    subroutine initPlant(self)
      class(plant), intent(inout) :: self
      call self%pars%init()
      call self%state%init()
    end subroutine initPlant

    !subroutine initPlantWPhases(self, phaseNames)
    !class(plant), intent(inout) :: self
    !character(len=*), dimension(:), intent(in) :: phases
    !allocate(plant_phases(size(phases))) ! n phases, allocate them here
    !stage_pointer = 1
    !initialized = .true.
    !end subroutine initPlantWPhases
    !
    !subroutine destory_plant(self)
    !! Variables
    !type(plant), intent(inout) :: self
    !! Body of destory_plant
    !
    !end subroutine destory_plant
    
    end module plant_mod
