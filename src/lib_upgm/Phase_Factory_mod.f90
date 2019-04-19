module phase_factory_mod
    use phases_mod
    use PhenologyMMSGermination_mod
    use PhenologyMMSBasephenol_mod
    use PhenologyMMSFallphenol_mod
    use PhenologyMMSSpringphenol_mod
    use WEPSShootGrow_mod
    
  contains
    
  function create_phase(phaseName, phaseLabel, phaseType) result(phasePtr)
    class(phase), pointer :: phasePtr
    character(len=*), intent(in) :: phaseName ! please trim, all lower case.
    character(len=*), intent(in) :: phaseLabel ! please trim, all lower case.
    integer :: phaseType ! 0 normal, 1 regrowth, 2 sets a state and next runs immediately

    nullify(phasePtr)

    if (phaseName == "pmms_germination") then
        allocate(PhenologyMMS_Germination :: phasePtr)
    elseif (phaseName == "pmms_basephenol") then
        allocate(PhenologyMMS_Basephenol :: phasePtr)
    elseif (phaseName == "pmms_fallphenol") then
        allocate(PhenologyMMS_Fallphenol :: phasePtr)
    elseif (phaseName == "pmms_springphenol") then
        allocate(PhenologyMMS_Springphenol :: phasePtr)
    elseif(phaseName == "weps_shootgrow") then
        allocate(WEPS_ShootGrow:: phasePtr)
    !elseif(phaseName == "phase") then
        !allocate( phase:: phasePtr)
    else
        nullify(phasePtr)
    endif

    if( associated(phasePtr) ) then
      phasePtr%phaseName = phaseName
      phasePtr%phaseLabel = phaseLabel
      call phasePtr%phasePars%init()
      call phasePtr%phaseState%init()
      phasePtr%phaseType = phaseType
      nullify( phasePtr%phaseChild )
      nullify( phasePtr%phaseSub )
      nullify( phasePtr%phaseRegrow )
    end if
    
  end function create_phase
    
end module phase_factory_mod
