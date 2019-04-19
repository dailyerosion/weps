module Process_Factory
    use Preprocess_mod
    use gddmethod1_mod
    use gddmethodWEPS_mod
    use ritchieVernalization_mod
    use ritchieHardening_mod
    use WEPSwarmdays_mod
    use WEPStempstress_mod
    use WEPSFreezeDamage_mod

  contains
    
    function create_process(processName) result(processPtr)
      class(preprocess), pointer :: processPtr
      character(len=*), intent(in) :: processName ! please trim, all lower case.
    
      if (processName == "gddmethod1") then
        allocate(gdd1_method :: processPtr)
      elseif (processName == "gddweps_method") then
        allocate(gddWEPS_method :: processPtr)
      elseif (processName == "ritchie_vernalization") then
        allocate(ritchieVernalization :: processPtr)
      elseif (processName == "ritchie_winterhardening") then
        allocate(ritchieHardening :: processPtr)
      elseif (processName == "weps_warmdays") then
        allocate(WEPSwarmdays :: processPtr)
      elseif (processName == "weps_tempstress") then
        allocate(WEPSTempStress :: processPtr)
      elseif (processName == "weps_freezedamage") then
        allocate(WEPSFreezeDamage :: processPtr)
      else
        nullify(processPtr)
      endif

    end function create_process
    
end module Process_Factory
