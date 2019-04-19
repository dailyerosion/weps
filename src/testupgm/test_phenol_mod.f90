    module test_phenol_mod
    use upgm_mod
     use phase_factory_mod
    use phases_mod
    use constants, only : dp, int32
    use environment_state_mod
    implicit none

    contains

    subroutine test_phenol()
    implicit none
    type(upgm) :: model
    class(phase), pointer :: stage
    type(environment_state) :: env
    real(dp), dimension(5) :: soil_moisture
    real(dp), dimension(4) :: gdd_resp, gdd_curve
    real(dp) :: daygdd, stagegdd
    integer(int32) :: p_depth
    logical :: success = .false.
    ! Body of test_phenol
    print *, "start test phenol"
    env = environment_state()
    call env%init()
    model =  UPGM()
    call model%plant%plantstate%init()
!    allocate(PhenologyMMS_Germination :: stage) ! create germination method
    call model%plant%add_phase("pmms_germination", "Germination", 0)
    !stage => create_phase("pmms_germination")
    ! "ratio pore space filled"
    soil_moisture = [0.45, 0.35, 0.30, 0.35, 0.32]
    !"ratio" response
    gdd_curve = [0.45, 0.35, 0.25, -0.1]
    ! corresponding gdd value
    gdd_resp = [25, 30, 35, 600]
    ! planting in layer 2
    p_depth = 2

    stagegdd = 0.0_dp

    call env%state%put("swc", soil_moisture, success)
    print *, "swc inserted into environment", soil_moisture, " success=", success

    call model%plant%plantstate%state%put("p_depth", p_depth, success)
    print *, "p_depth inserted into plant", p_depth, " success=", success

    call model%plant%plantstate%state%put("gdd_curve", gdd_curve, success)
    print *, "gdd_curve inserted into plant", gdd_curve, " success=", success

    call model%plant%plantstate%state%put("gdd_resp", gdd_resp, success)
    print *, "gdd_resp inserted into plant", gdd_resp, " success=", success

    call model%plant%plantstate%state%put("stagegdd", stagegdd, success)
    print *, "stagegdd inserted into plant", stagegdd, " success=", success

    daygdd = 13.0
    call model%plant%plantstate%state%put("daygdd", daygdd, success)
    print *, "daygdd inserted into plant", daygdd, " success=", success

    !call stage%doPhase(model%plant%plantstate,env)
    call model%grow(env)
    print *,""
    daygdd = 18.0
    call model%plant%plantstate%state%replace("daygdd", daygdd, success)
    print *, "daygdd inserted into plant", daygdd, " success=", success
    !call stage%doPhase(model%plant%plantstate,env)
    call model%grow(env)

    call UPGM_DELETE(model)

    end subroutine test_phenol

    end module test_phenol_mod
