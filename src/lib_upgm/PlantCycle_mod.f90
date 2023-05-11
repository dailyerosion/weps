!$Author$
!$Date$
!$Revision$
!$HeadURL$

module plantcycle_mod
    use constants, only: dp, int32, precision_init
    use plant_mod
    use Preprocess_mod
    use Process_Factory
    use phases_mod
    use phase_factory_mod
    use environment_state_mod

    implicit none

    type :: processes
        class(Preprocess), public, pointer :: ptr
    end type processes

    type :: phases
        class(phase), public, pointer :: ptr
    end type phases

    type, public :: plantcycle
        type(plant), public :: plantstate
        type(processes), public:: processes      ! head of process linked list
        type(processes), public:: processCurrent ! used to index through processes
        type(phases), public:: phases       ! head of phase chain
        type(phases), public:: phaseCurrent ! used to index through phases
        type(phases), public:: phaseCurrentSub ! used to index through Secondary(Sub) phases
      contains
      procedure,  pass :: preproc => allProcesses
      procedure,  pass :: add_process => add_new_process
      procedure,  pass :: grow => growplant
      procedure,  pass :: add_phase => add_new_phase
    end type plantcycle

    interface plantcycle
      module procedure :: newplantcycle
    end interface plantcycle
    
  contains

    function newplantcycle() result(pcycle)
      implicit none
      type(plantcycle), pointer :: pcycle
      integer(int32) :: status
      allocate( pcycle, stat=status )
      pcycle%plantstate = plant()
      ! make sure new pointers point to nothing
      nullify(pcycle%processes%ptr)
      nullify(pcycle%processCurrent%ptr)
      nullify(pcycle%phases%ptr)
      nullify(pcycle%phaseCurrent%ptr)
      nullify(pcycle%phaseCurrentSub%ptr)
      ! initialize UPGM specific math precision constants
      call precision_init()
    end function newplantcycle

    subroutine allProcesses(self, env)
      ! Variables
      class(plantcycle), intent(inout) :: self
      type(environment_state), intent(inout) :: env

      ! Body of allProcesses
      do while( associated(self%processCurrent%ptr) ) 
        ! make sure we never try to run anything past last process.
        !run the process
        call self%processCurrent%ptr%doProcess(self%plantstate, env)
        
        self%processCurrent%ptr => self%processCurrent%ptr%processNext
      end do
    end subroutine allProcesses

    subroutine growplant(self, env)
      ! Variables
      class(plantcycle), intent(inout) :: self
      type(environment_state), intent(inout) :: env
      logical ::  succ
      real(dp) :: stagegdd
      integer(int32) :: specificStage, nextstage
      logical :: regrowth_trig

      ! Body of growplant
      ! make sure we never try to run anything past last phase.
      if( associated(self%phaseCurrent%ptr) ) then 
        ! points to a phase

        if( btest(self%phaseCurrent%ptr%phaseType,0) ) then
          ! phaseType status bit is set, execute this phase then set to next phase for execution today
          call self%phaseCurrent%ptr%doPhase(self%plantstate, env)
          call self%phaseCurrent%ptr%phaseState%get("stagegdd", stagegdd, succ)
          write(*,*) 'Phase, stagegdd: ', trim(self%phaseCurrent%ptr%phaseLabel), stagegdd
          ! just go to next phase
          !print *, "state phase complete, next phase requested"
          self%phaseCurrent%ptr => self%phaseCurrent%ptr%phaseChild
          nextstage = 0  ! pass this out to calling routine for use, and reset there
          call self%plantstate%state%replace("nextstage", nextstage, succ)
        end if

        !if( regrowth_trig ) then
        !  if( associated(self%phaseCurrent%ptr%phaseRegrow) ) then
        !    self%phaseCurrent%ptr => self%phaseCurrent%ptr%phaseRegrow
        !    ! reset phase state to start again
        !    stagegdd = 0.0_dp
        !    call self%phaseCurrent%ptr%phaseState%replace("phase_rel_gdd", stagegdd, succ)
        !    call self%phaseCurrent%ptr%phaseState%replace("stagegdd", stagegdd, succ)
        !  end if
        !end if
        
        !run the phase
        call self%phaseCurrent%ptr%doPhase(self%plantstate, env)

        !call self%phaseCurrent%ptr%phaseState%get("stagegdd", stagegdd, succ)
        !write(*,*) 'Phase, stagegdd: ', trim(self%phaseCurrent%ptr%phaseLabel), stagegdd
        
        !if the next stage is ready, check the specific stage value.
        !call self%plantstate%state%get("nextstage", nextstage, succ)

        !if (nextstage == 1.and.succ) then

        !    write(*,*) 'Degree Days: ', stagegdd, ' Phase Completed: ', trim(self%phaseCurrent%ptr%phaseLabel)

            !succ=.false.
            !call self%plantstate%state%get("specstage", specificStage, succ)
            !if (succ) then
                ! zero out stagegdd
            !    stagegdd = 0.0_dp
            !    call self%phaseCurrent%ptr%phaseState%replace("phase_rel_gdd", stagegdd, succ)
            !    call self%phaseCurrent%ptr%phaseState%replace("stagegdd", stagegdd, succ)
            !    if (specificStage .eq. 1) then
            !        print *, "regrow phase requested"
            !        self%phaseCurrent%ptr => self%phaseCurrent%ptr%phaseRegrow
            !        call self%phaseCurrent%ptr%doPhase(self%plantstate, env)
            !    else
                    ! print *, "next phase requested"
                    ! just go to next phase
            !        self%phaseCurrent%ptr => self%phaseCurrent%ptr%phaseChild
            !    endif
            !endif

!            if( associated(self%phaseCurrent%ptr) ) then 
!              ! write info for start of phase
!              write(*,*) 'Phase, stagegdd: ', trim(self%phaseCurrent%ptr%phaseLabel), stagegdd
!            end if

            ! reset controls
            !nextstage = 0  ! pass this out to calling routine for use, and reset there
            !specificStage = 0
            !call self%plantstate%state%replace("nextstage", nextstage, succ)
            !call self%plantstate%state%replace("specstage", specificStage, succ)
      !  endif
      endif
    end subroutine growplant

    subroutine add_new_process(self, processName, processLabel, processType)
      ! adds a new phase to the processNext of the "processCurrent"
      ! when adding processes, be sure to set processCurrent correctly
      ! updates processCurrent to point to the added processs
      ! Variables
      class(plantcycle), intent(inout) :: self
      character(len=*), intent(in) :: processName ! please trim, all lower case.
      character(len=*), intent(in) :: processLabel ! please trim, all lower case.
      integer(int32) :: processType
      ! Body of add_new_process
      !example process: "GDDWeps"
      if( associated(self%processCurrent%ptr) ) then
        select case (processType)
        case (0)    ! primary process
          ! process added to processNext
          self%processCurrent%ptr%processNext => create_process(processName, processLabel)
          if( associated(self%processCurrent%ptr%processNext) ) then
            ! current process pointed to added process
            self%processCurrent%ptr => self%processCurrent%ptr%processNext
            write(*,*) 'Next  Process: ', trim(processName), ' : ', trim(processLabel)
          end if
        end select
      else
        select case (processType)
        case (0)    ! primary process
          ! this is the first process, add to ptr
          self%processes%ptr => create_process(processName, processLabel)
          self%processCurrent%ptr => self%processes%ptr
          write(*,*) 'First Process: ', trim(processName), ' : ', trim(processLabel)
        case (1,2)    ! secondary process, influence
          write(*,*) 'Process type: ', processType, ' cannot be first process.'
        end select
      end if

    end subroutine add_new_process

    subroutine add_new_phase(self, phaseName, phaseLabel, phaseType)
      ! adds a new phase to the phaseChild of the "phaseCurrent"
      ! when adding phases, be sure to set phaseCurrent correctly
      ! updates phaseCurrent to point to the added phase
      ! Variables
      class(plantcycle), intent(inout) :: self
      character(len=*), intent(in) :: phaseName ! please trim, all lower case.
      character(len=*), intent(in) :: phaseLabel ! please trim, all lower case.
      integer :: phaseType

      type(phases) :: thisPhase ! used to index through phases
      type(phases) :: phaseParent ! used to index through phases

      ! Body of add_new_phase
      !example phase: "pmms_germination"
      if( associated(self%phaseCurrent%ptr) ) then
        phaseParent%ptr => self%phaseCurrent%ptr
        select case (phaseType)
        case (0,1,4,8)    ! primary phase
          ! phase added to phaseChild
          self%phaseCurrent%ptr%phaseChild => create_phase(phaseName, phaseLabel, phaseType)
          if( associated(self%phaseCurrent%ptr%phaseChild) ) then
            ! current phase pointed to added phase
            self%phaseCurrent%ptr => self%phaseCurrent%ptr%phaseChild
            write(*,*) 'Next  Phase: ', trim(phaseName), ' : ', trim(phaselabel)
            ! initialize phase pointers
            self%phaseCurrent%ptr%phaseParent => phaseParent%ptr
            nullify(self%phaseCurrent%ptr%phaseChild)
            nullify(self%phaseCurrent%ptr%phaseSub)
            nullify(self%phaseCurrent%ptr%phaseRegrow)
          end if
        case (2)    ! secondary stage
          if( associated(self%phaseCurrentSub%ptr) ) then
            ! add an additional secondary phase
            self%phaseCurrentSub%ptr%phaseChild => create_phase(phaseName, phaseLabel, phaseType)
            if( associated(self%phaseCurrentSub%ptr%phaseChild) ) then
              ! current secondary phase pointed to added phase
              self%phaseCurrentSub%ptr => self%phaseCurrentSub%ptr%phaseChild
              write(*,*) 'Next  Secondary Phase: ', trim(phaseName), ' : ', trim(phaselabel)
            end if
          else
            ! this is the first secondary phase in the current primary phase
            self%phaseCurrent%ptr%phaseSub => create_phase(phaseName, phaseLabel, phaseType)
            if( associated(self%phaseCurrent%ptr%phaseSub) ) then
              ! current secondary phase pointed to added phase
              self%phaseCurrentSub%ptr => self%phaseCurrent%ptr%phaseSub
              write(*,*) 'First Secondary Phase: ', trim(phaseName), ' : ', trim(phaselabel)
            end if
          end if
        !case (4)    ! Regrowth
        !  self%phaseCurrent%ptr%phaseRegrow => create_phase(phaseName, phaseLabel, phaseType)
        !    write(*,*) 'Regrowth Phase: ', trim(phaseName)
        !case (8)    ! Alternate Regrowth
        !  self%phaseCurrent%ptr%phaseRegrow => create_phase(phaseName, phaseLabel, phaseType)
        !    write(*,*) 'Alternate Regrowth Phase: ', trim(phaseName)
        end select

        ! search backward for regrowth phase
        thisPhase%ptr => self%phaseCurrent%ptr
        do while( associated(thisPhase%ptr) )
          if( (thisPhase%ptr%phaseType .eq. 4) .or. (thisPhase%ptr%phaseType .eq. 8) ) then
            self%phaseCurrent%ptr%phaseRegrow => thisPhase%ptr
            exit
          else if( associated(thisPhase%ptr%phaseRegrow) ) then
            self%phaseCurrent%ptr%phaseRegrow => thisPhase%ptr%phaseRegrow
            exit
          end if
          thisPhase%ptr => thisPhase%ptr%phaseParent
        end do

      else
        select case (phaseType)
        case (0,1,4,8)    ! primary phase
          ! this is the first phase, add to ptr
          self%phases%ptr => create_phase(phaseName, phaseLabel, phaseType)
          self%phaseCurrent%ptr => self%phases%ptr
          write(*,*) 'First Phase: ', trim(phaseName), ' : ', trim(phaselabel)
          ! initialize phase pointers
          nullify(self%phaseCurrent%ptr%phaseParent)
          nullify(self%phaseCurrent%ptr%phaseChild)
          nullify(self%phaseCurrent%ptr%phaseSub)
          nullify(self%phaseCurrent%ptr%phaseRegrow)
        case (2)    ! secondary phase
          write(*,*) 'Phase type: ', phaseType, ' cannot be first phase.'
        end select
      end if

    end subroutine add_new_phase

end module plantcycle_mod
