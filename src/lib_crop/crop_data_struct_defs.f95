!$Author$
!$Date$
!$Revision$
!$HeadURL$

module crop_data_struct_defs

  integer, dimension(:), allocatable :: am0cfl    ! flag to print CROP output
                                                     ! 0 = no output
                                                     ! 1 = detailed output file created
  integer, dimension(:), allocatable :: am0cdb    ! flag to print CROP variables before and after the call to CROP
                                                     ! 0 = no output
                                                     ! 1 = output
  integer :: tday
  integer :: tmo
  integer :: tyr
  integer :: tisr

  type crop_residue
    ! contains the crop growth values that are "eventually" going to be moved into the
    ! "decomp" pools.  Thus, these are used for temporary storage before that transfer occurs,
    ! ie. like a "temporary" pool.

    ! This is used in crop to handle residue created when a plant that regrows from the crown
    ! is cut or frozen, and any residual above ground crop crop material needs to be moved to residue. 

    ! The "transfer" is then done when crop is exited that day.

    real :: standstem    ! crop standing stem mass (kg/m^2)
    real :: standleaf    ! crop standing leaf mass (kg/m^2)
    real :: standstore   ! crop standing storage mass (kg/m^2) (head with seed, or vegetative head (cabbage, pineapple))

    real :: flatstem    ! crop flat stem mass (kg/m^2)
    real :: flatleaf    ! crop flat leaf mass (kg/m^2)
    real :: flatstore   ! crop flat storage mass (kg/m^2)

    real :: flatrootstore   ! crop flat root storage mass (kg/m^2) (tubers (potatoes, carrots), extended leaf (onion), seeds (peanuts))
    real :: flatrootfiber   ! crop flat root fibrous mass (kg/m^2)

    real, dimension(:), allocatable :: stemz    ! crop buried stem mass by layer (kg/m^2)
    real, dimension(:), allocatable :: leafz    ! crop buried leaf mass by layer (kg/m^2)
    real, dimension(:), allocatable :: storez   ! crop buried storage mass by layer (kg/m^2)

    real, dimension(:), allocatable :: rootstorez   ! crop root storage mass by layer (kg/m^2) (tubers (potatoes, carrots), extended leaf (onion), seeds (peanuts))
    real, dimension(:), allocatable :: rootfiberz   ! crop root fibrous mass by layer (kg/m^2)

    real :: zht    ! Crop height (m)
    real :: dstm   ! Number of crop stems per unit area (#/m^2)
                   ! It is computed by taking the tillering factor times the plant population density.
    double precision :: xstmrep   ! a representative diameter so that acdstm*acxstmrep*aczht=acrsai
    real :: zrtd      ! Crop root depth (m)
    real :: grainf    ! internally computed grain fraction of reproductive mass
  end type crop_residue

contains

  function create_crop_residue(nsoillay) result(cropres)
     integer, intent(in) :: nsoillay
     type(crop_residue) :: cropres

     ! local variable
     integer :: ldx         ! layer index
     integer :: alloc_stat  ! allocation status return
     integer :: sum_stat    ! accumulates allocation status results so only one write/exit statement needed

     sum_stat = 0
     ! allocate below and above ground arrays
     allocate(cropres%stemz(nsoillay), stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat
     allocate(cropres%leafz(nsoillay), stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat
     allocate(cropres%storez(nsoillay), stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat
     allocate(cropres%rootstorez(nsoillay), stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat
     allocate(cropres%rootfiberz(nsoillay), stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat

     if( sum_stat .gt. 0 ) then
        write(*,*) 'ERROR: unable to allocate memory for crop_residue'
        stop 1
     end if

     cropres%standstem = 0.0
     cropres%standleaf = 0.0
     cropres%standstore = 0.0
     cropres%flatstem = 0.0
     cropres%flatleaf = 0.0
     cropres%flatstore = 0.0
     cropres%flatrootstore = 0.0
     cropres%flatrootfiber = 0.0
     do ldx = 1, nsoillay
        cropres%stemz(ldx) = 0.0
        cropres%leafz(ldx) = 0.0
        cropres%storez(ldx) = 0.0
        cropres%rootstorez(ldx) = 0.0
        cropres%rootfiberz(ldx) = 0.0
     end do
     cropres%zht = 0.0
     cropres%dstm = 0.0
     cropres%xstmrep = 0.0
     cropres%zrtd = 0.0
     cropres%grainf = 0.0

  end function create_crop_residue

  subroutine destroy_crop_residue(cropres)
     type(crop_residue), intent(inout) :: cropres

     ! local variable
     integer :: dealloc_stat
     integer :: sum_stat    ! accumulates allocation status results so only one write/exit statement needed

     sum_stat = 0
     ! allocate below and above ground arrays
     deallocate(cropres%stemz, stat=dealloc_stat)
     sum_stat = sum_stat + dealloc_stat
     deallocate(cropres%leafz, stat=dealloc_stat)
     sum_stat = sum_stat + dealloc_stat
     deallocate(cropres%storez, stat=dealloc_stat)
     sum_stat = sum_stat + dealloc_stat
     deallocate(cropres%rootstorez, stat=dealloc_stat)
     sum_stat = sum_stat + dealloc_stat
     deallocate(cropres%rootfiberz, stat=dealloc_stat)
     sum_stat = sum_stat + dealloc_stat

     if( sum_stat .gt. 0 ) then
        write(*,*) 'ERROR: unable to deallocate memory for crop_residue'
     end if
  end subroutine destroy_crop_residue

end module crop_data_struct_defs

