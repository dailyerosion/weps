!$Author$
!$Date$
!$Revision$
!$HeadURL$

module file_io_mod

    ! unit number for file input (lui) / output (luo)
! global in scope, so only one unit required
    integer :: luicli          ! reading cligen input
    integer :: luiwin          ! reading windgen input
    integer :: luiwsd          ! reading wind subdaily

    integer :: luolog          ! write logfil.txt for cmdline and inprun

    integer :: luo_subday      ! for subdaily wind
    integer :: luo_egrd        ! For daily erosion grid
    integer :: luo_erod        ! For daily erosion summary
    integer :: luo_emit        ! For subdaily erosion summary
    integer :: luo_sgrd        ! For subdaily grid

    integer :: luogui1         ! write "gui1_data.out"
    integer :: luoci           ! write "ci.out" for confidence interval

! subregion in scope, so require nsubr dimension for allocation
    ! submodel information (always on)
!    integer, dimension(:), allocatable :: luomandate      ! write "mandate.out"
!    integer, dimension(:), allocatable :: luoseason       ! write season.out for crop end of season (harvest) status

!    integer, dimension(:), allocatable :: luoharvest_si   ! write harvest_si.out for info in SI units
!    integer, dimension(:), allocatable :: luoharvest_en   ! write harvest_en.out for info in English units
!    integer, dimension(:), allocatable :: luohydrobal     ! write hydrobal.out for water balance by crop harvest cycle

    ! submodel detail output (set on/off in weps.run or with command line switches)
!    integer, dimension(:), allocatable :: luoplt          ! write plot.out for erosion (and factors) detail
!    integer, dimension(:), allocatable :: luomanage       ! write manage.out for management detail
!    integer, dimension(:), allocatable :: luocrop         ! write crop.out for crop growth detail
!    integer, dimension(:), allocatable :: luoshoot        ! write shoot.out for shoot growth detail
!    integer, dimension(:), allocatable :: luoinpt         ! write inpt.out for crop input detail
!    integer, dimension(:), allocatable :: luod_above      ! write dabove.out for above ground decomp detail
!    integer, dimension(:), allocatable :: luod_below      ! write dbelow.out for below ground decomp detail
!    integer, dimension(:), allocatable :: luocrp1         ! write decomp.out for select decomp pools detail
!    integer, dimension(:,:), allocatable :: luodec        ! write dec[pool#].btmp for complete decomp pool detail
!    integer, dimension(:), allocatable :: luobio1         ! bio1.btmp for residue totals detail

!    integer, dimension(:), allocatable :: luosci          ! write sci_energy.out for soil conditioning index detail
!    integer, dimension(:), allocatable :: luostir         ! write stir_energy.out for soil tillage intesity rating detail

!    integer, dimension(:), allocatable :: luohydro        ! write hydro.out for hydrology surface details
!    integer, dimension(:), allocatable :: luohlayers      ! write hlayers.out for hydrology subsurface details
!    integer, dimension(:), allocatable :: luowater        ! write water.out for darcy detailed solution info

!    integer, dimension(:), allocatable :: luotempsoil     ! write tempsoil.out for soil temperature details
!    integer, dimension(:), allocatable :: luosoilsurf     ! write "soilsurf.out" for soil surface details
!    integer, dimension(:), allocatable :: luosoillay      ! write "soillay.out" for soil layer details

!    integer, dimension(:), allocatable :: luoharvest_calib ! write harvest_calib.out
!    integer, dimension(:), allocatable :: luoharvest_calib_parm ! write harvest_calib_param.out

!   legacy debugging output files (when "debug" flags set)
!    integer, dimension(:), allocatable :: luohdb          ! write hdbug.out
!    integer, dimension(:), allocatable :: luosdb          ! write sdbug.out
!    integer, dimension(:), allocatable :: luotdb          ! write tdbug.out
!    integer, dimension(:), allocatable :: luocdb          ! write cdbug.out
!    integer, dimension(:), allocatable :: luoddb          ! write ddbug.out

    integer :: luomandate      ! write "mandate.out"
    integer :: luoseason       ! write season.out for crop end of season (harvest) status

    integer :: luoharvest_si   ! write harvest_si.out for info in SI units
    integer :: luoharvest_en   ! write harvest_en.out for info in English units
    integer :: luohydrobal     ! write hydrobal.out for water balance by crop harvest cycle

    ! submodel detail output (set on/off in weps.run or with command line switches)
    integer :: luoplt          ! write plot.out for erosion (and factors) detail
    integer :: luomanage       ! write manage.out for management detail
    integer :: luocrop         ! write crop.out for crop growth detail
    integer :: luoshoot        ! write shoot.out for shoot growth detail
    integer :: luoinpt         ! write inpt.out for crop input detail
    integer, dimension(:), allocatable :: luod_above      ! write dabove.out for above ground decomp detail
    integer, dimension(:), allocatable :: luod_below      ! write dbelow.out for below ground decomp detail
    integer, dimension(:), allocatable :: luocrp1         ! write decomp.out for select decomp pools detail
    integer, dimension(:), allocatable :: luobio1         ! bio1.btmp for residue totals detail

    integer :: luosci          ! write sci_energy.out for soil conditioning index detail
    integer :: luostir         ! write stir_energy.out for soil tillage intesity rating detail

    integer :: luohydro        ! write hydro.out for hydrology surface details
    integer :: luohlayers      ! write hlayers.out for hydrology subsurface details
    integer :: luowater        ! write water.out for darcy detailed solution info

    integer :: luowepphdrive   ! write wepp_runoff.out for epp runoff details
    integer :: luowepperod     ! write wepp_eroevents.out water erosion event details
    integer :: luoweppplot     ! write wepp_eroplot.out
    integer :: luoweppsum      ! write wepp_summary.out

    integer :: luotempsoil     ! write tempsoil.out for soil temperature details
    integer :: luosoilsurf     ! write "soilsurf.out" for soil surface details
    integer :: luosoillay      ! write "soillay.out" for soil layer details

    integer :: luoharvest_calib ! write harvest_calib.out
    integer :: luoharvest_calib_parm ! write harvest_calib_param.out

!   legacy debugging output files (when "debug" flags set)
    integer :: luohdb          ! write hdbug.out
    integer :: luosdb          ! write sdbug.out
    integer :: luotdb          ! write tdbug.out
    integer :: luocdb          ! write cdbug.out
    integer, dimension(:), allocatable :: luoddb          ! write ddbug.out

contains

    ! Provides error trapped opening of files
    subroutine fopenk(filnumber, filname, filstatus)
        integer, intent(out) :: filnumber
        character*(*), intent(in) :: filname
        character*(*), intent(in) :: filstatus
        integer           ios

        open(newunit(filnumber), FILE=filname(1:len_trim(filname)), STATUS=filstatus, POSITION='REWIND', ERR=100, IOSTAT=ios)
        write(*,FMT="(' Opened file: ',a,' on unit ',i3,' with status ',a)") filname(1:len_trim(filname)), filnumber, filstatus
        return

100     write(0,FMT="(' Cannot open file: ',a,' on unit ',i3,' with status ',a, ' and I/O status ', i5)") &
                  filname(1:len_trim(filname)), filnumber, filstatus, ios
        call exit (1)
    end subroutine fopenk

    ! http://fortranwiki.org/fortran/show/newunit
    ! This is a simple function to search for an available unit.
    ! LUN_MIN and LUN_MAX define the range of possible LUNs to check.
    ! The UNIT value is returned by the function, and also by the optional
    ! argument. This allows the function to be used directly in an OPEN
    ! statement, and optionally save the result in a local variable.
    ! If no units are available, -1 is returned.
    integer function newunit(unit)
        integer, intent(out), optional :: unit
        ! local
        integer, parameter :: LUN_MIN=10, LUN_MAX=1000
        logical :: opened
        integer :: lun
        ! begin
        newunit=-1
        do lun=LUN_MIN,LUN_MAX
            inquire(unit=lun,opened=opened)
            if (.not. opened) then
                newunit=lun
                exit
            end if
        end do
        if (present(unit)) unit=newunit
    end function newunit

end module file_io_mod

