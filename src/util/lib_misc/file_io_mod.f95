!$Author$
!$Date$
!$Revision$
!$HeadURL$

module file_io_mod

    ! unit number for file input (lui) / output (luo)
    ! global in scope, so only one unit required
    integer :: luicli          ! reading cligen input
    integer :: luiwin          ! reading windgen input

    integer :: luolog          ! write logfil.txt for cmdline and inprun

    integer :: luo_subday      ! for subdaily wind
    integer :: luo_egrd        ! For daily erosion grid
    integer :: luo_erod        ! For daily erosion summary
    integer :: luo_emit        ! For subdaily erosion summary
    integer :: luo_sgrd        ! For subdaily grid
    integer :: luo_barr        ! for seasonal barrier daily output

    integer :: luoci           ! write "ci.out" for confidence interval

    ! this is for whole region and subregions
    integer, dimension(:), allocatable :: luogui1         ! write "gui1_data.out"

! subregion in scope, so require nsubr dimension for allocation
    ! submodel information (always on)
    integer, dimension(:), allocatable :: luomandate      ! write "mandate.out"

    integer, dimension(:), allocatable :: luoharvest_si   ! write harvest_si.out for info in SI units
    integer, dimension(:), allocatable :: luoharvest_en   ! write harvest_en.out for info in English units
    integer, dimension(:), allocatable :: luohydrobal     ! write hydrobal.out for water balance by crop harvest cycle
    integer, dimension(:), allocatable :: luoseason       ! write season.out for crop end of season (harvest) status
    integer, dimension(:), allocatable :: luosci          ! write sci_energy.out for soil conditioning index detail
    integer, dimension(:), allocatable :: luostir         ! write stir_energy.out for soil tillage intesity rating detail

    ! submodel detail output (set on/off in weps.run or with command line switches)
    integer, dimension(:), allocatable :: luoplt          ! write plot.out for erosion (and factors) detail
    integer, dimension(:), allocatable :: luomanage       ! write manage.out for management detail
    integer, dimension(:), allocatable :: luocrop         ! write crop.out for crop growth detail
    integer, dimension(:), allocatable :: luoshoot        ! write shoot.out for shoot growth detail
    integer, dimension(:), allocatable :: luoinpt         ! write inpt.out for crop input detail
    integer, dimension(:), allocatable :: luod_above      ! write dabove.out for above ground decomp detail
    integer, dimension(:), allocatable :: luod_below      ! write dbelow.out for below ground decomp detail
    integer, dimension(:), allocatable :: luocrp1         ! write decomp.out for select decomp pools detail
    integer, dimension(:), allocatable :: luobio1         ! bio1.btmp for residue totals detail

    integer, dimension(:), allocatable :: luohydro        ! write hydro.out for hydrology surface details
    integer, dimension(:), allocatable :: luohlayers      ! write hlayers.out for hydrology subsurface details
    integer, dimension(:), allocatable :: luowater        ! write water.out for darcy detailed solution info
    integer, dimension(:), allocatable :: luosurfwat      ! write surfwat.out for specific darcy detailed solution info
    integer, dimension(:), allocatable :: luoweather      ! write specific weather data for hydrology (esp net radiation)

    integer, dimension(:), allocatable :: luotempsoil     ! write tempsoil.out for soil temperature details

    integer, dimension(:), allocatable :: luowepphdrive   ! write wepp_runoff.out for wepp runoff details
    integer, dimension(:), allocatable :: luowepperod     ! write wepp_eroevents.out water erosion event details
    integer, dimension(:), allocatable :: luoweppplot     ! write wepp_eroplot.out
    integer, dimension(:), allocatable :: luoweppsum      ! write wepp_summary.out

    integer, dimension(:), allocatable :: luosoilsurf     ! write "soilsurf.out" for soil surface details
    integer, dimension(:), allocatable :: luosoillay      ! write "soillay.out" for soil layer details

    integer, dimension(:), allocatable :: luoharvest_calib ! write harvest_calib.out
    integer, dimension(:), allocatable :: luoharvest_calib_parm ! write harvest_calib_param.out

    integer, dimension(:), allocatable :: luoasd         ! write asd.out  - For asd analysis - LEW
    integer, dimension(:), allocatable :: luowc          ! write wc.out  - For testing set_wc() - LEW

!   legacy debugging output files (when "debug" flags set)
    integer, dimension(:), allocatable :: luohdb          ! write hdbug.out
    integer, dimension(:), allocatable :: luosdb          ! write sdbug.out
    integer, dimension(:), allocatable :: luotdb          ! write tdbug.out
    integer, dimension(:), allocatable :: luocdb          ! write cdbug.out
    integer, dimension(:), allocatable :: luoddb          ! write ddbug.out

    interface makenamnum
      module procedure makenamnum_1
      module procedure makenamnum_2
    end interface makenamnum

contains

    ! Provides error trapped opening of files
    subroutine fopenk(filnumber, filname, filstatus)
        integer, intent(out) :: filnumber
        character*(*), intent(in) :: filname
        character*(*), intent(in) :: filstatus
        integer           ios

        open(newunit(filnumber), FILE=trim(filname), STATUS=filstatus, POSITION='REWIND', ERR=100, IOSTAT=ios)
        write(*,FMT="(' Opened file: ',a,' on unit ',i3,' with status ',a)") trim(filname), filnumber, filstatus
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

    subroutine makedir(pathplusdirname)
        character(LEN = *), intent(in) :: pathplusdirname
        character(LEN = len_trim(pathplusdirname)+8) :: command

        character(LEN = len_trim(pathplusdirname)) :: tempname
!        integer :: npass
!        integer :: cdx
!        character :: delimiter
        integer :: filnumber

        !npass = 1           ! initial entry
        !delimiter = '/'     ! default value used in input string

        ! check for existence of subdirectory
!  10    open(newunit(filnumber), FILE=trim(pathplusdirname)//'exist.txt', STATUS='UNKNOWN', ERR=100)
        open(newunit(filnumber), FILE=trim(pathplusdirname)//'exist.txt', STATUS='UNKNOWN', ERR=100)
        ! no error, subdirectory exists
        close( filnumber, STATUS='DELETE')
        return
        
 100    continue       

        ! try alternate delimiter character
        !if( npass .eq. 2 ) then
        !    delimiter = '\'
        !else if( npass .gt. 2 ) then
        !    write(*,*) 'subdirectory create failed'
        !    exit(1)
        !end if

        tempname = trim(pathplusdirname) ! assign name to internal variable so it can be modified
        !if( delimiter .ne. '/' ) then
        !    ! try alternate file path delimiter, change it
        !    cdx = index( tempname, '/' )     ! location of delimiter character
        !    do while( cdx .gt. 0 )
        !        tempname(cdx:cdx) = delimiter    ! replace '/' with delimiter
        !        cdx = index( tempname, '/' )     ! location of delimiter character
        !    end do
        !end if

        ! commented out alternate method since putting quotes around command
        ! makes forward slashes work on windows XP both under DOS and CYGWIN
        command='mkdir '//'"'//tempname//'"'     ! command to create subdirectory
        !command='mkdir '//tempname     ! command to create subdirectory

        !write(*,*) command

        CALL system(command)                     ! create subdirectory
        !npass = npass + 1
        !goto 10
    end subroutine makedir

    function makenamnum_2( prename, innum_1, maxnum_1, separator, innum_2, maxnum_2, postname ) result(namnum)
        character(LEN = *), intent(in) :: prename
        integer, intent(in) :: innum_1
        integer, intent(in) :: maxnum_1
        character(LEN = *), intent(in) :: separator
        integer, intent(in) :: innum_2
        integer, intent(in) :: maxnum_2
        character(LEN = *), intent(in) :: postname

        character*40 :: namnum         ! the name/number combination created
        character*30 :: namnum_format  ! format string for creating the name/number combination
        character*10 :: num_1_format   ! format string for creating first number
        character*10 :: num_2_format   ! format string for creating second number
        integer :: numlen_1            ! maximum length of the first number part of the name
        integer :: numlen_2            ! maximum length of the second number part of the name
        integer :: prelen              ! length of the prefix name portion
        integer :: seplen              ! length of the separator name portion
        integer :: postlen             ! length of the postfix name portion

        write( num_1_format, '(i0)' ) maxnum_1
        numlen_1 = len_trim(num_1_format)
        write( num_2_format, '(i0)' ) maxnum_2
        numlen_2 = len_trim(num_1_format)
        prelen = len_trim(prename)
        seplen = len_trim(separator)
        postlen = len_trim(postname)

        if( prelen + numlen_1 + seplen + numlen_2 + postlen .gt. 40 ) then
            write(*,*) 'Name plus numbers combination is too long. Cannot create.'
            write(*,*) 'Name: ', prename, ' Number 1: ', innum_1, &
                 'Separator:', separator, ' Number 2: ', innum_2, 'Extension: ', postname
            stop 1
        end if
        write( namnum_format, '(a2, i0, a5, i0, a3, i0, a5, i0, a3, i0, a1)' )  &
                              '(a', prelen, ', i0.', numlen_1, ', a', seplen, ', i0.', numlen_2, ', a', postlen, ')'

        ! create the name
        write( namnum, namnum_format) trim(prename), innum_1, separator, innum_2, trim(postname)

    end function makenamnum_2

    function makenamnum_1( prename, innumber, maxnumber, postname ) result(namnum)
        character(LEN = *), intent(in) :: prename
        integer, intent(in) :: innumber
        integer, intent(in) :: maxnumber
        character(LEN = *), intent(in) :: postname

        character*30 :: namnum         ! the name/number combination created
        character*20 :: namnum_format  ! format string for creating the name/number combination
        integer :: numlen              ! maximum length of the number part of the name
        integer :: prelen              ! length of the prefix name portion
        integer :: postlen             ! length of the postfix name portion

        write( namnum_format, '(i0)' ) maxnumber
        numlen = len_trim(namnum_format)
        prelen = len_trim(prename)
        postlen = len_trim(postname)

        if( prelen + numlen + postlen .gt. 30 ) then
            write(*,*) 'Name plus number combination is too long. Cannot create.'
            write(*,*) 'Name: ', prename, ' Number: ', innumber, 'Extension: ', postname
            stop 1
        end if
        write( namnum_format, '(a2, i0, a5, i0, a3, i0, a1)' ) '(a', prelen, ', i0.', numlen, ', a', postlen, ')'

        ! create the name
        write( namnum, namnum_format) trim(prename), innumber, trim(postname)

    end function makenamnum_1

end module file_io_mod

