!$Author$
!$Date$
!$Revision$
!$HeadURL$

program test_compaction

  ! test of soil compaction routines

  use file_io_mod, only: fopenk
  use soilden_mod

  ! variable definitions
  character :: line*256               ! character buffer for input line
  integer :: lui1                     ! input file logical unit number
  integer :: luo1                     ! output file logical unit number
  integer :: ios                      ! input output status
  integer :: rds                      ! line read status
  character*20 :: tmpchar3(3)         ! temporary string array for test reading 3 string values
  integer :: tmpinteger1(1)           ! temporary integer array for reading 1 interger values
  integer :: tmpinteger3(3)           ! temporary integer array for reading 3 interger values
  real :: tmpreal6(6)                 ! temporary real array for test reading 6 values
  real :: tmpreal4(4)                 ! temporary real array for test reading 4 values
  integer :: class                    ! soil class index
  logical :: proc_serdp               ! command line switch
  real :: proc_wc_0_5                 ! water content adjusted bulk density 0-5cm depth
  real :: proc_wc_5_10                ! water content adjusted bulk density 5-10cm depth
  real :: proc_wc_10_15               ! water content adjusted bulk density 10-15cm depth
  real :: BD_norm_0_5                 ! normalized bulk density 0-5cm depth
  real :: BD_norm_5_10                ! normalized bulk density 5-10cm depth
  real :: BD_norm_10_15               ! normalized bulk density 10-15cm depth

  integer :: idx                      ! generic index
  integer :: nsoil                    ! number of soils
  integer, dimension(:), allocatable :: datanumber  ! data point number
  integer, dimension(:), allocatable :: plotnumber  ! plot number
  integer, dimension(:), allocatable :: rep_number  ! pass number
  integer, dimension(:), allocatable :: passnumber  ! pass number
  character(LEN=7), dimension(:), allocatable :: soilcode  ! Soil Code
  character(LEN=3), dimension(:), allocatable :: trackcode  ! Track Code (wheel or track)
  character(LEN=3), dimension(:), allocatable :: loc_code  ! Locate Code
  character(LEN=7), dimension(:), allocatable :: soilname  ! soil texture class names
!  real, dimension(:), allocatable :: veg_cover    ! Vegetation cover (%)
  real, dimension(:), allocatable :: bsdbd_0_5    ! bulk density 0 to 5 cm (Mg/m^3) = (g/cm^3)
  real, dimension(:), allocatable :: bsdbd_5_10   ! bulk density 5 to 10 cm (Mg/m^3) = (g/cm^3)
  real, dimension(:), allocatable :: bsdbd_10_15  ! bulk density 10 to 15 cm (Mg/m^3) = (g/cm^3)
  real, dimension(:), allocatable :: bhrwc_0_5    ! gravimetric water content 0 to 5 cm (g/g)
  real, dimension(:), allocatable :: bhrwc_5_10   ! gravimetric water content 5 to 10 cm (g/g)
  real, dimension(:), allocatable :: bhrwc_10_15  ! gravimetric water content 10 to 15 cm (g/g)
  real, dimension(:), allocatable :: bsdblk   ! bulk density (Mg/m^3) = (g/cm^3)
  real, dimension(:), allocatable :: bsdsblk  ! settled bulk density (Mg/m^3) = (g/cm^3)
  real, dimension(:), allocatable :: bsdprocblk  ! proctor bulk density (Mg/m^3) = (g/cm^3)
  real, dimension(:), allocatable :: bsdpart  ! particle density (Mg/m^3)
  real, dimension(:), allocatable :: bsfcla   ! fraction of soil mineral portion which is clay
  real, dimension(:), allocatable :: bsfsil   ! fraction of soil mineral portion which is silt
  real, dimension(:), allocatable :: bsfsan   ! fraction of soil mineral portion which is sand
  real, dimension(:), allocatable :: bsfom    ! fraction of total soil mass which is organic matter
  real, dimension(:), allocatable :: bhrwc    ! gravimetric water content

  ! Declarations for command line arguments
  INTEGER  COMMAND_ARGUMENT_COUNT ! required by the Lahey f95 compiler
  EXTERNAL COMMAND_ARGUMENT_COUNT
  character*512 argv    ! For Fortran 2k commandline parsing
  integer       icmd    ! index through numarg commands
  integer       numarg  ! number of command line arguments
  integer       ll,ss   ! length and status return

  ! check command line arguments
  numarg = COMMAND_ARGUMENT_COUNT()  !Fortran 2k compatible call

  if (numarg .gt. 0) then
    do icmd = 1, numarg
      call GET_COMMAND_ARGUMENT(icmd,argv,ll,ss)  !Fortran 2k compatible call
      select case (argv(1:4))
        case ('serd')
          proc_serdp = .TRUE.
        case default
          write(*,*) 'No valid commmand line arguments. Rerun without arguments to see options.'
          stop          
      end select
    end do
  else
    write(*,*) 'No command line arguments, No operation performed.'
    write(*,*) 'Options are: serdp'
    write(*,*) '   serdp - process SERDP bulk density data set'
    write(*,*) 'test_compaction [serdp]'
    stop
  end if

  if( proc_serdp ) then
    ! open test data set file
    call fopenk (lui1, 'Veg_BD_data.dat', 'old')

    ! count valid data points
    ! initial count
    nsoil = 0
    ! read first line
    read (lui1,'(a)',iostat=ios) line
    do while( ios .eq. 0 )
      ! parse line for data points
      ! Sample_# SoilCode Average%_Clay Average%_Silt Average%_Sand Average%_OM TrackCode Plot Rep Pass Loc 5cm_BD_(0-5cm) 10cm_BD_(5-10cm) 15cm_BD_(10-15cm) 5cm_WC_(0-5cm) 10cm_WC_(5-10cm) 15cm_WC_(10-15cm)
      read (line,*, iostat=rds) tmpinteger1, tmpchar3(1), tmpreal4, tmpchar3(2), tmpinteger3, tmpchar3(3), tmpreal6
      if( rds .eq. 0 ) then
        ! line read properly
        nsoil = nsoil + 1
      end if
      ! read next line
      read (lui1,'(a)',iostat=ios) line
    end do

    write(*,*) '# Number of Values to be output: ', nsoil
  end if

  ! index file back to beginning
  rewind( lui1 )

  ! allocate arrays
  allocate( datanumber(nsoil) )
  allocate( plotnumber(nsoil) )
  allocate( rep_number(nsoil) )
  allocate( passnumber(nsoil) )
  allocate( soilcode(nsoil) )
  allocate( loc_code(nsoil) )
  allocate( trackcode(nsoil) )
  allocate( soilname(nsoil) )
!  allocate( veg_cover(nsoil) )
  allocate( bsdbd_0_5(nsoil) )
  allocate( bsdbd_5_10(nsoil) )
  allocate( bsdbd_10_15(nsoil) )
  allocate( bhrwc_0_5(nsoil) )
  allocate( bhrwc_5_10(nsoil) )
  allocate( bhrwc_10_15(nsoil) )
  allocate( bsdblk(nsoil) )
  allocate( bsdsblk(nsoil) )
  allocate( bsdprocblk(nsoil) )
  allocate( bsdpart(nsoil) )
  allocate( bsfcla(nsoil) )
  allocate( bsfsil(nsoil) )
  allocate( bsfsan(nsoil) )
  allocate( bsfom(nsoil) )
  allocate( bhrwc(nsoil) )

  if( proc_serdp ) then
    ! set initial index
    idx = 0
    ! read first line
    read (lui1,'(a)',iostat=ios) line
    do while( ios .eq. 0 )
      ! parse line for data points
      read (line,*, iostat=rds) tmpinteger1, tmpchar3(1), tmpreal4, tmpchar3(2), tmpinteger3, tmpchar3(3), tmpreal6
      if( rds .eq. 0 ) then
        ! set index for this input value
        idx = idx + 1
        ! assign data name
        datanumber(idx) = tmpinteger1(1)
        soilcode(idx) = trim(tmpchar3(1))
        ! convert from % to fraction
        bsfcla(idx) = tmpreal4(1) / 100.0
        bsfsil(idx) = tmpreal4(2) / 100.0
        bsfsan(idx) = tmpreal4(3) / 100.0
        bsfom(idx) = tmpreal4(4) / 100.0
        trackcode(idx) = trim(tmpchar3(2))
        plotnumber(idx) = tmpinteger3(1)
        rep_number(idx) = tmpinteger3(2)
        passnumber(idx) = tmpinteger3(3)
        loc_code(idx) = trim(tmpchar3(3))
        bsdbd_0_5(idx) = tmpreal6(1)
        bsdbd_5_10(idx) = tmpreal6(2)
        bsdbd_10_15(idx) = tmpreal6(3)
        ! convert from % to fraction
        bhrwc_0_5(idx) = tmpreal6(4) / 100.0
        bhrwc_5_10(idx) = tmpreal6(5) / 100.0
        bhrwc_10_15(idx) = tmpreal6(6) / 100.0
        call usdatx( bsfsan(idx), bsfcla(idx), class)
        call usda_tx_name_frm_class( class, soilname(idx) )
      end if
      ! read next line
      read (lui1,'(a)',iostat=ios) line
    end do
  end if
  close(lui1)

  ! find densities adjusted for organic matter
  call proptext( nsoil, bsfcla, bsfsan, bsfom, bsdsblk, bsdprocblk, bsdpart )
    
  if( proc_serdp ) then
    ! open output file
    call fopenk (luo1, 'Veg_BD_norm.dat', 'replace')
    !write(luo1,'(a)',ADVANCE="NO") 'data.num SoilCode TrackCode Plot Rep Pass Loc soil.name BD.settled BD.proc Paticle.den '
    !write(luo1,'(a)',ADVANCE="NO") 'BD.proc.wc.0.5cm BD.proc.wc.5.10cm BD.proc.wc.10.15cm '
    !write(luo1,'(a)',ADVANCE="NO") 'bsdbd_0_5cm bsdbd_5_10cm bsdbd_10_15cm '
    !write(luo1,*) 'BD.norm.wc.0.5cm BD.norm.wc.5.10cm BD.norm.wc.10.15cm'
    !do idx = 1, nsoil
    !  proc_wc_0_5 = setbdproc_wc( bsfcla(idx), bsfsan(idx), bsfom(idx), bsdpart(idx), bhrwc_0_5(idx))
    !  proc_wc_5_10 = setbdproc_wc( bsfcla(idx), bsfsan(idx), bsfom(idx), bsdpart(idx), bhrwc_5_10(idx))
    !  proc_wc_10_15 = setbdproc_wc( bsfcla(idx), bsfsan(idx), bsfom(idx), bsdpart(idx), bhrwc_10_15(idx))
    !  BD_norm_0_5 = (bsdbd_0_5(idx) - bsdsblk(idx)) / (proc_wc_0_5 - bsdsblk(idx))
    !  BD_norm_5_10 = (bsdbd_5_10(idx) - bsdsblk(idx)) / (proc_wc_5_10 - bsdsblk(idx))
    !  BD_norm_10_15 = (bsdbd_10_15(idx) - bsdsblk(idx)) / (proc_wc_10_15 - bsdsblk(idx))
    !  write(luo1,*) datanumber(idx), soilcode(idx), trackcode(idx), plotnumber(idx), rep_number(idx), passnumber(idx), &
    !                loc_code(idx), soilname(idx), bsdsblk(idx), bsdprocblk(idx), bsdpart(idx), &
    !                proc_wc_0_5, proc_wc_5_10, proc_wc_10_15, &
    !                bsdbd_0_5(idx), bsdbd_5_10(idx), bsdbd_10_15(idx), &
    !                BD_norm_0_5, BD_norm_5_10, BD_norm_10_15
    write(luo1,'(a)',ADVANCE="NO") 'SoilCode TrackCode Plot Rep Pass Loc '
    write(luo1,*) 'BD.norm.wc.0.5cm BD.norm.wc.5.10cm BD.norm.wc.10.15cm '
    do idx = 1, nsoil
      proc_wc_0_5 = setbdproc_wc( bsfcla(idx), bsfsan(idx), bsfom(idx), bsdpart(idx), bhrwc_0_5(idx))
      proc_wc_5_10 = setbdproc_wc( bsfcla(idx), bsfsan(idx), bsfom(idx), bsdpart(idx), bhrwc_5_10(idx))
      proc_wc_10_15 = setbdproc_wc( bsfcla(idx), bsfsan(idx), bsfom(idx), bsdpart(idx), bhrwc_10_15(idx))
      BD_norm_0_5 = (bsdbd_0_5(idx) - bsdsblk(idx)) / (proc_wc_0_5 - bsdsblk(idx))
      BD_norm_5_10 = (bsdbd_5_10(idx) - bsdsblk(idx)) / (proc_wc_5_10 - bsdsblk(idx))
      BD_norm_10_15 = (bsdbd_10_15(idx) - bsdsblk(idx)) / (proc_wc_10_15 - bsdsblk(idx))
      write(luo1,*) soilcode(idx), trackcode(idx), plotnumber(idx), rep_number(idx), passnumber(idx), &
                    loc_code(idx), BD_norm_0_5, BD_norm_5_10, BD_norm_10_15

    end do
    close(luo1)
  end if

end program test_compaction

subroutine usda_tx_name_frm_class( class, soilname )
  integer class
  character(LEN=7) :: soilname  ! soil texture class name

  select case (class)
  case (1)
    soilname = 'S'
  case (2)
    soilname = 'LS'
  case (3)
    soilname = 'SL'
  case (4)
    soilname = 'L'
  case (5)
    soilname = 'SiL'
  case (6)
    soilname = 'Si'
  case (7)
    soilname = 'SCL'
  case (8)
    soilname = 'CL'
  case (9)
    soilname = 'SiCL'
  case (10)
    soilname = 'SC'
  case (11)
    soilname = 'SiC'
  case (12)
    soilname = 'C'
  end select
end subroutine usda_tx_name_frm_class


