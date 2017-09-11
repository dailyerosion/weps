!$Author$
!$Date$
!$Revision$
!$HeadURL$

module manage_xml_mod

  use flib_sax
  use manage_data_struct_defs, only: manFile, operation_date
  use manage_data_struct_defs, only: MAX_NAME_LEN, max_ogp, param_nt
  use manage_data_struct_mod, only: elemCreate, get_value_type_index

  type :: tag_def
    character(len=MAX_NAME_LEN)  :: name   ! tag name
    logical :: acquired                    ! .true. if tag has been read
    logical :: in_tag                      ! .true. if inside tag now
  end type tag_def

  type(tag_def), dimension(:), allocatable :: man_tag
  integer :: max_tags

  integer, parameter, public :: rotationyears = 1
  integer, parameter, public :: wepsmanvalue = 2
  integer, parameter, public :: date = 3
  integer, parameter, public :: operationDB = 4
  integer, parameter, public :: operationname = 5
  integer, parameter, public :: actionvalue = 6
  integer, parameter, public :: identity = 7
  integer, parameter, public :: code = 8
  integer, parameter, public :: id = 9
  integer, parameter, public :: param = 10
  integer, parameter, public :: p_name = 11
  integer, parameter, public :: value = 12
  integer, parameter, public :: version = 13
  integer, parameter, public :: wepsmanDB = 14

  integer :: isub ! current subregion number used in a routines in this module
  type(operation_date) :: t_operDate
  character(len=80) :: t_operName
  character(len=3) :: t_code
  character(len=4) :: p_type
  integer :: ogp_id_idx
  integer :: p_idx

  character(len=3) :: operID
  character(len=3) :: grpID
  character(len=3) :: procID

  logical :: all_wepsmanvalues
  logical :: all_operationDBs
  logical :: all_actionvalues
  logical :: all_params
  logical :: manfile_complete ! indicator that a complete manfile was read

  interface check_params
    module procedure oper_check_params
    module procedure grp_check_params
    module procedure proc_check_params
  end interface check_params

  interface readValues
    module procedure readOperValuesV1
    module procedure readOperValuesV2
    module procedure readOperValuesV5
    module procedure readOperValuesV7
    module procedure readGrpValuesV1
    module procedure readGrpValuesV6
    module procedure readProcValuesV1
    module procedure readProcValuesV2
    module procedure readProcValuesV3
    module procedure readProcValuesV4
    module procedure readProcValuesV5
    module procedure readProcValuesV6
    module procedure readProcValuesV7
    module procedure readProcValuesV8
    module procedure readProcValuesV9
    module procedure readProcValuesV12
  end interface readValues

contains

  subroutine init_man_xml( isubr )
    integer, intent(in) :: isubr

    integer :: idx
    integer :: sum_stat
    integer :: alloc_stat

    ! set subregion index used with manFile
    isub = isubr

    max_tags = 14   ! count of unique tags needed from management files
    allocate( man_tag(max_tags), stat=alloc_stat)
    if( alloc_stat .gt. 0 ) then
      write(*,*) 'ERROR: memory alloc., input_tag'
    end if

    ! assign defaults to flag status values
    do idx = 1, max_tags
      man_tag(idx)%acquired = .false.
      man_tag(idx)%in_tag = .false.
    end do

   ! assign tag names
    man_tag(1)%name = "rotationyears"
    man_tag(2)%name = "wepsmanvalue"
    man_tag(3)%name = "date"
    man_tag(4)%name = "operationDB"
    man_tag(5)%name = "operationname"
    man_tag(6)%name = "actionvalue"
    man_tag(7)%name = "identity"
    man_tag(8)%name = "code"
    man_tag(9)%name = "id"
    man_tag(10)%name = "param"
    man_tag(11)%name = "name"
    man_tag(12)%name = "value"
    man_tag(13)%name = "version"
    man_tag(14)%name = "wepsmanDB"

    max_ogp = 42   ! count of total number of operations, groups, and processes
    sum_stat = 0
    allocate( param_nt(max_ogp), stat=alloc_stat)
    sum_stat = sum_stat + alloc_stat
    param_nt(1)%ogp="O"
    param_nt(1)%id="00"
    allocate( param_nt(1)%i_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(1)%r_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(1)%s_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    param_nt(2)%ogp="O"
    param_nt(2)%id="01"
    allocate( param_nt(2)%i_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(2)%r_name(5), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(2)%s_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    param_nt(2)%r_name(1)="ospeed"
    param_nt(2)%r_name(2)="odirect"
    param_nt(2)%r_name(3)="ostdspeed"
    param_nt(2)%r_name(4)="ominspeed"
    param_nt(2)%r_name(5)="omaxspeed"
    param_nt(3)%ogp="O"
    param_nt(3)%id="02"
    allocate( param_nt(3)%i_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(3)%r_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(3)%s_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    param_nt(4)%ogp="O"
    param_nt(4)%id="03"
    allocate( param_nt(4)%i_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(4)%r_name(7), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(4)%s_name(1), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    param_nt(4)%r_name(1)="oenergyarea"
    param_nt(4)%r_name(2)="ostir"
    param_nt(4)%r_name(3)="ospeed"
    param_nt(4)%r_name(4)="odirect"
    param_nt(4)%r_name(5)="ostdspeed"
    param_nt(4)%r_name(6)="ominspeed"
    param_nt(4)%r_name(7)="omaxspeed"
    param_nt(4)%s_name(1)="ofuel"
    param_nt(5)%ogp="O"
    param_nt(5)%id="04"
    allocate( param_nt(5)%i_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(5)%r_name(2), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(5)%s_name(1), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    param_nt(5)%r_name(1)="oenergyarea"
    param_nt(5)%r_name(2)="ostir"
    param_nt(5)%s_name(1)="ofuel"
    param_nt(6)%ogp="G"
    param_nt(6)%id="01"
    allocate( param_nt(6)%i_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(6)%r_name(6), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(6)%s_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    param_nt(6)%r_name(1)="gtdepth"
    param_nt(6)%r_name(2)="gtilint"
    param_nt(6)%r_name(3)="gtilArea"
    param_nt(6)%r_name(4)="gtstddepth"
    param_nt(6)%r_name(5)="gtmindepth"
    param_nt(6)%r_name(6)="gtmaxdepth"
    param_nt(7)%ogp="G"
    param_nt(7)%id="02"
    allocate( param_nt(7)%i_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(7)%r_name(1), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(7)%s_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    param_nt(7)%r_name(1)="gbioarea"
    param_nt(8)%ogp="G"
    param_nt(8)%id="03"
    allocate( param_nt(8)%i_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(8)%r_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(8)%s_name(1), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    param_nt(8)%s_name(1)="gcropname"
    param_nt(9)%ogp="G"
    param_nt(9)%id="04"
    allocate( param_nt(9)%i_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(9)%r_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(9)%s_name(1), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    param_nt(9)%s_name(1)="gamdname"
    param_nt(10)%ogp="P"
    param_nt(10)%id="01"
    allocate( param_nt(10)%i_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(10)%r_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(10)%s_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    param_nt(11)%ogp="P"
    param_nt(11)%id="02"
    allocate( param_nt(11)%i_name(1), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(11)%r_name(1), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(11)%s_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    param_nt(11)%i_name(1)="rroughflag"
    param_nt(11)%r_name(1)="rrough"
    param_nt(12)%ogp="P"
    param_nt(12)%id="05"
    allocate( param_nt(12)%i_name(1), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(12)%r_name(5), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(12)%s_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    param_nt(12)%i_name(1)="rdgflag"
    param_nt(12)%r_name(1)="rdghit"
    param_nt(12)%r_name(2)="rdgspac"
    param_nt(12)%r_name(3)="rdgwidth"
    param_nt(12)%r_name(4)="dkhit"
    param_nt(12)%r_name(5)="dkspac"
    param_nt(13)%ogp="P"
    param_nt(13)%id="11"
    allocate( param_nt(13)%i_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(13)%r_name(2), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(13)%s_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    param_nt(13)%r_name(1)="asdf"
    param_nt(13)%r_name(2)="crif"
    param_nt(14)%ogp="P"
    param_nt(14)%id="12"
    allocate( param_nt(14)%i_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(14)%r_name(1), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(14)%s_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    param_nt(14)%r_name(1)="soilos"
    param_nt(15)%ogp="P"
    param_nt(15)%id="13"
    allocate( param_nt(15)%i_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(15)%r_name(1), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(15)%s_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    param_nt(15)%r_name(1)="laymix"
    param_nt(16)%ogp="P"
    param_nt(16)%id="14"
    allocate( param_nt(16)%i_name(1), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(16)%r_name(5), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(16)%s_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    param_nt(17)%ogp="P"
    param_nt(17)%id="24"
    allocate( param_nt(17)%i_name(1), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(17)%r_name(5), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(17)%s_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    param_nt(17)%i_name(1)="fbioflagvt"
    param_nt(17)%r_name(1)="massflatvt1"
    param_nt(17)%r_name(2)="massflatvt2"
    param_nt(17)%r_name(3)="massflatvt3"
    param_nt(17)%r_name(4)="massflatvt4"
    param_nt(17)%r_name(5)="massflatvt5"
    param_nt(18)%ogp="P"
    param_nt(18)%id="25"
    allocate( param_nt(18)%i_name(1), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(18)%r_name(5), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(18)%s_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    param_nt(18)%i_name(1)="burydist"
    param_nt(18)%r_name(1)="massburyvt1"
    param_nt(18)%r_name(2)="massburyvt2"
    param_nt(18)%r_name(3)="massburyvt3"
    param_nt(18)%r_name(4)="massburyvt4"
    param_nt(18)%r_name(5)="massburyvt5"
    param_nt(19)%ogp="P"
    param_nt(19)%id="26"
    allocate( param_nt(19)%i_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(19)%r_name(5), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(19)%s_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    param_nt(19)%r_name(1)="massresurvt1"
    param_nt(19)%r_name(2)="massresurvt2"
    param_nt(19)%r_name(3)="massresurvt3"
    param_nt(19)%r_name(4)="massresurvt4"
    param_nt(19)%r_name(5)="massresurvt5"
    param_nt(20)%ogp="P"
    param_nt(20)%id="31"
    allocate( param_nt(20)%i_name(1), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(20)%r_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(20)%s_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    param_nt(20)%i_name(1)="kilflag"
    param_nt(21)%ogp="P"
    param_nt(21)%id="32"
    allocate( param_nt(21)%i_name(1), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(21)%r_name(4), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(21)%s_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    param_nt(21)%i_name(1)="cutflag"
    param_nt(21)%r_name(1)="cutvalh"
    param_nt(21)%r_name(2)="cyldrmh"
    param_nt(21)%r_name(3)="cplrmh"
    param_nt(21)%r_name(4)="cstrmh"
    param_nt(22)%ogp="P"
    param_nt(22)%id="33"
    allocate( param_nt(22)%i_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(22)%r_name(4), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(22)%s_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    param_nt(22)%r_name(1)="cutvalf"
    param_nt(22)%r_name(2)="cyldrmf"
    param_nt(22)%r_name(3)="cplrmf"
    param_nt(22)%r_name(4)="cstrmf"
    param_nt(23)%ogp="P"
    param_nt(23)%id="34"
    allocate( param_nt(23)%i_name(1), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(23)%r_name(10), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(23)%s_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    param_nt(23)%i_name(1)="frselpool"
    param_nt(23)%r_name(1)="ratemultvt1"
    param_nt(23)%r_name(2)="ratemultvt2"
    param_nt(23)%r_name(3)="ratemultvt3"
    param_nt(23)%r_name(4)="ratemultvt4"
    param_nt(23)%r_name(5)="ratemultvt5"
    param_nt(23)%r_name(6)="threshmultvt1"
    param_nt(23)%r_name(7)="threshmultvt2"
    param_nt(23)%r_name(8)="threshmultvt3"
    param_nt(23)%r_name(9)="threshmultvt4"
    param_nt(23)%r_name(10)="threshmultvt5"
    param_nt(24)%ogp="P"
    param_nt(24)%id="37"
    allocate( param_nt(24)%i_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(24)%r_name(4), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(24)%s_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    param_nt(24)%r_name(1)="thinvalp"
    param_nt(24)%r_name(2)="tyldrmp"
    param_nt(24)%r_name(3)="tplrmp"
    param_nt(24)%r_name(4)="tstrmp"
    param_nt(25)%ogp="P"
    param_nt(25)%id="38"
    allocate( param_nt(25)%i_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(25)%r_name(4), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(25)%s_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    param_nt(25)%r_name(1)="thinvalf"
    param_nt(25)%r_name(2)="tyldrmf"
    param_nt(25)%r_name(3)="tplrmf"
    param_nt(25)%r_name(4)="tstrmf"
    param_nt(26)%ogp="P"
    param_nt(26)%id="40"
    allocate( param_nt(26)%i_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(26)%r_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(26)%s_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    param_nt(27)%ogp="P"
    param_nt(27)%id="42"
    allocate( param_nt(27)%i_name(5), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(27)%r_name(4), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(27)%s_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    param_nt(27)%i_name(1)="harv_report_flg"
    param_nt(27)%i_name(2)="harv_calib_flg"
    param_nt(27)%i_name(3)="harv_unit_flg"
    param_nt(27)%i_name(4)="mature_warn_flg"
    param_nt(27)%i_name(5)="cutflag"
    param_nt(27)%r_name(1)="cutvalh"
    param_nt(27)%r_name(2)="cyldrmh"
    param_nt(27)%r_name(3)="cplrmh"
    param_nt(27)%r_name(4)="cstrmh"
    param_nt(28)%ogp="P"
    param_nt(28)%id="43"
    allocate( param_nt(28)%i_name(4), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(28)%r_name(4), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(28)%s_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    param_nt(28)%i_name(1)="harv_report_flg"
    param_nt(28)%i_name(2)="harv_calib_flg"
    param_nt(28)%i_name(3)="harv_unit_flg"
    param_nt(28)%i_name(4)="mature_warn_flg"
    param_nt(28)%r_name(1)="cutvalf"
    param_nt(28)%r_name(2)="cyldrmf"
    param_nt(28)%r_name(3)="cplrmf"
    param_nt(28)%r_name(4)="cstrmf"
    param_nt(29)%ogp="P"
    param_nt(29)%id="47"
    allocate( param_nt(29)%i_name(4), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(29)%r_name(4), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(29)%s_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    param_nt(29)%i_name(1)="harv_report_flg"
    param_nt(29)%i_name(2)="harv_calib_flg"
    param_nt(29)%i_name(3)="harv_unit_flg"
    param_nt(29)%i_name(4)="mature_warn_flg"
    param_nt(29)%r_name(1)="thinvalp"
    param_nt(29)%r_name(2)="tyldrmp"
    param_nt(29)%r_name(3)="tplrmp"
    param_nt(29)%r_name(4)="tstrmp"
    param_nt(30)%ogp="P"
    param_nt(30)%id="48"
    allocate( param_nt(30)%i_name(4), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(30)%r_name(4), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(30)%s_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    param_nt(30)%i_name(1)="harv_report_flg"
    param_nt(30)%i_name(2)="harv_calib_flg"
    param_nt(30)%i_name(3)="harv_unit_flg"
    param_nt(30)%i_name(4)="mature_warn_flg"
    param_nt(30)%r_name(1)="thinvalf"
    param_nt(30)%r_name(2)="tyldrmf"
    param_nt(30)%r_name(3)="tplrmf"
    param_nt(30)%r_name(4)="tstrmf"
    param_nt(31)%ogp="P"
    param_nt(31)%id="50"
    allocate( param_nt(31)%i_name(1), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(31)%r_name(18), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(31)%s_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    param_nt(31)%r_name(1)="numst"
    param_nt(31)%r_name(2)="rstandht"
    param_nt(31)%r_name(3)="rstandmass"
    param_nt(31)%r_name(4)="rflatmass"
    param_nt(31)%i_name(1)="rbc"
    param_nt(31)%r_name(5)="rburiedmass"
    param_nt(31)%r_name(6)="rburieddepth"
    param_nt(31)%r_name(7)="rrootmass"
    param_nt(31)%r_name(8)="rrootdepth"
    param_nt(31)%r_name(9)="standdk"
    param_nt(31)%r_name(10)="surfdk"
    param_nt(31)%r_name(11)="burieddk"
    param_nt(31)%r_name(12)="rootdk"
    param_nt(31)%r_name(13)="stemnodk"
    param_nt(31)%r_name(14)="stemdia"
    param_nt(31)%r_name(15)="thrddys"
    param_nt(31)%r_name(16)="covfact"
    param_nt(31)%r_name(17)="resevapa"
    param_nt(31)%r_name(18)="resevapb"
    param_nt(32)%ogp="P"
    param_nt(32)%id="51"
    allocate( param_nt(32)%i_name(9), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(32)%r_name(61), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(32)%s_name(1), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    param_nt(32)%i_name(1)="rowflag"
    param_nt(32)%r_name(1)="rowspac"
    param_nt(32)%i_name(2)="rowridge"
    param_nt(32)%r_name(2)="plantpop"
    param_nt(32)%r_name(3)="dmaxshoot"
    param_nt(32)%i_name(3)="cbaflag"
    param_nt(32)%r_name(4)="tgtyield"
    param_nt(32)%r_name(5)="cbafact"
    param_nt(32)%r_name(6)="cyrafact"
    param_nt(32)%i_name(4)="hyldflag"
    param_nt(32)%s_name(1)="hyldunits"
    param_nt(32)%r_name(7)="hyldwater"
    param_nt(32)%r_name(8)="hyconfact"
    param_nt(32)%i_name(5)="idc"
    param_nt(32)%r_name(9)="grf"
    param_nt(32)%r_name(10)="ck"
    param_nt(32)%r_name(11)="hui0"
    param_nt(32)%r_name(12)="hmx"
    param_nt(32)%r_name(13)="growdepth"
    param_nt(32)%r_name(14)="rdmx"
    param_nt(32)%r_name(15)="tbas"
    param_nt(32)%r_name(16)="topt"
    param_nt(32)%i_name(6)="thudf"
    param_nt(32)%i_name(7)="dtm"
    param_nt(32)%r_name(17)="thum"
    param_nt(32)%r_name(18)="frsx1"
    param_nt(32)%r_name(19)="frsx2"
    param_nt(32)%r_name(20)="frsy1"
    param_nt(32)%r_name(21)="frsy2"
    param_nt(32)%r_name(22)="verndel"
    param_nt(32)%r_name(23)="bceff"
    param_nt(32)%r_name(24)="a_lf"
    param_nt(32)%r_name(25)="b_lf"
    param_nt(32)%r_name(26)="c_lf"
    param_nt(32)%r_name(27)="d_lf"
    param_nt(32)%r_name(28)="a_rp"
    param_nt(32)%r_name(29)="b_rp"
    param_nt(32)%r_name(30)="c_rp"
    param_nt(32)%r_name(31)="d_rp"
    param_nt(32)%r_name(32)="a_ht"
    param_nt(32)%r_name(33)="b_ht"
    param_nt(32)%r_name(34)="ssaa"
    param_nt(32)%r_name(35)="ssab"
    param_nt(32)%r_name(36)="sla"
    param_nt(32)%r_name(37)="huie"
    param_nt(32)%i_name(8)="transf"
    param_nt(32)%r_name(38)="diammax"
    param_nt(32)%r_name(39)="storeinit"
    param_nt(32)%r_name(40)="mshoot"
    param_nt(32)%r_name(41)="leafstem"
    param_nt(32)%r_name(42)="fshoot"
    param_nt(32)%r_name(43)="leaf2stor"
    param_nt(32)%r_name(44)="stem2stor"
    param_nt(32)%r_name(45)="stor2stor"
    param_nt(32)%i_name(9)="rbc"
    param_nt(32)%r_name(46)="standdk"
    param_nt(32)%r_name(47)="surfdk"
    param_nt(32)%r_name(48)="burieddk"
    param_nt(32)%r_name(49)="rootdk"
    param_nt(32)%r_name(50)="stemnodk"
    param_nt(32)%r_name(51)="stemdia"
    param_nt(32)%r_name(52)="thrddys"
    param_nt(32)%r_name(53)="covfact"
    param_nt(32)%r_name(54)="resevapa"
    param_nt(32)%r_name(55)="resevapb"
    param_nt(32)%r_name(56)="yield_coefficient"
    param_nt(32)%r_name(57)="residue_intercept"
    param_nt(32)%r_name(58)="regrow_location"
    param_nt(32)%r_name(59)="noparam3"
    param_nt(32)%r_name(60)="noparam2"
    param_nt(32)%r_name(61)="noparam1"
    param_nt(33)%ogp="P"
    param_nt(33)%id="61"
    allocate( param_nt(33)%i_name(2), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(33)%r_name(5), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(33)%s_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    param_nt(33)%i_name(1)="selpos"
    param_nt(33)%i_name(2)="selpool"
    param_nt(33)%r_name(1)="rstore"
    param_nt(33)%r_name(2)="rleaf"
    param_nt(33)%r_name(3)="rstem"
    param_nt(33)%r_name(4)="rrootstore"
    param_nt(33)%r_name(5)="rrootfiber"
    param_nt(34)%ogp="P"
    param_nt(34)%id="62"
    allocate( param_nt(34)%i_name(7), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(34)%r_name(5), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(34)%s_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    param_nt(34)%i_name(1)="harv_report_flg"
    param_nt(34)%i_name(2)="harv_calib_flg"
    param_nt(34)%i_name(3)="harv_unit_flg"
    param_nt(34)%i_name(4)="mature_warn_flg"
    param_nt(34)%i_name(5)="selpos"
    param_nt(34)%i_name(6)="selpool"
    param_nt(34)%i_name(7)="selagepool"
    param_nt(34)%r_name(1)="rstore"
    param_nt(34)%r_name(2)="rleaf"
    param_nt(34)%r_name(3)="rstem"
    param_nt(34)%r_name(4)="rrootstore"
    param_nt(34)%r_name(5)="rrootfiber"
    param_nt(35)%ogp="P"
    param_nt(35)%id="65"
    allocate( param_nt(35)%i_name(1), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(35)%r_name(18), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(35)%s_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    param_nt(35)%r_name(1)="numst"
    param_nt(35)%r_name(2)="rstandht"
    param_nt(35)%r_name(3)="rstandmass"
    param_nt(35)%r_name(4)="rflatmass"
    param_nt(35)%i_name(1)="rbc"
    param_nt(35)%r_name(5)="rburiedmass"
    param_nt(35)%r_name(6)="rburieddepth"
    param_nt(35)%r_name(7)="rrootmass"
    param_nt(35)%r_name(8)="rrootdepth"
    param_nt(35)%r_name(9)="standdk"
    param_nt(35)%r_name(10)="surfdk"
    param_nt(35)%r_name(11)="burieddk"
    param_nt(35)%r_name(12)="rootdk"
    param_nt(35)%r_name(13)="stemnodk"
    param_nt(35)%r_name(14)="stemdia"
    param_nt(35)%r_name(15)="thrddys"
    param_nt(35)%r_name(16)="covfact"
    param_nt(35)%r_name(17)="resevapa"
    param_nt(35)%r_name(18)="resevapb"
    param_nt(36)%ogp="P"
    param_nt(36)%id="66"
    allocate( param_nt(36)%i_name(1), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(36)%r_name(20), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(36)%s_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    param_nt(36)%r_name(1)="M_numst"
    param_nt(36)%r_name(2)="M_rstandht"
    param_nt(36)%r_name(3)="M_rstandmass"
    param_nt(36)%r_name(4)="M_rflatmass"
    param_nt(36)%i_name(1)="rbc"
    param_nt(36)%r_name(5)="M_rburiedmass"
    param_nt(36)%r_name(6)="M_rburieddepth"
    param_nt(36)%r_name(7)="M_rrootmass"
    param_nt(36)%r_name(8)="M_rrootdepth"
    param_nt(36)%r_name(9)="manure_total_mass"
    param_nt(36)%r_name(10)="manure_buried_ratio"
    param_nt(36)%r_name(11)="standdk"
    param_nt(36)%r_name(12)="surfdk"
    param_nt(36)%r_name(13)="burieddk"
    param_nt(36)%r_name(14)="rootdk"
    param_nt(36)%r_name(15)="stemnodk"
    param_nt(36)%r_name(16)="stemdia"
    param_nt(36)%r_name(17)="thrddys"
    param_nt(36)%r_name(18)="covfact"
    param_nt(36)%r_name(19)="resevapa"
    param_nt(36)%r_name(20)="resevapb"
    param_nt(37)%ogp="P"
    param_nt(37)%id="71"
    allocate( param_nt(37)%i_name(1), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(37)%r_name(1), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(37)%s_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    param_nt(37)%i_name(1)="irrtype"
    param_nt(37)%r_name(1)="irrdepth"
    param_nt(38)%ogp="P"
    param_nt(38)%id="72"
    allocate( param_nt(38)%i_name(2), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(38)%r_name(6), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(38)%s_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    param_nt(38)%i_name(1)="irrmonflag"
    param_nt(38)%i_name(2)="irrminint"
    param_nt(38)%r_name(1)="irrmaxapp"
    param_nt(38)%r_name(2)="irrrate"
    param_nt(38)%r_name(3)="irrduration"
    param_nt(38)%r_name(4)="irrapploc"
    param_nt(38)%r_name(5)="irrminapp"
    param_nt(38)%r_name(6)="irrmad"
    param_nt(39)%ogp="P"
    param_nt(39)%id="73"
    allocate( param_nt(39)%i_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(39)%r_name(4), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(39)%s_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    param_nt(39)%r_name(1)="irrdepth"
    param_nt(39)%r_name(2)="irrrate"
    param_nt(39)%r_name(3)="irrduration"
    param_nt(39)%r_name(4)="irrapploc"
    param_nt(40)%ogp="P"
    param_nt(40)%id="74"
    allocate( param_nt(40)%i_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(40)%r_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(40)%s_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    param_nt(41)%ogp="P"
    param_nt(41)%id="91"
    allocate( param_nt(41)%i_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(41)%r_name(5), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(41)%s_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    param_nt(41)%r_name(1)="asddepth"
    param_nt(41)%r_name(2)="gmdx"
    param_nt(41)%r_name(3)="gsdx"
    param_nt(41)%r_name(4)="mnot"
    param_nt(41)%r_name(5)="minf"
    param_nt(42)%ogp="P"
    param_nt(42)%id="92"
    allocate( param_nt(42)%i_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(42)%r_name(2), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    allocate( param_nt(42)%s_name(0), stat=alloc_stat )
    sum_stat = sum_stat + alloc_stat
    param_nt(42)%r_name(1)="wcdepth"
    param_nt(42)%r_name(2)="wc"
    if( alloc_stat .gt. 0 ) then
      write(*,*) 'ERROR: memory alloc., parameter names reference'
    end if

    all_wepsmanvalues = .true.  ! .true. indicates that no values are required
    all_operationDBs = .true.  ! .false. indicates that a value is required
    all_actionvalues = .true.
    all_params = .true.

  end subroutine init_man_xml

  function oper_check_params( operPtr ) result(acquired)
    use manage_data_struct_defs, only: operation
    type(operation), intent(in) :: operPtr
    logical :: acquired

    integer :: idx
    character(len=10) :: date_str

    ! set as default in case of no parameter action
    acquired = .true.

    do idx = 1, size(operPtr%i_param)
      if ( operPtr%i_param(idx)%p_acquired ) then
        acquired = acquired .and. .true.
      else
        acquired = acquired .and. .false.
        write(date_str, '(2(i2,"/"),i4)') operPtr%operDate%day, operPtr%operDate%month, operPtr%operDate%year
        write(*,*) 'Missing value for: ', trim(param_nt(operPtr%OGPidx)%i_name(idx)), ' Parameter in: O ', trim(operPtr%operID), &
                   ' of Operation: ', trim(operPtr%operName), ' on date: ', date_str
      end if
    end do
    do idx = 1, size(operPtr%r_param)
      if ( operPtr%r_param(idx)%p_acquired ) then
        acquired = acquired .and. .true.
      else
        acquired = acquired .and. .false.
        write(date_str, '(2(i2,"/"),i4)') operPtr%operDate%day, operPtr%operDate%month, operPtr%operDate%year
        write(*,*) 'Missing value for: ', trim(param_nt(operPtr%OGPidx)%r_name(idx)), ' Parameter in: O ', trim(operPtr%operID), &
                   ' of Operation: ', trim(operPtr%operName), ' on date: ', date_str
      end if
    end do
    do idx = 1, size(operPtr%s_param)
      if ( operPtr%s_param(idx)%p_acquired ) then
        acquired = acquired .and. .true.
      else
        acquired = acquired .and. .false.
        write(date_str, '(2(i2,"/"),i4)') operPtr%operDate%day, operPtr%operDate%month, operPtr%operDate%year
        write(*,*) 'Missing value for: ', trim(param_nt(operPtr%OGPidx)%s_name(idx)), ' Parameter in: O ', trim(operPtr%operID), &
                   ' of Operation: ', trim(operPtr%operName), ' on date: ', date_str
      end if
    end do
  end function oper_check_params

  function grp_check_params( operPtr, grpPtr ) result(acquired)
    use manage_data_struct_defs, only: operation, group
    type(operation), intent(in) :: operPtr
    type(group), intent(in) :: grpPtr
    logical :: acquired

    integer :: idx
    character(len=10) :: date_str

    ! set as default in case of no parameter action
    acquired = .true.

    do idx = 1, size(grpPtr%i_param)
      if ( grpPtr%i_param(idx)%p_acquired ) then
        acquired = acquired .and. .true.
      else
        acquired = acquired .and. .false.
        write(date_str, '(2(i2,"/"),i4)') operPtr%operDate%day, operPtr%operDate%month, operPtr%operDate%year
        write(*,*) 'Missing value for: ', trim(param_nt(grpPtr%OGPidx)%i_name(idx)), ' Parameter in: G ', trim(grpPtr%grpID), &
                   ' of Operation: ', trim(operPtr%operName), ' on date: ', date_str
      end if
    end do
    do idx = 1, size(grpPtr%r_param)
      if ( grpPtr%r_param(idx)%p_acquired ) then
        acquired = acquired .and. .true.
      else
        acquired = acquired .and. .false.
        write(date_str, '(2(i2,"/"),i4)') operPtr%operDate%day, operPtr%operDate%month, operPtr%operDate%year
        write(*,*) 'Missing value for: ', trim(param_nt(grpPtr%OGPidx)%r_name(idx)), ' Parameter in: G ', trim(grpPtr%grpID), &
                   ' of Operation: ', trim(operPtr%operName), ' on date: ', date_str
      end if
    end do
    do idx = 1, size(grpPtr%s_param)
      if ( grpPtr%s_param(idx)%p_acquired ) then
        acquired = acquired .and. .true.
      else
        acquired = acquired .and. .false.
        write(date_str, '(2(i2,"/"),i4)') operPtr%operDate%day, operPtr%operDate%month, operPtr%operDate%year
        write(*,*) 'Missing value for: ', trim(param_nt(grpPtr%OGPidx)%s_name(idx)), ' Parameter in: G ', trim(grpPtr%grpID), &
                   ' of Operation: ', trim(operPtr%operName), ' on date: ', date_str
      end if
    end do
  end function grp_check_params

  function proc_check_params( operPtr, procPtr ) result(acquired)
    use manage_data_struct_defs, only: operation, process
    type(operation), intent(in) :: operPtr
    type(process), intent(in) :: procPtr
    logical :: acquired

    integer :: idx
    character(len=10) :: date_str

    ! set as default in case of no parameter action
    acquired = .true.

    do idx = 1, size(procPtr%i_param)
      if ( procPtr%i_param(idx)%p_acquired ) then
        acquired = .true.
      else
        acquired = .false.
        write(date_str, '(2(i2,"/"),i4)') operPtr%operDate%day, operPtr%operDate%month, operPtr%operDate%year
        write(*,*) 'Missing value for: ', trim(param_nt(procPtr%OGPidx)%i_name(idx)), ' Parameter in: P ', trim(procPtr%procID), &
                   ' of Operation: ', trim(operPtr%operName), ' on date: ', date_str
      end if
    end do
    do idx = 1, size(procPtr%r_param)
      if ( procPtr%r_param(idx)%p_acquired ) then
        acquired = .true.
      else
        acquired = .false.
        write(date_str, '(2(i2,"/"),i4)') operPtr%operDate%day, operPtr%operDate%month, operPtr%operDate%year
        write(*,*) 'Missing value for: ', trim(param_nt(procPtr%OGPidx)%r_name(idx)), ' Parameter in: P ', trim(procPtr%procID), &
                   ' of Operation: ', trim(operPtr%operName), ' on date: ', date_str
      end if
    end do
    do idx = 1, size(procPtr%s_param)
      if ( procPtr%s_param(idx)%p_acquired ) then
        acquired = .true.
      else
        acquired = .false.
        write(date_str, '(2(i2,"/"),i4)') operPtr%operDate%day, operPtr%operDate%month, operPtr%operDate%year
        write(*,*) 'Missing value for: ', trim(param_nt(procPtr%OGPidx)%s_name(idx)), ' Parameter in: P ', trim(procPtr%procID), &
                   ' of Operation: ', trim(operPtr%operName), ' on date: ', date_str
      end if
    end do
  end function proc_check_params

!  subroutine read_manage_xml()

!    operType = 0
!    operFirst => elemCreate( operFirst, operType, real_cnt )
!    oper => operFirst

!    grpType = 0
!    oper%grpFirst => elemCreate( oper%grpFirst, grpType, real_cnt )
!    grp => oper%grpFirst
!    procType = 0
!    grp%procFirst => elemCreate( grp%procFirst, procType, int_cnt, real_cnt )
!    proc => grp%procFirst
!    do procType = 1, 5
!      proc%procNext => elemCreate( proc%procNext, procType, int_cnt, real_cnt )
!      proc => proc%procNext
!    end do
!    nullify( proc%procNext )
!    do grpType = 1, 5
!      grp => elemCreate( grp%grpNext, grpType )
!      procType = 0
!      grp%procFirst => elemCreate( grp%procFirst, procType )
!      proc => grp%procFirst
!      do procType = 1, 5
!        proc%procNext => elemCreate( proc%procNext, procType )
!        proc => proc%procNext
!      end do
!      nullify( proc%procNext )
!    end do
!    nullify( grp%grpNext )

!    do operType = 1, 5
!      oper => elemCreate( oper%operNext, operType )
!      grpType = 0
!      oper%grpFirst => elemCreate( oper%grpFirst, grpType )
!      grp => oper%grpFirst
!      procType = 0
!      grp%procFirst => elemCreate( grp%procFirst, procType )
!      proc => grp%procFirst
!      do procType = 1, 5
!        proc%procNext => elemCreate( proc%procNext, procType )
!        proc => proc%procNext
!      end do
!      nullify( proc%procNext )
!      do grpType = 1, 5
!        grp => elemCreate( grp%grpNext, grpType )
!        procType = 0
!        grp%procFirst => elemCreate( grp%procFirst, procType )
!        proc => grp%procFirst
!        do procType = 1, 5
!          proc%procNext => elemCreate( proc%procNext, procType )
!          proc => proc%procNext
!        end do
!        nullify( proc%procNext )
!      end do
!      nullify( grp%grpNext )
!    end do

!    oper%operNext => operFirst

!    oper => operFirst
!    operType = 0
!    do while( operType .lt. 12 )
!      write(*,'(a,i0)') 'OPER: ', oper%operType
!      grp => oper%grpFirst
!      do while( associated(grp) )
!        write(*,'(a,i0)') '  GRP: ', grp%grpType
!        proc => grp%procFirst
!        do while( associated(proc) )
!          write(*,'(a,i0)') '    PROC: ', proc%procType
!          proc => proc%procNext
!        end do
!        grp => grp%grpNext
!      end do
!      oper => oper%operNext
!      operType = operType + 1
!    end do

!  end subroutine read_manage_xml

  subroutine begin_man_element_handler(name,attributes)
    character(len=*), intent(in) :: name
    type(dictionary_t), intent(in) :: attributes

    integer :: idx

    do idx = 1, size(man_tag)
      if( man_tag(idx)%name .eq. name ) then
        man_tag(idx)%in_tag = .true.
        !write(*,*) 'In tag ', trim(name)
        exit  ! found tag, no need to look further
      end if
    end do

  end subroutine begin_man_element_handler

  subroutine end_man_element_handler(name)
    character(len=*), intent(in) :: name

    integer :: idx

    do idx = 1, size(man_tag)
      if( man_tag(idx)%name .eq. name ) then
        man_tag(idx)%in_tag = .false.
        !write(*,*) 'Out tag ', trim(name)
        exit  ! found tag, no need to look further
      end if
    end do

    if (idx .eq. wepsmanDB) then

      if ( man_tag(rotationyears)%acquired &
        .and. man_tag(version)%acquired &
        .and. all_wepsmanvalues ) then
        manfile_complete = .true.
      else
        manfile_complete = .false.
      end if

    else if (idx .eq. wepsmanvalue) then
      if( man_tag(date)%acquired &
        .and. all_operationDBs ) then
        man_tag(date)%acquired = .false.
        ! stays .true. if all previous values have been true
        all_wepsmanvalues = (all_wepsmanvalues .and. .true. )
      else
        all_wepsmanvalues = .false.
        if ( .not. man_tag(date)%acquired ) then
          write(*,'(3a)') 'Tag ', trim(man_tag(date)%name), ' is missing from Management file.'
        end if
      end if

    else if (idx .eq. date) then

    else if (idx .eq. operationDB) then
      if( man_tag(operationname)%acquired &
        .and. all_actionvalues ) then
        man_tag(operationname)%acquired = .false.
        ! stays .true. if all previous values have been true
        all_operationDBs = (all_operationDBs .and. .true. )
      else
        all_operationDBs = .false.
        if ( .not. man_tag(operationname)%acquired ) then
          write(*,'(3a)') 'Tag ', trim(man_tag(operationname)%name), ' is missing from Management file.'
        end if
      end if

    else if (idx .eq. actionvalue) then
      ! check if all parameters acquired for this action value
      if ( operID .ne. '' ) then
        all_params = check_params( manFile(isub)%oper )
      else if ( grpID .ne. '' ) then
        all_params = check_params( manFile(isub)%oper, manFile(isub)%grp )
      else if ( procID .ne. '' ) then
        all_params = check_params( manFile(isub)%oper, manFile(isub)%proc )
      end if
      if( man_tag(identity)%acquired &
        .and. all_params &
        ) then
        man_tag(identity)%acquired = .false.
        man_tag(actionvalue)%acquired = .true.
        ! stays .true. if all previous values have been true
        all_actionvalues = (all_actionvalues .and. .true. )
      else
        all_actionvalues = .false.
        if ( .not. man_tag(identity)%acquired ) then
          write(*,'(3a)') 'Tag ', trim(man_tag(identity)%name), ' is missing from Management file.'
        end if
      end if
      ! write(*,*) 'ALLACTIONVALUES', all_actionvalues

    else if (idx .eq. identity) then
      if( man_tag(code)%acquired &
        .and. man_tag(id)%acquired &
        ) then
        man_tag(code)%acquired = .false.
        man_tag(id)%acquired = .false.
        man_tag(identity)%acquired = .true.
      else
        if ( .not. man_tag(code)%acquired ) then
          write(*,'(3a)') 'Tag ', trim(man_tag(code)%name), ' is missing from Management file.'
        else if ( .not. man_tag(id)%acquired ) then
          write(*,'(3a)') 'Tag ', trim(man_tag(id)%name), ' is missing from Management file.'
        end if
      end if

    else if (idx .eq. param) then
      if( man_tag(p_name)%acquired &
        .and. man_tag(value)%acquired &
        ) then
        man_tag(p_name)%acquired = .false.
        man_tag(value)%acquired = .false.
        man_tag(param)%acquired = .true.
      end if

    end if

    !if ( idx .le. size(man_tag) ) then
    !  if ( man_tag(idx)%acquired ) then
    !    write(*,*) 'ACQUIRED: ', man_tag(idx)%name!, man_tag(idx)%acquired
    !  end if
    !end if

  end subroutine end_man_element_handler

  subroutine pcdata_man_chunk_handler(chunk)
    use read_write_xml_mod, only: read_param
    character(len=*), intent(in) :: chunk

    character(len=80) :: param_value

    param_value = trim(chunk)

    !write(*,*) 'CHUNK: ', trim(chunk)

    if (man_tag(wepsmanDB)%in_tag) then
      if (man_tag(version)%in_tag) then
        call read_param(man_tag(version)%name, param_value, manFile(isub)%mversion)
        man_tag(version)%acquired = .true.
      else if (man_tag(rotationyears)%in_tag) then
        call read_param(man_tag(rotationyears)%name, param_value, manFile(isub)%mperod)
        man_tag(rotationyears)%acquired = .true.
      else if (man_tag(wepsmanvalue)%in_tag) then
        if (man_tag(date)%in_tag) then
          call read_param(man_tag(date)%name, param_value, t_operDate)
          man_tag(date)%acquired = .true.
        else if (man_tag(operationDB)%in_tag ) then
          if (man_tag(operationname)%in_tag ) then
            t_operName = trim(param_value)
            man_tag(operationname)%acquired = .true.
          else if (man_tag(actionvalue)%in_tag ) then
            if (man_tag(identity)%in_tag ) then
              if (man_tag(code)%in_tag ) then
                t_code = trim(param_value)
                if (   t_code .eq. 'O' &
                  .or. t_code .eq. 'G' &
                  .or. t_code .eq. 'P' &
                  ) then
                  man_tag(code)%acquired = .true.
                  operID = ''
                  grpID = ''
                  procID = ''
                else
                  write(*,*) 'Unknown Identity code: "', trim(t_code), '" found in ', trim(manFile(isub)%tinfil)
                  call exit(1)
                end if
              else if (man_tag(id)%in_tag ) then
                man_tag(id)%acquired = .true.
                select case (t_code)
                case ('O')
                  operID = trim(param_value)
                  if ( .not. associated(manFile(isub)%operFirst) ) then
                    manFile(isub)%operFirst => elemCreate( manFile(isub)%operFirst, operID )
                    manFile(isub)%oper => manFile(isub)%operFirst
                  else
                    manFile(isub)%oper%operNext => elemCreate( manFile(isub)%oper%operNext, operID )
                    manFile(isub)%oper => manFile(isub)%oper%operNext
                  end if
                  manFile(isub)%oper%operDate = t_operDate
                  manFile(isub)%oper%operName = trim(t_operName)
                  ! new operation, so nullify current group and process
                  nullify(manFile(isub)%grp)
                  nullify(manFile(isub)%proc)
                  ogp_id_idx = manFile(isub)%oper%OGPidx
                case ('G')
                  grpID = trim(param_value)
                  if ( .not. associated(manFile(isub)%oper) ) then
                    write(*,*) 'Group appears before Operation in Management File: ', trim(manFile(isub)%tinfil)
                    call exit(1)
                  else if ( .not. associated(manFile(isub)%oper%grpFirst) ) then
                    manFile(isub)%oper%grpFirst => elemCreate( manFile(isub)%oper%grpFirst, grpID )
                    manFile(isub)%grp => manFile(isub)%oper%grpFirst
                  else
                    manFile(isub)%grp%grpNext => elemCreate( manFile(isub)%grp%grpNext, grpID )
                    manFile(isub)%grp => manFile(isub)%grp%grpNext
                  end if
                  nullify(manFile(isub)%proc)
                  ogp_id_idx = manFile(isub)%grp%OGPidx
                case ('P')
                  procID = trim(param_value)
                  if ( .not. associated(manFile(isub)%grp) ) then
                    ! Operation has process without group preceeding, create null group to support structure.
                    manFile(isub)%oper%grpFirst => elemCreate( manFile(isub)%oper%grpFirst, '00' )
                    manFile(isub)%grp => manFile(isub)%oper%grpFirst
                  end if
                  if ( .not. associated(manFile(isub)%grp%procFirst) ) then
                    manFile(isub)%grp%procFirst => elemCreate( manFile(isub)%grp%procFirst, procID )
                    manFile(isub)%proc => manFile(isub)%grp%procFirst
                  else
                    manFile(isub)%proc%procNext => elemCreate( manFile(isub)%proc%procNext, procID )
                    manFile(isub)%proc => manFile(isub)%proc%procNext
                  end if
                  ogp_id_idx = manFile(isub)%proc%OGPidx
                end select
              end if
            else if (man_tag(param)%in_tag ) then
              if (man_tag(p_name)%in_tag ) then
                ! sets index for placement of value into type array
                call get_value_type_index ( ogp_id_idx, param_value, p_type, p_idx)
                if ( p_idx .gt. 0 ) then
                  man_tag(p_name)%acquired = .true.
                end if
              else if (man_tag(value)%in_tag ) then
                if ( man_tag(p_name)%acquired ) then
                  select case (p_type)
                  case ('int')
                    if ( operID .ne. '' ) then
                      call read_param(man_tag(p_name)%name, param_value, manFile(isub)%oper%i_param(p_idx)%p_value )
                      manFile(isub)%oper%i_param(p_idx)%p_acquired = .true.
                    else if ( grpID .ne. '' ) then
                      call read_param(man_tag(p_name)%name, param_value, manFile(isub)%grp%i_param(p_idx)%p_value )
                      manFile(isub)%grp%i_param(p_idx)%p_acquired = .true.
                    else if ( procID .ne. '' ) then
                      call read_param(man_tag(p_name)%name, param_value, manFile(isub)%proc%i_param(p_idx)%p_value )
                      manFile(isub)%proc%i_param(p_idx)%p_acquired = .true.
                    end if
                  case ('real')
                    if ( operID .ne. '' ) then
                      call read_param(man_tag(p_name)%name, param_value, manFile(isub)%oper%r_param(p_idx)%p_value )
                      manFile(isub)%oper%r_param(p_idx)%p_acquired = .true.
                    else if ( grpID .ne. '' ) then
                      call read_param(man_tag(p_name)%name, param_value, manFile(isub)%grp%r_param(p_idx)%p_value )
                      manFile(isub)%grp%r_param(p_idx)%p_acquired = .true.
                    else if ( procID .ne. '' ) then
                      call read_param(man_tag(p_name)%name, param_value, manFile(isub)%proc%r_param(p_idx)%p_value )
                      manFile(isub)%proc%r_param(p_idx)%p_acquired = .true.
                    end if
                  case ('str')
                    if ( operID .ne. '' ) then
                      manFile(isub)%oper%s_param(p_idx)%p_value = trim(param_value)
                      manFile(isub)%oper%s_param(p_idx)%p_acquired = .true.
                    else if ( grpID .ne. '' ) then
                      manFile(isub)%grp%s_param(p_idx)%p_value = trim(param_value)
                      manFile(isub)%grp%s_param(p_idx)%p_acquired = .true.
                    else if ( procID .ne. '' ) then
                      manFile(isub)%proc%s_param(p_idx)%p_value = trim(param_value)
                      manFile(isub)%proc%s_param(p_idx)%p_acquired = .true.
                    end if
                  end select
                  man_tag(value)%acquired = .true.
                end if
              end if
            end if
          end if
        end if
      end if
    end if

  end subroutine pcdata_man_chunk_handler

  subroutine read_old_manfile ( isubr, luimanfile )
    integer, intent(in) :: isubr      ! current manfile subregion
    integer, intent(in) :: luimanfile             ! management file io unit number

    integer :: linidx
    integer :: eofidx
    integer :: endidx
    character*256 :: line

    type(operation_date) :: t_operDate
    character(len=1) :: t_code
    character(len=3) :: t_id
    character(len=80) :: t_name

    ! set subregion index used with manFile
    isub = isubr

      rewind(luimanfile)
      linidx = 0
      eofidx = 0
      endidx = 0
 10   read(luimanfile, '(a)', end=20) line
      select case (line(1:1))
      case ('V')  ! first line begins with word "Version: "
        linidx = linidx + 1
        if (line (1:8).eq.'Version: ') then
          ! We have found the version # of the management file
          ! Read the version into the common block variable
          read(line (10:13), *) manFile(isub)%mversion

          ! Report the version to stdout
          write (6, *) 'Management file version: ', manFile(isub)%mversion

          ! Test if the version is at least 1.4.  Version 1.5 adds the ability to test
          !       mversion within the operations, groups and procs so that graceful upgrades
          !       are possible.  This test version should not need to be updated as the format
          !       changes.  Upgrades can be handled within the dooper, dogroup and doproc subroutines.
          if (manFile(isub)%mversion .lt. 1.40) then
            write(0,*) 'Management file version: ', manFile(isub)%mversion
            write(0,*) 'Version >= 1.40 is required for this release.'
            write(0,*) 'You need to convert ', trim(manFile(isub)%tinfil)
            write(0,*) ' to the correct format.'
            call exit (1)
          end if
        else
          write(0,*) 'Version not found in management file ', trim(manFile(isub)%tinfil)
          call exit (1)
        endif

      case ('O')
        linidx = linidx + 1
        read(line, '(a1,1x,a2,1x,a)', err=1001) t_code, t_id, t_name
        ! create operation
        if ( .not. associated(manFile(isub)%operFirst) ) then
          manFile(isub)%operFirst => elemCreate( manFile(isub)%operFirst, t_id )
          manFile(isub)%oper => manFile(isub)%operFirst
        else
          manFile(isub)%oper%operNext => elemCreate( manFile(isub)%oper%operNext, t_id )
          manFile(isub)%oper => manFile(isub)%oper%operNext
        end if
        manFile(isub)%oper%operDate = t_operDate
        manFile(isub)%oper%operName = trim(t_name)
        ! new operation, so nullify current group and process
        nullify(manFile(isub)%grp)
        nullify(manFile(isub)%proc)

        ! read following lines as specified in operation type
        select case (manFile(isub)%oper%operID)
        case ('01')  ! original ground engaging operation
          ! get additional line of data
          do while (line(1:1) .ne. '+' )
            read(luimanfile, '(a)', end=20) line
          end do
          linidx = linidx + 1
          ! read tillage speed and direction
          call readValues(manFile(isub)%oper, line, 'ospeed', 'odirect', 'ostdspeed', 'ominspeed', 'omaxspeed')

        case ('03') ! added energy and stir to O1
          ! get additional line of data
          do while (line(1:1) .ne. '+' )
            read(luimanfile, '(a)', end=20) line
          end do
          linidx = linidx + 1
          ! read tillage speed and direction
          call readValues(manFile(isub)%oper, line, 'oenergyarea', 'ostir', &
            'ospeed', 'odirect', 'ostdspeed', 'ominspeed', 'omaxspeed')
          ! Version 1.5 added ofuel
          if (manFile(isub)%mversion .ge. 1.50) then
              ! get fuel line
              read(luimanfile, '(a)', end=20) line
              do while (line(1:1) .ne. '+' )
                 read(luimanfile, '(a)', end=20) line
              end do
              linidx = linidx + 1
              call readValues(manFile(isub)%oper, line, 'ofuel')
          end if
        case ('04') ! added energy and stir to O2
          do while (line(1:1) .ne. '+' )
            read(luimanfile, '(a)', end=20) line
          end do
          linidx = linidx + 1
          ! read tillage speed and direction
          call readValues(manFile(isub)%oper, line, 'oenergyarea', 'ostir')
          ! Version 1.5 added ofuel
          if (manFile(isub)%mversion .ge. 1.50) then
              ! get fuel line
              read(luimanfile, '(a)', end=20) line
              do while (line(1:1) .ne. '+' )
                 read(luimanfile, '(a)', end=20) line
              end do
              linidx = linidx + 1
              call readValues(manFile(isub)%oper, line, 'ofuel')
          end if
        end select

      case ('G')
        linidx = linidx + 1
        read(line, '(a1,1x,a2,1x,a)', err=1002) t_code, t_id, t_name
        ! create group
        if ( .not. associated(manFile(isub)%oper%grpFirst) ) then
          manFile(isub)%oper%grpFirst => elemCreate( manFile(isub)%oper%grpFirst, t_id )
          manFile(isub)%grp => manFile(isub)%oper%grpFirst
        else
          manFile(isub)%grp%grpNext => elemCreate( manFile(isub)%grp%grpNext, t_id )
          manFile(isub)%grp => manFile(isub)%grp%grpNext
        end if
        manFile(isub)%grp%grpName = trim(t_name)
        ! new group, so nullify current process
        nullify(manFile(isub)%proc)

        select case (manFile(isub)%grp%grpID)
        case ('01')
          do while (line(1:1) .ne. '+' )
            read(luimanfile, '(a)', end=20) line
          end do
          linidx = linidx + 1
          ! read tillage depth, intensity and area
          call readValues(manFile(isub)%grp, line, 'gtdepth', 'gtilint', 'gtilArea', 'gtstddepth', 'gtmindepth', 'gtmaxdepth')

        case ('02')
          do while (line(1:1) .ne. '+' )
            read(luimanfile, '(a)', end=20) line
          end do
          linidx = linidx + 1
          ! read biomass area affected
          call readValues(manFile(isub)%grp, line, 'gbioarea')

        case ('03')
          do while (line(1:1) .ne. '+' )
            read(luimanfile, '(a)', end=20) line
          end do
          linidx = linidx + 1

          ! read crop name
          call readValues(manFile(isub)%grp, line, 'gcropname')

        case ('04')
          do while (line(1:1) .ne. '+' )
            read(luimanfile, '(a)', end=20) line
          end do
          linidx = linidx + 1
          ! read amendment name
          call readValues(manFile(isub)%grp, line, 'gamdname')
        end select

      case ('P')
        read(line, '(a1,1x,a2,1x,a)', err=1003) t_code, t_id, t_name
        if ( .not. associated(manFile(isub)%grp) ) then
          ! Operation has process without group preceeding, create null group to support structure.
          manFile(isub)%oper%grpFirst => elemCreate( manFile(isub)%oper%grpFirst, '00' )
          manFile(isub)%grp => manFile(isub)%oper%grpFirst
        end if
        ! create process
        if ( .not. associated(manFile(isub)%grp%procFirst) ) then
          manFile(isub)%grp%procFirst => elemCreate( manFile(isub)%grp%procFirst, t_id )
          manFile(isub)%proc => manFile(isub)%grp%procFirst
        else
          manFile(isub)%proc%procNext => elemCreate( manFile(isub)%proc%procNext, t_id )
          manFile(isub)%proc => manFile(isub)%proc%procNext
        end if
        manFile(isub)%proc%procName = trim(t_name)

        select case (manFile(isub)%proc%procID)
        case ('02')
          do while (line(1:1) .ne. '+' )
            read(luimanfile, '(a)', end=20) line
          end do
          linidx = linidx + 1
          ! read the random roughness for the implement
          call readValues(manFile(isub)%proc, line, 'rroughflag', 'rrough')

        case ('05')
          do while (line(1:1) .ne. '+' )
            read(luimanfile, '(a)', end=20) line
          end do
          linidx = linidx + 1
          ! read the oriented roughness parameters for the implement
          call readValues(manFile(isub)%proc, line, 'rdgflag', 'rdghit', 'rdgspac', 'rdgwidth', 'dkhit', 'dkspac')

        case ('11')
          do while (line(1:1) .ne. '+' )
            read(luimanfile, '(a)', end=20) line
          end do
          linidx = linidx + 1
          ! read the crushing parameters for the implement
          call readValues(manFile(isub)%proc, line, 'asdf', 'crif') ! alpha, beta

        case ('12')
          do while (line(1:1) .ne. '+' )
            read(luimanfile, '(a)', end=20) line
          end do
          linidx = linidx + 1
          ! read the loosening parameter for the implement
          call readValues(manFile(isub)%proc, line, 'soilos') ! mu

        case ('13')
          do while (line(1:1) .ne. '+' )
            read(luimanfile, '(a)', end=20) line
          end do
          linidx = linidx + 1
          ! read the mixing coefficient from the data file
          call readValues(manFile(isub)%proc, line, 'laymix') ! rho

        case ('21')
          do while (line(1:1) .ne. '+' )
            read(luimanfile, '(a)', end=20) line
          end do
          linidx = linidx + 1
          ! read the compaction parameter for the implement
          call readValues(manFile(isub)%proc, line, 'mu', 'compact_load')

        case ('24')
          do while (line(1:1) .ne. '+' )
            read(luimanfile, '(a)', end=20) line
          end do
          linidx = linidx + 1
          call readValues(manFile(isub)%proc, line, 'fbioflagvt', &
            'massflatvt1', 'massflatvt2', 'massflatvt3', 'massflatvt4', 'massflatvt5')

        case ('25')
          do while (line(1:1) .ne. '+' )
            read(luimanfile, '(a)', end=20) line
          end do
          linidx = linidx + 1
          call readValues(manFile(isub)%proc, line, 'burydist', &
            'massburyvt1', 'massburyvt2', 'massburyvt3', 'massburyvt4', 'massburyvt5')

        case ('26')
          do while (line(1:1) .ne. '+' )
            read(luimanfile, '(a)', end=20) line
          end do
          linidx = linidx + 1
          call readValues(manFile(isub)%proc, line, 'massresurvt1', 'massresurvt2', 'massresurvt3', 'massresurvt4', 'massresurvt5')

        case ('31')
          do while (line(1:1) .ne. '+' )
            read(luimanfile, '(a)', end=20) line
          end do
          linidx = linidx + 1
          call readValues(manFile(isub)%proc, line, 'kilflag') ! am0kilfl

        case ('32')
          do while (line(1:1) .ne. '+' )
            read(luimanfile, '(a)', end=20) line
          end do
          linidx = linidx + 1
          call readValues(manFile(isub)%proc, line, 'cutflag', 'cutvalh', 'cyldrmh', 'cplrmh', 'cstrmh')

        case ('33')
          do while (line(1:1) .ne. '+' )
            read(luimanfile, '(a)', end=20) line
          end do
          linidx = linidx + 1
          call readValues(manFile(isub)%proc, line, 'cutvalf', 'cyldrmf', 'cplrmf', 'cstrmf')

        case ('34')
          do while (line(1:1) .ne. '+' )
            read(luimanfile, '(a)', end=20) line
          end do
          linidx = linidx + 1
          call readValues(manFile(isub)%proc, line, 'frselpool', &
            'ratemultvt1', 'ratemultvt2', 'ratemultvt3', 'ratemultvt4', 'ratemultvt5')

        ! get additional line of data
          read(luimanfile, '(a)', end=20) line
          do while (line(1:1) .ne. '+' )
            read(luimanfile, '(a)', end=20) line
          end do
          linidx = linidx + 1
          call readValues(manFile(isub)%proc, line, &
            'threshmultvt1', 'threshmultvt2', 'threshmultvt3', 'threshmultvt4', 'threshmultvt5')

        case ('37')
          do while (line(1:1) .ne. '+' )
            read(luimanfile, '(a)', end=20) line
          end do
          linidx = linidx + 1
          call readValues(manFile(isub)%proc, line, 'thinvalp', 'tyldrmp', 'tplrmp', 'tstrmp')

        case ('38')
          do while (line(1:1) .ne. '+' )
            read(luimanfile, '(a)', end=20) line
          end do
          linidx = linidx + 1
          call readValues(manFile(isub)%proc, line, 'thinvalf', 'tyldrmf', 'tplrmf', 'tstrmf')

        case ('42')
          do while (line(1:1) .ne. '+' )
            read(luimanfile, '(a)', end=20) line
          end do
          linidx = linidx + 1
          call readValues(manFile(isub)%proc, line, 'harv_report_flg', 'harv_calib_flg', 'harv_unit_flg', &
            'mature_warn_flg', 'cutflag', 'cutvalh', 'cyldrmh', 'cplrmh', 'cstrmh')

        case ('43')
          do while (line(1:1) .ne. '+' )
            read(luimanfile, '(a)', end=20) line
          end do
          linidx = linidx + 1
          call readValues(manFile(isub)%proc, line, 'harv_report_flg', 'harv_calib_flg', 'harv_unit_flg', &
                                                    'mature_warn_flg', 'cutvalf', 'cyldrmf', 'cplrmf', 'cstrmf')

        case ('47')
          do while (line(1:1) .ne. '+' )
            read(luimanfile, '(a)', end=20) line
          end do
          linidx = linidx + 1
          call readValues(manFile(isub)%proc, line, 'harv_report_flg', 'harv_calib_flg', 'harv_unit_flg', &
                                                    'mature_warn_flg', 'thinvalp', 'tyldrmp', 'tplrmp', 'tstrmp')

        case ('48')
          do while (line(1:1) .ne. '+' )
            read(luimanfile, '(a)', end=20) line
          end do
          linidx = linidx + 1
          call readValues(manFile(isub)%proc, line, 'harv_report_flg', 'harv_calib_flg', 'harv_unit_flg', &
                                                    'mature_warn_flg', 'thinvalf', 'tyldrmf', 'tplrmf', 'tstrmf')

        case ('50')
          do while (line(1:1) .ne. '+' )
            read(luimanfile, '(a)', end=20) line
          end do
          linidx = linidx + 1
          ! Read surface residue counts and amount
          call readValues(manFile(isub)%proc, line, 'numst', 'rstandht', 'rstandmass', 'rflatmass', 'rbc')

          ! get additional line of data
          read(luimanfile, '(a)', end=20) line
          do while (line(1:1) .ne. '+' )
            read(luimanfile, '(a)', end=20) line
          end do
          linidx = linidx + 1
          ! read buried residue amounts
          call readValues(manFile(isub)%proc, line, 'rburiedmass', 'rburieddepth', 'rrootmass', 'rrootdepth')

          ! get additional line of data
          read(luimanfile, '(a)', end=20) line
          do while (line(1:1) .ne. '+' )
            read(luimanfile, '(a)', end=20) line
          end do
          linidx = linidx + 1
          ! read decomposition parameters for type of residue buried
          call readValues(manFile(isub)%proc, line, 'standdk', 'surfdk', 'burieddk', 'rootdk', 'stemnodk', &
                                                    'stemdia', 'thrddys', 'covfact')

        ! get additional line of data
          read(luimanfile, '(a)', end=20) line
          do while (line(1:1) .ne. '+' )
            read(luimanfile, '(a)', end=20) line
          end do
          linidx = linidx + 1
          ! read decomposition parameters for type of residue buried
          call readValues(manFile(isub)%proc, line, 'resevapa', 'resevapb')

        case ('51')
          do while (line(1:1) .ne. '+' )
            read(luimanfile, '(a)', end=20) line
          end do
          linidx = linidx + 1
          ! read population, spacing and yield flags
          call readValues(manFile(isub)%proc, line, 'rowflag', 'rowspac', 'rowridge')

          read(luimanfile, '(a)', end=20) line
          do while (line(1:1) .ne. '+' )
            read(luimanfile, '(a)', end=20) line
          end do
          linidx = linidx + 1
          call readValues(manFile(isub)%proc, line, 'plantpop', 'dmaxshoot', 'cbaflag', 'tgtyield', &
                                                    'cbafact', 'cyrafact', 'hyldflag')

          read(luimanfile, '(a)', end=20) line
          do while (line(1:1) .ne. '+' )
            read(luimanfile, '(a)', end=20) line
          end do
          linidx = linidx + 1
          ! read yield reporting name
          call readValues(manFile(isub)%proc, line, 'hyldunits')

          read(luimanfile, '(a)', end=20) line
          do while (line(1:1) .ne. '+' )
            read(luimanfile, '(a)', end=20) line
          end do
          linidx = linidx + 1
          ! read yield reporting values and growth characteristics
          call readValues(manFile(isub)%proc, line, 'hyldwater', 'hyconfact', 'idc', 'grf', 'ck', 'hui0')

          read(luimanfile, '(a)', end=20) line
          do while (line(1:1) .ne. '+' )
            read(luimanfile, '(a)', end=20) line
          end do
          linidx = linidx + 1
          ! read crop growth parameters
          call readValues(manFile(isub)%proc, line, 'hmx', 'growdepth', 'rdmx', 'tbas', 'topt', 'thudf', 'dtm', 'thum')

          read(luimanfile, '(a)', end=20) line
          do while (line(1:1) .ne. '+' )
            read(luimanfile, '(a)', end=20) line
          end do
          linidx = linidx + 1
          call readValues(manFile(isub)%proc, line, 'frsx1', 'frsx2', 'frsy1', 'frsy2', 'verndel', 'bceff')

          read(luimanfile, '(a)', end=20) line
          do while (line(1:1) .ne. '+' )
            read(luimanfile, '(a)', end=20) line
          end do
          linidx = linidx + 1
          call readValues(manFile(isub)%proc, line, 'a_lf', 'b_lf', 'c_lf', 'd_lf', 'a_rp', 'b_rp', 'c_rp', 'd_rp')

          read(luimanfile, '(a)', end=20) line
          do while (line(1:1) .ne. '+' )
            read(luimanfile, '(a)', end=20) line
          end do
          linidx = linidx + 1
          call readValues(manFile(isub)%proc, line, 'a_ht', 'b_ht', 'ssaa', 'ssab', 'sla', 'huie', 'transf', 'diammax')

          read(luimanfile, '(a)', end=20) line
          do while (line(1:1) .ne. '+' )
            read(luimanfile, '(a)', end=20) line
          end do
          linidx = linidx + 1
          call readValues(manFile(isub)%proc, line, 'storeinit', 'mshoot', 'leafstem', 'fshoot', &
                                                    'leaf2stor', 'stem2stor', 'stor2stor', 'rbc')

          read(luimanfile, '(a)', end=20) line
          do while (line(1:1) .ne. '+' )
            read(luimanfile, '(a)', end=20) line
          end do
          linidx = linidx + 1
          call readValues(manFile(isub)%proc, line, 'standdk', 'surfdk', 'burieddk', 'rootdk', &
                                                    'stemnodk', 'stemdia', 'thrddys', 'covfact')

          read(luimanfile, '(a)', end=20) line
          do while (line(1:1) .ne. '+' )
            read(luimanfile, '(a)', end=20) line
          end do
          linidx = linidx + 1
          call readValues(manFile(isub)%proc, line, 'resevapa', 'resevapb', 'yield_coefficient', 'residue_intercept', &
            'regrow_location', 'noparam3', 'noparam2', 'noparam1')

        case ('61')
          do while (line(1:1) .ne. '+' )
            read(luimanfile, '(a)', end=20) line
          end do
          linidx = linidx + 1
          call readValues(manFile(isub)%proc, line, 'selpos', 'selpool', 'rstore', 'rleaf', 'rstem', 'rrootstore', 'rrootfiber')

        case ('62')
          do while (line(1:1) .ne. '+' )
            read(luimanfile, '(a)', end=20) line
          end do
          linidx = linidx + 1
          call readValues(manFile(isub)%proc, line, 'harv_report_flg', 'harv_calib_flg', 'harv_unit_flg', 'mature_warn_flg', &
            'selpos', 'selpool', 'selagepool', 'rstore', 'rleaf', 'rstem', 'rrootstore', 'rrootfiber')

        case ('65')
          do while (line(1:1) .ne. '+' )
            read(luimanfile, '(a)', end=20) line
          end do
          linidx = linidx + 1
          call readValues(manFile(isub)%proc, line, 'numst', 'rstandht', 'rstandmass', 'rflatmass', 'rbc')

          ! get additional line of data
          read(luimanfile, '(a)', end=20) line
          do while (line(1:1) .ne. '+' )
            read(luimanfile, '(a)', end=20) line
          end do
          linidx = linidx + 1
          ! read buried residue amounts
          call readValues(manFile(isub)%proc, line, 'rburiedmass', 'rburieddepth', 'rrootmass', 'rrootdepth')

          ! get additional line of data
          read(luimanfile, '(a)', end=20) line
          do while (line(1:1) .ne. '+' )
            read(luimanfile, '(a)', end=20) line
          end do
          linidx = linidx + 1
          ! read decomposition parameters
          call readValues(manFile(isub)%proc, line, 'standdk', 'surfdk', 'burieddk', 'rootdk', &
                                                    'stemnodk', 'stemdia', 'thrddys', 'covfact')

          ! get additional line of data
          read(luimanfile, '(a)', end=20) line
          do while (line(1:1) .ne. '+' )
            read(luimanfile, '(a)', end=20) line
          end do
          linidx = linidx + 1
          ! read parameters for residue suppression of evaporation
          call readValues(manFile(isub)%proc, line, 'resevapa', 'resevapb')

        case ('66')
          ! get additional line of data
          do while (line(1:1) .ne. '+' )
            read(luimanfile, '(a)', end=20) line
          end do
          linidx = linidx + 1
          call readValues(manFile(isub)%proc, line, 'M_numst', 'M_rstandht', 'M_rstandmass', 'M_rflatmass', 'rbc')

          ! get additional line of data
          read(luimanfile, '(a)', end=20) line
          do while (line(1:1) .ne. '+' )
            read(luimanfile, '(a)', end=20) line
          end do
          linidx = linidx + 1
          ! read buried residue amounts
          call readValues(manFile(isub)%proc, line, 'M_rburiedmass', 'M_rburieddepth', 'M_rrootmass', 'M_rrootdepth')

          ! get additional line of data
          read(luimanfile, '(a)', end=20) line
          do while (line(1:1) .ne. '+' )
            read(luimanfile, '(a)', end=20) line
          end do
          linidx = linidx + 1
          ! read total manure mass amount and buried fraction
          call readValues(manFile(isub)%proc, line, 'manure_total_mass', 'manure_buried_ratio')

          ! get additional line of data
          read(luimanfile, '(a)', end=20) line
          do while (line(1:1) .ne. '+' )
            read(luimanfile, '(a)', end=20) line
          end do
          linidx = linidx + 1
          ! read decomposition parameters
          call readValues(manFile(isub)%proc, line, 'standdk', 'surfdk', 'burieddk', 'rootdk', &
                                                    'stemnodk', 'stemdia', 'thrddys', 'covfact')

          ! get additional line of data
          read(luimanfile, '(a)', end=20) line
          do while (line(1:1) .ne. '+' )
            read(luimanfile, '(a)', end=20) line
          end do
          linidx = linidx + 1
          ! read parameters for residue suppression of evaporation
          call readValues(manFile(isub)%proc, line, 'resevapa', 'resevapb')

        case ('71')
          do while (line(1:1) .ne. '+' )
            read(luimanfile, '(a)', end=20) line
          end do
          linidx = linidx + 1
          ! read in flag, irrigation depth
          call readValues(manFile(isub)%proc, line, 'irrtype', 'irrdepth')

        case ('72')
          do while (line(1:1) .ne. '+' )
            read(luimanfile, '(a)', end=20) line
          end do
          linidx = linidx + 1
          ! read in irrigation monitor flag, max depth, rate, duration, location, min depth, mad, minrate
          call readValues(manFile(isub)%proc, line, 'irrmonflag', 'irrmaxapp', 'irrrate', 'irrduration', &
                                                    'irrapploc', 'irrminapp', 'irrmad', 'irrminint')

        case ('73')
          do while (line(1:1) .ne. '+' )
            read(luimanfile, '(a)', end=20) line
          end do
          linidx = linidx + 1
          ! read in single irrigation depth, rate, duration and location
          call readValues(manFile(isub)%proc, line, 'irrdepth', 'irrrate', 'irrduration', 'irrapploc')

        case ('91')
          do while (line(1:1) .ne. '+' )
            read(luimanfile, '(a)', end=20) line
          end do
          linidx = linidx + 1
          ! read in asd variables here
          call readValues(manFile(isub)%proc, line, 'asddepth', 'gmdx', 'gsdx', 'mnot', 'minf')

        case ('92')
          do while (line(1:1) .ne. '+' )
            read(luimanfile, '(a)', end=20) line
          end do
          linidx = linidx + 1
          ! read in wc variables here
          call readValues(manFile(isub)%proc, line, 'wcdepth', 'wc')

        end select

      case ('D')
        linidx = linidx + 1
        ! If we aren't pointing at a date, we have a problem
        if (line(1:1).ne.'D') goto 901
        ! Must be a space between 'D' and date in dd/mm/yyyy format
        read ( line(3:12), '(i2,1x,i2,1x,i4)' ,err=902) &
          t_operDate%day, t_operDate%month, t_operDate%year

      case ('*')
        linidx = linidx + 1
        if (linidx .eq. 2) then
          if (line (1:6).eq.'*START') then
            ! "*START" position found
            ! Obtain the number of years for the subregion's management cycle
            read (line (8:10), '(i3)', err=901) manFile(isub)%mperod
          else
            write(0,*) '*START not second non-comment line in ', trim(manFile(isub)%tinfil)
            call exit (1)
          end if
        else
          if (line (1:4).eq.'*END') then
            if (endidx.ne.0) goto 902
            endidx = linidx
          endif
          if (line (1:4).eq.'*EOF') then
            if (eofidx.ne.0) goto 903
            eofidx = linidx
          endif
          if ( (endidx .gt. 0) .and. (eofidx .gt. 0) ) then
            if (eofidx .ne. (endidx + 1) ) then
              ! end is not next to last
              goto 904
            end if
          end if
        end if

      end select
      goto 10
 20   close(luimanfile)
      if ( linidx .gt. eofidx ) then
        ! eof is not last
        goto 905
      end if

      ! check for at least 1 operation
      if ( .not. associated(manFile(isub)%operFirst) ) then
        goto 906
      end if

      return

      ! Error stops
  901 write(0,*) 'Error reading start param ', line(8:10)
      call exit (1)
  902 write(0,*) 'Duplicate *END statements in ', trim(manFile(isub)%tinfil)
      call exit (1)
  903 write(0,*) 'Duplicate *EOF statements in ', trim(manFile(isub)%tinfil)
      call exit (1)
  904 write(0,*) '*END not penultimate line in ', trim(manFile(isub)%tinfil)
      call exit (1)
  905 write(0,*) '*EOF not last line in ', trim(manFile(isub)%tinfil)
      call exit (1)
  906 write(0,*) 'No starting date specified in ', trim(manFile(isub)%tinfil)
      call exit (1)
 1001 write(0,*) 'Error reading Operation line'
      call exit (1)
 1002 write(0,*) 'Error reading Group line'
      call exit (1)
 1003 write(0,*) 'Error reading Process line'
      call exit (1)

  end subroutine read_old_manfile

  subroutine readOperValuesV1(operPtr, inStr, nameV1)
    use manage_data_struct_defs, only: operation
    type(operation), pointer :: operPtr
    character(len=*), intent(in) :: inStr
    character(len=*), intent(in) :: nameV1

    integer :: idxV1
    character(len=4) :: typeV1

    call get_value_type_index ( operPtr%OGPidx, nameV1, typeV1, idxV1 )

    if (    (typeV1 .eq. 'real') ) then
      read( inStr(2:len_trim(inStr)), *, err=1901 ) operPtr%r_param(idxV1)%p_value
    else if ( (typeV1 .eq. 'str') ) then
      operPtr%s_param(idxV1)%p_value = inStr(3:len_trim(inStr))
    else
      write(*,*) 'Error reading O ', operPtr%operID, '. Not all variable types accounted for.'
    end if
    return
 1901 write(0,*) 'Error reading Operation: ', 'O', operPtr%operID, ' in: ', trim(manFile(isub)%tinfil), ' Line: ', trim(inStr)
    call exit (1)
  end subroutine readOperValuesV1

  subroutine readOperValuesV2(operPtr, inStr, nameV1, nameV2)
    use manage_data_struct_defs, only: operation
    type(operation), pointer :: operPtr
    character(len=*), intent(in) :: inStr
    character(len=*), intent(in) :: nameV1
    character(len=*), intent(in) :: nameV2

    integer :: idxV1
    integer :: idxV2
    character(len=4) :: typeV1
    character(len=4) :: typeV2

    call get_value_type_index ( operPtr%OGPidx, nameV1, typeV1, idxV1 )
    call get_value_type_index ( operPtr%OGPidx, nameV2, typeV2, idxV2 )

    if (    (typeV1 .eq. 'real') &
      .and. (typeV2 .eq. 'real') &
      ) then
      read(inStr(2:len_trim(inStr)), *, err=1902) operPtr%r_param(idxV1)%p_value, &
                                                operPtr%r_param(idxV2)%p_value
    else
      write(*,*) 'Error reading O ', operPtr%operID, '. Not all variable types accounted for.'
    end if
    return
 1902 write(0,*) 'Error reading Operation: ', 'O', operPtr%operID, ' in: ', trim(manFile(isub)%tinfil), ' Line: ', trim(inStr)
    call exit (1)
  end subroutine readOperValuesV2

  subroutine readOperValuesV5(operPtr, inStr, nameV1, nameV2, nameV3, nameV4, nameV5)
    use manage_data_struct_defs, only: operation
    type(operation), pointer :: operPtr
    character(len=*), intent(in) :: inStr
    character(len=*), intent(in) :: nameV1
    character(len=*), intent(in) :: nameV2
    character(len=*), intent(in) :: nameV3
    character(len=*), intent(in) :: nameV4
    character(len=*), intent(in) :: nameV5

    integer :: idxV1
    integer :: idxV2
    integer :: idxV3
    integer :: idxV4
    integer :: idxV5
    character(len=4) :: typeV1
    character(len=4) :: typeV2
    character(len=4) :: typeV3
    character(len=4) :: typeV4
    character(len=4) :: typeV5

    call get_value_type_index ( operPtr%OGPidx, nameV1, typeV1, idxV1 )
    call get_value_type_index ( operPtr%OGPidx, nameV2, typeV2, idxV2 )
    call get_value_type_index ( operPtr%OGPidx, nameV3, typeV3, idxV3 )
    call get_value_type_index ( operPtr%OGPidx, nameV4, typeV4, idxV4 )
    call get_value_type_index ( operPtr%OGPidx, nameV5, typeV5, idxV5 )

    if (    (typeV1 .eq. 'real') &
      .and. (typeV2 .eq. 'real') &
      .and. (typeV3 .eq. 'real') &
      .and. (typeV4 .eq. 'real') &
      .and. (typeV5 .eq. 'real') &
      ) then
      read(inStr(2:len_trim(inStr)), *, err=1905) operPtr%r_param(idxV1)%p_value, &
                                                operPtr%r_param(idxV2)%p_value, &
                                                operPtr%r_param(idxV3)%p_value, &
                                                operPtr%r_param(idxV4)%p_value, &
                                                operPtr%r_param(idxV5)%p_value
    else
      write(*,*) 'Error reading O ', operPtr%operID, '. Not all variable types accounted for.'
    end if
    return
 1905 write(0,*) 'Error reading Operation: ', 'O', operPtr%operID, ' in: ', trim(manFile(isub)%tinfil), ' Line: ', trim(inStr)
    call exit (1)
  end subroutine readOperValuesV5

  subroutine readOperValuesV7(operPtr, inStr, nameV1, nameV2, nameV3, nameV4, nameV5, nameV6, nameV7)
    use manage_data_struct_defs, only: operation
    type(operation), pointer :: operPtr
    character(len=*), intent(in) :: inStr
    character(len=*), intent(in) :: nameV1
    character(len=*), intent(in) :: nameV2
    character(len=*), intent(in) :: nameV3
    character(len=*), intent(in) :: nameV4
    character(len=*), intent(in) :: nameV5
    character(len=*), intent(in) :: nameV6
    character(len=*), intent(in) :: nameV7

    integer :: idxV1
    integer :: idxV2
    integer :: idxV3
    integer :: idxV4
    integer :: idxV5
    integer :: idxV6
    integer :: idxV7
    character(len=4) :: typeV1
    character(len=4) :: typeV2
    character(len=4) :: typeV3
    character(len=4) :: typeV4
    character(len=4) :: typeV5
    character(len=4) :: typeV6
    character(len=4) :: typeV7

    call get_value_type_index ( operPtr%OGPidx, nameV1, typeV1, idxV1 )
    call get_value_type_index ( operPtr%OGPidx, nameV2, typeV2, idxV2 )
    call get_value_type_index ( operPtr%OGPidx, nameV3, typeV3, idxV3 )
    call get_value_type_index ( operPtr%OGPidx, nameV4, typeV4, idxV4 )
    call get_value_type_index ( operPtr%OGPidx, nameV5, typeV5, idxV5 )
    call get_value_type_index ( operPtr%OGPidx, nameV6, typeV6, idxV6 )
    call get_value_type_index ( operPtr%OGPidx, nameV7, typeV7, idxV7 )

    if (    (typeV1 .eq. 'real') &
      .and. (typeV2 .eq. 'real') &
      .and. (typeV3 .eq. 'real') &
      .and. (typeV4 .eq. 'real') &
      .and. (typeV5 .eq. 'real') &
      .and. (typeV6 .eq. 'real') &
      .and. (typeV7 .eq. 'real') &
      ) then
      read(inStr(2:len_trim(inStr)), *, err=1907) operPtr%r_param(idxV1)%p_value, &
                                                operPtr%r_param(idxV2)%p_value, &
                                                operPtr%r_param(idxV3)%p_value, &
                                                operPtr%r_param(idxV4)%p_value, &
                                                operPtr%r_param(idxV5)%p_value, &
                                                operPtr%r_param(idxV6)%p_value, &
                                                operPtr%r_param(idxV7)%p_value
    else
      write(*,*) 'Error reading O ', operPtr%operID, '. Not all variable types accounted for.'
    end if
    return
 1907 write(0,*) 'Error reading Operation: ', 'O', operPtr%operID, ' in: ', trim(manFile(isub)%tinfil), ' Line: ', trim(inStr)
    call exit (1)
  end subroutine readOperValuesV7

  subroutine readGrpValuesV1(grpPtr, inStr, nameV1)
    use manage_data_struct_defs, only: group
    type(group), pointer :: grpPtr
    character(len=*), intent(in) :: inStr
    character(len=*), intent(in) :: nameV1

    integer :: idxV1
    character(len=4) :: typeV1

    call get_value_type_index ( grpPtr%OGPidx, nameV1, typeV1, idxV1 )

    if (    (typeV1 .eq. 'real') ) then
      read(inStr(2:len_trim(inStr)), *, err=2006) grpPtr%r_param(idxV1)%p_value
    else if ( (typeV1 .eq. 'str') ) then
      grpPtr%s_param(idxV1)%p_value = inStr(3:len_trim(inStr))
    else
      write(*,*) 'Error reading G', grpPtr%grpID, '. Not all variable types accounted for.'
    end if
    return
 2006 write(0,*) 'Error reading Group: ', 'G', grpPtr%grpID, ' in: ', trim(manFile(isub)%tinfil), ' Line: ', trim(inStr)
    call exit (1)
  end subroutine readGrpValuesV1

  subroutine readGrpValuesV6(grpPtr, inStr, nameV1, nameV2, nameV3, nameV4, nameV5, nameV6)
    use manage_data_struct_defs, only: group
    type(group), pointer :: grpPtr
    character(len=*), intent(in) :: inStr
    character(len=*), intent(in) :: nameV1
    character(len=*), intent(in) :: nameV2
    character(len=*), intent(in) :: nameV3
    character(len=*), intent(in) :: nameV4
    character(len=*), intent(in) :: nameV5
    character(len=*), intent(in) :: nameV6

    integer :: idxV1
    integer :: idxV2
    integer :: idxV3
    integer :: idxV4
    integer :: idxV5
    integer :: idxV6
    character(len=4) :: typeV1
    character(len=4) :: typeV2
    character(len=4) :: typeV3
    character(len=4) :: typeV4
    character(len=4) :: typeV5
    character(len=4) :: typeV6

    call get_value_type_index ( grpPtr%OGPidx, nameV1, typeV1, idxV1 )
    call get_value_type_index ( grpPtr%OGPidx, nameV2, typeV2, idxV2 )
    call get_value_type_index ( grpPtr%OGPidx, nameV3, typeV3, idxV3 )
    call get_value_type_index ( grpPtr%OGPidx, nameV4, typeV4, idxV4 )
    call get_value_type_index ( grpPtr%OGPidx, nameV5, typeV5, idxV5 )
    call get_value_type_index ( grpPtr%OGPidx, nameV6, typeV6, idxV6 )

    if (    (typeV1 .eq. 'real') &
      .and. (typeV2 .eq. 'real') &
      .and. (typeV3 .eq. 'real') &
      .and. (typeV4 .eq. 'real') &
      .and. (typeV5 .eq. 'real') &
      .and. (typeV6 .eq. 'real') &
      ) then
      read(inStr(2:len_trim(inStr)), *, err=2006) grpPtr%r_param(idxV1)%p_value, &
                                                grpPtr%r_param(idxV2)%p_value, &
                                                grpPtr%r_param(idxV3)%p_value, &
                                                grpPtr%r_param(idxV4)%p_value, &
                                                grpPtr%r_param(idxV5)%p_value, &
                                                grpPtr%r_param(idxV6)%p_value
    else
      write(*,*) 'Error reading G', grpPtr%grpID, '. Not all variable types accounted for.'
    end if
    return
 2006 write(0,*) 'Error reading Group: ', 'G', grpPtr%grpID, ' in: ', trim(manFile(isub)%tinfil), ' Line: ', trim(inStr)
    call exit (1)
  end subroutine readGrpValuesV6

  subroutine readProcValuesV1(procPtr, inStr, nameV1)
    use manage_data_struct_defs, only: process
    type(process), pointer :: procPtr
    character(len=*), intent(in) :: inStr
    character(len=*), intent(in) :: nameV1

    integer :: idxV1
    character(len=4) :: typeV1

    call get_value_type_index ( procPtr%OGPidx, nameV1, typeV1, idxV1 )

    if (    (typeV1 .eq. 'int') &
      ) then
      read(inStr(2:len_trim(inStr)), *, err=2102) procPtr%i_param(idxV1)%p_value
    else if ( (typeV1 .eq. 'real') &
      ) then
      read(inStr(2:len_trim(inStr)), *, err=2102) procPtr%r_param(idxV1)%p_value
    else if ( (typeV1 .eq. 'str') &
      ) then
      procPtr%s_param(idxV1)%p_value = inStr(3:len_trim(inStr))
    else
      write(*,*) 'Error reading P', procPtr%procID, '. Not all variable types accounted for.'
    end if
    return
 2102 write(0,*) 'Error reading Process: ', 'P', procPtr%procID, ' in: ', trim(manFile(isub)%tinfil), ' Line: ', trim(inStr)
    call exit (1)
  end subroutine readprocValuesV1

  subroutine readProcValuesV2(procPtr, inStr, nameV1, nameV2)
    use manage_data_struct_defs, only: process
    type(process), pointer :: procPtr
    character(len=*), intent(in) :: inStr
    character(len=*), intent(in) :: nameV1
    character(len=*), intent(in) :: nameV2

    integer :: idxV1
    integer :: idxV2
    character(len=4) :: typeV1
    character(len=4) :: typeV2

    call get_value_type_index ( procPtr%OGPidx, nameV1, typeV1, idxV1 )
    call get_value_type_index ( procPtr%OGPidx, nameV2, typeV2, idxV2 )

    if (    (typeV1 .eq. 'int') &
      .and. (typeV2 .eq. 'real') &
      ) then
      read(inStr(2:len_trim(inStr)), *, err=2102) procPtr%i_param(idxV1)%p_value, &
                                                procPtr%r_param(idxV2)%p_value
    else if ( (typeV1 .eq. 'real') &
      .and. (typeV2 .eq. 'real') &
      ) then
      read(inStr(2:len_trim(inStr)), *, err=2102) procPtr%r_param(idxV1)%p_value, &
                                                procPtr%r_param(idxV2)%p_value
    else
      write(*,*) 'Error reading P', procPtr%procID, '. Not all variable types accounted for.'
    end if
    return
 2102 write(0,*) 'Error reading Process: ', 'P', procPtr%procID, ' in: ', trim(manFile(isub)%tinfil), ' Line: ', trim(inStr)
    call exit (1)
  end subroutine readprocValuesV2

  subroutine readProcValuesV3(procPtr, inStr, nameV1, nameV2, nameV3)
    use manage_data_struct_defs, only: process
    type(process), pointer :: procPtr
    character(len=*), intent(in) :: inStr
    character(len=*), intent(in) :: nameV1
    character(len=*), intent(in) :: nameV2
    character(len=*), intent(in) :: nameV3

    integer :: idxV1
    integer :: idxV2
    integer :: idxV3
    character(len=4) :: typeV1
    character(len=4) :: typeV2
    character(len=4) :: typeV3

    call get_value_type_index ( procPtr%OGPidx, nameV1, typeV1, idxV1 )
    call get_value_type_index ( procPtr%OGPidx, nameV2, typeV2, idxV2 )
    call get_value_type_index ( procPtr%OGPidx, nameV3, typeV3, idxV3 )

    if (    (typeV1 .eq. 'int') &
      .and. (typeV2 .eq. 'real') &
      .and. (typeV3 .eq. 'int') &
      ) then
      read(inStr(2:len_trim(inStr)), *, err=2103) procPtr%i_param(idxV1)%p_value, &
                                                procPtr%r_param(idxV2)%p_value, &
                                                procPtr%i_param(idxV3)%p_value
    else if ( (typeV1 .eq. 'real') &
      .and. (typeV2 .eq. 'real') &
      .and. (typeV3 .eq. 'real') &
      ) then
      read(inStr(2:len_trim(inStr)), *, err=2103) procPtr%r_param(idxV1)%p_value, &
                                                procPtr%r_param(idxV2)%p_value, &
                                                procPtr%r_param(idxV3)%p_value
    else
      write(*,*) 'Error reading P', procPtr%procID, '. Not all variable types accounted for.'
    end if
    return
 2103 write(0,*) 'Error reading Process: ', 'P', procPtr%procID, ' in: ', trim(manFile(isub)%tinfil), ' Line: ', trim(inStr)
    call exit (1)
  end subroutine readprocValuesV3

  subroutine readProcValuesV4(procPtr, inStr, nameV1, nameV2, nameV3, nameV4)
    use manage_data_struct_defs, only: process
    type(process), pointer :: procPtr
    character(len=*), intent(in) :: inStr
    character(len=*), intent(in) :: nameV1
    character(len=*), intent(in) :: nameV2
    character(len=*), intent(in) :: nameV3
    character(len=*), intent(in) :: nameV4

    integer :: idxV1
    integer :: idxV2
    integer :: idxV3
    integer :: idxV4
    character(len=4) :: typeV1
    character(len=4) :: typeV2
    character(len=4) :: typeV3
    character(len=4) :: typeV4

    call get_value_type_index ( procPtr%OGPidx, nameV1, typeV1, idxV1 )
    call get_value_type_index ( procPtr%OGPidx, nameV2, typeV2, idxV2 )
    call get_value_type_index ( procPtr%OGPidx, nameV3, typeV3, idxV3 )
    call get_value_type_index ( procPtr%OGPidx, nameV4, typeV4, idxV4 )

    if (    (typeV1 .eq. 'int') &
      .and. (typeV2 .eq. 'real') &
      .and. (typeV3 .eq. 'real') &
      .and. (typeV4 .eq. 'real') &
      ) then
      read(inStr(2:len_trim(inStr)), *, err=2104) procPtr%i_param(idxV1)%p_value, &
                                                procPtr%r_param(idxV2)%p_value, &
                                                procPtr%r_param(idxV3)%p_value, &
                                                procPtr%r_param(idxV4)%p_value
    else if ( (typeV1 .eq. 'real') &
      .and. (typeV2 .eq. 'real') &
      .and. (typeV3 .eq. 'real') &
      .and. (typeV4 .eq. 'real') &
      ) then
      read(inStr(2:len_trim(inStr)), *, err=2104) procPtr%r_param(idxV1)%p_value, &
                                                procPtr%r_param(idxV2)%p_value, &
                                                procPtr%r_param(idxV3)%p_value, &
                                                procPtr%r_param(idxV4)%p_value
    else
      write(*,*) 'Error reading P', procPtr%procID, '. Not all variable types accounted for.'
    end if
    return
 2104 write(0,*) 'Error reading Process: ', 'P', procPtr%procID, ' in: ', trim(manFile(isub)%tinfil), ' Line: ', trim(inStr)
    call exit (1)
  end subroutine readprocValuesV4

  subroutine readProcValuesV5(procPtr, inStr, nameV1, nameV2, nameV3, nameV4, nameV5)
    use manage_data_struct_defs, only: process
    type(process), pointer :: procPtr
    character(len=*), intent(in) :: inStr
    character(len=*), intent(in) :: nameV1
    character(len=*), intent(in) :: nameV2
    character(len=*), intent(in) :: nameV3
    character(len=*), intent(in) :: nameV4
    character(len=*), intent(in) :: nameV5

    integer :: idxV1
    integer :: idxV2
    integer :: idxV3
    integer :: idxV4
    integer :: idxV5
    character(len=4) :: typeV1
    character(len=4) :: typeV2
    character(len=4) :: typeV3
    character(len=4) :: typeV4
    character(len=4) :: typeV5

    call get_value_type_index ( procPtr%OGPidx, nameV1, typeV1, idxV1 )
    call get_value_type_index ( procPtr%OGPidx, nameV2, typeV2, idxV2 )
    call get_value_type_index ( procPtr%OGPidx, nameV3, typeV3, idxV3 )
    call get_value_type_index ( procPtr%OGPidx, nameV4, typeV4, idxV4 )
    call get_value_type_index ( procPtr%OGPidx, nameV5, typeV5, idxV5 )

    if (    (typeV1 .eq. 'int') &
      .and. (typeV2 .eq. 'real') &
      .and. (typeV3 .eq. 'real') &
      .and. (typeV4 .eq. 'real') &
      .and. (typeV5 .eq. 'real') &
      ) then
      read(inStr(2:len_trim(inStr)), *, err=2105) procPtr%i_param(idxV1)%p_value, &
                                                procPtr%r_param(idxV2)%p_value, &
                                                procPtr%r_param(idxV3)%p_value, &
                                                procPtr%r_param(idxV4)%p_value, &
                                                procPtr%r_param(idxV5)%p_value
    else if ( (typeV1 .eq. 'real') &
      .and. (typeV2 .eq. 'real') &
      .and. (typeV3 .eq. 'real') &
      .and. (typeV4 .eq. 'real') &
      .and. (typeV5 .eq. 'real') &
      ) then
      read(inStr(2:len_trim(inStr)), *, err=2105) procPtr%r_param(idxV1)%p_value, &
                                                procPtr%r_param(idxV2)%p_value, &
                                                procPtr%r_param(idxV3)%p_value, &
                                                procPtr%r_param(idxV4)%p_value, &
                                                procPtr%r_param(idxV5)%p_value
    else if ( (typeV1 .eq. 'real') &
      .and. (typeV2 .eq. 'real') &
      .and. (typeV3 .eq. 'real') &
      .and. (typeV4 .eq. 'real') &
      .and. (typeV5 .eq. 'int') &
      ) then
      read(inStr(2:len_trim(inStr)), *, err=2105) procPtr%r_param(idxV1)%p_value, &
                                                procPtr%r_param(idxV2)%p_value, &
                                                procPtr%r_param(idxV3)%p_value, &
                                                procPtr%r_param(idxV4)%p_value, &
                                                procPtr%i_param(idxV5)%p_value
    else
      write(*,*) 'Error reading P', procPtr%procID, '. Not all variable types accounted for.'
    end if
    return
 2105 write(0,*) 'Error reading Process: ', 'P', procPtr%procID, ' in: ', trim(manFile(isub)%tinfil), ' Line: ', trim(inStr)
    call exit (1)
  end subroutine readprocValuesV5

  subroutine readProcValuesV6(procPtr, inStr, nameV1, nameV2, nameV3, nameV4, nameV5, nameV6)
    use manage_data_struct_defs, only: process
    type(process), pointer :: procPtr
    character(len=*), intent(in) :: inStr
    character(len=*), intent(in) :: nameV1
    character(len=*), intent(in) :: nameV2
    character(len=*), intent(in) :: nameV3
    character(len=*), intent(in) :: nameV4
    character(len=*), intent(in) :: nameV5
    character(len=*), intent(in) :: nameV6

    integer :: idxV1
    integer :: idxV2
    integer :: idxV3
    integer :: idxV4
    integer :: idxV5
    integer :: idxV6
    character(len=4) :: typeV1
    character(len=4) :: typeV2
    character(len=4) :: typeV3
    character(len=4) :: typeV4
    character(len=4) :: typeV5
    character(len=4) :: typeV6

    call get_value_type_index ( procPtr%OGPidx, nameV1, typeV1, idxV1 )
    call get_value_type_index ( procPtr%OGPidx, nameV2, typeV2, idxV2 )
    call get_value_type_index ( procPtr%OGPidx, nameV3, typeV3, idxV3 )
    call get_value_type_index ( procPtr%OGPidx, nameV4, typeV4, idxV4 )
    call get_value_type_index ( procPtr%OGPidx, nameV5, typeV5, idxV5 )
    call get_value_type_index ( procPtr%OGPidx, nameV6, typeV6, idxV6 )

    if (    (typeV1 .eq. 'int') &
      .and. (typeV2 .eq. 'real') &
      .and. (typeV3 .eq. 'real') &
      .and. (typeV4 .eq. 'real') &
      .and. (typeV5 .eq. 'real') &
      .and. (typeV6 .eq. 'real') &
      ) then
      read(inStr(2:len_trim(inStr)), *, err=2106) procPtr%i_param(idxV1)%p_value, &
                                                procPtr%r_param(idxV2)%p_value, &
                                                procPtr%r_param(idxV3)%p_value, &
                                                procPtr%r_param(idxV4)%p_value, &
                                                procPtr%r_param(idxV5)%p_value, &
                                                procPtr%r_param(idxV6)%p_value
    else if ( (typeV1 .eq. 'real') &
      .and. (typeV2 .eq. 'real') &
      .and. (typeV3 .eq. 'real') &
      .and. (typeV4 .eq. 'real') &
      .and. (typeV5 .eq. 'real') &
      .and. (typeV6 .eq. 'real') &
      ) then
      read(inStr(2:len_trim(inStr)), *, err=2106) procPtr%r_param(idxV1)%p_value, &
                                                procPtr%r_param(idxV2)%p_value, &
                                                procPtr%r_param(idxV3)%p_value, &
                                                procPtr%r_param(idxV4)%p_value, &
                                                procPtr%r_param(idxV5)%p_value, &
                                                procPtr%r_param(idxV6)%p_value
    else if ( (typeV1 .eq. 'real') &
      .and. (typeV2 .eq. 'real') &
      .and. (typeV3 .eq. 'int') &
      .and. (typeV4 .eq. 'real') &
      .and. (typeV5 .eq. 'real') &
      .and. (typeV6 .eq. 'real') &
      ) then
      read(inStr(2:len_trim(inStr)), *, err=2106) procPtr%r_param(idxV1)%p_value, &
                                                procPtr%r_param(idxV2)%p_value, &
                                                procPtr%i_param(idxV3)%p_value, &
                                                procPtr%r_param(idxV4)%p_value, &
                                                procPtr%r_param(idxV5)%p_value, &
                                                procPtr%r_param(idxV6)%p_value
    else
      write(*,*) 'Error reading P', procPtr%procID, '. Not all variable types accounted for.'
    end if
    return
 2106 write(0,*) 'Error reading Process: ', 'P', procPtr%procID, ' in: ', trim(manFile(isub)%tinfil), ' Line: ', trim(inStr)
    call exit (1)
  end subroutine readprocValuesV6

  subroutine readProcValuesV7(procPtr, inStr, nameV1, nameV2, nameV3, nameV4, nameV5, nameV6, nameV7)
    use manage_data_struct_defs, only: process
    type(process), pointer :: procPtr
    character(len=*), intent(in) :: inStr
    character(len=*), intent(in) :: nameV1
    character(len=*), intent(in) :: nameV2
    character(len=*), intent(in) :: nameV3
    character(len=*), intent(in) :: nameV4
    character(len=*), intent(in) :: nameV5
    character(len=*), intent(in) :: nameV6
    character(len=*), intent(in) :: nameV7

    integer :: idxV1
    integer :: idxV2
    integer :: idxV3
    integer :: idxV4
    integer :: idxV5
    integer :: idxV6
    integer :: idxV7
    character(len=4) :: typeV1
    character(len=4) :: typeV2
    character(len=4) :: typeV3
    character(len=4) :: typeV4
    character(len=4) :: typeV5
    character(len=4) :: typeV6
    character(len=4) :: typeV7

    call get_value_type_index ( procPtr%OGPidx, nameV1, typeV1, idxV1 )
    call get_value_type_index ( procPtr%OGPidx, nameV2, typeV2, idxV2 )
    call get_value_type_index ( procPtr%OGPidx, nameV3, typeV3, idxV3 )
    call get_value_type_index ( procPtr%OGPidx, nameV4, typeV4, idxV4 )
    call get_value_type_index ( procPtr%OGPidx, nameV5, typeV5, idxV5 )
    call get_value_type_index ( procPtr%OGPidx, nameV6, typeV6, idxV6 )
    call get_value_type_index ( procPtr%OGPidx, nameV7, typeV7, idxV7 )

    if (    (typeV1 .eq. 'int') &
      .and. (typeV2 .eq. 'int') &
      .and. (typeV3 .eq. 'int') &
      .and. (typeV4 .eq. 'int') &
      .and. (typeV5 .eq. 'real') &
      .and. (typeV6 .eq. 'real') &
      .and. (typeV7 .eq. 'real') &
      ) then
      read(inStr(2:len_trim(inStr)), *, err=2107) procPtr%i_param(idxV1)%p_value, &
                                                procPtr%i_param(idxV2)%p_value, &
                                                procPtr%i_param(idxV3)%p_value, &
                                                procPtr%i_param(idxV4)%p_value, &
                                                procPtr%r_param(idxV5)%p_value, &
                                                procPtr%r_param(idxV6)%p_value, &
                                                procPtr%r_param(idxV7)%p_value
    else if ( (typeV1 .eq. 'real') &
      .and. (typeV2 .eq. 'real') &
      .and. (typeV3 .eq. 'int') &
      .and. (typeV4 .eq. 'real') &
      .and. (typeV5 .eq. 'real') &
      .and. (typeV6 .eq. 'real') &
      .and. (typeV7 .eq. 'int') &
      ) then
      read(inStr(2:len_trim(inStr)), *, err=2107) procPtr%r_param(idxV1)%p_value, &
                                                procPtr%r_param(idxV2)%p_value, &
                                                procPtr%i_param(idxV3)%p_value, &
                                                procPtr%r_param(idxV4)%p_value, &
                                                procPtr%r_param(idxV5)%p_value, &
                                                procPtr%r_param(idxV6)%p_value, &
                                                procPtr%i_param(idxV7)%p_value
    else if ( (typeV1 .eq. 'int') &
      .and. (typeV2 .eq. 'int') &
      .and. (typeV3 .eq. 'real') &
      .and. (typeV4 .eq. 'real') &
      .and. (typeV5 .eq. 'real') &
      .and. (typeV6 .eq. 'real') &
      .and. (typeV7 .eq. 'real') &
      ) then
      read(inStr(2:len_trim(inStr)), *, err=2107) procPtr%i_param(idxV1)%p_value, &
                                                procPtr%i_param(idxV2)%p_value, &
                                                procPtr%r_param(idxV3)%p_value, &
                                                procPtr%r_param(idxV4)%p_value, &
                                                procPtr%r_param(idxV5)%p_value, &
                                                procPtr%r_param(idxV6)%p_value, &
                                                procPtr%r_param(idxV7)%p_value
    else
      write(*,*) 'Error reading P', procPtr%procID, '. Not all variable types accounted for.'
    end if
    return
 2107 write(0,*) 'Error reading Process: ', 'P', procPtr%procID, ' in: ', trim(manFile(isub)%tinfil), ' Line: ', trim(inStr)
    call exit (1)
  end subroutine readprocValuesV7

  subroutine readProcValuesV8(procPtr, inStr, nameV1, nameV2, nameV3, nameV4, nameV5, nameV6, nameV7, nameV8)
    use manage_data_struct_defs, only: process
    type(process), pointer :: procPtr
    character(len=*), intent(in) :: inStr
    character(len=*), intent(in) :: nameV1
    character(len=*), intent(in) :: nameV2
    character(len=*), intent(in) :: nameV3
    character(len=*), intent(in) :: nameV4
    character(len=*), intent(in) :: nameV5
    character(len=*), intent(in) :: nameV6
    character(len=*), intent(in) :: nameV7
    character(len=*), intent(in) :: nameV8

    integer :: idxV1
    integer :: idxV2
    integer :: idxV3
    integer :: idxV4
    integer :: idxV5
    integer :: idxV6
    integer :: idxV7
    integer :: idxV8
    character(len=4) :: typeV1
    character(len=4) :: typeV2
    character(len=4) :: typeV3
    character(len=4) :: typeV4
    character(len=4) :: typeV5
    character(len=4) :: typeV6
    character(len=4) :: typeV7
    character(len=4) :: typeV8

    call get_value_type_index ( procPtr%OGPidx, nameV1, typeV1, idxV1 )
    call get_value_type_index ( procPtr%OGPidx, nameV2, typeV2, idxV2 )
    call get_value_type_index ( procPtr%OGPidx, nameV3, typeV3, idxV3 )
    call get_value_type_index ( procPtr%OGPidx, nameV4, typeV4, idxV4 )
    call get_value_type_index ( procPtr%OGPidx, nameV5, typeV5, idxV5 )
    call get_value_type_index ( procPtr%OGPidx, nameV6, typeV6, idxV6 )
    call get_value_type_index ( procPtr%OGPidx, nameV7, typeV7, idxV7 )
    call get_value_type_index ( procPtr%OGPidx, nameV8, typeV8, idxV8 )

    if (    (typeV1 .eq. 'int') &
      .and. (typeV2 .eq. 'int') &
      .and. (typeV3 .eq. 'int') &
      .and. (typeV4 .eq. 'int') &
      .and. (typeV5 .eq. 'real') &
      .and. (typeV6 .eq. 'real') &
      .and. (typeV7 .eq. 'real') &
      .and. (typeV8 .eq. 'real') &
      ) then
      read(inStr(2:len_trim(inStr)), *, err=2108) procPtr%i_param(idxV1)%p_value, &
                                                procPtr%i_param(idxV2)%p_value, &
                                                procPtr%i_param(idxV3)%p_value, &
                                                procPtr%i_param(idxV4)%p_value, &
                                                procPtr%r_param(idxV5)%p_value, &
                                                procPtr%r_param(idxV6)%p_value, &
                                                procPtr%r_param(idxV7)%p_value, &
                                                procPtr%r_param(idxV8)%p_value
    else if ( (typeV1 .eq. 'int') &
      .and. (typeV2 .eq. 'real') &
      .and. (typeV3 .eq. 'real') &
      .and. (typeV4 .eq. 'real') &
      .and. (typeV5 .eq. 'real') &
      .and. (typeV6 .eq. 'real') &
      .and. (typeV7 .eq. 'real') &
      .and. (typeV8 .eq. 'int') &
      ) then
      read(inStr(2:len_trim(inStr)), *, err=2108) procPtr%i_param(idxV1)%p_value, &
                                                procPtr%r_param(idxV2)%p_value, &
                                                procPtr%r_param(idxV3)%p_value, &
                                                procPtr%r_param(idxV4)%p_value, &
                                                procPtr%r_param(idxV5)%p_value, &
                                                procPtr%r_param(idxV6)%p_value, &
                                                procPtr%r_param(idxV7)%p_value, &
                                                procPtr%i_param(idxV8)%p_value
    else if ( (typeV1 .eq. 'int') &
      .and. (typeV2 .eq. 'real') &
      .and. (typeV3 .eq. 'real') &
      .and. (typeV4 .eq. 'real') &
      .and. (typeV5 .eq. 'real') &
      .and. (typeV6 .eq. 'real') &
      .and. (typeV7 .eq. 'real') &
      .and. (typeV8 .eq. 'real') &
      ) then
      read(inStr(2:len_trim(inStr)), *, err=2108) procPtr%i_param(idxV1)%p_value, &
                                                procPtr%r_param(idxV2)%p_value, &
                                                procPtr%r_param(idxV3)%p_value, &
                                                procPtr%r_param(idxV4)%p_value, &
                                                procPtr%r_param(idxV5)%p_value, &
                                                procPtr%r_param(idxV6)%p_value, &
                                                procPtr%r_param(idxV7)%p_value, &
                                                procPtr%r_param(idxV8)%p_value
    else if ( (typeV1 .eq. 'real') &
      .and. (typeV2 .eq. 'real') &
      .and. (typeV3 .eq. 'real') &
      .and. (typeV4 .eq. 'real') &
      .and. (typeV5 .eq. 'real') &
      .and. (typeV6 .eq. 'real') &
      .and. (typeV7 .eq. 'real') &
      .and. (typeV8 .eq. 'real') &
      ) then
      read(inStr(2:len_trim(inStr)), *, err=2108) procPtr%r_param(idxV1)%p_value, &
                                                procPtr%r_param(idxV2)%p_value, &
                                                procPtr%r_param(idxV3)%p_value, &
                                                procPtr%r_param(idxV4)%p_value, &
                                                procPtr%r_param(idxV5)%p_value, &
                                                procPtr%r_param(idxV6)%p_value, &
                                                procPtr%r_param(idxV7)%p_value, &
                                                procPtr%r_param(idxV8)%p_value
    else if ( (typeV1 .eq. 'real') &
      .and. (typeV2 .eq. 'real') &
      .and. (typeV3 .eq. 'real') &
      .and. (typeV4 .eq. 'real') &
      .and. (typeV5 .eq. 'real') &
      .and. (typeV6 .eq. 'int') &
      .and. (typeV7 .eq. 'int') &
      .and. (typeV8 .eq. 'real') &
      ) then
      read(inStr(2:len_trim(inStr)), *, err=2108) procPtr%r_param(idxV1)%p_value, &
                                                procPtr%r_param(idxV2)%p_value, &
                                                procPtr%r_param(idxV3)%p_value, &
                                                procPtr%r_param(idxV4)%p_value, &
                                                procPtr%r_param(idxV5)%p_value, &
                                                procPtr%i_param(idxV6)%p_value, &
                                                procPtr%i_param(idxV7)%p_value, &
                                                procPtr%r_param(idxV8)%p_value
    else if ( (typeV1 .eq. 'real') &
      .and. (typeV2 .eq. 'real') &
      .and. (typeV3 .eq. 'real') &
      .and. (typeV4 .eq. 'real') &
      .and. (typeV5 .eq. 'real') &
      .and. (typeV6 .eq. 'real') &
      .and. (typeV7 .eq. 'int') &
      .and. (typeV8 .eq. 'real') &
      ) then
      read(inStr(2:len_trim(inStr)), *, err=2108) procPtr%r_param(idxV1)%p_value, &
                                                procPtr%r_param(idxV2)%p_value, &
                                                procPtr%r_param(idxV3)%p_value, &
                                                procPtr%r_param(idxV4)%p_value, &
                                                procPtr%r_param(idxV5)%p_value, &
                                                procPtr%r_param(idxV6)%p_value, &
                                                procPtr%i_param(idxV7)%p_value, &
                                                procPtr%r_param(idxV8)%p_value
    else if ( (typeV1 .eq. 'real') &
      .and. (typeV2 .eq. 'real') &
      .and. (typeV3 .eq. 'real') &
      .and. (typeV4 .eq. 'real') &
      .and. (typeV5 .eq. 'real') &
      .and. (typeV6 .eq. 'real') &
      .and. (typeV7 .eq. 'real') &
      .and. (typeV8 .eq. 'int') &
      ) then
      read(inStr(2:len_trim(inStr)), *, err=2108) procPtr%r_param(idxV1)%p_value, &
                                                procPtr%r_param(idxV2)%p_value, &
                                                procPtr%r_param(idxV3)%p_value, &
                                                procPtr%r_param(idxV4)%p_value, &
                                                procPtr%r_param(idxV5)%p_value, &
                                                procPtr%r_param(idxV6)%p_value, &
                                                procPtr%r_param(idxV7)%p_value, &
                                                procPtr%i_param(idxV8)%p_value
    else
      write(*,*) 'Error reading P', procPtr%procID, '. Not all variable types accounted for.'
    end if
    return
 2108 write(0,*) 'Error reading Process: ', 'P', procPtr%procID, ' in: ', trim(manFile(isub)%tinfil), ' Line: ', trim(inStr)
    call exit (1)
  end subroutine readprocValuesV8

  subroutine readProcValuesV9(procPtr, inStr, nameV1, nameV2, nameV3, nameV4, nameV5, nameV6, nameV7, nameV8, nameV9)
    use manage_data_struct_defs, only: process
    type(process), pointer :: procPtr
    character(len=*), intent(in) :: inStr
    character(len=*), intent(in) :: nameV1
    character(len=*), intent(in) :: nameV2
    character(len=*), intent(in) :: nameV3
    character(len=*), intent(in) :: nameV4
    character(len=*), intent(in) :: nameV5
    character(len=*), intent(in) :: nameV6
    character(len=*), intent(in) :: nameV7
    character(len=*), intent(in) :: nameV8
    character(len=*), intent(in) :: nameV9

    integer :: idxV1
    integer :: idxV2
    integer :: idxV3
    integer :: idxV4
    integer :: idxV5
    integer :: idxV6
    integer :: idxV7
    integer :: idxV8
    integer :: idxV9
    character(len=4) :: typeV1
    character(len=4) :: typeV2
    character(len=4) :: typeV3
    character(len=4) :: typeV4
    character(len=4) :: typeV5
    character(len=4) :: typeV6
    character(len=4) :: typeV7
    character(len=4) :: typeV8
    character(len=4) :: typeV9

    call get_value_type_index ( procPtr%OGPidx, nameV1, typeV1, idxV1 )
    call get_value_type_index ( procPtr%OGPidx, nameV2, typeV2, idxV2 )
    call get_value_type_index ( procPtr%OGPidx, nameV3, typeV3, idxV3 )
    call get_value_type_index ( procPtr%OGPidx, nameV4, typeV4, idxV4 )
    call get_value_type_index ( procPtr%OGPidx, nameV5, typeV5, idxV5 )
    call get_value_type_index ( procPtr%OGPidx, nameV6, typeV6, idxV6 )
    call get_value_type_index ( procPtr%OGPidx, nameV7, typeV7, idxV7 )
    call get_value_type_index ( procPtr%OGPidx, nameV8, typeV8, idxV8 )
    call get_value_type_index ( procPtr%OGPidx, nameV9, typeV9, idxV9 )

    if (    (typeV1 .eq. 'int') &
      .and. (typeV2 .eq. 'int') &
      .and. (typeV3 .eq. 'int') &
      .and. (typeV4 .eq. 'int') &
      .and. (typeV5 .eq. 'int') &
      .and. (typeV6 .eq. 'real') &
      .and. (typeV7 .eq. 'real') &
      .and. (typeV8 .eq. 'real') &
      .and. (typeV9 .eq. 'real') &
      ) then
      read(inStr(2:len_trim(inStr)), *, err=2109) procPtr%i_param(idxV1)%p_value, &
                                                procPtr%i_param(idxV2)%p_value, &
                                                procPtr%i_param(idxV3)%p_value, &
                                                procPtr%i_param(idxV4)%p_value, &
                                                procPtr%i_param(idxV5)%p_value, &
                                                procPtr%r_param(idxV6)%p_value, &
                                                procPtr%r_param(idxV7)%p_value, &
                                                procPtr%r_param(idxV8)%p_value, &
                                                procPtr%r_param(idxV9)%p_value
    else
      write(*,*) 'Error reading P', procPtr%procID, '. Not all variable types accounted for.'
    end if
    return
 2109 write(0,*) 'Error reading Process: ', 'P', procPtr%procID, ' in: ', trim(manFile(isub)%tinfil), ' Line: ', trim(inStr)
    call exit (1)
  end subroutine readprocValuesV9

  subroutine readProcValuesV12(procPtr, inStr, nameV1, nameV2, nameV3, nameV4, nameV5, nameV6, &
                                                nameV7, nameV8, nameV9, nameV10, nameV11, nameV12)
    use manage_data_struct_defs, only: process
    type(process), pointer :: procPtr
    character(len=*), intent(in) :: inStr
    character(len=*), intent(in) :: nameV1
    character(len=*), intent(in) :: nameV2
    character(len=*), intent(in) :: nameV3
    character(len=*), intent(in) :: nameV4
    character(len=*), intent(in) :: nameV5
    character(len=*), intent(in) :: nameV6
    character(len=*), intent(in) :: nameV7
    character(len=*), intent(in) :: nameV8
    character(len=*), intent(in) :: nameV9
    character(len=*), intent(in) :: nameV10
    character(len=*), intent(in) :: nameV11
    character(len=*), intent(in) :: nameV12

    integer :: idxV1
    integer :: idxV2
    integer :: idxV3
    integer :: idxV4
    integer :: idxV5
    integer :: idxV6
    integer :: idxV7
    integer :: idxV8
    integer :: idxV9
    integer :: idxV10
    integer :: idxV11
    integer :: idxV12
    character(len=4) :: typeV1
    character(len=4) :: typeV2
    character(len=4) :: typeV3
    character(len=4) :: typeV4
    character(len=4) :: typeV5
    character(len=4) :: typeV6
    character(len=4) :: typeV7
    character(len=4) :: typeV8
    character(len=4) :: typeV9
    character(len=4) :: typeV10
    character(len=4) :: typeV11
    character(len=4) :: typeV12

    call get_value_type_index ( procPtr%OGPidx, nameV1, typeV1, idxV1 )
    call get_value_type_index ( procPtr%OGPidx, nameV2, typeV2, idxV2 )
    call get_value_type_index ( procPtr%OGPidx, nameV3, typeV3, idxV3 )
    call get_value_type_index ( procPtr%OGPidx, nameV4, typeV4, idxV4 )
    call get_value_type_index ( procPtr%OGPidx, nameV5, typeV5, idxV5 )
    call get_value_type_index ( procPtr%OGPidx, nameV6, typeV6, idxV6 )
    call get_value_type_index ( procPtr%OGPidx, nameV7, typeV7, idxV7 )
    call get_value_type_index ( procPtr%OGPidx, nameV8, typeV8, idxV8 )
    call get_value_type_index ( procPtr%OGPidx, nameV9, typeV9, idxV9 )
    call get_value_type_index ( procPtr%OGPidx, nameV10, typeV10, idxV10 )
    call get_value_type_index ( procPtr%OGPidx, nameV11, typeV11, idxV11 )
    call get_value_type_index ( procPtr%OGPidx, nameV12, typeV12, idxV12 )

    if (    (typeV1 .eq. 'int') &
      .and. (typeV2 .eq. 'int') &
      .and. (typeV3 .eq. 'int') &
      .and. (typeV4 .eq. 'int') &
      .and. (typeV5 .eq. 'int') &
      .and. (typeV6 .eq. 'int') &
      .and. (typeV7 .eq. 'int') &
      .and. (typeV8 .eq. 'real') &
      .and. (typeV9 .eq. 'real') &
      .and. (typeV10 .eq. 'real') &
      .and. (typeV11 .eq. 'real') &
      .and. (typeV12 .eq. 'real') &
      ) then
      read(inStr(2:len_trim(inStr)), *, err=2112) procPtr%i_param(idxV1)%p_value, &
                                                procPtr%i_param(idxV2)%p_value, &
                                                procPtr%i_param(idxV3)%p_value, &
                                                procPtr%i_param(idxV4)%p_value, &
                                                procPtr%i_param(idxV5)%p_value, &
                                                procPtr%i_param(idxV6)%p_value, &
                                                procPtr%i_param(idxV7)%p_value, &
                                                procPtr%r_param(idxV8)%p_value, &
                                                procPtr%r_param(idxV9)%p_value, &
                                                procPtr%r_param(idxV10)%p_value, &
                                                procPtr%r_param(idxV11)%p_value, &
                                                procPtr%r_param(idxV12)%p_value
    else
      write(*,*) 'Error reading P', procPtr%procID, '. Not all variable types accounted for.'
    end if
    return
 2112 write(0,*) 'Error reading Process: ', 'P', procPtr%procID, ' in: ', trim(manFile(isub)%tinfil), ' Line: ', trim(inStr)
    call exit (1)
  end subroutine readprocValuesV12

end module manage_xml_mod

