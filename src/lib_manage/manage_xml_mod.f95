!$Author$
!$Date$
!$Revision$
!$HeadURL$

module manage_xml_mod

  use flib_sax
  use manage_data_struct_defs, only: manFile, operation_date, elemCreate

  integer, parameter :: MAX_NAME_LEN = 40
  integer, parameter :: MAX_TYPE_LEN = 10

  type :: tag_def
    character(len=MAX_NAME_LEN)  :: name   ! tag name
    logical :: acquired                    ! .true. if tag has been read
    logical :: in_tag                      ! .true. if inside tag now
  end type tag_def

  type(tag_def), dimension(:), allocatable :: man_tag
  integer :: max_tags

  type :: name_type
    character(len=MAX_NAME_LEN)  :: p_name   ! parameter name
    character(len=MAX_TYPE_LEN)  :: p_type   ! parameter type
  end type name_type

  integer :: max_p_names
  type(name_type), dimension(:), allocatable :: param_nt

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

  integer :: int_cnt     ! count of integer values to be read into an operation, group or process, for allocation
  integer :: real_cnt    ! count of real values to be read into an operation, group or process, for allocation
  integer :: str_cnt     ! count of string values to be read into an operation, group or process, for allocation

  integer :: i_cnt       ! count of integer values acutally read
  integer :: r_cnt       ! count of real values acutally read
  integer :: s_cnt       ! count of string values acutally read

  integer :: isub ! current subregion number used in a routines in this module
  type(operation_date) :: t_operDate
  character(len=80) :: t_operName
  character(len=3) :: t_code
  integer :: t_id

  logical :: all_wepsmanvalues
  logical :: all_operationDBs
  logical :: all_actionvalues
  logical :: all_params
  logical :: manfile_complete ! indicator that a complete manfile was read

contains

  subroutine init_man_xml( isubr )
    integer, intent(in) :: isubr

    integer :: idx
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

    max_p_names = 192   ! count of unique paramnames operation definitions
    allocate( param_nt(max_p_names), stat=alloc_stat)
    if( alloc_stat .gt. 0 ) then
      write(*,*) 'ERROR: memory alloc., input_tag'
    end if

    param_nt(1)%p_name = 'a_ht'
    param_nt(1)%p_type = 'float'
    param_nt(2)%p_name = 'a_lf'
    param_nt(2)%p_type = 'float'
    param_nt(3)%p_name = 'a_rp'
    param_nt(3)%p_type = 'float'
    param_nt(4)%p_name = 'asddepth'
    param_nt(4)%p_type = 'float'
    param_nt(5)%p_name = 'asdf'
    param_nt(5)%p_type = 'float'
    param_nt(6)%p_name = 'bceff'
    param_nt(6)%p_type = 'float'
    param_nt(7)%p_name = 'b_ht'
    param_nt(7)%p_type = 'float'
    param_nt(8)%p_name = 'b_lf'
    param_nt(8)%p_type = 'float'
    param_nt(9)%p_name = 'b_rp'
    param_nt(9)%p_type = 'float'
    param_nt(10)%p_name = 'burieddk'
    param_nt(10)%p_type = 'float'
    param_nt(11)%p_name = 'burydist'
    param_nt(11)%p_type = 'int'
    param_nt(12)%p_name = 'cbafact'
    param_nt(12)%p_type = 'float'
    param_nt(13)%p_name = 'cbaflag'
    param_nt(13)%p_type = 'int'
    param_nt(14)%p_name = 'ck'
    param_nt(14)%p_type = 'float'
    param_nt(15)%p_name = 'c_lf'
    param_nt(15)%p_type = 'float'
    param_nt(16)%p_name = 'covfact'
    param_nt(16)%p_type = 'float'
    param_nt(17)%p_name = 'cplrmf'
    param_nt(17)%p_type = 'float'
    param_nt(18)%p_name = 'cplrmh'
    param_nt(18)%p_type = 'float'
    param_nt(19)%p_name = 'crif'
    param_nt(19)%p_type = 'float'
    param_nt(20)%p_name = 'c_rp'
    param_nt(20)%p_type = 'float'
    param_nt(21)%p_name = 'cstrmf'
    param_nt(21)%p_type = 'float'
    param_nt(22)%p_name = 'cstrmh'
    param_nt(22)%p_type = 'float'
    param_nt(23)%p_name = 'cutflag'
    param_nt(23)%p_type = 'int'
    param_nt(24)%p_name = 'cutvalf'
    param_nt(24)%p_type = 'float'
    param_nt(25)%p_name = 'cutvalh'
    param_nt(25)%p_type = 'float'
    param_nt(26)%p_name = 'cyldrmf'
    param_nt(26)%p_type = 'float'
    param_nt(27)%p_name = 'cyldrmh'
    param_nt(27)%p_type = 'float'
    param_nt(28)%p_name = 'cyrafact'
    param_nt(28)%p_type = 'float'
    param_nt(29)%p_name = 'defoliateflag'
    param_nt(29)%p_type = 'int'
    param_nt(30)%p_name = 'diammax'
    param_nt(30)%p_type = 'float'
    param_nt(31)%p_name = 'dkhit'
    param_nt(31)%p_type = 'float'
    param_nt(32)%p_name = 'dkspac'
    param_nt(32)%p_type = 'float'
    param_nt(33)%p_name = 'd_lf'
    param_nt(33)%p_type = 'float'
    param_nt(34)%p_name = 'dmaxshoot'
    param_nt(34)%p_type = 'float'
    param_nt(35)%p_name = 'd_rp'
    param_nt(35)%p_type = 'float'
    param_nt(36)%p_name = 'dtm'
    param_nt(36)%p_type = 'int'
    param_nt(37)%p_name = 'fbioflagvt'
    param_nt(37)%p_type = 'int'
    param_nt(38)%p_name = 'frselpool'
    param_nt(38)%p_type = 'int'
    param_nt(39)%p_name = 'frsx1'
    param_nt(39)%p_type = 'float'
    param_nt(40)%p_name = 'frsx2'
    param_nt(40)%p_type = 'float'
    param_nt(41)%p_name = 'frsy1'
    param_nt(41)%p_type = 'float'
    param_nt(42)%p_name = 'frsy2'
    param_nt(42)%p_type = 'float'
    param_nt(43)%p_name = 'fshoot'
    param_nt(43)%p_type = 'float'
    param_nt(44)%p_name = 'gamdname'
    param_nt(44)%p_type = 'string'
    param_nt(45)%p_name = 'gbioarea'
    param_nt(45)%p_type = 'float'
    param_nt(46)%p_name = 'gcropname'
    param_nt(46)%p_type = 'string'
    param_nt(47)%p_name = 'gmdx'
    param_nt(47)%p_type = 'float'
    param_nt(48)%p_name = 'grf'
    param_nt(48)%p_type = 'float'
    param_nt(49)%p_name = 'growdepth'
    param_nt(49)%p_type = 'float'
    param_nt(50)%p_name = 'gsdx'
    param_nt(50)%p_type = 'float'
    param_nt(51)%p_name = 'gtdepth'
    param_nt(51)%p_type = 'float'
    param_nt(52)%p_name = 'gtilArea'
    param_nt(52)%p_type = 'float'
    param_nt(53)%p_name = 'gtilint'
    param_nt(53)%p_type = 'float'
    param_nt(54)%p_name = 'gtmaxdepth'
    param_nt(54)%p_type = 'float'
    param_nt(55)%p_name = 'gtmindepth'
    param_nt(55)%p_type = 'float'
    param_nt(56)%p_name = 'gtstddepth'
    param_nt(56)%p_type = 'float'
    param_nt(57)%p_name = 'harv_calib_flg'
    param_nt(57)%p_type = 'int'
    param_nt(58)%p_name = 'harv_report_flg'
    param_nt(58)%p_type = 'int'
    param_nt(59)%p_name = 'harv_unit_flg'
    param_nt(59)%p_type = 'int'
    param_nt(60)%p_name = 'hmx'
    param_nt(60)%p_type = 'float'
    param_nt(61)%p_name = 'hui0'
    param_nt(61)%p_type = 'float'
    param_nt(62)%p_name = 'huie'
    param_nt(62)%p_type = 'float'
    param_nt(63)%p_name = 'hyconfact'
    param_nt(63)%p_type = 'float'
    param_nt(64)%p_name = 'hyldflag'
    param_nt(64)%p_type = 'int'
    param_nt(65)%p_name = 'hyldunits'
    param_nt(65)%p_type = 'string'
    param_nt(66)%p_name = 'hyldwater'
    param_nt(66)%p_type = 'float'
    param_nt(67)%p_name = 'idc'
    param_nt(67)%p_type = 'int'
    param_nt(68)%p_name = 'irrapploc'
    param_nt(68)%p_type = 'float'
    param_nt(69)%p_name = 'irrdepth'
    param_nt(69)%p_type = 'float'
    param_nt(70)%p_name = 'irrduration'
    param_nt(70)%p_type = 'float'
    param_nt(71)%p_name = 'irrmad'
    param_nt(71)%p_type = 'float'
    param_nt(72)%p_name = 'irrmaxapp'
    param_nt(72)%p_type = 'float'
    param_nt(73)%p_name = 'irrminapp'
    param_nt(73)%p_type = 'float'
    param_nt(74)%p_name = 'irrminint'
    param_nt(74)%p_type = 'float'
    param_nt(75)%p_name = 'irrmonflag'
    param_nt(75)%p_type = 'int'
    param_nt(76)%p_name = 'irrrate'
    param_nt(76)%p_type = 'float'
    param_nt(77)%p_name = 'irrtype'
    param_nt(77)%p_type = 'int'
    param_nt(78)%p_name = 'kilflag'
    param_nt(78)%p_type = 'int'
    param_nt(79)%p_name = 'laymix'
    param_nt(79)%p_type = 'float'
    param_nt(80)%p_name = 'leaf2stor'
    param_nt(80)%p_type = 'float'
    param_nt(81)%p_name = 'leafstem'
    param_nt(81)%p_type = 'float'
    param_nt(82)%p_name = 'manure_buried_ratio'
    param_nt(82)%p_type = 'float'
    param_nt(83)%p_name = 'manure_total_mass'
    param_nt(83)%p_type = 'float'
    param_nt(84)%p_name = 'massburyvt1'
    param_nt(84)%p_type = 'float'
    param_nt(85)%p_name = 'massburyvt2'
    param_nt(85)%p_type = 'float'
    param_nt(86)%p_name = 'massburyvt3'
    param_nt(86)%p_type = 'float'
    param_nt(87)%p_name = 'massburyvt4'
    param_nt(87)%p_type = 'float'
    param_nt(88)%p_name = 'massburyvt5'
    param_nt(88)%p_type = 'float'
    param_nt(89)%p_name = 'massflatvt1'
    param_nt(89)%p_type = 'float'
    param_nt(90)%p_name = 'massflatvt2'
    param_nt(90)%p_type = 'float'
    param_nt(91)%p_name = 'massflatvt3'
    param_nt(91)%p_type = 'float'
    param_nt(92)%p_name = 'massflatvt4'
    param_nt(92)%p_type = 'float'
    param_nt(93)%p_name = 'massflatvt5'
    param_nt(93)%p_type = 'float'
    param_nt(94)%p_name = 'massresurvt1'
    param_nt(94)%p_type = 'float'
    param_nt(95)%p_name = 'massresurvt2'
    param_nt(95)%p_type = 'float'
    param_nt(96)%p_name = 'massresurvt3'
    param_nt(96)%p_type = 'float'
    param_nt(97)%p_name = 'massresurvt4'
    param_nt(97)%p_type = 'float'
    param_nt(98)%p_name = 'massresurvt5'
    param_nt(98)%p_type = 'float'
    param_nt(99)%p_name = 'mature_warn_flg'
    param_nt(99)%p_type = 'int'
    param_nt(100)%p_name = 'minf'
    param_nt(100)%p_type = 'float'
    param_nt(101)%p_name = 'mnot'
    param_nt(101)%p_type = 'float'
    param_nt(102)%p_name = 'M_numst'
    param_nt(102)%p_type = 'float'
    param_nt(103)%p_name = 'M_rburieddepth'
    param_nt(103)%p_type = 'float'
    param_nt(104)%p_name = 'M_rburiedmass'
    param_nt(104)%p_type = 'float'
    param_nt(105)%p_name = 'M_rflatmass'
    param_nt(105)%p_type = 'float'
    param_nt(106)%p_name = 'M_rrootdepth'
    param_nt(106)%p_type = 'float'
    param_nt(107)%p_name = 'M_rrootmass'
    param_nt(107)%p_type = 'float'
    param_nt(108)%p_name = 'M_rstandht'
    param_nt(108)%p_type = 'float'
    param_nt(109)%p_name = 'M_rstandmass'
    param_nt(109)%p_type = 'float'
    param_nt(110)%p_name = 'mshoot'
    param_nt(110)%p_type = 'float'
    param_nt(111)%p_name = 'noparam1'
    param_nt(111)%p_type = 'float'
    param_nt(112)%p_name = 'noparam2'
    param_nt(112)%p_type = 'float'
    param_nt(113)%p_name = 'noparam3'
    param_nt(113)%p_type = 'float'
    param_nt(114)%p_name = 'numst'
    param_nt(114)%p_type = 'float'
    param_nt(115)%p_name = 'odirect'
    param_nt(115)%p_type = 'float'
    param_nt(116)%p_name = 'oenergyarea'
    param_nt(116)%p_type = 'float'
    param_nt(117)%p_name = 'ofuel'
    param_nt(117)%p_type = 'string'
    param_nt(118)%p_name = 'omaxspeed'
    param_nt(118)%p_type = 'float'
    param_nt(119)%p_name = 'ominspeed'
    param_nt(119)%p_type = 'float'
    param_nt(120)%p_name = 'ospeed'
    param_nt(120)%p_type = 'float'
    param_nt(121)%p_name = 'ostdspeed'
    param_nt(121)%p_type = 'float'
    param_nt(122)%p_name = 'ostir'
    param_nt(122)%p_type = 'float'
    param_nt(123)%p_name = 'plantpop'
    param_nt(123)%p_type = 'float'
    param_nt(124)%p_name = 'ratemultvt1'
    param_nt(124)%p_type = 'float'
    param_nt(125)%p_name = 'ratemultvt2'
    param_nt(125)%p_type = 'float'
    param_nt(126)%p_name = 'ratemultvt3'
    param_nt(126)%p_type = 'float'
    param_nt(127)%p_name = 'ratemultvt4'
    param_nt(127)%p_type = 'float'
    param_nt(128)%p_name = 'ratemultvt5'
    param_nt(128)%p_type = 'float'
    param_nt(129)%p_name = 'rbc'
    param_nt(129)%p_type = 'int'
    param_nt(130)%p_name = 'rburieddepth'
    param_nt(130)%p_type = 'float'
    param_nt(131)%p_name = 'rburiedmass'
    param_nt(131)%p_type = 'float'
    param_nt(132)%p_name = 'rdgflag'
    param_nt(132)%p_type = 'int'
    param_nt(133)%p_name = 'rdghit'
    param_nt(133)%p_type = 'float'
    param_nt(134)%p_name = 'rdgspac'
    param_nt(134)%p_type = 'float'
    param_nt(135)%p_name = 'rdgwidth'
    param_nt(135)%p_type = 'float'
    param_nt(136)%p_name = 'rdmx'
    param_nt(136)%p_type = 'float'
    param_nt(137)%p_name = 'regrow_location'
    param_nt(137)%p_type = 'float'
    param_nt(138)%p_name = 'resevapa'
    param_nt(138)%p_type = 'float'
    param_nt(139)%p_name = 'resevapb'
    param_nt(139)%p_type = 'float'
    param_nt(140)%p_name = 'residue_intercept'
    param_nt(140)%p_type = 'float'
    param_nt(141)%p_name = 'rflatmass'
    param_nt(141)%p_type = 'float'
    param_nt(142)%p_name = 'rleaf'
    param_nt(142)%p_type = 'float'
    param_nt(143)%p_name = 'rootdk'
    param_nt(143)%p_type = 'float'
    param_nt(144)%p_name = 'rowflag'
    param_nt(144)%p_type = 'int'
    param_nt(145)%p_name = 'rowridge'
    param_nt(145)%p_type = 'int'
    param_nt(146)%p_name = 'rowspac'
    param_nt(146)%p_type = 'float'
    param_nt(147)%p_name = 'rrootdepth'
    param_nt(147)%p_type = 'float'
    param_nt(148)%p_name = 'rrootfiber'
    param_nt(148)%p_type = 'float'
    param_nt(149)%p_name = 'rrootmass'
    param_nt(149)%p_type = 'float'
    param_nt(150)%p_name = 'rrootstore'
    param_nt(150)%p_type = 'float'
    param_nt(151)%p_name = 'rroughflag'
    param_nt(151)%p_type = 'int'
    param_nt(152)%p_name = 'rrough'
    param_nt(152)%p_type = 'float'
    param_nt(153)%p_name = 'rstandht'
    param_nt(153)%p_type = 'float'
    param_nt(154)%p_name = 'rstandmass'
    param_nt(154)%p_type = 'float'
    param_nt(155)%p_name = 'rstem'
    param_nt(155)%p_type = 'float'
    param_nt(156)%p_name = 'rstore'
    param_nt(156)%p_type = 'float'
    param_nt(157)%p_name = 'selagepool'
    param_nt(157)%p_type = 'int'
    param_nt(158)%p_name = 'selpool'
    param_nt(158)%p_type = 'int'
    param_nt(159)%p_name = 'selpos'
    param_nt(159)%p_type = 'int'
    param_nt(160)%p_name = 'sla'
    param_nt(160)%p_type = 'float'
    param_nt(161)%p_name = 'soilos'
    param_nt(161)%p_type = 'float'
    param_nt(162)%p_name = 'ssaa'
    param_nt(162)%p_type = 'float'
    param_nt(163)%p_name = 'ssab'
    param_nt(163)%p_type = 'float'
    param_nt(164)%p_name = 'standdk'
    param_nt(164)%p_type = 'float'
    param_nt(165)%p_name = 'stem2stor'
    param_nt(165)%p_type = 'float'
    param_nt(166)%p_name = 'stemdia'
    param_nt(166)%p_type = 'float'
    param_nt(167)%p_name = 'stemnodk'
    param_nt(167)%p_type = 'float'
    param_nt(168)%p_name = 'stor2stor'
    param_nt(168)%p_type = 'float'
    param_nt(169)%p_name = 'storeinit'
    param_nt(169)%p_type = 'float'
    param_nt(170)%p_name = 'surfdk'
    param_nt(170)%p_type = 'float'
    param_nt(171)%p_name = 'tbas'
    param_nt(171)%p_type = 'float'
    param_nt(172)%p_name = 'tgtyield'
    param_nt(172)%p_type = 'float'
    param_nt(173)%p_name = 'thinvalf'
    param_nt(173)%p_type = 'float'
    param_nt(174)%p_name = 'thinvalp'
    param_nt(174)%p_type = 'float'
    param_nt(175)%p_name = 'thrddys'
    param_nt(175)%p_type = 'float'
    param_nt(176)%p_name = 'threshmultvt1'
    param_nt(176)%p_type = 'float'
    param_nt(177)%p_name = 'threshmultvt2'
    param_nt(177)%p_type = 'float'
    param_nt(178)%p_name = 'threshmultvt3'
    param_nt(178)%p_type = 'float'
    param_nt(179)%p_name = 'threshmultvt4'
    param_nt(179)%p_type = 'float'
    param_nt(180)%p_name = 'threshmultvt5'
    param_nt(180)%p_type = 'float'
    param_nt(181)%p_name = 'thudf'
    param_nt(181)%p_type = 'int'
    param_nt(182)%p_name = 'thum'
    param_nt(182)%p_type = 'float'
    param_nt(183)%p_name = 'topt'
    param_nt(183)%p_type = 'float'
    param_nt(184)%p_name = 'tplrmf'
    param_nt(184)%p_type = 'float'
    param_nt(185)%p_name = 'tplrmp'
    param_nt(185)%p_type = 'float'
    param_nt(186)%p_name = 'transf'
    param_nt(186)%p_type = 'int'
    param_nt(187)%p_name = 'tstrmf'
    param_nt(187)%p_type = 'float'
    param_nt(188)%p_name = 'tstrmp'
    param_nt(188)%p_type = 'float'
    param_nt(189)%p_name = 'tyldrmf'
    param_nt(189)%p_type = 'float'
    param_nt(190)%p_name = 'tyldrmp'
    param_nt(190)%p_type = 'float'
    param_nt(191)%p_name = 'verndel'
    param_nt(191)%p_type = 'float'
    param_nt(192)%p_name = 'yield_coefficient'
    param_nt(192)%p_type = 'float'

    all_wepsmanvalues = .true.  ! .true. indicates that no values are required
    all_operationDBs = .true.  ! .false. indicates that a value is required
    all_actionvalues = .true.
    all_params = .true.

  end subroutine init_man_xml

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

        if ( man_tag(idx)%acquired ) then
          write(*,*) 'ACQUIRED: ', man_tag(idx)%name!, man_tag(idx)%acquired
        end if

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
      end if

    else if (idx .eq. actionvalue) then
      if( man_tag(identity)%acquired &
        .and. all_params &
        ) then
        man_tag(identity)%acquired = .false.
        ! stays .true. if all previous values have been true
        all_actionvalues = (all_actionvalues .and. .true. )
      else
        all_actionvalues = .false.
      end if

    else if (idx .eq. identity) then
      if( man_tag(code)%acquired &
        .and. man_tag(id)%acquired &
        ) then
        man_tag(code)%acquired = .false.
        man_tag(id)%acquired = .false.
        man_tag(identity)%acquired = .true.
      end if

    else if (idx .eq. param) then
      if( man_tag(p_name)%acquired &
        .and. man_tag(value)%acquired &
        ) then
        man_tag(p_name)%acquired = .false.
        man_tag(value)%acquired = .false.
        ! stays .true. if all previous values have been true
        all_params = (all_params .and. .true. )
      else
        all_params = .false.
      end if

    end if

  end subroutine end_man_element_handler

  subroutine pcdata_man_chunk_handler(chunk)
    use read_write_xml_mod, only: read_param
    character(len=*), intent(in) :: chunk

    integer :: idx
    character(len=80) :: param_value

    character(len=3) :: operID
    character(len=3) :: grpID
    character(len=3) :: procID

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
                int_cnt = 0
                real_cnt = 0
                str_cnt = 0
                i_cnt = 0
                r_cnt = 0
                s_cnt = 0
                select case (t_code)
                case ('O')
                  operID = trim(param_value)
                  select case (operID)
                  case ('01')
                    real_cnt = 5
                  case ('03')
                    real_cnt = 7
                    str_cnt = 1
                  case ('04')
                    real_cnt = 2
                    str_cnt = 1
                  end select
                  if ( .not. associated(manFile(isub)%operFirst) ) then
                    manFile(isub)%operFirst => elemCreate( manFile(isub)%operFirst, operID, int_cnt, real_cnt, str_cnt )
                    manFile(isub)%oper => manFile(isub)%operFirst
                  else
                    manFile(isub)%oper%operNext => elemCreate( manFile(isub)%oper%operNext, operID, int_cnt, real_cnt, str_cnt )
                    manFile(isub)%oper => manFile(isub)%oper%operNext
                  end if
                  ! new operation, so nullify current group and process
                  nullify(manFile(isub)%grp)
                  nullify(manFile(isub)%proc)
                case ('G')
                  grpID = trim(param_value)
                  select case (grpID)
                  case ('01')
                    real_cnt = 6
                  case ('02')
                    real_cnt = 1
                  case ('03')
                    str_cnt = 1
                  case ('04')
                    str_cnt = 1
                  end select
                  if ( .not. associated(manFile(isub)%oper) ) then
                    write(*,*) 'Group appears before Operation in Management File: ', trim(manFile(isub)%tinfil)
                    call exit(1)
                  else if ( .not. associated(manFile(isub)%oper%grpFirst) ) then
                    manFile(isub)%oper%grpFirst => elemCreate( manFile(isub)%oper%grpFirst, grpID, int_cnt, real_cnt, str_cnt )
                    manFile(isub)%grp => manFile(isub)%oper%grpFirst
                  else
                    manFile(isub)%grp%grpNext => elemCreate( manFile(isub)%grp%grpNext, grpID, int_cnt, real_cnt, str_cnt )
                    manFile(isub)%grp => manFile(isub)%grp%grpNext
                  end if
                case ('P')
                  procID = trim(param_value)
                  select case (procID)
                  case ('02')
                    int_cnt = 1
                    real_cnt = 1
                  case ('05')
                    int_cnt = 1
                    real_cnt = 5
                  case ('11')
                    real_cnt = 2
                  case ('12')
                    real_cnt = 1
                  case ('13')
                    real_cnt = 1
                  case ('24')
                    int_cnt = 1
                    real_cnt = 5
                  case ('25')
                    int_cnt = 1
                    real_cnt = 5
                  case ('26')
                    real_cnt = 5
                  case ('30')
                    int_cnt = 1
                  case ('31')
                    int_cnt = 1
                  case ('32')
                    int_cnt = 1
                    real_cnt = 4
                  case ('33')
                    real_cnt = 4
                  case ('34')
                    int_cnt = 1
                    real_cnt = 10
                  case ('37')
                    real_cnt = 4
                  case ('38')
                    real_cnt = 4
                  case ('42')
                    int_cnt = 5
                    real_cnt = 4
                  case ('43')
                    int_cnt = 4
                    real_cnt = 4
                  case ('47')
                    int_cnt = 4
                    real_cnt = 4
                  case ('48')
                    int_cnt = 4
                    real_cnt = 4
                  case ('50')
                    int_cnt = 1
                    real_cnt = 18
                  case ('51')
                    int_cnt = 9
                    real_cnt = 61
                    str_cnt = 1
                  case ('61')
                    int_cnt = 2
                    real_cnt = 5
                  case ('62')
                    int_cnt = 7
                    real_cnt = 5
                  case ('65')
                    int_cnt = 1
                    real_cnt = 18
                  case ('66')
                    int_cnt = 1
                    real_cnt = 20
                  case ('71')
                    int_cnt = 1
                    real_cnt = 1
                  case ('72')
                    int_cnt = 1
                    real_cnt = 7
                  case ('73')
                    real_cnt = 4
                  case ('91')
                    real_cnt = 5
                  end select
                  if ( .not. associated(manFile(isub)%grp) ) then
                    ! Operation has process without group preceeding, create null group to support structure.
                    manFile(isub)%oper%grpFirst => elemCreate( manFile(isub)%oper%grpFirst, '00', 0, 0, 0 )
                    manFile(isub)%grp => manFile(isub)%oper%grpFirst
                  end if
                  if ( .not. associated(manFile(isub)%grp%procFirst) ) then
                    manFile(isub)%grp%procFirst => elemCreate( manFile(isub)%grp%procFirst, procID, int_cnt, real_cnt, str_cnt )
                    manFile(isub)%proc => manFile(isub)%grp%procFirst
                  else
                    manFile(isub)%proc%procNext => elemCreate( manFile(isub)%proc%procNext, procID, int_cnt, real_cnt, str_cnt )
                    manFile(isub)%proc => manFile(isub)%proc%procNext
                  end if
                end select
              end if
            else if (man_tag(param)%in_tag ) then
              if (man_tag(p_name)%in_tag ) then
                do idx = 1, max_p_names
                  if ( param_nt(idx)%p_name .eq. param_value ) then
                    select case (param_nt(idx)%p_type)
                    case ('int')
                      i_cnt = i_cnt + 1
                      if ( i_cnt .le. int_cnt ) then
                        if ( operID .ne. '' ) then
                          manFile(isub)%oper%i_params(i_cnt)%p_name = trim(param_value)
                        else if ( grpID .ne. '' ) then
                          manFile(isub)%grp%i_params(i_cnt)%p_name = trim(param_value)
                        else if ( procID .ne. '' ) then
                          manFile(isub)%proc%i_params(i_cnt)%p_name = trim(param_value)
                        end if
                      else
                        write(*,*) 'Too many parameter values for: '
                      end if
                    case ('float')
                      r_cnt = r_cnt + 1
                    case ('string')
                      s_cnt = s_cnt + 1
                    end select
                    exit  ! found name in list, look no further
                  end if
                end do
              else if (man_tag(value)%in_tag ) then
                     !call read_param(man_tag(p_name)%name, param_value, manFile(isub)%oper%i_params(i_cnt)%p_value )
              end if
            end if
          end if
        end if

      end if
    end if

  end subroutine pcdata_man_chunk_handler

end module manage_xml_mod

