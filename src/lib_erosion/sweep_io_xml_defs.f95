!$Author$
!$Date$
!$Revision$
!$HeadURL$

module sweep_io_xml_defs

  implicit none

  integer, parameter :: MAX_NAME_LEN  = 40

  integer :: indent  ! indent level of tag being written
  integer, parameter :: INDENT_SPACES = 2

  type :: tag_def
    character(len=MAX_NAME_LEN)  :: name   ! tag name
    logical :: acquired                    ! .true. if tag has been read
    logical :: in_tag                      ! .true. if inside tag now
  end type tag_def

  type(tag_def), dimension(:), allocatable :: input_tag
  integer :: max_tags
  
  integer, parameter :: GUI_lat = 1
  integer, parameter :: GUI_lon = 2
  integer, parameter :: SCI_Accounts = 3
  integer, parameter :: SCI_Account = 4
  integer, parameter :: SCI_AerodynamicRoughness = 5
  integer, parameter :: SCI_AggregateDensity = 6
  integer, parameter :: SCI_AggregateGMD = 7
  integer, parameter :: SCI_AggregateGSD = 8
  integer, parameter :: SCI_AggregateMAX = 9
  integer, parameter :: SCI_AggregateMIN = 10
  integer, parameter :: SCI_AggregateStability = 11
  integer, parameter :: SCI_AirDensity = 12
  integer, parameter :: SCI_AnemometerFlag = 13
  integer, parameter :: SCI_AnemometerHeight = 14
  integer, parameter :: SCI_AverageAnnualPrecipitation = 15
  integer, parameter :: SCI_BarPoint = 16
  integer, parameter :: SCI_Barrier = 17
  integer, parameter :: SCI_Barriers = 18
  integer, parameter :: SCI_BiomassFlatCover = 19
  integer, parameter :: SCI_BulkDensity = 20
  integer, parameter :: SCI_Clay = 21
  integer, parameter :: SCI_coord = 22
  integer, parameter :: SCI_coords = 23
  integer, parameter :: SCI_CropHeight = 24
  integer, parameter :: SCI_CropLAI = 25
  integer, parameter :: SCI_CropRowSpacing = 26
  integer, parameter :: SCI_CropSAI = 27
  integer, parameter :: SCI_CropSeedPlace = 28
  integer, parameter :: SCI_CrustCover = 29
  integer, parameter :: SCI_CrustDensity = 30
  integer, parameter :: SCI_CrustFracCoverLoose = 31
  integer, parameter :: SCI_CrustMassCoverLoose = 32
  integer, parameter :: SCI_CrustStability = 33
  integer, parameter :: SCI_CrustThick = 34
  integer, parameter :: SCI_DikeSpacing = 35
  integer, parameter :: SCI_height = 36
  integer, parameter :: SCI_index = 37
  integer, parameter :: SCI_LayerThickness = 38
  integer, parameter :: SCI_number = 39
  integer, parameter :: SCI_BarPoints = 40
  integer, parameter :: SCI_porosity = 41
  integer, parameter :: SCI_RandomRoughness = 42
  integer, parameter :: SCI_RegionAngle = 43
  integer, parameter :: SCI_ResidueHeight = 44
  integer, parameter :: SCI_ResidueLAI = 45
  integer, parameter :: SCI_ResidueSAI = 46
  integer, parameter :: SCI_RidgeHeight = 47
  integer, parameter :: SCI_RidgeOrientation = 48
  integer, parameter :: SCI_RidgeSpacing = 49
  integer, parameter :: SCI_RidgeWidth = 50
  integer, parameter :: SCI_RockVolume = 51
  integer, parameter :: SCI_Sand = 52
  integer, parameter :: SCI_Silt = 53
  integer, parameter :: SCI_SnowDepth = 54
  integer, parameter :: SCI_SoilLay = 55
  integer, parameter :: SCI_SoilLays = 56
  integer, parameter :: SCI_Subregion = 57
  integer, parameter :: SCI_Subregions = 58
  integer, parameter :: SCI_SurfaceSubDayWater = 59
  integer, parameter :: SCI_SurfaceSubDayWaters = 60
  integer, parameter :: SCI_VeryFineSand = 61
  integer, parameter :: SCI_WaterContent = 62
  integer, parameter :: SCI_width = 63
  integer, parameter :: SCI_WiltingPoint = 64
  integer, parameter :: SCI_WindDirection = 65
  integer, parameter :: SCI_WindSpeed = 66
  integer, parameter :: SCI_WindSpeeds = 67
  integer, parameter :: SCI_x = 68
  integer, parameter :: SCI_XGrid = 69
  integer, parameter :: SCI_XLength = 70
  integer, parameter :: SCI_XOrigin = 71
  integer, parameter :: SCI_y = 72
  integer, parameter :: SCI_YGrid = 73
  integer, parameter :: SCI_YLength = 74
  integer, parameter :: SCI_YOrigin = 75
  integer, parameter :: sweepData = 76

  interface w_begin_tag
    module procedure w_begin_tag_a0
    module procedure w_begin_tag_a1
    module procedure w_begin_tag_a2
  end interface

contains

  subroutine init_input_xml()

    integer :: idx
    integer :: alloc_stat

    indent = 0

    max_tags = 76   ! count of unique tags needed from all dtd files
    allocate( input_tag(max_tags), stat=alloc_stat)
    if( alloc_stat .gt. 0 ) then
      write(*,*) 'ERROR: memory alloc., input_tag'
    end if

    ! assign defaults to flag status values
    do idx = 1, max_tags
      input_tag(idx)%acquired = .false.
      input_tag(idx)%in_tag = .false.
    end do

    ! assign tag names
    input_tag(1)%name = "GUI_lat"
    input_tag(2)%name = "GUI_lon"
    input_tag(3)%name = "SCI_Accounts"
    input_tag(4)%name = "SCI_Account"
    input_tag(5)%name = "SCI_AerodynamicRoughness"
    input_tag(6)%name = "SCI_AggregateDensity"
    input_tag(7)%name = "SCI_AggregateGMD"
    input_tag(8)%name = "SCI_AggregateGSD"
    input_tag(9)%name = "SCI_AggregateMAX"
    input_tag(10)%name = "SCI_AggregateMIN"
    input_tag(11)%name = "SCI_AggregateStability"
    input_tag(12)%name = "SCI_AirDensity"
    input_tag(13)%name = "SCI_AnemometerFlag"
    input_tag(14)%name = "SCI_AnemometerHeight"
    input_tag(15)%name = "SCI_AverageAnnualPrecipitation"
    input_tag(16)%name = "SCI_BarPoint"
    input_tag(17)%name = "SCI_Barrier"
    input_tag(18)%name = "SCI_Barriers"
    input_tag(19)%name = "SCI_BiomassFlatCover"
    input_tag(20)%name = "SCI_BulkDensity"
    input_tag(21)%name = "SCI_Clay"
    input_tag(22)%name = "SCI_coord"
    input_tag(23)%name = "SCI_coords"
    input_tag(24)%name = "SCI_CropHeight"
    input_tag(25)%name = "SCI_CropLAI"
    input_tag(26)%name = "SCI_CropRowSpacing"
    input_tag(27)%name = "SCI_CropSAI"
    input_tag(28)%name = "SCI_CropSeedPlace"
    input_tag(29)%name = "SCI_CrustCover"
    input_tag(30)%name = "SCI_CrustDensity"
    input_tag(31)%name = "SCI_CrustFracCoverLoose"
    input_tag(32)%name = "SCI_CrustMassCoverLoose"
    input_tag(33)%name = "SCI_CrustStability"
    input_tag(34)%name = "SCI_CrustThick"
    input_tag(35)%name = "SCI_DikeSpacing"
    input_tag(36)%name = "SCI_height"
    input_tag(37)%name = "SCI_index"
    input_tag(38)%name = "SCI_LayerThickness"
    input_tag(39)%name = "SCI_number"
    input_tag(40)%name = "SCI_BarPoints"
    input_tag(41)%name = "SCI_porosity"
    input_tag(42)%name = "SCI_RandomRoughness"
    input_tag(43)%name = "SCI_RegionAngle"
    input_tag(44)%name = "SCI_ResidueHeight"
    input_tag(45)%name = "SCI_ResidueLAI"
    input_tag(46)%name = "SCI_ResidueSAI"
    input_tag(47)%name = "SCI_RidgeHeight"
    input_tag(48)%name = "SCI_RidgeOrientation"
    input_tag(49)%name = "SCI_RidgeSpacing"
    input_tag(50)%name = "SCI_RidgeWidth"
    input_tag(51)%name = "SCI_RockVolume"
    input_tag(52)%name = "SCI_Sand"
    input_tag(53)%name = "SCI_Silt"
    input_tag(54)%name = "SCI_SnowDepth"
    input_tag(55)%name = "SCI_SoilLay"
    input_tag(56)%name = "SCI_SoilLays"
    input_tag(57)%name = "SCI_Subregion"
    input_tag(58)%name = "SCI_Subregions"
    input_tag(59)%name = "SCI_SurfaceSubDayWater"
    input_tag(60)%name = "SCI_SurfaceSubDayWaters"
    input_tag(61)%name = "SCI_VeryFineSand"
    input_tag(62)%name = "SCI_WaterContent"
    input_tag(63)%name = "SCI_width"
    input_tag(64)%name = "SCI_WiltingPoint"
    input_tag(65)%name = "SCI_WindDirection"
    input_tag(66)%name = "SCI_WindSpeed"
    input_tag(67)%name = "SCI_WindSpeeds"
    input_tag(68)%name = "SCI_x"
    input_tag(69)%name = "SCI_XGrid"
    input_tag(70)%name = "SCI_XLength"
    input_tag(71)%name = "SCI_XOrigin"
    input_tag(72)%name = "SCI_y"
    input_tag(73)%name = "SCI_YGrid"
    input_tag(74)%name = "SCI_YLength"
    input_tag(75)%name = "SCI_YOrigin"
    input_tag(76)%name = "sweepData"

    ! See above:
    ! create integer variable names for tags and assign index number.
    ! makes chunk code more understandable.

  end subroutine init_input_xml

  subroutine w_begin_tag_a0( luo_saeinp, tag_name )
    integer, intent(inout) :: luo_saeinp      ! output unit number
    character(len=*) :: tag_name
    character(len=indent) :: spaces

    write(luo_saeinp,*) spaces, '<', trim(tag_name), '>'

    indent = indent + INDENT_SPACES
  end subroutine w_begin_tag_a0

  subroutine w_begin_tag_a1( luo_saeinp, tag_name, attrib1, attr1_value )
    integer, intent(inout) :: luo_saeinp      ! output unit number
    character(len=*) :: tag_name
    character(len=*) :: attrib1
    integer :: attr1_value
    character(len=indent) :: spaces

    write(luo_saeinp,*) spaces, '<', trim(tag_name), &
                        ' ', trim(attrib1), '="', attr1_value, '">'

    indent = indent + INDENT_SPACES
  end subroutine w_begin_tag_a1

  subroutine w_begin_tag_a2( luo_saeinp, tag_name, attrib1, attr1_value, attrib2, attr2_value )
    integer, intent(inout) :: luo_saeinp      ! output unit number
    character(len=*) :: tag_name
    character(len=*) :: attrib1
    integer :: attr1_value
    character(len=*) :: attrib2
    integer :: attr2_value
    character(len=indent) :: spaces

    indent = indent + INDENT_SPACES
  end subroutine w_begin_tag_a2

  subroutine w_end_tag( luo_saeinp, tag_name )
    integer, intent(inout) :: luo_saeinp      ! output unit number
    character(len=*) :: tag_name
    character(len=indent) :: spaces

    indent = indent - INDENT_SPACES

    write(luo_saeinp,*) spaces, '<', trim(tag_name), '>'
  end subroutine w_end_tag

end module sweep_io_xml_defs



