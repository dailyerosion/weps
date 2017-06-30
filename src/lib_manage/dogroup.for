!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine   dogroup (soil, manFile)

!     + + + PURPOSE + + +
!     Dogroup reads in any coefficients associated with the group of
!     processes. 

!     + + + KEYWORDS + + +
!     tillage, operation, management

      use weps_interface_defs, ignore_me=>dogroup
      use manage_data_struct_defs, only: lastoper, man_file_struct
      use soil_data_struct_defs, only: soil_def
      use manage_data_struct_mod, only: getManVal

!     + + + PARAMETERS AND COMMON BLOCKS + + +
      include 'p1werm.inc'
      include 'm1flag.inc'
      include 'manage/mproc.inc'

!     + + + ARGUMENT DECLARATIONS + + +
      type(soil_def), intent(in) :: soil  ! soil for this subregion
      type(man_file_struct), intent(in) :: manFile

!     + + + LOCAL VARIABLES + + +
      integer :: sr  ! the subregion being processed

!     + + + DATA INITIALIZATIONS + + +
      sr = manFile%isub

!     + + + END SPECIFICATIONS + + +

      lastoper(sr)%grcode = manFile%grp%grpType
      lastoper(sr)%grname = manFile%grp%grpName

      select case (lastoper(sr)%grcode)

      case (1)  ! tillage group
        ! read tillage depth, intensity and area
        call getManVal(manFile%grp, 'gtdepth', tdepth)
        call getManVal(manFile%grp, 'gtilint', ti)
        call getManVal(manFile%grp, 'gtilArea', fracarea)
        call getManVal(manFile%grp, 'gtstddepth', tstddepth)
        call getManVal(manFile%grp, 'gtmindepth', tmindepth)
        call getManVal(manFile%grp, 'gtmaxdepth', tmaxdepth)

        tlayer = tillay(tdepth, soil%aszlyt, soil%nslay)

      case (2)  ! biomass manipulation group
        ! read biomass area affected
        call getManVal(manFile%grp, 'gbioarea', fracarea)

      case (3) ! grow group
        ! read crop name
        call getManVal(manFile%grp, 'gcropname', cropname)

      case (4) ! ammend group
        ! read amendment name
        call getManVal(manFile%grp, 'gamdname', amdname)

      case default
        write(0, *) 'Invalid Group: ', lastoper(sr)%grcode,             &
     &                                 manFile%grp%grpName
        call exit (1)

      end select

      return

      end
