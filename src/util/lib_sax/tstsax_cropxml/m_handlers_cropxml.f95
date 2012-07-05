!$Author$
!$Date$
!$Revision$
!$HeadURL$


module m_handlers_cropxml

use flib_sax
use m_crop_type         ! Data type (crop record structure)

private

! It defines the routines that are called by the XML parser in response
! to particular events.
!
! In this particular example we just print the names of the elements,
! the attribute list, and the content of the pcdata chunks
!
! A module such as this could use "utility routines" to convert pcdata
! to numerical arrays, and to populate specific data structures.
!
public :: begin_element_handler, end_element_handler, pcdata_chunk_handler

logical, private  :: in_cropname = .false., in_param = .false.
logical, private  :: in_name = .false., in_value = .false.

type(crop_t), private, target, save :: crop_data

character(len=80), private, target, save :: param_name
character(len=128), private, target, save :: param_value
character(len=10000), private, target, save :: catstring

! Pointers to make it easier to manage the data
type(crop_t), private, pointer   :: cp

CONTAINS  !=============================================================

!---------------------------------------------------------------------------
subroutine begin_element_handler(name,attributes)
character(len=*), intent(in)   :: name
type(dictionary_t), intent(in) :: attributes

character(len=100)  :: value
integer             :: status

!write(*,*) ">>Begin Element: ", name
!write(*,*) "--- ", len(attributes), " attributes:"
!call print_dict(attributes)

select case(name)

      case ("cropDB")
!            print *, "We are working on a WEPS/MCREW crop record"
      case ("cropname")
           in_cropname = .true.
!           print *, "In cropname"
           cp => crop_data    !init crop pointer to crop data structure
           catstring=""       !need to initialize tmp multi-line string
           cp%crop_notes = "" !need to initialize multi-line variables
      case ("param")
           in_param = .true.
!           print *, "In param"
      case ("name")
           in_name = .true.
!           print *, "In name"
      case ("value")
           in_value = .true.
!           print *, "In value"
end select

end subroutine begin_element_handler

!---------------------------------------------------------------------------
subroutine end_element_handler(name)
character(len=*), intent(in)     :: name

select case(name)

      case ("cropDB")
           print *,"Done working on WEPS/MCREW crop record"
           call dump_crop_data(crop_data)
      case ("cropname")
           in_cropname = .false.
!           print *, "Leaving cropname"
      case ("param")
           in_param = .false.
!           print *, "Leaving param"
      case ("name")
           in_name = .false.
!           print *, "Leaving name"
      case ("value")
           in_value = .false.
!           print *, "Leaving value"
end select

end subroutine end_element_handler

!---------------------------------------------------------------------------
subroutine pcdata_chunk_handler(chunk)
character(len=*), intent(in) :: chunk

    !write(unit=*,fmt="(a)",advance="no") trim(chunk)
    !write(*,*) trim(chunk)

    if (in_cropname .EQV. .true.) then
           cp%cropname = trim(chunk)
!           print *,"Crop name is: ", trim(cp%cropname)
    else if (in_param .EQV. .true. .AND. in_name .EQV. .true.) then
           !Get param name here
           param_name = trim(chunk)
!           print *,"Param name is: ", trim(param_name)
    else if (in_param .EQV. .true. .AND. in_value .EQV. .true.) then
           !Get param value here
           param_value = trim(chunk)
!           print *,"Param value is: ", trim(param_value)

           select case(param_name)

             case ("plantpop")
                  read(param_value,*) cp%plantpop
             case ("dmaxshoot")
                  read(param_value,*) cp%dmaxshoot
             case ("cbaflag")
                  read(param_value,*) cp%cbaflag
             case ("tgtyield")
                  read(param_value,*) cp%tgtyield
             case ("cbafact")
                  read(param_value,*) cp%cbafact
             case ("cyrafact")
                  read(param_value,*) cp%cyrafact
             case ("hyldflag")
                  read(param_value,*) cp%hyldflag
             case ("hyldunits")
                  cp%hyldunits = param_value
             case ("hyldwater")
                  read(param_value,*) cp%hyldwater
             case ("hyconfact")
                  read(param_value,*) cp%hyconfact
             case ("idc")
                  read(param_value,*) cp%idc
             case ("grf")
                  read(param_value,*) cp%grf
             case ("ck")
                  read(param_value,*) cp%ck
             case ("hui0")
                  read(param_value,*) cp%hui0
             case ("hmx")
                  read(param_value,*) cp%hmx
             case ("growdepth")
                  read(param_value,*) cp%growdepth
             case ("rdmx")
                  read(param_value,*) cp%rdmx
             case ("tbas")
                  read(param_value,*) cp%tbas
             case ("topt")
                  read(param_value,*) cp%topt
             case ("thudf")
                  read(param_value,*) cp%thudf
             case ("dtm")
                  read(param_value,*) cp%dtm
             case ("thum")
                  read(param_value,*) cp%thum
             case ("frsx1")
                  read(param_value,*) cp%frsx1
             case ("frsx2")
                  read(param_value,*) cp%frsx2
             case ("frsy1")
                  read(param_value,*) cp%frsy1
             case ("frsy2")
                  read(param_value,*) cp%frsy2
             case ("verndel")
                  read(param_value,*) cp%verndel
             case ("bceff")
                  read(param_value,*) cp%bceff
             case ("a_lf")
                  read(param_value,*) cp%a_lf
             case ("b_lf")
                  read(param_value,*) cp%b_lf
             case ("c_lf")
                  read(param_value,*) cp%c_lf
             case ("d_lf")
                  read(param_value,*) cp%d_lf
             case ("a_rp")
                  read(param_value,*) cp%a_rp
             case ("b_rp")
                  read(param_value,*) cp%b_rp
             case ("c_rp")
                  read(param_value,*) cp%c_rp
             case ("d_rp")
                  read(param_value,*) cp%d_rp
             case ("a_ht")
                  read(param_value,*) cp%a_ht
             case ("b_ht")
                  read(param_value,*) cp%b_ht
             case ("ssaa")
                  read(param_value,*) cp%ssaa
             case ("ssab")
                  read(param_value,*) cp%ssab
             case ("sla")
                  read(param_value,*) cp%sla
             case ("huie")
                  read(param_value,*) cp%huie
             case ("tranf")
                  read(param_value,*) cp%tranf
             case ("diammax")
                  read(param_value,*) cp%diammax
             case ("storeinit")
                  read(param_value,*) cp%storeinit
             case ("mshoot")
                  read(param_value,*) cp%mshoot
             case ("leafstem")
                  read(param_value,*) cp%leafstem
             case ("fshoot")
                  read(param_value,*) cp%fshoot
             case ("leaf2stor")
                  read(param_value,*) cp%leaf2stor
             case ("stem2stor")
                  read(param_value,*) cp%stem2stor
             case ("stor2stor")
                  read(param_value,*) cp%stor2stor
             case ("rbc")
                  read(param_value,*) cp%rbc
             case ("standdk")
                  read(param_value,*) cp%standdk
             case ("surfdk")
                  read(param_value,*) cp%surfdk
             case ("burieddk")
                  read(param_value,*) cp%burieddk
             case ("rootdk")
                  read(param_value,*) cp%rootdk
             case ("stemnodk")
                  read(param_value,*) cp%stemnodk
             case ("stemdia")
                  read(param_value,*) cp%stemdia
             case ("thrddys")
                  read(param_value,*) cp%thrddys
             case ("covfact")
                  read(param_value,*) cp%covfact
             case ("resevapa")
                  read(param_value,*) cp%resevapa
             case ("resevapb")
                  read(param_value,*) cp%resevapb
             case ("yield_coefficient")
                  read(param_value,*) cp%yield_coefficient
             case ("residue_intercept")
                  read(param_value,*) cp%residue_intercept
             case ("noparam4")
                  read(param_value,*) cp%noparam4
             case ("noparam3")
                  read(param_value,*) cp%noparam3
             case ("noparam2")
                  read(param_value,*) cp%noparam2
             case ("noparam1")
                  read(param_value,*) cp%noparam1
             case ("crop_notes")
                  !Deal with multi-line string variable
                  catstring = trim(cp%crop_notes) // param_value
                  cp%crop_notes = trim(catstring)

           end select

    end if

end subroutine pcdata_chunk_handler
!---------------------------------------------------------------------------


end module m_handlers_cropxml
