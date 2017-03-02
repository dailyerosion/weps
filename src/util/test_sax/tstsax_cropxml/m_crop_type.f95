!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!
module m_crop_type
!
! Data structures for an XML crop record
!
public  :: dump_crop_data
!
!-----------------------------------------------------------
      
type, public :: crop_t
!
!     Crop record
!
      character(len=1024)            :: cropname
      real                           :: plantpop
      real                           :: dmaxshoot
      integer                        :: cbaflag
      real                           :: tgtyield
      real                           :: cbafact
      real                           :: cyrafact
      integer                        :: hyldflag
      character(len=80)              :: hyldunits
      real                           :: hyldwater
      real                           :: hyconfact
      integer                        :: idc
      real                           :: grf
      real                           :: ck
      real                           :: hui0
      real                           :: hmx
      real                           :: growdepth
      real                           :: rdmx
      real                           :: tbas
      real                           :: topt
      integer                        :: thudf
      integer                        :: dtm
      real                           :: thum
      real                           :: frsx1
      real                           :: frsx2
      real                           :: frsy1
      real                           :: frsy2
      real                           :: verndel
      real                           :: bceff
      real                           :: a_lf
      real                           :: b_lf
      real                           :: c_lf
      real                           :: d_lf
      real                           :: a_rp
      real                           :: b_rp
      real                           :: c_rp
      real                           :: d_rp
      real                           :: a_ht
      real                           :: b_ht
      real                           :: ssaa
      real                           :: ssab
      real                           :: sla
      real                           :: huie
      real                           :: tranf
      real                           :: diammax
      real                           :: storeinit
      real                           :: mshoot
      real                           :: leafstem
      real                           :: fshoot
      real                           :: leaf2stor
      real                           :: stem2stor
      real                           :: stor2stor
      integer                        :: rbc
      real                           :: standdk
      real                           :: surfdk
      real                           :: burieddk
      real                           :: rootdk
      real                           :: stemnodk
      real                           :: stemdia
      real                           :: thrddys
      real                           :: covfact
      real                           :: resevapa
      real                           :: resevapb
      real                           :: yield_coefficient
      real                           :: residue_intercept
      real                           :: noparam4
      real                           :: noparam3
      real                           :: noparam2
      real                           :: noparam1
      character(len=10000)           :: crop_notes
end type crop_t      

CONTAINS !===============================================

subroutine dump_crop_data(crop_data)
type(crop_t), intent(in), target   :: crop_data

integer  :: i
type(crop_t), pointer :: cp

print *, "---CROP data:"

cp => crop_data
print *, "cropname: ", trim(cp%cropname)
print *, "plantpop: ", cp%plantpop
print *, "dmaxshoot: ", cp%dmaxshoot
print *, "cbaflag: ", cp%cbaflag
print *, "tgtyield: ", cp%tgtyield
print *, "cbafact: ", cp%cbafact
print *, "cyrafact: ", cp%cyrafact
print *, "hyldflag: ", cp%hyldflag
print *, "hyldunits: ", trim(cp%hyldunits)
print *, "hyldwater: ", cp%hyldwater
print *, "hyconfact: ", cp%hyconfact
print *, "idc: ", cp%idc
print *, "grf: ", cp%grf
print *, "ck: ", cp%ck
print *, "hui0: ", cp%hui0
print *, "hmx: ", cp%hmx
print *, "growdepth: ", cp%growdepth
print *, "rdmx: ", cp%rdmx
print *, "tbas: ", cp%tbas
print *, "topt: ", cp%topt
print *, "thudf: ", cp%thudf
print *, "dtm: ", cp%dtm
print *, "thum: ", cp%thum
print *, "frsx1: ", cp%frsx1
print *, "frsx2: ", cp%frsx2
print *, "frsy1: ", cp%frsy1
print *, "frsy2: ", cp%frsy2
print *, "verndel: ", cp%verndel
print *, "bceff: ", cp%bceff
print *, "a_lf: ", cp%a_lf
print *, "b_lf: ", cp%b_lf
print *, "c_lf: ", cp%c_lf
print *, "d_lf: ", cp%d_lf
print *, "a_rp: ", cp%a_rp
print *, "b_rp: ", cp%b_rp
print *, "c_rp: ", cp%c_rp
print *, "d_rp: ", cp%d_rp
print *, "a_ht: ", cp%a_ht
print *, "b_ht: ", cp%b_ht
print *, "ssaa: ", cp%ssaa
print *, "ssab: ", cp%ssab
print *, "sla: ", cp%sla
print *, "huie: ", cp%huie
print *, "tranf: ", cp%tranf
print *, "diammax: ", cp%diammax
print *, "storeinit: ", cp%storeinit
print *, "mshoot: ", cp%mshoot
print *, "leafstem: ", cp%leafstem
print *, "fshoot: ", cp%fshoot
print *, "leaf2stor: ", cp%leaf2stor
print *, "stem2stor: ", cp%stem2stor
print *, "stor2stor: ", cp%stor2stor
print *, "rbc: ", cp%rbc
print *, "standdk: ", cp%standdk
print *, "surfdk: ", cp%surfdk
print *, "burieddk: ", cp%burieddk
print *, "rootdk: ", cp%rootdk
print *, "stemnodk: ", cp%stemnodk
print *, "stemdia: ", cp%stemdia
print *, "thrddys: ", cp%thrddys
print *, "covfact: ", cp%covfact
print *, "resevapa: ", cp%resevapa
print *, "resevapb: ", cp%resevapb
print *, "yield_coefficient: ", cp%yield_coefficient
print *, "residue_intercept: ", cp%residue_intercept
print *, "noparam4: ", cp%noparam4
print *, "noparam3: ", cp%noparam3
print *, "noparam2: ", cp%noparam2
print *, "noparam1: ", cp%noparam1
print *, "crop_notes: ", trim(cp%crop_notes)


end subroutine dump_crop_data

end module m_crop_type
