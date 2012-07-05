!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!
module m_cropxml_type
!
! Data structures for an XML crop record
!
public  :: dump_crop_data
public  :: transfer_crop_data
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
      integer                        :: transf
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

print *, "---dumping CROP data:"

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
print *, "transf: ", cp%transf
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

subroutine transfer_crop_data(crop_data)
type(crop_t), intent(in), target   :: crop_data

include 'p1werm.inc'
include 'c1info.inc'
include 'c1gen.inc'
include 'c1db1.inc'
include 'c1db2.inc'

integer  :: i
type(crop_t), pointer :: cp

!print *, "Transferring CROP data:"

cp => crop_data
ac0nam(1) = trim(cp%cropname)                 !crop name
acdpop(1) = cp%plantpop                       !plant population density
acdmaxshoot(1) = cp%dmaxshoot                 !max number of shoots possible per plant
acbaflg(1) = cp%cbaflag                       !biomass adj flag
acytgt(1) = cp%tgtyield                       !target yield in specified units
acbaf(1) = cp%cbafact                         !crop biomass adj factor
acyraf(1)= cp%cyrafact                        !yield to biomass ratio adj factor
achyfg(1) = cp%hyldflag                       !defines what crop component is yield
acynmu(1) = trim(cp%hyldunits)                !yield units (text string)
acywct(1) = cp%hyldwater                      !water content yield is expressed in for user
acycon(1) = cp%hyconfact                      !unit conversion factor (internal WEPS to user specified units)
ac0idc(1) = cp%idc                            !crop type
acgrf(1) = cp%grf                             !fraction of grain in reproductive biomass
ac0ck(1) = cp%ck                              !canopy light extinction coefficient
acehu0(1) = cp%hui0                           !heat unit index leaf senescence starts
aczmxc(1) = cp%hmx                            !maximum crop height
ac0growdepth(1) = cp%growdepth                !depth of growing point at time of planting
aczmrt(1) = cp%rdmx                            !maximum root depth
actmin(1) = cp%tbas                            !minimum (base) temp for plant growth
actopt(1) = cp%topt                            !optimum temp for plant growth
acthudf(1) = cp%thudf                          !heat units or days to maturity flag
actdtm(1) = cp%dtm                             !days to reach maturity
acthum(1) = cp%thum                            !heat units to reach maturity
ac0fd1(1,1) = cp%frsx1                         !frost damage points
ac0fd2(1,1) = cp%frsx2
ac0fd1(2,1) = cp%frsy1
ac0fd2(2,1) = cp%frsy2
actverndel(1) = cp%verndel                     !thermal delay coeff pre-vernalization
ac0bceff(1) = cp%bceff                         !biomass conversion efficiency
ac0alf(1) = cp%a_lf                            !leaf fraction coeff a
ac0blf(1) = cp%b_lf                            !leaf fraction coeff b
ac0clf(1) = cp%c_lf                            !leaf fraction coeff c
ac0dlf(1) = cp%d_lf                            !leaf fraction coeff d
ac0arp(1) = cp%a_rp                            !reproductive mass fraction coeff a
ac0brp(1) = cp%b_rp                            !reproductive mass fraction coeff b
ac0crp(1) = cp%c_rp                            !reproductive mass fraction coeff c
ac0drp(1) = cp%d_rp                            !reproductive mass fraction coeff d
ac0aht(1) = cp%a_ht                            !crop height coeff a
ac0bht(1) = cp%b_ht                            !crop height coeff b
ac0ssa(1) = cp%ssaa                            !stem silhouette area coeff a
ac0ssb(1) = cp%ssab                            !stem silhouette area coeff b
ac0sla(1) = cp%sla                              !specific leaf area
ac0hue(1) = cp%huie                             !heat unit index at emergence
ac0transf(1) = cp%transf                             !transplant or seed flag
ac0diammax(1) = cp%diammax                           !maximum plant diameter
ac0storeinit(1) = cp%storeinit                       !crop storage root mass initialization
ac0shoot(1) = cp%mshoot                              !mass from root storage required for each regrowth shoot
acfleafstem(1) = cp%leafstem                         !crop leaf to stem mass ratio
acfshoot(1) = cp%fshoot                              !crop ratio of shoot diameter to length
acfleaf2stor(1) = cp%leaf2stor                       !fract of assimilate partitioned to leaf
acfstem2stor(1) = cp%stem2stor                       !fract of assimilate partitioned to stem
acfstor2stor(1) = cp%stor2stor                       !fract of assimilate partitioned to standing storage
acrbc(1) = cp%rbc                                 !crop residue burial class
acdkrate(1,1) = cp%standdk                        !standing residue mass decomp rate
acdkrate(2,1) = cp%surfdk                         !flat residue mass decomp rate
acdkrate(3,1) = cp%burieddk                       !buried residue mass decomp rate
acdkrate(4,1) = cp%rootdk                         !root residue mass decomp rate
acdkrate(5,1) = cp%stemnodk                       !stem residue number decline rate
acxstm(1) = cp%stemdia                               !mature stem diameter (residue)
acddsthrsh(1) = cp%thrddys                           !decomp days required for first stem fall
accovfact(1) = cp%covfact                            !residue cover factor
acresevapa(1) = cp%resevapa                          !coeff a in relation ea/ep = ...
acresevapb(1) = cp%resevapb                          !coeff b in relation ea/ep = ...
acyld_coef(1) = cp%yield_coefficient                 !harvest_residue = acyld_coef(kg/kg) * Yield + acresid_int (kg/m^2)
acresid_int(1) = cp%residue_intercept                !harvest_residue = acyld_coef(kg/kg) * Yield + acresid_int (kg/m^2)
! noparam4 = cp%noparam4                          !Not used in WEPS plant growth submodel
! noparam3 = cp%noparam3                          !Not used in WEPS plant growth submodel
! noparam2 = cp%noparam2                          !Not used in WEPS plant growth submodel
! noparam1 = cp%noparam1                          !Not used in WEPS plant growth submodel
! crop_notes = trim(cp%crop_notes)                !Not used in WEPS plant growth submodel


end subroutine transfer_crop_data

end module m_cropxml_type
