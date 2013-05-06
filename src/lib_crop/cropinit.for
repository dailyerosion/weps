!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine cropinit(isr, crop)

      use weps_interface_defs
      use biomaterial, only: biomatter, biototal

!     + + + ARGUMENT DECLARATIONS + + +
      integer isr
      type(biomatter), intent(inout) :: crop    ! structure containing full crop description

!     + + + PARAMETERS AND COMMON BLOCKS + + +
      include 'p1werm.inc'
      include 'c1info.inc'
      include 'c1gen.inc'
      include 'c1report.inc'
      include 'c1db1.inc'
      include 'c1db2.inc'
      include 'c1glob.inc'
      include 's1layr.inc'
      include 's1sgeo.inc'
      include 'manage/tcrop.inc'
      include 'crop/gcrop.inc'

!     + + + LOCAL VARIABLE DECLARATIONS + + +
      integer idx

      ! no crop growing at start of simulation
      crop%growth%am0cgf = .false.
      crop%growth%am0cif = .false.

      acmstandstem(isr) = 0.0
      acmstandleaf(isr) = 0.0
      acmstandstore(isr) = 0.0
      acmflatstem(isr) = 0.0
      acmflatleaf(isr) = 0.0
      acmflatstore(isr) = 0.0

      do idx = 1, mnsz
          acmrootstorez(idx,isr) = 0.0
          acmrootfiberz(idx,isr) = 0.0
          acmbgstemz(idx,isr) = 0.0
      end do

      aczht(isr) = 0.0
      acdstm(isr) = 0.0
      aczrtd(isr) = 0.0
      acdayap(isr) = 0
      acthucum(isr) = 0.0
      actrthucum(isr) = 0.0
      acgrainf(isr) = 0.0
      acmrootstore(isr) = 0.0
      acmrootfiber(isr) = 0.0
      acxstmrep(isr) = 0.0
      acfliveleaf(isr) = 0.0

      acm(isr) = 0.0
      acmst(isr) = 0.0
      acmf(isr) = 0.0
      acmrt(isr) = 0.0

      do idx = 1, mnsz
          acmrtz(idx,isr) = 0.0
      end do

      acrsai(isr) = 0.0
      acrlai(isr) = 0.0

      do idx = 1, mncz
          acrsaz(idx,isr) = 0.0
          acrlaz(idx,isr) = 0.0
      end do

      acffcv(isr) = 0.0
      acfscv(isr) = 0.0
      acftcv(isr) = 0.0

      acxstm(isr) = 0.0
      acrbc(isr) = 1
      accovfact(isr) = 0.0
      ac0ck(isr) = 0.0

      ! initialize some derived globals for crop global variables
      acfcancov(isr) = 0.0
      acrcd(isr) = 0.0

!     crop harvest reporting day counters
      cprevrotation(isr) = 1

!     initialize crop yield reporting parameters in case harvest call before planting
      ac0nam(isr) = ''
      acynmu(isr) = ''
      acycon(isr) = 1.0
      acywct(isr) = 0.0

!     initialize crop type id to 0 indicating no crop type is growing
      ac0idc(isr) = 0
      ac0sla(isr) = 0.0
      acdpop(isr) = 0.0

!     initialize row placement to be on the ridge
      ac0rg(isr) = 1
!     initialize harvestable yield fraction flag
      achyfg(isr) = 0

      ! initialize decomp parameters since they are used before a crop is growing
      do idx = 1, mndk
          acdkrate(idx,isr) = 0.0
      end do
      acddsthrsh(isr) = 0.0

      ! temporary crop 
      atmstandstem(isr) = 0.0
      atmstandleaf(isr) = 0.0
      atmstandstore(isr) = 0.0
      atmflatstem(isr) = 0.0
      atmflatleaf(isr) = 0.0
      atmflatstore(isr) = 0.0
      atmflatrootstore(isr) = 0.0
      atmflatrootfiber(isr) = 0.0

      do idx = 1, mnsz
          atmbgstemz(idx,isr) = 0.0
          atmbgleafz(idx,isr) = 0.0
          atmbgstorez(idx,isr) = 0.0
          atmbgrootstorez(idx,isr) = 0.0
          atmbgrootfiberz(idx,isr) = 0.0
      end do

      atzht(isr) = 0.0
      atdstm(isr) = 0.0
      atxstmrep(isr) = 0.0
      atzrtd(isr) = 0.0
      atgrainf(isr) = 0.0

      ! temporary crop 
      agmstandstem(isr) = 0.0
      agmstandleaf(isr) = 0.0
      agmstandstore(isr) = 0.0
      agmflatstem(isr) = 0.0
      agmflatleaf(isr) = 0.0
      agmflatstore(isr) = 0.0
      agmflatrootstore(isr) = 0.0
      agmflatrootfiber(isr) = 0.0

      do idx = 1, mnsz
          agmbgstemz(idx,isr) = 0.0
          agmbgleafz(idx,isr) = 0.0
          agmbgstorez(idx,isr) = 0.0
          agmbgrootstorez(idx,isr) = 0.0
          agmbgrootfiberz(idx,isr) = 0.0
      end do

      agzht(isr) = 0.0
      agdstm(isr) = 0.0
      agxstmrep(isr) = 0.0
      agzrtd(isr) = 0.0
      aggrainf(isr) = 0.0

      ! values that need initialization for cdbug calls (before initial crop entry)
      actdtm(isr) = 0

      ac0shoot(isr) = 0.0

      return
      end
