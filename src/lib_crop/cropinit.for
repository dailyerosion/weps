!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine cropinit(isr, crop)

      use weps_interface_defs, ignore_me=>cropinit
      use biomaterial, only: biomatter

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
      include 'manage/tcrop.inc'
      include 'crop/gcrop.inc'

!     + + + LOCAL VARIABLE DECLARATIONS + + +
      integer idx

      ! no crop growing at start of simulation
      crop%growth%am0cgf = .false.
      crop%growth%am0cif = .false.

      crop%mass%standstem = 0.0
      crop%mass%standleaf = 0.0
      crop%mass%standstore = 0.0
      crop%mass%flatstem = 0.0
      crop%mass%flatleaf = 0.0
      crop%mass%flatstore = 0.0
      crop%mass%flatrootstore = 0.0
      crop%mass%flatrootfiber = 0.0
      do idx = 1, size(crop%mass%rootstorez)
          crop%mass%stemz(idx) = 0.0
          crop%mass%leafz(idx) = 0.0
          crop%mass%storez(idx) = 0.0
          crop%mass%rootstorez(idx) = 0.0
          crop%mass%rootfiberz(idx) = 0.0
      end do

      acxrow(isr) = 0.0
      crop%geometry%zht = 0.0
      crop%geometry%dstm = 0.0
      crop%geometry%zrtd = 0.0
      crop%growth%dayap = 0
      crop%growth%thucum = 0.0
      crop%growth%trthucum = 0.0
      crop%geometry%grainf = 0.0
      crop%deriv%mbgrootstore = 0.0
      crop%deriv%mbgrootfiber = 0.0
      crop%geometry%xstmrep = 0.0
      crop%growth%fliveleaf = 0.0

      crop%deriv%m = 0.0
      crop%deriv%mst = 0.0
      crop%deriv%mf = 0.0
      crop%deriv%mrt = 0.0

      do idx = 1, size(crop%deriv%mrtz)
          crop%deriv%mrtz(idx) = 0.0
      end do

      crop%deriv%rsai = 0.0
      crop%deriv%rlai = 0.0

      do idx = 1, size(crop%deriv%rsaz)
          crop%deriv%rsaz(idx) = 0.0
          crop%deriv%rlaz(idx) = 0.0
      end do

      crop%deriv%ffcv = 0.0
      crop%deriv%fscv = 0.0
      crop%deriv%ftcv = 0.0

      crop%database%xstm = 0.0
      crop%database%rbc = 1
      crop%database%covfact = 0.0
      crop%database%ck = 0.0

      ! initialize some derived globals for crop global variables
      crop%deriv%fcancov = 0.0
      crop%deriv%rcd = 0.0

!     crop harvest reporting day counters
      cprevrotation(isr) = 1

!     initialize crop yield reporting parameters in case harvest call before planting
      crop%bname = ''
      acynmu(isr) = ''
      acycon(isr) = 1.0
      acywct(isr) = 0.0

!     initialize crop type id to 0 indicating no crop type is growing
      ac0idc(isr) = 0
      crop%database%sla = 0.0
      acdpop(isr) = 0.0

!     initialize row placement to be on the ridge
      ac0rg(isr) = 1
!     initialize harvestable yield fraction flag
      crop%geometry%hyfg = 0

      ! initialize decomp parameters since they are used before a crop is growing
      do idx = 1, size(crop%database%dkrate)
          crop%database%dkrate(idx) = 0.0
      end do
      crop%database%ddsthrsh = 0.0

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
