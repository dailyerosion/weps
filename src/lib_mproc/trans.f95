!$Author$
!$Date$
!$Revision$
!$HeadURL$
!
      subroutine trans(                                                 &
     &           bcmstandstem, bcmstandleaf, bcmstandstore,             &
     &           bcmflatstem, bcmflatleaf, bcmflatstore,                &
     &           bcmflatrootstore, bcmflatrootfiber,                    &
     &           bcmbgstemz, bcmbgleafz, bcmbgstorez,                   &
     &           bcmbgrootstorez, bcmbgrootfiberz,                      &
     &           bczht, bcdstm, bcxstmrep, bcgrainf,                    &
     &         bc0nam, bcxstm, bcrbc, bc0sla, bc0ck,                    &
     &         bcdkrate, bccovfact, bcddsthrsh, bchyfg,                 &
     &         bcresevapa, bcresevapb,                                  &
     &         nslay, residue )

!     + + + PURPOSE + + +

!     This subroutine performs the biomass manipulation of transferring
!     biomass.  Transfer of biomass is performed on both the standing
!     or above ground biomass and the root biomass.  The transfer is
!     from the "temporary" crop pool to the decomp biomass pools
!     (when called from within "doeffect".

!     + + + KEYWORDS + + +
!     transfer, biomass manipulation

      use biomaterial, only: biomatter

!     + + +   ARGUMENT DECLARATIONS + + +
      type(biomatter), dimension(:), intent(inout) :: residue

      include 'p1werm.inc'

!     + + + ARGUMENT DECLARATIONS + + +
!
      real             bcmstandstem !added state
      real             bcmstandleaf !added state
      real             bcmstandstore !added state

      real             bcmflatstem !added state
      real             bcmflatleaf !added state
      real             bcmflatstore !added state

      real             bcmflatrootstore !added state
      real             bcmflatrootfiber !added state

      real             bcmbgstemz(mnsz) !added state
      real             bcmbgleafz(mnsz) !added state
      real             bcmbgstorez(mnsz) !added state

      real             bcmbgrootstorez(mnsz) !added state
      real             bcmbgrootfiberz(mnsz) !added state

      real             bczht  !changed from tczht state
      real             bcdstm !changed from tcdstm state
      real             bcxstmrep !changed from tcxstmrep state
      real             bcgrainf !added state

      ! present crop
      character*(80)  bc0nam
      real       bcxstm
      integer    bcrbc
      real       bc0sla
      real       bc0ck

      real       bcdkrate(mndk)
      real       bccovfact
      real       bcddsthrsh
      integer    bchyfg

      real       bcresevapa 
      real       bcresevapb

      integer    nslay

!     + + + ARGUMENT DEFINITIONS + + +
!
!     nslay       - number of soil layers
!
!     + + + ACCESSED COMMON BLOCK VARIABLE DEFINITIONS + + +
!
!     mnbpls        - max number of decomposition pools (currently=3)
!     mnsz          - max number of soil layers
!     mndk          - max number of decay coefficients
!
!     + + + PARAMETERS + + +
!
!     + + + LOCAL VARIABLES + + +
!
      integer lay, ip, idk
!
!     + + + LOCAL VARIABLE DEFINITIONS + + +
!
!     lay        - soil layer index
!     ip         - decomp pool index
!     idk        - dkrate components index
!     idx        - crop mass by height index

!     + + + END SPECIFICATIONS + + +

! transfer standing residue height and diameter, and grain fraction
      do ip = mnbpls,2,-1
         residue(ip)%geometry%zht = residue(ip-1)%geometry%zht
         residue(ip)%database%xstm = residue(ip-1)%database%xstm
         residue(ip)%geometry%xstmrep = residue(ip-1)%geometry%xstmrep
         residue(ip)%geometry%grainf = residue(ip-1)%geometry%grainf
         residue(ip)%geometry%hyfg = residue(ip-1)%geometry%hyfg
      end do
      residue(1)%geometry%zht = bczht
      residue(1)%database%xstm = bcxstm
      residue(1)%geometry%xstmrep = bcxstmrep
      residue(1)%geometry%grainf = bcgrainf
      residue(1)%geometry%hyfg = bchyfg
      bczht = 0.0
      bcgrainf = 0.0
      ! do not zero out, this still applies if the crop has not been killed
      ! bcxstm = 0.0
      ! do not zero out, this still applies until it is recalculated
      ! bcxstmrep = 0.0
      ! bchyfg = 0.0

! transfer stem numbers
      residue(mnbpls)%geometry%dstm = residue(mnbpls)%geometry%dstm + residue(mnbpls-1)%geometry%dstm
      do ip = mnbpls-1,2,-1
         residue(ip)%geometry%dstm = residue(ip-1)%geometry%dstm
      end do
      residue(1)%geometry%dstm = bcdstm
      bcdstm = 0.0

      ! add next to last pool biomass to last pool biomass
      ! surface pools
      residue(mnbpls)%mass%standstem = residue(mnbpls)%mass%standstem + residue(mnbpls-1)%mass%standstem
      residue(mnbpls)%mass%standleaf = residue(mnbpls)%mass%standleaf                       &
     &                     + residue(mnbpls-1)%mass%standleaf
      residue(mnbpls)%mass%standstore = residue(mnbpls)%mass%standstore                     &
     &                      + residue(mnbpls-1)%mass%standstore
      residue(mnbpls)%mass%flatstem = residue(mnbpls)%mass%flatstem + residue(mnbpls-1)%mass%flatstem
      residue(mnbpls)%mass%flatleaf = residue(mnbpls)%mass%flatleaf + residue(mnbpls-1)%mass%flatleaf
      residue(mnbpls)%mass%flatstore = residue(mnbpls)%mass%flatstore+residue(mnbpls-1)%mass%flatstore
      residue(mnbpls)%mass%flatrootstore = residue(mnbpls)%mass%flatrootstore               &
     &                         + residue(mnbpls-1)%mass%flatrootstore
      residue(mnbpls)%mass%flatrootfiber = residue(mnbpls)%mass%flatrootfiber               &
     &                         + residue(mnbpls-1)%mass%flatrootfiber
      ! below ground by layer
      do lay = 1, nslay
          residue(mnbpls)%mass%stemz(lay) = residue(mnbpls)%mass%stemz(lay) + residue(mnbpls-1)%mass%stemz(lay)
          residue(mnbpls)%mass%leafz(lay) = residue(mnbpls)%mass%leafz(lay) + residue(mnbpls-1)%mass%leafz(lay)
          residue(mnbpls)%mass%storez(lay) = residue(mnbpls)%mass%storez(lay) + residue(mnbpls-1)%mass%storez(lay)
          residue(mnbpls)%mass%rootstorez(lay) = residue(mnbpls)%mass%rootstorez(lay) + residue(mnbpls-1)%mass%rootstorez(lay)
          residue(mnbpls)%mass%rootfiberz(lay) = residue(mnbpls)%mass%rootfiberz(lay) + residue(mnbpls-1)%mass%rootfiberz(lay)
      end do

      ! transfer biomass down one level in pools
      do ip = mnbpls-1,2,-1
          ! surface pools
          residue(ip)%mass%standstem = residue(ip-1)%mass%standstem
          residue(ip)%mass%standleaf = residue(ip-1)%mass%standleaf
          residue(ip)%mass%standstore = residue(ip-1)%mass%standstore
          residue(ip)%mass%flatstem = residue(ip-1)%mass%flatstem
          residue(ip)%mass%flatleaf = residue(ip-1)%mass%flatleaf
          residue(ip)%mass%flatstore = residue(ip-1)%mass%flatstore
          residue(ip)%mass%flatrootstore = residue(ip-1)%mass%flatrootstore
          residue(ip)%mass%flatrootfiber = residue(ip-1)%mass%flatrootfiber
          ! below ground by layer
          do lay = 1, nslay
              residue(ip)%mass%stemz(lay) = residue(ip-1)%mass%stemz(lay)
              residue(ip)%mass%leafz(lay) = residue(ip-1)%mass%leafz(lay)
              residue(ip)%mass%storez(lay) = residue(ip-1)%mass%storez(lay)
              residue(ip)%mass%rootstorez(lay) = residue(ip-1)%mass%rootstorez(lay)
              residue(ip)%mass%rootfiberz(lay) = residue(ip-1)%mass%rootfiberz(lay)
          end do
      end do

      ! transfer incoming biomass into first pool
      ! surface pools
      residue(1)%mass%standstem =  bcmstandstem
      residue(1)%mass%standleaf = bcmstandleaf
      residue(1)%mass%standstore = bcmstandstore
      residue(1)%mass%flatstem = bcmflatstem
      residue(1)%mass%flatleaf = bcmflatleaf
      residue(1)%mass%flatstore = bcmflatstore
      residue(1)%mass%flatrootstore = bcmflatrootstore
      residue(1)%mass%flatrootfiber = bcmflatrootfiber
      ! below ground by layer
      do lay = 1, nslay
          residue(1)%mass%stemz(lay) = bcmbgstemz(lay)
          residue(1)%mass%leafz(lay) = bcmbgleafz(lay)
          residue(1)%mass%storez(lay) = bcmbgstorez(lay)
          residue(1)%mass%rootstorez(lay) = bcmbgrootstorez(lay)
          residue(1)%mass%rootfiberz(lay) = bcmbgrootfiberz(lay)
      end do

      ! zero out the incoming biomass
      ! surface pools
      bcmstandstem = 0.0
      bcmstandleaf = 0.0
      bcmstandstore = 0.0
      bcmflatstem = 0.0
      bcmflatleaf = 0.0
      bcmflatstore = 0.0
      bcmflatrootstore = 0.0
      bcmflatrootfiber = 0.0
      ! below ground by layer
      do lay = 1, nslay
          bcmbgstemz(lay) = 0.0
          bcmbgleafz(lay) = 0.0
          bcmbgstorez(lay) = 0.0
          bcmbgrootstorez(lay) = 0.0
          bcmbgrootfiberz(lay) = 0.0
      end do

! transfer CUMM DDAYS for s standing, f flat, and g below ground pools
      do ip = mnbpls,2,-1
         residue(ip)%decomp%resday = residue(ip-1)%decomp%resday
         residue(ip)%decomp%resyear = residue(ip-1)%decomp%resyear
         residue(ip)%decomp%cumdds = residue(ip-1)%decomp%cumdds
         residue(ip)%decomp%cumddf = residue(ip-1)%decomp%cumddf
         do lay = 1, nslay
            residue(ip)%decomp%cumddg(lay) = residue(ip-1)%decomp%cumddg(lay)
         end do
      end do
      residue(1)%decomp%resday = 0
      residue(1)%decomp%resyear = residue(2)%decomp%resyear + 1
      residue(1)%decomp%cumdds = 0.0    ! reset cummdds to zero
      residue(1)%decomp%cumddf = 0.0    ! reset cummddf to zero
      do lay = 1, nslay
         residue(1)%decomp%cumddg(lay) = 0.0    ! reset cummddg to zero
      end do


! transfer decay rates for standing, flat, below ground, roots,
! stem_no pools, and harvestable yield flag
      do idk = 1,mndk
          do ip = mnbpls,2,-1
              residue(ip)%database%dkrate(idk) = residue(ip-1)%database%dkrate(idk)
          end do
          residue(1)%database%dkrate(idk) = bcdkrate(idk)
          ! do not zero out, this still applies if the crop has not been killed
          ! bcdkrate(idk) = 0.0
      end do

      do ip = mnbpls,2,-1
          residue(ip)%bname = residue(ip-1)%bname
          residue(ip)%database%rbc = residue(ip-1)%database%rbc
          residue(ip)%database%sla = residue(ip-1)%database%sla
          residue(ip)%database%ck = residue(ip-1)%database%ck
          residue(ip)%database%covfact = residue(ip-1)%database%covfact
          residue(ip)%database%ddsthrsh = residue(ip-1)%database%ddsthrsh
          residue(ip)%database%resevapa = residue(ip-1)%database%resevapa
          residue(ip)%database%resevapb = residue(ip-1)%database%resevapb
      end do
      residue(1)%bname = bc0nam
      residue(1)%database%rbc = bcrbc
      residue(1)%database%sla = bc0sla
      residue(1)%database%ck = bc0ck
      residue(1)%database%covfact = bccovfact
      residue(1)%database%ddsthrsh = bcddsthrsh
      residue(1)%database%resevapa = bcresevapa
      residue(1)%database%resevapb = bcresevapb
      ! do not zero out, this still applies if the crop has not been killed
      ! bc0nam = ""
      ! bcrbc = 0
      ! bc0sla = 0.0
      ! bccovfact = 0.0
      ! bcddsthrsh = 0.0

      return
      end
