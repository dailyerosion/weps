!
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
     &           bdmstandstem, bdmstandleaf, bdmstandstore,             &
     &           bdmflatstem, bdmflatleaf, bdmflatstore,                &
     &           bdmflatrootstore, bdmflatrootfiber,                    &
     &           bdmbgstemz, bdmbgleafz, bdmbgstorez,                   &
     &           bdmbgrootstorez, bdmbgrootfiberz,                      &
     &           bdzht, bddstm, bdxstmrep, bdgrainf,                    &
     &         bc0nam, bcxstm, bcrbc, bc0sla, bc0ck,                    &
     &         bcdkrate, bccovfact, bcddsthrsh, bchyfg,                 &
     &         bcresevapa, bcresevapb,                                  &
     &         bd0nam, bdxstm, bdrbc, bd0sla, bd0ck,                    &
     &         bdkrate, bcovfact, bddsthrsh, bdhyfg,                    &
     &         bdresevapa, bdresevapb,                                  &
     &         bcumdds, bcumddf, bcumddg,                               &
     &         nslay )

!     + + + PURPOSE + + +

!     This subroutine performs the biomass manipulation of transferring
!     biomass.  Transfer of biomass is performed on both the standing
!     or above ground biomass and the root biomass.  The transfer is
!     from the "temporary" crop pool to the decomp biomass pools
!     (when called from within "doeffect".

!     + + + KEYWORDS + + +
!     transfer, biomass manipulation

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

      real             bdmstandstem(mnbpls) !added state
      real             bdmstandleaf(mnbpls) !added state
      real             bdmstandstore(mnbpls) !added state

      real             bdmflatstem(mnbpls) !added state
      real             bdmflatleaf(mnbpls) !added state
      real             bdmflatstore(mnbpls) !added state

      real             bdmflatrootstore(mnbpls) !added state
      real             bdmflatrootfiber(mnbpls) !added state

      real             bdmbgstemz(mnsz,mnbpls) !added state
      real             bdmbgleafz(mnsz,mnbpls) !added state
      real             bdmbgstorez(mnsz,mnbpls) !added state

      real             bdmbgrootstorez(mnsz,mnbpls) !added state
      real             bdmbgrootfiberz(mnsz,mnbpls) !added state

      real             bdzht(mnbpls) !state
      real             bddstm(mnbpls) !state
      real             bdxstmrep(mnbpls) !state
      real             bdgrainf(mnbpls) !added state

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

      ! decompostion
      character*(80)  bd0nam(mnbpls)
      real       bdxstm(mnbpls)
      integer    bdrbc(mnbpls)
      real       bd0sla(mnbpls)
      real       bd0ck(mnbpls)

      real       bdkrate(mndk,mnbpls)
      real       bcovfact(mnbpls)
      real       bddsthrsh(mnbpls)
      integer    bdhyfg(mnbpls)

      real       bdresevapa(mnbpls)
      real       bdresevapb(mnbpls)

      real       bcumdds(mnbpls)
      real       bcumddf(mnbpls)
      real       bcumddg(mnsz,mnbpls)

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
         bdzht(ip) = bdzht(ip-1)
         bdxstm(ip) = bdxstm(ip-1)
         bdxstmrep(ip) = bdxstmrep(ip-1)
         bdgrainf(ip) = bdgrainf(ip-1)
         bdhyfg(ip) = bdhyfg(ip-1)
      end do
      bdzht(1) = bczht
      bdxstm(1) = bcxstm
      bdxstmrep(1) = bcxstmrep
      bdgrainf(1) = bcgrainf
      bczht = 0.0
      bcgrainf = 0.0
      ! do not zero out, this still applies if the crop has not been killed
      ! bcxstm = 0.0
      ! do not zero out, this still applies until it is recalculated
      ! bcxstmrep = 0.0

! transfer stem numbers
      bddstm(mnbpls) = bddstm(mnbpls) + bddstm(mnbpls-1)
      do ip = mnbpls-1,2,-1
         bddstm(ip) = bddstm(ip-1)
      end do
      bddstm(1) = bcdstm
      bcdstm = 0.0

      ! add next to last pool biomass to last pool biomass
      ! surface pools
      bdmstandstem(mnbpls) = bdmstandstem(mnbpls)                       &
     &                     + bdmstandstem(mnbpls-1)
      bdmstandleaf(mnbpls) = bdmstandleaf(mnbpls)                       &
     &                     + bdmstandleaf(mnbpls-1)
      bdmstandstore(mnbpls) = bdmstandstore(mnbpls)                     &
     &                      + bdmstandstore(mnbpls-1)
      bdmflatstem(mnbpls) = bdmflatstem(mnbpls) + bdmflatstem(mnbpls-1)
      bdmflatleaf(mnbpls) = bdmflatleaf(mnbpls) + bdmflatleaf(mnbpls-1)
      bdmflatstore(mnbpls) = bdmflatstore(mnbpls)+bdmflatstore(mnbpls-1)
      bdmflatrootstore(mnbpls) = bdmflatrootstore(mnbpls)               &
     &                         + bdmflatrootstore(mnbpls-1)
      bdmflatrootfiber(mnbpls) = bdmflatrootfiber(mnbpls)               &
     &                         + bdmflatrootfiber(mnbpls-1)
      ! below ground by layer
      do lay = 1, nslay
          bdmbgstemz(lay,mnbpls) = bdmbgstemz(lay,mnbpls)               &
     &                           + bdmbgstemz(lay,mnbpls-1)
          bdmbgleafz(lay,mnbpls) = bdmbgleafz(lay,mnbpls)               &
     &                           + bdmbgleafz(lay,mnbpls-1)
          bdmbgstorez(lay,mnbpls) = bdmbgstorez(lay,mnbpls)             &
     &                            + bdmbgstorez(lay,mnbpls-1)
          bdmbgrootstorez(lay,mnbpls) = bdmbgrootstorez(lay,mnbpls)     &
     &                                + bdmbgrootstorez(lay,mnbpls-1)
          bdmbgrootfiberz(lay,mnbpls) = bdmbgrootfiberz(lay,mnbpls)     &
     &                                + bdmbgrootfiberz(lay,mnbpls-1)
      end do

      ! transfer biomass down one level in pools
      do ip = mnbpls-1,2,-1
          ! surface pools
          bdmstandstem(ip) = bdmstandstem(ip-1)
          bdmstandleaf(ip) = bdmstandleaf(ip-1)
          bdmstandstore(ip) = bdmstandstore(ip-1)
          bdmflatstem(ip) = bdmflatstem(ip-1)
          bdmflatleaf(ip) = bdmflatleaf(ip-1)
          bdmflatstore(ip) = bdmflatstore(ip-1)
          bdmflatrootstore(ip) = bdmflatrootstore(ip-1)
          bdmflatrootfiber(ip) = bdmflatrootfiber(ip-1)
          ! below ground by layer
          do lay = 1, nslay
              bdmbgstemz(lay,ip) = bdmbgstemz(lay,ip-1)
              bdmbgleafz(lay,ip) = bdmbgleafz(lay,ip-1)
              bdmbgstorez(lay,ip) = bdmbgstorez(lay,ip-1)
              bdmbgrootstorez(lay,ip) = bdmbgrootstorez(lay,ip-1)
              bdmbgrootfiberz(lay,ip) = bdmbgrootfiberz(lay,ip-1)
          end do
      end do

      ! transfer incoming biomass into first pool
      ! surface pools
      bdmstandstem(1) =  bcmstandstem
      bdmstandleaf(1) = bcmstandleaf
      bdmstandstore(1) = bcmstandstore
      bdmflatstem(1) = bcmflatstem
      bdmflatleaf(1) = bcmflatleaf
      bdmflatstore(1) = bcmflatstore
      bdmflatrootstore(1) = bcmflatrootstore
      bdmflatrootfiber(1) = bcmflatrootfiber
      ! below ground by layer
      do lay = 1, nslay
          bdmbgstemz(lay,1) = bcmbgstemz(lay)
          bdmbgleafz(lay,1) = bcmbgleafz(lay)
          bdmbgstorez(lay,1) = bcmbgstorez(lay)
          bdmbgrootstorez(lay,1) = bcmbgrootstorez(lay)
          bdmbgrootfiberz(lay,1) = bcmbgrootfiberz(lay)
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
         bcumdds(ip) = bcumdds(ip-1)
         bcumddf(ip) = bcumddf(ip-1)
         do lay = 1, nslay
            bcumddg(lay,ip) = bcumddg(lay,ip-1)
         end do
      end do
      bcumdds(1) = 0.0    ! reset cummdds to zero
      bcumddf(1) = 0.0    ! reset cummddf to zero
      do lay = 1, nslay
         bcumddg(lay,1) = 0.0    ! reset cummddg to zero
      end do


! transfer decay rates for standing, flat, below ground, roots,
! stem_no pools, and harvestable yield flag
      do idk = 1,mndk
          do ip = mnbpls,2,-1
              bdkrate(idk,ip) = bdkrate(idk,ip-1)
          end do
          bdkrate(idk,1) = bcdkrate(idk)
          ! do not zero out, this still applies if the crop has not been killed
          ! bcdkrate(idk) = 0.0
      end do

      do ip = mnbpls,2,-1
          bd0nam(ip) = bd0nam(ip-1)
          bdrbc(ip) = bdrbc(ip-1)
          bd0sla(ip) = bd0sla(ip-1)
          bd0ck(ip) = bd0ck(ip-1)
          bcovfact(ip) = bcovfact(ip-1)
          bddsthrsh(ip) = bddsthrsh(ip-1)
          bdhyfg(ip) = bdhyfg(ip-1)
          bdresevapa(ip) = bdresevapa(ip-1)
          bdresevapb(ip) = bdresevapb(ip-1)
      end do
      bd0nam(1) = bc0nam
      bdrbc(1) = bcrbc
      bd0sla(1) = bc0sla
      bd0ck(1) = bc0ck
      bcovfact(1) = bccovfact
      bddsthrsh(1) = bcddsthrsh
      bdhyfg(1) = bchyfg
      bdresevapa(1) = bcresevapa
      bdresevapb(1) = bcresevapb
      ! do not zero out, this still applies if the crop has not been killed
      ! bc0nam = ""
      ! bcrbc = 0
      ! bc0sla = 0.0
      ! bccovfact = 0.0
      ! bcddsthrsh = 0.0
      ! bchyfg = 0.0

      return
      end
