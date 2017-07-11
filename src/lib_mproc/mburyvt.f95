!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine mburyvt                                                &
     &          (buryf,tillf,bcrbc,burydistflg,                         &
     &           nlay,lthick,ldepth,                                    &
     &           btmflatstem, btmflatleaf, btmflatstore,                &
     &           btmflatrootstore, btmflatrootfiber,                    &
     &           btmbgstemz, btmbgleafz, btmbgstorez,                   &
     &           btmbgrootstorez, btmbgrootfiberz,                      &
     &           residue, bflg)

!     + + + PURPOSE + + +
!
!     This subroutine performs the biomass manipulation process of transfering
!     the above ground biomass into the soil or the inverse process of bringing
!     buried biomass to the surface.  It deals only with the biomass
!     pools (ie no live crop is involved)
!
!     + + + KEYWORDS + + +
!     bury, lift, biomass manipulation

      use weps_interface_defs, ignore_me=>mburyvt
      use biomaterial, only: biomatter

      include 'p1werm.inc'
!
!     + + + ARGUMENT DECLARATIONS + + +
      real    buryf(mnrbc)
      real    tillf
      integer bcrbc
      integer burydistflg

      integer nlay
      real    lthick(*)
      real    ldepth(*)

      real   btmflatstem
      real   btmflatleaf
      real   btmflatstore

      real   btmflatrootstore
      real   btmflatrootfiber

      real   btmbgstemz(*)
      real   btmbgleafz(*)
      real   btmbgstorez(*)

      real   btmbgrootstorez(*)
      real   btmbgrootfiberz(*)

      type(biomatter), dimension(:), intent(inout) :: residue
      integer bflg

!     + + + ARGUMENT DEFINITIONS + + +

!     buryf     - fraction of flat material buried for
!                 different residue burial classes (m^2/m^2)
!     tillf    - fraction of soil area tilled by the machine
!     bcrbc     - residue burial class for standing crop
!     nlay      - number of soil layers used in the operation(s)
!     lthick    - distance from soil surface to bottom of layer
!                 for each soil layer
!     btmflatstem  - crop flat stem mass (kg/m^2)
!     btmflatleaf  - crop flat leaf mass (kg/m^2)
!     btmflatstore - crop flat storage mass (kg/m^2)

!     btmflatrootstore - crop flat root storage mass (kg/m^2)
!                   (tubers (potatoes, carrots), extended leaf (onion), seeds (peanuts))
!     btmflatrootfiber - crop flat root fibrous mass (kg/m^2)

!     btmbgflatstemz  - crop buried stem mass by layer (kg/m^2)
!     btmbgflatleafz  - crop buried leaf mass by layer (kg/m^2)
!     btmbgflatstorez - crop buried storage mass by layer (kg/m^2)

!     btmbgrootstorez - crop root storage mass by layer (kg/m^2)
!                   (tubers (potatoes, carrots), extended leaf (onion), seeds (peanuts))
!     btmbgrootfiberz - crop root fibrous mass by layer (kg/m^2)


!     residue - structure containing residue state variables to be modified
!     bflg      - flag indicating what to manipulate
!       0 - All standing material is manipulate (both crop and residue)
!       1 - Crop
!       2 - 1'st residue pool
!       4 - 2'nd residue pool
!       ....
!       2**n - nth residue pool

!       Note that any combination of pools or crop may be used
!       A bit test is done on the binary number to see what to modify
!
!     + + + ACCESSED COMMON BLOCK VARIABLE DEFINITIONS + + +
!     mnrbc         - max number of residue burial classes

!     + + + FUNCTIONS + + +
!      real burydist

!     + + + LOCAL VARIABLES + + +

      integer  lay,idy,tflg
      real     tbury
      real     fracbury(nlay)
      integer :: npools  ! number of residue age pools

!     + + + LOCAL VARIABLE DEFINITIONS + + +
!
!     bury      - mass of biomass that is buried
!     tbury     - mass of crop biomass that is buried
!     idy       - biomass pools (1-3)
!     lay       - number of layers in a specified subregion
!     tflg      - temporary biomass flag
!
!     + + + END SPECIFICATIONS + + +

      npools = size(residue)

      !set tflg bits correctly for "all" pools if bflg=0
      if (bflg .eq. 0) then
         tflg = 1                   ! crop pool
         do idy = 1, npools
            tflg = tflg + 2**idy    ! decomp pools
         end do
      else
        tflg = bflg
      endif

!     calculate fractions of total to be buried in each layer
      do lay = 1, nlay
          fracbury(lay) = burydist(lay,burydistflg,lthick,ldepth,nlay)
      end do
      !perform the burying of biomass
!      print *, 'mbury tflg/bflg: ', tflg, bflg
!      print *, 'tflat before mbury: ', tflat
!      print *, 'dflat before mbury: ', dflat(1), dflat(2),dflat(3)

!     check for proper indexes in bcrbc
      if( (bcrbc.ge.1).and.(bcrbc.le.mnrbc) ) then
          if (BTEST(tflg,0)) then       ! crop pool
              ! stem component
              tbury = btmflatstem*buryf(bcrbc)*tillf
              do lay=1,nlay
                  btmbgstemz(lay) = btmbgstemz(lay)+tbury*fracbury(lay)
              end do
              btmflatstem = btmflatstem-tbury
              ! leaf component
              tbury = btmflatleaf*buryf(bcrbc)*tillf
              do lay=1,nlay
                  btmbgleafz(lay) = btmbgleafz(lay)+tbury*fracbury(lay)
              end do
              btmflatleaf = btmflatleaf-tbury
              ! storage component
              tbury = btmflatstore*buryf(bcrbc)*tillf
              do lay=1,nlay
                  btmbgstorez(lay) = btmbgstorez(lay)                   &
     &                             + tbury*fracbury(lay)
              end do
              btmflatstore = btmflatstore-tbury
              ! root storage component
              tbury = btmflatrootstore*buryf(bcrbc)*tillf
              do lay=1,nlay
                  btmbgrootstorez(lay) = btmbgrootstorez(lay)           &
     &                                 + tbury*fracbury(lay)
              end do
              btmflatrootstore = btmflatrootstore-tbury
              ! root fiber component
              tbury = btmflatrootfiber*buryf(bcrbc)*tillf
              do lay=1,nlay
                  btmbgrootfiberz(lay) = btmbgrootfiberz(lay)           &
     &                                 + tbury*fracbury(lay)
              end do
              btmflatrootfiber = btmflatrootfiber-tbury
          endif
      endif

      do idy = 1, npools
!         check for proper indexes in bdrbc
          if( (residue(idy)%database%rbc.ge.1).and.(residue(idy)%database%rbc.le.mnrbc) ) then
              if (BTEST(tflg,idy)) then
                  tbury = residue(idy)%mass%flatstem * buryf(residue(idy)%database%rbc) * tillf
                  do lay=1,nlay
                      residue(idy)%mass%stemz(lay) = residue(idy)%mass%stemz(lay) + tbury*fracbury(lay)
                  end do
                  residue(idy)%mass%flatstem = residue(idy)%mass%flatstem - tbury

                  tbury = residue(idy)%mass%flatleaf * buryf(residue(idy)%database%rbc) * tillf
                  do lay=1,nlay
                      residue(idy)%mass%leafz(lay) = residue(idy)%mass%leafz(lay) + tbury*fracbury(lay)
                  end do
                  residue(idy)%mass%flatleaf = residue(idy)%mass%flatleaf - tbury

                  tbury = residue(idy)%mass%flatstore * buryf(residue(idy)%database%rbc) * tillf
                  do lay=1,nlay
                      residue(idy)%mass%storez(lay) = residue(idy)%mass%storez(lay) + tbury*fracbury(lay)
                  end do
                  residue(idy)%mass%flatstore = residue(idy)%mass%flatstore - tbury

                  tbury = residue(idy)%mass%flatrootstore * buryf(residue(idy)%database%rbc) * tillf
                  do lay=1,nlay
                      residue(idy)%mass%rootstorez(lay) = residue(idy)%mass%rootstorez(lay) + tbury*fracbury(lay)
                  end do
                  residue(idy)%mass%flatrootstore = residue(idy)%mass%flatrootstore - tbury

                  tbury = residue(idy)%mass%flatrootfiber * buryf(residue(idy)%database%rbc) * tillf
                  do lay=1,nlay
                      residue(idy)%mass%rootfiberz(lay) = residue(idy)%mass%rootfiberz(lay) + tbury*fracbury(lay)
                  end do
                  residue(idy)%mass%flatrootfiber = residue(idy)%mass%flatrootfiber - tbury
              endif
          endif
      end do

!      print *, 'tflat after mbury: ', tflat
!      print *, 'dflat after mbury: ', dflat(1), dflat(2),dflat(3)
      return
      end
