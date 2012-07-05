!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!
!
!
      subroutine mburyvt                                                &
     &          (buryf,tillf,bcrbc,bdrbc,burydistflg,                   &
     &           nlay,lthick,ldepth,                                    &
     &           btmflatstem, btmflatleaf, btmflatstore,                &
     &           btmflatrootstore, btmflatrootfiber,                    &
     &           btmbgstemz, btmbgleafz, btmbgstorez,                   &
     &           btmbgrootstorez, btmbgrootfiberz,                      &
     &           bdmflatstem, bdmflatleaf, bdmflatstore,                &
     &           bdmflatrootstore, bdmflatrootfiber,                    &
     &           bdmbgstemz, bdmbgleafz, bdmbgstorez,                   &
     &           bdmbgrootstorez, bdmbgrootfiberz,                      &
     &           bflg)

!     + + + PURPOSE + + +
!
!     This subroutine performs the biomass manipulation process of transfering
!     the above ground biomass into the soil or the inverse process of bringing
!     buried biomass to the surface.  It deals only with the biomass
!     pools (ie no live crop is involved)
!
!     + + + KEYWORDS + + +
!     bury, lift, biomass manipulation

      include 'p1werm.inc'
!
!     + + + ARGUMENT DECLARATIONS + + +
      real    buryf(mnrbc)
      real    tillf
      integer bcrbc
      integer bdrbc(mnbpls)
      integer burydistflg

      integer nlay
      real    lthick(mnsz)
      real    ldepth(mnsz)

      real   btmflatstem
      real   btmflatleaf
      real   btmflatstore

      real   btmflatrootstore
      real   btmflatrootfiber

      real   btmbgstemz(mnsz)
      real   btmbgleafz(mnsz)
      real   btmbgstorez(mnsz)

      real   btmbgrootstorez(mnsz)
      real   btmbgrootfiberz(mnsz)

      real   bdmflatstem(mnbpls)
      real   bdmflatleaf(mnbpls)
      real   bdmflatstore(mnbpls)

      real   bdmflatrootstore(mnbpls)
      real   bdmflatrootfiber(mnbpls)

      real   bdmbgstemz(mnsz,mnbpls)
      real   bdmbgleafz(mnsz,mnbpls)
      real   bdmbgstorez(mnsz,mnbpls)

      real   bdmbgrootstorez(mnsz,mnbpls)
      real   bdmbgrootfiberz(mnsz,mnbpls)

      integer bflg

!     + + + ARGUMENT DEFINITIONS + + +

!     buryf     - fraction of flat material buried for
!                 different residue burial classes (m^2/m^2)
!     tillf    - fraction of soil area tilled by the machine
!     bcrbc     - residue burial class for standing crop
!     bdrbc     - residue burial classes for residue
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

!     bdmflatstem  - flat stem mass (kg/m^2)
!     bdmflatleaf  - flat leaf mass (kg/m^2)
!     bdmflatstore - flat storage mass (kg/m^2)

!     bdmflatstore - flat storage root mass (kg/m^2)
!     bdmflatfiber - flat fibrous root mass (kg/m^2)

!     bdmbgstemz  - buried stem mass by layer (kg/m^2)
!     bdmbgleafz  - buried leaf mass by layer (kg/m^2)
!     bdmbgstorez - buried (from above ground) storage mass by layer (kg/m^2)

!     bdmbgrootstorez - buried storage root mass by layer (kg/m^2)
!     bdmbgrootfiberz - buried fibrous root mass by layer (kg/m^2)

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
!
!     mnrbc         - max number of residue burial classes
!     mnbpls        - max number of biomass pools
!     mnsz          - max number of soil layers
!
!     + + + FUNCTIONS + + +
      real burydist
!
!     + + + LOCAL VARIABLES + + +
!
      integer  lay,idy,tflg
      real     tbury
      real     fracbury(nlay)
!
!     + + + LOCAL VARIABLE DEFINITIONS + + +
!
!     bury      - mass of biomass that is buried
!     tbury     - mass of crop biomass that is buried
!     idy       - biomass pools (1-3)
!     lay       - number of layers in a specified subregion
!     tflg      - temporary biomass flag
!
!     + + + END SPECIFICATIONS + + +
!
      !set tflg bits correctly for "all" pools if bflg=0
      if (bflg .eq. 0) then
         tflg = 1                   ! crop pool
         do idy = 1, mnbpls
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

      do idy = 1, mnbpls
!         check for proper indexes in bdrbc
          if( (bdrbc(idy).ge.1).and.(bdrbc(idy).le.mnrbc) ) then
              if (BTEST(tflg,idy)) then
                  tbury = bdmflatstem(idy)*buryf(bdrbc(idy))*tillf
                  do lay=1,nlay
                      bdmbgstemz(lay,idy) = bdmbgstemz(lay,idy)         &
     &                                    + tbury*fracbury(lay)
                  end do
                  bdmflatstem(idy) = bdmflatstem(idy) - tbury

                  tbury = bdmflatleaf(idy)*buryf(bdrbc(idy))*tillf
                  do lay=1,nlay
                      bdmbgleafz(lay,idy) = bdmbgleafz(lay,idy)         &
     &                                    + tbury*fracbury(lay)
                  end do
                  bdmflatleaf(idy) = bdmflatleaf(idy) - tbury

                  tbury = bdmflatstore(idy)*buryf(bdrbc(idy))*tillf
                  do lay=1,nlay
                      bdmbgstorez(lay,idy) = bdmbgstorez(lay,idy)       &
     &                                     + tbury*fracbury(lay)
                  end do
                  bdmflatstore(idy) = bdmflatstore(idy) - tbury

                  tbury = bdmflatrootstore(idy)*buryf(bdrbc(idy))*tillf
                  do lay=1,nlay
                      bdmbgrootstorez(lay,idy) =bdmbgrootstorez(lay,idy)&
     &                                         + tbury*fracbury(lay)
                  end do
                  bdmflatrootstore(idy) = bdmflatrootstore(idy) - tbury

                  tbury = bdmflatrootfiber(idy)*buryf(bdrbc(idy))*tillf
                  do lay=1,nlay
                      bdmbgrootfiberz(lay,idy) =bdmbgrootfiberz(lay,idy)&
     &                                         + tbury*fracbury(lay)
                  end do
                  bdmflatrootfiber(idy) = bdmflatrootfiber(idy) - tbury
              endif
          endif
      end do

!      print *, 'tflat after mbury: ', tflat
!      print *, 'dflat after mbury: ', dflat(1), dflat(2),dflat(3)
      return
      end
