!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!
!
!
      subroutine liftvt                                                 &
     &              (liftf, tillf, bdrbc, nlay,                         &
     &           bdmflatstem, bdmflatleaf, bdmflatstore,                &
     &           bdmflatrootstore, bdmflatrootfiber,                    &
     &           bdmbgstemz, bdmbgleafz, bdmbgstorez,                   &
     &           bdmbgrootstorez, bdmbgrootfiberz,                      &
     &           resurface_roots, bflg)


!     + + + PURPOSE + + +
!
!     This subroutine performs the biomass manipulation process of transfering
!     the above ground biomass into the soil or the inverse process of bringing
!     buried biomass to the surface.  It deals only with the biomass
!     pools (ie no live crop is involved)
!
!
!     + + + KEYWORDS + + +
!     bury, lift, biomass manipulation

      include 'p1werm.inc'
!
!     + + + ARGUMENT DECLARATIONS + + +
      integer resurface_roots
      integer nlay, bflg
      real    liftf(mnrbc)
      real    tillf
      integer bdrbc(mnbpls)

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


!     + + + ARGUMENT DEFINITIONS + + +
!     resurface_roots - flag to specify whether roots are resurfaced or not
!     liftf     - fraction of buried material lifted to the surface for
!                 different residue burial classes (m^2/m^2)
!     tillf    - fraction of soil area tilled by the machine
!     nlay      - number of soil layers used in the operation(s)
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
!       1 - Crop is cut
!       2 - 1'st residue pool
!       4 - 2'nd residue pool
!       ....
!       2**n - nth residue pool

!       Note that any combination of pools or crop may be used
!       A bit test is done on the binary number to see what to modify

!     + + + ACCESSED COMMON BLOCK VARIABLE DEFINITIONS + + +
!
!     mnrbc         - max number of residue burial classes
!     mnbpls        - max number of biomass pools
!     mnsz          - max number of soil layers
!
!     + + + PARAMETERS + + +
!
!     + + + LOCAL VARIABLES + + +
!
      integer  lay, idy, tflg
      real     liftlay(mnsz), lifttot
!
!     + + + LOCAL VARIABLE DEFINITIONS + + +
!
!     idy       - biomass pools (1-3)
!     lay       - number of layers in a specified subregion
!     liftlay   - buried material lifted to the surface in each layer
!     lifttot   - total buried material lifted to the surface
!
!     + + + END SPECIFICATIONS + + +

      !set tflg bits correctly for "all" pools if bflg=0
      if (bflg .eq. 0) then
         tflg = 1                   ! crop pool
         do 10 idy = 1,mnbpls
            tflg = tflg + 2**idy    ! decomp pools
10        continue
      else
        tflg = bflg
      endif

!     perform the lifting of biomass
      do idy = 1,mnbpls
!         check for proper indexes in bdrbc
          if( (bdrbc(idy).ge.1).and.(bdrbc(idy).le.mnrbc) ) then
!             lift it if biomass flag right
              if (BTEST(tflg,idy))then

                  ! stem
                  lifttot = 0.0
                  do lay=1,nlay
                      liftlay(lay) = bdmbgstemz(lay,idy)                &
     &                             * liftf(bdrbc(idy))*tillf
                      lifttot = lifttot + liftlay(lay)
                      bdmbgstemz(lay,idy) = bdmbgstemz(lay,idy)         &
     &                                    - liftlay(lay)
                  end do
                  bdmflatstem(idy) = bdmflatstem(idy) + lifttot

                  ! leaf
                  lifttot = 0.0
                  do lay=1,nlay
                      liftlay(lay) = bdmbgleafz(lay,idy)                &
     &                             * liftf(bdrbc(idy))*tillf
                      lifttot = lifttot + liftlay(lay)
                      bdmbgleafz(lay,idy) = bdmbgleafz(lay,idy)         &
     &                                    - liftlay(lay)
                  end do
                  bdmflatleaf(idy) = bdmflatleaf(idy) + lifttot

                  ! store
                  lifttot = 0.0
                  do lay=1,nlay
                      liftlay(lay) = bdmbgstorez(lay,idy)               &
     &                             * liftf(bdrbc(idy))*tillf
                      lifttot = lifttot + liftlay(lay)
                      bdmbgstorez(lay,idy) = bdmbgstorez(lay,idy)       &
     &                                     - liftlay(lay)
                  end do
                  bdmflatstore(idy) = bdmflatstore(idy) + lifttot

                  ! rootstore
                  if (resurface_roots == 1) then
                  lifttot = 0.0
                  do lay=1,nlay
                      liftlay(lay) = bdmbgrootstorez(lay,idy)           &
     &                             * liftf(bdrbc(idy))*tillf
                      lifttot = lifttot + liftlay(lay)
                      bdmbgrootstorez(lay,idy)= bdmbgrootstorez(lay,idy)&
     &                                        - liftlay(lay)
                  end do
                  bdmflatrootstore(idy)= bdmflatrootstore(idy) + lifttot
                  endif

                  ! rootfiber
                  lifttot = 0.0
                  if (resurface_roots == 1) then
                  do lay=1,nlay
                      liftlay(lay) = bdmbgrootfiberz(lay,idy)           &
     &                             * liftf(bdrbc(idy))*tillf
                      lifttot = lifttot + liftlay(lay)
                      bdmbgrootfiberz(lay,idy)= bdmbgrootfiberz(lay,idy)&
     &                                        - liftlay(lay)
                  end do
                  bdmflatrootfiber(idy)= bdmflatrootfiber(idy) + lifttot
                  endif

              endif
          endif
      end do

      return
      end
