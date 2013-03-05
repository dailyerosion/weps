!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine liftvt (liftf, tillf, nlay, residue, resurface_roots, bflg)

!     + + + PURPOSE + + +
!
!     This subroutine performs the biomass manipulation process of transfering
!     the above ground biomass into the soil or the inverse process of bringing
!     buried biomass to the surface.  It deals only with the biomass
!     pools (ie no live crop is involved)
!
!     + + + KEYWORDS + + +
!     bury, lift, biomass manipulation

      use biomaterial, only: biomatter

      include 'p1werm.inc'
!
!     + + + ARGUMENT DECLARATIONS + + +
      real    liftf(mnrbc)
      real    tillf
      integer nlay
      type(biomatter), dimension(:), intent(inout) :: residue
      integer resurface_roots
      integer bflg

!     + + + ARGUMENT DEFINITIONS + + +
!     liftf     - fraction of buried material lifted to the surface for
!                 different residue burial classes (m^2/m^2)
!     tillf    - fraction of soil area tilled by the machine
!     nlay      - number of soil layers used in the operation(s)
!     residue - structure containing residue state variables to be modified
!     resurface_roots - flag to specify whether roots are resurfaced or not
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
          if( (residue(idy)%database%rbc .ge. 1) .and. (residue(idy)%database%rbc .le. mnrbc) ) then
!             lift it if biomass flag right
              if (BTEST(tflg,idy))then

                  ! stem
                  lifttot = 0.0
                  do lay=1,nlay
                      liftlay(lay) = residue(idy)%mass%stemz(lay) * liftf(residue(idy)%database%rbc) * tillf
                      lifttot = lifttot + liftlay(lay)
                      residue(idy)%mass%stemz(lay) = residue(idy)%mass%stemz(lay) - liftlay(lay)
                  end do
                  residue(idy)%mass%flatstem = residue(idy)%mass%flatstem + lifttot

                  ! leaf
                  lifttot = 0.0
                  do lay=1,nlay
                      liftlay(lay) = residue(idy)%mass%leafz(lay) * liftf(residue(idy)%database%rbc) * tillf
                      lifttot = lifttot + liftlay(lay)
                      residue(idy)%mass%leafz(lay) = residue(idy)%mass%leafz(lay) - liftlay(lay)
                  end do
                  residue(idy)%mass%flatleaf = residue(idy)%mass%flatleaf + lifttot

                  ! store
                  lifttot = 0.0
                  do lay=1,nlay
                      liftlay(lay) = residue(idy)%mass%storez(lay) * liftf(residue(idy)%database%rbc) * tillf
                      lifttot = lifttot + liftlay(lay)
                      residue(idy)%mass%storez(lay) = residue(idy)%mass%storez(lay) - liftlay(lay)
                  end do
                  residue(idy)%mass%flatstore = residue(idy)%mass%flatstore + lifttot

                  ! rootstore
                  if (resurface_roots == 1) then
                  lifttot = 0.0
                  do lay=1,nlay
                      liftlay(lay) = residue(idy)%mass%rootstorez(lay) * liftf(residue(idy)%database%rbc) * tillf
                      lifttot = lifttot + liftlay(lay)
                      residue(idy)%mass%rootstorez(lay) = residue(idy)%mass%rootstorez(lay) - liftlay(lay)
                  end do
                  residue(idy)%mass%flatrootstore = residue(idy)%mass%flatrootstore + lifttot
                  endif

                  ! rootfiber
                  lifttot = 0.0
                  if (resurface_roots == 1) then
                  do lay=1,nlay
                      liftlay(lay) = residue(idy)%mass%rootfiberz(lay) * liftf(residue(idy)%database%rbc) * tillf
                      lifttot = lifttot + liftlay(lay)
                      residue(idy)%mass%rootfiberz(lay) = residue(idy)%mass%rootfiberz(lay) - liftlay(lay)
                  end do
                  residue(idy)%mass%flatrootfiber = residue(idy)%mass%flatrootfiber + lifttot
                  endif

              endif
          endif
      end do

      return
      end
