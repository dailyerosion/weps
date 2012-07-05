!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!
! This routine takes a residue amount and depth and distributes it
! into the array of soil layers

      subroutine resinit(resmass, resdepth, nlay, resarray, laythick)

!     + + + INPUT VARIABLE DECLARATIONS + + +
      real resmass
      real resdepth
      integer nlay
      real resarray(nlay)
      real laythick(nlay)

!     + + + INPUT VARIABLE DEFINITIONS + + +
!     resmass - residue mass (Kg/m^2)
!     resdepth - Depth residue is distributed in soil (mm)
!     nlay - number of soil layers
!     resarray(nlay) - soil residue array by layer (Kg/m^2)
!     laythick(nlay) - soil layer thickness (mm)

!     + + + LOCAL VARIABLE DECLARATIONS + + +
      integer ilay
      real    depth

!     + + + LOCAL VARIABLE DEFINITIONS + + +
!     ilay - array index
!     depth - accumulator for depth
!     thick - thickness of slice to which residue is to be added

      depth = resdepth
      do ilay = 1, nlay
          if (depth.gt.0.0) then
              resarray(ilay) = resmass                                  &
     &                       * min( depth,laythick(ilay)) / (resdepth)
              depth = depth - laythick(ilay)
          else
              resarray(ilay) = 0.0
          end if
      end do

      return
      end
