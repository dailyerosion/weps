!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine clear_erosion( cellstate )

      use erosion_data_struct_defs, only: cellsurfacestate

!     + + + ARGUMENT DECLARATIONS + + +
      type(cellsurfacestate), dimension(0:,0:), intent(inout)::cellstate     ! initialized grid cell state values

        include "p1werm.inc"
        include "erosion/m2geo.inc"

        integer i,j

        do 215 i = 0, imax
           do 210 j = 0, jmax
              cellstate(i,j)%egt = 0.0
              cellstate(i,j)%egtcs = 0.0
              cellstate(i,j)%egtss = 0.0
              cellstate(i,j)%egt10 = 0.0
210        continue
215     continue

        return
        end
