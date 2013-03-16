!$Author$
!$Date$
!$Revision$
!$HeadURL$
!**********************************************************************
!     subroutine sbigrd
!**********************************************************************
      subroutine sbigrd( cellstate )

!     + + + PURPOSE + + +
!     To set the grid output arrays to zero

      use erosion_data_struct_defs, only: cellsurfacestate

!     +++ ARGUMENT DECLARATIONS +++
      type(cellsurfacestate), dimension(0:,0:), intent(inout) :: cellstate     ! initialized grid cell state values

!     + + + GLOBAL COMON BLOCKS + + +
!     compiler instr.- no warn of unreferenced symbols in include files

      include 'p1werm.inc'      
!
!     + + + LOCAL COMMON BLOCKS + + +
      include 'erosion/m2geo.inc'

!     + + + LOCAL VARIABLES + + +
      integer i, j
!
!     + + + END SPECIFICATIONS + + +
!
      do 20 j = 0, jmax
         do 10 i = 0, imax
            cellstate(i,j)%egt = 0
            cellstate(i,j)%egtss = 0
            cellstate(i,j)%egt10 = 0
   10    continue
   20 continue
!      
      return
      end      
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++           
