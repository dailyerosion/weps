!$Author$
!$Date$
!$Revision$
!$HeadURL$
!**********************************************************************
!     subroutine sbigrd
!**********************************************************************
      subroutine sbigrd
!
!     + + + PURPOSE + + +
!     To set the grid output arrays to zero
!
!     + + + ARGUMENT DECLARATIONS
!
!     + + + GLOBAL COMON BLOCKS + + +
!     compiler instr.- no warn of unreferenced symbols in include files

      include 'p1werm.inc'      
!
!     + + + LOCAL COMMON BLOCKS + + +
      include 'erosion/m2geo.inc'
      include 'erosion/e2erod.inc'
!
!     + + + ARGUMENT DEFINITIONS + + +
!
!     + + + SUBROUTINES CALLED + + +
!
!     + + + LOCAL VARIABLES + + +
      integer i, j
!
!     + + + END SPECIFICATIONS + + +
!
      do 20 j = 0, jmax
      do 10 i = 0, imax
      egt(i,j) = 0
      egtss(i,j) = 0
      egt10(i,j) = 0
   10 continue
   20 continue
!      
      return
      end      
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++           
