!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine depthini(nlay, bszlyt, bszlyd)

      integer nlay
      real    bszlyt(*), bszlyd(*)

      integer idx

!     nlay - number of soil layers
!     bszlyt - soil layer thickness (mm)
!     bszlyd - depth to bottom of soil layer (mm)

      bszlyd(1) = bszlyt(1)
      do idx = 2, nlay
        bszlyd(idx) = bszlyt(idx) + bszlyd(idx-1)
      end do

      return
      end

