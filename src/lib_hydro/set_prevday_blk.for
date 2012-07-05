!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine set_prevday_blk( nlay, bsdblk, bsdblk0 )

!     + + + PURPOSE + + +
!     This subroutine sets the previous day bulk density to the present
!     day bulk density

!     + + + KEYWORDS + + +
!     bulk density

!     + + + ARGUMENT DECLARATIONS + + +
      integer nlay
      real bsdblk(*), bsdblk0(*)

!     + + + ARGUMENT DEFINITIONS + + +
!     nlay     - number of soil layers to be updated
!     bsdblk   - bulk density (Mg/m^3)

!     + + + LOCAL VARIABLES + + +
      integer lay

!     + + + END SPECIFICATIONS + + + 

      do lay = 1,nlay
          bsdblk0(lay) = bsdblk(lay)
      end do

      end
