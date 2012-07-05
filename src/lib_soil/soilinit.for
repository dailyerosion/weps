!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!
      subroutine soilinit(isr)
! ***************************************************************** wjr
! Contains init code from main
!
!       Edit History
!       04-Mar-99       wjr     created
!
      include 'p1werm.inc'
      include 's1layr.inc'
      include 's1dbc.inc'
!
      integer isr
!
      ! recalculate  depth to bottom of soil layer
      call depthini( nslay(isr), aszlyt(1,isr), aszlyd(1,isr) )
!
! This should go away, possibly becoming a data statement   
      asmno3(isr) = 0.

      end

