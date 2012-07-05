!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!
      subroutine timer(timnum,timact)
! ****************************************************************** wjr
!
! Provides benchmark data
!
!       Edit History
!       05-Feb-99       wjr     Original coding
!
      include 'timer.inc'
!      
      integer		timnum			!# of timer being used
      integer		timact			!action: 1==start, 2==end, 3==print, 4==reset
      integer		idx
!
      real          timarr(0:11)
      real          tim
      integer		lsttim
      real          time(2)
      real          rtntim

      real          etime    

      data timarr /12*0.0/
      data lsttim / 0 /

      save timarr

      rtntim = etime(time)
      if (rtntim.eq.-1.0) rtntim = etime(time)

      tim = time(1)

      select case (timact)
      case (1)									! start a timer
        if (timnum .ne. TIMWEPS) then
            timarr(0) = timarr(0) + tim
        endif
        timarr(timnum) = timarr(timnum) - tim
      case (2)									! stop a timer
        if (timnum .ne. TIMWEPS) then
          timarr(0) = timarr(0) - tim
        endif
        timarr(timnum) = timarr(timnum) + tim
      case (3)
        timarr(0) = timarr(0) + tim
        do idx = 0,11
          write(*,fmt="(' ',a8,2x,f8.2)") timnam(idx),timarr(idx)
        end do
        timarr(0) = timarr(0) - tim
      case (4)
        timarr(timnum) = 0.0
      end select
      end      
