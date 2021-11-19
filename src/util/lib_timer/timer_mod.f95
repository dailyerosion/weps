!$Author$
!$Date$
!$Revision$
!$HeadURL$

module timer_mod

      integer, parameter :: TIMSTART = 1
      integer, parameter :: TIMSTOP = 2
      integer, parameter :: TIMPRINT = 3
      integer, parameter :: TIMRESET = 4

      integer, parameter :: TIMINIT = 1
      integer, parameter :: TIMSUBR = 2
      integer, parameter :: TIMEROD = 3
      integer, parameter :: TIMREPO = 4
      integer, parameter :: TIMWEPS = 5

      integer, parameter :: ntim = 5

      character*8 timnam(0:ntim)
      data timnam /'system', 'initialize', 'subregion', 'erosion', 'reports', 'weps'/

      real :: timarr(0:ntim)
      
      data timarr /6*0.0/

  contains

    subroutine timer(timnum,timact)

      ! Provides benchmark data
      ! 05-Feb-99       wjr     Original coding

      integer     timnum                        !# of timer being used
      integer     timact                        !action: 1==start, 2==end, 3==print, 4==reset
      integer     idx

      real        tim
      integer     lsttim
    
      data lsttim / 0 /

      call cpu_time(tim)

      select case (timact)
      case (1)                                  ! start a timer
        if (timnum .ne. TIMWEPS) then
            timarr(0) = timarr(0) + tim
        endif
        timarr(timnum) = timarr(timnum) - tim
      case (2)                                  ! stop a timer
        if (timnum .ne. TIMWEPS) then
            timarr(0) = timarr(0) - tim
        endif
        timarr(timnum) = timarr(timnum) + tim
      case (3)                                  ! print out timers
        timarr(0) = timarr(0) + tim
        do idx = 0, ntim
          write(*,fmt="(' ',a8,2x,f8.2)") timnam(idx),timarr(idx)
        end do
        timarr(0) = timarr(0) - tim
      case (4)                                  ! reset a timer
        timarr(timnum) = 0.0
      end select

    end subroutine timer     

end module timer_mod
