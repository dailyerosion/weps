!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine wepsinit

      ! Initializes variables in common blocks

      include 'p1werm.inc'
      include 'm1flag.inc'
      include 'm1subr.inc'
      include 'main/main.inc'
      include 'manage/man.inc'

      integer idx

      ! main/weps.for
      do idx = 1, mnsub
          mcur(idx) = 0          ! manage/man.inc
          amnryr(idx) = 1        ! m1subr.inc
          amnrotcycle(idx) = 1   ! m1subr.inc
      end do
      do idx = 1, mnsub+1
          mbeg(idx) = 0          ! manage/man.inc
      end do
      daysim = 0                 ! main/main.inc
      outcnt = 0                 ! main/main.inc
      maxper = 1                 ! main/main.inc
      lopday = 0                 ! main/main.inc
      lopmon = 0                 ! main/main.inc
      lopyr = 0                  ! main/main.inc


      ! set initialization flags
      am0dif = .true.            ! m1flag.inc
      am0eif = .true.            ! m1flag.inc
      am0sif = .true.            ! m1flag.inc
      am0ifl = .true.            ! m1flag.inc

      ! no crop growing at start of simulation
      am0cgf = .false.           ! m1flag.inc
      am0cif = .false.           ! m1flag.inc

      subflg = .true.            ! main/main.inc

      ! set grid flag until first gridding is done
      am0gdf = .false.           ! m1flag.inc

      ! set output flag to initialize output arrays
      am0oif = .true.            ! m1flag.inc

      ! set initialization, calibration, and report loop flags
      init_loop = .false.        ! m1flag.inc
      calib_loop = .false.       ! m1flag.inc
      report_loop = .false.      ! m1flag.inc


      ! main/input.for
      clifil = "cli_gen.cli"     ! main/main.inc
      simout = "out.out"         ! main/main.inc
      runfil = "weps.run"        ! main/main.inc
      winfil = "win_gen.win"     ! main/main.inc
      subfil = ""                ! main/main.inc

      return
      end

