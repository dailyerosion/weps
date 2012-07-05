!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!
      subroutine cliginit
! *************************************************** wjr
! Contains init code from main
!
!       Edit History
!       04-Mar-99       wjr     created
!
      include 'p1werm.inc'
      include 'w1clig.inc'
      include 'file.inc'
      include 'm1flag.inc'
      include 'main/w1cli.inc'
      include 'main/w1win.inc'

      integer idx
      character header*128
      real junk1, junk2

!     + + + Definitions + + +
!     awtyav - Average yearly air temperature (deg C).
!     awtmav - Average monthly air temperature (deg C).
!     awtmnav - Average monthly minimum air temperature (deg C).
!     awtmxav - Average monthly maximum air temperature (deg C).
!
      rewind luicli

      if (cli_gen_fmt_flag == 3) then
             read(luicli,'(a128)') header
      end if

      if (cli_gen_fmt_flag >= 2) then  !Forest Service cligen db format

          do 10 idx = 1, 5
             read(luicli,'(a128)') header
             ! write(6,*) 'header: ', header,':'
   10     continue

          ! read monthy average of daily maximum temperature
          read(luicli,*) (awtmxav(idx), idx = 1,12)
          ! write(6,*)  (awtmxav(idx), idx = 1,12)

          read(luicli,'(a128)') header
          ! write(6,*) 'header: ', header

          ! read monthy average of daily minimum temperature
          read(luicli,*) (awtmnav(idx), idx = 1,12)
          ! write(6,*)  (awtmnav(idx), idx = 1,12)

          ! find yearly average temperature
          awtyav = 0.0
          do 11 idx = 1, 12
          ! average temperature is mean of maximum and minimum
             awtmav(idx) = (awtmnav(idx) + awtmxav(idx)) / 2.0
             awtyav = awtyav + awtmav(idx)
   11     continue
          awtyav = awtyav/12.0

          ! read three lines to get to precipitation values
          do idx = 1, 3
             read(luicli,'(a128)') header
             ! write(6,*) 'header: ', header,':'
          end do

          ! read average monthy total precipitation
          read(luicli,*) (awzmpt(idx), idx = 1,12)
          ! write(6,*)  (awzmpt(idx), idx = 1,12)

          ! find average yearly total precipitation
          awzypt = 0.0
          do idx = 1, 12
             awzypt = awzypt + awzmpt(idx)
          end do

      else
 
          ! Old obsolete cligen file format (Canadians are still using it)
          write(*,*) 'Warning: Attempting to use old cligen format file'
          do 20 idx = 1, 3       ! Skip the first 3 lines
             read(luicli,'(a128)') header
   20     continue
          read(luicli,*) junk1, junk2, awtyav        ! skip lat/lon and get yearly ave temp
          read(luicli,*) (awtmav(idx), idx = 1,12)   ! read 12 monthly ave temps
      endif

      rewind luicli
      daycdx = 0
      cwrnflg = .true.

      ! this is a windgen intialization
      ! placed here rather tahn create a whole new file.
      daywdx = 0
      wwrnflg = .true.

      end

