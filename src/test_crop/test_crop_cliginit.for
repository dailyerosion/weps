!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!
!NOTE:  Taken from main/cliginit.for on 2/9/05
!       so one didn't have to include the "main"
!       subdirectory in the test_crop build - LEW

! Modified to assume that we are only reading the
! newest version of cligen files (Forest Service version)

      subroutine test_crop_cliginit
! *************************************************** wjr
! Contains init code from main
!
!       Edit History
!       04-Mar-99       wjr     created
!
      include 'p1werm.inc'
      include 'w1clig.inc'
      include 'file.inc'
!
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

      do 10 idx = 1, 6                          !Skip 1st 6 lines
         read(luicli,'(a128)') header
         ! write(6,*) 'header: ', header,':'
   10 continue

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
   11  continue
       awtyav = awtyav/12.0

       ! read three lines to get to precipitation values
       do idx = 1, 3
          read(luicli,'(a128)') header
          ! write(6,*) 'header: ', header,':'
       end do

       ! read average monthy total precipitation
       read(luicli,*) (awzmpt(idx), idx = 1,12)
       ! write(6,*)  (awzmpt(idx), idx = 1,12)

      rewind luicli
      end

