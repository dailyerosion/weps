!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!

!     file: 'wsum.for'

      program wsum

!     + + + PURPOSE + + +
!     This subroutine summarizes the WINDGEN and CLIGEN weather
!     simulation output files by month and year.

!     author: John Tatarko
!     version: 04/02/92

!     + + + KEY WORDS + + +
!     WERM, CLIGEN, WINDGEN, weather, summary

!     + + + GLOBAL COMMON BLOCKS + + +

      include 'p1werm.inc'
      include 'wpath.inc'
      include 'w1clig.inc'
      include 'w1wind.inc'
      include 'w1pavg.inc'
      include 'm1sim.inc'

!     + + + LOCAL COMMON BLACKS + + +
      include 'main/main.inc'

!     + + + LOCAL VARIABLES + + +
      character header*80

      logical mofl, yrfl
      integer bd, bm, ccd, ccm, ccy, cwd, cwm, cwy, count, ed, em
      integer i, j, k, last, tucnt, ucnt, ycount
      real avedt, avepir, awuyav, avwe, dur, rad
      real sumdpt, sumdt, sumdir, sumdu, sumwe, tp
      real wdir, wvel, winde, xmav
      real ysumpt, ysumdt, ysumpi, ysumu, yavedt, yavepi
      real ysumwe, yavewe

!     + + + LOCAL DEFINITIONS + + +

!   avedt     - average temperature for the month.
!   avepir    - average global radiation for the month.
!   avwe      - average wind energy
!   awtdav    - average of the maximum and minimum temperature for
!               each day.
!   awudav    - average of the maximum and minimum wind speed for
!               each day.
!   awupav    - average wind speed for the month.
!   awuyav    - average wind speed for the year.
!   bd        - beginning day of the period.
!   bm        - beginning month of the period.
!   ccd,ccm,ccy - current day, month, and year of the CLIGEN file.
!   cwd,cwm,cwy - current day, month, and year of the WINDGEN file.
!   clifil    - CLIGEN input file name.
!   count     - total number of days in the month.
!   dur       - duration pf precipitation produced by CLIGEN
!   ed        - last day of the period.
!   em        - last month of the period.
!   header    - character header information.
!   i         - counter for simulation loops.
!   irise     - time of sunrise (not used).
!   j,k       - iocheck variables which are set to -1 when the end of
!               file is encountered.
!   last      - last day of the current month as determined by the
!               function 'lstday'.
!   mofl      - flag set to record the first day of period.
!   rad       - global radiation.
!   sumdpt    - sum of daily precipitation.
!   sumdt     - sum of daily temperature.
!   sumdir    - sum of daily global radiation.
!   sumdu     - sum of daily average wind speed.
!   sumwe     - sum of daily wind energy.
!   tp        - produced by CLIGEN (not used here).
!   tucnt     - total number of days wind speed > 8 m/s for year.
!   ucnt      - total number of days wind speed > 8 m/s for month.
!   wdir      - wind direction produced by CLIGEN (not used here).
!   wvel      - wind speed produced by CLIGEN (not used here).
!   winfil    - WINDGEN input file name.
!   winde     - wind energy for the day, MJ
!   xmav      - produced by CLIGEN (not used here).
!   ycount    - total number of days in the year.
!   ysumpt    - sum of yearly precipitation.
!   ysumdt    - sum of yearly temperature.
!   ysumpi    - sum of yearly global radiation.
!   ysumu     - sum of average yearly wind speed.
!   yavedt    - average of yearly temperature.
!   yavepi    - average of yearly global radiation.
!   yrfl      - flag set to record the current year.

!     + + + FUNCTIONS CALLED + + +
!       lstday

!     + + + FUNCTIONS DECLARATIONS + + +
       integer  lstday

!     + + + DATA INITIALIZATIONS + + +
!     set initialization flags
      mofl = .true.
      yrfl = .true.

      count = 0
      sumdpt = 0.0
      sumdt = 0.0
      sumdir = 0.0
      sumdu = 0.0
      sumwe = 0.0
      tucnt = 0
      ucnt = 0

      ysumpt = 0.0
      ysumdt = 0.0
      ysumpi = 0.0
      ysumu = 0.0
      ysumwe = 0.0
      ycount = 0
      ycount = 0

!     + + + INPUT FORMATS + + +
 1020 format (a80)
 1025 format (a60)
 1030 format (2x,i2,2x,i2,1x,i4,1x,2f6.2,f5.2,1x,f6.2,3f7.2,f6.2,2f7.2)
 1040 format (1x,i2,1x,i2,1x,i4,3f6.1,f6.2,f6.1)

!     + + + OUTPUT FORMATS + + +
 2000 format ('    ')
 2025 format (' CLIGEN file:  ', a60)
 2027 format (' WINDGEN file:  ', a60)
 2026 format (a60)
 2030 format (/,' CLIGEN date -                ',i4,i4,'  ',i4,/        &
     &         ,' does not match WINDGEN date -',i4,i4,'  ',i4,/        &
     &         ,' check files       ')
 2210 format (/,' warning, the output file - ',a25,/,' already exists - &
     &press enter to overwrite this file or control break to stop')
 2220 format (/,25x,' Period Weather Summary Report',/,80('-'),/)
 2230 format (/,80('-'),/,                                              &
     &'   Period dates     Total     Ave.    Ave. gl.  Ave. wind   wind &
     &>   Total wind',/,                                                &
     &' begin      end     precip.  temp.   radiation    speed     8 m/s&
     &   energy >8m/s',/,                                               &
     &' dd/mm     dd/mm     (mm)     (c)     (MJ/m^2)    (m/s)     (days&
     &)      (MJ)',/,                                                   &
     &80('-'))
!2300 format (80('-'))
 2890 format (6x,i4)
 2900 format(1x,i2.2,'/',i2.2,5x,i2.2,'/',i2.2,1x,2f9.2,2f10.2,i9,f12.2)
 2910 format(1x,5('-'),5x,5('-'),1x,2(2x,7('-')),2(3x,7('-')),4x,5('-'))
 2920 format (1x,'01/01',5x,'31/12',1x,2f9.2,2f10.2,i9,f12.2,/)

!     + + + END SPECIFICATIONS + + +

!     open CLIGEN file
!10   write (*,*) ' Enter the CLIGEN file name '
!     read (*,1025) clifil
!     inquire(file=clifil,exist=fexist)
!     if(.not. fexist) write(*,*) clifil,'    file not found'
!     if(.not. fexist) goto 10
      open (3, file = clifil)

!     open WINDGEN file
!15   write (*,*) ' Enter the WINDGEN file name '
!     read (*,1025) winfil
!     inquire(file=winfil,exist=fexist)
!     if(.not. fexist) write(*,*) winfil,'    file not found'
!     if(.not. fexist) goto 15
      open (4, file = winfil)

!     open weather summary output file
!     write (*,*) ' Enter the output file name '
!     read (*,1025) wsum
!     inquire(file=wsum,exist=fexist)
!     if(fexist) then
!       write (*,2210) wsum
!       read (*,1020)
!     end if
!     open (8, file = wsum)
      write (2,2220)

!     read CLIGEN header - remove at a later date ?

      read(3,1020) header
      read(3,1025) header
      write(2,2025) clifil
      write(2,1025) header
      do 20 i=1,2
        read(3,1020) header
        write(2,1020) header
   20 continue
      read(3,1020) header
      read(3,1020) header
      read(3,1020) header
      read(3,1020) header
!      write(*,*) 'cli header=',header

!     read WINDGEN header - remove at a later date ?
      do 30 i = 1,7
         read (4,1020) header
   30 continue
      write(2,2000)
      write(2,2027) winfil
      write(2,2230)

!           read CLIGEN and WINDGEN files

 50   read(3,*,err=100,iostat=j) ccd,ccm,ccy,awzdpt,dur,tp,             &
     &                   xmav,awtdmx,awtdmn,rad,wvel,wdir,awtdpt
!      write(*,*) 'cli',ccd,ccm,ccy,awzdpt,dur,tp,
!     &           xmav,awtdmx,awtdmn,rad,wvel,wdir,awtdpt
      read(4,*,err=100,iostat=k) cwd,cwm,cwy,awadir,awudmx,awudmn,awhrmx
!      write(*,*) 'wind',cwd,cwm,cwy,awadir,awudmx,awudmn,awhrmx
!      write (*,*) ' wsum ', awudmx

      if ((j .ne. 0) .or. (k .ne. 0)) then
         write(*,*) 'error reading files j =',j,'  k=',k
         go to 100
      end if

!      ccy = ccy + 1900

      awudav = (awudmx+awudmn)/2.
      awtdav = (awtdmx+awtdmn)/2.
      rad = rad * 0.04186
      if (awudmx .gt. 8.0) ucnt = ucnt+1

      awdair = 348.546 * (1.013 - 0.1183 * (amzele/1000.)               &
     &       + 0.0048 * (amzele/1000.)**2.) / (awtdav + 273.1)
      if (awudav .gt. 8.0) then
        winde = (0.5*awdair*(awudav**2)*(awudav - 8.0))
      end if

!     sum and average
      if (yrfl .eqv. .true.) write (2,2890) cwy
      yrfl = .false.
      if (mofl .eqv. .true.) then
        bd = ccd
        bm = ccm
      end if
      mofl = .false.
      sumdpt = sumdpt + awzdpt
      sumdt = sumdt + awtdav
      sumdir = sumdir + rad
      sumdu = sumdu + awudav
      count = count + 1
      if (awudmx .gt. 8.0) sumwe = sumwe + winde
      last = lstday(ccm, ccy)
      if (ccd .eq. last) then
         ed = ccd
         em = ccm
         avedt = sumdt / count
         avepir = sumdir / count
         awupav = sumdu / count
         avwe = sumwe / count
         write (2,2900) bd,bm,ed,em,sumdpt,avedt,avepir,awupav,ucnt,avwe
         mofl = .true.
         ysumpt = ysumpt + sumdpt
         ysumdt = ysumdt + sumdt
         ysumpi = ysumpi + sumdir
         ysumu = ysumu + sumdu
         ysumwe = ysumwe + sumwe
         ycount = ycount + count
         tucnt = tucnt + ucnt
         sumdpt = 0.0
         sumdt = 0.0
         sumdir = 0.0
         sumdu = 0.0
         sumwe = 0.0
         count = 0
         ucnt = 0
      end if
      if ((ccd .eq. 31) .and. (ccm .eq. 12)) then
         yavedt = ysumdt / ycount
         yavepi = ysumpi / ycount
         awuyav = ysumu / ycount
         yavewe = ysumwe / ycount
         write (2,2910)
         write (2,2920) ysumpt,yavedt,yavepi,awuyav,tucnt,yavewe
         yrfl = .true.
         ysumpt = 0.0
         ysumdt = 0.0
         ysumpi = 0.0
         ysumu = 0.0
         ycount = 0
         tucnt = 0
      end if
      go to 50
  100 write (2,2000)

      close (unit = 2)
      close (unit = 3)
      close (unit = 4)

      return
      end
