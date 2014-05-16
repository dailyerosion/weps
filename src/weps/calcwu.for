!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine calcwu

!     + + + PURPOSE + + +
!    This subroutine reads sub-daily wind speeds from a user supplied
!    file or simulates the sub-daily wind speeds if the file is not
!    supplied.

!    programmer: John Tatarko
!    version: 07/28/92

!     Edit History
!     07-Mar-99   wjr   changed unit 8 to luiwsd

!     + + + KEY WORDS + + +
!     wind speed, wind direction, sub-daily wind speed

      use datetime_mod, only: julday, get_simdate
      use file_io_mod, only: luiwsd, luo_subday
      use p1unconv_mod, only: pi
      use erosion_data_struct_defs, only: awadir, awhrmx, awudmx, awudmn
      use erosion_data_struct_defs, only: awudav, subday, ntstep, am0efl
      use climate_input_mod, only: wind_gen_fmt_flag

!     + + + GLOBAL COMMON BLOCKS + + +
      include 'p1werm.inc'
      include 'm1sim.inc'    ! am0jd
!      include 'm1flag.inc'   ! wind_gen_fmt_flag

!     + + + LOCAL COMMON BLOCKS + + +
      include 'main/main.inc'  ! subfil, subflg

!     + + + LOCAL VARIABLES + + +
      character line*80

      logical fexist

      integer day, i, jd, month, year

!      real    large,
!     r        small

!     + + + LOCAL DEFINITIONS + + +

!   i         - Index on subdaily loop (i=1,ntstep)
!   day       - The current day in the sub-daily wind file.
!   month     - The current month in the sub-daily wind file.
!   year      - The current year in the sub-daily wind file.
!   large     - Variable initialized with small value so that MAX
!               intrinsic function may find the maximum windspeed.
!   line      - This character variable is used to read the header
!               information in the file.
!   small     - Variable initialized with large value so that MIN
!               intrinsic function may find the minimum windspeed.
!   subfil    - This variable holds the subdaily wind information
!               file name.
!   subflg    - This logical variable is a flag to read header
!               information in the sub-daily wind file
!               (if .true., read header; if .false., skip).

!     + + + FUNCTIONS CALLED + + +
!      integer julday

!     + + + FUNCTION DEFINITIONS + + +
!     julday  -  Calculates the julian date given the day, month, year

!     + + + OUTPUT FORMATS + + +
 2000 format (2i2,2x,i4,1x,f6.1,24f6.2)
 2005 format (/,' using subdaily wind file:  ',a80)
 2010 format (/,' error reading subdaily wind file: ',a80)
 2020 format (/,' no subdaily wind in file for',2i3,1x,i4,' - it will be&
     & generated')

!     + + + END SPECIFICATIONS + + +

!     + + + INITIALIZATION + + +

!      small = 1e20
!      large = -1e20

!    if 'real' sub-daily data exists - read it
      inquire (file = subfil, exist = fexist)
      if (fexist) then
         write(*,2005) subfil
         if (subflg .eqv. .false.) go to 30
   20    read (luiwsd,'(a)') line
         if (line(1:1) .eq. '#') go to 20
         backspace (unit = luiwsd)
         subflg = .false.
   30    read (luiwsd, *, end = 80, err = 90) day, month, year, awadir, &
     &                                   (subday(i)%awu, i = 1,ntstep)

!        test for current date
!        write (*,*) 'julday',day, month,year
         jd = julday (day, month, year)
         if (jd .ne. am0jd) then
            write(*,2020)
            go to 50
         end if

!        find max, min, and avg
!         do 40 i = 1, ntstep
!            large = max(large, subday(i)%awu)
!            small = min(small, subday(i)%awu)
!   40    continue
!         awudmx = large
!         awudmn = small
!         awudav = (awudmx + awudmn) / 2.

         go to 600
   80    backspace (unit= luiwsd)
      end if

!     if 'real' data does not exist - generate (for original wind_gen data only)
   50 if (wind_gen_fmt_flag == 1) then   ! original wind_gen file format
         do 60 i = 1, ntstep
            subday(i)%awu = awudav + 0.5 * (awudmx - awudmn) *          &
     &      cos( 2.*pi * ((ntstep * 24) / ntstep - awhrmx + i) / ntstep)
   60    continue
      else                               ! wind_gen2/3 file format
         !subday(i)%awu array should already be populated
         if (ntstep .ne. 24) then
            write(0,*) 'ntstep not equal to 24 - code changes needed!'
            goto 500
         endif
      endif
      go to 600

!     if error reading sub-daily file
   90 write (0,2010) subfil
  500 call exit (1)
! 500 stop

  600 call get_simdate (day,month,year)
      if( am0efl.gt.0) then
        write(luo_subday,2000)                                          &
     &        day, month, year, awadir, (subday(i)%awu,i=1,ntstep)
      endif

      return
      end
