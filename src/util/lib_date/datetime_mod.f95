!$Author$
!$Date$
!$Revision$
!$HeadURL$

module datetime_mod
  implicit none
  private
    
  type date_time_numbers_strings
    integer :: dt(8)            ! values as read in from the date_and_time fortran subroutine
                                ! (1) - year
                                ! (2) - month of year
                                ! (3) - day of month
                                ! (4) - time difference from UTC (minutes)
                                ! (5) - hour of day (0-23)
                                ! (6) - minute of hour (0-59)
                                ! (7) - second of minute (0-59)
                                ! (8) - milliseconds of second (0-999)
    character(len=3) :: mstring ! three letter string for the name of the month
    integer :: julian_day
    integer :: day_of_year
  end type date_time_numbers_strings


  type(date_time_numbers_strings) :: sys_time
  save sys_time
  type(date_time_numbers_strings) :: sim_date
  save sim_date

  public :: update_system_time

  interface get_systime
      module procedure get_systime_day_month_year
      module procedure get_systime_mname_dyhms
      module procedure get_systime_jday_doy
  end interface get_systime

  public :: get_systime
  public :: get_systime_string

!  public :: update_simulation_date

!  public interface get_simdate
!      module procedure get_simdate_day_month_year
!      module procedure get_simdate_mname_day_year
!      module procedure get_simdate_jday_doy
!  end interface get_simdate

contains

    subroutine update_system_time
      ! Determine date of Run from system clock
      call date_and_time(values=sys_time%dt)
      ! set three letter abbreviation for month of year
      sys_time%mstring = find_month_string( sys_time%dt(2) )
      ! set julian_day
      sys_time%julian_day = julday( sys_time%dt(3), sys_time%dt(2), sys_time%dt(1) )
      ! set day_of_year
      sys_time%day_of_year = dayear( sys_time%dt(3), sys_time%dt(2), sys_time%dt(1) )
    end subroutine update_system_time

    subroutine get_systime_day_month_year( day, month, year )
      integer, intent(out) :: day, month, year
      ! call internal routine with time desired
      call get_time_day_month_year(sys_time, day, month, year)
    end subroutine get_systime_day_month_year

    subroutine get_systime_mname_dyhms( mname, day, year, hour, minute, second )
      character(len=3), intent(out) :: mname ! three letter string for the name of the month
      integer, intent(out) :: day, year, hour, minute, second
      call get_time_mname_dyhms(sys_time, mname, day, year, hour, minute, second)
    end subroutine get_systime_mname_dyhms

    subroutine get_systime_jday_doy( julian_day, day_of_year )
      integer, intent(out) :: julian_day, day_of_year
      call get_time_jday_doy( sys_time, julian_day, day_of_year )
    end subroutine get_systime_jday_doy

    function get_systime_string() result( systime_string )
      character(len=21) :: systime_string
      character(len=3) :: mname ! three letter string for the name of the month
      integer :: day, year, hour, minute, second
      call get_time_mname_dyhms(sys_time, mname, day, year, hour, minute, second)
      write( systime_string, "(a3,' ',i2.2,', ',i4,' ', i2.2,':',i2.2,':',i2.2)" ) mname, day, year, hour, minute, second
    end function get_systime_string

    subroutine get_simdate_day_month_year( day, month, year )
      integer, intent(out) :: day, month, year
      ! call internal routine with time desired
      call get_time_day_month_year( sim_date, day, month, year )
    end subroutine get_simdate_day_month_year

    subroutine get_simdate_mname_day_year( mname, day, year )
      character(len=3), intent(out) :: mname ! three letter string for the name of the month
      integer, intent(out) :: day, year
      integer :: hour, minute, second
      call get_time_mname_dyhms( sim_date, mname, day, year, hour, minute, second )
    end subroutine get_simdate_mname_day_year

    subroutine get_simdate_jday_doy( julian_day, day_of_year )
      integer, intent(out) :: julian_day, day_of_year
      call get_time_jday_doy( sim_date, julian_day, day_of_year )
    end subroutine get_simdate_jday_doy

    subroutine get_time_day_month_year( datetime, day, month, year )
      type(date_time_numbers_strings), intent(in) :: datetime
      integer, intent(out) :: day, month, year
      ! retrieve values from datetime structure
      day = datetime%dt(3)
      month = datetime%dt(2)
      year = datetime%dt(1)
    end subroutine get_time_day_month_year

    subroutine get_time_mname_dyhms( datetime, mname, day, year, hour, minute, second )
      type(date_time_numbers_strings), intent(in) :: datetime
      character(len=3), intent(out) :: mname ! three letter string for the name of the month
      integer, intent(out) :: day, year, hour, minute, second
      mname = datetime%mstring
      day = datetime%dt(3)
      year = datetime%dt(1)
      hour = datetime%dt(5)
      minute = datetime%dt(6)
      second = datetime%dt(7)
    end subroutine get_time_mname_dyhms

    subroutine get_time_jday_doy( datetime, julian_day, day_of_year )
      type(date_time_numbers_strings), intent(in) :: datetime
      integer, intent(out) :: julian_day, day_of_year
      julian_day = datetime%julian_day
      day_of_year = datetime%day_of_year
    end subroutine get_time_jday_doy

    function find_month_string( num_month ) result( mstring )
      integer, intent(in) :: num_month
      character(len=3) :: mstring

      ! Determine month of year
      select case (num_month)
        case (1); mstring = "Jan"
        case (2); mstring = "Feb"
        case (3); mstring = "Mar"
        case (4); mstring = "Apr"
        case (5); mstring = "May"
        case (6); mstring = "Jun"
        case (7); mstring = "Jul"
        case (8); mstring = "Aug"
        case (9); mstring = "Sep"
        case (10); mstring = "Oct"
        case (11); mstring = "Nov"
        case (12); mstring = "Dec"
        case default; mstring = "???"
      end select
    end function find_month_string

    function julday( dd, mm, yyyy ) result( julian_day )

!     + + + PURPOSE + + +
!     In this routine JULDAY returns the Julian Day Number which begins at
!     noon of the gregorian calendar date specified by day "dd", month "mm", & year "yyyy"
!     All are integer variables. Positive year signifies A.D.; zero and negative, B.C.
!     Calendar dates before 1582 will not match dates on the Julian calendar used
!     at the time.

!     JULDAY is taken from _Numerical_Recipes:_The_Art_of_Scientific_Computing_

!     problems were found with the method above for long runs such as:
!     - the ten missing days in 1582 (we really just need 365.25 day in each year)
!     - after 1700, leap years return feb 31, not 29 and the wrong year
!     - it may only be the fortran implementation and floating point problems

!     Based on info from http://en.wikipedia.org/wiki/Julian_day, which references
!     http://www.astro.uu.nl/~strous/AA/en/reken/juliaansedag.html, the code
!     was revised to use the Astronomical Gregorian calendar, which takes the
!     present pattern of leap years back into the past. This is ideal for
!     our purposes with no year getting short changed. Integer math method
!     is taken from Wikipedia article.

!     + + + ARGUMENT DECLARATIONS + + +
      integer, intent(in) :: dd     ! integer value of day in the range 1-31
      integer, intent(in) :: mm     !                month in the range 1-12
      integer, intent(in) :: yyyy   !                 year (negative B.C., positive A.D.)

      integer :: julian_day

      julian_day = (1461 * (yyyy + 4800 + (mm - 14)/12))/4 &
                 + (367 * (mm - 2 - 12 * ((mm - 14)/12)))/12 &
                 - (3 * ((yyyy + 4900 + (mm - 14)/12)/100))/4 + dd - 32075

      end function julday

      function difdat( d1, m1, yyy1, d2, m2, yyy2 ) result( diff )

!     + + + PURPOSE + + +
!     Two dates are passed to this function and the number of days between
!     them is returned. The important thing to remember here is that the
!     first date is subtracted _from_ the second. 
!     Example:
!        d1 m1 yyy1    d2 m2 yyy2   returns   meaning
!        01 01 1992    02 01 1992   1         1 day from 01/01/1992 it will
!                                             be 02/01/1992
!        02 01 1992    01 01 1992   -1        -1 day from 02/01/1992 (or 1
!                                             day ago) it was 01/01/1992

!     + + + ARGUMENT DECLARATIONS + + + 
      integer, intent(in) :: d1     ! day 
      integer, intent(in) :: m1     ! month
      integer, intent(in) :: yyy1   ! year
      integer, intent(in) :: d2     ! day
      integer, intent(in) :: m2     ! month
      integer, intent(in) :: yyy2   ! year

      integer :: diff

      diff = julday (d2, m2, yyy2) - julday (d1, m1, yyy1)

      end function difdat

      function dayear( dd, mm, yyyy ) result( day_of_year )

!     + + + PURPOSE + + +
!     Given a date in dd/mm/yyyy format,
!     dayear will return the number of days
!     from the first of that year.

!     + + + ARGUMENT DECLARATIONS + + + 
      integer, intent(in) :: dd     ! day
      integer, intent(in) :: mm     ! month
      integer, intent(in) :: yyyy   ! year

      integer :: day_of_year

!     Get the difference in days + 1
      day_of_year = difdat( 1, 1, yyyy, dd, mm, yyyy ) + 1

      end function dayear

end module datetime_mod
