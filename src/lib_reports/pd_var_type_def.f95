!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!
! The "cnt" variable may be used to track the current day
! within the "period of time" (offset from starting date), or
! be used as a counter of the number of objects (values) being
! summed, averaged, etc. by other derived-type variables including
! this one.

MODULE pd_var_type_def

! This derived-type definition contains a "val" variable,
! a "cnt" variable, and a pointer to another derived-type
! variable which contains  variables to represent a
! "period of time", based upon a "start date" and an
! "ending date" (specified by day, month, and year variables).

! The "val" variable can contain the sum, running average, etc.
! of any selected object.

! The "cnt" variable may be used to track the current of day
! within the "period of time" (offset from starting date), or
! be used as a counter of the number of objects (values) being
! summed, averaged, etc.

! Note that there is no guarantee that the day, month, and year
! variables represent an actual valid date (e.g., day 45, month 13,
! year -21, etc.).

! Also, there is no expectation that the "period" is to consist
! of a constant length time frame.  If the start date was:
! day 1, month 2, and the end date was: day 1, month 3, with
! the start and end year values being equal, the number of days
! represented would depend on whether the year was a leap year or not.


! Two intended uses of this derived type are:
!
! 1.  Track a specific variable's value during the "time period"
!     specified.  For example, the "start and end dates" determine
!     the time frame the value is to be summed, runnning averaged, etc.
!     The "cnt" variable can keep track of the number of variable values 
!     being considered during the duration of the "valid time period".
!
! 2.  Track a specific variable's value over some given period (e.g.
!     many years to determine a yearly average of the variable).
!     The variable's yearly values being considered may only pertain
!     to a specific "time period" which can be represented by the
!     "start and end dates".  The "cnt" variable, in this case
!     would be keeping track of the number of "valid period" values
!     (number of years in this case) for each year being considered
!     in the yearly average estimate.

  USE pd_dates_type_def  ! definition of pd_dates_type var

  IMPLICIT NONE

  TYPE :: pd_var_type
    REAL    :: val
    INTEGER :: cnt
    TYPE (pd_dates_type), POINTER :: date 
  END TYPE pd_var_type

END MODULE pd_var_type_def
