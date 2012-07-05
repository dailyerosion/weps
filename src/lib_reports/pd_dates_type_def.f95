!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!
MODULE pd_dates_type_def

! This derived-type definition contains variables to represent
! a "period of time", based upon a "start date" and an
! "ending date" (specified by day, month, and year variables).


! Note that there is no guarantee that the day, month, and year
! variables represent an actual valid date (e.g., day 45, month 13,
! year -21, etc.).

! Also, there is no expectation that the "period" is to consist
! of a constant length time frame.  If the start date was:
! day 1, month 2, and the end date was: day 1, month 3, with
! the start and end year values being equal, the number of days
! represented would depend on whether the year was a leap year or not.

	IMPLICIT NONE

	TYPE :: pd_dates_type
		INTEGER	:: sd, sm, sy	! Start date
		INTEGER	:: ed, em, ey	! End date
	END TYPE pd_dates_type

END MODULE pd_dates_type_def
