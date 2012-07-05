!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!
MODULE period_mandates_var

	USE period_mandates_table_def

	IMPLICIT NONE

	! Specify the "mandate var" structure
	TYPE (mandate_type), DIMENSION(:), TARGET, ALLOCATABLE :: mandate_var

END MODULE period_mandates_var
