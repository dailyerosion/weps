!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!
MODULE period_mandates_table_def

	IMPLICIT NONE

	! Table of management operation dates to use for testing
	TYPE :: mandate_type
		INTEGER :: d, m, y
	END TYPE mandate_type

END MODULE period_mandates_table_def
