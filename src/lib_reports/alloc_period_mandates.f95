!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!
FUNCTION alloc_period_mandates (nrot_yrs)

	USE period_mandates_var

	IMPLICIT NONE

	INTEGER :: nrot_yrs
	INTEGER :: alloc_status = 0

	TYPE (mandate_type), POINTER :: alloc_period_mandates

        IF (ALLOCATED (mandate_var)) then
           goto 999     !already allocated
        END IF

	ALLOCATE (mandate_var(nrot_yrs + 50), STAT = alloc_status)
	IF (alloc_status /= 0) THEN
		print *, "Error allocating mandate_var(nrot_yrs + 50)"
	END IF

999	alloc_period_mandates = mandate_var(1)  ! ptr to array

END FUNCTION alloc_period_mandates
