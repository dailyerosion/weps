!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!
! return the total number of periods based upon the number of years
! for the rotation and the dates of the operations.

! Hmm, looks like I have it checking for "end periods".
! We will change it to look for "start periods" and also have
! the regular "start periods" be the 1st and 15th days of each month.
! (we will use day 15 as a starting day since many ops will be
! scheduled then)

FUNCTION get_nperiods (nrot_yrs)

	USE mandate_vars

	IMPLICIT NONE

	INTEGER :: get_nperiods
	INTEGER, INTENT (IN) :: nrot_yrs	! Number of rotation years

	INTEGER :: nperiods	 		! Number of periods
	INTEGER :: minperiods = 24		! Minimum number of periods

	INTEGER :: i				! local loop variable


	! Example Operation Dates (d,m,y) - must be in date order
!	INTEGER, DIMENSION(3,24) :: man_dates =             &
!		RESHAPE ( (/ 1,1,1,  2,1,1,  5,1,1, 14,1,1, 15,1,1, 16, 1,1,   &
!			    31,1,1,  1,2,1,  2,2,1, 13,2,1, 14,2,1, 15, 2,1,   &
!			    28,2,1, 29,2,1,  1,3,1, 23,3,1, 23,3,1, 30,12,1,   &
!			    31,12,1, 1,2,2, 11,3,2,  7,5,2, 18,7,2, 30,12,3 /),&
!			(/ 3,24 /) )

	nperiods = minperiods * nrot_yrs	! Minimum total periods

! This looks for "end dates" rather than "starting dates"
!	DO i = 1, size(mandate) 
!		IF (mandate(i)%y <= nrot_yrs) THEN
!
!			! Check if 1st day of month
!			IF (mandate(i)%d == 1 ) THEN
!				CONTINUE
!			! Check if middle day of month
!			ELSE IF ((mandate(i)%d == 16) .AND.               &
!				 (mandate(i)%m /= 2))        THEN
!				CONTINUE
!			! Check if middle day of month (Feb)
!			ELSE IF ((mandate(i)%d == 15) .AND.               &
!                                 (mandate(i)%m == 2))        THEN
!				CONTINUE
!			! Check if operation date .NE. previous operation date
!			ELSE IF ((i /= 1) .AND.                             &
!				 (mandate(i)%d == mandate(i-1)%d) .AND. &
!	 			 (mandate(i)%m == mandate(i-1)%m) .AND. &
!				 (mandate(i)%y == mandate(i-1)%y)) THEN
!				CONTINUE
!			! We have a new period
!			ELSE
!				nperiods = nperiods + 1
!!				print *, mandate(i)%d,                    &
!!					mandate(i)%m, mandate(i)%y
!			END IF
!		END IF
!	END DO


	! This will look for "start date" matches for elimination
	! of possible new periods.  Also, we will assume the "end date"
	! of normal periods is the 14th day of each month.

	DO i = 1, size(mandate) 
!        print *, "op date: ", mandate(i)%d, mandate(i)%m, mandate(i)%y
	    IF (mandate(i)%y <= nrot_yrs) THEN
	        ! Check if not 1st day of month and
	        ! not middle start period day of month
	        IF ((mandate(i)%d == 1 ) .OR. (mandate(i)%d == 15)) THEN
!                print *, "No new period (day == 1 or day == 15)"
	            CONTINUE               ! no new period here
	        ! Check if operation date .NE. previous operation date
	        ELSE IF (i /= 1) THEN 
                 IF ((mandate(i)%d == mandate(i-1)%d) .AND.         &
	 	         (mandate(i)%m == mandate(i-1)%m) .AND.     &
	             (mandate(i)%y == mandate(i-1)%y)) THEN
                    print *, "No new period (2nd op on this date)"
	                 CONTINUE               ! no new period here
	         ELSE                       ! We must have a new period
	            nperiods = nperiods + 1
                    print *, "new period start date", nperiods,    &
                    mandate(i)%d, mandate(i)%m, mandate(i)%y
                 END IF
                ELSE
                  nperiods = nperiods +1
                  print *, "first new period start date", nperiods,    &
                  mandate(i)%d, mandate(i)%m, mandate(i)%y
	        END IF
	    END IF
	END DO

	get_nperiods = nperiods

END FUNCTION get_nperiods
