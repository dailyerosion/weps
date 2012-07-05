!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!
MODULE pd_update_vars

! The purpose of the derived-type "update" variables is to
! keep track of selected variable values over a specified period
! of time.  This may include summation, running average, beginning
! period value, median, last period value, etc.

! Once the "update" variable has been "filled" after reaching
! the end of it's specified period, the value is passed on to the
! selected period "report" variables for each particular parameter.


    USE pd_var_type_def

    IMPLICIT NONE

    ! Specify "pd_update_vars" variable structures
    ! (rotation year, months per rotation year, and half-months per rotation year)
    TYPE (pd_var_type), DIMENSION(:), TARGET, ALLOCATABLE :: yrly_update
    TYPE (pd_var_type), DIMENSION(:), TARGET, ALLOCATABLE :: monthly_update
    TYPE (pd_var_type), DIMENSION(:), TARGET, ALLOCATABLE :: hmonth_update
    TYPE (pd_var_type), DIMENSION(:), TARGET, ALLOCATABLE :: period_update
    TYPE (pd_var_type), DIMENSION(:), TARGET, ALLOCATABLE :: yr_update
    ! "Rotation length" update variable structures
    ! (yrs, months, and half-months across rotation years)
    TYPE (pd_var_type), DIMENSION(:), TARGET, ALLOCATABLE :: yrot_update
    TYPE (pd_var_type), DIMENSION(:,:), TARGET, ALLOCATABLE :: mrot_update
    TYPE (pd_var_type), DIMENSION(:,:), TARGET, ALLOCATABLE :: hmrot_update

END MODULE pd_update_vars
