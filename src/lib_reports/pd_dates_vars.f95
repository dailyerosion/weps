!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!
! The purpose of the derived-type "dates" variables is to
! to keep track of the time period (duration) that a variable
! is actively being tracked (summed, averaged, etc.)

MODULE pd_dates_vars

    USE pd_dates_type_def

    ! "pd_dates" structures used by "pd_update" structures
    TYPE (pd_dates_type),DIMENSION(:), TARGET, ALLOCATABLE :: yrly_dates
    TYPE (pd_dates_type),DIMENSION(:,:),TARGET, ALLOCATABLE :: monthly_dates
    TYPE (pd_dates_type),DIMENSION(:,:), TARGET, ALLOCATABLE :: hmonth_dates
    TYPE (pd_dates_type),DIMENSION(:), TARGET, ALLOCATABLE :: period_dates
    TYPE (pd_dates_type),DIMENSION(:), TARGET, ALLOCATABLE :: yr_dates

END MODULE pd_dates_vars
