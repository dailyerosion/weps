!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!
MODULE pd_report_vars

! The derived-type "pd_report_vars" variables are intended to update
! and store the values of specified parameters based upon informaton
! passed to it from the "pd_update_vars" derived-type variables.
! The "report" variables may be storing a sum, running average,
! median, etc. values of the specified parameters.

    USE pd_var_type_def

    IMPLICIT NONE

    ! Specify "period var" structures
    TYPE (pd_var_type),DIMENSION(:,:),TARGET,ALLOCATABLE :: yrly_report
    TYPE (pd_var_type),DIMENSION(:,:,:),TARGET,ALLOCATABLE :: monthly_report
    TYPE (pd_var_type),DIMENSION(:,:,:),TARGET,ALLOCATABLE :: hmonth_report
    TYPE (pd_var_type),DIMENSION(:,:),TARGET,ALLOCATABLE :: period_report
    TYPE (pd_var_type),DIMENSION(:,:),TARGET,ALLOCATABLE :: yr_report

! NOTE:  These already exist because of how the previous variables are defined.
!        See "alloc_pd_vars.f95"
    ! yrot_report(i) == yrly_report(i,0)
    ! mrot_report(i,m) == monthly_report(i,m,0)
    ! hmrot_report(i,hm) == hmonth_report(i,hm,0)

!   ! "Rotation length" structures
!   ! (yrly, month, and half-month averages across entire rotation)
!   TYPE (pd_var_type),DIMENSION(:,:),TARGET,ALLOCATABLE :: yrot_report
!   TYPE (pd_var_type),DIMENSION(:,:,:),TARGET,ALLOCATABLE :: mrot_report
!   TYPE (pd_var_type),DIMENSION(:,:,:),TARGET,ALLOCATABLE :: hmrot_report

END MODULE pd_report_vars
