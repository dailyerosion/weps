!$Author$
!$Date$
!$Revision$
!$HeadURL$

SUBROUTINE update_hmonth_update_vars(isr, cd, cm, hmonth_update, hmrot_update, h1et)

    USE pd_var_type_def, only: pd_var_type
    USE pd_var_tables
    use erosion_data_struct_defs, only: subday, awudmx, awdair, ntstep
    use hydro_data_struct_defs, only: hydro_derived_et

    IMPLICIT NONE

    INTEGER, intent (in) :: isr  ! current subregion
    INTEGER, INTENT (IN) :: cd  ! current day
    INTEGER, INTENT (IN) :: cm  ! current month
    TYPE (pd_var_type), DIMENSION(Min_hmonth_vars:), intent(inout) :: hmonth_update
    TYPE (pd_var_type), DIMENSION(Min_hmonth_vars:,:), intent(inout) :: hmrot_update
    type(hydro_derived_et), intent(in) :: h1et

    include "w1clig.inc"        ! precip
    include "p1werm.inc"        ! mntime (maximum # of time steps/day)

    include "h1db1.inc"         ! ahzsnd(s) snow depth in mm

    INTEGER :: i                ! local loop variables

    INTEGER :: hm               ! current hmonth period

    REAL :: we

    ! Threshold value for determining erosive wind energy (m/s)
    REAL, PARAMETER :: wind_energy_thresh = 8.0
    ! Threshold value for determining protective snow depth (mm)
    REAL, PARAMETER :: snow_depth_thresh = 20.0

    hm = (2 * cm) - 1           !1st half of month
    IF (cd > 14) THEN           !2nd half of month
      hm = hm + 1
    END IF

    !variables summed for period
    hmonth_update(Precipi)%val = hmonth_update(Precipi)%val + awzdpt
    hmonth_update(Precipi)%cnt = hmonth_update(Precipi)%cnt + 1

    !variables running averaged for period
    we = 0.0
    IF (awudmx > wind_energy_thresh) THEN
       DO i = 1, ntstep
          IF (subday(i)%awu > 8.0) THEN
            we = we + 0.5*awdair*(subday(i)%awu**2) * (subday(i)%awu - 8.0) *        &
               (86400.0/ntstep) * (0.001)    ! (s/day) and (J/kJ)
          END IF
       END DO
    END IF
    CALL run_ave(hmonth_update(Wind_energy), we, 1)
    CALL run_ave(hmrot_update(Wind_energy,hm), we, 1)

    CALL run_ave(hmonth_update(Dryness_ratio), h1et%drat, 1)
    CALL run_ave(hmrot_update(Dryness_ratio,hm), h1et%drat, 1)

    ! Note that the 20mm depth should be a global parameter
    ! It is currently stuck in erosion.for as a local parameter there
    IF (ahzsnd(isr) > snow_depth_thresh) THEN
       CALL run_ave(hmonth_update(Snow_cover), 1.0, 1)
       CALL run_ave(hmrot_update(Snow_cover,hm), 1.0, 1)
    ELSE
       CALL run_ave(hmonth_update(Snow_cover), 0.0, 1)
       CALL run_ave(hmrot_update(Snow_cover,hm), 0.0, 1)
    END IF

END SUBROUTINE update_hmonth_update_vars


SUBROUTINE update_hmonth_report_vars(cur_day, cur_month, cur_yr, nrot_years, hmonth_update, hmrot_update, hmonth_report)

    USE pd_var_type_def
    USE pd_var_tables

    IMPLICIT NONE

    INTEGER, INTENT (IN) :: cur_day  
    INTEGER, INTENT (IN) :: cur_month  
    INTEGER, INTENT (IN) :: cur_yr  
    INTEGER, INTENT (IN) :: nrot_years
    TYPE (pd_var_type), DIMENSION(Min_hmonth_vars:), intent(inout) :: hmonth_update
    TYPE (pd_var_type), DIMENSION(Min_hmonth_vars:,:), intent(inout) :: hmrot_update
    TYPE (pd_var_type), DIMENSION(Min_hmonth_vars:,:,0:), intent(inout) :: hmonth_report

    INTEGER :: rot_y

    INTEGER :: i, hm    ! local loop variables

    rot_y = mod(cur_yr-1,nrot_years)+1

    ! determine the half-month period
    hm = 2 * cur_month - 1
    IF (cur_day > 14) THEN  !2nd half of month
      hm = hm + 1
    END IF

! print *, "update_hmonth_vars(y,m,d,ry,hm):", cur_yr, cur_month, cur_day, rot_y, hm 

    !variables averaged for reporting period

    DO i=Min_cli_vars, Max_cli_vars
       CALL run_ave (hmonth_report(i,hm,rot_y), hmonth_update(i)%val, 1)
    END DO

    ! update the full rotation average variables
    IF (rot_y == nrot_years) THEN
      DO i=Min_cli_vars, Max_cli_vars
         CALL run_ave (hmonth_report(i,hm,0), hmonth_update(i)%val, nrot_years)
      END DO
    END IF


    ! reset hmonth update vars
    DO i=Min_hmonth_vars, Max_hmonth_vars
       hmonth_update(i)%cnt = 0
       hmonth_update(i)%val = 0.0
    END DO
!   hmonth_dates(hm,rot_y)%ey = hmonth_dates(hm,rot_y)%ey + 1

    ! reset hmonth rotation update vars
    IF (rot_y == nrot_years) THEN
       DO i=Min_hmonth_vars,Max_hmonth_vars
          hmrot_update(i,hm)%cnt = 0
          hmrot_update(i,hm)%val = 0.0
       END DO
!      hmonth_dates(hm,0)%ey = hmonth_dates(hm,0)%ey + nrot_years
    END IF

END SUBROUTINE update_hmonth_report_vars
