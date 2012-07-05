!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!
SUBROUTINE update_hmonth_update_vars(cd,cm)

    USE pd_dates_vars
    USE pd_update_vars
    USE pd_report_vars

    USE pd_var_tables

    IMPLICIT NONE

    INTEGER, INTENT (IN) :: cd  ! current day
    INTEGER, INTENT (IN) :: cm  ! current month

    include "w1clig.inc"        ! precip
    include "p1werm.inc"        ! mntime (maximum # of time steps/day)
    include "w1wind.inc"        ! awu(mntime), awudmx
    include "w1pavg.inc"        ! awdair
    include "m1sim.inc"         ! ntstep (actual # of time steps/day)

    include "h1et.inc"          ! ah0drat (dryness ratio)

    include "h1db1.inc"         ! ahzsnd(s) snow depth in mm

    INTEGER :: i                ! local loop variables
    INTEGER :: s                ! local variable (subregion)

    INTEGER :: hm               ! current hmonth period

    REAL :: we


    hm = (2 * cm) - 1           !1st half of month
    IF (cd > 14) THEN           !2nd half of month
      hm = hm + 1
    END IF

    !variables summed for period
    hmonth_update(Precipi)%val = hmonth_update(Precipi)%val + awzdpt
    hmonth_update(Precipi)%cnt = hmonth_update(Precipi)%cnt + 1

    !variables running averaged for period
    we = 0.0
    IF (awudmx > 8.0) THEN
       DO i = 1, ntstep
          IF (awu(i) > 8.0) THEN
            we = we + 0.5*awdair*(awu(i)**2) * (awu(i) - 8.0) *        &
               (86400.0/ntstep) * (0.001)    ! (s/day) and (J/kJ)
          END IF
       END DO
    END IF
    CALL run_ave(hmonth_update(Wind_energy), we, 1)
    CALL run_ave(hmrot_update(Wind_energy,hm), we, 1)

    CALL run_ave(hmonth_update(Dryness_ratio), ah0drat, 1)
    CALL run_ave(hmrot_update(Dryness_ratio,hm), ah0drat, 1)

    s = 1  !currently have only one subregion
    ! Note that the 20mm depth should be a global parameter
    ! It is currently stuck in erosion.for as a local parameter there
    IF (ahzsnd(s) > 20.0) THEN
       CALL run_ave(hmonth_update(Snow_cover), 1.0, 1)
       CALL run_ave(hmrot_update(Snow_cover,hm), 1.0, 1)
    ELSE
       CALL run_ave(hmonth_update(Snow_cover), 0.0, 1)
       CALL run_ave(hmrot_update(Snow_cover,hm), 0.0, 1)
    END IF

END SUBROUTINE update_hmonth_update_vars


SUBROUTINE update_hmonth_report_vars(cur_day, cur_month, cur_yr, nrot_years)

    USE pd_dates_vars
    USE pd_update_vars
    USE pd_report_vars

    USE pd_var_tables

    IMPLICIT NONE

    INTEGER, INTENT (IN) :: cur_day  
    INTEGER, INTENT (IN) :: cur_month  
    INTEGER, INTENT (IN) :: cur_yr  
    INTEGER, INTENT (IN) :: nrot_years

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
