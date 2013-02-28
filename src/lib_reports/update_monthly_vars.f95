!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!
SUBROUTINE update_monthly_update_vars(cm, monthly_update, mrot_update)

    USE pd_var_type_def
    USE pd_var_tables

    IMPLICIT NONE

    INTEGER, INTENT (IN) :: cm  ! current month
    TYPE (pd_var_type), DIMENSION(:), intent(inout) :: monthly_update
    TYPE (pd_var_type), DIMENSION(:,:), intent(inout) :: mrot_update

    include "w1clig.inc"        ! precip
    include "p1werm.inc"        ! mntime (maximum # of time steps/day)
    include "w1wind.inc"        ! awu(mntime), awudmx
    include "w1pavg.inc"        ! awdair
    include "m1sim.inc"         ! ntstep (actual # of time steps/day)
    include "m1geo.inc"         ! sim_area - area of simulation region (m^2)

    include "h1et.inc"          ! ah0drat (dryness ratio)

    include "h1db1.inc"         ! ahzsnd(s) snow depth in mm

    include "erosion/m2geo.inc" ! imax, jmax, ix, jy of simulation grid
    include "erosion/e2erod.inc"! egt, egtcs, egtss, egt10

    INTEGER :: s                ! local variable (subregion)
    INTEGER :: i,j              ! local loop variables
    INTEGER :: ngdpt            ! number of simulation grid datapoints
    REAL    :: gdpt_area        ! area of a grid cell (point) in m^2

    INTEGER :: cnt              ! number of simulation grid datapoints
    REAL    :: sum_salt_loss
    REAL    :: sum_salt_dep

    INTEGER :: cnt_eros          ! number of simulation grid datapoints with net erosion
    INTEGER :: cnt_dep           ! number of simulation grid datapoints with net deposition

    INTEGER :: cnt_transp       ! number of simulation grid datapoints at TC
    INTEGER :: cnt_sheltered    ! number of simulation grid datapoints sheltered

    REAL :: we

    REAL, PARAMETER :: m2_to_ha = 10000.0  ! m^2 in a ha

    ! Threshold value for determining erosive wind energy (m/s)
    REAL, PARAMETER :: wind_energy_thresh = 8.0
    ! Threshold value for determining protective snow depth (mm)
    REAL, PARAMETER :: snow_depth_thresh = 20.0

    ! Threshold value for determining erosion loss and deposition regions
    REAL, PARAMETER :: eros_thresh = 0.025 !kg/m^2
    ! Threshold value for determining sheltered regions
    REAL, PARAMETER :: susp_thresh = 0.001 !kg/m^2

    ! Flag to specify whether we have experienced an erosion event or not.
    ! It is set in the "Salt_loss2" section and used in the "Trans_cap"
    ! and "Sheltered" code sections.
    LOGICAL :: Have_Erosion
    LOGICAL :: Have_Deposition

    Have_Erosion    = .FALSE.   ! Initialize for each invocation of routine
    Have_Deposition = .FALSE.   ! Initialize for each invocation of routine

    ngdpt = (imax-1) * (jmax-1)  !Number of grid cells
    gdpt_area = sim_area/ngdpt   !Area of single grid cell

    !variables summed for period
    monthly_update(Precipi)%val = monthly_update(Precipi)%val + awzdpt
    monthly_update(Precipi)%cnt = monthly_update(Precipi)%cnt + 1
    mrot_update(Precipi,cm)%val = mrot_update(Precipi,cm)%val + awzdpt
    mrot_update(Precipi,cm)%cnt = mrot_update(Precipi,cm)%cnt + 1

! ------------------------------------------------------------------------------------------------------------------
    ! Determine if we have any net soil loss occurring from any grid cell (erosion)
    ! We assume that we don't have any net suspension loss if we don't have any net salt/creep loss
    sum_salt_loss = 0.0; cnt_eros = 0
    DO i = 1, imax-1 
       DO j = 1, jmax-1 
          IF ((egt(i,j) - egtss(i,j)) < -eros_thresh) THEN
             sum_salt_loss = sum_salt_loss + (egt(i,j) - egtss(i,j))
             cnt_eros = cnt_eros + 1
          END IF
       END DO
    END DO
    IF (cnt_eros /= 0) Have_Erosion = .TRUE.  !We have erosion occurring, set flag for use later

   ! Determine if we have any net soil deposition occurring from any grid cell (deposition)
    ! We assume that we don't have any net suspension deposition if we don't have any mry salt/creep deposition
    sum_salt_dep = 0.0; cnt_dep = 0
    DO i = 1, imax-1 
       DO j = 1, jmax-1 
          IF ((egt(i,j) - egtss(i,j)) > eros_thresh) THEN
             sum_salt_dep = sum_salt_dep + (egt(i,j) - egtss(i,j))
             cnt_dep = cnt_dep + 1
          END IF
       END DO
    END DO
    IF (cnt_dep /= 0) Have_Deposition = .TRUE.  !We have deposition occurring, set flag for use later
! ------------------------------------------------------------------------------------------------------------------

    DO i = 1, imax-1 
       DO j = 1, jmax-1 
          monthly_update(Eros_loss)%val = monthly_update(Eros_loss)%val + egt(i,j)/ngdpt
          monthly_update(Salt_loss)%val = monthly_update(Salt_loss)%val + (egt(i,j) - egtss(i,j))/ngdpt
          monthly_update(Susp_loss)%val = monthly_update(Susp_loss)%val + egtss(i,j)/ngdpt
          monthly_update(PM10_loss)%val = monthly_update(PM10_loss)%val + egt10(i,j)/ngdpt

          mrot_update(Eros_loss,cm)%val = mrot_update(Eros_loss,cm)%val + egt(i,j)/ngdpt
          mrot_update(Salt_loss,cm)%val = mrot_update(Salt_loss,cm)%val + (egt(i,j) - egtss(i,j))/ngdpt
          mrot_update(Susp_loss,cm)%val = mrot_update(Susp_loss,cm)%val + egtss(i,j)/ngdpt
          mrot_update(PM10_loss,cm)%val = mrot_update(PM10_loss,cm)%val + egt10(i,j)/ngdpt
       END DO
    END DO
    monthly_update(Eros_loss)%cnt = monthly_update(Eros_loss)%cnt + 1
    monthly_update(Salt_loss)%cnt = monthly_update(Salt_loss)%cnt + 1
    monthly_update(Susp_loss)%cnt = monthly_update(Susp_loss)%cnt + 1
    monthly_update(PM10_loss)%cnt = monthly_update(PM10_loss)%cnt + 1

    mrot_update(Eros_loss,cm)%cnt = mrot_update(Eros_loss,cm)%cnt + 1
    mrot_update(Salt_loss,cm)%cnt = mrot_update(Salt_loss,cm)%cnt + 1
    mrot_update(Susp_loss,cm)%cnt = mrot_update(Susp_loss,cm)%cnt + 1
    mrot_update(PM10_loss,cm)%cnt = mrot_update(PM10_loss,cm)%cnt + 1

    IF (Have_Erosion) THEN !We have erosion somewhere
       monthly_update(N_eros_events)%val = monthly_update(N_eros_events)%val + 1.0  ! Count the erosion events
       mrot_update(N_eros_events,cm)%val = mrot_update(N_eros_events,cm)%val + 1.0  ! Count the erosion events
    END IF
    monthly_update(N_eros_events)%cnt = monthly_update(N_eros_events)%cnt + 1
    mrot_update(N_eros_events,cm)%cnt = mrot_update(N_eros_events,cm)%cnt + 1


    IF (Have_Erosion) THEN
 
       monthly_update(Salt_loss2)%val = monthly_update(Salt_loss2)%val + sum_salt_loss/ngdpt
       monthly_update(Salt_loss2)%cnt = monthly_update(Salt_loss2)%cnt + 1
       monthly_update(Salt_loss2_mass)%val = monthly_update(Salt_loss2_mass)%val + (sum_salt_loss*gdpt_area)
       monthly_update(Salt_loss2_mass)%cnt = monthly_update(Salt_loss2_mass)%cnt + 1
       CALL run_ave (monthly_update(Salt_loss2_area), REAL(cnt_eros)*gdpt_area/m2_to_ha, 1)
       CALL run_ave (monthly_update(Salt_loss2_frac), REAL(cnt_eros)/ngdpt, 1)
       !compute as: Salt_loss2_mass/(Salt_loss2_area * m2_to_ha)
       monthly_update(Salt_loss2_rate)%val =                              &
            monthly_update(Salt_loss2_mass)%val /                         &
            (monthly_update(Salt_loss2_area)%val * m2_to_ha)
       monthly_update(Salt_loss2_rate)%cnt = monthly_update(Salt_loss2_rate)%cnt + 1

       mrot_update(Salt_loss2,cm)%val = mrot_update(Salt_loss2,cm)%val + sum_salt_loss/ngdpt
       mrot_update(Salt_loss2,cm)%cnt = mrot_update(Salt_loss2,cm)%cnt + 1
       mrot_update(Salt_loss2_mass,cm)%val = mrot_update(Salt_loss2_mass,cm)%val + (sum_salt_loss*gdpt_area)
       mrot_update(Salt_loss2_mass,cm)%cnt = mrot_update(Salt_loss2_mass,cm)%cnt + 1
       CALL run_ave (mrot_update(Salt_loss2_area,cm), REAL(cnt_eros)*gdpt_area/m2_to_ha, 1)
       CALL run_ave (mrot_update(Salt_loss2_frac,cm), REAL(cnt_eros)/ngdpt, 1)
       !compute as: Salt_loss2_mass/(Salt_loss2_area * m2_to_ha)
       mrot_update(Salt_loss2_rate,cm)%val =                              &
            mrot_update(Salt_loss2_mass,cm)%val /                         &
            (mrot_update(Salt_loss2_area,cm)%val * m2_to_ha)
       mrot_update(Salt_loss2_rate,cm)%cnt = mrot_update(Salt_loss2_rate,cm)%cnt + 1
    END IF


    IF (Have_Deposition) THEN
       monthly_update(Salt_dep2)%val = monthly_update(Salt_dep2)%val + sum_salt_dep/ngdpt
       monthly_update(Salt_dep2)%cnt = monthly_update(Salt_dep2)%cnt + 1
       monthly_update(Salt_dep2_mass)%val = monthly_update(Salt_dep2_mass)%val + (sum_salt_dep*gdpt_area)
       monthly_update(Salt_dep2_mass)%cnt = monthly_update(Salt_dep2_mass)%cnt + 1
       CALL run_ave (monthly_update(Salt_dep2_area), REAL(cnt_dep)*gdpt_area/m2_to_ha, 1)
       CALL run_ave (monthly_update(Salt_dep2_frac), REAL(cnt_dep)/ngdpt, 1)
       !compute as: Salt_dep2_mass/(Salt_dep2_area * m2_to_ha)
       monthly_update(Salt_dep2_rate)%val =                                  &
                   monthly_update(Salt_dep2_mass)%val /                      &
                   (monthly_update(Salt_dep2_area)%val * m2_to_ha)
       monthly_update(Salt_dep2_rate)%cnt = monthly_update(Salt_dep2_rate)%cnt + 1

       mrot_update(Salt_dep2,cm)%val = mrot_update(Salt_dep2,cm)%val + sum_salt_dep/ngdpt
       mrot_update(Salt_dep2,cm)%cnt = mrot_update(Salt_dep2,cm)%cnt + 1
       mrot_update(Salt_dep2_mass,cm)%val = mrot_update(Salt_dep2_mass,cm)%val + (sum_salt_dep*gdpt_area)
       mrot_update(Salt_dep2_mass,cm)%cnt = mrot_update(Salt_dep2_mass,cm)%cnt + 1
       CALL run_ave (mrot_update(Salt_dep2_area,cm), REAL(cnt_dep)*gdpt_area/m2_to_ha, 1)
       CALL run_ave (mrot_update(Salt_dep2_frac,cm), REAL(cnt_dep)/ngdpt, 1)
       !compute as: Salt_dep2_mass/(Salt_dep2_area * m2_to_ha)
       mrot_update(Salt_dep2_rate,cm)%val =                                  &
                   mrot_update(Salt_dep2_mass,cm)%val /                      &
                   (mrot_update(Salt_dep2_area,cm)%val * m2_to_ha)
       mrot_update(Salt_dep2_rate,cm)%cnt = mrot_update(Salt_dep2_rate,cm)%cnt + 1
    END IF

IF (Have_Erosion) THEN      !We have erosion, so compute TC, etc.

    ! Determine the region under saltation transport capacity
    ! (heavy erosion flux rates but zero net erosion and deposition)
    ! Sheltered regions are determined by grid pts that generate no suspension
    ! (well very little)
    ! NOTE:  We are assuming that the "eros_thresh" is less restrictive than
    !        than the "susp_thresh".  If not, we may have "Transport Capacity"
    !        areas considered to be "sheltered"  or not have any "sheltered"
    !        areas at all when we do. 

    cnt_transp = 0
    cnt_sheltered = 0
    DO i = 1, imax-1 
       DO j = 1, jmax-1 
          IF ( ABS((egt(i,j) - egtss(i,j))) <= eros_thresh) THEN  !Sheltered/TC
             IF ( ABS(egtss(i,j)) <= susp_thresh) THEN  ! Sheltered area
                cnt_sheltered = cnt_sheltered + 1
             ELSE                                       ! At TC
                cnt_transp = cnt_transp + 1
             END IF
          END IF
       END DO
    END DO

    ! Note: Don't currently have a way of computing Flux Rate over a grid area
    CALL run_ave (monthly_update(Trans_cap_area), REAL(cnt_transp)*gdpt_area/m2_to_ha, 1)
    CALL run_ave (monthly_update(Trans_cap_frac), REAL(cnt_transp)/ngdpt, 1)
    CALL run_ave (mrot_update(Trans_cap_area,cm), REAL(cnt_transp)*gdpt_area/m2_to_ha, 1)
    CALL run_ave (mrot_update(Trans_cap_frac,cm), REAL(cnt_transp)/ngdpt, 1)

    !Sheltered region is now computed.
    CALL run_ave (monthly_update(Sheltered_area), REAL(cnt_sheltered)*gdpt_area/m2_to_ha, 1)
    CALL run_ave (monthly_update(Sheltered_frac), REAL(cnt_sheltered)/ngdpt, 1)
    CALL run_ave (mrot_update(Sheltered_area,cm), REAL(cnt_sheltered)*gdpt_area/m2_to_ha, 1)
    CALL run_ave (mrot_update(Sheltered_frac,cm), REAL(cnt_sheltered)/ngdpt, 1)
END IF  !Have_Erosion flag

! Sum boundary losses  (ave value per boundary grid point)
  DO i = 0, imax 
    ! Note that egt contains creep+saltation not total soil loss on boundary
    monthly_update(Salt_1)%val = monthly_update(Salt_1)%val + egt(i,0)/(imax-1)
    monthly_update(Salt_3)%val = monthly_update(Salt_3)%val + egt(i,jmax)/(imax-1)
    monthly_update(Susp_1)%val = monthly_update(Susp_1)%val + egtss(i,0)/(imax-1)
    monthly_update(Susp_3)%val = monthly_update(Susp_3)%val + egtss(i,jmax)/(imax-1)
    monthly_update(PM10_1)%val = monthly_update(PM10_1)%val + egt10(i,0)/(imax-1)
    monthly_update(PM10_3)%val = monthly_update(PM10_3)%val + egt10(i,jmax)/(imax-1)

    mrot_update(Salt_1,cm)%val = mrot_update(Salt_1,cm)%val + egt(i,0)/(imax-1)
    mrot_update(Salt_3,cm)%val = mrot_update(Salt_3,cm)%val + egt(i,jmax)/(imax-1)
    mrot_update(Susp_1,cm)%val = mrot_update(Susp_1,cm)%val + egtss(i,0)/(imax-1)
    mrot_update(Susp_3,cm)%val = mrot_update(Susp_3,cm)%val + egtss(i,jmax)/(imax-1)
    mrot_update(PM10_1,cm)%val = mrot_update(PM10_1,cm)%val + egt10(i,0)/(imax-1)
    mrot_update(PM10_3,cm)%val = mrot_update(PM10_3,cm)%val + egt10(i,jmax)/(imax-1)
  END DO
  monthly_update(Salt_1)%cnt = monthly_update(Salt_1)%cnt + 1
  monthly_update(Salt_3)%cnt = monthly_update(Salt_3)%cnt + 1
  monthly_update(Susp_1)%cnt = monthly_update(Susp_1)%cnt + 1
  monthly_update(Susp_3)%cnt = monthly_update(Susp_3)%cnt + 1
  monthly_update(PM10_1)%cnt = monthly_update(PM10_1)%cnt + 1
  monthly_update(PM10_3)%cnt = monthly_update(PM10_3)%cnt + 1

  mrot_update(Salt_1,cm)%cnt = mrot_update(Salt_1,cm)%cnt + 1
  mrot_update(Salt_3,cm)%cnt = mrot_update(Salt_3,cm)%cnt + 1
  mrot_update(Susp_1,cm)%cnt = mrot_update(Susp_1,cm)%cnt + 1
  mrot_update(Susp_3,cm)%cnt = mrot_update(Susp_3,cm)%cnt + 1
  mrot_update(PM10_1,cm)%cnt = mrot_update(PM10_1,cm)%cnt + 1
  mrot_update(PM10_3,cm)%cnt = mrot_update(PM10_3,cm)%cnt + 1

  DO j = 0, jmax 
    ! Note that egt contains creep+saltation not total soil loss on boundary
    monthly_update(Salt_2)%val = monthly_update(Salt_2)%val + egt(0,j)/(jmax-1)
    monthly_update(Salt_4)%val = monthly_update(Salt_4)%val + egt(imax,j)/(jmax-1)
    monthly_update(Susp_2)%val = monthly_update(Susp_2)%val + egtss(0,j)/(jmax-1)
    monthly_update(Susp_4)%val = monthly_update(Susp_4)%val + egtss(imax,j)/(jmax-1)
    monthly_update(PM10_2)%val = monthly_update(PM10_2)%val + egt10(0,j)/(jmax-1)
    monthly_update(PM10_4)%val = monthly_update(PM10_4)%val + egt10(imax,j)/(jmax-1)

    mrot_update(Salt_2,cm)%val = mrot_update(Salt_2,cm)%val + egt(0,j)/(jmax-1)
    mrot_update(Salt_4,cm)%val = mrot_update(Salt_4,cm)%val + egt(imax,j)/(jmax-1)
    mrot_update(Susp_2,cm)%val = mrot_update(Susp_2,cm)%val + egtss(0,j)/(jmax-1)
    mrot_update(Susp_4,cm)%val = mrot_update(Susp_4,cm)%val + egtss(imax,j)/(jmax-1)
    mrot_update(PM10_2,cm)%val = mrot_update(PM10_2,cm)%val + egt10(0,j)/(jmax-1)
    mrot_update(PM10_4,cm)%val = mrot_update(PM10_4,cm)%val + egt10(imax,j)/(jmax-1)
  END DO
  monthly_update(Salt_2)%cnt = monthly_update(Salt_2)%cnt + 1
  monthly_update(Salt_4)%cnt = monthly_update(Salt_4)%cnt + 1
  monthly_update(Susp_2)%cnt = monthly_update(Susp_2)%cnt + 1
  monthly_update(Susp_4)%cnt = monthly_update(Susp_4)%cnt + 1
  monthly_update(PM10_2)%cnt = monthly_update(PM10_2)%cnt + 1
  monthly_update(PM10_4)%cnt = monthly_update(PM10_4)%cnt + 1

  mrot_update(Salt_2,cm)%cnt = mrot_update(Salt_2,cm)%cnt + 1
  mrot_update(Salt_4,cm)%cnt = mrot_update(Salt_4,cm)%cnt + 1
  mrot_update(Susp_2,cm)%cnt = mrot_update(Susp_2,cm)%cnt + 1
  mrot_update(Susp_4,cm)%cnt = mrot_update(Susp_4,cm)%cnt + 1
  mrot_update(PM10_2,cm)%cnt = mrot_update(PM10_2,cm)%cnt + 1
  mrot_update(PM10_4,cm)%cnt = mrot_update(PM10_4,cm)%cnt + 1

  !variables running averaged for period
  we = 0.0
  IF (awudmx > wind_energy_thresh) THEN
     DO i = 1, ntstep
        IF (awu(i) > wind_energy_thresh) THEN
          we = we + 0.5*awdair*(awu(i)**2) * (awu(i) - wind_energy_thresh) *        &
             (86400.0/ntstep) * (0.001)    ! (s/day) and (J/kJ)
        END IF
     END DO
  END IF
  CALL run_ave(monthly_update(Wind_energy), we, 1)
  CALL run_ave(mrot_update(Wind_energy,cm), we, 1)

  CALL run_ave(monthly_update(Dryness_ratio), ah0drat, 1)
  CALL run_ave(mrot_update(Dryness_ratio,cm), ah0drat, 1)

  s = 1  !currently have only one subregion
  ! Note that the 20mm depth should be a global parameter
  ! It is currently stuck in erosion.for as a local parameter there
  IF (ahzsnd(s) > snow_depth_thresh) THEN
     CALL run_ave(monthly_update(Snow_cover), 1.0, 1)
     CALL run_ave(mrot_update(Snow_cover,cm), 1.0, 1)
  ELSE
     CALL run_ave(monthly_update(Snow_cover), 0.0, 1)
     CALL run_ave(mrot_update(Snow_cover,cm), 0.0, 1)
  END IF

END SUBROUTINE update_monthly_update_vars


SUBROUTINE update_monthly_report_vars(cur_month, cur_year, nrot_years, monthly_update, mrot_update, monthly_report)

    USE pd_dates_vars
    USE pd_var_type_def
    USE pd_var_tables

    IMPLICIT NONE

    INTEGER, INTENT (IN) :: cur_month
    INTEGER, INTENT (IN) :: cur_year
    INTEGER, INTENT (IN) :: nrot_years
    TYPE (pd_var_type), DIMENSION(:), intent(inout) :: monthly_update
    TYPE (pd_var_type), DIMENSION(:,:), intent(inout) :: mrot_update
    TYPE (pd_var_type), DIMENSION(:,:,:), intent(inout) :: monthly_report

    INTEGER :: i        ! local loop variables
    INTEGER :: rot_y    ! local variables

    REAL, PARAMETER :: m2_to_ha = 10000.0  ! m^2 in a ha

    rot_y = mod(cur_year-1,nrot_years)+1

    !variables averaged for reporting period
    DO i=Min_cli_vars, Max_cli_vars
       CALL run_ave (monthly_report(i,cur_month,rot_y), monthly_update(i)%val, 1 )
    END DO
    DO i=Min_eave_vars, Max_eave_vars
       CALL run_ave (monthly_report(i,cur_month,rot_y), monthly_update(i)%val, 1 )
    END DO

    ! If we have saltating loss, add the area and fraction info
    ! else only "running ave" the Salt_loss2 rate
    DO i=Min_lave_vars, Max_lave_vars
      IF (monthly_update(Salt_loss2)%cnt > 0) THEN 
          CALL run_ave (monthly_report(i,cur_month,rot_y), monthly_update(i)%val, 1 )
      ELSE !Don't change the area and fract values
         IF (i == Salt_loss2) THEN
            CALL run_ave (monthly_report(i,cur_month,rot_y), monthly_update(i)%val, 1 )
         END IF
      END IF
    END DO
    IF (monthly_report(Salt_loss2_rate,cur_month,rot_y)%val > 0.0) THEN
       monthly_report(Salt_loss2_rate,cur_month,rot_y)%val =               &
           monthly_report(Salt_loss2_mass,cur_month,rot_y)%val /           &
           (monthly_report(Salt_loss2_area,cur_month,rot_y)%val * m2_to_ha)
    END IF

    ! If we have deposition, add the area and fraction info
    DO i=Min_dave_vars, Max_dave_vars
      IF (monthly_update(Salt_dep2)%cnt > 0) THEN 
         CALL run_ave (monthly_report(i,cur_month,rot_y), monthly_update(i)%val, 1 )
      ELSE
         IF (i == Salt_dep2) THEN
            CALL run_ave (monthly_report(i,cur_month,rot_y), monthly_update(i)%val, 1 )
         END IF
      END IF
    END DO
    IF (monthly_report(Salt_dep2_rate,cur_month,rot_y)%val > 0.0) THEN
       monthly_report(Salt_dep2_rate,cur_month,rot_y)%val =                &
           monthly_report(Salt_dep2_mass,cur_month,rot_y)%val /            &
           (monthly_report(Salt_dep2_area,cur_month,rot_y)%val * m2_to_ha)
    END IF

    ! If we have salt loss, then we may have some transp cap
    DO i=Min_tave_vars, Max_tave_vars
      IF (monthly_update(Salt_loss2)%cnt > 0) THEN 
         CALL run_ave (monthly_report(i,cur_month,rot_y), monthly_update(i)%val, 1 )
      END IF
    END DO

    ! update the full rotation average variables
    IF (rot_y == nrot_years) THEN

       !variables averaged for reporting period
       DO i=Min_cli_vars, Max_cli_vars
          IF (i == Precipi) THEN  ! These have only been "summed"
             CALL run_ave (monthly_report(i,cur_month,0), mrot_update(i,cur_month)%val, nrot_years)
          ELSE  !These have already been "running averaged"
             CALL run_ave (monthly_report(i,cur_month,0), mrot_update(i,cur_month)%val, 1)
          END IF
       END DO
       DO i=Min_eave_vars, Max_eave_vars
          CALL run_ave (monthly_report(i,cur_month,0), mrot_update(i,cur_month)%val, nrot_years)
       END DO
   
       ! If we have saltating loss, add the area and fraction info
       DO i=Min_lave_vars, Max_lave_vars
         IF (i == Salt_loss2) THEN
            !Compute the average "yearly" salt loss per unit area over the entire simulation region
            CALL run_ave (monthly_report(i,cur_month,0), mrot_update(i,cur_month)%val, nrot_years)
         ELSE
            !Pass a count of one so that they don't "average" the area across
            !rotation years.  We want to present the "total" average area
            !affected due to erosion events during the rotation, 
            !NOT the average area "per year" due to erosion events.
            CALL run_ave (monthly_report(i,cur_month,0), mrot_update(i,cur_month)%val, 1)
         END IF
       END DO     
       IF (mrot_update(Salt_loss2_area,cur_month)%cnt > 0) THEN 
          monthly_report(Salt_loss2_rate,cur_month,0)%val =               &
              monthly_report(Salt_loss2_mass,cur_month,0)%val /           &
              (monthly_report(Salt_loss2_area,cur_month,0)%val * m2_to_ha) / nrot_years
          monthly_report(Salt_loss2_rate,cur_month,0)%cnt = monthly_report(Salt_loss2_rate,cur_month,0)%cnt + 1
       END IF
   
       ! If we have deposition, add the area and fraction info
       DO i=Min_dave_vars, Max_dave_vars
         IF (i == Salt_dep2) THEN
            !Compute the average "yearly" deposition per unit area over the entire simulation region
             CALL run_ave (monthly_report(i,cur_month,0), mrot_update(i,cur_month)%val, nrot_years)
         ELSE
            !Pass a count of one so that they don't "average" the area across
            !rotation years.  We want to present the "total" average area
            !NOT the "average" area "per year" due to erosion events.
            CALL run_ave (monthly_report(i,cur_month,0), mrot_update(i,cur_month)%val, 1)
         END IF
       END DO
       IF (mrot_update(Salt_dep2_area,cur_month)%val > 0) THEN 
          monthly_report(Salt_dep2_rate,cur_month,0)%val =                &
              monthly_report(Salt_dep2_mass,cur_month,0)%val /            &
              (monthly_report(Salt_dep2_area,cur_month,0)%val * m2_to_ha) /nrot_years
       END IF
   
       ! If we have salt loss, then we may have some transp cap
       DO i = Min_tave_vars, Max_tave_vars
          !Pass a count of one so that they don't "average" the area across
          !rotation years.  We want to present the "total" area affected
          !not the "average" area per year.
          CALL run_ave (monthly_report(i,cur_month,0), mrot_update(i,cur_month)%val, 1)
       END DO
    END IF

    ! reset monthly update vars
    DO i=Min_monthly_vars, Max_monthly_vars
       monthly_update(i)%cnt = 0
       monthly_update(i)%val = 0.0
    END DO
    monthly_dates(cur_month,rot_y)%ey = monthly_dates(cur_month,rot_y)%ey + 1

    ! reset monthly rotation update vars
    IF (rot_y == nrot_years) THEN
       DO i=Min_monthly_vars,Max_monthly_vars
          mrot_update(i,cur_month)%cnt = 0
          mrot_update(i,cur_month)%val = 0.0
       END DO
!      monthly_dates(cur_month,0)%ey = monthly_dates(cur_month,0)%ey + nrot_years 
    END IF
END SUBROUTINE update_monthly_report_vars
