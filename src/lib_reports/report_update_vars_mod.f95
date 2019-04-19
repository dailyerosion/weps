!$Author$
!$Date$
!$Revision$
!$HeadURL$

module report_update_vars_mod

  contains

  SUBROUTINE update_period_update_vars(isr, period_update, soil, restot, croptot, biotot, cellstate, h1et, h1bal)

    USE pd_var_tables
    USE pd_var_type_def
    use soil_data_struct_defs, only: soil_def
    use biomaterial, only: biototal
    use erosion_data_struct_defs, only: cellsurfacestate
    use hydro_data_struct_defs, only: hydro_derived_et
    use report_hydrobal_mod, only: hydro_balance
    use grid_mod, only: imax, jmax, sim_area
    use process_mod, only: sbsfdi

    IMPLICIT NONE

!   + + + ARGUMENT DECLARATIONS + + +
    INTEGER :: isr              ! current subregion
    TYPE (pd_var_type), DIMENSION(Min_period_vars:), intent(inout) :: period_update
    type(soil_def), intent(in) :: soil  ! contains:
                                ! aslagm, as0ags, aslagn, aslagx (ASD parms)
                                ! aseags (agg stability), asdagd (agg density)
                                ! asfcr (crust fraction)
                                ! aszcr (crust thickness)
                                ! asmlos (mass of loose material on crusted surface)
                                ! asflos (fraction of crusted surface with loose material)
                                ! asdcr (density of crust)
                                ! asecr (stability of crust)
                                ! acancr(crust coeff. of abrasion)
                                ! acanag (agg. coeff. of abrasion)
                                ! aslrr Allmaras RR values
                                ! aszrgh Ridge height
                                ! asxrgs Ridge spacing
                                ! asargo Ridge dir
    type(biototal), intent(in) :: restot  ! contains:
                                ! adftcvtot(isr)  total dead flat cover
                                ! adrcdtot(isr)   total effective silhouette
                                ! admftot(isr)    total dead flat mass
                                ! admsttot(isr)   total dead standing mass
    type(biototal), intent(in) :: croptot  ! contains:
                                ! acfcancov(isr)  crop canopy cover
                                ! acftcv(isr)     crop flat cover
                                ! acrcd(isr)      crop effective silhouette
                                ! acmf(isr)       crop flat mass
                                ! acmst(isr)      crop standing mass
                                ! acmstandstore(isr)      crop standing repr mass
                                ! acmflatstore(isr)      crop flat repr mass
    type(biototal), intent(in) :: biotot  ! contains:
                                ! abftcv(isr)     all flat cover
                                ! abrcd           all effective silhouette
                                ! abmf(isr)       all flat mass
                                ! abmst(isr)      all standing mass
    type(cellsurfacestate), dimension(0:,0:), intent(in) :: cellstate  ! egt, egtcs, egtss, egt10
    type(hydro_derived_et), intent(in) :: h1et  ! contains:
                                ! ahzpta(isr)     daily transpiration (mm)
                                ! ahzpta(isr)     daily evaporation (mm)
    type(hydro_balance), intent(in) :: h1bal  ! contains:
                                ! presswc(isr)  daily water content in all soil layers (mm)

!    REAL :: biodrag		! biodrag() function in util/misc/biodrag.for

    REAL :: ef84                ! erodible agg. size fraction below 0.84mm

    INTEGER :: i,j              ! local loop variables
    INTEGER :: ngdpt            ! number of simulation grid datapoints
    REAL    :: gdpt_area        ! area of a grid cell (point) in m^2

    ! INTEGER :: cnt              ! number of simulation grid datapoints
    REAL    :: sum_salt_loss
    REAL    :: sum_salt_dep

    INTEGER :: cnt_eros          ! number of simulation grid datapoints with net erosion
    INTEGER :: cnt_dep           ! number of simulation grid datapoints with net deposition

    INTEGER :: cnt_transp       ! number of simulation grid datapoints at TC
    INTEGER :: cnt_sheltered    ! number of simulation grid datapoints sheltered

    REAL, PARAMETER :: m2_to_ha = 10000.0  ! m^2 in a ha

    ! Threshold value for determining erosive wind energy (m/s)
    REAL, PARAMETER :: wind_energy_thresh = 8.0
    ! Threshold value for determining protective snow depth (mm)
    REAL, PARAMETER :: snow_depth_thresh = 20.0

    ! Threshold value for determining erosion loss and deposition regions
    REAL, PARAMETER :: eros_thresh = 0.025 !kg/m^2
    ! Threshold value for determining sheltered regions
    REAL, PARAMETER :: susp_thresh = 0.001 !kg/m^2

    ! Flags to specify whether we have experienced an erosion event or not.
    ! It is set in the "Salt_loss2" section and used in the "Trans_cap"
    ! and "Sheltered" code sections.
    LOGICAL :: Have_Erosion
    LOGICAL :: Have_Deposition

    Have_Erosion    = .FALSE.   ! Initialize for each invocation of routine
    Have_Deposition = .FALSE.   ! Initialize for each invocation of routine

    gdpt_area = sim_area/( (imax-1)*(jmax-1) )   !Area of single grid cell

    !End of period (eop) variables

    ! Roughness vars
    period_update(Random_rough)%val = soil%aslrr
    period_update(Random_rough)%cnt = period_update(Random_rough)%cnt + 1

    period_update(Ridge_ht)%val = soil%aszrgh
    period_update(Ridge_ht)%cnt = period_update(Ridge_ht)%cnt + 1

    period_update(Ridge_sp)%val = soil%asxrgs
    period_update(Ridge_sp)%cnt = period_update(Ridge_sp)%cnt + 1

    period_update(Ridge_dir)%val = soil%asargo
    period_update(Ridge_dir)%cnt = period_update(Ridge_dir)%cnt + 1

    call sbsfdi(soil%aslagm(1), soil%as0ags(1), soil%aslagn(1), soil%aslagx(1),0.84,ef84)
    period_update(Surface_Ag_84)%val = ef84
    period_update(Surface_Ag_84)%cnt = period_update(Surface_Ag_84)%cnt + 1

    period_update(Surface_Ag_AS)%val = soil%aseags(1)  !Ag Stability (J/m^2)
    period_update(Surface_Ag_AS)%cnt = period_update(Surface_Ag_AS)%cnt + 1

    period_update(Surface_Ag_DN)%val = soil%asdagd(1)  !Ag Density (Mg/m^3)
    period_update(Surface_Ag_DN)%cnt = period_update(Surface_Ag_DN)%cnt + 1

    period_update(Surface_Ag_CA)%val = soil%acanag  !Ag Coeff. of abrasion (1/m)
    period_update(Surface_Ag_CA)%cnt = period_update(Surface_Ag_CA)%cnt + 1

    period_update(Surface_Cr)%val = soil%asfcr  !Surface Crust fraction
    period_update(Surface_Cr)%cnt = period_update(Surface_Cr)%cnt + 1

    period_update(Surface_Cr_AS)%val = soil%asecr  !Surface Crust stability (J/m^2)
    period_update(Surface_Cr_AS)%cnt = period_update(Surface_Cr_AS)%cnt + 1

    period_update(Surface_Cr_LM)%val = soil%asmlos  !Surface Crust loose material (Mg/m^2)
    period_update(Surface_Cr_LM)%cnt = period_update(Surface_Cr_LM)%cnt + 1

    period_update(Surface_Cr_TH)%val = soil%aszcr  !Surface Crust thickness (mm)
    period_update(Surface_Cr_TH)%cnt = period_update(Surface_Cr_TH)%cnt + 1

    period_update(Surface_Cr_DN)%val = soil%asdcr  !Surface Crust density (Mg/m^3)
    period_update(Surface_Cr_DN)%cnt = period_update(Surface_Cr_DN)%cnt + 1

    period_update(Surface_Cr_LF)%val = soil%asflos  !Surface Crust - fraction of loose material (m^2/m^2)
    period_update(Surface_Cr_LF)%cnt = period_update(Surface_Cr_LF)%cnt + 1

    period_update(Surface_Cr_CA)%val = soil%acancr  !Surface Crust Coeff. of abrasion (1/m)
    period_update(Surface_Cr_CA)%cnt = period_update(Surface_Cr_CA)%cnt + 1

    ! Soil Water
    period_update(Soil_Water)%val = h1bal%presswc  !Soil Water content in full soil profile (mm)
    period_update(Soil_Water)%cnt = period_update(Soil_Water)%cnt + 1

    ! Crop vars
    period_update(Crop_canopy_cov)%val = croptot%ftcancov
    period_update(Crop_canopy_cov)%cnt = period_update(Crop_canopy_cov)%cnt + 1

    period_update(Crop_stand_sil)%val = croptot%rcdtot
    period_update(Crop_stand_sil)%cnt = period_update(Crop_stand_sil)%cnt + 1

   !period_update(Crop_flat_mass)%val = croptot%mftot
   !period_update(Crop_flat_mass)%cnt = period_update(Crop_flat_mass)%cnt + 1

   !Note currently we report this as "Above Ground Mass" not "Standing Mass"
   !Note that we are also subtracting the "store" portion
   !which contains the reproductive (seed and fruit) components
    ! Remove live standing and flat crop reproductive mass from reported value
    period_update(Crop_stand_mass)%val = croptot%msttot + croptot%mftot - croptot%mstandstore - croptot%mflatstore
    period_update(Crop_stand_mass)%cnt = period_update(Crop_stand_mass)%cnt + 1

    period_update(Crop_root_mass)%val = croptot%mrttot
    period_update(Crop_root_mass)%cnt = period_update(Crop_root_mass)%cnt + 1

    period_update(Crop_stand_height)%val = croptot%zht_ave
    period_update(Crop_stand_height)%cnt = period_update(Crop_stand_height)%cnt + 1

    period_update(Crop_number_stems)%val = croptot%dstmtot
    period_update(Crop_number_stems)%cnt = period_update(Crop_number_stems)%cnt + 1

    ! Residue vars
    period_update(Res_flat_cov)%val = restot%ftcvtot
    period_update(Res_flat_cov)%cnt = period_update(Res_flat_cov)%cnt + 1

    period_update(Res_stand_sil)%val = restot%rcdtot
    period_update(Res_stand_sil)%cnt = period_update(Res_stand_sil)%cnt + 1

    period_update(Res_flat_mass)%val = restot%mftot
    period_update(Res_flat_mass)%cnt = period_update(Res_flat_mass)%cnt + 1

    period_update(Res_stand_mass)%val = restot%msttot
    period_update(Res_stand_mass)%cnt = period_update(Res_stand_mass)%cnt + 1

    period_update(Res_buried_mass)%val = restot%mbgtot
    period_update(Res_buried_mass)%cnt = period_update(Res_buried_mass)%cnt + 1

    period_update(Res_root_mass)%val = restot%mrttot
    period_update(Res_root_mass)%cnt = period_update(Res_root_mass)%cnt + 1

    period_update(Res_stand_height)%val = restot%zht_ave
    period_update(Res_stand_height)%cnt = period_update(Res_stand_height)%cnt + 1

    period_update(Res_number_stems)%val = restot%dstmtot
    period_update(Res_number_stems)%cnt = period_update(Res_number_stems)%cnt + 1

    ! Biomass vars
    period_update(All_flat_cov)%val = biotot%ftcvtot
    period_update(All_flat_cov)%cnt = period_update(All_flat_cov)%cnt + 1

    period_update(All_stand_sil)%val = biotot%rcdtot
    period_update(All_stand_sil)%cnt = period_update(All_stand_sil)%cnt + 1

    ! Remove live flat crop reproductive mass from reported value
    period_update(All_flat_mass)%val = biotot%mftot - croptot%mflatstore
    period_update(All_flat_mass)%cnt = period_update(All_flat_mass)%cnt + 1

    ! Remove live standing crop reproductive mass from reported value
    period_update(All_stand_mass)%val = biotot%msttot - croptot%mstandstore
    period_update(All_stand_mass)%cnt = period_update(All_stand_mass)%cnt + 1

    period_update(All_buried_mass)%val = croptot%mrttot + restot%mrttot + restot%mbgtot
    period_update(All_buried_mass)%cnt = period_update(All_buried_mass)%cnt + 1

! ------------------------------------------------------------------------------------------------------------------
    ! Determine if we have any net soil loss occurring from any grid cell (erosion)
    ! We assume that we don't have any net suspension loss if we don't have any net salt/creep loss
    ngdpt = 0   ! count number of grid points in this subregion (if zero, it is not used)
    sum_salt_loss = 0.0; cnt_eros = 0
    DO i = 1, imax-1 
       DO j = 1, jmax-1 
          if( (isr .eq. 0) .or. (isr .eq. cellstate(i,j)%csr) ) then
             IF ((cellstate(i,j)%egt - cellstate(i,j)%egtss) < -eros_thresh) THEN
                sum_salt_loss = sum_salt_loss + (cellstate(i,j)%egt - cellstate(i,j)%egtss)
                cnt_eros = cnt_eros + 1
             END IF
             ngdpt = ngdpt + 1
          end if
       END DO
    END DO
    IF (cnt_eros /= 0) Have_Erosion = .TRUE.  !We have erosion occurring, set flag for use later

   ! Determine if we have any net soil deposition occurring from any grid cell (deposition)
    ! We assume that we don't have any net suspension deposition if we don't have any mry salt/creep deposition
    sum_salt_dep = 0.0; cnt_dep = 0
    DO i = 1, imax-1 
       DO j = 1, jmax-1 
          if( (isr .eq. 0) .or. (isr .eq. cellstate(i,j)%csr) ) then
             IF ((cellstate(i,j)%egt - cellstate(i,j)%egtss) > eros_thresh) THEN
                sum_salt_dep = sum_salt_dep + (cellstate(i,j)%egt - cellstate(i,j)%egtss)
                cnt_dep = cnt_dep + 1
             END IF
          end if
       END DO
    END DO
    IF (cnt_dep /= 0) Have_Deposition = .TRUE.  !We have deposition occurring, set flag for use later
! ------------------------------------------------------------------------------------------------------------------

!variables summed for period

    period_update(Crop_Transp)%val = period_update(Crop_Transp)%val + h1et%zpta
    period_update(Crop_Transp)%cnt = period_update(Crop_Transp)%cnt + 1

    period_update(Evaporation)%val = period_update(Evaporation)%val + h1et%zea
    period_update(Evaporation)%cnt = period_update(Evaporation)%cnt + 1

    period_update(Runoff)%val = period_update(Runoff)%val + h1et%zrun
    period_update(Runoff)%cnt = period_update(Runoff)%cnt + 1

    period_update(Drainage)%val = period_update(Drainage)%val + h1et%zper
    period_update(Drainage)%cnt = period_update(Drainage)%cnt + 1

    DO i = 1, imax-1 
       DO j = 1, jmax-1 
          if( (isr .eq. 0) .or. (isr .eq. cellstate(i,j)%csr) ) then
             period_update(Eros_loss)%val = period_update(Eros_loss)%val + cellstate(i,j)%egt/ngdpt
             period_update(Salt_loss)%val = period_update(Salt_loss)%val + (cellstate(i,j)%egt - cellstate(i,j)%egtss)/ngdpt
             period_update(Susp_loss)%val = period_update(Susp_loss)%val + cellstate(i,j)%egtss/ngdpt
             period_update(PM10_loss)%val = period_update(PM10_loss)%val + cellstate(i,j)%egt10/ngdpt
             period_update(PM2_5_loss)%val = period_update(PM2_5_loss)%val + cellstate(i,j)%egt2_5/ngdpt
          end if
       END DO
    END DO
    period_update(Eros_loss)%cnt = period_update(Eros_loss)%cnt + 1
    period_update(Salt_loss)%cnt = period_update(Salt_loss)%cnt + 1
    period_update(Susp_loss)%cnt = period_update(Susp_loss)%cnt + 1
    period_update(PM10_loss)%cnt = period_update(PM10_loss)%cnt + 1
    period_update(PM2_5_loss)%cnt = period_update(PM2_5_loss)%cnt + 1

     IF (Have_Erosion) THEN !We have erosion somewhere
       period_update(N_eros_events)%val = period_update(N_eros_events)%val + 1.0  ! Count the erosion events
    END IF
    period_update(N_eros_events)%cnt = period_update(N_eros_events)%cnt + 1

! Sum boundary losses  (ave value per boundary grid point)
    DO i = 0, imax 
      ! Note that egt contains creep+saltation not total soil loss on boundary
      period_update(Salt_1)%val = period_update(Salt_1)%val + cellstate(i,0)%egt/(imax-1)
      period_update(Salt_3)%val = period_update(Salt_3)%val + cellstate(i,jmax)%egt/(imax-1)
      period_update(Susp_1)%val = period_update(Susp_1)%val + cellstate(i,0)%egtss/(imax-1)
      period_update(Susp_3)%val = period_update(Susp_3)%val + cellstate(i,jmax)%egtss/(imax-1)
      period_update(PM10_1)%val = period_update(PM10_1)%val + cellstate(i,0)%egt10/(imax-1)
      period_update(PM10_3)%val = period_update(PM10_3)%val + cellstate(i,jmax)%egt10/(imax-1)
    END DO
    period_update(Salt_1)%cnt = period_update(Salt_1)%cnt + 1
    period_update(Salt_3)%cnt = period_update(Salt_3)%cnt + 1
    period_update(Susp_1)%cnt = period_update(Susp_1)%cnt + 1
    period_update(Susp_3)%cnt = period_update(Susp_3)%cnt + 1
    period_update(PM10_1)%cnt = period_update(PM10_1)%cnt + 1
    period_update(PM10_3)%cnt = period_update(PM10_3)%cnt + 1

    DO j = 0, jmax 
      ! Note that egt contains creep+saltation not total soil loss on boundary
      period_update(Salt_2)%val = period_update(Salt_2)%val + cellstate(0,j)%egt/(jmax-1)
      period_update(Salt_4)%val = period_update(Salt_4)%val + cellstate(imax,j)%egt/(jmax-1)
      period_update(Susp_2)%val = period_update(Susp_2)%val + cellstate(0,j)%egtss/(jmax-1)
      period_update(Susp_4)%val = period_update(Susp_4)%val + cellstate(imax,j)%egtss/(jmax-1)
      period_update(PM10_2)%val = period_update(PM10_2)%val + cellstate(0,j)%egt10/(jmax-1)
      period_update(PM10_4)%val = period_update(PM10_4)%val + cellstate(imax,j)%egt10/(jmax-1)
    END DO
    period_update(Salt_2)%cnt = period_update(Salt_2)%cnt + 1
    period_update(Salt_4)%cnt = period_update(Salt_4)%cnt + 1
    period_update(Susp_2)%cnt = period_update(Susp_2)%cnt + 1
    period_update(Susp_4)%cnt = period_update(Susp_4)%cnt + 1
    period_update(PM10_2)%cnt = period_update(PM10_2)%cnt + 1
    period_update(PM10_4)%cnt = period_update(PM10_4)%cnt + 1


     IF (Have_Erosion) THEN !We have erosion somewhere
 
!write(6,*) period_update(Salt_loss2)%val, sum_salt, &
!period_update(Salt_loss2)%cnt, cnt, &
!period_update(Salt_loss2)%date

       period_update(Salt_loss2)%val = period_update(Salt_loss2)%val + sum_salt_loss/ngdpt
       period_update(Salt_loss2)%cnt = period_update(Salt_loss2)%cnt + 1

       period_update(Salt_loss2_mass)%val = period_update(Salt_loss2_mass)%val + (sum_salt_loss*gdpt_area)
       period_update(Salt_loss2_mass)%cnt = period_update(Salt_loss2_mass)%cnt + 1

       CALL run_ave (period_update(Salt_loss2_area), REAL(cnt_eros)*gdpt_area/m2_to_ha, 1)
       CALL run_ave (period_update(Salt_loss2_frac), REAL(cnt_eros)/ngdpt, 1)

       !compute as: Salt_loss2_mass/(Salt_loss2_area * m2_to_ha)
       period_update(Salt_loss2_rate)%val =                              &
             period_update(Salt_loss2_mass)%val /                        &
             (period_update(Salt_loss2_area)%val * m2_to_ha)
       period_update(Salt_loss2_rate)%cnt = period_update(Salt_loss2_rate)%cnt + 1
    END IF


    IF (Have_Deposition) THEN  ! We have deposition somewhere

       period_update(Salt_dep2)%val = period_update(Salt_dep2)%val + sum_salt_dep/ngdpt
       period_update(Salt_dep2)%cnt = period_update(Salt_dep2)%cnt + 1

       period_update(Salt_dep2_mass)%val = period_update(Salt_dep2_mass)%val + (sum_salt_dep*gdpt_area)
       period_update(Salt_dep2_mass)%cnt = period_update(Salt_dep2_mass)%cnt + 1

       CALL run_ave (period_update(Salt_dep2_area), REAL(cnt_dep)*gdpt_area/m2_to_ha, 1)
       CALL run_ave (period_update(Salt_dep2_frac), REAL(cnt_dep)/ngdpt, 1)

       !compute as: Salt_dep2_mass/(Salt_dep2_area * m2_to_ha)
       period_update(Salt_dep2_rate)%val =                                &
              period_update(Salt_dep2_mass)%val /                         &
              (period_update(Salt_dep2_area)%val * m2_to_ha)
       period_update(Salt_dep2_rate)%cnt = period_update(Salt_dep2_rate)%cnt + 1
    END IF

! IF (Have_Erosion .or. Have_Deposition) THEN  !Should this be the correct if statement?
  IF (Have_Erosion) THEN      !We have erosion so compute TC, etc.

    ! Determine the region under saltation transport capacity
    ! (heavy erosion flux rates but zero net erosion and deposition)
    ! Sheltered regions are determined by grid pts that generate no suspension
    ! (well very little)
    ! NOTE:  We are assuming that the "eros_thresh" is less restrictive than
    !        than the "susp_thresh".  If not, we may have "Transport capacity"
    !        areas considered to be "sheltered"  or not have any "sheltered"
    !        areas at all when we do. 

    cnt_transp = 0
    cnt_sheltered = 0
    DO i = 1, imax-1 
       DO j = 1, jmax-1 
          if( (isr .eq. 0) .or. (isr .eq. cellstate(i,j)%csr) ) then
             IF ( ABS((cellstate(i,j)%egt - cellstate(i,j)%egtss)) <= eros_thresh) THEN  !Sheltered/TC
!print*, 'cellstate(i,j)%egt-cellstate(i,j)%egtss: ',ABS(cellstate(i,j)%egt-cellstate(i,j)%egtss), 'egtss: ',ABS(cellstate(i,j)%egtss)

                IF ( ABS(cellstate(i,j)%egtss) <= susp_thresh) THEN  ! Sheltered area
                   cnt_sheltered = cnt_sheltered + 1
                ELSE                                       ! At TC
                   cnt_transp = cnt_transp + 1
                END IF
!print*, 'cnt sheltered/transp: ', cnt_sheltered, cnt_transp
             END IF
          end if
       END DO
    END DO

!   IF (cnt_transp /= 0) THEN
    ! Note: Don't currently have a way of computing Flux Rate over a grid area
       CALL run_ave (period_update(Trans_cap_area), REAL(cnt_transp)*gdpt_area/m2_to_ha, 1)
       CALL run_ave (period_update(Trans_cap_frac), REAL(cnt_transp)/ngdpt, 1)

       !Sheltered region is now computed.
       CALL run_ave (period_update(Sheltered_area), REAL(cnt_sheltered)*gdpt_area/m2_to_ha, 1)
       CALL run_ave (period_update(Sheltered_frac), REAL(cnt_sheltered)/ngdpt, 1)
!   END IF

  END IF  !Have_Erosion flag

  END SUBROUTINE update_period_update_vars

  SUBROUTINE update_period_report_vars(pd, npd, cur_yr, nrot_years, period_update, period_report, period_dates)

    USE pd_dates_vars
    USE pd_var_type_def
    USE pd_var_tables

    IMPLICIT NONE

    INTEGER, INTENT (IN) :: pd, npd
    INTEGER, INTENT (IN) :: cur_yr
    INTEGER, INTENT (IN) :: nrot_years
    TYPE (pd_var_type), DIMENSION(Min_period_vars:), intent(inout) :: period_update
    TYPE (pd_var_type), DIMENSION(Min_period_vars:,:), intent(inout) :: period_report
    TYPE (pd_dates_type), target, DIMENSION(:), intent(inout) :: period_dates

    INTEGER :: i    ! local loop variables
    INTEGER :: rot_y    ! local variables

    REAL, PARAMETER :: m2_to_ha = 10000.0  ! m^2 in a ha

    rot_y = mod(cur_yr-1,nrot_years)+1

    !Variables averaged for reporting period
    !This should be all of "erosion" and "eop" vars 

    DO i = Min_eave_vars, Max_eave_vars
       CALL run_ave (period_report(i,pd), period_update(i)%val, 1 )
    END DO
    DO i = Min_eop_vars, Max_eop_vars
       CALL run_ave (period_report(i,pd), period_update(i)%val, 1 )
    END DO

    ! If we have saltating loss, add the area and fraction info
    DO i = Min_lave_vars, Max_lave_vars
      IF (period_update(Salt_loss2)%cnt > 0) THEN 
          CALL run_ave (period_report(i,pd), period_update(i)%val, 1 )
      ELSE !Don't change the area and fract values
         IF (i == Salt_loss2) THEN
            CALL run_ave (period_report(i,pd), period_update(i)%val, 1 )
         END IF
      END IF
    END DO

    IF (period_report(Salt_loss2_area,pd)%val > 0.0) THEN
       period_report(Salt_loss2_rate,pd)%val =                 &
             period_report(Salt_loss2_mass,pd)%val /           &
             (period_report(Salt_loss2_area,pd)%val *  m2_to_ha)
    END IF

    DO i = Min_dave_vars, Max_dave_vars
      IF (period_update(Salt_dep2)%cnt > 0) THEN 
          CALL run_ave (period_report(i,pd), period_update(i)%val, 1 )
      ELSE !Don't change the area and fract values
         IF (i == Salt_dep2) THEN
          CALL run_ave (period_report(i,pd), period_update(i)%val, 1 )
         END IF
      END IF
    END DO

    IF (period_report(Salt_dep2_area,pd)%val > 0.0) THEN
       period_report(Salt_dep2_rate,pd)%val =                  &
             period_report(Salt_dep2_mass,pd)%val /            &
             (period_report(Salt_dep2_area,pd)%val *  m2_to_ha)
    END IF

    ! If we have salt loss, then we may have some transp cap
    DO i=Min_tave_vars, Max_tave_vars
      IF (period_update(Salt_loss2)%cnt > 0) THEN 
          CALL run_ave (period_report(i,pd), period_update(i)%val, 1 )
      END IF
    END DO

    ! reset update_vars
    DO i=Min_period_vars,Max_period_vars
       period_update(i)%cnt = 0
       period_update(i)%val = 0.0
       IF (pd == npd) THEN
          period_update(i)%date => period_dates(1)
       ELSE
          period_update(i)%date => period_dates(pd+1)
       END IF
    END DO

  END SUBROUTINE update_period_report_vars

  SUBROUTINE update_hmonth_update_vars(isr, cd, cm, hmonth_update, hmrot_update, h1et)

    USE pd_var_type_def, only: pd_var_type
    USE pd_var_tables
    use erosion_data_struct_defs, only: subday, awudmx, awdair, ntstep
    use hydro_data_struct_defs, only: hydro_derived_et
    use climate_input_mod, only: cli_today

    IMPLICIT NONE

    INTEGER, intent (in) :: isr  ! current subregion
    INTEGER, INTENT (IN) :: cd  ! current day
    INTEGER, INTENT (IN) :: cm  ! current month
    TYPE (pd_var_type), DIMENSION(Min_hmonth_vars:), intent(inout) :: hmonth_update
    TYPE (pd_var_type), DIMENSION(Min_hmonth_vars:,:), intent(inout) :: hmrot_update
    type(hydro_derived_et), intent(in) :: h1et

    INTEGER :: i                ! local loop variables

    INTEGER :: hm               ! current hmonth period

    REAL :: we

    ! Threshold value for determining erosive wind energy (m/s)
    REAL, PARAMETER :: wind_energy_thresh = 8.0

    hm = (2 * cm) - 1           !1st half of month
    IF (cd > 14) THEN           !2nd half of month
      hm = hm + 1
    END IF

    !variables summed for period
    hmonth_update(Precipi)%val = hmonth_update(Precipi)%val + cli_today%zdpt
    hmonth_update(Precipi)%cnt = hmonth_update(Precipi)%cnt + 1
    hmonth_update(Irrigation)%val = hmonth_update(Irrigation)%val + h1et%zirr
    hmonth_update(Irrigation)%cnt = hmonth_update(Irrigation)%cnt + 1

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

    CALL run_ave(hmonth_update(Snow_cover), h1et%snow_protect, 1)
    CALL run_ave(hmrot_update(Snow_cover,hm), h1et%snow_protect, 1)

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

    ! reset hmonth rotation update vars
    IF (rot_y == nrot_years) THEN
       DO i=Min_hmonth_vars,Max_hmonth_vars
          hmrot_update(i,hm)%cnt = 0
          hmrot_update(i,hm)%val = 0.0
       END DO
    END IF

  END SUBROUTINE update_hmonth_report_vars

  SUBROUTINE update_monthly_update_vars(isr, cm, monthly_update, mrot_update, cellstate, h1et)

    USE pd_var_type_def
    USE pd_var_tables
    use erosion_data_struct_defs, only: cellsurfacestate, awdair, awudmx, subday, ntstep 
    use grid_mod, only: imax, jmax, sim_area
    use hydro_data_struct_defs, only: hydro_derived_et
    use climate_input_mod, only: cli_today

    IMPLICIT NONE

    INTEGER, intent (in) :: isr  ! current subregion
    INTEGER, INTENT (IN) :: cm  ! current month
    TYPE (pd_var_type), DIMENSION(Min_monthly_vars:), intent(inout) :: monthly_update
    TYPE (pd_var_type), DIMENSION(Min_monthly_vars:,:), intent(inout) :: mrot_update
    type(cellsurfacestate), dimension(0:,0:), intent(in) :: cellstate     ! initialized grid cell state values
    type(hydro_derived_et), intent(in) :: h1et

    INTEGER :: i,j              ! local loop variables
    INTEGER :: ngdpt            ! number of simulation grid datapoints
    REAL    :: gdpt_area        ! area of a grid cell (point) in m^2

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

    gdpt_area = sim_area/( (imax-1) * (jmax-1) )   ! Area of single grid cell

    !variables summed for period
    monthly_update(Precipi)%val = monthly_update(Precipi)%val + cli_today%zdpt
    monthly_update(Precipi)%cnt = monthly_update(Precipi)%cnt + 1
    mrot_update(Precipi,cm)%val = mrot_update(Precipi,cm)%val + cli_today%zdpt
    mrot_update(Precipi,cm)%cnt = mrot_update(Precipi,cm)%cnt + 1

    monthly_update(Irrigation)%val = monthly_update(Irrigation)%val + h1et%zirr
    monthly_update(Irrigation)%cnt = monthly_update(Irrigation)%cnt + 1
    mrot_update(Irrigation,cm)%val = mrot_update(Irrigation,cm)%val + h1et%zirr
    mrot_update(Irrigation,cm)%cnt = mrot_update(Irrigation,cm)%cnt + 1
! ------------------------------------------------------------------------------------------------------------------
    ! Determine if we have any net soil loss occurring from any grid cell (erosion)
    ! We assume that we don't have any net suspension loss if we don't have any net salt/creep loss
    ngdpt = 0   ! count number of grid points in this subregion (if zero, it is not used)
    sum_salt_loss = 0.0; cnt_eros = 0
    DO i = 1, imax-1 
       DO j = 1, jmax-1 
          if( (isr .eq. 0) .or. (isr .eq. cellstate(i,j)%csr) ) then
             IF ((cellstate(i,j)%egt - cellstate(i,j)%egtss) < -eros_thresh) THEN
                sum_salt_loss = sum_salt_loss + (cellstate(i,j)%egt - cellstate(i,j)%egtss)
                cnt_eros = cnt_eros + 1
             END IF
             ngdpt = ngdpt + 1
          end if
       END DO
    END DO
    IF (cnt_eros /= 0) Have_Erosion = .TRUE.  !We have erosion occurring, set flag for use later

   ! Determine if we have any net soil deposition occurring from any grid cell (deposition)
    ! We assume that we don't have any net suspension deposition if we don't have any mry salt/creep deposition
    sum_salt_dep = 0.0; cnt_dep = 0
    DO i = 1, imax-1 
       DO j = 1, jmax-1 
          if( (isr .eq. 0) .or. (isr .eq. cellstate(i,j)%csr) ) then
             IF ((cellstate(i,j)%egt - cellstate(i,j)%egtss) > eros_thresh) THEN
                sum_salt_dep = sum_salt_dep + (cellstate(i,j)%egt - cellstate(i,j)%egtss)
                cnt_dep = cnt_dep + 1
             END IF
          end if
       END DO
    END DO
    IF (cnt_dep /= 0) Have_Deposition = .TRUE.  !We have deposition occurring, set flag for use later
! ------------------------------------------------------------------------------------------------------------------

    monthly_update(Crop_Transp)%val = monthly_update(Crop_Transp)%val + h1et%zpta
    monthly_update(Crop_Transp)%cnt = monthly_update(Crop_Transp)%cnt + 1
    mrot_update(Crop_Transp,cm)%val = mrot_update(Crop_Transp,cm)%val + h1et%zpta
    mrot_update(Crop_Transp,cm)%cnt = mrot_update(Crop_Transp,cm)%cnt + 1

    monthly_update(Evaporation)%val = monthly_update(Evaporation)%val + h1et%zea
    monthly_update(Evaporation)%cnt = monthly_update(Evaporation)%cnt + 1
    mrot_update(Evaporation,cm)%val = mrot_update(Evaporation,cm)%val + h1et%zea
    mrot_update(Evaporation,cm)%cnt = mrot_update(Evaporation,cm)%cnt + 1

    monthly_update(Runoff)%val = monthly_update(Runoff)%val + h1et%zrun
    monthly_update(Runoff)%cnt = monthly_update(Runoff)%cnt + 1
    mrot_update(Runoff,cm)%val = mrot_update(Runoff,cm)%val + h1et%zrun
    mrot_update(Runoff,cm)%cnt = mrot_update(Runoff,cm)%cnt + 1

    monthly_update(Drainage)%val = monthly_update(Drainage)%val + h1et%zper
    monthly_update(Drainage)%cnt = monthly_update(Drainage)%cnt + 1
    mrot_update(Drainage,cm)%val = mrot_update(Drainage,cm)%val + h1et%zper
    mrot_update(Drainage,cm)%cnt = mrot_update(Drainage,cm)%cnt + 1

    DO i = 1, imax-1 
       DO j = 1, jmax-1 
          if( (isr .eq. 0) .or. (isr .eq. cellstate(i,j)%csr) ) then
             monthly_update(Eros_loss)%val = monthly_update(Eros_loss)%val + cellstate(i,j)%egt/ngdpt
             monthly_update(Salt_loss)%val = monthly_update(Salt_loss)%val + (cellstate(i,j)%egt - cellstate(i,j)%egtss)/ngdpt
             monthly_update(Susp_loss)%val = monthly_update(Susp_loss)%val + cellstate(i,j)%egtss/ngdpt
             monthly_update(PM10_loss)%val = monthly_update(PM10_loss)%val + cellstate(i,j)%egt10/ngdpt
             monthly_update(PM2_5_loss)%val = monthly_update(PM2_5_loss)%val + cellstate(i,j)%egt2_5/ngdpt

             mrot_update(Eros_loss,cm)%val = mrot_update(Eros_loss,cm)%val + cellstate(i,j)%egt/ngdpt
             mrot_update(Salt_loss,cm)%val = mrot_update(Salt_loss,cm)%val + (cellstate(i,j)%egt - cellstate(i,j)%egtss)/ngdpt
             mrot_update(Susp_loss,cm)%val = mrot_update(Susp_loss,cm)%val + cellstate(i,j)%egtss/ngdpt
             mrot_update(PM10_loss,cm)%val = mrot_update(PM10_loss,cm)%val + cellstate(i,j)%egt10/ngdpt
             mrot_update(PM2_5_loss,cm)%val = mrot_update(PM2_5_loss,cm)%val + cellstate(i,j)%egt2_5/ngdpt
          end if
       END DO
    END DO
    monthly_update(Eros_loss)%cnt = monthly_update(Eros_loss)%cnt + 1
    monthly_update(Salt_loss)%cnt = monthly_update(Salt_loss)%cnt + 1
    monthly_update(Susp_loss)%cnt = monthly_update(Susp_loss)%cnt + 1
    monthly_update(PM10_loss)%cnt = monthly_update(PM10_loss)%cnt + 1
    monthly_update(PM2_5_loss)%cnt = monthly_update(PM2_5_loss)%cnt + 1

    mrot_update(Eros_loss,cm)%cnt = mrot_update(Eros_loss,cm)%cnt + 1
    mrot_update(Salt_loss,cm)%cnt = mrot_update(Salt_loss,cm)%cnt + 1
    mrot_update(Susp_loss,cm)%cnt = mrot_update(Susp_loss,cm)%cnt + 1
    mrot_update(PM10_loss,cm)%cnt = mrot_update(PM10_loss,cm)%cnt + 1
    mrot_update(PM2_5_loss,cm)%cnt = mrot_update(PM2_5_loss,cm)%cnt + 1

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
          IF ( ABS((cellstate(i,j)%egt - cellstate(i,j)%egtss)) <= eros_thresh) THEN  !Sheltered/TC
             IF ( ABS(cellstate(i,j)%egtss) <= susp_thresh) THEN  ! Sheltered area
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
    monthly_update(Salt_1)%val = monthly_update(Salt_1)%val + cellstate(i,0)%egt/(imax-1)
    monthly_update(Salt_3)%val = monthly_update(Salt_3)%val + cellstate(i,jmax)%egt/(imax-1)
    monthly_update(Susp_1)%val = monthly_update(Susp_1)%val + cellstate(i,0)%egtss/(imax-1)
    monthly_update(Susp_3)%val = monthly_update(Susp_3)%val + cellstate(i,jmax)%egtss/(imax-1)
    monthly_update(PM10_1)%val = monthly_update(PM10_1)%val + cellstate(i,0)%egt10/(imax-1)
    monthly_update(PM10_3)%val = monthly_update(PM10_3)%val + cellstate(i,jmax)%egt10/(imax-1)
    monthly_update(PM2_5_1)%val = monthly_update(PM2_5_1)%val + cellstate(i,0)%egt2_5/(imax-1)
    monthly_update(PM2_5_3)%val = monthly_update(PM2_5_3)%val + cellstate(i,jmax)%egt2_5/(imax-1)

    mrot_update(Salt_1,cm)%val = mrot_update(Salt_1,cm)%val + cellstate(i,0)%egt/(imax-1)
    mrot_update(Salt_3,cm)%val = mrot_update(Salt_3,cm)%val + cellstate(i,jmax)%egt/(imax-1)
    mrot_update(Susp_1,cm)%val = mrot_update(Susp_1,cm)%val + cellstate(i,0)%egtss/(imax-1)
    mrot_update(Susp_3,cm)%val = mrot_update(Susp_3,cm)%val + cellstate(i,jmax)%egtss/(imax-1)
    mrot_update(PM10_1,cm)%val = mrot_update(PM10_1,cm)%val + cellstate(i,0)%egt10/(imax-1)
    mrot_update(PM10_3,cm)%val = mrot_update(PM10_3,cm)%val + cellstate(i,jmax)%egt10/(imax-1)
    mrot_update(PM2_5_1,cm)%val = mrot_update(PM2_5_1,cm)%val + cellstate(i,0)%egt2_5/(imax-1)
    mrot_update(PM2_5_3,cm)%val = mrot_update(PM2_5_3,cm)%val + cellstate(i,jmax)%egt2_5/(imax-1)
  END DO
  monthly_update(Salt_1)%cnt = monthly_update(Salt_1)%cnt + 1
  monthly_update(Salt_3)%cnt = monthly_update(Salt_3)%cnt + 1
  monthly_update(Susp_1)%cnt = monthly_update(Susp_1)%cnt + 1
  monthly_update(Susp_3)%cnt = monthly_update(Susp_3)%cnt + 1
  monthly_update(PM10_1)%cnt = monthly_update(PM10_1)%cnt + 1
  monthly_update(PM10_3)%cnt = monthly_update(PM10_3)%cnt + 1
  monthly_update(PM2_5_1)%cnt = monthly_update(PM2_5_1)%cnt + 1
  monthly_update(PM2_5_3)%cnt = monthly_update(PM2_5_3)%cnt + 1

  mrot_update(Salt_1,cm)%cnt = mrot_update(Salt_1,cm)%cnt + 1
  mrot_update(Salt_3,cm)%cnt = mrot_update(Salt_3,cm)%cnt + 1
  mrot_update(Susp_1,cm)%cnt = mrot_update(Susp_1,cm)%cnt + 1
  mrot_update(Susp_3,cm)%cnt = mrot_update(Susp_3,cm)%cnt + 1
  mrot_update(PM10_1,cm)%cnt = mrot_update(PM10_1,cm)%cnt + 1
  mrot_update(PM10_3,cm)%cnt = mrot_update(PM10_3,cm)%cnt + 1
  mrot_update(PM2_5_1,cm)%cnt = mrot_update(PM2_5_1,cm)%cnt + 1
  mrot_update(PM2_5_3,cm)%cnt = mrot_update(PM2_5_3,cm)%cnt + 1

  DO j = 0, jmax 
    ! Note that egt contains creep+saltation not total soil loss on boundary
    monthly_update(Salt_2)%val = monthly_update(Salt_2)%val + cellstate(0,j)%egt/(jmax-1)
    monthly_update(Salt_4)%val = monthly_update(Salt_4)%val + cellstate(imax,j)%egt/(jmax-1)
    monthly_update(Susp_2)%val = monthly_update(Susp_2)%val + cellstate(0,j)%egtss/(jmax-1)
    monthly_update(Susp_4)%val = monthly_update(Susp_4)%val + cellstate(imax,j)%egtss/(jmax-1)
    monthly_update(PM10_2)%val = monthly_update(PM10_2)%val + cellstate(0,j)%egt10/(jmax-1)
    monthly_update(PM10_4)%val = monthly_update(PM10_4)%val + cellstate(imax,j)%egt10/(jmax-1)
    monthly_update(PM2_5_2)%val = monthly_update(PM2_5_2)%val + cellstate(0,j)%egt2_5/(jmax-1)
    monthly_update(PM2_5_4)%val = monthly_update(PM2_5_4)%val + cellstate(imax,j)%egt2_5/(jmax-1)

    mrot_update(Salt_2,cm)%val = mrot_update(Salt_2,cm)%val + cellstate(0,j)%egt/(jmax-1)
    mrot_update(Salt_4,cm)%val = mrot_update(Salt_4,cm)%val + cellstate(imax,j)%egt/(jmax-1)
    mrot_update(Susp_2,cm)%val = mrot_update(Susp_2,cm)%val + cellstate(0,j)%egtss/(jmax-1)
    mrot_update(Susp_4,cm)%val = mrot_update(Susp_4,cm)%val + cellstate(imax,j)%egtss/(jmax-1)
    mrot_update(PM10_2,cm)%val = mrot_update(PM10_2,cm)%val + cellstate(0,j)%egt10/(jmax-1)
    mrot_update(PM10_4,cm)%val = mrot_update(PM10_4,cm)%val + cellstate(imax,j)%egt10/(jmax-1)
    mrot_update(PM2_5_2,cm)%val = mrot_update(PM2_5_2,cm)%val + cellstate(0,j)%egt2_5/(jmax-1)
    mrot_update(PM2_5_4,cm)%val = mrot_update(PM2_5_4,cm)%val + cellstate(imax,j)%egt2_5/(jmax-1)
  END DO
  monthly_update(Salt_2)%cnt = monthly_update(Salt_2)%cnt + 1
  monthly_update(Salt_4)%cnt = monthly_update(Salt_4)%cnt + 1
  monthly_update(Susp_2)%cnt = monthly_update(Susp_2)%cnt + 1
  monthly_update(Susp_4)%cnt = monthly_update(Susp_4)%cnt + 1
  monthly_update(PM10_2)%cnt = monthly_update(PM10_2)%cnt + 1
  monthly_update(PM10_4)%cnt = monthly_update(PM10_4)%cnt + 1
  monthly_update(PM2_5_2)%cnt = monthly_update(PM2_5_2)%cnt + 1
  monthly_update(PM2_5_4)%cnt = monthly_update(PM2_5_4)%cnt + 1

  mrot_update(Salt_2,cm)%cnt = mrot_update(Salt_2,cm)%cnt + 1
  mrot_update(Salt_4,cm)%cnt = mrot_update(Salt_4,cm)%cnt + 1
  mrot_update(Susp_2,cm)%cnt = mrot_update(Susp_2,cm)%cnt + 1
  mrot_update(Susp_4,cm)%cnt = mrot_update(Susp_4,cm)%cnt + 1
  mrot_update(PM10_2,cm)%cnt = mrot_update(PM10_2,cm)%cnt + 1
  mrot_update(PM10_4,cm)%cnt = mrot_update(PM10_4,cm)%cnt + 1
  mrot_update(PM2_5_2,cm)%cnt = mrot_update(PM2_5_2,cm)%cnt + 1
  mrot_update(PM2_5_4,cm)%cnt = mrot_update(PM2_5_4,cm)%cnt + 1

  !variables running averaged for period
  we = 0.0
  IF (awudmx > wind_energy_thresh) THEN
     DO i = 1, ntstep
        IF (subday(i)%awu > wind_energy_thresh) THEN
          we = we + 0.5*awdair*(subday(i)%awu**2) * (subday(i)%awu - wind_energy_thresh) * (86400.0/ntstep) * (0.001)    ! (s/day) and (J/kJ)
        END IF
     END DO
  END IF
  CALL run_ave(monthly_update(Wind_energy), we, 1)
  CALL run_ave(mrot_update(Wind_energy,cm), we, 1)

  CALL run_ave(monthly_update(Dryness_ratio), h1et%drat, 1)
  CALL run_ave(mrot_update(Dryness_ratio,cm), h1et%drat, 1)

  CALL run_ave(monthly_update(Snow_cover), h1et%snow_protect, 1)
  CALL run_ave(mrot_update(Snow_cover,cm), h1et%snow_protect, 1)

  END SUBROUTINE update_monthly_update_vars

  ! Update both monthly and rot_month reporting variables
  SUBROUTINE update_monthly_report_vars(cur_month, cur_year, nrot_years, monthly_update, mrot_update, monthly_report, monthly_dates)

    USE pd_dates_vars
    USE pd_var_type_def
    USE pd_var_tables

    IMPLICIT NONE

    INTEGER, INTENT (IN) :: cur_month
    INTEGER, INTENT (IN) :: cur_year
    INTEGER, INTENT (IN) :: nrot_years
    TYPE (pd_var_type), DIMENSION(Min_monthly_vars:), intent(inout) :: monthly_update
    TYPE (pd_var_type), DIMENSION(Min_monthly_vars:,:), intent(inout) :: mrot_update
    TYPE (pd_var_type), DIMENSION(Min_monthly_vars:,:,0:), intent(inout) :: monthly_report
    TYPE (pd_dates_type), DIMENSION(:,:), intent(inout) :: monthly_dates

    INTEGER :: i        ! local loop variables
    INTEGER :: rot_y    ! local variables

    REAL, PARAMETER :: m2_to_ha = 10000.0  ! m^2 in a ha

    rot_y = mod(cur_year-1,nrot_years)+1

    ! Compute monthly reporting variables here

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
          IF (i == Precipi .OR. i==Irrigation) THEN  ! These have only been "summed"
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

    ! reset monthly rotation (mrot) update vars
    IF (rot_y == nrot_years) THEN
       DO i=Min_monthly_vars,Max_monthly_vars
          mrot_update(i,cur_month)%cnt = 0
          mrot_update(i,cur_month)%val = 0.0
       END DO
       ! monthly_dates(cur_month,0)%ey = monthly_dates(cur_month,0)%ey + nrot_years 
    END IF
  END SUBROUTINE update_monthly_report_vars

  SUBROUTINE update_yrly_update_vars(isr, yrly_update, yrot_update, yr_update, cellstate, h1et)

    USE pd_var_type_def
    USE pd_var_tables
    use erosion_data_struct_defs, only: cellsurfacestate, awdair, awudmx, subday, ntstep 
    use grid_mod, only: imax, jmax, sim_area
    use hydro_data_struct_defs, only: hydro_derived_et
    use climate_input_mod, only: cli_today

    IMPLICIT NONE

    INTEGER, intent (in) :: isr  ! current subregion
    TYPE (pd_var_type), DIMENSION(Min_yrly_vars:), intent(inout) :: yrly_update
    TYPE (pd_var_type), DIMENSION(Min_yrly_vars:), intent(inout) :: yrot_update
    TYPE (pd_var_type), DIMENSION(Min_yrly_vars:), intent(inout) :: yr_update
    type(cellsurfacestate), dimension(0:,0:), intent(in) :: cellstate  ! egt, egtcs, egtss, egt10
    type(hydro_derived_et), intent(in) :: h1et

    INTEGER :: i,j              ! local loop variables
    INTEGER :: ngdpt            ! number of simulation grid datapoints
    REAL    :: gdpt_area        ! area of a grid cell (point) in m^2

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

    gdpt_area = sim_area/( (imax-1) * (jmax-1) )   !Area of single grid cell

    !variables summed for period
    yrly_update(Precipi)%val = yrly_update(Precipi)%val + cli_today%zdpt
    yrly_update(Precipi)%cnt = yrly_update(Precipi)%cnt + 1
    yrot_update(Precipi)%val = yrot_update(Precipi)%val + cli_today%zdpt
    yrot_update(Precipi)%cnt = yrot_update(Precipi)%cnt + 1

    yrly_update(Irrigation)%val = yrly_update(Irrigation)%val + h1et%zirr
    yrly_update(Irrigation)%cnt = yrly_update(Irrigation)%cnt + 1
    yrot_update(Irrigation)%val = yrot_update(Irrigation)%val + h1et%zirr
    yrot_update(Irrigation)%cnt = yrot_update(Irrigation)%cnt + 1

    ! For a year by year report of yearly (and rotation year) averaged variables
    yr_update(Precipi)%val = yr_update(Precipi)%val + cli_today%zdpt
    yr_update(Precipi)%cnt = yr_update(Precipi)%cnt + 1

    yr_update(Irrigation)%val = yr_update(Irrigation)%val + h1et%zirr
    yr_update(Irrigation)%cnt = yr_update(Irrigation)%cnt + 1
! ------------------------------------------------------------------------------------------------------------------
    ! Determine if we have any net soil loss occurring from any grid cell (erosion)
    ! We assume that we don't have any net suspension loss if we don't have any net salt/creep loss
    ngdpt = 0   ! count number of grid points in this subregion (if zero, it is not used)
    sum_salt_loss = 0.0; cnt_eros = 0
    DO i = 1, imax-1 
       DO j = 1, jmax-1 
          if( (isr .eq. 0) .or. (isr .eq. cellstate(i,j)%csr) ) then
             IF ((cellstate(i,j)%egt - cellstate(i,j)%egtss) < 0.0) THEN
                sum_salt_loss = sum_salt_loss + (cellstate(i,j)%egt - cellstate(i,j)%egtss)
                cnt_eros = cnt_eros + 1
             END IF
             ngdpt = ngdpt + 1
          end if
       END DO
    END DO
    IF (cnt_eros /= 0) Have_Erosion = .TRUE.  !We have erosion occurring, set flag for use later

   ! Determine if we have any net soil deposition occurring from any grid cell (deposition)
    ! We assume that we don't have any net suspension deposition if we don't have any mry salt/creep deposition
    sum_salt_dep = 0.0; cnt_dep = 0
    DO i = 1, imax-1 
       DO j = 1, jmax-1 
          if( (isr .eq. 0) .or. (isr .eq. cellstate(i,j)%csr) ) then
             IF ((cellstate(i,j)%egt - cellstate(i,j)%egtss) > 0.0) THEN
                sum_salt_dep = sum_salt_dep + (cellstate(i,j)%egt - cellstate(i,j)%egtss)
                cnt_dep = cnt_dep + 1
             END IF
          end if
       END DO
    END DO
    IF (cnt_dep /= 0) Have_Deposition = .TRUE.  !We have deposition occurring, set flag for use later
! ------------------------------------------------------------------------------------------------------------------

    yrly_update(Crop_Transp)%val = yrly_update(Crop_Transp)%val + h1et%zpta
    yrly_update(Crop_Transp)%cnt = yrly_update(Crop_Transp)%cnt + 1
    yrot_update(Crop_Transp)%val = yrot_update(Crop_Transp)%val + h1et%zpta
    yrot_update(Crop_Transp)%cnt = yrot_update(Crop_Transp)%cnt + 1
    yr_update(Crop_Transp)%val = yr_update(Crop_Transp)%val + h1et%zpta
    yr_update(Crop_Transp)%cnt = yr_update(Crop_Transp)%cnt + 1

    yrly_update(Evaporation)%val = yrly_update(Evaporation)%val + h1et%zea
    yrly_update(Evaporation)%cnt = yrly_update(Evaporation)%cnt + 1
    yrot_update(Evaporation)%val = yrot_update(Evaporation)%val + h1et%zea
    yrot_update(Evaporation)%cnt = yrot_update(Evaporation)%cnt + 1
    yr_update(Evaporation)%val = yr_update(Evaporation)%val + h1et%zea
    yr_update(Evaporation)%cnt = yr_update(Evaporation)%cnt + 1

    yrly_update(Runoff)%val = yrly_update(Runoff)%val + h1et%zrun
    yrly_update(Runoff)%cnt = yrly_update(Runoff)%cnt + 1
    yrot_update(Runoff)%val = yrot_update(Runoff)%val + h1et%zrun
    yrot_update(Runoff)%cnt = yrot_update(Runoff)%cnt + 1
    yr_update(Runoff)%val = yr_update(Runoff)%val + h1et%zrun
    yr_update(Runoff)%cnt = yr_update(Runoff)%cnt + 1

    yrly_update(Drainage)%val = yrly_update(Drainage)%val + h1et%zper
    yrly_update(Drainage)%cnt = yrly_update(Drainage)%cnt + 1
    yrot_update(Drainage)%val = yrot_update(Drainage)%val + h1et%zper
    yrot_update(Drainage)%cnt = yrot_update(Drainage)%cnt + 1
    yr_update(Drainage)%val = yr_update(Drainage)%val + h1et%zper
    yr_update(Drainage)%cnt = yr_update(Drainage)%cnt + 1

    DO i = 1, imax-1 
       DO j = 1, jmax-1 
          if( (isr .eq. 0) .or. (isr .eq. cellstate(i,j)%csr) ) then
             yrly_update(Eros_loss)%val = yrly_update(Eros_loss)%val + cellstate(i,j)%egt/ngdpt
             yrly_update(Salt_loss)%val = yrly_update(Salt_loss)%val + (cellstate(i,j)%egt - cellstate(i,j)%egtss)/ngdpt
             yrly_update(Susp_loss)%val = yrly_update(Susp_loss)%val + cellstate(i,j)%egtss/ngdpt
             yrly_update(PM10_loss)%val = yrly_update(PM10_loss)%val + cellstate(i,j)%egt10/ngdpt
             yrly_update(PM2_5_loss)%val = yrly_update(PM2_5_loss)%val + cellstate(i,j)%egt2_5/ngdpt

             yrot_update(Eros_loss)%val = yrot_update(Eros_loss)%val + cellstate(i,j)%egt/ngdpt
             yrot_update(Salt_loss)%val = yrot_update(Salt_loss)%val + (cellstate(i,j)%egt - cellstate(i,j)%egtss)/ngdpt
             yrot_update(Susp_loss)%val = yrot_update(Susp_loss)%val + cellstate(i,j)%egtss/ngdpt
             yrot_update(PM10_loss)%val = yrot_update(PM10_loss)%val + cellstate(i,j)%egt10/ngdpt
             yrot_update(PM2_5_loss)%val = yrot_update(PM2_5_loss)%val + cellstate(i,j)%egt2_5/ngdpt

             ! For a year by year report of yearly (and rotation year) averaged variables
             yr_update(Eros_loss)%val = yr_update(Eros_loss)%val + cellstate(i,j)%egt/ngdpt
             yr_update(Salt_loss)%val = yr_update(Salt_loss)%val + (cellstate(i,j)%egt - cellstate(i,j)%egtss)/ngdpt
             yr_update(Susp_loss)%val = yr_update(Susp_loss)%val + cellstate(i,j)%egtss/ngdpt
             yr_update(PM10_loss)%val = yr_update(PM10_loss)%val + cellstate(i,j)%egt10/ngdpt
             yr_update(PM2_5_loss)%val = yr_update(PM2_5_loss)%val + cellstate(i,j)%egt10/ngdpt
          end if
       END DO
    END DO
    yrly_update(Eros_loss)%cnt = yrly_update(Eros_loss)%cnt + 1
    yrly_update(Salt_loss)%cnt = yrly_update(Salt_loss)%cnt + 1
    yrly_update(Susp_loss)%cnt = yrly_update(Susp_loss)%cnt + 1
    yrly_update(PM10_loss)%cnt = yrly_update(PM10_loss)%cnt + 1
    yrly_update(PM2_5_loss)%cnt = yrly_update(PM2_5_loss)%cnt + 1

    yrot_update(Eros_loss)%cnt = yrot_update(Eros_loss)%cnt + 1
    yrot_update(Salt_loss)%cnt = yrot_update(Salt_loss)%cnt + 1
    yrot_update(Susp_loss)%cnt = yrot_update(Susp_loss)%cnt + 1
    yrot_update(PM10_loss)%cnt = yrot_update(PM10_loss)%cnt + 1
    yrot_update(PM2_5_loss)%cnt = yrot_update(PM2_5_loss)%cnt + 1

    ! For a year by year report of yearly (and rotation year) averaged variables
    yr_update(Eros_loss)%cnt = yr_update(Eros_loss)%cnt + 1
    yr_update(Salt_loss)%cnt = yr_update(Salt_loss)%cnt + 1
    yr_update(Susp_loss)%cnt = yr_update(Susp_loss)%cnt + 1
    yr_update(PM10_loss)%cnt = yr_update(PM10_loss)%cnt + 1
    yr_update(PM2_5_loss)%cnt = yr_update(PM2_5_loss)%cnt + 1

    IF (Have_Erosion) THEN !We have erosion somewhere
       yrly_update(N_eros_events)%val = yrly_update(N_eros_events)%val + 1.0  ! Count the erosion events
       yrot_update(N_eros_events)%val = yrot_update(N_eros_events)%val + 1.0  ! Count the erosion events
       yr_update(N_eros_events)%val = yr_update(N_eros_events)%val + 1.0  ! Count the erosion events
    END IF
    yrly_update(N_eros_events)%cnt = yrly_update(N_eros_events)%cnt + 1
    yrot_update(N_eros_events)%cnt = yrot_update(N_eros_events)%cnt + 1
    yr_update(N_eros_events)%cnt = yr_update(N_eros_events)%cnt + 1

    IF (Have_Erosion) THEN

       yrly_update(Salt_loss2)%val = yrly_update(Salt_loss2)%val + sum_salt_loss/ngdpt
       yrly_update(Salt_loss2)%cnt = yrly_update(Salt_loss2)%cnt + 1
       !To get total mass: (sum_salt_loss/cnt)*(cnt*gdpt_area)
       yrly_update(Salt_loss2_mass)%val = yrly_update(Salt_loss2_mass)%val + (sum_salt_loss*gdpt_area)
       yrly_update(Salt_loss2_mass)%cnt = yrly_update(Salt_loss2_mass)%cnt + 1
       !Total salt loss area in (ha)
       CALL run_ave (yrly_update(Salt_loss2_area), REAL(cnt_eros)*gdpt_area/m2_to_ha, 1)
       CALL run_ave (yrly_update(Salt_loss2_frac), REAL(cnt_eros)/ngdpt, 1)
       !compute as: Salt_loss2_mass/(Salt_loss2_area * m2_to_ha)
       yrly_update(Salt_loss2_rate)%val =                                 &
                   yrly_update(Salt_loss2_mass)%val /                     &
                   (yrly_update(Salt_loss2_area)%val * m2_to_ha)
       yrly_update(Salt_loss2_rate)%cnt = yrly_update(Salt_loss2_rate)%cnt + 1

       yrot_update(Salt_loss2)%val = yrot_update(Salt_loss2)%val + sum_salt_loss/ngdpt
       yrot_update(Salt_loss2)%cnt = yrot_update(Salt_loss2)%cnt + 1
       yrot_update(Salt_loss2_mass)%val = yrot_update(Salt_loss2_mass)%val + (sum_salt_loss*gdpt_area)
       yrot_update(Salt_loss2_mass)%cnt = yrot_update(Salt_loss2_mass)%cnt + 1
       CALL run_ave (yrot_update(Salt_loss2_area), REAL(cnt_eros)*gdpt_area/m2_to_ha, 1)
       CALL run_ave (yrot_update(Salt_loss2_frac), REAL(cnt_eros)/ngdpt, 1)
       !compute as: Salt_loss2_mass/(Salt_loss2_area * m2_to_ha)
       yrot_update(Salt_loss2_rate)%val =                                 &
                   yrot_update(Salt_loss2_mass)%val /                     &
                   (yrot_update(Salt_loss2_area)%val * m2_to_ha)
       yrot_update(Salt_loss2_rate)%cnt = yrot_update(Salt_loss2_rate)%cnt + 1

       ! For a year by year report of yearly (and rotation year) averaged variables
       yr_update(Salt_loss2)%val = yr_update(Salt_loss2)%val + sum_salt_loss/ngdpt
       yr_update(Salt_loss2)%cnt = yr_update(Salt_loss2)%cnt + 1
       !To get total mass: (sum_salt_loss/cnt)*(cnt*gdpt_area)
       yr_update(Salt_loss2_mass)%val = yr_update(Salt_loss2_mass)%val + (sum_salt_loss*gdpt_area)
       yr_update(Salt_loss2_mass)%cnt = yr_update(Salt_loss2_mass)%cnt + 1
       !Total salt loss area in (ha)
       CALL run_ave (yr_update(Salt_loss2_area), REAL(cnt_eros)*gdpt_area/m2_to_ha, 1)
       CALL run_ave (yr_update(Salt_loss2_frac), REAL(cnt_eros)/ngdpt, 1)
       !compute as: Salt_loss2_mass/(Salt_loss2_area * m2_to_ha)
       yr_update(Salt_loss2_rate)%val =                                 &
                   yr_update(Salt_loss2_mass)%val /                     &
                   (yr_update(Salt_loss2_area)%val * m2_to_ha)
       yr_update(Salt_loss2_rate)%cnt = yr_update(Salt_loss2_rate)%cnt + 1
    END IF

     IF (Have_Deposition) THEN
       yrly_update(Salt_dep2)%val = yrly_update(Salt_dep2)%val + sum_salt_dep/ngdpt
       yrly_update(Salt_dep2)%cnt = yrly_update(Salt_dep2)%cnt + 1
       !To get total mass: (sum_salt_dep/cnt)*(cnt*gdpt_area)
       yrly_update(Salt_dep2_mass)%val = yrly_update(Salt_dep2_mass)%val + (sum_salt_dep*gdpt_area)
       yrly_update(Salt_dep2_mass)%cnt = yrly_update(Salt_dep2_mass)%cnt + 1
       CALL run_ave (yrly_update(Salt_dep2_area), REAL(cnt_dep)*gdpt_area/m2_to_ha, 1)
       CALL run_ave (yrly_update(Salt_dep2_frac), REAL(cnt_dep)/ngdpt, 1)
       !compute as: Salt_dep2_mass/(Salt_dep2_area * m2_to_ha)
       yrly_update(Salt_dep2_rate)%val =                                  &
                   yrly_update(Salt_dep2_mass)%val /                      &
                   (yrly_update(Salt_dep2_area)%val * m2_to_ha)
       yrly_update(Salt_dep2_rate)%cnt = yrly_update(Salt_dep2_rate)%cnt + 1

       yrot_update(Salt_dep2)%val = yrot_update(Salt_dep2)%val + sum_salt_dep/ngdpt
       yrot_update(Salt_dep2)%cnt = yrot_update(Salt_dep2)%cnt + 1
       yrot_update(Salt_dep2_mass)%val = yrot_update(Salt_dep2_mass)%val + (sum_salt_dep*gdpt_area)
       yrot_update(Salt_dep2_mass)%cnt = yrot_update(Salt_dep2_mass)%cnt + 1
       CALL run_ave (yrot_update(Salt_dep2_area), REAL(cnt_dep)*gdpt_area/m2_to_ha, 1)
       CALL run_ave (yrot_update(Salt_dep2_frac), REAL(cnt_dep)/ngdpt, 1)
       !compute as: Salt_dep2_mass/(Salt_dep2_area * m2_to_ha)
       yrot_update(Salt_dep2_rate)%val =                                  &
                   yrot_update(Salt_dep2_mass)%val /                      &
                   (yrot_update(Salt_dep2_area)%val * m2_to_ha)
       yrot_update(Salt_dep2_rate)%cnt = yrot_update(Salt_dep2_rate)%cnt + 1

       ! For a year by year report of yearly (and rotation year) averaged variables
       yr_update(Salt_dep2)%val = yr_update(Salt_dep2)%val + sum_salt_dep/ngdpt
       yr_update(Salt_dep2)%cnt = yr_update(Salt_dep2)%cnt + 1
       !To get total mass: (sum_salt_dep/cnt)*(cnt*gdpt_area)
       yr_update(Salt_dep2_mass)%val = yr_update(Salt_dep2_mass)%val + (sum_salt_dep*gdpt_area)
       yr_update(Salt_dep2_mass)%cnt = yr_update(Salt_dep2_mass)%cnt + 1
       CALL run_ave (yr_update(Salt_dep2_area), REAL(cnt_dep)*gdpt_area/m2_to_ha, 1)
       CALL run_ave (yr_update(Salt_dep2_frac), REAL(cnt_dep)/ngdpt, 1)
       !compute as: Salt_dep2_mass/(Salt_dep2_area * m2_to_ha)
       yr_update(Salt_dep2_rate)%val =                                  &
                   yr_update(Salt_dep2_mass)%val /                      &
                   (yr_update(Salt_dep2_area)%val * m2_to_ha)
       yr_update(Salt_dep2_rate)%cnt = yr_update(Salt_dep2_rate)%cnt + 1
    END IF

  IF (Have_Erosion) THEN      !We have erosion so compute TC, etc.

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
          IF ( ABS((cellstate(i,j)%egt - cellstate(i,j)%egtss)) <= eros_thresh) THEN  !Sheltered/TC
             IF ( ABS(cellstate(i,j)%egtss) <= susp_thresh) THEN  ! Sheltered area
                cnt_sheltered = cnt_sheltered + 1
             ELSE                                       ! At TC
                cnt_transp = cnt_transp + 1
             END IF
          END IF
       END DO
    END DO

    ! Note: Don't currently have a way of computing Flux Rate over a grid area
    CALL run_ave (yrly_update(Trans_cap_area), REAL(cnt_transp)*gdpt_area/m2_to_ha, 1)
    CALL run_ave (yrly_update(Trans_cap_frac), REAL(cnt_transp)/ngdpt, 1)
    CALL run_ave (yrot_update(Trans_cap_area), REAL(cnt_transp)*gdpt_area/m2_to_ha, 1)
    CALL run_ave (yrot_update(Trans_cap_frac), REAL(cnt_transp)/ngdpt, 1)
    ! For a year by year report of yearly (and rotation year) averaged variables
    CALL run_ave (yr_update(Trans_cap_area), REAL(cnt_transp)*gdpt_area/m2_to_ha, 1)
    CALL run_ave (yr_update(Trans_cap_frac), REAL(cnt_transp)/ngdpt, 1)

    !Sheltered region is now computed.
    CALL run_ave (yrly_update(Sheltered_area), REAL(cnt_sheltered)*gdpt_area/m2_to_ha, 1)
    CALL run_ave (yrly_update(Sheltered_frac), REAL(cnt_sheltered)/ngdpt, 1)
    CALL run_ave (yrot_update(Sheltered_area), REAL(cnt_sheltered)*gdpt_area/m2_to_ha, 1)
    CALL run_ave (yrot_update(Sheltered_frac), REAL(cnt_sheltered)/ngdpt, 1)
    ! For a year by year report of yearly (and rotation year) averaged variables
    CALL run_ave (yr_update(Sheltered_area), REAL(cnt_sheltered)*gdpt_area/m2_to_ha, 1)
    CALL run_ave (yr_update(Sheltered_frac), REAL(cnt_sheltered)/ngdpt, 1)
  END IF  !Have_Erosion flag

! Sum boundary losses  (ave value per boundary grid point)
  DO i = 0, imax 
    ! Note that egt contains creep+saltation not total soil loss on boundary
    yrly_update(Salt_1)%val = yrly_update(Salt_1)%val + cellstate(i,0)%egt/(imax-1)
    yrly_update(Salt_3)%val = yrly_update(Salt_3)%val + cellstate(i,jmax)%egt/(imax-1)
    yrly_update(Susp_1)%val = yrly_update(Susp_1)%val + cellstate(i,0)%egtss/(imax-1)
    yrly_update(Susp_3)%val = yrly_update(Susp_3)%val + cellstate(i,jmax)%egtss/(imax-1)
    yrly_update(PM10_1)%val = yrly_update(PM10_1)%val + cellstate(i,0)%egt10/(imax-1)
    yrly_update(PM10_3)%val = yrly_update(PM10_3)%val + cellstate(i,jmax)%egt10/(imax-1)
    yrly_update(PM2_5_1)%val = yrly_update(PM2_5_1)%val + cellstate(i,0)%egt2_5/(imax-1)
    yrly_update(PM2_5_3)%val = yrly_update(PM2_5_3)%val + cellstate(i,jmax)%egt2_5/(imax-1)

    yrot_update(Salt_1)%val = yrot_update(Salt_1)%val + cellstate(i,0)%egt/(imax-1)
    yrot_update(Salt_3)%val = yrot_update(Salt_3)%val + cellstate(i,jmax)%egt/(imax-1)
    yrot_update(Susp_1)%val = yrot_update(Susp_1)%val + cellstate(i,0)%egtss/(imax-1)
    yrot_update(Susp_3)%val = yrot_update(Susp_3)%val + cellstate(i,jmax)%egtss/(imax-1)
    yrot_update(PM10_1)%val = yrot_update(PM10_1)%val + cellstate(i,0)%egt10/(imax-1)
    yrot_update(PM10_3)%val = yrot_update(PM10_3)%val + cellstate(i,jmax)%egt10/(imax-1)
    yrot_update(PM2_5_1)%val = yrot_update(PM2_5_1)%val + cellstate(i,0)%egt2_5/(imax-1)
    yrot_update(PM2_5_3)%val = yrot_update(PM2_5_3)%val + cellstate(i,jmax)%egt2_5/(imax-1)

    ! For a year by year report of yearly (and rotation year) averaged variables
    yr_update(Salt_1)%val = yr_update(Salt_1)%val + cellstate(i,0)%egt/(imax-1)
    yr_update(Salt_3)%val = yr_update(Salt_3)%val + cellstate(i,jmax)%egt/(imax-1)
    yr_update(Susp_1)%val = yr_update(Susp_1)%val + cellstate(i,0)%egtss/(imax-1)
    yr_update(Susp_3)%val = yr_update(Susp_3)%val + cellstate(i,jmax)%egtss/(imax-1)
    yr_update(PM10_1)%val = yr_update(PM10_1)%val + cellstate(i,0)%egt10/(imax-1)
    yr_update(PM10_3)%val = yr_update(PM10_3)%val + cellstate(i,jmax)%egt10/(imax-1)
    yr_update(PM2_5_1)%val = yr_update(PM2_5_1)%val + cellstate(i,0)%egt2_5/(imax-1)
    yr_update(PM2_5_3)%val = yr_update(PM2_5_3)%val + cellstate(i,jmax)%egt2_5/(imax-1)
  END DO
  yrly_update(Salt_1)%cnt = yrly_update(Salt_1)%cnt + 1
  yrly_update(Salt_3)%cnt = yrly_update(Salt_3)%cnt + 1
  yrly_update(Susp_1)%cnt = yrly_update(Susp_1)%cnt + 1
  yrly_update(Susp_3)%cnt = yrly_update(Susp_3)%cnt + 1
  yrly_update(PM10_1)%cnt = yrly_update(PM10_1)%cnt + 1
  yrly_update(PM10_3)%cnt = yrly_update(PM10_3)%cnt + 1
  yrly_update(PM2_5_1)%cnt = yrly_update(PM2_5_1)%cnt + 1
  yrly_update(PM2_5_3)%cnt = yrly_update(PM2_5_3)%cnt + 1

  yrot_update(Salt_1)%cnt = yrot_update(Salt_1)%cnt + 1
  yrot_update(Salt_3)%cnt = yrot_update(Salt_3)%cnt + 1
  yrot_update(Susp_1)%cnt = yrot_update(Susp_1)%cnt + 1
  yrot_update(Susp_3)%cnt = yrot_update(Susp_3)%cnt + 1
  yrot_update(PM10_1)%cnt = yrot_update(PM10_1)%cnt + 1
  yrot_update(PM10_3)%cnt = yrot_update(PM10_3)%cnt + 1
  yrot_update(PM2_5_1)%cnt = yrot_update(PM2_5_1)%cnt + 1
  yrot_update(PM2_5_3)%cnt = yrot_update(PM2_5_3)%cnt + 1

  ! For a year by year report of yearly (and rotation year) averaged variables
  yr_update(Salt_1)%cnt = yr_update(Salt_1)%cnt + 1
  yr_update(Salt_3)%cnt = yr_update(Salt_3)%cnt + 1
  yr_update(Susp_1)%cnt = yr_update(Susp_1)%cnt + 1
  yr_update(Susp_3)%cnt = yr_update(Susp_3)%cnt + 1
  yr_update(PM10_1)%cnt = yr_update(PM10_1)%cnt + 1
  yr_update(PM10_3)%cnt = yr_update(PM10_3)%cnt + 1
  yr_update(PM2_5_1)%cnt = yr_update(PM2_5_1)%cnt + 1
  yr_update(PM2_5_3)%cnt = yr_update(PM2_5_3)%cnt + 1

  DO j = 0, jmax 
    ! Note that egt contains creep+saltation not total soil loss on boundary
    yrly_update(Salt_2)%val = yrly_update(Salt_2)%val + cellstate(0,j)%egt/(jmax-1)
    yrly_update(Salt_4)%val = yrly_update(Salt_4)%val + cellstate(imax,j)%egt/(jmax-1)
    yrly_update(Susp_2)%val = yrly_update(Susp_2)%val + cellstate(0,j)%egtss/(jmax-1)
    yrly_update(Susp_4)%val = yrly_update(Susp_4)%val + cellstate(imax,j)%egtss/(jmax-1)
    yrly_update(PM10_2)%val = yrly_update(PM10_2)%val + cellstate(0,j)%egt10/(jmax-1)
    yrly_update(PM10_4)%val = yrly_update(PM10_4)%val + cellstate(imax,j)%egt10/(jmax-1)
    yrly_update(PM2_5_2)%val = yrly_update(PM2_5_2)%val + cellstate(0,j)%egt2_5/(jmax-1)
    yrly_update(PM2_5_4)%val = yrly_update(PM2_5_4)%val + cellstate(imax,j)%egt2_5/(jmax-1)

    yrot_update(Salt_2)%val = yrot_update(Salt_2)%val + cellstate(0,j)%egt/(jmax-1)
    yrot_update(Salt_4)%val = yrot_update(Salt_4)%val + cellstate(imax,j)%egt/(jmax-1)
    yrot_update(Susp_2)%val = yrot_update(Susp_2)%val + cellstate(0,j)%egtss/(jmax-1)
    yrot_update(Susp_4)%val = yrot_update(Susp_4)%val + cellstate(imax,j)%egtss/(jmax-1)
    yrot_update(PM10_2)%val = yrot_update(PM10_2)%val + cellstate(0,j)%egt10/(jmax-1)
    yrot_update(PM10_4)%val = yrot_update(PM10_4)%val + cellstate(imax,j)%egt10/(jmax-1)
    yrot_update(PM2_5_2)%val = yrot_update(PM2_5_2)%val + cellstate(0,j)%egt2_5/(jmax-1)
    yrot_update(PM2_5_4)%val = yrot_update(PM2_5_4)%val + cellstate(imax,j)%egt2_5/(jmax-1)

    ! For a year by year report of yearly (and rotation year) averaged variables
    yr_update(Salt_2)%val = yr_update(Salt_2)%val + cellstate(0,j)%egt/(jmax-1)
    yr_update(Salt_4)%val = yr_update(Salt_4)%val + cellstate(imax,j)%egt/(jmax-1)
    yr_update(Susp_2)%val = yr_update(Susp_2)%val + cellstate(0,j)%egtss/(jmax-1)
    yr_update(Susp_4)%val = yr_update(Susp_4)%val + cellstate(imax,j)%egtss/(jmax-1)
    yr_update(PM10_2)%val = yr_update(PM10_2)%val + cellstate(0,j)%egt10/(jmax-1)
    yr_update(PM10_4)%val = yr_update(PM10_4)%val + cellstate(imax,j)%egt10/(jmax-1)
    yr_update(PM2_5_2)%val = yr_update(PM2_5_2)%val + cellstate(0,j)%egt2_5/(jmax-1)
    yr_update(PM2_5_4)%val = yr_update(PM2_5_4)%val + cellstate(imax,j)%egt2_5/(jmax-1)
  END DO
  yrly_update(Salt_2)%cnt = yrly_update(Salt_2)%cnt + 1
  yrly_update(Salt_4)%cnt = yrly_update(Salt_4)%cnt + 1
  yrly_update(Susp_2)%cnt = yrly_update(Susp_2)%cnt + 1
  yrly_update(Susp_4)%cnt = yrly_update(Susp_4)%cnt + 1
  yrly_update(PM10_2)%cnt = yrly_update(PM10_2)%cnt + 1
  yrly_update(PM10_4)%cnt = yrly_update(PM10_4)%cnt + 1
  yrly_update(PM2_5_2)%cnt = yrly_update(PM2_5_2)%cnt + 1
  yrly_update(PM2_5_4)%cnt = yrly_update(PM2_5_4)%cnt + 1

  yrot_update(Salt_2)%cnt = yrot_update(Salt_2)%cnt + 1
  yrot_update(Salt_4)%cnt = yrot_update(Salt_4)%cnt + 1
  yrot_update(Susp_2)%cnt = yrot_update(Susp_2)%cnt + 1
  yrot_update(Susp_4)%cnt = yrot_update(Susp_4)%cnt + 1
  yrot_update(PM10_2)%cnt = yrot_update(PM10_2)%cnt + 1
  yrot_update(PM10_4)%cnt = yrot_update(PM10_4)%cnt + 1
  yrot_update(PM2_5_2)%cnt = yrot_update(PM2_5_2)%cnt + 1
  yrot_update(PM2_5_4)%cnt = yrot_update(PM2_5_4)%cnt + 1

  ! For a year by year report of yearly (and rotation year) averaged variables
  yr_update(Salt_2)%cnt = yr_update(Salt_2)%cnt + 1
  yr_update(Salt_4)%cnt = yr_update(Salt_4)%cnt + 1
  yr_update(Susp_2)%cnt = yr_update(Susp_2)%cnt + 1
  yr_update(Susp_4)%cnt = yr_update(Susp_4)%cnt + 1
  yr_update(PM10_2)%cnt = yr_update(PM10_2)%cnt + 1
  yr_update(PM10_4)%cnt = yr_update(PM10_4)%cnt + 1
  yr_update(PM2_5_2)%cnt = yr_update(PM2_5_2)%cnt + 1
  yr_update(PM2_5_4)%cnt = yr_update(PM2_5_4)%cnt + 1

  !variables running averaged for period
  we = 0.0
  IF (awudmx > wind_energy_thresh) THEN
     DO i = 1, ntstep
        IF (subday(i)%awu > wind_energy_thresh) THEN
          we = we + 0.5*awdair*(subday(i)%awu**2) * (subday(i)%awu - wind_energy_thresh) *        &
             (86400.0/ntstep) * (0.001)    ! (s/day) and (J/kJ)
        END IF
     END DO
  END IF
  CALL run_ave(yrly_update(Wind_energy), we, 1)
  CALL run_ave(yrot_update(Wind_energy), we, 1)
  CALL run_ave(yr_update(Wind_energy), we, 1)

  CALL run_ave(yrly_update(Dryness_ratio), h1et%drat, 1)
  CALL run_ave(yrot_update(Dryness_ratio), h1et%drat, 1)
  CALL run_ave(yr_update(Dryness_ratio), h1et%drat, 1)

  CALL run_ave(yrly_update(Snow_cover), h1et%snow_protect, 1)
  CALL run_ave(yrot_update(Snow_cover), h1et%snow_protect, 1)
  CALL run_ave(yr_update(Snow_cover), h1et%snow_protect, 1)

  END SUBROUTINE update_yrly_update_vars


  SUBROUTINE update_yrly_report_vars(cur_year, nrot_years, &
                  yrly_update, yrot_update, yr_update, yrly_report, yr_report, yrly_dates, yr_dates)

    USE pd_dates_vars
    USE pd_var_type_def
    USE pd_var_tables

    IMPLICIT NONE

    INTEGER, INTENT (IN) :: nrot_years
    INTEGER, INTENT (IN) :: cur_year
    TYPE (pd_var_type), DIMENSION(Min_yrly_vars:), intent(inout) :: yrly_update
    TYPE (pd_var_type), DIMENSION(Min_yrly_vars:), intent(inout) :: yrot_update
    TYPE (pd_var_type), DIMENSION(Min_yrly_vars:), intent(inout) :: yr_update
    TYPE (pd_var_type), DIMENSION(Min_yrly_vars:,0:), intent(inout) :: yrly_report
    TYPE (pd_var_type), DIMENSION(Min_yrly_vars:,:), intent(inout) :: yr_report
    TYPE (pd_dates_type), DIMENSION(:), intent(inout) :: yrly_dates
    TYPE (pd_dates_type), DIMENSION(:), intent(inout) :: yr_dates

    INTEGER :: i        ! local loop variables
    INTEGER :: rot_y    ! local variables

    REAL, PARAMETER :: m2_to_ha = 10000.0  ! m^2 in a ha

    rot_y = mod(cur_year-1,nrot_years)+1

    !variables averaged for reporting period
    DO i = Min_cli_vars, Max_cli_vars
       CALL run_ave (yrly_report(i,rot_y), yrly_update(i)%val, 1)
    END DO
    DO i = Min_eave_vars, Max_eave_vars
       CALL run_ave (yrly_report(i,rot_y), yrly_update(i)%val, 1)
    END DO

    ! If we have saltating loss, add the area and fraction info
    ! else only "running ave" the Salt_loss2 rate
    DO i = Min_lave_vars, Max_lave_vars
      IF (yrly_update(Salt_loss2)%cnt > 0) THEN 
            CALL run_ave (yrly_report(i,rot_y), yrly_update(i)%val, 1)
      ELSE !Don't change the area and fract values
         IF (i == Salt_loss2) THEN
            CALL run_ave (yrly_report(i,rot_y), yrly_update(i)%val, 1)
         END IF
      END IF
    END DO
    IF (yrly_report(Salt_loss2_area,rot_y)%val > 0.0) THEN
       yrly_report(Salt_loss2_rate,rot_y)%val =              &
          yrly_report(Salt_loss2_mass,rot_y)%val /           &
          (yrly_report(Salt_loss2_area,rot_y)%val * m2_to_ha)
       yrly_report(Salt_loss2_rate,rot_y)%cnt = yrly_report(Salt_loss2_rate,rot_y)%cnt + 1;
    END IF

    ! If we have deposition, add the area and fraction info
    DO i = Min_dave_vars, Max_dave_vars
      IF (yrly_update(Salt_dep2)%cnt > 0) THEN 
         CALL run_ave (yrly_report(i,rot_y), yrly_update(i)%val, 1 )
      ELSE
         IF (i == Salt_dep2) THEN
            CALL run_ave (yrly_report(i,rot_y), yrly_update(i)%val, 1 )
         END IF
      END IF
    END DO
    IF (yrly_report(Salt_dep2_area,rot_y)%val > 0.0) THEN  !Ever had any deposition during this rot year
       yrly_report(Salt_dep2_rate,rot_y)%val =               &
          yrly_report(Salt_dep2_mass,rot_y)%val /            &
          (yrly_report(Salt_dep2_area,rot_y)%val * m2_to_ha)
    END IF

    ! If we have salt loss, then we may have some transp cap
    DO i = Min_tave_vars, Max_tave_vars
      IF (yrly_update(Salt_loss2)%cnt > 0) THEN 
         CALL run_ave (yrly_report(i,rot_y), yrly_update(i)%val, 1 )
      END IF
    END DO

    ! update the full rotation average variables
    IF (rot_y == nrot_years) THEN

       !variables averaged for reporting period
       DO i = Min_cli_vars, Max_cli_vars
          IF (i == Precipi .OR. i==Irrigation) THEN  ! These have only been "summed"
             CALL run_ave (yrly_report(i,0), yrot_update(i)%val, nrot_years)
          ELSE  !These have already been "running averaged"
             CALL run_ave (yrly_report(i,0), yrot_update(i)%val, 1)
          END IF
       END DO
       DO i = Min_eave_vars, Max_eave_vars
          CALL run_ave (yrly_report(i,0), yrot_update(i)%val, nrot_years)
       END DO
   
       ! If we have saltating loss, add the area and fraction info
       DO i = Min_lave_vars, Max_lave_vars
         IF (i == Salt_loss2) THEN
            !Compute the average "yearly" salt loss per unit area over the entire simulation region
            CALL run_ave (yrly_report(i,0), yrot_update(i)%val, nrot_years)
         ELSE
            IF (yrot_update(Salt_loss2)%cnt > 0) THEN 
               !Pass a count of one so that they don't "average" the area across
               !rotation years.  We want to present the "total" average area
               !affected due to erosion events during the rotation, 
               !NOT the average area "per year" due to erosion events.
               CALL run_ave (yrly_report(i,0), yrot_update(i)%val, 1)
            END IF
         END IF
       END DO
       IF (yrly_report(Salt_loss2_area,0)%val > 0.0) THEN
          yrly_report(Salt_loss2_rate,0)%val =              &
             yrly_report(Salt_loss2_mass,0)%val /           &
             (yrly_report(Salt_loss2_area,0)%val * m2_to_ha) / nrot_years
          yrly_report(Salt_loss2_rate,0)%cnt = yrly_report(Salt_loss2_rate,0)%cnt + 1;
       END IF
   
       ! If we have deposition, add the area and fraction info
       DO i = Min_dave_vars, Max_dave_vars
         IF (i == Salt_dep2) THEN
            !Compute the average "yearly" deposition per unit area over the entire simulation region
            CALL run_ave (yrly_report(i,0), yrot_update(i)%val, nrot_years)
         ELSE
            IF (yrot_update(Salt_loss2)%cnt > 0) THEN 
               !Pass a count of one so that they don't "average" the area across
               !rotation years.  We want to present the "total" average area
               !NOT the "average" area "per year" due to erosion events.
               CALL run_ave (yrly_report(i,0), yrot_update(i)%val, 1)
            END IF
         END IF
       END DO
       IF (yrly_report(Salt_dep2_area,0)%val > 0.0) THEN
          yrly_report(Salt_dep2_rate,0)%val =               &
             yrly_report(Salt_dep2_mass,0)%val /            &
             (yrly_report(Salt_dep2_area,0)%val * m2_to_ha) / nrot_years
       END IF

       ! If we have salt loss, then we may have some transp cap
       DO i = Min_tave_vars, Max_tave_vars
          IF (yrot_update(Salt_loss2)%cnt > 0) THEN 
             !Pass a count of one so that they don't "average" the area across
             !rotation years.  We want to present the "total" area affected
             !not the "average" area per year.
             CALL run_ave (yrly_report(i,0), yrot_update(i)%val, 1)
          END IF
       END DO
    END IF

    !year by year variables averaged for reporting period
    DO i = Min_cli_vars, Max_cli_vars
       CALL run_ave (yr_report(i,cur_year), yr_update(i)%val, 1)
    END DO
    DO i = Min_eave_vars, Max_eave_vars
       CALL run_ave (yr_report(i,cur_year), yr_update(i)%val, 1)
    END DO

    ! If we have saltating loss, add the area and fraction info
    ! else only "running ave" the Salt_loss2 rate
    DO i = Min_lave_vars, Max_lave_vars
      IF (yr_update(Salt_loss2)%cnt > 0) THEN 
            CALL run_ave (yr_report(i,cur_year), yr_update(i)%val, 1)
      ELSE !Don't change the area and fract values
         IF (i == Salt_loss2) THEN
            CALL run_ave (yr_report(i,cur_year), yr_update(i)%val, 1)
         END IF
      END IF
    END DO
    IF (yr_report(Salt_loss2_area,cur_year)%val > 0.0) THEN
       yr_report(Salt_loss2_rate,cur_year)%val =              &
          yr_report(Salt_loss2_mass,cur_year)%val /           &
          (yr_report(Salt_loss2_area,cur_year)%val * m2_to_ha)
       yr_report(Salt_loss2_rate,cur_year)%cnt = yr_report(Salt_loss2_rate,cur_year)%cnt + 1;
    END IF

    ! If we have deposition, add the area and fraction info
    DO i = Min_dave_vars, Max_dave_vars
      IF (yr_update(Salt_dep2)%cnt > 0) THEN 
         CALL run_ave (yr_report(i,cur_year), yr_update(i)%val, 1 )
      ELSE
         IF (i == Salt_dep2) THEN
            CALL run_ave (yr_report(i,cur_year), yr_update(i)%val, 1 )
         END IF
      END IF
    END DO
    IF (yr_report(Salt_dep2_area,cur_year)%val > 0.0) THEN  !Ever had any deposition during this year
       yr_report(Salt_dep2_rate,cur_year)%val =               &
          yr_report(Salt_dep2_mass,cur_year)%val /            &
          (yr_report(Salt_dep2_area,cur_year)%val * m2_to_ha)
    END IF

    ! If we have salt loss, then we may have some transp cap
    DO i = Min_tave_vars, Max_tave_vars
      IF (yr_update(Salt_loss2)%cnt > 0) THEN 
         CALL run_ave (yr_report(i,cur_year), yr_update(i)%val, 1 )
      END IF
    END DO


    ! reset yrly update vars
    DO i=Min_yrly_vars,Max_yrly_vars
       yrly_update(i)%cnt = 0
       yrly_update(i)%val = 0.0
    END DO
    yrly_dates(rot_y)%ey = yrly_dates(rot_y)%ey + 1

    ! reset yearly rotation update vars
    IF (rot_y == nrot_years) THEN
       DO i=Min_yrly_vars,Max_yrly_vars
          yrot_update(i)%cnt = 0
          yrot_update(i)%val = 0.0
       END DO
!      yrly_dates(0)%ey = yrly_dates(0)%ey + nrot_years 
    END IF

    ! reset yr update vars
    DO i=Min_yrly_vars,Max_yrly_vars
       yr_update(i)%cnt = 0
       yr_update(i)%val = 0.0
    END DO

  END SUBROUTINE update_yrly_report_vars

  SUBROUTINE run_ave(pd_ave, new_val, cnt) 

    USE pd_var_type_def

    IMPLICIT NONE

    TYPE (pd_var_type), INTENT (IN OUT) :: pd_ave
    REAL,    INTENT (IN) :: new_val
    INTEGER, INTENT (IN) :: cnt

    if (cnt == 0) then
      write(0,*) "cnt is: ", cnt
      write(0,*) 'cnt error in running_ave'
      call exit (1)
    endif

    pd_ave%val = pd_ave%val * pd_ave%cnt / (pd_ave%cnt+cnt) +  &
              new_val / (pd_ave%cnt+cnt)

    pd_ave%cnt = pd_ave%cnt + cnt

  END SUBROUTINE run_ave

end module report_update_vars_mod
