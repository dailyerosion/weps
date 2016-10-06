!$Author$
!$Date$
!$Revision$
!$HeadURL$

SUBROUTINE update_period_update_vars(isr, period_update, restot, croptot, biotot, cellstate, h1et, subrsurf)

    use weps_interface_defs, ignore_me=>update_period_update_vars
    USE pd_var_tables
    USE pd_var_type_def
    use biomaterial, only: biototal
    use erosion_data_struct_defs, only: cellsurfacestate
    use hydro_data_struct_defs, only: hydro_derived_et
    use grid_mod, only: imax, jmax, sim_area
    use process_mod, only: sbsfdi
    use erosion_data_struct_defs, only: subregionsurfacestate

    IMPLICIT NONE

!   + + + ARGUMENT DECLARATIONS + + +
    INTEGER :: isr              ! current subregion
    TYPE (pd_var_type), DIMENSION(Min_period_vars:), intent(inout) :: period_update
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
    type(subregionsurfacestate), intent(in) :: subrsurf  ! subregion surface conditions
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



    include "p1werm.inc"        ! needed by other include files

    include "s1agg.inc"         ! aslagm, as0ags, aslagn, aslagx (ASD parms)
                                ! aseags (agg stability), asdagd (agg density)
    include "h1balance.inc"     ! pressswc(isr)  daily surface water content in all soil layers (mm)

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

  if( isr .gt. 0 ) then

! Roughness vars
    period_update(Random_rough)%val = subrsurf%aslrr
    period_update(Random_rough)%cnt = period_update(Random_rough)%cnt + 1

    period_update(Ridge_ht)%val = subrsurf%aszrgh
    period_update(Ridge_ht)%cnt = period_update(Ridge_ht)%cnt + 1

    period_update(Ridge_sp)%val = subrsurf%asxrgs
    period_update(Ridge_sp)%cnt = period_update(Ridge_sp)%cnt + 1

    period_update(Ridge_dir)%val = subrsurf%asargo
    period_update(Ridge_dir)%cnt = period_update(Ridge_dir)%cnt + 1

    call sbsfdi(aslagm(1,isr), as0ags(1,isr), aslagn(1,isr),                &
         aslagx(1,isr),0.84,ef84)
    period_update(Surface_Ag_84)%val = ef84
    period_update(Surface_Ag_84)%cnt = period_update(Surface_Ag_84)%cnt + 1

    period_update(Surface_Ag_AS)%val = aseags(1,isr)  !Ag Stability (J/m^2)
    period_update(Surface_Ag_AS)%cnt = period_update(Surface_Ag_AS)%cnt + 1

    period_update(Surface_Ag_DN)%val = asdagd(1,isr)  !Ag Density (Mg/m^3)
    period_update(Surface_Ag_DN)%cnt = period_update(Surface_Ag_DN)%cnt + 1

    period_update(Surface_Ag_CA)%val = subrsurf%acanag  !Ag Coeff. of abrasion (1/m)
    period_update(Surface_Ag_CA)%cnt = period_update(Surface_Ag_CA)%cnt + 1

    period_update(Surface_Cr)%val = subrsurf%asfcr  !Surface Crust fraction
    period_update(Surface_Cr)%cnt = period_update(Surface_Cr)%cnt + 1

    period_update(Surface_Cr_AS)%val = subrsurf%asecr  !Surface Crust stability (J/m^2)
    period_update(Surface_Cr_AS)%cnt = period_update(Surface_Cr_AS)%cnt + 1

    period_update(Surface_Cr_LM)%val = subrsurf%asmlos  !Surface Crust loose material (Mg/m^2)
    period_update(Surface_Cr_LM)%cnt = period_update(Surface_Cr_LM)%cnt + 1

    period_update(Surface_Cr_TH)%val = subrsurf%aszcr  !Surface Crust thickness (mm)
    period_update(Surface_Cr_TH)%cnt = period_update(Surface_Cr_TH)%cnt + 1

    period_update(Surface_Cr_DN)%val = subrsurf%asdcr  !Surface Crust density (Mg/m^3)
    period_update(Surface_Cr_DN)%cnt = period_update(Surface_Cr_DN)%cnt + 1

    period_update(Surface_Cr_LF)%val = subrsurf%asflos  !Surface Crust - fraction of loose material (m^2/m^2)
    period_update(Surface_Cr_LF)%cnt = period_update(Surface_Cr_LF)%cnt + 1

    period_update(Surface_Cr_CA)%val = subrsurf%acancr  !Surface Crust Coeff. of abrasion (1/m)
    period_update(Surface_Cr_CA)%cnt = period_update(Surface_Cr_CA)%cnt + 1

! Soil Water
    period_update(Soil_Water)%val = presswc(isr)  !Soil Water content in full soil profile (mm)
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

  end if
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

  if( isr .gt. 0 ) then

  ! Remove live flat crop reproductive mass from reported value
    period_update(All_flat_mass)%val = biotot%mftot - croptot%mflatstore
    period_update(All_flat_mass)%cnt = period_update(All_flat_mass)%cnt + 1

    ! Remove live standing crop reproductive mass from reported value
    period_update(All_stand_mass)%val = biotot%msttot - croptot%mstandstore
    period_update(All_stand_mass)%cnt = period_update(All_stand_mass)%cnt + 1

    period_update(All_buried_mass)%val = croptot%mrttot + restot%mrttot + restot%mbgtot
    period_update(All_buried_mass)%cnt = period_update(All_buried_mass)%cnt + 1

  end if
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
