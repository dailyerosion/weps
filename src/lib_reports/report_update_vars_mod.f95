!$Author$
!$Date$
!$Revision$
!$HeadURL$

module report_update_vars_mod

   interface update_daily_vars
      module procedure update_daily_h1et
      module procedure update_daily_cellstate
   end interface update_daily_vars

   real, dimension(:,:,:), allocatable :: rep_daily

  contains
  
  subroutine alloc_rep_daily( nsubr, sjulday, ejulday )
    use pd_var_tables, only: n_rep_vars
    
    integer, intent(in) :: nsubr    ! number of subregions
    integer, intent(in) :: sjulday  ! start julian day of simulation
    integer, intent(in) :: ejulday  ! end julian day of simulation
    
    INTEGER :: alloc_status         ! Local allocate status return
    integer :: isr, jday, kvar

    IF (ALLOCATED (rep_daily) .neqv. .TRUE.) then
        ALLOCATE (rep_daily(0:nsubr, sjulday:ejulday, n_rep_vars), STAT = alloc_status) 
        IF (alloc_status /= 0) THEN
           write(0,*) "Error allocating rep_daily(sjulday:ejulday, n_rep_vars)"
           call exit(1)
        END IF
        
        ! zero all values
        do isr = 0, nsubr
          do jday = sjulday, ejulday
            do kvar = 1, n_rep_vars
              rep_daily(isr, jday, kvar) = 0.0
            end do
          end do
        end do
    END IF
  end subroutine alloc_rep_daily

  subroutine sim_area_average( nsubr, sjulday, ejulday )
    use pd_var_tables, only: n_rep_vars, N_eros_events, Salt_loss2_area, Salt_dep2_area, &
                             Trans_cap_area, Sheltered_area
    use grid_mod, only: imax, jmax
    use erosion_data_struct_defs, only: cellstate

    integer, intent(in) :: nsubr    ! number of subregions
    integer, intent(in) :: sjulday  ! start julian day of simulation
    integer, intent(in) :: ejulday  ! end julian day of simulation

    integer :: idx
    integer :: jdx
    integer :: tot_cnt
    integer, dimension(:), allocatable :: frac_cnt
    real, dimension(:), allocatable :: frac_area

    integer :: idy  ! loop variable for days
    integer :: irp  ! loop variable for report vars
    integer :: isr  ! loop variable for subregions

    INTEGER :: alloc_status         ! Local allocate status return

    allocate( frac_cnt(nsubr), stat=alloc_status )
    IF (alloc_status .ne. 0) THEN
       write(0,*) "Error allocating frac_cnt(nsubr)"
       call exit(1)
    END IF

    allocate( frac_area(nsubr), stat=alloc_status )
    IF (alloc_status .ne. 0) THEN
       write(0,*) "Error allocating frac_area(nsubr)"
       call exit(1)
    END IF

    ! To set subregion cell counts
    do isr = 1, nsubr
      frac_cnt(isr) = 0
    end do
    tot_cnt = 0
    do jdx = 1, jmax-1
      do idx = 1, imax-1
         ! count total cells
         ! count cells in each subregion
         if( cellstate(idx,jdx)%csr .ge. 1 ) then
           tot_cnt = tot_cnt + 1
         end if
         if( (cellstate(idx,jdx)%csr .ge. 1) .and. (cellstate(idx,jdx)%csr .le. nsubr) ) then
           frac_cnt(cellstate(idx,jdx)%csr) = frac_cnt(cellstate(idx,jdx)%csr) + 1
         else if( cellstate(idx,jdx)%csr .ne. 0 ) then  ! csr of 0 is a non simulated area
           write(*,*) 'Grid contains invalid subregion index'
           write(*,"(3(a,i0))") 'cellstate(', idx, ',', jdx, ')%csr = ', cellstate(idx,jdx)%csr
           stop
         end if
       end do
    end do
    ! calculate fraction of area for each subregion
    do isr = 1, nsubr
      frac_area(isr) = real(frac_cnt(isr)) / real(tot_cnt)
    end do

    ! area average all report variables or subregion 0
    do idy = sjulday, ejulday
      do irp = 1, n_rep_vars
        rep_daily(0, idy, irp) = 0.0
        do isr = 1, nsubr
          select case (irp)
          case (N_eros_events)
            rep_daily(0, idy, irp) = max( rep_daily(0, idy, irp), rep_daily(isr, idy, irp) )
          case (Salt_loss2_area, &
                Salt_dep2_area, &
                Trans_cap_area, &
                Sheltered_area)
            rep_daily(0, idy, irp) = rep_daily(0, idy, irp) + rep_daily(isr, idy, irp)
          case default
            rep_daily(0, idy, irp) = rep_daily(0, idy, irp) + rep_daily(isr, idy, irp) * frac_area(isr)
          end select
        end do
      end do
    end do

    deallocate( frac_cnt, stat=alloc_status )
    deallocate( frac_area, stat=alloc_status )

  end subroutine sim_area_average

  subroutine update_daily_h1et(isr, julday, soil, restot, croptot, biotot, h1et, h1bal)
    USE pd_var_tables
    use soil_data_struct_defs, only: soil_def
    use biomaterial, only: biototal
    use hydro_data_struct_defs, only: hydro_derived_et
    use report_hydrobal_mod, only: hydro_balance
    use climate_input_mod, only: cli_day
    use process_mod, only: sbsfdi  ! routine to find erodible fraction

    integer, intent(in) :: isr     ! subregion
    integer, intent(in) :: julday  ! julian day of the simulation
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
    type(hydro_derived_et), intent(inout) :: h1et  ! contains:
                                ! ahzpta(isr)     daily transpiration (mm)
                                ! ahzpta(isr)     daily evaporation (mm)
    type(hydro_balance), intent(in) :: h1bal  ! contains:
                                ! presswc(isr)  daily water content in all soil layers (mm)

    REAL :: ef84                ! erodible agg. size fraction below 0.84mm
    REAL, PARAMETER :: snow_depth_thresh = 20.0

    rep_daily(isr, julday, Precipi) = cli_day(julday)%zdpt
    rep_daily(isr, julday, Dryness_ratio) = h1et%drat

    if( h1et%zsnd .gt. snow_depth_thresh ) then
        h1et%snow_protect = 1.0
    else
        h1et%snow_protect = 0.0
    end if
    rep_daily(isr, julday, Snow_cover) = h1et%snow_protect

    rep_daily(isr, julday, Heat_units) = 0.0
    rep_daily(isr, julday, Irrigation) = h1et%zirr

    rep_daily(isr, julday, Crop_Transp) = h1et%zpta
    rep_daily(isr, julday, Evaporation) = h1et%zea
    rep_daily(isr, julday, Runoff) = h1et%zrun
    rep_daily(isr, julday, Drainage) = h1et%zper

    rep_daily(isr, julday, Random_rough) = soil%aslrr
    rep_daily(isr, julday, Ridge_ht) = soil%aszrgh
    rep_daily(isr, julday, Ridge_sp) = soil%asxrgs
    rep_daily(isr, julday, Ridge_dir) = soil%asargo

    rep_daily(isr, julday, Crop_canopy_cov) = croptot%ftcancov
    !Flat crop cover (includes stem area and flat stems+leaves) Added 03/03/2020 - LEW
    rep_daily(isr, julday, Crop_flat_cov) = croptot%ftcvtot
    rep_daily(isr, julday, Crop_stand_sil) = croptot%rcdtot

    !Note currently we report this as "Leaf and Stem Mass" not "Total Standing Mass" or "Total Above Ground Mass"
    !Note that we are also subtracting the "store" portion
    !which contains the reproductive (seed and fruit) components
    ! Remove live standing and flat crop reproductive mass from reported value
    ! Changed name from "Crop_stand_mass" to "Crop_leaf_stem_mass" - 03/03/2020 LEW
    rep_daily(isr, julday, Crop_leaf_stem_mass) = croptot%msttot + croptot%mftot - croptot%mstandstore - croptot%mflatstore
    !Total above ground crop mass - includes all biomass, standing+flat, including storage (reproductive) mass Added 03/03/2020 - LEW
    rep_daily(isr, julday, Crop_total_above_ground_mass) = croptot%msttot + croptot%mftot
    rep_daily(isr, julday, Crop_root_mass) = croptot%mrttot
    rep_daily(isr, julday, Crop_stand_height) = croptot%zht_ave
    rep_daily(isr, julday, Crop_number_stems) = croptot%dstmtot

    ! Residue vars
    rep_daily(isr, julday, Res_flat_cov) = restot%ftcvtot
    rep_daily(isr, julday, Res_stand_sil) = restot%rcdtot
    rep_daily(isr, julday, Res_flat_mass) = restot%mftot
    rep_daily(isr, julday, Res_stand_mass) = restot%msttot

    !Total above ground residue mass - includes all biomass, standing+flat, including storage (reproductive) mass Added 03/04/2020 - LEW
    rep_daily(isr, julday, Res_total_above_ground_mass) = restot%msttot + restot%mftot
    rep_daily(isr, julday, Res_buried_mass) = restot%mbgtot
    rep_daily(isr, julday, Res_root_mass) = restot%mrttot
    rep_daily(isr, julday, Res_stand_height) = restot%zht_ave
    rep_daily(isr, julday, Res_number_stems) = restot%dstmtot

    ! Biomass vars
    rep_daily(isr, julday, All_flat_cov) = biotot%ftcvtot
    rep_daily(isr, julday, All_stand_sil) = biotot%rcdtot
    ! Remove live flat crop reproductive mass from reported value
    rep_daily(isr, julday, All_flat_mass) = biotot%mftot - croptot%mflatstore

    ! Remove live standing crop reproductive mass from reported value
    rep_daily(isr, julday, All_stand_mass) = biotot%msttot - croptot%mstandstore

    !Total above ground (live and dead) mass - includes all biomass, standing+flat, including storage (reproductive) mass Added 03/04/2020 - LEW
    rep_daily(isr, julday, All_total_above_ground_mass) = croptot%msttot + croptot%mftot + restot%msttot + restot%mftot
    rep_daily(isr, julday, All_buried_mass) = croptot%mrttot + restot%mrttot + restot%mbgtot

    call sbsfdi(soil%aslagm(1), soil%as0ags(1), soil%aslagn(1), soil%aslagx(1),0.84,ef84)
    rep_daily(isr, julday, Surface_Ag_84) = ef84
    rep_daily(isr, julday, Surface_Ag_AS) = soil%aseags(1)  !Ag Stability (J/m^2)
    rep_daily(isr, julday, Surface_Ag_DN) = soil%asdagd(1)  !Ag Density (Mg/m^3)
    rep_daily(isr, julday, Surface_Ag_CA) = soil%acanag  !Ag Coeff. of abrasion (1/m)
    rep_daily(isr, julday, Surface_Cr) = soil%asfcr  !Surface Crust fraction
    rep_daily(isr, julday, Surface_Cr_AS) = soil%asecr  !Surface Crust stability (J/m^2)
    rep_daily(isr, julday, Surface_Cr_LM) = soil%asmlos  !Surface Crust loose material (Mg/m^2)
    rep_daily(isr, julday, Surface_Cr_TH) = soil%aszcr  !Surface Crust thickness (mm)
    rep_daily(isr, julday, Surface_Cr_DN) = soil%asdcr  !Surface Crust density (Mg/m^3)
    rep_daily(isr, julday, Surface_Cr_LF) = soil%asflos  !Surface Crust - fraction of loose material (m^2/m^2)
    rep_daily(isr, julday, Surface_Cr_CA) = soil%acancr  !Surface Crust Coeff. of abrasion (1/m)

    ! Soil Water
    rep_daily(isr, julday, Soil_Water) = h1bal%presswc  !Soil Water content in full soil profile (mm)

  end subroutine update_daily_h1et

  subroutine update_daily_cellstate(isr, julday, cellstate)
    USE pd_var_tables
    use erosion_data_struct_defs, only: cellsurfacestate
    use grid_mod, only: imax, jmax, sim_area
    use erosion_data_struct_defs, only: subday, awudmx, awdair, ntstep

    INTEGER, intent(in) :: isr     ! current subregion
    integer, intent(in) :: julday  ! julian day of the simulation
    type(cellsurfacestate), dimension(0:,0:), intent(in) :: cellstate  ! egt, egtcs, egtss, egt10

    REAL :: we                  ! erosive wind energy summed for day

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

    ! Threshold value for determining erosion loss and deposition regions
    REAL, PARAMETER :: eros_thresh = 0.025 !kg/m^2
    ! Threshold value for determining sheltered regions
    REAL, PARAMETER :: susp_thresh = 0.001 !kg/m^2

    ! Threshold value for determining erosive wind energy (m/s)
    REAL, PARAMETER :: wind_energy_thresh = 8.0

    ! Flags to specify whether we have experienced an erosion event or not.
    ! It is set in the "Salt_loss2" section and used in the "Trans_cap"
    ! and "Sheltered" code sections.
    LOGICAL :: Have_Erosion
    LOGICAL :: Have_Deposition

    Have_Erosion    = .FALSE.   ! Initialize for each invocation of routine
    Have_Deposition = .FALSE.   ! Initialize for each invocation of routine

    gdpt_area = sim_area/( (imax-1)*(jmax-1) )   !Area of single grid cell

    !variables running averaged for period
    we = 0.0
    IF (awudmx > wind_energy_thresh) THEN
       DO i = 1, ntstep
          IF (subday(i)%awu > 8.0) THEN
            we = we + 0.5*awdair*(subday(i)%awu**2) * (subday(i)%awu - 8.0) * (86400.0/ntstep) * (0.001) ! (s/day) and (J/kJ)
          END IF
       END DO

    END IF
    rep_daily(isr, julday, Wind_energy) = we

    ! Determine if we have any net soil loss occurring from any grid cell (erosion)
    ! We assume that we don't have any net suspension loss if we don't have any net salt/creep loss
    ngdpt = 0   ! count number of grid points in this subregion (if zero, it is not used)
    sum_salt_loss = 0.0
    cnt_eros = 0
    DO i = 1, imax-1 
       DO j = 1, jmax-1 
          if( isr .eq. cellstate(i,j)%csr ) then
             IF ((cellstate(i,j)%egtcs) < -eros_thresh) THEN
                sum_salt_loss = sum_salt_loss + (cellstate(i,j)%egtcs)
                cnt_eros = cnt_eros + 1
             END IF
             ngdpt = ngdpt + 1
          end if
       END DO
    END DO
    IF (cnt_eros /= 0) Have_Erosion = .TRUE.  !We have erosion occurring, set flag for use later

    ! Determine if we have any net soil deposition occurring from any grid cell (deposition)
    ! We assume that we don't have any net suspension deposition if we don't have any mry salt/creep deposition
    sum_salt_dep = 0.0
    cnt_dep = 0
    DO i = 1, imax-1 
       DO j = 1, jmax-1 
          if( isr .eq. cellstate(i,j)%csr ) then
             IF ((cellstate(i,j)%egtcs) > eros_thresh) THEN
                sum_salt_dep = sum_salt_dep + (cellstate(i,j)%egtcs)
                cnt_dep = cnt_dep + 1
             END IF
          end if
       END DO
    END DO
    IF (cnt_dep /= 0) Have_Deposition = .TRUE.  !We have deposition occurring, set flag for use later

    !variables summed for period
    rep_daily(isr, julday, Eros_loss) = 0.0
    rep_daily(isr, julday, Salt_loss) = 0.0
    rep_daily(isr, julday, Susp_loss) = 0.0
    rep_daily(isr, julday, PM10_loss) = 0.0
    rep_daily(isr, julday, PM2_5_loss) = 0.0
    DO i = 1, imax-1 
       DO j = 1, jmax-1 
          if( isr .eq. cellstate(i,j)%csr ) then
             rep_daily(isr, julday, Eros_loss) = rep_daily(isr, julday, Eros_loss) + cellstate(i,j)%egt/ngdpt
             rep_daily(isr, julday, Salt_loss) = rep_daily(isr, julday, Salt_loss) + (cellstate(i,j)%egtcs)/ngdpt
             rep_daily(isr, julday, Susp_loss) = rep_daily(isr, julday, Susp_loss) + cellstate(i,j)%egtss/ngdpt
             rep_daily(isr, julday, PM10_loss) = rep_daily(isr, julday, PM10_loss) + cellstate(i,j)%egt10/ngdpt
             rep_daily(isr, julday, PM2_5_loss) = rep_daily(isr, julday, PM2_5_loss) + cellstate(i,j)%egt2_5/ngdpt
          end if
       END DO
    END DO

    ! Sum boundary losses  (ave value per boundary grid point)
    rep_daily(isr, julday, Salt_1) = 0.0
    rep_daily(isr, julday, Salt_3) = 0.0
    rep_daily(isr, julday, Susp_1) = 0.0
    rep_daily(isr, julday, Susp_3) = 0.0
    rep_daily(isr, julday, PM10_1) = 0.0
    rep_daily(isr, julday, PM10_3) = 0.0
    rep_daily(isr, julday, PM2_5_1) = 0.0
    rep_daily(isr, julday, PM2_5_3) = 0.0
    DO i = 0, imax 
      rep_daily(isr, julday, Salt_1) = rep_daily(isr, julday, Salt_1) + cellstate(i,0)%egtcs/(imax-1)
      rep_daily(isr, julday, Salt_3) = rep_daily(isr, julday, Salt_3) + cellstate(i,jmax)%egtcs/(imax-1)
      rep_daily(isr, julday, Susp_1) = rep_daily(isr, julday, Susp_1) + cellstate(i,0)%egtss/(imax-1)
      rep_daily(isr, julday, Susp_3) = rep_daily(isr, julday, Susp_3) + cellstate(i,jmax)%egtss/(imax-1)
      rep_daily(isr, julday, PM10_1) = rep_daily(isr, julday, PM10_1) + cellstate(i,0)%egt10/(imax-1)
      rep_daily(isr, julday, PM10_3) = rep_daily(isr, julday, PM10_3) + cellstate(i,jmax)%egt10/(imax-1)
      rep_daily(isr, julday, PM2_5_1) = rep_daily(isr, julday, PM2_5_1) + cellstate(i,0)%egt2_5/(imax-1)
      rep_daily(isr, julday, PM2_5_3) = rep_daily(isr, julday, PM2_5_3) + cellstate(i,jmax)%egt2_5/(imax-1)
    END DO

    rep_daily(isr, julday, Salt_2) = 0.0
    rep_daily(isr, julday, Salt_4) = 0.0
    rep_daily(isr, julday, Susp_2) = 0.0
    rep_daily(isr, julday, Susp_4) = 0.0
    rep_daily(isr, julday, PM10_2) = 0.0
    rep_daily(isr, julday, PM10_4) = 0.0
    rep_daily(isr, julday, PM2_5_2) = 0.0
    rep_daily(isr, julday, PM2_5_4) = 0.0
    DO j = 0, jmax 
      rep_daily(isr, julday, Salt_2) = rep_daily(isr, julday, Salt_2) + cellstate(0,j)%egtcs/(jmax-1)
      rep_daily(isr, julday, Salt_4) = rep_daily(isr, julday, Salt_4) + cellstate(imax,j)%egtcs/(jmax-1)
      rep_daily(isr, julday, Susp_2) = rep_daily(isr, julday, Susp_2) + cellstate(0,j)%egtss/(jmax-1)
      rep_daily(isr, julday, Susp_4) = rep_daily(isr, julday, Susp_4) + cellstate(imax,j)%egtss/(jmax-1)
      rep_daily(isr, julday, PM10_2) = rep_daily(isr, julday, PM10_2) + cellstate(0,j)%egt10/(jmax-1)
      rep_daily(isr, julday, PM10_4) = rep_daily(isr, julday, PM10_4) + cellstate(imax,j)%egt10/(jmax-1)
      rep_daily(isr, julday, PM2_5_2) = rep_daily(isr, julday, PM2_5_2) + cellstate(0,j)%egt2_5/(jmax-1)
      rep_daily(isr, julday, PM2_5_4) = rep_daily(isr, julday, PM2_5_4) + cellstate(imax,j)%egt2_5/(jmax-1)
    END DO

    IF (Have_Erosion) THEN !We have erosion somewhere
       rep_daily(isr, julday, N_eros_events) = 1.0  ! erosion event
       rep_daily(isr, julday, Salt_loss2) = sum_salt_loss/ngdpt
       rep_daily(isr, julday, Salt_loss2_mass) = sum_salt_loss*gdpt_area
       rep_daily(isr, julday, Salt_loss2_area) = REAL(cnt_eros)*gdpt_area/m2_to_ha
       rep_daily(isr, julday, Salt_loss2_frac) = REAL(cnt_eros)/ngdpt
       rep_daily(isr, julday, Salt_loss2_rate) = rep_daily(isr, julday, Salt_loss2_mass) / &
                                                 (rep_daily(isr, julday, Salt_loss2_area) * m2_to_ha)
    END IF

    IF (Have_Deposition) THEN  ! We have deposition somewhere
       rep_daily(isr, julday, Salt_dep2) = sum_salt_dep/ngdpt
       rep_daily(isr, julday, Salt_dep2_mass) = sum_salt_dep*gdpt_area
       rep_daily(isr, julday, Salt_dep2_area) = REAL(cnt_dep)*gdpt_area/m2_to_ha
       rep_daily(isr, julday, Salt_dep2_frac) = REAL(cnt_dep)/ngdpt
       rep_daily(isr, julday, Salt_dep2_rate) = rep_daily(isr, julday, Salt_dep2_mass) / &
                                               (rep_daily(isr, julday, Salt_dep2_area) * m2_to_ha)
    END IF
    rep_daily(isr, julday, Trans_cap) = 0.0 ! not used yet

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
            if( isr .eq. cellstate(i,j)%csr ) then
               IF ( ABS((cellstate(i,j)%egtcs)) <= eros_thresh) THEN  !Sheltered/TC
                  IF ( ABS(cellstate(i,j)%egtss) <= susp_thresh) THEN  ! Sheltered area
                     cnt_sheltered = cnt_sheltered + 1
                  ELSE                                       ! At TC
                     cnt_transp = cnt_transp + 1
                  END IF
               END IF
            end if
         END DO
      END DO

      ! Note: Don't currently have a way of computing Flux Rate over a grid area
      rep_daily(isr, julday, Trans_cap_area) = REAL(cnt_transp)*gdpt_area/m2_to_ha
      rep_daily(isr, julday, Trans_cap_frac) = REAL(cnt_transp)/ngdpt
      !Sheltered region is now computed.
      rep_daily(isr, julday, Sheltered_area) = REAL(cnt_sheltered)*gdpt_area/m2_to_ha
      rep_daily(isr, julday, Sheltered_frac) = REAL(cnt_sheltered)/ngdpt
    END IF  !Have_Erosion flag

  end subroutine update_daily_cellstate

  SUBROUTINE update_period_update_vars(isr, julday, period_update )

    USE pd_var_tables, only: Min_period_vars
    USE pd_var_tables, only: Min_eop_vars, Max_eop_vars
    USE pd_var_tables, only: Min_eave_vars, Max_eave_vars
    USE pd_var_tables, only: Salt_loss2, Salt_loss2_mass, Salt_loss2_area, Salt_loss2_frac, Salt_loss2_rate
    USE pd_var_tables, only: Salt_dep2, Salt_dep2_mass, Salt_dep2_area, Salt_dep2_frac, Salt_dep2_rate
    USE pd_var_tables, only: Min_tave_vars, Max_tave_vars
    USE pd_var_type_def, only: pd_var_type

    IMPLICIT NONE

    ! + + + ARGUMENT DECLARATIONS + + +
    INTEGER :: isr                 ! current subregion
    integer, intent(in) :: julday  ! julian day of the simulation
    TYPE (pd_var_type), DIMENSION(Min_period_vars:), intent(inout) :: period_update

    INTEGER :: i              ! local loop variable

    ! End of period (eop) variables
    do i = Min_eop_vars, Max_eop_vars
      period_update(i)%val = rep_daily( isr, julday, i )
      period_update(i)%cnt = period_update(i)%cnt + 1
    end do

    ! variables summed for period
    do i = Min_eave_vars, Max_eave_vars
      period_update(i)%val = period_update(i)%val + rep_daily( isr, julday, i )
      period_update(i)%cnt = period_update(i)%cnt + 1
    end do

    ! Min_lave_vars, Max_lave_vars
    if( rep_daily( isr, julday, Salt_loss2 ) .lt. 0.0 ) then
      period_update(Salt_loss2)%val = period_update(Salt_loss2)%val + rep_daily( isr, julday, Salt_loss2 )
      period_update(Salt_loss2)%cnt = period_update(Salt_loss2)%cnt + 1

      period_update(Salt_loss2_mass)%val = period_update(Salt_loss2_mass)%val + rep_daily( isr, julday, Salt_loss2_mass )
      period_update(Salt_loss2_mass)%cnt = period_update(Salt_loss2_mass)%cnt + 1

      CALL run_ave (period_update(Salt_loss2_area), rep_daily( isr, julday, Salt_loss2_area ), 1)
      CALL run_ave (period_update(Salt_loss2_frac), rep_daily( isr, julday, Salt_loss2_frac ), 1)
    end if

    ! this is not actually used in the report
    ! compute as: Salt_loss2_mass/(Salt_loss2_area * m2_to_ha)
    period_update(Salt_loss2_rate)%val = rep_daily( isr, julday, Salt_loss2_rate )
    period_update(Salt_loss2_rate)%cnt = period_update(Salt_loss2_rate)%cnt + 1

    ! Min_dave_vars, Max_dave_vars
    if( rep_daily( isr, julday, Salt_dep2 ) .gt. 0.0 ) then
      period_update(Salt_dep2)%val = period_update(Salt_dep2)%val + rep_daily( isr, julday, Salt_dep2 )
      period_update(Salt_dep2)%cnt = period_update(Salt_dep2)%cnt + 1

      period_update(Salt_dep2_mass)%val = period_update(Salt_dep2_mass)%val + rep_daily( isr, julday, Salt_dep2_mass )
      period_update(Salt_dep2_mass)%cnt = period_update(Salt_dep2_mass)%cnt + 1

      CALL run_ave (period_update(Salt_dep2_area), rep_daily( isr, julday, Salt_dep2_area ), 1)
      CALL run_ave (period_update(Salt_dep2_frac), rep_daily( isr, julday, Salt_dep2_frac ), 1)
    end if

    ! this is not actually used in the report
    !compute as: Salt_dep2_mass/(Salt_dep2_area * m2_to_ha)
    period_update(Salt_dep2_rate)%val = rep_daily( isr, julday, Salt_dep2_rate )
    period_update(Salt_dep2_rate)%cnt = period_update(Salt_dep2_rate)%cnt + 1

    if( rep_daily( isr, julday, Salt_loss2 ) .lt. 0.0 ) then
      do i = Min_tave_vars, Max_tave_vars
        CALL run_ave (period_update(i), rep_daily( isr, julday, i ), 1)
      end do
    end if

  END SUBROUTINE update_period_update_vars

  SUBROUTINE update_period_report_vars(pd, npd, cur_yr, nrot_years, period_update, period_report, period_dates)

    USE pd_dates_vars, only: pd_dates_type
    USE pd_var_type_def, only: pd_var_type
    USE pd_var_tables, only: Min_period_vars, Max_period_vars
    USE pd_var_tables, only: Min_eave_vars, Max_eave_vars
    USE pd_var_tables, only: Min_eop_vars, Max_eop_vars
    USE pd_var_tables, only: Min_lave_vars, Max_lave_vars
    USE pd_var_tables, only: Min_dave_vars, Max_dave_vars
    USE pd_var_tables, only: Salt_loss2, Salt_loss2_mass, Salt_loss2_area, Salt_loss2_rate
    USE pd_var_tables, only: Salt_dep2, Salt_dep2_mass, Salt_dep2_area, Salt_dep2_rate
    USE pd_var_tables, only: Min_tave_vars, Max_tave_vars

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

  SUBROUTINE update_hmonth_update_vars(isr, julday, cd, cm, hmonth_update, hmrot_update)

    USE pd_var_type_def, only: pd_var_type
    USE pd_var_tables, only: Min_hmonth_vars
    USE pd_var_tables, only: Precipi, Irrigation, Wind_energy, Dryness_ratio, Snow_cover

    IMPLICIT NONE

    INTEGER, intent(in) :: isr ! current subregion
    integer, intent(in) :: julday  ! julian day of the simulation
    INTEGER, INTENT(IN) :: cd  ! current day
    INTEGER, INTENT(IN) :: cm  ! current month
    TYPE (pd_var_type), DIMENSION(Min_hmonth_vars:), intent(inout) :: hmonth_update
    TYPE (pd_var_type), DIMENSION(Min_hmonth_vars:,:), intent(inout) :: hmrot_update

    INTEGER :: hm               ! current hmonth period

    hm = (2 * cm) - 1           !1st half of month
    IF (cd > 14) THEN           !2nd half of month
      hm = hm + 1
    END IF

    !variables summed for period
    hmonth_update(Precipi)%val = hmonth_update(Precipi)%val + rep_daily( isr, julday, Precipi )
    hmonth_update(Precipi)%cnt = hmonth_update(Precipi)%cnt + 1
    hmonth_update(Irrigation)%val = hmonth_update(Irrigation)%val + rep_daily( isr, julday, Irrigation )
    hmonth_update(Irrigation)%cnt = hmonth_update(Irrigation)%cnt + 1

    !variables running averaged 
    CALL run_ave(hmonth_update(Wind_energy), rep_daily( isr, julday, Wind_energy ), 1)
    CALL run_ave(hmrot_update(Wind_energy,hm), rep_daily( isr, julday, Wind_energy ), 1)

    CALL run_ave(hmonth_update(Dryness_ratio), rep_daily( isr, julday, Dryness_ratio ), 1)
    CALL run_ave(hmrot_update(Dryness_ratio,hm), rep_daily( isr, julday, Dryness_ratio ), 1)

    CALL run_ave(hmonth_update(Snow_cover), rep_daily( isr, julday, Snow_cover ), 1)
    CALL run_ave(hmrot_update(Snow_cover,hm), rep_daily( isr, julday, Snow_cover ), 1)

  END SUBROUTINE update_hmonth_update_vars

  SUBROUTINE update_hmonth_report_vars(cur_day, cur_month, cur_yr, nrot_years, hmonth_update, hmrot_update, hmonth_report)

    USE pd_var_type_def, only: pd_var_type
    USE pd_var_tables, only: Min_hmonth_vars, Max_hmonth_vars, Min_cli_vars, Max_cli_vars

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

    DO i = Min_cli_vars, Max_cli_vars
       CALL run_ave (hmonth_report(i,hm,rot_y), hmonth_update(i)%val, 1)
    END DO

    ! update the full rotation average variables
    IF (rot_y == nrot_years) THEN
      DO i=Min_cli_vars, Max_cli_vars
         CALL run_ave (hmonth_report(i,hm,0), hmonth_update(i)%val, nrot_years)
      END DO
    END IF


    ! reset hmonth update vars
    DO i = Min_hmonth_vars, Max_hmonth_vars
       hmonth_update(i)%cnt = 0
       hmonth_update(i)%val = 0.0
    END DO

    ! reset hmonth rotation update vars
    IF (rot_y == nrot_years) THEN
       DO i = Min_hmonth_vars, Max_hmonth_vars
          hmrot_update(i,hm)%cnt = 0
          hmrot_update(i,hm)%val = 0.0
       END DO
    END IF

  END SUBROUTINE update_hmonth_report_vars

  SUBROUTINE update_monthly_update_vars(isr, julday, cm, monthly_update, mrot_update)

    USE pd_var_type_def, only: pd_var_type
    USE pd_var_tables, only: Min_monthly_vars, Max_monthly_vars
    USE pd_var_tables, only: Wind_energy, Dryness_ratio, Snow_cover
    USE pd_var_tables, only: Salt_loss2, Salt_loss2_mass, Salt_loss2_area, Salt_loss2_frac
    USE pd_var_tables, only: Salt_dep2, Salt_dep2_mass, Salt_dep2_area, Salt_dep2_frac
    USE pd_var_tables, only: Trans_cap_area, Trans_cap_frac
    USE pd_var_tables, only: Sheltered_area, Sheltered_frac

    IMPLICIT NONE

    INTEGER, intent (in) :: isr  ! current subregion
    integer, intent(in) :: julday  ! julian day of the simulation
    INTEGER, INTENT (IN) :: cm  ! current month
    TYPE (pd_var_type), DIMENSION(Min_monthly_vars:), intent(inout) :: monthly_update
    TYPE (pd_var_type), DIMENSION(Min_monthly_vars:,:), intent(inout) :: mrot_update

    INTEGER :: i              ! local loop variable

    do i = Min_monthly_vars, Max_monthly_vars
      if(    (i .eq. Wind_energy) .or. (i .eq. Dryness_ratio) .or. (i .eq. Snow_cover) ) then
        
        CALL run_ave(monthly_update(i), rep_daily( isr, julday, i ), 1)
        CALL run_ave(mrot_update(i,cm), rep_daily( isr, julday, i ), 1)

      else if( (i .eq. Salt_loss2) .or. (i .eq. Salt_loss2_mass) ) then

        if( rep_daily( isr, julday, Salt_loss2 ) .lt. 0.0 ) then
          monthly_update(i)%val = monthly_update(i)%val + rep_daily( isr, julday, i )
          monthly_update(i)%cnt = monthly_update(i)%cnt + 1
          mrot_update(i,cm)%val = mrot_update(i,cm)%val + rep_daily( isr, julday, i )
          mrot_update(i,cm)%cnt = mrot_update(i,cm)%cnt + 1
        end if

      else if( (i .eq. Salt_loss2_area) .or. (i .eq. Salt_loss2_frac) ) then

        if( rep_daily( isr, julday, Salt_loss2 ) .lt. 0.0 ) then
          CALL run_ave(monthly_update(i), rep_daily( isr, julday, i ), 1)
          CALL run_ave(mrot_update(i,cm), rep_daily( isr, julday, i ), 1)
        end if

      else if( (i .eq. Salt_dep2) .or. (i .eq. Salt_dep2_mass) ) then
 
        if( rep_daily( isr, julday, Salt_dep2 ) .gt. 0.0 ) then
          monthly_update(i)%val = monthly_update(i)%val + rep_daily( isr, julday, i )
          monthly_update(i)%cnt = monthly_update(i)%cnt + 1
          mrot_update(i,cm)%val = mrot_update(i,cm)%val + rep_daily( isr, julday, i )
          mrot_update(i,cm)%cnt = mrot_update(i,cm)%cnt + 1
        end if

      else if( (i .eq. Salt_dep2_area) .or. (i .eq. Salt_dep2_frac) ) then

        if( rep_daily( isr, julday, Salt_dep2 ) .gt. 0.0 ) then
          CALL run_ave(monthly_update(i), rep_daily( isr, julday, i ), 1)
          CALL run_ave(mrot_update(i,cm), rep_daily( isr, julday, i ), 1)
        end if

      else if( (i .eq. Trans_cap_area) .or. (i .eq. Trans_cap_frac) &
          .or. (i .eq. Sheltered_area) .or. (i .eq. Sheltered_frac) ) then

        if( rep_daily( isr, julday, Salt_loss2 ) .lt. 0.0 ) then
          CALL run_ave(monthly_update(i), rep_daily( isr, julday, i ), 1)
          CALL run_ave(mrot_update(i,cm), rep_daily( isr, julday, i ), 1)
        end if

      else

        !variables summed for period
        monthly_update(i)%val = monthly_update(i)%val + rep_daily( isr, julday, i )
        monthly_update(i)%cnt = monthly_update(i)%cnt + 1
        mrot_update(i,cm)%val = mrot_update(i,cm)%val + rep_daily( isr, julday, i )
        mrot_update(i,cm)%cnt = mrot_update(i,cm)%cnt + 1

      end if
    end do

  END SUBROUTINE update_monthly_update_vars

  ! Update both monthly and rot_month reporting variables
  SUBROUTINE update_monthly_report_vars(cur_month, cur_year, nrot_years, monthly_update, mrot_update, monthly_report, monthly_dates)

    USE pd_dates_vars, only: pd_dates_type
    USE pd_var_type_def, only: pd_var_type
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
       IF (mrot_update(Salt_loss2_area,cur_month)%val > 0) THEN 
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

  SUBROUTINE update_yrly_update_vars(isr, julday, yrly_update, yrot_update, yr_update)

    USE pd_var_type_def, only: pd_var_type
    USE pd_var_tables, only: Min_yrly_vars, Max_yrly_vars
    USE pd_var_tables, only: Wind_energy, Dryness_ratio, Snow_cover
    USE pd_var_tables, only: Salt_loss2, Salt_loss2_mass, Salt_loss2_area, Salt_loss2_frac
    USE pd_var_tables, only: Salt_dep2, Salt_dep2_mass, Salt_dep2_area, Salt_dep2_frac
    USE pd_var_tables, only: Trans_cap_area, Trans_cap_frac
    USE pd_var_tables, only: Sheltered_area, Sheltered_frac

    IMPLICIT NONE

    INTEGER, intent(in) :: isr  ! current subregion
    integer, intent(in) :: julday  ! julian day of the simulation
    TYPE (pd_var_type), DIMENSION(Min_yrly_vars:), intent(inout) :: yrly_update
    TYPE (pd_var_type), DIMENSION(Min_yrly_vars:), intent(inout) :: yrot_update
    TYPE (pd_var_type), DIMENSION(Min_yrly_vars:), intent(inout) :: yr_update

    INTEGER :: i              ! local loop variables

    do i = Min_yrly_vars, Max_yrly_vars
      if(    (i .eq. Wind_energy) .or. (i .eq. Dryness_ratio) .or. (i .eq. Snow_cover) ) then

        CALL run_ave(yrly_update(i), rep_daily( isr, julday, i ), 1)
        CALL run_ave(yrot_update(i), rep_daily( isr, julday, i ), 1)
        CALL run_ave(yr_update(i), rep_daily( isr, julday, i ), 1)

      else if( (i .eq. Salt_loss2) .or. (i .eq. Salt_loss2_mass) ) then

        if( rep_daily( isr, julday, Salt_loss2 ) .lt. 0.0 ) then
          yrly_update(i)%val = yrly_update(i)%val + rep_daily( isr, julday, i )
          yrly_update(i)%cnt = yrly_update(i)%cnt + 1
          yrot_update(i)%val = yrot_update(i)%val + rep_daily( isr, julday, i )
          yrot_update(i)%cnt = yrot_update(i)%cnt + 1
          yr_update(i)%val = yr_update(i)%val + rep_daily( isr, julday, i )
          yr_update(i)%cnt = yr_update(i)%cnt + 1
        end if

      else if( (i .eq. Salt_loss2_area) .or. (i .eq. Salt_loss2_frac) ) then

        if( rep_daily( isr, julday, Salt_loss2 ) .lt. 0.0 ) then
          CALL run_ave(yrly_update(i), rep_daily( isr, julday, i ), 1)
          CALL run_ave(yrot_update(i), rep_daily( isr, julday, i ), 1)
          CALL run_ave(yr_update(i), rep_daily( isr, julday, i ), 1)
        end if

      else if( (i .eq. Salt_dep2) .or. (i .eq. Salt_dep2_mass) ) then
 
        if( rep_daily( isr, julday, Salt_dep2 ) .gt. 0.0 ) then
          yrly_update(i)%val = yrly_update(i)%val + rep_daily( isr, julday, i )
          yrly_update(i)%cnt = yrly_update(i)%cnt + 1
          yrot_update(i)%val = yrot_update(i)%val + rep_daily( isr, julday, i )
          yrot_update(i)%cnt = yrot_update(i)%cnt + 1
          yr_update(i)%val = yr_update(i)%val + rep_daily( isr, julday, i )
          yr_update(i)%cnt = yr_update(i)%cnt + 1
        end if

      else if( (i .eq. Salt_dep2_area) .or. (i .eq. Salt_dep2_frac) ) then

        if( rep_daily( isr, julday, Salt_dep2 ) .gt. 0.0 ) then
          CALL run_ave(yrly_update(i), rep_daily( isr, julday, i ), 1)
          CALL run_ave(yrot_update(i), rep_daily( isr, julday, i ), 1)
          CALL run_ave(yr_update(i), rep_daily( isr, julday, i ), 1)
        end if

      else if( (i .eq. Trans_cap_area) .or. (i .eq. Trans_cap_frac) &
          .or. (i .eq. Sheltered_area) .or. (i .eq. Sheltered_frac) ) then

        if( rep_daily( isr, julday, Salt_loss2 ) .lt. 0.0 ) then
          CALL run_ave(yrly_update(i), rep_daily( isr, julday, i ), 1)
          CALL run_ave(yrot_update(i), rep_daily( isr, julday, i ), 1)
          CALL run_ave(yr_update(i), rep_daily( isr, julday, i ), 1)
        end if

      else

        !variables summed for period
        yrly_update(i)%val = yrly_update(i)%val + rep_daily( isr, julday, i )
        yrly_update(i)%cnt = yrly_update(i)%cnt + 1
        yrot_update(i)%val = yrot_update(i)%val + rep_daily( isr, julday, i )
        yrot_update(i)%cnt = yrot_update(i)%cnt + 1
        ! For a year by year report of yearly (and rotation year) averaged variables
        yr_update(i)%val = yr_update(i)%val + rep_daily( isr, julday, i )
        yr_update(i)%cnt = yr_update(i)%cnt + 1

      end if
    end do

  END SUBROUTINE update_yrly_update_vars

  SUBROUTINE update_yrly_report_vars(start_year, cur_year, nrot_years, &
                  yrly_update, yrot_update, yr_update, yrly_report, yr_report, yrly_dates)

    USE pd_dates_vars, only: pd_dates_type
    USE pd_var_type_def, only: pd_var_type
    USE pd_var_tables

    IMPLICIT NONE

    INTEGER, INTENT (IN) :: start_year
    INTEGER, INTENT (IN) :: cur_year
    INTEGER, INTENT (IN) :: nrot_years
    TYPE (pd_var_type), DIMENSION(Min_yrly_vars:), intent(inout) :: yrly_update
    TYPE (pd_var_type), DIMENSION(Min_yrly_vars:), intent(inout) :: yrot_update
    TYPE (pd_var_type), DIMENSION(Min_yrly_vars:), intent(inout) :: yr_update
    TYPE (pd_var_type), DIMENSION(Min_yrly_vars:,0:), intent(inout) :: yrly_report
    TYPE (pd_var_type), DIMENSION(Min_yrly_vars:,:), intent(inout) :: yr_report
    TYPE (pd_dates_type), DIMENSION(:), intent(inout) :: yrly_dates

    INTEGER :: i        ! local loop variables
    INTEGER :: rot_y    ! local variables
    INTEGER :: rep_yr 

    REAL, PARAMETER :: m2_to_ha = 10000.0  ! m^2 in a ha

    rot_y = mod((cur_year-start_year), nrot_years)+1
    rep_yr = cur_year - start_year + 1

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
       CALL run_ave (yr_report(i,rep_yr), yr_update(i)%val, 1)
    END DO
    DO i = Min_eave_vars, Max_eave_vars
       CALL run_ave (yr_report(i,rep_yr), yr_update(i)%val, 1)
    END DO

    ! If we have saltating loss, add the area and fraction info
    ! else only "running ave" the Salt_loss2 rate
    DO i = Min_lave_vars, Max_lave_vars
      IF (yr_update(Salt_loss2)%cnt > 0) THEN 
            CALL run_ave (yr_report(i,rep_yr), yr_update(i)%val, 1)
      ELSE !Don't change the area and fract values
         IF (i == Salt_loss2) THEN
            CALL run_ave (yr_report(i,rep_yr), yr_update(i)%val, 1)
         END IF
      END IF
    END DO
    IF (yr_report(Salt_loss2_area,rep_yr)%val > 0.0) THEN
       yr_report(Salt_loss2_rate,rep_yr)%val =              &
          yr_report(Salt_loss2_mass,rep_yr)%val /           &
          (yr_report(Salt_loss2_area,rep_yr)%val * m2_to_ha)
       yr_report(Salt_loss2_rate,rep_yr)%cnt = yr_report(Salt_loss2_rate,rep_yr)%cnt + 1;
    END IF

    ! If we have deposition, add the area and fraction info
    DO i = Min_dave_vars, Max_dave_vars
      IF (yr_update(Salt_dep2)%cnt > 0) THEN 
         CALL run_ave (yr_report(i,rep_yr), yr_update(i)%val, 1 )
      ELSE
         IF (i == Salt_dep2) THEN
            CALL run_ave (yr_report(i,rep_yr), yr_update(i)%val, 1 )
         END IF
      END IF
    END DO
    IF (yr_report(Salt_dep2_area,rep_yr)%val > 0.0) THEN  !Ever had any deposition during this year
       yr_report(Salt_dep2_rate,rep_yr)%val =               &
          yr_report(Salt_dep2_mass,rep_yr)%val /            &
          (yr_report(Salt_dep2_area,rep_yr)%val * m2_to_ha)
    END IF

    ! If we have salt loss, then we may have some transp cap
    DO i = Min_tave_vars, Max_tave_vars
      IF (yr_update(Salt_loss2)%cnt > 0) THEN 
         CALL run_ave (yr_report(i,rep_yr), yr_update(i)%val, 1 )
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

    USE pd_var_type_def, only: pd_var_type

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
