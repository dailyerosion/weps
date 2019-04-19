!$Author$
!$Date$
!$Revision$
!$HeadURL$

module print_ui1_output_mod

  contains

! This subroutine generates the output file used by the Java 2 interface.
! It includes a new initial column that defines the type of data on each
! output line.  The key definitions are:

!     # - comment line (interface should ignore these lines)
!     P - period or "detail" lines
!     H - rotation half month lines - (currently not generated)
!     M - rotation month lines - (currently not generated)
!     Y - rotation year total lines
!     T - rotation total line

!     m - month lines
!     h - half month lines
!     y - individual year lines
!     t - total line - (same as rotation total line)

! Note that all key definitions in CAPS refer to "rotation year" info.
! All non-CAP key definitions refer to calendar year info
! (rotation years are combined, eg. rotation year 1 and 2 monthly info combined)

  SUBROUTINE print_ui1_output(luogui1, nperiods, nrot_years, ncycles, rep_report, rep_dates, mandate)

    USE pd_dates_vars
    USE pd_report_vars

    USE pd_var_tables
    use mandate_mod, only: opercrop_date
    use input_run_mod, only: old_run_file

    IMPLICIT NONE

    integer, intent(in) :: luogui1         ! output file unit number
    INTEGER, INTENT (IN) :: nperiods
    INTEGER, INTENT (IN) :: nrot_years
    INTEGER, INTENT (IN) :: ncycles
    type(reporting_report), intent(in) :: rep_report
    type(reporting_dates), intent(in) :: rep_dates
    type (opercrop_date), dimension(:), intent(in) :: mandate

    INTEGER :: i,hm,m,y            ! local loop variables
    INTEGER :: p,x                 ! local loop variables

    LOGICAL :: match = .false.
    INTEGER :: match_no = 0
    CHARACTER(len=256) :: opname, cropname

    ! Print out the header for ui1 output report file
    write (UNIT=luogui1,FMT="(1(A))",ADVANCE="NO")                      &
              'key|'
    write (UNIT=luogui1,FMT="(1(A))",ADVANCE="NO")                      &
              'sd ed mo yr|'
    write (UNIT=luogui1,FMT="(1x,A,A115,'|')",ADVANCE="NO") 'operation ',""
    write (UNIT=luogui1,FMT="(1x,A,A115,'|')",ADVANCE="NO") 'crop      ',""
!    write (UNIT=luogui1,FMT="(2(A))",ADVANCE="NO")                      &
!         ' operation'                                             |',   &
!         ' crop                                                   |'
    write (UNIT=luogui1,FMT="(1(A))",ADVANCE="NO") 'ave_no_evnt|'
    write (UNIT=luogui1,FMT="(1(A))",ADVANCE="NO") 'tot_no_evnt|'
    write (UNIT=luogui1,FMT="(1(A))",ADVANCE="NO") 'gloss_per_evnt|'
    write (UNIT=luogui1,FMT="(1(A))",ADVANCE="NO") 'nloss_per_evnt|'
    write (UNIT=luogui1,FMT="(1(A))",ADVANCE="NO") 'gross_loss|'

    if( old_run_file ) then
       write (UNIT=luogui1,FMT="(16(A))",ADVANCE="NO")                     &
              '  tot_loss|','  crp+salt|','    suspen|','      pm10|',  &
              '       cs1|','       cs2|','       cs3|','       cs4|',  &
              '       ss1|','       ss2|','       ss3|','       ss4|',  &
              '       pm1|','       pm2|','       pm3|','       pm4|'
    else
       write (UNIT=luogui1,FMT="(21(A))",ADVANCE="NO")                     &
              '  tot_loss|','  crp+salt|','    suspen|','      pm10|','     pm2.5|',  &
              '       cs1|','       cs2|','       cs3|','       cs4|',  &
              '       ss1|','       ss2|','       ss3|','       ss4|',  &
              '    pm10_1|','    pm10_2|','    pm10_3|','    pm10_4|',  &
              '   pm2.5_1|','   pm2.5_2|','   pm2.5_3|','   pm2.5_4|'
    end if

    write (UNIT=luogui1,FMT="(11(A))",ADVANCE="NO")                     &
              ' salt_loss|',' loss_area|',' loss_frac|',                &
              '  salt_dep|','  dep_area|','  dep_frac|',                &
              ' flux_rate|',' flux_area|',' flux_frac|',                &
                            'shelt_area|','shelt_frac|'
   write (UNIT=luogui1,FMT="(4(A))",ADVANCE="NO")                      &
              '    precip|','  w_energy|','snow_cover|','     irrig|'
!              '    precip|','  w_energy|','snow_cover|'
!              '    precip|','  w_energy|','snow_cover|','   dry_idx|'
    write (UNIT=luogui1,FMT="(6(A))",ADVANCE="NO")                      &
              ' l_can_cov|','l_sil_area|',' l_st_mass|',' l_rt_mass|',  &
              '   l_st_ht|',' l_no_stem|'
    write (UNIT=luogui1,FMT="(8(A))",ADVANCE="NO")                      &
              '  d_fl_cov|','  d_st_sil|',' d_fl_mass|',' d_st_mass|',  &
              ' d_bg_mass|',' d_rt_mass|','   d_st_ht|',' d_no_stem|'
    write (UNIT=luogui1,FMT="(5(A))",ADVANCE="NO")                      &
              'b_f_fl_cov|','b_f_st_sil|','b_m_fl_cov|','b_m_st_sil|',  &
              'b_m_bg_all|'
    write (UNIT=luogui1,FMT="(4(A))",ADVANCE="NO")                      &
              '    rdg_or|','    rdg_ht|','    rdg_sp|','        rr|'
    write (UNIT=luogui1,FMT="(4(A))",ADVANCE="NO")                      &
              '   surf_84|','  surf_AS|','surf_ag_den|','surf_ag_CA|'
    write (UNIT=luogui1,FMT="(4(A))",ADVANCE="NO")                      &
              ' surf_crust|',' crust_AS|',' crust_LM|','crust_thick|'
    write (UNIT=luogui1,FMT="(3(A))",ADVANCE="NO")                      &
              '  crust_den|',' crust_LF|','  crust_CA|'
    write (UNIT=luogui1,FMT="(5(A))",ADVANCE="YES")                     &
              'soil_water|','crop_trans|','  evaporat|','    runoff|',  &
              '  drainage|'

    ! Set outer loop for nrot_years
    DO y = 1, nrot_years

       ! Print out the "P" rows here
       x = 1
       DO p = 1, nperiods
          IF (rep_dates%period(p)%sy == y) THEN
             write (UNIT=luogui1,FMT="(' P |')",ADVANCE="NO")
             if( old_run_file ) then
                write (UNIT=luogui1,FMT="(i2, '-',i2,'/',i2,'/',i2,'|')",ADVANCE="NO") &
                 rep_dates%period(p)%sd, rep_dates%period(p)%ed,                   &
                 rep_dates%period(p)%sm, rep_dates%period(p)%sy
             else
                write (UNIT=luogui1,FMT="(i2, '-',i2,'/',i2,'/',i0,'|')",ADVANCE="NO") &
                 rep_dates%period(p)%sd, rep_dates%period(p)%ed,                   &
                 rep_dates%period(p)%sm, rep_dates%period(p)%sy
             end if

             ! Check to see if an operation occurs on this date
             ! If so, set the flag and then look for any additional
             ! operations on the same date.  When done, print the
             ! concatenated list of operations and any cropname(s)
             match = .false.
             match_no = 0
             DO i = 1, size(mandate)
                IF ((mandate(i)%d == rep_dates%period(p)%sd) .and.          &
                    (mandate(i)%m == rep_dates%period(p)%sm) .and.          &
                    (mandate(i)%y == y)) THEN
                   match = .true.
                   match_no = match_no + 1
                   IF (match_no == 1) THEN
                      opname = mandate(i)%opname
                      cropname = mandate(i)%cropname
                   ELSE
                      opname = trim(opname) // "~" // mandate(i)%opname
                      cropname = trim(cropname) // "~" // mandate(i)%cropname
                   END IF
                END IF
             END DO

             IF (match) THEN
                IF (len_trim(opname) .lt. 125) THEN
                   write (UNIT=luogui1,FMT="(1x,A125,'|')",ADVANCE="NO") &
                          trim(opname)
                ELSE   ! long name, write it all out
                   write (UNIT=luogui1,FMT="(1x,A,'|')",ADVANCE="NO")   &
                          trim(opname)
                END IF
                IF (len_trim(cropname) .lt. 125) THEN
                   write (UNIT=luogui1,FMT="(1x,A125,'|')",ADVANCE="NO") &
                          trim(cropname)
                ELSE   ! long name, write it all out
                   write (UNIT=luogui1,FMT="(1x,A,'|')",ADVANCE="NO")   &
                          trim(cropname)
                END IF
             ELSE   ! No operation or crop on this date
                 write (UNIT=luogui1,FMT="(1x,A125,'|')",ADVANCE="NO") ""
                 write (UNIT=luogui1,FMT="(1x,A125,'|')",ADVANCE="NO") ""
             END IF

             write (UNIT=luogui1,FMT="(1(f10.4,'|'))",ADVANCE="NO")     & !Ave cnt
                      rep_report%period_report(N_eros_events,p)%val
             write (UNIT=luogui1,FMT="(1(f12.4,'|'))",ADVANCE="NO")     & !Total cnt
                      rep_report%period_report(N_eros_events,p)%val *              &
                      rep_report%period_report(N_eros_events,p)%cnt

             IF (rep_report%period_report(N_eros_events,p)%val > 0.0) THEN
             write (UNIT=luogui1,FMT="(1(f14.4,'|'))",ADVANCE="NO")     & !Gross loss per erosion event
                      rep_report%period_report(Eros_loss,p)%cnt *                  &
                      (rep_report%period_report(Eros_loss,p)%val -                 &
                                  rep_report%period_report(Salt_dep2,p)%val) /     &
                      (rep_report%period_report(N_eros_events,p)%val *             &
                      rep_report%period_report(N_eros_events,p)%cnt)
              write (UNIT=luogui1,FMT="(1(f14.4,'|'))",ADVANCE="NO")     & !Net loss per erosion event
                      rep_report%period_report(Eros_loss,p)%cnt *                  &
                      rep_report%period_report(Eros_loss,p)%val /                  &
                      (rep_report%period_report(N_eros_events,p)%val *             &
                      rep_report%period_report(N_eros_events,p)%cnt)
             ELSE
             write (UNIT=luogui1,FMT="(1(f14.4,'|'))",ADVANCE="NO") 0.0
             write (UNIT=luogui1,FMT="(1(f14.4,'|'))",ADVANCE="NO") 0.0
             END IF

             write (UNIT=luogui1,FMT="(1(f10.4,'|'))",ADVANCE="NO")     &
                      rep_report%period_report(Eros_loss,p)%val -                  &
                                  rep_report%period_report(Salt_dep2,p)%val

             if( old_run_file ) then
                write (UNIT=luogui1,FMT="(16(f10.4,'|'))",ADVANCE="NO")    &
                      rep_report%period_report(Eros_loss,p)%val,                   &
                      rep_report%period_report(Salt_loss,p)%val,                   &
                      rep_report%period_report(Susp_loss,p)%val,                   &
                      rep_report%period_report(PM10_loss,p)%val,                   &
                      rep_report%period_report(Salt_1,p)%val,                      &
                      rep_report%period_report(Salt_2,p)%val,                      &
                      rep_report%period_report(Salt_3,p)%val,                      &
                      rep_report%period_report(Salt_4,p)%val,                      &
                      rep_report%period_report(Susp_1,p)%val,                      &
                      rep_report%period_report(Susp_2,p)%val,                      &
                      rep_report%period_report(Susp_3,p)%val,                      &
                      rep_report%period_report(Susp_4,p)%val,                      &
                      rep_report%period_report(PM10_1,p)%val,                      &
                      rep_report%period_report(PM10_2,p)%val,                      &
                      rep_report%period_report(PM10_3,p)%val,                      &
                      rep_report%period_report(PM10_4,p)%val
             else
                write (UNIT=luogui1,FMT="(21(f10.4,'|'))",ADVANCE="NO")    &
                      rep_report%period_report(Eros_loss,p)%val,                   &
                      rep_report%period_report(Salt_loss,p)%val,                   &
                      rep_report%period_report(Susp_loss,p)%val,                   &
                      rep_report%period_report(PM10_loss,p)%val,                   &
                      rep_report%period_report(PM2_5_loss,p)%val,                  &
                      rep_report%period_report(Salt_1,p)%val,                      &
                      rep_report%period_report(Salt_2,p)%val,                      &
                      rep_report%period_report(Salt_3,p)%val,                      &
                      rep_report%period_report(Salt_4,p)%val,                      &
                      rep_report%period_report(Susp_1,p)%val,                      &
                      rep_report%period_report(Susp_2,p)%val,                      &
                      rep_report%period_report(Susp_3,p)%val,                      &
                      rep_report%period_report(Susp_4,p)%val,                      &
                      rep_report%period_report(PM10_1,p)%val,                      &
                      rep_report%period_report(PM10_2,p)%val,                      &
                      rep_report%period_report(PM10_3,p)%val,                      &
                      rep_report%period_report(PM10_4,p)%val,                      &
                      rep_report%period_report(PM2_5_1,p)%val,                     &
                      rep_report%period_report(PM2_5_2,p)%val,                     &
                      rep_report%period_report(PM2_5_3,p)%val,                     &
                      rep_report%period_report(PM2_5_4,p)%val
             end if

             write (UNIT=luogui1,FMT="(11(f10.4,'|'))",ADVANCE="NO")    &
                      rep_report%period_report(Salt_loss2_rate,p)%val,             &
                      rep_report%period_report(Salt_loss2_area,p)%val,             &
                      rep_report%period_report(Salt_loss2_frac,p)%val,             &
                      rep_report%period_report(Salt_dep2_rate,p)%val,              &
                      rep_report%period_report(Salt_dep2_area,p)%val,              &
                      rep_report%period_report(Salt_dep2_frac,p)%val,              &
                      rep_report%period_report(Trans_cap,p)%val,                   &
                      rep_report%period_report(Trans_cap_area,p)%val,              &
                      rep_report%period_report(Trans_cap_frac,p)%val,              &
                      rep_report%period_report(Sheltered_area,p)%val,              &
                      rep_report%period_report(Sheltered_frac,p)%val

             ! Check to see if period is an "end of half month" period
             ! If so, set the flag and print the "weather" variables
             ! x = half_month index value that we last had a match
             match = .false.
             DO hm = x, 24
              !print *, "p/hm/y: ", p, hm,y, rep_dates%period(p), rep_dates%hmonth(hm,y)
                IF ((rep_dates%hmonth(hm,y)%ed == rep_dates%period(p)%ed) .and.  &
                    (rep_dates%hmonth(hm,y)%em == rep_dates%period(p)%em) .and.  &
                    (rep_dates%hmonth(hm,y)%ey == rep_dates%period(p)%ey)) THEN
                   match = .true.
                   x = hm 
                   write (UNIT=luogui1,FMT="(4(f10.4,'|'))",ADVANCE="NO") &
                         rep_report%hmonth_report(Precipi,hm,y)%val,                  &
                         rep_report%hmonth_report(Wind_energy,hm,y)%val,              &
                         rep_report%hmonth_report(Snow_cover,hm,y)%val,               &
                         rep_report%hmonth_report(Irrigation,hm,y)%val
                   GOTO 20
                END IF
             END DO
20           IF (.not. match) THEN  ! No climate info on this date
                 write (UNIT=luogui1,FMT="(4(A10,'|'))",ADVANCE="NO") "","","",""
             END IF

             write (UNIT=luogui1,FMT="(6(f10.4,'|'))",ADVANCE="NO")    &
                      rep_report%period_report(Crop_canopy_cov,p)%val,             &
                      rep_report%period_report(Crop_stand_sil,p)%val,              &
                      rep_report%period_report(Crop_stand_mass,p)%val,             &
                      rep_report%period_report(Crop_root_mass,p)%val,              &
                      rep_report%period_report(Crop_stand_height,p)%val,           &
                      rep_report%period_report(Crop_number_stems,p)%val
             write (UNIT=luogui1,FMT="(8(f10.4,'|'))",ADVANCE="NO")    &
                      rep_report%period_report(Res_flat_cov,p)%val,                &
                      rep_report%period_report(Res_stand_sil,p)%val,               &
                      rep_report%period_report(Res_flat_mass,p)%val,               &
                      rep_report%period_report(Res_stand_mass,p)%val,              &
                      rep_report%period_report(Res_buried_mass,p)%val,             &
                      rep_report%period_report(Res_root_mass,p)%val,               &
                      rep_report%period_report(Res_stand_height,p)%val,            &
                      rep_report%period_report(Res_number_stems,p)%val
             write (UNIT=luogui1,FMT="(5(f10.4,'|'))",ADVANCE="NO")    &
                      rep_report%period_report(All_flat_cov,p)%val,                &
                      rep_report%period_report(All_stand_sil,p)%val,               &
                      rep_report%period_report(All_flat_mass,p)%val,               &
                      rep_report%period_report(All_stand_mass,p)%val,              &
                      rep_report%period_report(All_buried_mass,p)%val

             write (UNIT=luogui1,FMT="(4(f10.4,'|'))",ADVANCE="NO")     &
                      rep_report%period_report(Ridge_dir,p)%val,                   &
                      rep_report%period_report(Ridge_ht,p)%val,                    &
                      rep_report%period_report(Ridge_sp,p)%val,                    &
                      rep_report%period_report(Random_rough,p)%val

             write (UNIT=luogui1,FMT="(4(f10.4,'|'))",ADVANCE="NO")     &
                      rep_report%period_report(Surface_Ag_84,p)%val,               &
                      rep_report%period_report(Surface_Ag_AS,p)%val,               &
                      rep_report%period_report(Surface_Ag_DN,p)%val,               &
                      rep_report%period_report(Surface_Ag_CA,p)%val

             write (UNIT=luogui1,FMT="(7(f10.4,'|'))",ADVANCE="NO")     &
                      rep_report%period_report(Surface_Cr,p)%val,                  &
                      rep_report%period_report(Surface_Cr_AS,p)%val,               &
                      rep_report%period_report(Surface_Cr_LM,p)%val,               &
                      rep_report%period_report(Surface_Cr_TH,p)%val,               &
                      rep_report%period_report(Surface_Cr_DN,p)%val,               &
                      rep_report%period_report(Surface_Cr_LF,p)%val,               &
                      rep_report%period_report(Surface_Cr_CA,p)%val

            write (UNIT=luogui1,FMT="(5(f10.4,'|'))",ADVANCE="YES")     &
                      rep_report%period_report(Soil_Water,p)%val,                  &
                      rep_report%period_report(Crop_Transp,p)%val,                 &
                      rep_report%period_report(Evaporation,p)%val,                 &
                      rep_report%period_report(Runoff,p)%val,                      &
                      rep_report%period_report(Drainage,p)%val               
         END IF
       END DO

       ! Print out the "Y" rows (rotation yearly values) here
       write (UNIT=luogui1,FMT="(' Y |')",ADVANCE="NO")
       if( old_run_file ) then
          write (UNIT=luogui1,FMT="(1('Rot. yr: ',i2,'|'))",ADVANCE="NO")  &
              rep_dates%yrly(y)%sy
       else
          write (UNIT=luogui1,FMT="(1('Rot. yr: ',i0,'|'))",ADVANCE="NO")  &
              rep_dates%yrly(y)%sy
       end if
       write (UNIT=luogui1,FMT="(1x,A125,'|')",ADVANCE="NO") "" !skip op field
       write (UNIT=luogui1,FMT="(1x,A125,'|')",ADVANCE="NO") "" !skip crop field

       write (UNIT=luogui1,FMT="(1(f10.4,'|'))",ADVANCE="NO")     & !Ave cnt
                 rep_report%yrly_report(N_eros_events,y)%val
       write (UNIT=luogui1,FMT="(1(f12.4,'|'))",ADVANCE="NO")     & !Total cnt
                 rep_report%yrly_report(N_eros_events,y)%val *              &
                 rep_report%yrly_report(N_eros_events,y)%cnt

       IF (rep_report%yrly_report(N_eros_events,y)%val > 0.0) THEN
       write (UNIT=luogui1,FMT="(1(f14.4,'|'))",ADVANCE="NO")     & !Gross loss per erosion event
                 rep_report%yrly_report(Eros_loss,y)%cnt *                  &
                 (rep_report%yrly_report(Eros_loss,y)%val -                 &
                              rep_report%yrly_report(Salt_dep2,y)%val) /    &
                 (rep_report%yrly_report(N_eros_events,y)%val *             &
                 rep_report%yrly_report(N_eros_events,y)%cnt)
       write (UNIT=luogui1,FMT="(1(f14.4,'|'))",ADVANCE="NO")     & !Net loss per erosion event
                 rep_report%yrly_report(Eros_loss,y)%cnt *                  &
                 rep_report%yrly_report(Eros_loss,y)%val /                  &
                 (rep_report%yrly_report(N_eros_events,y)%val *             &
                 rep_report%yrly_report(N_eros_events,y)%cnt)
       ELSE
       write (UNIT=luogui1,FMT="(1(f14.4,'|'))",ADVANCE="NO") 0.0
       write (UNIT=luogui1,FMT="(1(f14.4,'|'))",ADVANCE="NO") 0.0
       END IF
 
       write (UNIT=luogui1,FMT="(1(f10.4,'|'))",ADVANCE="NO")           &
                      rep_report%yrly_report(Eros_loss,y)%val -                    &
                               rep_report%yrly_report(Salt_dep2,y)%val

       if( old_run_file ) then
          write (UNIT=luogui1,FMT="(21(f10.4,'|'))",ADVANCE="NO")          &
                      rep_report%yrly_report(Eros_loss,y)%val,                     &
                      rep_report%yrly_report(Salt_loss,y)%val,                     &
                      rep_report%yrly_report(Susp_loss,y)%val,                     &
                      rep_report%yrly_report(PM10_loss,y)%val,                     &
                      rep_report%yrly_report(Salt_1,y)%val,                        &
                      rep_report%yrly_report(Salt_2,y)%val,                        &
                      rep_report%yrly_report(Salt_3,y)%val,                        &
                      rep_report%yrly_report(Salt_4,y)%val,                        &
                      rep_report%yrly_report(Susp_1,y)%val,                        &
                      rep_report%yrly_report(Susp_2,y)%val,                        &
                      rep_report%yrly_report(Susp_3,y)%val,                        &
                      rep_report%yrly_report(Susp_4,y)%val,                        &
                      rep_report%yrly_report(PM10_1,y)%val,                        &
                      rep_report%yrly_report(PM10_2,y)%val,                        &
                      rep_report%yrly_report(PM10_3,y)%val,                        &
                      rep_report%yrly_report(PM10_4,y)%val
       else
          write (UNIT=luogui1,FMT="(21(f10.4,'|'))",ADVANCE="NO")          &
                      rep_report%yrly_report(Eros_loss,y)%val,                     &
                      rep_report%yrly_report(Salt_loss,y)%val,                     &
                      rep_report%yrly_report(Susp_loss,y)%val,                     &
                      rep_report%yrly_report(PM10_loss,y)%val,                     &
                      rep_report%yrly_report(PM2_5_loss,y)%val,                    &
                      rep_report%yrly_report(Salt_1,y)%val,                        &
                      rep_report%yrly_report(Salt_2,y)%val,                        &
                      rep_report%yrly_report(Salt_3,y)%val,                        &
                      rep_report%yrly_report(Salt_4,y)%val,                        &
                      rep_report%yrly_report(Susp_1,y)%val,                        &
                      rep_report%yrly_report(Susp_2,y)%val,                        &
                      rep_report%yrly_report(Susp_3,y)%val,                        &
                      rep_report%yrly_report(Susp_4,y)%val,                        &
                      rep_report%yrly_report(PM10_1,y)%val,                        &
                      rep_report%yrly_report(PM10_2,y)%val,                        &
                      rep_report%yrly_report(PM10_3,y)%val,                        &
                      rep_report%yrly_report(PM10_4,y)%val,                        &
                      rep_report%yrly_report(PM2_5_1,y)%val,                       &
                      rep_report%yrly_report(PM2_5_2,y)%val,                       &
                      rep_report%yrly_report(PM2_5_3,y)%val,                       &
                      rep_report%yrly_report(PM2_5_4,y)%val
       end if
             write (UNIT=luogui1,FMT="(11(f10.4,'|'))",ADVANCE="NO")    &
                      rep_report%yrly_report(Salt_loss2_rate,y)%val,               &
                      rep_report%yrly_report(Salt_loss2_area,y)%val,               &
                      rep_report%yrly_report(Salt_loss2_frac,y)%val,               &
                      rep_report%yrly_report(Salt_dep2_rate,y)%val,                &
                      rep_report%yrly_report(Salt_dep2_area,y)%val,                &
                      rep_report%yrly_report(Salt_dep2_frac,y)%val,                &
                      rep_report%yrly_report(Trans_cap,y)%val,                     &
                      rep_report%yrly_report(Trans_cap_area,y)%val,                &
                      rep_report%yrly_report(Trans_cap_frac,y)%val,                &
                      rep_report%yrly_report(Sheltered_area,y)%val,                &
                      rep_report%yrly_report(Sheltered_frac,y)%val

        write (UNIT=luogui1,FMT="(4(f10.4,'|'))",ADVANCE="NO")          &
                         rep_report%yrly_report(Precipi,y)%val,                    &
                         rep_report%yrly_report(Wind_energy,y)%val,                &
                         rep_report%yrly_report(Snow_cover,y)%val,                 &
                         rep_report%yrly_report(Irrigation,y)%val

       DO i = 1, N_eop_vars !skip veg/surf fields
         write (UNIT=luogui1,FMT="(A7,'N/A|')",ADVANCE="NO") ""
       END DO

            write (UNIT=luogui1,FMT="(4(f10.4,'|'))",ADVANCE="NO")      &
                      rep_report%yrly_report(Crop_Transp,y)%val,                   &
                      rep_report%yrly_report(Evaporation,y)%val,                   &
                      rep_report%yrly_report(Runoff,y)%val,                        &
                      rep_report%yrly_report(Drainage,y)%val 
              
       write (UNIT=luogui1,FMT="(A)",ADVANCE="YES") ""
    END DO


    ! Print out the "m" rows (monthly values) here
    y = 0
    DO m = 1, 12
       write (UNIT=luogui1,FMT="(' m |')",ADVANCE="NO")
       write (UNIT=luogui1,FMT="(A,i3,A)",ADVANCE="NO")                      &
                 'Month:  ',m,'|'
              
       write (UNIT=luogui1,FMT="(1x,A125,'|')",ADVANCE="NO") "" !skip op field
       write (UNIT=luogui1,FMT="(1x,A125,'|')",ADVANCE="NO") "" !skip crop field

       write (UNIT=luogui1,FMT="(1(f10.4,'|'))",ADVANCE="NO")     & !Ave cnt
                 rep_report%monthly_report(N_eros_events,m,y)%val
       write (UNIT=luogui1,FMT="(1(f12.4,'|'))",ADVANCE="NO")     & !Total cnt
                 rep_report%monthly_report(N_eros_events,m,y)%val *              &
                 rep_report%monthly_report(N_eros_events,m,y)%cnt
 
       IF (rep_report%monthly_report(N_eros_events,m,y)%val > 0.0) THEN  
       write (UNIT=luogui1,FMT="(1(f14.4,'|'))",ADVANCE="NO")     & !Gross loss per erosion event
                 rep_report%monthly_report(Eros_loss,m,y)%cnt *                  &
                 (rep_report%monthly_report(Eros_loss,m,y)%val -                 &
                              rep_report%monthly_report(Salt_dep2,m,y)%val) /    &
                 (rep_report%monthly_report(N_eros_events,m,y)%val *             &
                 rep_report%monthly_report(N_eros_events,m,y)%cnt)
       write (UNIT=luogui1,FMT="(1(f14.4,'|'))",ADVANCE="NO")     & !Net loss per erosion event
                 rep_report%monthly_report(Eros_loss,m,y)%cnt *                  &
                 rep_report%monthly_report(Eros_loss,m,y)%val /                  &
                 (rep_report%monthly_report(N_eros_events,m,y)%val *             &
                 rep_report%monthly_report(N_eros_events,m,y)%cnt)
       ELSE
       write (UNIT=luogui1,FMT="(1(f14.4,'|'))",ADVANCE="NO") 0.0
       write (UNIT=luogui1,FMT="(1(f14.4,'|'))",ADVANCE="NO") 0.0
       END IF

       write (UNIT=luogui1,FMT="(1(f10.4,'|'))",ADVANCE="NO")           &
                      rep_report%monthly_report(Eros_loss,m,y)%val -               &
                                rep_report%monthly_report(Salt_dep2,m,y)%val

       if( old_run_file ) then
          write (UNIT=luogui1,FMT="(16(f10.4,'|'))",ADVANCE="NO")          &
                      rep_report%monthly_report(Eros_loss,m,y)%val,                &
                      rep_report%monthly_report(Salt_loss,m,y)%val,                &
                      rep_report%monthly_report(Susp_loss,m,y)%val,                &
                      rep_report%monthly_report(PM10_loss,m,y)%val,                &
                      rep_report%monthly_report(Salt_1,m,y)%val,                   &
                      rep_report%monthly_report(Salt_2,m,y)%val,                   &
                      rep_report%monthly_report(Salt_3,m,y)%val,                   &
                      rep_report%monthly_report(Salt_4,m,y)%val,                   &
                      rep_report%monthly_report(Susp_1,m,y)%val,                   &
                      rep_report%monthly_report(Susp_2,m,y)%val,                   &
                      rep_report%monthly_report(Susp_3,m,y)%val,                   &
                      rep_report%monthly_report(Susp_4,m,y)%val,                   &
                      rep_report%monthly_report(PM10_1,m,y)%val,                   &
                      rep_report%monthly_report(PM10_2,m,y)%val,                   &
                      rep_report%monthly_report(PM10_3,m,y)%val,                   &
                      rep_report%monthly_report(PM10_4,m,y)%val
       else
          write (UNIT=luogui1,FMT="(21(f10.4,'|'))",ADVANCE="NO")          &
                      rep_report%monthly_report(Eros_loss,m,y)%val,                &
                      rep_report%monthly_report(Salt_loss,m,y)%val,                &
                      rep_report%monthly_report(Susp_loss,m,y)%val,                &
                      rep_report%monthly_report(PM10_loss,m,y)%val,                &
                      rep_report%monthly_report(PM2_5_loss,m,y)%val,               &
                      rep_report%monthly_report(Salt_1,m,y)%val,                   &
                      rep_report%monthly_report(Salt_2,m,y)%val,                   &
                      rep_report%monthly_report(Salt_3,m,y)%val,                   &
                      rep_report%monthly_report(Salt_4,m,y)%val,                   &
                      rep_report%monthly_report(Susp_1,m,y)%val,                   &
                      rep_report%monthly_report(Susp_2,m,y)%val,                   &
                      rep_report%monthly_report(Susp_3,m,y)%val,                   &
                      rep_report%monthly_report(Susp_4,m,y)%val,                   &
                      rep_report%monthly_report(PM10_1,m,y)%val,                   &
                      rep_report%monthly_report(PM10_2,m,y)%val,                   &
                      rep_report%monthly_report(PM10_3,m,y)%val,                   &
                      rep_report%monthly_report(PM10_4,m,y)%val,                   &
                      rep_report%monthly_report(PM2_5_1,m,y)%val,                  &
                      rep_report%monthly_report(PM2_5_2,m,y)%val,                  &
                      rep_report%monthly_report(PM2_5_3,m,y)%val,                  &
                      rep_report%monthly_report(PM2_5_4,m,y)%val
       end if

             write (UNIT=luogui1,FMT="(11(f10.4,'|'))",ADVANCE="NO")    &
                      rep_report%monthly_report(Salt_loss2_rate,m,y)%val,          &
                      rep_report%monthly_report(Salt_loss2_area,m,y)%val,          &
                      rep_report%monthly_report(Salt_loss2_frac,m,y)%val,          &
                      rep_report%monthly_report(Salt_dep2_rate,m,y)%val,           &
                      rep_report%monthly_report(Salt_dep2_area,m,y)%val,           &
                      rep_report%monthly_report(Salt_dep2_frac,m,y)%val,           &
                      rep_report%monthly_report(Trans_cap,m,y)%val,                &
                      rep_report%monthly_report(Trans_cap_area,m,y)%val,           &
                      rep_report%monthly_report(Trans_cap_frac,m,y)%val,           &
                      rep_report%monthly_report(Sheltered_area,m,y)%val,           &
                      rep_report%monthly_report(Sheltered_frac,m,y)%val

        write (UNIT=luogui1,FMT="(4(f10.4,'|'))",ADVANCE="NO")          &
                         rep_report%monthly_report(Precipi,m,y)%val,               &
                         rep_report%monthly_report(Wind_energy,m,y)%val,           &
                         rep_report%monthly_report(Snow_cover,m,y)%val,            &
                         rep_report%monthly_report(Irrigation,m,y)%val


        DO i = 1, N_eop_vars !skip veg/surf fields
           write (UNIT=luogui1,FMT="(A7,'N/A|')",ADVANCE="NO") ""
        END DO

            write (UNIT=luogui1,FMT="(4(f10.4,'|'))",ADVANCE="NO")      &
                      rep_report%monthly_report(Crop_Transp,m,y)%val,              &
                      rep_report%monthly_report(Evaporation,m,y)%val,              &
                      rep_report%monthly_report(Runoff,m,y)%val,                   &
                      rep_report%monthly_report(Drainage,m,y)%val 

        write (UNIT=luogui1,FMT="(A)",ADVANCE="YES") ""

    END DO

    ! Print out the "y" rows (averaged individual year) values here
    DO y = 1, nrot_years*ncycles
        ! print the simulation run individual yearly ave values here
        write (UNIT=luogui1,FMT="(' y |')",ADVANCE="NO")
        if( old_run_file ) then
           write (UNIT=luogui1,FMT="(A,i4,A)",ADVANCE="NO")                      &
                 'Year:  ',y,'|'
        else
           write (UNIT=luogui1,FMT="(A,i0,A)",ADVANCE="NO")                      &
                 'Year:  ',y,'|'
        end if
        write (UNIT=luogui1,FMT="(1x,A125,'|')",ADVANCE="NO") "" !skip op field
        write (UNIT=luogui1,FMT="(1x,A125,'|')",ADVANCE="NO") "" !skip crop field

       write (UNIT=luogui1,FMT="(1(f10.4,'|'))",ADVANCE="NO")     & !Ave cnt
                 rep_report%yr_report(N_eros_events,y)%val
       write (UNIT=luogui1,FMT="(1(f12.4,'|'))",ADVANCE="NO")     & !Total cnt
                 rep_report%yr_report(N_eros_events,y)%val *              &
                 rep_report%yr_report(N_eros_events,y)%cnt

       IF (rep_report%yr_report(N_eros_events,y)%val > 0.0) THEN
       write (UNIT=luogui1,FMT="(1(f14.4,'|'))",ADVANCE="NO")     & !Gross loss per erosion event
                 rep_report%yr_report(Eros_loss,y)%cnt *                  &
                 (rep_report%yr_report(Eros_loss,y)%val -                 &
                              rep_report%yr_report(Salt_dep2,y)%val) /    &
                 (rep_report%yr_report(N_eros_events,y)%val *             &
                 rep_report%yr_report(N_eros_events,y)%cnt)
       write (UNIT=luogui1,FMT="(1(f14.4,'|'))",ADVANCE="NO")     & !Net loss per erosion event
                 rep_report%yr_report(Eros_loss,y)%cnt *                  &
                 rep_report%yr_report(Eros_loss,y)%val /                  &
                 (rep_report%yr_report(N_eros_events,y)%val *              &
                 rep_report%yr_report(N_eros_events,y)%cnt)
       ELSE
       write (UNIT=luogui1,FMT="(1(f14.4,'|'))",ADVANCE="NO") 0.0
       write (UNIT=luogui1,FMT="(1(f14.4,'|'))",ADVANCE="NO") 0.0
       END IF

        write (UNIT=luogui1,FMT="(1(f10.4,'|'))",ADVANCE="NO")            &
                          rep_report%yr_report(Eros_loss,y)%val -                    &
                                 rep_report%yr_report(Salt_dep2,y)%val

       if( old_run_file ) then
          write (UNIT=luogui1,FMT="(16(f10.4,'|'))",ADVANCE="NO")           &
                          rep_report%yr_report(Eros_loss,y)%val,                     &
                          rep_report%yr_report(Salt_loss,y)%val,                     &
                          rep_report%yr_report(Susp_loss,y)%val,                     &
                          rep_report%yr_report(PM10_loss,y)%val,                     &
                          rep_report%yr_report(Salt_1,y)%val,                        &
                          rep_report%yr_report(Salt_2,y)%val,                        &
                          rep_report%yr_report(Salt_3,y)%val,                        &
                          rep_report%yr_report(Salt_4,y)%val,                        &
                          rep_report%yr_report(Susp_1,y)%val,                        &
                          rep_report%yr_report(Susp_2,y)%val,                        &
                          rep_report%yr_report(Susp_3,y)%val,                        &
                          rep_report%yr_report(Susp_4,y)%val,                        &
                          rep_report%yr_report(PM10_1,y)%val,                        &
                          rep_report%yr_report(PM10_2,y)%val,                        &
                          rep_report%yr_report(PM10_3,y)%val,                        &
                          rep_report%yr_report(PM10_4,y)%val
       else
          write (UNIT=luogui1,FMT="(21(f10.4,'|'))",ADVANCE="NO")           &
                          rep_report%yr_report(Eros_loss,y)%val,                     &
                          rep_report%yr_report(Salt_loss,y)%val,                     &
                          rep_report%yr_report(Susp_loss,y)%val,                     &
                          rep_report%yr_report(PM10_loss,y)%val,                     &
                          rep_report%yr_report(PM2_5_loss,y)%val,                    &
                          rep_report%yr_report(Salt_1,y)%val,                        &
                          rep_report%yr_report(Salt_2,y)%val,                        &
                          rep_report%yr_report(Salt_3,y)%val,                        &
                          rep_report%yr_report(Salt_4,y)%val,                        &
                          rep_report%yr_report(Susp_1,y)%val,                        &
                          rep_report%yr_report(Susp_2,y)%val,                        &
                          rep_report%yr_report(Susp_3,y)%val,                        &
                          rep_report%yr_report(Susp_4,y)%val,                        &
                          rep_report%yr_report(PM10_1,y)%val,                        &
                          rep_report%yr_report(PM10_2,y)%val,                        &
                          rep_report%yr_report(PM10_3,y)%val,                        &
                          rep_report%yr_report(PM10_4,y)%val,                        &
                          rep_report%yr_report(PM2_5_1,y)%val,                       &
                          rep_report%yr_report(PM2_5_2,y)%val,                       &
                          rep_report%yr_report(PM2_5_3,y)%val,                       &
                          rep_report%yr_report(PM2_5_4,y)%val
       end if
    
        write (UNIT=luogui1,FMT="(11(f10.4,'|'))",ADVANCE="NO")           &
                          rep_report%yr_report(Salt_loss2_rate,y)%val,               &
                          rep_report%yr_report(Salt_loss2_area,y)%val,               &
                          rep_report%yr_report(Salt_loss2_frac,y)%val,               &
                          rep_report%yr_report(Salt_dep2_rate,y)%val,                &
                          rep_report%yr_report(Salt_dep2_area,y)%val,                &
                          rep_report%yr_report(Salt_dep2_frac,y)%val,                &
                          rep_report%yr_report(Trans_cap,y)%val,                     &
                          rep_report%yr_report(Trans_cap_area,y)%val,                &
                          rep_report%yr_report(Trans_cap_frac,y)%val,                &
                          rep_report%yr_report(Sheltered_area,y)%val,                &
                          rep_report%yr_report(Sheltered_frac,y)%val
    
    
        write (UNIT=luogui1,FMT="(4(f10.4,'|'))",ADVANCE="NO")            &
                             rep_report%yr_report(Precipi,y)%val,                    &
                             rep_report%yr_report(Wind_energy,y)%val,                &
                             rep_report%yr_report(Snow_cover,y)%val,                 &
                             rep_report%yr_report(Irrigation,y)%val
   
        DO i = 1, N_eop_vars !skip veg/surf fields
          write (UNIT=luogui1,FMT="(A7,'N/A|')",ADVANCE="NO") ""
        END DO

           write (UNIT=luogui1,FMT="(4(f10.4,'|'))",ADVANCE="NO")        &
                      rep_report%yr_report(Crop_Transp,y)%val,                      &
                      rep_report%yr_report(Evaporation,y)%val,                      &
                      rep_report%yr_report(Runoff,y)%val,                           &
                      rep_report%yr_report(Drainage,y)%val 

        write (UNIT=luogui1,FMT="(A)",ADVANCE="YES") ""
    END DO

    ! Print out the "T" row (total rotation average yearly) values here
    ! print the simulation run yearly average values here
    write (UNIT=luogui1,FMT="(' T |')",ADVANCE="NO")
    write (UNIT=luogui1,FMT="(2(A))",ADVANCE="NO")                      &
             'Ave. Annual','|'
    write (UNIT=luogui1,FMT="(1x,A125,'|')",ADVANCE="NO") "" !skip op field
    write (UNIT=luogui1,FMT="(1x,A125,'|')",ADVANCE="NO") "" !skip crop field
    y = 0

    write (UNIT=luogui1,FMT="(1(f10.4,'|'))",ADVANCE="NO")     & !Ave cnt
                 rep_report%yrly_report(N_eros_events,y)%val
    write (UNIT=luogui1,FMT="(1(f12.4,'|'))",ADVANCE="NO")     & !Total cnt
                 rep_report%yrly_report(N_eros_events,y)%val *              &
                 rep_report%yrly_report(N_eros_events,y)%cnt

    IF (rep_report%yrly_report(N_eros_events,y)%val > 0.0) THEN
    write (UNIT=luogui1,FMT="(1(f14.4,'|'))",ADVANCE="NO")     & !Gross loss per erosion event
                 rep_report%yrly_report(Eros_loss,y)%cnt *                  &
                 (rep_report%yrly_report(Eros_loss,y)%val -                 &
                              rep_report%yrly_report(Salt_dep2,y)%val) /    &
                 (rep_report%yrly_report(N_eros_events,y)%val *             &
                 rep_report%yrly_report(N_eros_events,y)%cnt)
    write (UNIT=luogui1,FMT="(1(f14.4,'|'))",ADVANCE="NO")     & !Net loss per erosion event
                 rep_report%yrly_report(Eros_loss,y)%cnt *                  &
                 rep_report%yrly_report(Eros_loss,y)%val /                  &
                 (rep_report%yrly_report(N_eros_events,y)%val *             &
                 rep_report%yrly_report(N_eros_events,y)%cnt)
    ELSE
    write (UNIT=luogui1,FMT="(1(f14.4,'|'))",ADVANCE="NO") 0.0
    write (UNIT=luogui1,FMT="(1(f14.4,'|'))",ADVANCE="NO") 0.0
    END IF

    write (UNIT=luogui1,FMT="(1(f10.4,'|'))",ADVANCE="NO")              &
                      rep_report%yrly_report(Eros_loss,y)%val -                    &
                            rep_report%yrly_report(Salt_dep2,y)%val

    if( old_run_file ) then
       write (UNIT=luogui1,FMT="(16(f10.4,'|'))",ADVANCE="NO")             &
                      rep_report%yrly_report(Eros_loss,y)%val,                     &
                      rep_report%yrly_report(Salt_loss,y)%val,                     &
                      rep_report%yrly_report(Susp_loss,y)%val,                     &
                      rep_report%yrly_report(PM10_loss,y)%val,                     &
                      rep_report%yrly_report(Salt_1,y)%val,                        &
                      rep_report%yrly_report(Salt_2,y)%val,                        &
                      rep_report%yrly_report(Salt_3,y)%val,                        &
                      rep_report%yrly_report(Salt_4,y)%val,                        &
                      rep_report%yrly_report(Susp_1,y)%val,                        &
                      rep_report%yrly_report(Susp_2,y)%val,                        &
                      rep_report%yrly_report(Susp_3,y)%val,                        &
                      rep_report%yrly_report(Susp_4,y)%val,                        &
                      rep_report%yrly_report(PM10_1,y)%val,                        &
                      rep_report%yrly_report(PM10_2,y)%val,                        &
                      rep_report%yrly_report(PM10_3,y)%val,                        &
                      rep_report%yrly_report(PM10_4,y)%val
    else
       write (UNIT=luogui1,FMT="(21(f10.4,'|'))",ADVANCE="NO")             &
                      rep_report%yrly_report(Eros_loss,y)%val,                     &
                      rep_report%yrly_report(Salt_loss,y)%val,                     &
                      rep_report%yrly_report(Susp_loss,y)%val,                     &
                      rep_report%yrly_report(PM10_loss,y)%val,                     &
                      rep_report%yrly_report(PM2_5_loss,y)%val,                    &
                      rep_report%yrly_report(Salt_1,y)%val,                        &
                      rep_report%yrly_report(Salt_2,y)%val,                        &
                      rep_report%yrly_report(Salt_3,y)%val,                        &
                      rep_report%yrly_report(Salt_4,y)%val,                        &
                      rep_report%yrly_report(Susp_1,y)%val,                        &
                      rep_report%yrly_report(Susp_2,y)%val,                        &
                      rep_report%yrly_report(Susp_3,y)%val,                        &
                      rep_report%yrly_report(Susp_4,y)%val,                        &
                      rep_report%yrly_report(PM10_1,y)%val,                        &
                      rep_report%yrly_report(PM10_2,y)%val,                        &
                      rep_report%yrly_report(PM10_3,y)%val,                        &
                      rep_report%yrly_report(PM10_4,y)%val,                        &
                      rep_report%yrly_report(PM2_5_1,y)%val,                       &
                      rep_report%yrly_report(PM2_5_2,y)%val,                       &
                      rep_report%yrly_report(PM2_5_3,y)%val,                       &
                      rep_report%yrly_report(PM2_5_4,y)%val
    end if

    write (UNIT=luogui1,FMT="(11(f10.4,'|'))",ADVANCE="NO")             &
                      rep_report%yrly_report(Salt_loss2_rate,y)%val,               &
                      rep_report%yrly_report(Salt_loss2_area,y)%val,               &
                      rep_report%yrly_report(Salt_loss2_frac,y)%val,               &
                      rep_report%yrly_report(Salt_dep2_rate,y)%val,                &
                      rep_report%yrly_report(Salt_dep2_area,y)%val,                &
                      rep_report%yrly_report(Salt_dep2_frac,y)%val,                &
                      rep_report%yrly_report(Trans_cap,y)%val,                     &
                      rep_report%yrly_report(Trans_cap_area,y)%val,                &
                      rep_report%yrly_report(Trans_cap_frac,y)%val,                &
                      rep_report%yrly_report(Sheltered_area,y)%val,                &
                      rep_report%yrly_report(Sheltered_frac,y)%val

    write (UNIT=luogui1,FMT="(4(f10.4,'|'))",ADVANCE="NO")              &
                         rep_report%yrly_report(Precipi,y)%val,                    &
                         rep_report%yrly_report(Wind_energy,y)%val,                &
                         rep_report%yrly_report(Snow_cover,y)%val,                 &
                         rep_report%yrly_report(Irrigation,y)%val

    DO i = 1, N_eop_vars !skip veg/surf fields
      write (UNIT=luogui1,FMT="(A7,'N/A|')",ADVANCE="NO") ""
    END DO

           write (UNIT=luogui1,FMT="(4(f10.4,'|'))",ADVANCE="NO")      &
                      rep_report%yrly_report(Crop_Transp,y)%val,                  &
                      rep_report%yrly_report(Evaporation,y)%val,                  &
                      rep_report%yrly_report(Runoff,y)%val,                       &
                      rep_report%yrly_report(Drainage,y)%val 

    write (UNIT=luogui1,FMT="(A)",ADVANCE="YES") ""

  END SUBROUTINE print_ui1_output

end module print_ui1_output_mod

