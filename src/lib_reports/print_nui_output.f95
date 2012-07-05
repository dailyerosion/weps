!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!
! This is to eventually be the "new" output data file used to populate
! the WEPS 1.0 user interface tabular output forms.
!
! The general output format will consist of all columns in the
! specified header order as printed.
! The "time period" of the output will be in the following order:
! 1. rotation year periods
!    (rotation year half months split by management operations)
! 2. rotation year half month periods (half month averages by rotation year)
! 3. rotation year monthly periods (monthly averages by rotation year)
! 4. rotation year periods (rotation year averages)
! 5. monthly periods (total rotation monthly averages)
! 6. yearly period (total rotation yearly average)

! Things to be aware of:
! A) Only the last management operation is printed
!    if multiple operations are listed on the same day.

SUBROUTINE print_nui_output(nperiods, nrot_years)

    USE pd_dates_vars
    USE pd_update_vars
    USE pd_report_vars

    USE pd_var_tables
    USE mandate_vars

    IMPLICIT NONE

    INTEGER, INTENT (IN) :: nperiods
    INTEGER, INTENT (IN) :: nrot_years
    INTEGER :: i,m,y,p              ! local loop variables

    LOGICAL :: match

!   format for header of ui output report file

    write (UNIT=52,FMT="(2(A))",ADVANCE="NO")                           &
              ' d mo yr ', ' d mo yr '
    write (UNIT=52,FMT="(2(A))",ADVANCE="NO")                           &
              ' operation                                ',             &
              ' crop/vegetation                          '
    write (UNIT=52,FMT="(4(A))",ADVANCE="NO")                           &
              '  tot_loss ','  crp+salt ','    suspen ','      pm10 '
    write (UNIT=52,FMT="(12(A))",ADVANCE="NO")                          &
              '       cs1 ','       cs2 ','       cs3 ','       cs4 ',  &
              '       ss1 ','       ss2 ','       ss3 ','       ss4 ',  &
              '       pm1 ','       pm2 ','       pm3 ','       pm4 '
    write (UNIT=52,FMT="(3(A))",ADVANCE="NO")                           &
              '    precip ','  w_energy ','   dry_idx '
    write (UNIT=52,FMT="(3(A))",ADVANCE="NO")                           &
              ' l_can_cov ','l_sil_area ',' l_st_mass '
    write (UNIT=52,FMT="(4(A))",ADVANCE="NO")                           &
              '  d_fl_cov ','  d_st_sil ',' d_fl_mass ',' d_st_mass '
    write (UNIT=52,FMT="(4(A))",ADVANCE="NO")                           &
              'b_f_fl_cov ','b_f_st_sil ','b_m_fl_cov ','b_m_st_sil '
    write (UNIT=52,FMT="(12(A))",ADVANCE="YES")                         &
              '    rdg_or ','    rdg_ht ','    rdg_sp ','        rr '

    ! Rotation period output
    write (UNIT=52,FMT="(1(A))",ADVANCE="YES") '# Rotation period output'
    DO p = 1, nperiods

       write (UNIT=52,FMT="(i2,'/',i2,'/',i2,' ')",ADVANCE="NO")        &
       period_dates(p)%sd, period_dates(p)%sm, period_dates(p)%sy
       write (UNIT=52,FMT="(i2,'/',i2,'/',i2,' ')",ADVANCE="NO")        &
       period_dates(p)%ed, period_dates(p)%em, period_dates(p)%ey

       ! Check to see if an operation occurs on this date
       ! If so, set the flag and print the last operation
       ! done on this date (if multiple operations on same date)
       match = .false.
       DO i = size(mandate), 1, -1
          IF ((mandate(i)%d == period_dates(p)%sd) .and.                &
              (mandate(i)%m == period_dates(p)%sm) .and.                &
              (mandate(i)%y == y)) THEN
             match = .true.
             write (UNIT=52,FMT="(1x,'',A80,' ')",ADVANCE="NO")       &
                   mandate(i)%opname
             write (UNIT=52,FMT="(1x,'',A80,' ')",ADVANCE="NO")       &
                   mandate(i)%cropname
             GOTO 10
          END IF
       END DO
!10     IF (match == .false.) THEN  ! No operation (or crop) on this date
10     IF (.not. match) THEN  ! No operation (or crop) on this date
           write (UNIT=52,FMT="(1x,A80,' ')",ADVANCE="NO") ""
           write (UNIT=52,FMT="(1x,A80,' ')",ADVANCE="NO") ""
       END IF

       write (UNIT=52,FMT="(4(f10.4,' '))",ADVANCE="NO")                &
             period_report(Eros_loss,p)%val,                            &
             period_report(Salt_loss,p)%val,                            &
             period_report(Susp_loss,p)%val,                            &
             period_report(PM10_loss,p)%val
       write (UNIT=52,FMT="(12(f10.4,' '))",ADVANCE="NO")               &
             period_report(Salt_1,p)%val,                               &
             period_report(Salt_2,p)%val,                               &
             period_report(Salt_3,p)%val,                               &
             period_report(Salt_4,p)%val,                               &
             period_report(Susp_1,p)%val,                               &
             period_report(Susp_2,p)%val,                               &
             period_report(Susp_3,p)%val,                               &
             period_report(Susp_4,p)%val,                               &
             period_report(PM10_1,p)%val,                               &
             period_report(PM10_2,p)%val,                               &
             period_report(PM10_3,p)%val,                               &
             period_report(PM10_4,p)%val
       write (UNIT=52,FMT="(3(f10.4,' '))",ADVANCE="NO")                &
             period_report(Precipi,p)%val,                              &
             period_report(Wind_energy,p)%val,                          &
             period_report(Dryness_ratio,p)%val

       write (UNIT=52,FMT="(3(f10.4,' '))",ADVANCE="NO")                &
             period_report(Crop_canopy_cov,p)%val,                      &
             period_report(Crop_stand_sil,p)%val,                       &
             period_report(Crop_stand_mass,p)%val
       write (UNIT=52,FMT="(4(f10.4,' '))",ADVANCE="NO")                &
             period_report(Res_flat_cov,p)%val,                         &
             period_report(Res_stand_sil,p)%val,                        &
             period_report(Res_flat_mass,p)%val,                        &
             period_report(Res_stand_mass,p)%val
       write (UNIT=52,FMT="(4(f10.4,' '))",ADVANCE="NO")                &
             period_report(All_flat_cov,p)%val,                         &
             period_report(All_stand_sil,p)%val,                        &
             period_report(All_flat_mass,p)%val,                        &
             period_report(All_stand_mass,p)%val

       write (UNIT=52,FMT="(4(f10.4,' '))",ADVANCE="YES")               &
             period_report(Ridge_dir,p)%val,                            &
             period_report(Ridge_ht,p)%val,                             &
             period_report(Ridge_sp,p)%val,                             &
             period_report(Random_rough,p)%val
    END DO

    ! Rotation year output
    write (UNIT=52,FMT="(1(A))",ADVANCE="YES") '# Rotation year output'
    DO y = 1, nrot_years
       write (UNIT=52,FMT="(1('      ',i2,' '))",ADVANCE="NO")          &
             yrly_dates(y)%sy
       write (UNIT=52,FMT="(1('      ',i2,' '))",ADVANCE="NO")          &
             yrly_dates(y)%ey
       write (UNIT=52,FMT="(1x,A80,' ')",ADVANCE="NO") "" !skip op field
       write (UNIT=52,FMT="(1x,A80,' ')",ADVANCE="NO") "" !skip crop field
       write (UNIT=52,FMT="(17(f10.4,' '))",ADVANCE="NO")               &
             yrly_report(Eros_loss,y)%val,                              &
             yrly_report(Salt_loss,y)%val,                              &
             yrly_report(Susp_loss,y)%val,                              &
             yrly_report(PM10_loss,y)%val,                              &
             yrly_report(Salt_1,y)%val,                                 &
             yrly_report(Salt_2,y)%val,                                 &
             yrly_report(Salt_3,y)%val,                                 &
             yrly_report(Salt_4,y)%val,                                 &
             yrly_report(Susp_1,y)%val,                                 &
             yrly_report(Susp_2,y)%val,                                 &
             yrly_report(Susp_3,y)%val,                                 &
             yrly_report(Susp_4,y)%val,                                 &
             yrly_report(PM10_1,y)%val,                                 &
             yrly_report(PM10_2,y)%val,                                 &
             yrly_report(PM10_3,y)%val,                                 &
             yrly_report(PM10_4,y)%val

       write (UNIT=52,FMT="(3(f10.4,' '))",ADVANCE="NO")                &
             yrly_report(Precipi,y)%val,                                &
             yrly_report(Wind_energy,y)%val,                            &
             yrly_report(Dryness_ratio,y)%val

       DO i = 1, 15 !skip veg/surf fields
         write (UNIT=52,FMT="(A7,'N/A ')",ADVANCE="NO") ""
       END DO
       write (UNIT=52,FMT="(A)",ADVANCE="YES") ""
    END DO


    ! Monthly output
    write (UNIT=52,FMT="(1(A))",ADVANCE="YES") '# Monthly output'
    y = 0
    DO m = 1, 12
       write (UNIT=52,FMT="(1('   ',i2,'    '))",ADVANCE="NO") m
       write (UNIT=52,FMT="(1('   ',i2,'    '))",ADVANCE="NO") m
              
       write (UNIT=52,FMT="(1x,A80,' ')",ADVANCE="NO") "" !skip op field
       write (UNIT=52,FMT="(1x,A80,' ')",ADVANCE="NO") "" !skip crop field

       write (UNIT=52,FMT="(17(f10.4,' '))",ADVANCE="NO")               &
             monthly_report(Eros_loss,m,y)%val,                         &
             monthly_report(Salt_loss,m,y)%val,                         &
             monthly_report(Susp_loss,m,y)%val,                         &
             monthly_report(PM10_loss,m,y)%val,                         &
             monthly_report(Salt_1,m,y)%val,                            &
             monthly_report(Salt_2,m,y)%val,                            &
             monthly_report(Salt_3,m,y)%val,                            &
             monthly_report(Salt_4,m,y)%val,                            &
             monthly_report(Susp_1,m,y)%val,                            &
             monthly_report(Susp_2,m,y)%val,                            &
             monthly_report(Susp_3,m,y)%val,                            &
             monthly_report(Susp_4,m,y)%val,                            &
             monthly_report(PM10_1,m,y)%val,                            &
             monthly_report(PM10_2,m,y)%val,                            &
             monthly_report(PM10_3,m,y)%val,                            &
             monthly_report(PM10_4,m,y)%val

       write (UNIT=52,FMT="(3(f10.4,' '))",ADVANCE="NO")                &
             monthly_report(Precipi,y,m)%val,                           &
             monthly_report(Wind_energy,y,m)%val,                       &
             monthly_report(Dryness_ratio,y,m)%val

       DO i = 1, 15 !skip veg/surf fields
          write (UNIT=52,FMT="(A7,'N/A ')",ADVANCE="NO") ""
        END DO
        write (UNIT=52,FMT="(A)",ADVANCE="YES") ""

    END DO

    ! Rotation output (yearly average values)
    write (UNIT=52,FMT="(1(A))",ADVANCE="YES") '# Rotation output (yearly average values)'
    y = 0
    write (UNIT=52,FMT="(2(A))",ADVANCE="NO")                           &
             '*       ',' '
    write (UNIT=52,FMT="(2(A))",ADVANCE="NO")                           &
             '*       ',' '
    write (UNIT=52,FMT="(1x,A80,' ')",ADVANCE="NO") "" !skip op field
    write (UNIT=52,FMT="(1x,A80,' ')",ADVANCE="NO") "" !skip crop field
    write (UNIT=52,FMT="(17(f10.4,' '))",ADVANCE="NO")                  &
          yrly_report(Eros_loss,y)%val,                                 &
          yrly_report(Salt_loss,y)%val,                                 &
          yrly_report(Susp_loss,y)%val,                                 &
          yrly_report(PM10_loss,y)%val,                                 &
          yrly_report(Salt_1,y)%val,                                    &
          yrly_report(Salt_2,y)%val,                                    &
          yrly_report(Salt_3,y)%val,                                    &
          yrly_report(Salt_4,y)%val,                                    &
          yrly_report(Susp_1,y)%val,                                    &
          yrly_report(Susp_2,y)%val,                                    &
          yrly_report(Susp_3,y)%val,                                    &
          yrly_report(Susp_4,y)%val,                                    &
          yrly_report(PM10_1,y)%val,                                    &
          yrly_report(PM10_2,y)%val,                                    &
          yrly_report(PM10_3,y)%val,                                    &
          yrly_report(PM10_4,y)%val

    write (UNIT=52,FMT="(3(f10.4,' '))",ADVANCE="NO")                   &
          yrly_report(Precipi,y)%val,                                   &
          yrly_report(Wind_energy,y)%val,                               &
          yrly_report(Dryness_ratio,y)%val

    DO i = 1, 15 !skip veg/surf fields
      write (UNIT=52,FMT="(A7,'N/A ')",ADVANCE="NO") ""
    END DO
    write (UNIT=52,FMT="(A)",ADVANCE="YES") ""

END SUBROUTINE print_nui_output

