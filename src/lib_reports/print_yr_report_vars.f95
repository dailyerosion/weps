!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!
SUBROUTINE print_yr_report_vars(nperiods, nrot_yrs, ncycles)

    USE pd_dates_vars
    USE pd_update_vars
    USE pd_report_vars

    USE pd_var_tables

    USE mandate_vars

    IMPLICIT NONE

    INTEGER, INTENT (IN) :: nperiods
    INTEGER, INTENT (IN) :: nrot_yrs
    INTEGER, INTENT (IN) :: ncycles
    INTEGER :: i,p,hm,m,y               ! local loop variables

    print *,""
    print *, "  yr        Precip       WE       Dry Ratio   Snow Cover"
    print *, "           val  cnt   val  cnt     val  cnt     val  cnt"
    print *, "--------------------------------------------------------"
    do y = 1, nrot_yrs*ncycles
!         write (UNIT=6,FMT="(i5,4(f10.2,i3))",ADVANCE="YES")    &
          write (UNIT=6,FMT="(i5,4(f10.2,i3))",ADVANCE="NO")     &
                      y,                                         &
                      yr_report(Precipi,y)%val,                &
                      yr_report(Precipi,y)%cnt,                &
                      yr_report(Wind_energy,y)%val,            &
                      yr_report(Wind_energy,y)%cnt,            &
                      yr_report(Dryness_ratio,y)%val,          &
                      yr_report(Dryness_ratio,y)%cnt,          &
                      yr_report(Snow_cover,y)%val,             &
                      yr_report(Snow_cover,y)%cnt

          write (UNIT=6,FMT="(24(i3))",ADVANCE="YES")            &
                      yr_report(Precipi,y)%date,               &
                      yr_report(Wind_energy,y)%date,           &
                      yr_report(Dryness_ratio,y)%date,         &
                      yr_report(Snow_cover,y)%date

    end do
    print *, "--------------------------------------------------------"
    print *,""

    print *,""
    write (UNIT=6,FMT="(A)",ADVANCE="NO") &
          "  yr   Erosion     Salt     Susp     PM10   "
    write (UNIT=6,FMT="(3(A))",ADVANCE="YES") &
          "Salt_1   Salt_2   Salt_3   Salt_4   ", &
          "Susp_1   Susp_2   Susp_3   Susp_4   ", &
          "PM10_1   PM10_2   PM10_3   PM10_4"
    write (UNIT=6,FMT="(4(A))",ADVANCE="YES") &
          "--------------------------------------------", &
          "------------------------------------",         &
          "------------------------------------",         &
          "---------------------------------"

    do y = 1, nrot_yrs*ncycles
          write (UNIT=6,FMT="(i5)",ADVANCE="NO") y
          write (UNIT=6,FMT="(16(f9.2))",ADVANCE="YES")                 &
                      yr_report(Eros_loss,y)%val,                     &
                      yr_report(Salt_loss,y)%val,                     &
                      yr_report(Susp_loss,y)%val,                     &
                      yr_report(PM10_loss,y)%val,                     &
                      yr_report(Salt_1,y)%val,                        &
                      yr_report(Salt_2,y)%val,                        &
                      yr_report(Salt_3,y)%val,                        &
                      yr_report(Salt_4,y)%val,                        &
                      yr_report(Susp_1,y)%val,                        &
                      yr_report(Susp_2,y)%val,                        &
                      yr_report(Susp_3,y)%val,                        &
                      yr_report(Susp_4,y)%val,                        &
                      yr_report(PM10_1,y)%val,                        &
                      yr_report(PM10_2,y)%val,                        &
                      yr_report(PM10_3,y)%val,                        &
                      yr_report(PM10_4,y)%val

    end do
    write (UNIT=6,FMT="(4(A))",ADVANCE="YES") &
          "--------------------------------------------", &
          "------------------------------------",         &
          "------------------------------------",         &
          "---------------------------------"
    print *,""

    print *,""
    write (UNIT=6,FMT="(2(A))",ADVANCE="YES") &
          " rot   Salt_loss  Salt_loss   Salt_loss  Area      Fract   ", &
          "Salt_dep   Salt_dep   Salt_dep  Area      Fract   Trans Cap   Area      Fract    Shelt     Shelt" 
    write (UNIT=6,FMT="(2(A))",ADVANCE="YES") &
          "  yr   loss area  field area    (kg)     (ha)              ", &
          "dep area  field area    (kg)    (ha)                          (ha)              Area(ha)   Fract" 
    write (UNIT=6,FMT="(2(A))",ADVANCE="YES") &
          "-------------------------------------------------------------", &
          "------------------------------------------------------------------------------------------------"

    do y = 1, nrot_yrs*ncycles
          write (UNIT=6,FMT="(i5)",ADVANCE="NO") y
          write (UNIT=6,FMT="(2(f10.4),f12.0,2(f10.4))",ADVANCE="NO")  &
                      yr_report(Salt_loss2_rate,y)%val,              &
                      yr_report(Salt_loss2,y)%val,                   &
                      yr_report(Salt_loss2_mass,y)%val,              &
                      yr_report(Salt_loss2_area,y)%val,              &
                      yr_report(Salt_loss2_frac,y)%val
          write (UNIT=6,FMT="(2(f10.4),f10.0,2(f10.4))",ADVANCE="NO")  &
                      yr_report(Salt_dep2_rate,y)%val,               &
                      yr_report(Salt_dep2,y)%val,                    &
                      yr_report(Salt_dep2_mass,y)%val,               &
                      yr_report(Salt_dep2_area,y)%val,               &
                      yr_report(Salt_dep2_frac,y)%val
          write (UNIT=6,FMT="(5(f10.4))",ADVANCE="YES")                &
                      yr_report(Trans_cap,y)%val,                    &
                      yr_report(Trans_cap_area,y)%val,               &
                      yr_report(Trans_cap_frac,y)%val,               &
                      yr_report(Sheltered_area,y)%val,               &
                      yr_report(Sheltered_frac,y)%val

    end do
    write (UNIT=6,FMT="(2(A))",ADVANCE="YES") &
          "-----------------------------------------------------------", &
          "------------------------------------------------------------------------------------------------"
    print *,""





END SUBROUTINE print_yr_report_vars
