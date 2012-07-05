!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!
SUBROUTINE print_report_vars(nperiods, nrot_yrs)

    USE pd_dates_vars
    USE pd_update_vars
    USE pd_report_vars

    USE pd_var_tables

    USE mandate_vars

    IMPLICIT NONE

    INTEGER, INTENT (IN) :: nperiods
    INTEGER, INTENT (IN) :: nrot_yrs
    INTEGER :: i,p,hm,m,y               ! local loop variables

    print *,""
    print *, "  yr        Precip       WE       Dry Ratio   Snow Cover"
    print *, "           val  cnt   val  cnt     val  cnt     val  cnt"
    print *, "--------------------------------------------------------"
    do y = 0, nrot_yrs
!         write (UNIT=6,FMT="(i5,4(f10.2,i3))",ADVANCE="YES")    &
          write (UNIT=6,FMT="(i5,4(f10.2,i3))",ADVANCE="NO")     &
                      y,                                         &
                      yrly_report(Precipi,y)%val,                &
                      yrly_report(Precipi,y)%cnt,                &
                      yrly_report(Wind_energy,y)%val,            &
                      yrly_report(Wind_energy,y)%cnt,            &
                      yrly_report(Dryness_ratio,y)%val,          &
                      yrly_report(Dryness_ratio,y)%cnt,          &
                      yrly_report(Snow_cover,y)%val,             &
                      yrly_report(Snow_cover,y)%cnt

          write (UNIT=6,FMT="(24(i3))",ADVANCE="YES")            &
                      yrly_report(Precipi,y)%date,               &
                      yrly_report(Wind_energy,y)%date,           &
                      yrly_report(Dryness_ratio,y)%date,         &
                      yrly_report(Snow_cover,y)%date

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

    do y = 0, nrot_yrs
          write (UNIT=6,FMT="(i5)",ADVANCE="NO") y
          write (UNIT=6,FMT="(16(f9.2))",ADVANCE="YES")                 &
                      yrly_report(Eros_loss,y)%val,                     &
                      yrly_report(Salt_loss,y)%val,                     &
                      yrly_report(Susp_loss,y)%val,                     &
                      yrly_report(PM10_loss,y)%val,                     &
                      yrly_report(Salt_1,y)%val,                        &
                      yrly_report(Salt_2,y)%val,                        &
                      yrly_report(Salt_3,y)%val,                        &
                      yrly_report(Salt_4,y)%val,                        &
                      yrly_report(Susp_1,y)%val,                        &
                      yrly_report(Susp_2,y)%val,                        &
                      yrly_report(Susp_3,y)%val,                        &
                      yrly_report(Susp_4,y)%val,                        &
                      yrly_report(PM10_1,y)%val,                        &
                      yrly_report(PM10_2,y)%val,                        &
                      yrly_report(PM10_3,y)%val,                        &
                      yrly_report(PM10_4,y)%val

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

    do y = 0, nrot_yrs
          write (UNIT=6,FMT="(i5)",ADVANCE="NO") y
          write (UNIT=6,FMT="(2(f10.4),f12.0,2(f10.4))",ADVANCE="NO")  &
                      yrly_report(Salt_loss2_rate,y)%val,              &
                      yrly_report(Salt_loss2,y)%val,                   &
                      yrly_report(Salt_loss2_mass,y)%val,              &
                      yrly_report(Salt_loss2_area,y)%val,              &
                      yrly_report(Salt_loss2_frac,y)%val
          write (UNIT=6,FMT="(2(f10.4),f10.0,2(f10.4))",ADVANCE="NO")  &
                      yrly_report(Salt_dep2_rate,y)%val,               &
                      yrly_report(Salt_dep2,y)%val,                    &
                      yrly_report(Salt_dep2_mass,y)%val,               &
                      yrly_report(Salt_dep2_area,y)%val,               &
                      yrly_report(Salt_dep2_frac,y)%val
          write (UNIT=6,FMT="(5(f10.4))",ADVANCE="YES")                &
                      yrly_report(Trans_cap,y)%val,                    &
                      yrly_report(Trans_cap_area,y)%val,               &
                      yrly_report(Trans_cap_frac,y)%val,               &
                      yrly_report(Sheltered_area,y)%val,               &
                      yrly_report(Sheltered_frac,y)%val

    end do
    write (UNIT=6,FMT="(2(A))",ADVANCE="YES") &
          "-----------------------------------------------------------", &
          "------------------------------------------------------------------------------------------------"
    print *,""


! monthly period stuff
    print *,""
    print *, "  yr  mn      Precip        WE       Dry Ratio   Snow Cover"
    print *, "             val  cnt    val  cnt     val  cnt     val  cnt"
    print *, "-----------------------------------------------------------"
    do y = 0, nrot_yrs
       do m = 1, 12
!         do i = Min_cli_vars, Max_cli_vars
!         write (UNIT=6,FMT="(i5,i3,4(f10.2,i3))",ADVANCE="YES")        &
          write (UNIT=6,FMT="(i5,i3,4(f10.2,i3))",ADVANCE="NO")         &
                         y, m,                                          &
                         monthly_report(Precipi,m,y)%val,               &
                         monthly_report(Precipi,m,y)%cnt,               &
                         monthly_report(Wind_energy,m,y)%val,           &
                         monthly_report(Wind_energy,m,y)%cnt,           &
                         monthly_report(Dryness_ratio,m,y)%val,         &
                         monthly_report(Dryness_ratio,m,y)%cnt,         &
                         monthly_report(Snow_cover,m,y)%val,            &
                         monthly_report(Snow_cover,m,y)%cnt

          write (UNIT=6,FMT="(24(i3))",ADVANCE="YES")                   &
                      monthly_report(Precipi,m,y)%date,                 &
                      monthly_report(Wind_energy,m,y)%date,             &
                      monthly_report(Dryness_ratio,m,y)%date,           &
                      monthly_report(Snow_cover,m,y)%date
          end do
!      end do
    end do
    print *, "-----------------------------------------------------------"
    print *, ""

    print *,""
    write (UNIT=6,FMT="(A)",ADVANCE="NO") &
          " yr mn  Erosion     Salt     Susp     PM10   " 
    write (UNIT=6,FMT="(3(A))",ADVANCE="YES") &
          "Salt_1   Salt_2   Salt_3   Salt_4   ", &
          "Susp_1   Susp_2   Susp_3   Susp_4   ", &
          "PM10_1   PM10_2   PM10_3   PM10_4"
    write (UNIT=6,FMT="(5(A))",ADVANCE="YES") &
          " --------------------------------------------", &
          "------------------------------------",          &
          "------------------------------------",          &
          "---------------------------------"

    do y = 0, nrot_yrs
       do m = 1, 12
          write (UNIT=6,FMT="(i3,i3)",ADVANCE="NO") y,m
          write (UNIT=6,FMT="(16(f9.2))",ADVANCE="YES")                &
                      monthly_report(Eros_loss,m,y)%val,               &
                      monthly_report(Salt_loss,m,y)%val,               &
                      monthly_report(Susp_loss,m,y)%val,               &
                      monthly_report(PM10_loss,m,y)%val,               &
                      monthly_report(Salt_1,m,y)%val,                  &
                      monthly_report(Salt_2,m,y)%val,                  &
                      monthly_report(Salt_3,m,y)%val,                  &
                      monthly_report(Salt_4,m,y)%val,                  &
                      monthly_report(Susp_1,m,y)%val,                  &
                      monthly_report(Susp_2,m,y)%val,                  &
                      monthly_report(Susp_3,m,y)%val,                  &
                      monthly_report(Susp_4,m,y)%val,                  &
                      monthly_report(PM10_1,m,y)%val,                  &
                      monthly_report(PM10_2,m,y)%val,                  &
                      monthly_report(PM10_3,m,y)%val,                  &
                      monthly_report(PM10_4,m,y)%val

       end do
    end do
    write (UNIT=6,FMT="(5(A))",ADVANCE="YES") &
          " --------------------------------------------", &
          "------------------------------------",          &
          "------------------------------------",          &
          "---------------------------------"
    print *,""

    print *,""
    write (UNIT=6,FMT="(2(A))",ADVANCE="YES") &
          "  rot  Salt_loss  Salt_loss   Salt_loss  Area      Fract   ", &
          "Salt_dep   Salt_dep   Salt_dep  Area      Fract   Trans Cap   Area      Fract    Shelt     Shelt" 
    write (UNIT=6,FMT="(2(A))",ADVANCE="YES") &
          " yr mn loss area  field area    (kg)     (ha)              ", &
          "dep area  field area    (kg)    (ha)                          (ha)              Area(ha)   Fract" 
    write (UNIT=6,FMT="(2(A))",ADVANCE="YES") &
          "-----------------------------------------------------------", &
          "------------------------------------------------------------------------------------------------"

    do y = 0, nrot_yrs
       do m = 1, 12
          write (UNIT=6,FMT="(i3,i3)",ADVANCE="NO") y,m
          write (UNIT=6,FMT="(2(f10.4),f10.0,2(f10.4))",ADVANCE="NO")  &
                      monthly_report(Salt_loss2_rate,m,y)%val,         &
                      monthly_report(Salt_loss2,m,y)%val,              &
                      monthly_report(Salt_loss2_mass,m,y)%val,         &
                      monthly_report(Salt_loss2_area,m,y)%val,         &
                      monthly_report(Salt_loss2_frac,m,y)%val
          write (UNIT=6,FMT="(2(f10.4),f10.0,2(f10.4))",ADVANCE="NO")  &
                      monthly_report(Salt_dep2_rate,m,y)%val,          &
                      monthly_report(Salt_dep2,m,y)%val,               &
                      monthly_report(Salt_dep2_mass,m,y)%val,          &
                      monthly_report(Salt_dep2_area,m,y)%val,          &
                      monthly_report(Salt_dep2_frac,m,y)%val
          write (UNIT=6,FMT="(5(f10.4))",ADVANCE="YES")                &
                      monthly_report(Trans_cap,m,y)%val,               &
                      monthly_report(Trans_cap_area,m,y)%val,          &
                      monthly_report(Trans_cap_frac,m,y)%val,          &
                      monthly_report(Sheltered_area,m,y)%val,          &
                      monthly_report(Sheltered_frac,m,y)%val
       end do
    end do
    write (UNIT=6,FMT="(2(A))",ADVANCE="YES") &
          "-----------------------------------------------------------", &
          "------------------------------------------------------------------------------------------------"
    print *,""

! half month period stuff
    print *,""
    print *, "rot   hm       Precip       WE       Dry Ratio   Snow Cover"
    print *, " yr   pd      val  cnt   val  cnt     val  cnt     val  cnt"
    print *, "-----------------------------------------------------------"
    do y = 0, nrot_yrs
       do hm = 1, 24
          write (UNIT=6,FMT="(i4, i5,4(f10.2,i3))",ADVANCE="NO") &
                      y, hm,                                     &
                      hmonth_report(Precipi,hm,y)%val,           &
                      hmonth_report(Precipi,hm,y)%cnt,           &
                      hmonth_report(Wind_energy,hm,y)%val,       &
                      hmonth_report(Wind_energy,hm,y)%cnt,       &
                      hmonth_report(Dryness_ratio,hm,y)%val,     &
                      hmonth_report(Dryness_ratio,hm,y)%cnt,     &
                      hmonth_report(Snow_cover,hm,y)%val,        &
                      hmonth_report(Snow_cover,hm,y)%cnt

          write (UNIT=6,FMT="(24(i3))",ADVANCE="YES")            &
                      hmonth_report(Precipi,hm,y)%date,          &
                      hmonth_report(Wind_energy,hm,y)%date,      &
                      hmonth_report(Dryness_ratio,hm,y)%date,    &
                      hmonth_report(Snow_cover,hm,y)%date
       end do
    end do
    print *, "-----------------------------------------------------------"
    print *,""


! period stuff
    print *,""
    print *, "              RR         RH"
    print *, "  pd       val  cnt   val  cnt"
    print *, "------------------------------"
    do p = 1, nperiods
!         write (UNIT=6,FMT="(i5,2(f10.2,i3))",ADVANCE="YES")    &
          write (UNIT=6,FMT="(i5,2(f10.2,i3))",ADVANCE="NO")     &
                      p,                                         &
                      period_report(Random_rough,p)%val,         &
                      period_report(Random_rough,p)%cnt,         &
                      period_report(Ridge_ht,p)%val,             &
                      period_report(Ridge_ht,p)%cnt

          write (UNIT=6,FMT="(4(i5,i3,i3))",ADVANCE="YES")       &
                      period_report(Random_rough,p)%date,        &
                      period_report(Ridge_ht,p)%date

    end do
    print *, "------------------------------"
    print *,""

! all period stuff
    print *,""
    write (UNIT=6,FMT="(A)",ADVANCE="NO")   &
              " pd      RR       RH        RS       RD      "
    write (UNIT=6,FMT="(A)",ADVANCE="NO")   &
              "Ccan    Cst      Cstm     Rcov      Rst     Rflm     Rstm    "
    write (UNIT=6,FMT="(A)",ADVANCE="NO")   &
              "Acov     Ast      Aflm      Astm     "
    write (UNIT=6,FMT="(A)",ADVANCE="NO")   &
              "Erod fr   AS     AG_den     Ag_coeff  "
    write (UNIT=6,FMT="(A)",ADVANCE="YES")   &
              "Crust     Cr_AS  Cr_LM      Cr_th   Cr_den   Cr_LF   Cr_coeff"
    print *, "------------------------------"

    DO p = 1, nperiods
       write (UNIT=6,FMT="(i4)",ADVANCE="NO") p 
       DO i = Min_eop_vars, Max_eop_vars
         write (UNIT=6,FMT="(f9.2)",ADVANCE="NO")  period_report(i,p)%val
       END DO
       write (UNIT=6,FMT="(A)",ADVANCE="YES") ""
    END DO
    print *, "------------------------------"

    print *,""
    write (UNIT=6,FMT="(A)",ADVANCE="NO") &
          "  pd   Erosion     Salt     Susp     PM10   " 
    write (UNIT=6,FMT="(3(A))",ADVANCE="YES") &
          "Salt_1   Salt_2   Salt_3   Salt_4   ", &
          "Susp_1   Susp_2   Susp_3   Susp_4   ", &
          "PM10_1   PM10_2   PM10_3   PM10_4"
    write (UNIT=6,FMT="(5(A))",ADVANCE="YES") &
          "--------------------------------------------", &
          "------------------------------------",         &
          "------------------------------------",         &
          "---------------------------------"

    do p = 1, nperiods
          write (UNIT=6,FMT="(i5)",ADVANCE="NO") p
          write (UNIT=6,FMT="(16(f9.2))",ADVANCE="YES")                &
                      period_report(Eros_loss,p)%val,                     &
                      period_report(Salt_loss,p)%val,                     &
                      period_report(Susp_loss,p)%val,                     &
                      period_report(PM10_loss,p)%val,                     &
                      period_report(Salt_1,p)%val,                        &
                      period_report(Salt_2,p)%val,                        &
                      period_report(Salt_3,p)%val,                        &
                      period_report(Salt_4,p)%val,                        &
                      period_report(Susp_1,p)%val,                        &
                      period_report(Susp_2,p)%val,                        &
                      period_report(Susp_3,p)%val,                        &
                      period_report(Susp_4,p)%val,                        &
                      period_report(PM10_1,p)%val,                        &
                      period_report(PM10_2,p)%val,                        &
                      period_report(PM10_3,p)%val,                        &
                      period_report(PM10_4,p)%val

    end do
    write (UNIT=6,FMT="(5(A))",ADVANCE="YES") &
          "--------------------------------------------", &
          "------------------------------------",         &
          "------------------------------------",         &
          "---------------------------------"
    print *,""

    print *,""
!   write (UNIT=6,FMT="(A)",ADVANCE="NO") &
!         " pd  Salt_loss    Area      Fract   Salt_dep  Area    Fract   "
!   write (UNIT=6,FMT="(A)",ADVANCE="YES") &
!         "Trans Cap  Area     Fract    PArea    PFract" 
!   write (UNIT=6,FMT="(1(A))",ADVANCE="NO") &
!         "--------------------------------------------------------------"
!   write (UNIT=6,FMT="(1(A))",ADVANCE="YES") &
!         "--------------------------------------------"
    write (UNIT=6,FMT="(2(A))",ADVANCE="YES") &
          " pd  Salt_loss  Salt_loss   Salt_loss  Area      Fract   ", &
          "Salt_dep   Salt_dep   Salt_dep  Area      Fract   Trans Cap   Area      Fract    Shelt     Shelt" 
    write (UNIT=6,FMT="(2(A))",ADVANCE="YES") &
          "     loss area  field area    (kg)     (ha)              ", &
          "dep area  field area    (kg)    (ha)                          (ha)              Area(ha)   Fract" 
    write (UNIT=6,FMT="(2(A))",ADVANCE="YES") &
          "---------------------------------------------------------", &
          "------------------------------------------------------------------------------------------------"
    do p = 1, nperiods
          write (UNIT=6,FMT="(i5)",ADVANCE="NO") p
          write (UNIT=6,FMT="(2(f10.4),f10.0,2(f10.4))",ADVANCE="NO")     &
                      period_report(Salt_loss2_rate,p)%val,               &
                      period_report(Salt_loss2,p)%val,                    &
                      period_report(Salt_loss2_mass,p)%val,               &
                      period_report(Salt_loss2_area,p)%val,               &
                      period_report(Salt_loss2_frac,p)%val
          write (UNIT=6,FMT="(2(f10.4),f10.0,2(f10.4))",ADVANCE="NO")     &
                      period_report(Salt_dep2_rate,p)%val,                &
                      period_report(Salt_dep2,p)%val,                     &
                      period_report(Salt_dep2_mass,p)%val,                &
                      period_report(Salt_dep2_area,p)%val,                &
                      period_report(Salt_dep2_frac,p)%val
          write (UNIT=6,FMT="(5(f10.4))",ADVANCE="YES")                   &
                      period_report(Trans_cap,p)%val,                     &
                      period_report(Trans_cap_area,p)%val,                &
                      period_report(Trans_cap_frac,p)%val,                &
                      period_report(Sheltered_area,p)%val,                &
                      period_report(Sheltered_frac,p)%val
    end do
    write (UNIT=6,FMT="(2(A))",ADVANCE="YES") &
          "---------------------------------------------------------", &
          "------------------------------------------------------------------------------------------------"

    DO i = 1, size(mandate)
       WRITE (UNIT=6, FMT="(2(i2,'/'),i2,' ',A80,A,A80)", ADVANCE="YES") &
           mandate(i)%d, mandate(i)%m, mandate(i)%y,                       &
!           trim(mandate(i)%opname), ' | ', trim(mandate(i)%cropname)
           mandate(i)%opname, ' | ', mandate(i)%cropname
    END DO
    write (UNIT=6,FMT="(1(A))",ADVANCE="YES") &
          "--------------------------------------------"
END SUBROUTINE print_report_vars
