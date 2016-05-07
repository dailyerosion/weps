!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!
MODULE alloc_pd_vars_func

contains
  FUNCTION alloc_pd_vars (nperiods, nrot_yrs, ncycles, rep_report, rep_update)

    USE pd_var_type_def
    USE pd_dates_vars
    USE pd_update_vars
    USE pd_report_vars

    USE pd_var_tables

    IMPLICIT NONE

    INCLUDE 'command.inc'

    INTEGER :: alloc_pd_vars            ! Allocate status return
    INTEGER, INTENT (IN) :: nperiods    ! Number of total periods
    INTEGER, INTENT (IN) :: nrot_yrs    ! Number of rotation years
    INTEGER, INTENT (IN) :: ncycles    ! number of rotation cycles
    type(reporting_report), intent(inout) :: rep_report
    type(reporting_update), intent(inout) :: rep_update

    INTRINSIC ASSOCIATED                ! use to verify status of pointers

    INTEGER :: i, p, hm, m, y           ! Local loop variables

    INTEGER :: N_yrly, N_monthly        ! Local variables
    INTEGER :: N_hmonth, N_period       ! Local variables

    INTEGER :: alloc_status = 0         ! Local allocate status return
    INTEGER :: ret_status = 0           ! Local allocate status return

    alloc_pd_vars = 0                   ! Init return value to zero


! Allocate for "update" variables
    IF (ALLOCATED (rep_update%yrly_update) .neqv. .TRUE.) then
        ALLOCATE (rep_update%yrly_update(Min_yrly_vars:Max_yrly_vars), STAT = alloc_status) 
        IF (alloc_status /= 0) THEN
           print *, "Error allocating rep_update%yrly_update(Max_yrly_vars)"
           ret_status = ret_status + alloc_status
        END IF
    END IF

    IF (ALLOCATED (rep_update%monthly_update) .neqv. .TRUE.) then
        ALLOCATE (rep_update%monthly_update(Min_monthly_vars:Max_monthly_vars), STAT = alloc_status) 
        IF (alloc_status /= 0) THEN
            print *, "Error allocating rep_update%monthly_update(Min_monthly_vars:Max_monthly_vars)"
            ret_status = ret_status + alloc_status
        END IF
    END IF

    IF (ALLOCATED (rep_update%hmonth_update) .neqv. .TRUE.) then
        ALLOCATE (rep_update%hmonth_update(Min_hmonth_vars:Max_hmonth_vars), STAT = alloc_status) 
        IF (alloc_status /= 0) THEN
            print *, "Error allocating rep_update%hmonth_update(Min_hmonth_vars:Max_hmonth_vars)"
            ret_status = ret_status + alloc_status
        END IF
    END IF

    IF (ALLOCATED (rep_update%period_update) .neqv. .TRUE.) then
        ALLOCATE (rep_update%period_update(Min_period_vars:Max_period_vars), STAT = alloc_status) 
        IF (alloc_status /= 0) THEN
            print *, "Error allocating rep_update%period_update(Min_period_vars:Max_period_vars)"
            ret_status = ret_status + alloc_status
        END IF
    END IF


    IF (ALLOCATED (rep_update%yrot_update) .neqv. .TRUE.) then
        ALLOCATE (rep_update%yrot_update(Min_yrly_vars:Max_yrly_vars), STAT = alloc_status) 
        IF (alloc_status /= 0) THEN
            print *, "Error allocating rep_update%yrot_update(Max_yrly_vars)"
            ret_status = ret_status + alloc_status
        END IF
    END IF

    IF (ALLOCATED (rep_update%mrot_update) .neqv. .TRUE.) then
        ALLOCATE (rep_update%mrot_update(Min_monthly_vars:Max_monthly_vars,12), STAT = alloc_status) 
        IF (alloc_status /= 0) THEN
            print *, "Error allocating rep_update%mrot_update(Min_monthly_vars:Max_monthly_vars,12)"
            ret_status = ret_status + alloc_status
        END IF
    END IF

    IF (ALLOCATED (rep_update%hmrot_update) .neqv. .TRUE.) then
        ALLOCATE (rep_update%hmrot_update(Min_hmonth_vars:Max_hmonth_vars,24), STAT = alloc_status) 
        IF (alloc_status /= 0) THEN
            print *, "Error allocating rep_update%hmrot_update(Min_hmonth_vars:Max_hmonth_vars,24)"
            ret_status = ret_status + alloc_status
        END IF
    END IF

    ! For a year by year report of yearly (and rotation year) averaged variables
    IF (ALLOCATED (rep_update%yr_update) .neqv. .TRUE.) then
        ALLOCATE (rep_update%yr_update(Min_yrly_vars:Max_yrly_vars), STAT = alloc_status) 
        IF (alloc_status /= 0) THEN
            print *, "Error allocating rep_update%yr_update(Max_yr_vars)"
            ret_status = ret_status + alloc_status
        END IF
    END IF


! Allocate for all "report" variables
    IF (.not. ALLOCATED (rep_report%yrly_report)) then
        ALLOCATE (rep_report%yrly_report(Min_yrly_vars:Max_yrly_vars,0:nrot_yrs), STAT = alloc_status) 
        IF (alloc_status /= 0) THEN
            print *, "Error allocating rep_report%yrly_report(Min_yrly_vars:Max_yrly_vars,0:nrot_yrs)"
            ret_status = ret_status + alloc_status
        END IF
    END IF
    IF (.not. ALLOCATED (rep_report%monthly_report)) then
        ALLOCATE (rep_report%monthly_report(Min_monthly_vars:Max_monthly_vars,1:12,0:nrot_yrs), STAT = alloc_status) 
        IF (alloc_status /= 0) THEN
            print *, "Error allocating rep_report%monthly_report(Min_monthly_vars:Max_monthly_vars,1:12,0:nrot_yrs)"
            ret_status = ret_status + alloc_status
        END IF
    END IF
    IF (.not. ALLOCATED (rep_report%hmonth_report)) then
        ALLOCATE (rep_report%hmonth_report(Min_hmonth_vars:Max_hmonth_vars,1:24,0:nrot_yrs), STAT = alloc_status) 
        IF (alloc_status /= 0) THEN
            print *, "Error allocating rep_report%hmonth_report(Min_hmonth_vars:Max_hmonth_vars,1:24,0:nrot_yrs)"
            ret_status = ret_status + alloc_status
        END IF
    END IF

    IF (.not. ALLOCATED (rep_report%period_report)) then
        ALLOCATE (rep_report%period_report(Min_period_vars:Max_period_vars,1:nperiods), STAT = alloc_status) 
        IF (alloc_status /= 0) THEN
            print *, "Error allocating rep_report%period_report(Min_period_vars:Max_period_vars,1:nperiods)"
            ret_status = ret_status + alloc_status
        END IF
    END IF

    ! For a year by year report of yearly (and rotation year) averaged variables
    IF (.not. ALLOCATED (rep_report%yr_report)) then
        ALLOCATE (rep_report%yr_report(Min_yrly_vars:Max_yrly_vars,1:nrot_yrs*ncycles), STAT = alloc_status) 
        IF (alloc_status /= 0) THEN
            print *, "Error allocating rep_report%yr_report(Min_yrly_vars:Max_yrly_vars,1:nrot_yrs*ncycles)"
            ret_status = ret_status + alloc_status
        END IF
    END IF

! Allocate for reporting period dates
    IF (.not. ALLOCATED (yrly_dates)) then
        ALLOCATE (yrly_dates(0:nrot_yrs), STAT = alloc_status) 
        IF (alloc_status /= 0) THEN
            print *, "Error allocating yrly_dates(0:nrot_yrs)"
            ret_status = ret_status + alloc_status
        END IF
    END IF
    IF (.not. ALLOCATED (monthly_dates)) then
        ALLOCATE (monthly_dates(1:12,0:nrot_yrs), STAT = alloc_status) 
        IF (alloc_status /= 0) THEN
            print *, "Error allocating monthly_dates(1:12,0:nrot_yrs)"
            ret_status = ret_status + alloc_status
        END IF
    END IF
    IF (.not. ALLOCATED (hmonth_dates)) then
        ALLOCATE (hmonth_dates(1:24,0:nrot_yrs), STAT = alloc_status) 
        IF (alloc_status /= 0) THEN
            print *, "Error allocating hmonth_dates(1:24,0:nrot_yrs)"
            ret_status = ret_status + alloc_status
        END IF
    END IF
    IF (.not. ALLOCATED (period_dates)) then
        ALLOCATE (period_dates(1:nperiods), STAT = alloc_status) 
        IF (alloc_status /= 0) THEN
            print *, "Error allocating period_dates(1:nperiods)"
            ret_status = ret_status + alloc_status
        END IF
    END IF

    ! For a year by year report of yearly (and rotation year) averaged variables
    IF (.not. ALLOCATED (yr_dates)) then
        ALLOCATE (yr_dates(1:nrot_yrs*ncycles), STAT = alloc_status) 
        IF (alloc_status /= 0) THEN
            print *, "Error allocating yr_dates(1:nrot_yrs*ncycles)"
            ret_status = ret_status + alloc_status
        END IF
    END IF


! Associate "dates" pointers in "report" variable structures
    do i=Min_yrly_vars, Max_yrly_vars
      do y=0, nrot_yrs
        rep_report%yrly_report(i,y)%date => yrly_dates(y)
        IF (.not. ASSOCIATED(rep_report%yrly_report(i,y)%date)) THEN
        !IF (.not. ASSOCIATED(rep_report%yrly_report(i,y)%date,yrly_dates(y))) THEN
           write(0,*) "Error: rep_report%yrly_report(",i,y,")%date not ASSOCIATED with yrly_dates(",y,")"
           write(0,*) "Error: rep_report%yrly_report var not associated with a yrly_dates var"
           call exit (1)
        END IF
      end do
    end do

    do i=Min_monthly_vars, Max_monthly_vars
      do m=1, 12 !for each month
        do y=0, nrot_yrs
        rep_report%monthly_report(i,m,y)%date => monthly_dates(m,y)
        IF (.not. ASSOCIATED(rep_report%monthly_report(i,m,y)%date)) THEN
        !IF (.not. ASSOCIATED(rep_report%monthly_report(i,m,y)%date,monthly_dates(m,y))) THEN
           write(0,*) "Error: rep_report%monthly_report(",i,m,y,")%date not ASSOCIATED with monthly_dates(",m,y,")"
           write(0,*) "Error: rep_report%monthly_report var not associated with a monthly_dates var"
           call exit (1)
        END IF
        end do
      end do
    end do

    do i=Min_hmonth_vars, Max_hmonth_vars
      do hm=1, 24
        do y=0, nrot_yrs
          rep_report%hmonth_report(i,hm,y)%date => hmonth_dates(hm,y)
          IF (.not. ASSOCIATED(rep_report%hmonth_report(i,hm,y)%date)) THEN
          !IF (.not. ASSOCIATED(rep_report%hmonth_report(i,hm,y)%date,hmonth_dates(hm,y))) THEN
             write(0,*) "Error: rep_report%hmonth_report(",i,hm,y,")%date not ASSOCIATED with hmonth_dates(",hm,y,")"
             write(0,*) "Error: rep_report%hmonth_report var not associated with a hmonth_dates var"
             call exit (1)
          END IF
        end do
      end do
    end do

    do i=Min_period_vars, Max_period_vars
      do p=1, nperiods
        rep_report%period_report(i,p)%date => period_dates(p)
        IF (.not. ASSOCIATED(rep_report%period_report(i,p)%date)) THEN
        !IF (.not. ASSOCIATED(rep_report%period_report(i,p)%date,period_dates(p))) THEN
           write(0,*) "Error: rep_report%period_report(",i,p,")%date not ASSOCIATED with period_dates(",p,")"
           write(0,*) "Error: rep_report%period_report var not associated with a period_dates var"
           call exit (1)
        END IF
      end do
    end do

    ! For a year by year report of yearly (and rotation year) averaged variables
    do i=Min_yrly_vars, Max_yrly_vars
      do y=1, nrot_yrs*ncycles
        rep_report%yr_report(i,y)%date => yr_dates(y)
        IF (.not. ASSOCIATED(rep_report%yr_report(i,y)%date)) THEN
        !IF (.not. ASSOCIATED(rep_report%yr_report(i,y)%date,yr_dates(y))) THEN
           write(0,*) "Error: rep_report%yr_report(",i,y,")%date not ASSOCIATED with yr_dates(",y,")"
           write(0,*) "Error: rep_report%yr_report var not associated with a yr_dates var"
           call exit (1)
        END IF
      end do
    end do

!Uncomment test code if there is a problem with "dates" being
!ASSOCIATED with the "report" variables
!  print *, "Min_yrly_vars: ", Min_yrly_vars
!  print *, "Max_yrly_vars: ", Max_yrly_vars
!  print *, "Wind_energy: ", Wind_energy
!  do y=0, nrot_yrs
!    IF (ASSOCIATED (rep_report%yrly_report(Wind_energy,y)%date)) THEN
!        print *, "alloc: %date is associated"
!    ELSE
!        print *, "alloc: %date is NOT associated"
!    END IF
!    print *, "alloc: ", y, rep_report%yrly_report(Wind_energy,y)%date
!    write (UNIT=6,FMT="(9(i3))",ADVANCE="YES")y, rep_report%yrly_report(Wind_energy,y)%date
!  end do


! Associate "pd_dates" pointers in "pd_update" structures
! (initially point to first "pd_report_dates" for each var)

    DO i=Min_yrly_vars, Max_yrly_vars
        rep_update%yrly_update(i)%date => yrly_dates(1)         ! Use yrly dates
    END DO
    DO i=Min_monthly_vars, Max_monthly_vars
        rep_update%monthly_update(i)%date => monthly_dates(1,1) ! 1st mon of 1st yr dates
    END DO

    DO i=Min_hmonth_vars, Max_hmonth_vars
        rep_update%hmonth_update(i)%date => hmonth_dates(1,1)   ! 1st half month period
    END DO

    DO i=Min_period_vars, Max_period_vars
        rep_update%period_update(i)%date => period_dates(1)     ! 1st period of rotation
    END DO

    DO i=Min_yrly_vars, Max_yrly_vars
        rep_update%yrot_update(i)%date => yrly_dates(0)         ! Use yrly dates
    END DO
    DO i=Min_monthly_vars, Max_monthly_vars
      DO m = 1,12
        rep_update%mrot_update(i,m)%date => monthly_dates(m,0)    ! 1st mon of 1st yr dates
      END DO
    END DO

    DO i=Min_hmonth_vars, Max_hmonth_vars
      DO hm = 1,24
        rep_update%hmrot_update(i,hm)%date => hmonth_dates(hm,0)    ! 1st half month period
      END DO
    END DO

    ! For a year by year report of yearly (and rotation year) averaged variables
    DO i=Min_yrly_vars, Max_yrly_vars
        rep_update%yr_update(i)%date => yr_dates(1)         ! Use yr dates
    END DO

    if (report_debug >= 1) then   !Validate pd vars allocation sizes

      N_yrly = N_cli_vars+N_eave_vars+N_lave_vars+N_dave_vars+N_tave_vars+N_mave_vars
      if ((Max_yrly_vars-Min_yrly_vars+1) /= N_yrly) then
        print *, "Error in allocated size of yrly vars"
        print *, "Max-Min: ", Max_yrly_vars-Min_yrly_vars+1,"N_yrly: ", N_yrly
      end if
      if (size(rep_update%yrly_update) /= N_yrly) then
        print *, "Error in allocated size of rep_update%yrly_update vars"
      end if
      if (size(rep_update%yrot_update) /= N_yrly) then
        print *, "Error in allocated size of rep_update%yrot_update vars"
      end if
      if (size(rep_report%yrly_report,1) /= N_yrly) then
        print *, "Error in allocated size of rep_report%yrly_report vars"
      end if

      ! For a year by year report of yearly (and rotation year) averaged variables
      if (size(rep_update%yr_update) /= N_yrly) then
        print *, "Error in allocated size of rep_update%yrly_update vars"
      end if
      if (size(rep_report%yr_report,1) /= N_yrly) then
        print *, "Error in allocated size of rep_report%yr_report vars"
      end if

      N_monthly = N_yrly
      if ((Max_monthly_vars-Min_monthly_vars+1) /= N_monthly) then
        print *, "Error in allocated size of monthly vars"
        print *, "Max-Min: ", Max_monthly_vars-Min_monthly_vars+1,"N_monthly: ", N_monthly
      end if
      if (size(rep_update%monthly_update) /= N_monthly) then
        print *, "Error in allocated size of rep_update%monthly_update vars"
      end if
      if (size(rep_update%mrot_update,1) /= N_monthly) then
        print *, "Error in allocated size of rep_update%mrot_update vars"
      end if
      if (size(rep_report%monthly_report,1) /= N_monthly) then
        print *, "Error in allocated size of rep_report%monthly_report vars"
      end if

      N_hmonth = N_cli_vars
      if ((Max_hmonth_vars-Min_hmonth_vars+1) /= N_hmonth) then
        print *, "Error in allocated size of hmonth vars"
        print *, "Max-Min: ", Max_hmonth_vars-Min_hmonth_vars+1,"N_hmonth: ", N_hmonth
      end if
      if (size(rep_update%hmonth_update) /= N_hmonth) then
        print *, "Error in allocated size of rep_update%hmonth_update vars"
      end if
      if (size(rep_update%hmrot_update,1) /= N_hmonth) then
        print *, "Error in allocated size of rep_update%hmrot_update vars"
      end if
      if (size(rep_report%hmonth_report,1) /= N_hmonth) then
        print *, "Error in allocated size of rep_report%hmonth_report vars"
      end if

      N_period = N_eave_vars+N_lave_vars+N_dave_vars+N_tave_vars+N_mave_vars+N_eop_vars
      if ((Max_period_vars-Min_period_vars+1) /= N_period) then
        print *, "Error in allocated size of period vars"
        print *, "Max-Min: ", Max_period_vars-Min_period_vars+1,"N_period: ", N_period
      end if
      if (size(rep_update%period_update) /= N_period) then
        print *, "Error in allocated size of rep_update%period_update vars"
      end if
      if (size(rep_report%period_report,1) /= N_period) then
        print *, "Error in allocated size of rep_report%period_report vars"
      end if
    end if


    alloc_pd_vars = ret_status

  END FUNCTION alloc_pd_vars

END MODULE alloc_pd_vars_func
