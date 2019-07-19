!
!$Author$
!$Date$
!$Revision$
!$HeadURL$

module report_init_mod

  contains

  SUBROUTINE init_report_vars(nperiods, nrot_yrs, ncycles, mandate, rep_report, rep_update, rep_dates)

    use weps_cmdline_parms, only: report_debug
    USE pd_dates_vars
    USE pd_update_vars
    USE pd_report_vars

    USE pd_var_tables

    USE mandate_mod, only: opercrop_date

    USE alloc_pd_vars_func         !defines alloc_pd_vars function

    IMPLICIT NONE

    INTEGER, INTENT (IN) :: nperiods   ! 24 is minimum value per rotation year
    INTEGER, INTENT (IN) :: nrot_yrs   ! Minimum is 1
    INTEGER, INTENT (IN) :: ncycles    ! number of rotation cycles
    type (opercrop_date), dimension(:), intent(in) :: mandate
    type(reporting_report), intent(inout) :: rep_report
    type(reporting_update), intent(inout) :: rep_update
    type(reporting_dates), target, intent(inout) :: rep_dates

    INTEGER :: status = 0
    INTEGER :: i,p,hm,m,y,z        ! local loop variables

    status = alloc_pd_vars(nperiods, nrot_yrs, ncycles, rep_report, rep_update, rep_dates) !Allocate space for all pd variables
    IF (status >= 1) THEN
       write(0,*) "Status of alloc_pd_vars: ", status
       write(0,*) "Error allocating pd_vars"
       call exit (1)
    END IF

    ! Init the period start and stop dates for "report" and "update" variables

    ! yrly dates
    DO y=0, nrot_yrs
        ! set "start" dates
        rep_dates%yrly(y)%sd = 1
        rep_dates%yrly(y)%sm = 1
        IF (y == 0) THEN
            rep_dates%yrly(y)%sy = 1
        ELSE
            rep_dates%yrly(y)%sy = y
        END IF

        ! set "end" dates
        rep_dates%yrly(y)%ed = 31
        rep_dates%yrly(y)%em = 12
        IF (y == 0) THEN
            rep_dates%yrly(y)%ey = nrot_yrs
        ELSE
            rep_dates%yrly(y)%ey = y
        END IF
    END DO

    ! yr dates
    ! For a year by year report of yearly (and rotation year) averaged variables
    DO y=1, nrot_yrs*ncycles
        ! set "start" dates
        rep_dates%yr(y)%sd = 1
        rep_dates%yr(y)%sm = 1
        rep_dates%yr(y)%sy = y

        ! set "end" dates
        rep_dates%yr(y)%ed = 31
        rep_dates%yr(y)%em = 12
        rep_dates%yr(y)%ey = y
    END DO


    ! monthly dates
    DO y=0, nrot_yrs
        DO m=1, 12
            ! set "start" dates
            rep_dates%monthly(m,y)%sd = 1
            rep_dates%monthly(m,y)%sm = m
            IF (y == 0) THEN
               rep_dates%monthly(m,y)%sy = 1
            ELSE
               rep_dates%monthly(m,y)%sy = y
            END IF

            ! set "end" dates
            IF ( (m==1).or.(m==3).or.(m==5).or.(m==7).or.   &
                 (m==8).or.(m==10).or.(m==12) ) THEN
                rep_dates%monthly(m,y)%ed = 31
            ELSE IF ( (m==4).or.(m==6).or.(m==9).or.(m==11) ) THEN
                rep_dates%monthly(m,y)%ed = 30
            ELSE   ! m==2 (Feb)
                rep_dates%monthly(m,y)%ed = 29
            END IF
            rep_dates%monthly(m,y)%em = m 
            IF (y == 0) THEN
               rep_dates%monthly(m,y)%ey = nrot_yrs
            ELSE
               rep_dates%monthly(m,y)%ey = y
            END IF
        END DO
    END DO


    ! half-month dates
    DO y = 0, nrot_yrs
       DO hm = 1, 24
          i = modulo(hm,2)   ! if 1, then 1st half of month
          m = (hm+1)/2       ! determine the month of the half-month period
          !print *, "y/m/hm/i: ",y,m,hm,i
          IF (i == 1) THEN   ! 1st half of each month
             rep_dates%hmonth(hm,y)%sd = 1
             rep_dates%hmonth(hm,y)%sm = m
             IF (y == 0) THEN
                rep_dates%hmonth(hm,y)%sy = 1
             ELSE
                rep_dates%hmonth(hm,y)%sy = y
             END IF

             rep_dates%hmonth(hm,y)%ed = 14
             rep_dates%hmonth(hm,y)%em = m
             IF (y == 0) THEN
                rep_dates%hmonth(hm,y)%ey = nrot_yrs
             ELSE
                rep_dates%hmonth(hm,y)%ey = y
             END IF
          ELSE               ! 2nd half month period
             rep_dates%hmonth(hm,y)%sd = 15
             rep_dates%hmonth(hm,y)%sm = m
             IF (y == 0) THEN
                rep_dates%hmonth(hm,y)%sy = 1
             ELSE
                rep_dates%hmonth(hm,y)%sy = y
             END IF

             IF ( (m==1).or.(m==3).or.(m==5).or.(m==7).or.   &
                   (m==8).or.(m==10).or.(m==12) ) THEN
                rep_dates%hmonth(hm,y)%ed = 31
             ELSE IF ( (m==4).or.(m==6).or.(m==9).or.(m==11) ) THEN
                rep_dates%hmonth(hm,y)%ed = 30
             ELSE                                  ! m==2 (Feb)
                rep_dates%hmonth(hm,y)%ed = 29
             END IF
             rep_dates%hmonth(hm,y)%em = m
             IF (y == 0) THEN
                rep_dates%hmonth(hm,y)%ey = nrot_yrs
             ELSE
                rep_dates%hmonth(hm,y)%ey = y
             END IF
          END IF    
       END DO    
    END DO    


    ! period dates
    i = 0
    DO y = 1, nrot_yrs
      DO m = 1, 12
        ! 1st half month period
        i = i + 1    
        rep_dates%period(i)%sd = 1
        rep_dates%period(i)%sm = m
        rep_dates%period(i)%sy = y

        rep_dates%period(i)%ed = 14
        rep_dates%period(i)%em = m
        rep_dates%period(i)%ey = y

        ! Hmm, this doesn't look like an efficient way to do this
        ! but, if it works that will be fine for now
        ! Get all op dates in first "half" of month
        ! screen out multiple operations on the same day
        do z=1,size(mandate)
          ! Check to see if we have a man operation within a 1st
          ! monthly period not starting on the first day of a month
          ! (e.g. an operation date of day 2-14 of this month)
          if( mandate(z)%d >  1 .AND. mandate(z)%d < 15 .AND. &
              mandate(z)%m == m .AND. mandate(z)%y == y .AND. &
              !The next line checks for additional ops on same date
              mandate(z)%d /= rep_dates%period(i)%sd ) then
            i = i + 1    
            rep_dates%period(i)%sd = mandate(z)%d 
            rep_dates%period(i)%sm = mandate(z)%m
            rep_dates%period(i)%sy = mandate(z)%y

            rep_dates%period(i)%ed = 14
            rep_dates%period(i)%em = m
            rep_dates%period(i)%ey = y

            ! Fix previous period end date (day)
            rep_dates%period(i-1)%ed = rep_dates%period(i)%sd-1
          end if
        end do

        ! 2nd half month period
        i = i + 1
        rep_dates%period(i)%sd = 15
        rep_dates%period(i)%sm = m
        rep_dates%period(i)%sy = y

        IF ( (m==1).or.(m==3).or.(m==5).or.(m==7).or.   &
            (m==8).or.(m==10).or.(m==12) ) THEN
            rep_dates%period(i)%ed = 31
        ELSE IF ( (m==4).or.(m==6).or.(m==9).or.(m==11) ) THEN
            rep_dates%period(i)%ed = 30
        ELSE                                  ! m==2 (Feb)
            rep_dates%period(i)%ed = 29
        END IF
        rep_dates%period(i)%em = m
        rep_dates%period(i)%ey = y

        ! Get all op dates in second "half" of month
        SELECT CASE (m)

          CASE (1,3,5,7,8,10,12)
            do z=1,size(mandate)
              if( mandate(z)%d > 15 .AND. mandate(z)%d <= 31 .AND. &
                  mandate(z)%m == m .AND. mandate(z)%y ==  y .AND. &
                  !The next line checks for additional ops on same date
                  mandate(z)%d /= rep_dates%period(i)%sd ) then
                i = i + 1    
                rep_dates%period(i)%sd = mandate(z)%d 
                rep_dates%period(i)%sm = mandate(z)%m
                rep_dates%period(i)%sy = mandate(z)%y

                rep_dates%period(i)%em = m
                rep_dates%period(i)%ey = y

                rep_dates%period(i)%ed = 31

                ! Fix previous period end date (day)
                rep_dates%period(i-1)%ed = rep_dates%period(i)%sd-1
              end if
            end do

          CASE (4,6,9,11)
            do z=1,size(mandate)
              if( mandate(z)%d > 15 .AND. mandate(z)%d <= 30 .AND. &
                  mandate(z)%m == m .AND. mandate(z)%y ==  y .AND. &
                  !The next line checks for additional ops on same date
                  mandate(z)%d /= rep_dates%period(i)%sd ) then
                i = i + 1    
                rep_dates%period(i)%sd = mandate(z)%d 
                rep_dates%period(i)%sm = mandate(z)%m
                rep_dates%period(i)%sy = mandate(z)%y

                rep_dates%period(i)%em = m
                rep_dates%period(i)%ey = y

                rep_dates%period(i)%ed = 30

                ! Fix previous period end date (day)
                rep_dates%period(i-1)%ed = rep_dates%period(i)%sd-1
              end if
            end do
          CASE DEFAULT
            do z=1,size(mandate)
              if( mandate(z)%d > 15 .AND. mandate(z)%d <= 29 .AND. &
                  mandate(z)%m == m .AND. mandate(z)%y ==  y .AND. &
                  !The next line checks for additional ops on same date
                  mandate(z)%d /= rep_dates%period(i)%sd ) then
                i = i + 1    
                rep_dates%period(i)%sd = mandate(z)%d 
                rep_dates%period(i)%sm = mandate(z)%m
                rep_dates%period(i)%sy = mandate(z)%y

                rep_dates%period(i)%em = m
                rep_dates%period(i)%ey = y

                rep_dates%period(i)%ed = 29

                ! Fix previous period end date (day)
                rep_dates%period(i-1)%ed = rep_dates%period(i)%sd-1
              end if
            end do
        END SELECT
      END DO
    END DO

    IF (i /= nperiods) THEN
       write(0,*) "init_report_vars: No. periods computed here doesn't match nperiods: ",i,"<>",nperiods
       write(0,*) "Error: No. of periods don't match"
       call exit (1)
    END IF

    ! initialize "update" and "report" vars and their cnts
    DO i=Min_yrly_vars,Max_yrly_vars
       rep_update%yrly_update(i)%cnt = 0
       rep_update%yrly_update(i)%val = 0.0
       rep_update%yrot_update(i)%cnt = 0
       rep_update%yrot_update(i)%val = 0.0
       rep_update%yr_update(i)%cnt = 0
       rep_update%yr_update(i)%val = 0.0
       DO y=0, nrot_yrs
          rep_report%yrly_report(i,y)%cnt = 0
          rep_report%yrly_report(i,y)%val = 0.0
       END DO
       ! For a year by year report of yearly (and rotation year) averaged variables
       DO y=1, nrot_yrs*ncycles
          rep_report%yr_report(i,y)%cnt = 0
          rep_report%yr_report(i,y)%val = 0.0
       END DO
    END DO

    DO i=Min_monthly_vars,Max_monthly_vars
       rep_update%monthly_update(i)%cnt = 0
       rep_update%monthly_update(i)%val = 0.0
       DO m=1, 12
          rep_update%mrot_update(i,m)%cnt = 0
          rep_update%mrot_update(i,m)%val = 0.0
          DO y=0, nrot_yrs
             rep_report%monthly_report(i,m,y)%cnt = 0
             rep_report%monthly_report(i,m,y)%val = 0.0
          END DO
       END DO
    END DO

    DO i=Min_hmonth_vars,Max_hmonth_vars
       rep_update%hmonth_update(i)%cnt = 0
       rep_update%hmonth_update(i)%val = 0.0
       DO hm=1, 24
          rep_update%hmrot_update(i,hm)%cnt = 0
          rep_update%hmrot_update(i,hm)%val = 0.0
          DO y=0, nrot_yrs
             rep_report%hmonth_report(i,hm,y)%cnt = 0
             rep_report%hmonth_report(i,hm,y)%val = 0.0
          END DO
       END DO
    END DO

    DO i=Min_period_vars,Max_period_vars
       rep_update%period_update(i)%cnt = 0
       rep_update%period_update(i)%val = 0.0
       DO p=1, nperiods
          rep_report%period_report(i,p)%cnt = 0
          rep_report%period_report(i,p)%val = 0.0
       END DO
    END DO

! print all initialized "start" and "end" dates

    if( report_debug >= 1 ) then

        ! yrly dates
        DO y=0, nrot_yrs
            print *, "rep_dates%yrly(",y,")", rep_dates%yrly(y)
        END DO

        ! monthly dates
        DO y=0, nrot_yrs
            DO m=1, 12
              print *, "rep_dates%monthly(",m,",",y,")", rep_dates%monthly(m,y)
            END DO
        END DO

        ! half month dates
        DO y=0, nrot_yrs
            DO hm=1, 24
              print *, "rep_dates%hmonth(",hm,",",y,")", rep_dates%hmonth(hm,y)
            END DO
        END DO

        ! period dates
        DO i=1, nperiods
            print *, "rep_dates%period(",i,")", rep_dates%period(i)
        END DO

        ! yr dates
        ! For a year by year report of yearly (and rotation year) averaged variables
        DO y=1, nrot_yrs*ncycles
            print *, "rep_dates%yr(",y,")", rep_dates%yr(y)
        END DO
    end if

! print all "update" values

    if( report_debug >= 1 ) then
        DO i=Min_yrly_vars,Max_yrly_vars
            print *, "rep_update%yrly_update(",i,")%val,cnt,date,ed", rep_update%yrly_update(i)%val,  &
                rep_update%yrly_update(i)%cnt, rep_update%yrly_update(i)%date,       &
                rep_update%yrly_update(i)%date%ed
        END DO
    endif

    if( report_debug == 2 ) then
  
     DO i=Min_yrly_vars,Max_yrly_vars
         print *, "rep_update%yrly_update(",i,")%val,cnt,date", rep_update%yrly_update(i)%val,   &
                  rep_update%yrly_update(i)%cnt, rep_update%yrly_update(i)%date
     END DO
     DO i=Min_monthly_vars,Max_monthly_vars
         print *, "rep_update%monthly_update(",i,")%val,cnt,date", rep_update%monthly_update(i)%val,   &
              rep_update%monthly_update(i)%cnt, rep_update%monthly_update(i)%date
     END DO
     DO i=Min_hmonth_vars,Max_hmonth_vars
         print *, "rep_update%hmonth_update(",i,")%val,cnt,date", rep_update%hmonth_update(i)%val,   &
              rep_update%hmonth_update(i)%cnt, rep_update%hmonth_update(i)%date
     END DO
     DO i=Min_period_vars,Max_period_vars
         print *, "rep_update%period_update(",i,")%val,cnt,date", rep_update%period_update(i)%val,   &
              rep_update%period_update(i)%cnt,rep_update%period_update(i)%date
     END DO
 
     DO i=Min_yrly_vars,Max_yrly_vars
         print *, "rep_update%yrot_update(",i,")%val,cnt,date", rep_update%yrot_update(i)%val,   &
                  rep_update%yrot_update(i)%cnt, rep_update%yrot_update(i)%date
     END DO
!hmm, this doesn't look correct here for "mrot_update" data - LEW
!Ok, we aren't necessarily printing out all the mrot vars by rot year here, only the 1st 12 in the first year
     DO i=Min_monthly_vars,Max_monthly_vars
       DO m=1,12
         print *, "rep_update%mrot_update(",i,",",m,")%val,cnt,date", rep_update%mrot_update(i,m)%val,   &
              rep_update%mrot_update(i,m)%cnt, rep_update%mrot_update(i,m)%date
       END DO
     END DO

!hmm, this doesn't look correct here for "hmrot_update" data - LEW
!Ok, we aren't necessarily printing out all the hmrot vars by rot year here, only the 1st 24 in the 1st year
     DO i=Min_hmonth_vars,Max_hmonth_vars
       DO hm=1,24
         print *, "rep_update%hmrot_update(",i,",",hm,")%val,cnt,date", rep_update%hmrot_update(i,hm)%val,   &
              rep_update%hmrot_update(i,hm)%cnt, rep_update%hmrot_update(i,hm)%date
       END DO
     END DO

     DO i=Min_yrly_vars,Max_yrly_vars
         print *, "rep_update%yr_update(",i,")%val,cnt,date", rep_update%yr_update(i)%val,   &
                  rep_update%yr_update(i)%cnt, rep_update%yr_update(i)%date
     END DO
    end if
 
    ! print out SOME (not all) of the info for the "report" variables
    if( report_debug == 3 ) then

     ! Note that we are intentionally evaluating only one variable here for debugging purposes - LEW
     DO i=Min_yrly_vars,Min_yrly_vars
       DO y=0,nrot_yrs
         IF (.not. ASSOCIATED(rep_report%yrly_report(i,y)%date,rep_dates%yrly(y))) THEN
           print *, "Error: rep_report%yrly_report(",i,y,")%date not ASSOCIATED with rep_dates%yrly(",y,")"
         ELSE
           print *, "rep_report%yrly_report(",i,",",y,")%val,cnt,date", rep_report%yrly_report(i,y)%val,   &
                  rep_report%yrly_report(i,y)%cnt, rep_report%yrly_report(i,y)%date
         END IF
       END DO
     END DO
     ! Note that we are intentionally evaluating only one variable here for debugging purposes - LEW
     DO i=Max_yrly_vars,Max_yrly_vars
       DO y=0,nrot_yrs
         IF (.not. ASSOCIATED(rep_report%yrly_report(i,y)%date,rep_dates%yrly(y))) THEN
           print *, "Error: rep_report%yrly_report(",i,y,")%date not ASSOCIATED with rep_dates%yrly(",y,")"
         ELSE
           print *, "rep_report%yrly_report(",i,",",y,")%val,cnt,date", rep_report%yrly_report(i,y)%val,   &
                  rep_report%yrly_report(i,y)%cnt, rep_report%yrly_report(i,y)%date
         END IF
       END DO
     END DO

     ! Note that we are intentionally evaluating only one variable here for debugging purposes - LEW
     DO i=Min_monthly_vars,Min_monthly_vars
       DO y=0,nrot_yrs
         ! Note that we are intentionally evaluating only one m period here for debugging purposes - LEW
         DO m=1,1
           IF (.not. ASSOCIATED(rep_report%monthly_report(i,m,y)%date,rep_dates%monthly(m,y))) THEN
             print *, "Error: rep_report%monthly_report(",i,m,y,")%date not ASSOCIATED with rep_dates%monthly(",m,y,")"
           ELSE
             print *, "rep_report%monthly_report(",i, ",",m,",",y,")%val,cnt,date", rep_report%monthly_report(i,m,y)%val,   &
                  rep_report%monthly_report(i,m,y)%cnt, rep_report%monthly_report(i,m,y)%date
           END IF
         END DO
         ! Note that we are intentionally evaluating only one m period here for debugging purposes - LEW
         DO m=12,12
           IF (.not. ASSOCIATED(rep_report%monthly_report(i,m,y)%date,rep_dates%monthly(m,y))) THEN
             print *, "Error: rep_report%monthly_report(",i,m,y,")%date not ASSOCIATED with rep_dates%monthly(",m,y,")"
           ELSE
             print *, "rep_report%monthly_report(",i, ",",m,",",y,")%val,cnt,date", rep_report%monthly_report(i,m,y)%val,   &
                  rep_report%monthly_report(i,m,y)%cnt, rep_report%monthly_report(i,m,y)%date
           END IF
         END DO
       END DO
     END DO

     ! Note that we are intentionally evaluating only one variable here for debugging purposes - LEW
     DO i=Max_monthly_vars,Max_monthly_vars
       DO y=0,nrot_yrs
         ! Note that we are intentionally evaluating only one m period here for debugging purposes - LEW
         DO m=1,1
           IF (.not. ASSOCIATED(rep_report%monthly_report(i,m,y)%date,rep_dates%monthly(m,y))) THEN
             print *, "Error: rep_report%monthly_report(",i,m,y,")%date not ASSOCIATED with rep_dates%monthly(",m,y,")"
           ELSE
             print *, "rep_report%monthly_report(",i, ",",m,",",y,")%val,cnt,date", rep_report%monthly_report(i,m,y)%val,   &
                  rep_report%monthly_report(i,m,y)%cnt, rep_report%monthly_report(i,m,y)%date
           END IF
         END DO
         ! Note that we are intentionally evaluating only one m period here for debugging purposes - LEW
         DO m=12,12
           IF (.not. ASSOCIATED(rep_report%monthly_report(i,m,y)%date,rep_dates%monthly(m,y))) THEN
             print *, "Error: rep_report%monthly_report(",i,m,y,")%date not ASSOCIATED with rep_dates%monthly(",m,y,")"
           ELSE
             print *, "rep_report%monthly_report(",i, ",",m,",",y,")%val,cnt,date", rep_report%monthly_report(i,m,y)%val,   &
                  rep_report%monthly_report(i,m,y)%cnt, rep_report%monthly_report(i,m,y)%date
           END IF
         END DO
       END DO
     END DO

     ! Note that we are intentionally evaluating only one variable here for debugging purposes - LEW
     DO i=Min_hmonth_vars,Min_hmonth_vars
       DO y=0,nrot_yrs
         ! Note that we are intentionally evaluating only one hm period here for debugging purposes - LEW
         DO hm=1,1
           IF (.not. ASSOCIATED(rep_report%hmonth_report(i,hm,y)%date,rep_dates%hmonth(hm,y))) THEN
               print *, "Error: rep_report%hmonth_report(",i,hm,y,")%date not ASSOCIATED with rep_dates%hmonth(",hm,y,")"
           ELSE
             print *, "rep_report%hmonth_report(",i, ",",hm,",",y,")%val,cnt,date", rep_report%hmonth_report(i,hm,y)%val,   &
                  rep_report%hmonth_report(i,hm,y)%cnt, rep_report%hmonth_report(i,hm,y)%date
           END IF
         END DO
         ! Note that we are intentionally evaluating only one hm period here for debugging purposes - LEW
         DO hm=24,24
           IF (.not. ASSOCIATED(rep_report%hmonth_report(i,hm,y)%date,rep_dates%hmonth(hm,y))) THEN
               print *, "Error: rep_report%hmonth_report(",i,hm,y,")%date not ASSOCIATED with rep_dates%hmonth(",hm,y,")"
           ELSE
             print *, "rep_report%hmonth_report(",i, ",",hm,",",y,")%val,cnt,date", rep_report%hmonth_report(i,hm,y)%val,   &
                  rep_report%hmonth_report(i,hm,y)%cnt, rep_report%hmonth_report(i,hm,y)%date
           END IF
         END DO
       END DO
     END DO
     ! Note that we are intentionally evaluating only one variable here for debugging purposes - LEW
     DO i=Max_hmonth_vars,Max_hmonth_vars
       DO y=0,nrot_yrs
         ! Note that we are intentionally evaluating only one hm period here for debugging purposes - LEW
         DO hm=1,1
           IF (.not. ASSOCIATED(rep_report%hmonth_report(i,hm,y)%date,rep_dates%hmonth(hm,y))) THEN
               print *, "Error: rep_report%hmonth_report(",i,hm,y,")%date not ASSOCIATED with rep_dates%hmonth(",hm,y,")"
           ELSE
             print *, "rep_report%hmonth_report(",i, ",",hm,",",y,")%val,cnt,date", rep_report%hmonth_report(i,hm,y)%val,   &
                  rep_report%hmonth_report(i,hm,y)%cnt, rep_report%hmonth_report(i,hm,y)%date
           END IF
         END DO
         ! Note that we are intentionally evaluating only one hm period here for debugging purposes - LEW
         DO hm=24,24
           IF (.not. ASSOCIATED(rep_report%hmonth_report(i,hm,y)%date,rep_dates%hmonth(hm,y))) THEN
               print *, "Error: rep_report%hmonth_report(",i,hm,y,")%date not ASSOCIATED with rep_dates%hmonth(",hm,y,")"
           ELSE
             print *, "rep_report%hmonth_report(",i, ",",hm,",",y,")%val,cnt,date", rep_report%hmonth_report(i,hm,y)%val,   &
                  rep_report%hmonth_report(i,hm,y)%cnt, rep_report%hmonth_report(i,hm,y)%date
           END IF
         END DO
       END DO
     END DO

     ! Note that we are intentionally evaluating only one variable here for debugging purposes - LEW
     DO i=Min_period_vars,Min_period_vars
         ! Note that we are intentionally evaluating only one period here for debugging purposes - LEW
         DO p=1,1
           IF (.not. ASSOCIATED(rep_report%period_report(i,p)%date,rep_dates%period(p))) THEN
              print *, "Error: rep_report%period_report(",i,p,")%date not ASSOCIATED with rep_dates%period(",p,")"
           ELSE
              print *, "rep_report%period_report(",i,",",p,")%val,cnt,date", rep_report%period_report(i,p)%val,   &
                 rep_report%period_report(i,p)%cnt,rep_report%period_report(i,p)%date
           END IF
         END DO
         ! Note that we are intentionally evaluating only one period here for debugging purposes - LEW
         DO p=nperiods,nperiods
           IF (.not. ASSOCIATED(rep_report%period_report(i,p)%date,rep_dates%period(p))) THEN
              print *, "Error: rep_report%period_report(",i,p,")%date not ASSOCIATED with rep_dates%period(",p,")"
           ELSE
              print *, "rep_report%period_report(",i,",",p,")%val,cnt,date", rep_report%period_report(i,p)%val,   &
                 rep_report%period_report(i,p)%cnt,rep_report%period_report(i,p)%date
           END IF
         END DO
     END DO
     ! Note that we are intentionally evaluating only one variable here for debugging purposes - LEW
     DO i=Max_period_vars,Max_period_vars
         ! Note that we are intentionally evaluating only one period here for debugging purposes - LEW
         DO p=1,1
           IF (.not. ASSOCIATED(rep_report%period_report(i,p)%date,rep_dates%period(p))) THEN
              print *, "Error: rep_report%period_report(",i,p,")%date not ASSOCIATED with rep_dates%period(",p,")"
           ELSE
              print *, "rep_report%period_report(",i,",",p,")%val,cnt,date", rep_report%period_report(i,p)%val,   &
                 rep_report%period_report(i,p)%cnt,rep_report%period_report(i,p)%date
           END IF
         END DO
         ! Note that we are intentionally evaluating only one period here for debugging purposes - LEW
         DO p=nperiods,nperiods
           IF (.not. ASSOCIATED(rep_report%period_report(i,p)%date,rep_dates%period(p))) THEN
              print *, "Error: rep_report%period_report(",i,p,")%date not ASSOCIATED with rep_dates%period(",p,")"
           ELSE
              print *, "rep_report%period_report(",i,",",p,")%val,cnt,date", rep_report%period_report(i,p)%val,   &
                 rep_report%period_report(i,p)%cnt,rep_report%period_report(i,p)%date
           END IF
         END DO
     END DO

     ! For a year by year report of yearly (and rotation year) averaged variables
     ! Note that we are intentionally evaluating only one variable here for debugging purposes - LEW
     DO i=Min_yrly_vars,Min_yrly_vars
       DO y=1,nrot_yrs*ncycles
         IF (.not. ASSOCIATED(rep_report%yr_report(i,y)%date,rep_dates%yr(y))) THEN
           print *, "Error: rep_report%yr_report(",i,y,")%date not ASSOCIATED with rep_dates%yr(",y,")"
         ELSE
           print *, "rep_report%yr_report(",i,",",y,")%val,cnt,date", rep_report%yr_report(i,y)%val,   &
                  rep_report%yr_report(i,y)%cnt, rep_report%yr_report(i,y)%date
         END IF
       END DO
     END DO
    end if

  END SUBROUTINE init_report_vars

end module report_init_mod

