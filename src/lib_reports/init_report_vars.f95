!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!
SUBROUTINE init_report_vars(nperiods, nrot_yrs, ncycles)

    USE pd_dates_vars
    USE pd_update_vars
    USE pd_report_vars

    USE pd_var_tables

    USE mandate_vars

    USE alloc_pd_vars_func         !defines alloc_pd_vars function

    IMPLICIT NONE

    INTEGER, INTENT (IN) :: nperiods   ! 24 is minimum value per rotation year
    INTEGER, INTENT (IN) :: nrot_yrs   ! Minimum is 1
    INTEGER, INTENT (IN) :: ncycles    ! number of rotation cycles
    INTEGER :: status = 0

    INTEGER :: i,p,hm,m,y,z        ! local loop variables

    include 'command.inc'          !declarations for commandline args

    status = alloc_pd_vars(nperiods, nrot_yrs, ncycles) !Allocate space for all pd variables
    IF (status >= 1) THEN
       write(0,*) "Status of alloc_pd_vars: ", status
       write(0,*) "Error allocating pd_vars"
       call exit (1)
    END IF

    ! Init the period start and stop dates for "report" and "update" variables

    ! yrly dates
    DO y=0, nrot_yrs
        ! set "start" dates
        yrly_dates(y)%sd = 1
        yrly_dates(y)%sm = 1
        IF (y == 0) THEN
            yrly_dates(y)%sy = 1
        ELSE
            yrly_dates(y)%sy = y
        END IF

        ! set "end" dates
        yrly_dates(y)%ed = 31
        yrly_dates(y)%em = 12
        IF (y == 0) THEN
            yrly_dates(y)%ey = nrot_yrs
        ELSE
            yrly_dates(y)%ey = y
        END IF
    END DO

    ! yr dates
    ! For a year by year report of yearly (and rotation year) averaged variables
    DO y=1, nrot_yrs*ncycles
        ! set "start" dates
        yr_dates(y)%sd = 1
        yr_dates(y)%sm = 1
        yr_dates(y)%sy = y

        ! set "end" dates
        yr_dates(y)%ed = 31
        yr_dates(y)%em = 12
        yr_dates(y)%ey = y
    END DO


    ! monthly dates
    DO y=0, nrot_yrs
        DO m=1, 12
            ! set "start" dates
            monthly_dates(m,y)%sd = 1
            monthly_dates(m,y)%sm = m
            IF (y == 0) THEN
               monthly_dates(m,y)%sy = 1
            ELSE
               monthly_dates(m,y)%sy = y
            END IF

            ! set "end" dates
            IF ( (m==1).or.(m==3).or.(m==5).or.(m==7).or.   &
                 (m==8).or.(m==10).or.(m==12) ) THEN
                monthly_dates(m,y)%ed = 31
            ELSE IF ( (m==4).or.(m==6).or.(m==9).or.(m==11) ) THEN
                monthly_dates(m,y)%ed = 30
            ELSE   ! m==2 (Feb)
                monthly_dates(m,y)%ed = 29
            END IF
            monthly_dates(m,y)%em = m 
            IF (y == 0) THEN
               monthly_dates(m,y)%ey = nrot_yrs
            ELSE
               monthly_dates(m,y)%ey = y
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
             hmonth_dates(hm,y)%sd = 1
             hmonth_dates(hm,y)%sm = m
             IF (y == 0) THEN
                hmonth_dates(hm,y)%sy = 1
             ELSE
                hmonth_dates(hm,y)%sy = y
             END IF

             hmonth_dates(hm,y)%ed = 14
             hmonth_dates(hm,y)%em = m
             IF (y == 0) THEN
                hmonth_dates(hm,y)%ey = nrot_yrs
             ELSE
                hmonth_dates(hm,y)%ey = y
             END IF
          ELSE               ! 2nd half month period
             hmonth_dates(hm,y)%sd = 15
             hmonth_dates(hm,y)%sm = m
             IF (y == 0) THEN
                hmonth_dates(hm,y)%sy = 1
             ELSE
                hmonth_dates(hm,y)%sy = y
             END IF

             IF ( (m==1).or.(m==3).or.(m==5).or.(m==7).or.   &
                   (m==8).or.(m==10).or.(m==12) ) THEN
                hmonth_dates(hm,y)%ed = 31
             ELSE IF ( (m==4).or.(m==6).or.(m==9).or.(m==11) ) THEN
                hmonth_dates(hm,y)%ed = 30
             ELSE                                  ! m==2 (Feb)
                hmonth_dates(hm,y)%ed = 29
             END IF
             hmonth_dates(hm,y)%em = m
             IF (y == 0) THEN
                hmonth_dates(hm,y)%ey = nrot_yrs
             ELSE
                hmonth_dates(hm,y)%ey = y
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
        period_dates(i)%sd = 1
        period_dates(i)%sm = m
        period_dates(i)%sy = y

        period_dates(i)%ed = 14
        period_dates(i)%em = m
        period_dates(i)%ey = y

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
              mandate(z)%d /= period_dates(i)%sd ) then
            i = i + 1    
            period_dates(i)%sd = mandate(z)%d 
            period_dates(i)%sm = mandate(z)%m
            period_dates(i)%sy = mandate(z)%y

            period_dates(i)%ed = 14
            period_dates(i)%em = m
            period_dates(i)%ey = y

            ! Fix previous period end date (day)
            period_dates(i-1)%ed = period_dates(i)%sd-1
          end if
        end do

        ! 2nd half month period
        i = i + 1
        period_dates(i)%sd = 15
        period_dates(i)%sm = m
        period_dates(i)%sy = y

        IF ( (m==1).or.(m==3).or.(m==5).or.(m==7).or.   &
            (m==8).or.(m==10).or.(m==12) ) THEN
            period_dates(i)%ed = 31
        ELSE IF ( (m==4).or.(m==6).or.(m==9).or.(m==11) ) THEN
            period_dates(i)%ed = 30
        ELSE                                  ! m==2 (Feb)
            period_dates(i)%ed = 29
        END IF
        period_dates(i)%em = m
        period_dates(i)%ey = y

        ! Get all op dates in second "half" of month
        SELECT CASE (m)

          CASE (1,3,5,7,8,10,12)
            do z=1,size(mandate)
              if( mandate(z)%d > 15 .AND. mandate(z)%d <= 31 .AND. &
                  mandate(z)%m == m .AND. mandate(z)%y ==  y .AND. &
                  !The next line checks for additional ops on same date
                  mandate(z)%d /= period_dates(i)%sd ) then
                i = i + 1    
                period_dates(i)%sd = mandate(z)%d 
                period_dates(i)%sm = mandate(z)%m
                period_dates(i)%sy = mandate(z)%y

                period_dates(i)%em = m
                period_dates(i)%ey = y

                period_dates(i)%ed = 31

                ! Fix previous period end date (day)
                period_dates(i-1)%ed = period_dates(i)%sd-1
              end if
            end do

          CASE (4,6,9,11)
            do z=1,size(mandate)
              if( mandate(z)%d > 15 .AND. mandate(z)%d <= 30 .AND. &
                  mandate(z)%m == m .AND. mandate(z)%y ==  y .AND. &
                  !The next line checks for additional ops on same date
                  mandate(z)%d /= period_dates(i)%sd ) then
                i = i + 1    
                period_dates(i)%sd = mandate(z)%d 
                period_dates(i)%sm = mandate(z)%m
                period_dates(i)%sy = mandate(z)%y

                period_dates(i)%em = m
                period_dates(i)%ey = y

                period_dates(i)%ed = 30

                ! Fix previous period end date (day)
                period_dates(i-1)%ed = period_dates(i)%sd-1
              end if
            end do
          CASE DEFAULT
            do z=1,size(mandate)
              if( mandate(z)%d > 15 .AND. mandate(z)%d <= 29 .AND. &
                  mandate(z)%m == m .AND. mandate(z)%y ==  y .AND. &
                  !The next line checks for additional ops on same date
                  mandate(z)%d /= period_dates(i)%sd ) then
                i = i + 1    
                period_dates(i)%sd = mandate(z)%d 
                period_dates(i)%sm = mandate(z)%m
                period_dates(i)%sy = mandate(z)%y

                period_dates(i)%em = m
                period_dates(i)%ey = y

                period_dates(i)%ed = 29

                ! Fix previous period end date (day)
                period_dates(i-1)%ed = period_dates(i)%sd-1
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
       yrly_update(i)%cnt = 0
       yrly_update(i)%val = 0.0
       yrot_update(i)%cnt = 0
       yrot_update(i)%val = 0.0
       yr_update(i)%cnt = 0
       yr_update(i)%val = 0.0
       DO y=0, nrot_yrs
          yrly_report(i,y)%cnt = 0
          yrly_report(i,y)%val = 0.0
       END DO
       ! For a year by year report of yearly (and rotation year) averaged variables
       DO y=1, nrot_yrs*ncycles
          yr_report(i,y)%cnt = 0
          yr_report(i,y)%val = 0.0
       END DO
    END DO

    DO i=Min_monthly_vars,Max_monthly_vars
       monthly_update(i)%cnt = 0
       monthly_update(i)%val = 0.0
       DO m=1, 12
          mrot_update(i,m)%cnt = 0
          mrot_update(i,m)%val = 0.0
          DO y=0, nrot_yrs
             monthly_report(i,m,y)%cnt = 0
             monthly_report(i,m,y)%val = 0.0
          END DO
       END DO
    END DO

    DO i=Min_hmonth_vars,Max_hmonth_vars
       hmonth_update(i)%cnt = 0
       hmonth_update(i)%val = 0.0
       DO hm=1, 24
          hmrot_update(i,hm)%cnt = 0
          hmrot_update(i,hm)%val = 0.0
          DO y=0, nrot_yrs
             hmonth_report(i,hm,y)%cnt = 0
             hmonth_report(i,hm,y)%val = 0.0
          END DO
       END DO
    END DO

    DO i=Min_period_vars,Max_period_vars
       period_update(i)%cnt = 0
       period_update(i)%val = 0.0
       DO p=1, nperiods
          period_report(i,p)%cnt = 0
          period_report(i,p)%val = 0.0
       END DO
    END DO

! print all initialized "start" and "end" dates

    if( report_debug >= 1 ) then

        ! yrly dates
        DO y=0, nrot_yrs
            print *, "yrly_dates(",y,")", yrly_dates(y)
        END DO

        ! monthly dates
        DO y=0, nrot_yrs
            DO m=1, 12
              print *, "monthly_dates(",m,",",y,")", monthly_dates(m,y)
            END DO
        END DO

        ! half month dates
        DO y=0, nrot_yrs
            DO hm=1, 24
              print *, "hmonth_dates(",hm,",",y,")", hmonth_dates(hm,y)
            END DO
        END DO

        ! period dates
        DO i=1, nperiods
            print *, "period_dates(",i,")", period_dates(i)
        END DO

        ! yr dates
        ! For a year by year report of yearly (and rotation year) averaged variables
        DO y=1, nrot_yrs*ncycles
            print *, "yr_dates(",y,")", yr_dates(y)
        END DO
    end if

! print all "update" values

    if( report_debug >= 1 ) then
        DO i=Min_yrly_vars,Max_yrly_vars
            print *, "yrly_update(",i,")%val,cnt,date,ed", yrly_update(i)%val,  &
                yrly_update(i)%cnt, yrly_update(i)%date,       &
                yrly_update(i)%date%ed
        END DO
    endif

    if( report_debug == 2 ) then
  
     DO i=Min_yrly_vars,Max_yrly_vars
         print *, "yrly_update(",i,")%val,cnt,date", yrly_update(i)%val,   &
                  yrly_update(i)%cnt, yrly_update(i)%date
     END DO
     DO i=Min_monthly_vars,Max_monthly_vars
         print *, "monthly_update(",i,")%val,cnt,date", monthly_update(i)%val,   &
              monthly_update(i)%cnt, monthly_update(i)%date
     END DO
     DO i=Min_hmonth_vars,Max_hmonth_vars
         print *, "hmonth_update(",i,")%val,cnt,date", hmonth_update(i)%val,   &
              hmonth_update(i)%cnt, hmonth_update(i)%date
     END DO
     DO i=Min_period_vars,Max_period_vars
         print *, "period_update(",i,")%val,cnt,date", period_update(i)%val,   &
              period_update(i)%cnt,period_update(i)%date
     END DO
 
     DO i=Min_yrly_vars,Max_yrly_vars
         print *, "yrot_update(",i,")%val,cnt,date", yrot_update(i)%val,   &
                  yrot_update(i)%cnt, yrot_update(i)%date
     END DO
     DO i=Min_monthly_vars,Max_monthly_vars
       DO m=1,12
         print *, "mrot_update(",i,",",m,")%val,cnt,date", mrot_update(i,m)%val,   &
              mrot_update(i,m)%cnt, mrot_update(i,m)%date
       END DO
     END DO
     DO i=Min_hmonth_vars,Max_hmonth_vars
       DO hm=1,24
         print *, "hmrot_update(",i,",",hm,")%val,cnt,date", hmrot_update(i,hm)%val,   &
              hmrot_update(i,hm)%cnt, hmrot_update(i,hm)%date
       END DO
     END DO

     DO i=Min_yrly_vars,Max_yrly_vars
         print *, "yr_update(",i,")%val,cnt,date", yr_update(i)%val,   &
                  yr_update(i)%cnt, yr_update(i)%date
     END DO
    end if
 
    ! print out SOME (not all) of the info for the "report" variables
    if( report_debug == 3 ) then

     DO i=Min_yrly_vars,Min_yrly_vars
       DO y=0,nrot_yrs
         IF (.not. ASSOCIATED(yrly_report(i,y)%date,yrly_dates(y))) THEN
           print *, "Error: yrly_report(",i,y,")%date not ASSOCIATED with yrly_dates(",y,")"
         ELSE
           print *, "yrly_report(",i,",",y,")%val,cnt,date", yrly_report(i,y)%val,   &
                  yrly_report(i,y)%cnt, yrly_report(i,y)%date
         END IF
       END DO
     END DO
     DO i=Max_yrly_vars,Max_yrly_vars
       DO y=0,nrot_yrs
         IF (.not. ASSOCIATED(yrly_report(i,y)%date,yrly_dates(y))) THEN
           print *, "Error: yrly_report(",i,y,")%date not ASSOCIATED with yrly_dates(",y,")"
         ELSE
           print *, "yrly_report(",i,",",y,")%val,cnt,date", yrly_report(i,y)%val,   &
                  yrly_report(i,y)%cnt, yrly_report(i,y)%date
         END IF
       END DO
     END DO

     DO i=Min_monthly_vars,Min_monthly_vars
       DO y=0,nrot_yrs
         DO m=1,1
           IF (.not. ASSOCIATED(monthly_report(i,m,y)%date,monthly_dates(m,y))) THEN
             print *, "Error: monthly_report(",i,m,y,")%date not ASSOCIATED with monthly_dates(",m,y,")"
           ELSE
             print *, "monthly_report(",i, ",",m,",",y,")%val,cnt,date", monthly_report(i,m,y)%val,   &
                  monthly_report(i,m,y)%cnt, monthly_report(i,m,y)%date
           END IF
         END DO
         DO m=12,12
           IF (.not. ASSOCIATED(monthly_report(i,m,y)%date,monthly_dates(m,y))) THEN
             print *, "Error: monthly_report(",i,m,y,")%date not ASSOCIATED with monthly_dates(",m,y,")"
           ELSE
             print *, "monthly_report(",i, ",",m,",",y,")%val,cnt,date", monthly_report(i,m,y)%val,   &
                  monthly_report(i,m,y)%cnt, monthly_report(i,m,y)%date
           END IF
         END DO
       END DO
     END DO
     DO i=Max_monthly_vars,Max_monthly_vars
       DO y=0,nrot_yrs
         DO m=1,1
           IF (.not. ASSOCIATED(monthly_report(i,m,y)%date,monthly_dates(m,y))) THEN
             print *, "Error: monthly_report(",i,m,y,")%date not ASSOCIATED with monthly_dates(",m,y,")"
           ELSE
             print *, "monthly_report(",i, ",",m,",",y,")%val,cnt,date", monthly_report(i,m,y)%val,   &
                  monthly_report(i,m,y)%cnt, monthly_report(i,m,y)%date
           END IF
         END DO
         DO m=12,12
           IF (.not. ASSOCIATED(monthly_report(i,m,y)%date,monthly_dates(m,y))) THEN
             print *, "Error: monthly_report(",i,m,y,")%date not ASSOCIATED with monthly_dates(",m,y,")"
           ELSE
             print *, "monthly_report(",i, ",",m,",",y,")%val,cnt,date", monthly_report(i,m,y)%val,   &
                  monthly_report(i,m,y)%cnt, monthly_report(i,m,y)%date
           END IF
         END DO
       END DO
     END DO

     DO i=Min_hmonth_vars,Min_hmonth_vars
       DO y=0,nrot_yrs
         DO hm=1,1
           IF (.not. ASSOCIATED(hmonth_report(i,hm,y)%date,hmonth_dates(hm,y))) THEN
               print *, "Error: hmonth_report(",i,hm,y,")%date not ASSOCIATED with hmonth_dates(",hm,y,")"
           ELSE
             print *, "hmonth_report(",i, ",",hm,",",y,")%val,cnt,date", hmonth_report(i,hm,y)%val,   &
                  hmonth_report(i,hm,y)%cnt, hmonth_report(i,hm,y)%date
           END IF
         END DO
         DO hm=24,24
           IF (.not. ASSOCIATED(hmonth_report(i,hm,y)%date,hmonth_dates(hm,y))) THEN
               print *, "Error: hmonth_report(",i,hm,y,")%date not ASSOCIATED with hmonth_dates(",hm,y,")"
           ELSE
             print *, "hmonth_report(",i, ",",hm,",",y,")%val,cnt,date", hmonth_report(i,hm,y)%val,   &
                  hmonth_report(i,hm,y)%cnt, hmonth_report(i,hm,y)%date
           END IF
         END DO
       END DO
     END DO
     DO i=Max_hmonth_vars,Max_hmonth_vars
       DO y=0,nrot_yrs
         DO hm=1,1
           IF (.not. ASSOCIATED(hmonth_report(i,hm,y)%date,hmonth_dates(hm,y))) THEN
               print *, "Error: hmonth_report(",i,hm,y,")%date not ASSOCIATED with hmonth_dates(",hm,y,")"
           ELSE
             print *, "hmonth_report(",i, ",",hm,",",y,")%val,cnt,date", hmonth_report(i,hm,y)%val,   &
                  hmonth_report(i,hm,y)%cnt, hmonth_report(i,hm,y)%date
           END IF
         END DO
         DO hm=24,24
           IF (.not. ASSOCIATED(hmonth_report(i,hm,y)%date,hmonth_dates(hm,y))) THEN
               print *, "Error: hmonth_report(",i,hm,y,")%date not ASSOCIATED with hmonth_dates(",hm,y,")"
           ELSE
             print *, "hmonth_report(",i, ",",hm,",",y,")%val,cnt,date", hmonth_report(i,hm,y)%val,   &
                  hmonth_report(i,hm,y)%cnt, hmonth_report(i,hm,y)%date
           END IF
         END DO
       END DO
     END DO

     DO i=Min_period_vars,Min_period_vars
         DO p=1,1
           IF (.not. ASSOCIATED(period_report(i,p)%date,period_dates(p))) THEN
              print *, "Error: period_report(",i,p,")%date not ASSOCIATED with period_dates(",p,")"
           ELSE
              print *, "period_report(",i,",",p,")%val,cnt,date", period_report(i,p)%val,   &
                 period_report(i,p)%cnt,period_report(i,p)%date
           END IF
         END DO
         DO p=nperiods,nperiods
           IF (.not. ASSOCIATED(period_report(i,p)%date,period_dates(p))) THEN
              print *, "Error: period_report(",i,p,")%date not ASSOCIATED with period_dates(",p,")"
           ELSE
              print *, "period_report(",i,",",p,")%val,cnt,date", period_report(i,p)%val,   &
                 period_report(i,p)%cnt,period_report(i,p)%date
           END IF
         END DO
     END DO
     DO i=Max_period_vars,Max_period_vars
         DO p=1,1
           IF (.not. ASSOCIATED(period_report(i,p)%date,period_dates(p))) THEN
              print *, "Error: period_report(",i,p,")%date not ASSOCIATED with period_dates(",p,")"
           ELSE
              print *, "period_report(",i,",",p,")%val,cnt,date", period_report(i,p)%val,   &
                 period_report(i,p)%cnt,period_report(i,p)%date
           END IF
         END DO
         DO p=nperiods,nperiods
           IF (.not. ASSOCIATED(period_report(i,p)%date,period_dates(p))) THEN
              print *, "Error: period_report(",i,p,")%date not ASSOCIATED with period_dates(",p,")"
           ELSE
              print *, "period_report(",i,",",p,")%val,cnt,date", period_report(i,p)%val,   &
                 period_report(i,p)%cnt,period_report(i,p)%date
           END IF
         END DO
     END DO

     ! For a year by year report of yearly (and rotation year) averaged variables
     DO i=Min_yrly_vars,Min_yrly_vars
       DO y=1,nrot_yrs*ncycles
         IF (.not. ASSOCIATED(yr_report(i,y)%date,yr_dates(y))) THEN
           print *, "Error: yr_report(",i,y,")%date not ASSOCIATED with yr_dates(",y,")"
         ELSE
           print *, "yr_report(",i,",",y,")%val,cnt,date", yr_report(i,y)%val,   &
                  yr_report(i,y)%cnt, yr_report(i,y)%date
         END IF
       END DO
     END DO
    end if


END SUBROUTINE init_report_vars
