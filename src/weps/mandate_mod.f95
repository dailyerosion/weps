!$Author$
!$Date$
!$Revision$
!$HeadURL$

! This module needs to be "used" by mandates.for and others

MODULE mandate_mod

    IMPLICIT NONE

    type :: opercrop_date
       integer :: sr, d, m, y 
       character(80) :: opname
       character(80) :: cropname
    end type opercrop_date

    type :: mandate_array
       integer :: mperod
       type (opercrop_date), dimension(:), allocatable :: mandate
       !type (opercrop_date), dimension(:), pointer :: mp
    end type mandate_array

  contains

    subroutine create_mandate( cnt_dates, mandate )
       integer :: cnt_dates
       type (opercrop_date), dimension(:), allocatable :: mandate

       integer :: alloc_stat  ! allocation status return

       allocate(mandate(1:cnt_dates), stat=alloc_stat)
       if( alloc_stat .gt. 0 ) then
          write(*,*) 'ERROR: unable to allocate memory for mandate'
          stop 1
       end if
    end subroutine create_mandate

    subroutine destroy_mandate( mandate )
       type (opercrop_date), dimension(:), allocatable :: mandate

       integer :: dealloc_stat  ! deallocation status return

       deallocate(mandate, stat=dealloc_stat)
       if( dealloc_stat .gt. 0 ) then
          write(*,*) 'ERROR: unable to deallocate memory for mandate'
          stop 1
       end if
    end subroutine destroy_mandate

    subroutine sync_harvcropnames( mandatbs )

       use datetime_mod, only: difdat

       type(mandate_array), dimension(:) :: mandatbs

       integer :: alloc_stat
       integer :: sum_stat

       integer :: ld, lm, ly  ! day, month, year values
       integer :: daydif      ! result from difdat

       integer :: isr   ! subregion loop index
       integer :: lsr   ! lower subregion index
       integer :: usr   ! upper subregion index
       integer :: osr   ! the composite 0 "subregion" index

       logical, dimension(:), allocatable :: subdone ! logical .true. indicates that the last date has been checked
       integer, dimension(:), allocatable :: ldx    ! lower index for each subregion date array
       integer, dimension(:), allocatable :: udx    ! upper index for each subregion date array
       integer, dimension(:), allocatable :: pdx    ! present index for each subregion date array

       integer, dimension(:), allocatable :: cnt_cycles    ! number of cycles for each subregion date array to fill 0 array cycle
       integer, dimension(:), allocatable :: idx_cycles    ! index indicating the present subregion date array cycle
       integer, dimension(:), allocatable :: add_yr    ! number of years too add subregion date to make it match multiple cycles

       ! note: this structure is passed with the (0) index array used to hold the result
       osr = lbound(mandatbs,1)
       lsr = osr + 1
       usr = ubound(mandatbs,1)

       sum_stat = 0
       ! allocate indexes into each mandate array
       allocate( subdone(osr:usr), stat = alloc_stat )
       sum_stat = sum_stat + alloc_stat
       allocate( ldx(osr:usr), stat = alloc_stat )
       sum_stat = sum_stat + alloc_stat
       allocate( udx(osr:usr), stat = alloc_stat )
       sum_stat = sum_stat + alloc_stat
       allocate( pdx(osr:usr), stat = alloc_stat )
       sum_stat = sum_stat + alloc_stat
       allocate( cnt_cycles(lsr:usr), stat = alloc_stat )
       sum_stat = sum_stat + alloc_stat
       allocate( idx_cycles(lsr:usr), stat = alloc_stat )
       sum_stat = sum_stat + alloc_stat
       allocate( add_yr(lsr:usr), stat = alloc_stat )
       sum_stat = sum_stat + alloc_stat
       if( sum_stat .gt. 0 ) then
          write(*,*) 'ERROR: unable to allocate memory in allmandates'
          stop 1
       end if

       do isr = osr, usr
          ldx(isr) = lbound(mandatbs(isr)%mandate,1)
          udx(isr) = ubound(mandatbs(isr)%mandate,1)
          pdx(isr) = ldx(isr)
          if( isr .ge. lsr ) then
             cnt_cycles(isr) = mandatbs(osr)%mperod / mandatbs(isr)%mperod
             idx_cycles(isr) = 1
             add_yr(isr) = 0
          end if
          subdone(isr) = .false.
       end do

       do while( .not. subdone(osr))
          ! use subregion index in master array to retrieve crop name from subregion array
          isr = mandatbs(osr)%mandate(pdx(osr))%sr + 1
          if( .not. subdone(isr) ) then
             ! difference between present date and subregion date
             daydif = difdat(mandatbs(osr)%mandate(pdx(osr))%d, mandatbs(osr)%mandate(pdx(osr))%m, &
                                mandatbs(osr)%mandate(pdx(osr))%y, &
                                mandatbs(isr)%mandate(pdx(isr))%d, mandatbs(isr)%mandate(pdx(isr))%m, &
                                mandatbs(isr)%mandate(pdx(isr))%y + add_yr(isr))
             if( daydif .eq. 0 ) then
                ! The dates match, copy crop name from subregion to master region
                mandatbs(osr)%mandate(pdx(osr)) = mandatbs(isr)%mandate(pdx(isr))
                ! increment indexes
                if( pdx(osr) .lt. udx(osr) ) then
                   pdx(osr) = pdx(osr) + 1
                   if( pdx(isr) .lt. udx(isr) ) then
                      ! index is less than maximum
                      ! bump index to next date
                      pdx(isr) = pdx(isr) + 1
                   else ! pdx(isr) .eq. udx(isr)
                      if( idx_cycles(isr) .lt. cnt_cycles(isr) ) then
                         pdx(isr) = ldx(isr)
                         add_yr(isr) = add_yr(isr) + mandatbs(isr)%mperod
                         idx_cycles(isr) = idx_cycles(isr) + 1
                      else ! idx_cycles(isr) .eq. cnt_cycles(isr)
                         subdone(isr) = .true.
                      end if
                   end if
                else ! pdx(osr) .eq. udx(osr)
                   subdone(osr) = .true.
                end if
             else
                ! dates do not match, error in method
                write(*,*)"Date Error in sync_harvcropnames: subregion, day, month, year are not equal"
                write(*,*) osr, mandatbs(osr)%mandate(pdx(osr))%d, mandatbs(osr)%mandate(pdx(osr))%m, &
                                mandatbs(osr)%mandate(pdx(osr))%y
                write(*,*) isr, mandatbs(isr)%mandate(pdx(isr))%d, mandatbs(isr)%mandate(pdx(isr))%m, &
                                mandatbs(isr)%mandate(pdx(isr))%y + add_yr(isr)

                stop 1
             end if
          end if
       end do
    end subroutine sync_harvcropnames

    subroutine allmandates( mandatbs )

       use datetime_mod, only: difdat

       type(mandate_array), dimension(:) :: mandatbs

       integer :: npass       ! counter 1 = count dates prior to allocation, 2 = populate date array
       integer :: alloc_stat
       integer :: sum_stat

       integer :: ld, lm, ly  ! day, month, year values
       integer :: daydif      ! result from difdat
       integer :: mindif      ! minimum result from difdat
       integer :: minsr       ! index of mindif subregion

       integer :: isr   ! subregion loop index
       integer :: lsr   ! lower subregion index
       integer :: usr   ! upper subregion index
       integer :: osr   ! the composite 0 "subregion" index

       integer :: cnt_dates   ! count of dates
       integer :: cnt_match   ! number of subregions whose dates matched on this step
       integer :: cnt_remain  ! number of subregions with dates not yet processed
       logical, dimension(:), allocatable :: subdone ! logical .true. indicates that the last date has been checked
       integer, dimension(:), allocatable :: ldx    ! lower index for each subregion date array
       integer, dimension(:), allocatable :: udx    ! upper index for each subregion date array
       integer, dimension(:), allocatable :: pdx    ! present index for each subregion date array

       integer, dimension(:), allocatable :: cnt_cycles    ! number of cycles for each subregion date array to fill 0 array cycle
       integer, dimension(:), allocatable :: idx_cycles    ! index indicating the present subregion date array cycle
       integer, dimension(:), allocatable :: add_yr    ! number of years too add subregion date to make it match multiple cycles
       logical, save :: first_entry = .true.
       if ( .not. first_entry) then
       !if( allocated( mandatbs(lbound(mandatbs,1))%mandate ) ) then
          ! already allocated so values are already populated (calibration mode)
          write(*,*) 'ALLMANDATES already allocated'
          return
       end if

       first_entry = .false.

       ! note: this structure is passed with the (0) index array used to hold the result
       osr = lbound(mandatbs,1)
       lsr = osr + 1
       usr = ubound(mandatbs,1)

       sum_stat = 0
       ! allocate indexes into each mandate array
       allocate( subdone(osr:usr), stat = alloc_stat )
       sum_stat = sum_stat + alloc_stat
       allocate( ldx(osr:usr), stat = alloc_stat )
       sum_stat = sum_stat + alloc_stat
       allocate( udx(osr:usr), stat = alloc_stat )
       sum_stat = sum_stat + alloc_stat
       allocate( pdx(osr:usr), stat = alloc_stat )
       sum_stat = sum_stat + alloc_stat
       allocate( cnt_cycles(lsr:usr), stat = alloc_stat )
       sum_stat = sum_stat + alloc_stat
       allocate( idx_cycles(lsr:usr), stat = alloc_stat )
       sum_stat = sum_stat + alloc_stat
       allocate( add_yr(lsr:usr), stat = alloc_stat )
       sum_stat = sum_stat + alloc_stat
       if( sum_stat .gt. 0 ) then
          write(*,*) 'ERROR: unable to allocate memory in allmandates'
          stop 1
       end if

       do npass = 1, 2

        do isr = lsr, usr
          ldx(isr) = lbound(mandatbs(isr)%mandate,1)
          udx(isr) = ubound(mandatbs(isr)%mandate,1)
          pdx(isr) = ldx(isr)
          cnt_cycles(isr) = mandatbs(osr)%mperod / mandatbs(isr)%mperod
          idx_cycles(isr) = 1
          add_yr(isr) = 0
          subdone(isr) = .false.
        end do
        subdone(osr) = .false.

        ! find first date
        ld = mandatbs(lsr)%mandate(ldx(lsr))%d
        lm = mandatbs(lsr)%mandate(ldx(lsr))%m
        ly = mandatbs(lsr)%mandate(ldx(lsr))%y
        do isr = lsr+1, usr
          if( difdat(ld, lm, ly, mandatbs(isr)%mandate(ldx(isr))%d, mandatbs(isr)%mandate(ldx(isr))%m, &
                                 mandatbs(isr)%mandate(ldx(isr))%y) .lt. 0 ) then
             ! replace with earlier date
             ld = mandatbs(isr)%mandate(ldx(isr))%d
             lm = mandatbs(isr)%mandate(ldx(isr))%m
             ly = mandatbs(isr)%mandate(ldx(isr))%y
          end if             
        end do
        cnt_dates = 0
        do while( .not. subdone(osr))
          ! Find all subregion dates that match and add to array and increment subregion date array index
          cnt_match = 0
          do isr = lsr, usr
             if( .not. subdone(isr) ) then
                ! difference between present date and subregion date
                daydif = difdat(ld, lm, ly, mandatbs(isr)%mandate(pdx(isr))%d, mandatbs(isr)%mandate(pdx(isr))%m, &
                                            mandatbs(isr)%mandate(pdx(isr))%y + add_yr(isr))
                if( daydif .eq. 0 ) then
                   ! This date matches present date, add to array
                   cnt_dates = cnt_dates + 1
                   cnt_match = cnt_match + 1
                   !write(*,*) 'cnt_dates, ld, lm, ly:', cnt_dates, ld, lm, ly 
                   if( npass .eq. 2 ) then
                      if( pdx(osr) .gt. udx(osr) ) then
                         ! invalid array index
                         write(*,*) 'mandate array index out of bounds in allmandates'
                         stop 1
                      end if
                      ! assign subregion date and names to array including all subregions
                      mandatbs(osr)%mandate(pdx(osr)) = mandatbs(isr)%mandate(pdx(isr))
                      ! bump subregion year to match sequence
                      mandatbs(osr)%mandate(pdx(osr))%y = mandatbs(isr)%mandate(pdx(isr))%y + add_yr(isr)
                      ! increment index
                      pdx(osr) = pdx(osr) + 1
                   end if
                   if( pdx(isr) .lt. udx(isr) ) then
                      ! index is less than maximum
                      ! bump index to next date
                      pdx(isr) = pdx(isr) + 1
                   else ! pdx(isr) .eq. udx(isr)
                      !write(*,*) 'isr, idx_cycles(isr), cnt_cycles(isr):', isr, idx_cycles(isr), cnt_cycles(isr)
                      if( idx_cycles(isr) .lt. cnt_cycles(isr) ) then
                         pdx(isr) = ldx(isr)
                         add_yr(isr) = add_yr(isr) + mandatbs(isr)%mperod
                         idx_cycles(isr) = idx_cycles(isr) + 1
                      else ! idx_cycles(isr) .eq. cnt_cycles(isr)
                         subdone(isr) = .true.
                      end if
                   end if
                end if
             end if
          end do
          if( cnt_match .eq. 0 ) then
             cnt_remain = 0
             ! select next date
             mindif = huge(mindif)
             do isr = lsr, usr
                if( .not. subdone(isr) ) then
                   ! difference between present date and subregion date
                   daydif = difdat(ld, lm, ly, mandatbs(isr)%mandate(pdx(isr))%d, mandatbs(isr)%mandate(pdx(isr))%m, &
                                               mandatbs(isr)%mandate(pdx(isr))%y + add_yr(isr))
                   if( daydif .lt. mindif) then
                      ! select subregion index with minimum value
                      mindif = daydif
                      minsr = isr
                   end if
                   cnt_remain = cnt_remain + 1
                end if
             end do
             if( cnt_remain .gt. 0 ) then
                !set new date from selected subregion
                ld = mandatbs(minsr)%mandate(pdx(minsr))%d
                lm = mandatbs(minsr)%mandate(pdx(minsr))%m
                ly = mandatbs(minsr)%mandate(pdx(minsr))%y + add_yr(minsr)
             else  ! cnt_remain .eq. 0 so all dates have been checked
                subdone(osr) = .true.
             end if
          end if
        end do

        if( npass .eq. 1 ) then
          ! create 0 index mandate array
          !write(*,*) 'create 0 mandate count:', cnt_dates
          call create_mandate( cnt_dates, mandatbs(lbound(mandatbs,1))%mandate )

          ldx(osr) = lbound(mandatbs(osr)%mandate,1)
          udx(osr) = ubound(mandatbs(osr)%mandate,1)
          pdx(osr) = ldx(osr)
        end if

       end do  ! npass loop

    end subroutine allmandates

! return the total number of periods based upon the number of years
! for the rotation and the dates of the operations.

! Hmm, looks like I have it checking for "end periods".
! We will change it to look for "start periods" and also have
! the regular "start periods" be the 1st and 15th days of each month.
! (we will use day 15 as a starting day since many ops will be
! scheduled then)

    FUNCTION get_nperiods (nrot_yrs, mandate)

        IMPLICIT NONE

        INTEGER :: get_nperiods
        INTEGER :: nrot_yrs            ! Number of rotation years
        type(opercrop_date), dimension(:), intent(in) :: mandate ! array of mandates from management file

        INTEGER :: nperiods            ! Number of periods
        INTEGER :: minperiods = 24     ! Minimum number of periods

        INTEGER :: i                                ! local loop variable


        ! Example Operation Dates (d,m,y) - must be in date order
!        INTEGER, DIMENSION(3,24) :: man_dates =             &
!                RESHAPE ( (/ 1,1,1,  2,1,1,  5,1,1, 14,1,1, 15,1,1, 16, 1,1,   &
!                            31,1,1,  1,2,1,  2,2,1, 13,2,1, 14,2,1, 15, 2,1,   &
!                            28,2,1, 29,2,1,  1,3,1, 23,3,1, 23,3,1, 30,12,1,   &
!                            31,12,1, 1,2,2, 11,3,2,  7,5,2, 18,7,2, 30,12,3 /),&
!                        (/ 3,24 /) )

        nperiods = minperiods * nrot_yrs          ! Minimum total periods

! This looks for "end dates" rather than "starting dates"
!        DO i = 1, size(mandate) 
!                IF (mandate(i)%y <= nrot_yrs) THEN
!
!                        ! Check if 1st day of month
!                        IF (mandate(i)%d == 1 ) THEN
!                                CONTINUE
!                        ! Check if middle day of month
!                        ELSE IF ((mandate(i)%d == 16) .AND.               &
!                                 (mandate(i)%m /= 2))        THEN
!                                CONTINUE
!                        ! Check if middle day of month (Feb)
!                        ELSE IF ((mandate(i)%d == 15) .AND.               &
!                                 (mandate(i)%m == 2))        THEN
!                                CONTINUE
!                        ! Check if operation date .NE. previous operation date
!                        ELSE IF ((i /= 1) .AND.                             &
!                                 (mandate(i)%d == mandate(i-1)%d) .AND. &
!                                  (mandate(i)%m == mandate(i-1)%m) .AND. &
!                                 (mandate(i)%y == mandate(i-1)%y)) THEN
!                                CONTINUE
!                        ! We have a new period
!                        ELSE
!                                nperiods = nperiods + 1
!!                                print *, mandate(i)%d,                    &
!!                                        mandate(i)%m, mandate(i)%y
!                        END IF
!                END IF
!        END DO


        ! This will look for "start date" matches for elimination
        ! of possible new periods.  Also, we will assume the "end date"
        ! of normal periods is the 14th day of each month.

        DO i = 1, size(mandate) 
            IF (mandate(i)%y <= nrot_yrs) THEN
               ! Check if not 1st day of month and
               ! not middle start period day of month
               IF ((mandate(i)%d == 1 ) .OR. (mandate(i)%d == 15)) THEN
                  ! print *, "No new period (day == 1 or day == 15)"
                  CONTINUE               ! no new period here
                  ! Check if operation date .NE. previous operation date
               ELSE IF (i /= 1) THEN 
                  IF ((mandate(i)%d == mandate(i-1)%d) .AND. &
                     (mandate(i)%m == mandate(i-1)%m) .AND. &
                     (mandate(i)%y == mandate(i-1)%y)) THEN
                    !print *, "No new period (2nd op on this date)"
                    CONTINUE               ! no new period here
                  ELSE                       ! We must have a new period
                    nperiods = nperiods + 1
                  END IF
               ELSE
                  nperiods = nperiods +1
               END IF
            END IF
        END DO

        get_nperiods = nperiods

    END FUNCTION get_nperiods

END MODULE mandate_mod
