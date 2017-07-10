!$Author$
!$Date$
!$Revision$
!$HeadURL$

SUBROUTINE get_calib_yield(sr,rotation_no,mass_removed, mass_left, crop)

    use weps_main_mod, only: init_loop, report_loop, max_calib_cycles, calib_cycle, calib_done
    use weps_interface_defs, ignore_me=>get_calib_yield
    USE generic_list , ONLY : Link_Ptr_Type, Link_Type, List_Type
    USE generic_list , ONLY : LI_Init_List, LI_Add_To_Head
    USE generic_list , ONLY : LI_Get_Head, LI_Remove_Head
    USE generic_list , ONLY : LI_Get_Next, LI_Associated
    USE generic_list , ONLY : LI_Get_Len

    USE calib_crop_m
    use biomaterial, only: biomatter
    use manage_data_struct_defs, only: lastoper

    IMPLICIT NONE


!   + + + ARGUMENT DECLARATIONS + + +
    INTEGER :: sr
    INTEGER :: rotation_no
    REAL    :: mass_removed
    REAL    :: mass_left
    type(biomatter), intent(inout) :: crop    ! structure containing full crop description

!   + + + ARGUMENT DEFINITIONS + + +
!   sr           - subregion number
!   rotation_no  - rotation count updated in manage.for
!   mass_removed - mass removed by the harvest process
!   mass_left    - mass left behind by the harvest process

!   + + + PARAMETERS AND COMMON BLOCKS + + +
    include 'p1werm.inc'
    include 'command.inc'

!   + + + LOCAL DECLARATIONS + + +

      ! Counter to keep track of the initial number of calls to this routine
      ! Linked list is initialized on the first call
    INTEGER, save :: no_get_calib_yield_call = 0
      ! Flag to identify status of dynamic data allocation
      ! allocate all necessary dynamic data arrays when all crops have been identified
    LOGICAL, save :: initial_allocation_done = .FALSE.

      ! Array of flags - when target yield has been bracketed.
    LOGICAL, SAVE, ALLOCATABLE, DIMENSION (:) :: yield_bracketed
      ! Array of flags - to signify first time in bisection code.
    LOGICAL, SAVE, ALLOCATABLE, DIMENSION (:) :: first_bisection
      ! Array of flags - to signify when each crop has been calibrated.
    LOGICAL, SAVE, ALLOCATABLE, DIMENSION (:) :: crop_calib_done

    INTEGER, save :: calib_yield_cnt = 0 ! Count number of crops to calibrate

    INTEGER :: c_no = 0                  ! crop index no
    REAL    :: t_yld = 0.0               ! target yield
    REAL    :: FACTOR = 1.6              ! factor adjustment for bracketing
    INTEGER :: i = 0                     ! index counter
    INTEGER :: n = 0                     ! counter
    INTEGER :: status = 0                ! ALLOCATE return status
    REAL    :: dx

    REAL :: total_mass
    REAL :: harvest_index

!!! TYPE (calib_crop_type) :: var

    IF (crop%database%baflg == 0) RETURN           ! crop not flagged for calibration
    IF (init_loop .or. report_loop) RETURN ! not a calibrating cycle

    no_get_calib_yield_call = no_get_calib_yield_call + 1

    print *, "(get_calib_yield) no_get_calib_yield_call, calib_crop_cnt: ", &
             no_get_calib_yield_call, calib_crop_cnt

    IF (no_get_calib_yield_call == 1) THEN
        CALL LI_Init_List(Calib_Yield_List)

        ! Doing a check here for info purposes
        IF (got_all_calib_crops) THEN
           print *, "All crops identified for calibration in first get_calib_yield call (flag,cnt): ", &
                    got_all_calib_crops, calib_crop_cnt
        ELSE
           print *, "Don't yet have all crops identified for calibration in first get_calib_yield call (flag,cnt): ", &
                    got_all_calib_crops, calib_crop_cnt
        END IF
    END IF

!   IF (no_get_calib_yield_call >= 2) THEN
!   IF ( (no_get_calib_yield_call >= 2) .and. (.not. got_all_calib_crops) ) THEN 
!          print *, "Don't yet have all crops identified for calibration yet: ", &
!                    "# crops", calib_crop_cnt, "rot #", rotation_no,            &
!                    "# get_calib_yield calls", no_get_calib_yield_call
!   ELSE

    IF ( (no_get_calib_yield_call >= 1) .and. (.not. got_all_calib_crops) ) THEN 
           print *, "Don't yet have all crops identified for calibration yet: ", &
                     "got_all_calib_crops flag", got_all_calib_crops,            &
                     "# crops", calib_crop_cnt, "rot #", rotation_no,            &
                     "# get_calib_yield calls", no_get_calib_yield_call
    END IF

!       IF (.not. got_all_calib_crops) THEN 
!           write(6,*) 'Do not have all crops identified for calibration - cannot allocate space'
!           call exit(-1)
!       END IF

    IF ( (.not. initial_allocation_done) .and. (got_all_calib_crops) ) THEN 

       ! Get space for first_full_cycle flags
       IF (.not. ALLOCATED (first_full_cycle)) THEN
           ALLOCATE (first_full_cycle(calib_crop_cnt), stat=status)
           IF (status /= 0) THEN
              print *, "Can't allocate first_full_cycle flags", " Status: ", status
           ELSE
              DO i = 1, calib_crop_cnt
                 first_full_cycle(i) = .FALSE.
              END DO
           END IF
       END IF

       ! Get space for estimated yields
       IF (.not. ALLOCATED (est_yield)) THEN
           ALLOCATE (est_yield(calib_crop_cnt,max_calib_cycles), stat=status)
           IF (status /= 0) THEN
              print *, "Can't allocate est_yield", " Status: ", status
              call exit(-1)
           END IF
       END IF
       ! Get space for estimated adj values
       IF (.not. ALLOCATED (est_adj)) THEN
           ALLOCATE (est_adj(calib_crop_cnt,max_calib_cycles), stat=status)
           IF (status /= 0) THEN
              print *, "Can't allocate est_adj", " Status: ", status
              call exit(-1)
           END IF
       END IF
       ! Get space for new adj value
       IF (.not. ALLOCATED (new_adj)) THEN
           ALLOCATE (new_adj(calib_crop_cnt), stat=status)
           IF (status /= 0) THEN
              print *, "Can't allocate est_adj", " Status: ", status
           END IF
       END IF
       IF (.not. ALLOCATED (bracket_adj)) THEN
       ! Get space for bracket_adj ptrs
           ALLOCATE (bracket_adj(calib_crop_cnt), stat=status)
           IF (status /= 0) THEN
              print *, "Can't allocate bracket_adj ptrs", " Status: ", status
           END IF
       END IF
       IF (.not. ALLOCATED (bracket_yield)) THEN
       ! Get space for bracket_yield ptrs
           ALLOCATE (bracket_yield(calib_crop_cnt), stat=status)
           IF (status /= 0) THEN
              print *, "Can't allocate bracket_yield ptrs", " Status: ", status
           END IF
       END IF

       ! Get space for yield bracket flags
       IF (.not. ALLOCATED (yield_bracketed)) THEN
           ALLOCATE (yield_bracketed(calib_crop_cnt), stat=status)
           IF (status /= 0) THEN
              print *, "Can't allocate yield bracket flags", " Status: ", status
           ELSE
              DO i = 1, calib_crop_cnt
                 yield_bracketed(i) = .FALSE.
              END DO
           END IF
       END IF
       ! Get space for first_bisection flags
       IF (.not. ALLOCATED (first_bisection)) THEN
           ALLOCATE (first_bisection(calib_crop_cnt), stat=status)
           IF (status /= 0) THEN
              print *, "Can't allocate first_bisection flags", " Status: ", status
           ELSE
              DO i = 1, calib_crop_cnt
                 first_bisection(i) = .FALSE.
              END DO
           END IF
       END IF
       ! Get space for crop calib_done flags
       IF (.not. ALLOCATED (crop_calib_done)) THEN
           ALLOCATE (crop_calib_done(calib_crop_cnt), stat=status)
           IF (status /= 0) THEN
              print *, "Can't allocate crop calib_done flags", " Status: ", status
           ELSE
              DO i = 1, calib_crop_cnt
                 crop_calib_done(i) = .FALSE.
              END DO
           END IF
       END IF
       initial_allocation_done = .TRUE.
    END IF


! Track yield info from each harvest for all crops selected for calibration
    total_mass = mass_removed + mass_left
    IF (total_mass .le. 0.0) THEN
          harvest_index = 0.0
    ELSE
          harvest_index = mass_removed/total_mass
    END IF

    calib_yield_cnt = calib_yield_cnt + 1  ! Have another crop yield to include
    ALLOCATE (Calib_Yield%YP); ALLOCATE (Calib_Yield%YP%YData)
    Calib_Yield%YP%YData%Index = calib_yield_cnt
    Calib_Yield%YP%YData%calib_yield_info%rot_no = rotation_no
    Calib_Yield%YP%YData%calib_yield_info%cycle_no = calib_cycle
    Calib_Yield%YP%YData%calib_yield_info%bio_adj_val = crop%database%baf
    Calib_Yield%YP%YData%calib_yield_info%harv_yield = mass_removed

    ! Find "crop calibration info" for "this crop's harvest"
         ! Calib_Yield%YP%YData%calib_yield_info%crop_ptr = 
    CLink = LI_Get_Head(Calib_Crop_List)
    DO WHILE (LI_Associated(CLink))
       Calib_Crop = TRANSFER(CLink, Calib_Crop)
       IF (Calib_Crop%CP%CData%calib_crop_info%crop_name == trim(crop%bname) .and. &
           Calib_Crop%CP%CData%calib_crop_info%harv_day == lastoper(sr)%day .and. &
           Calib_Crop%CP%CData%calib_crop_info%harv_month == lastoper(sr)%mon .and. &
           Calib_Crop%CP%CData%calib_crop_info%harv_rotyear == lastoper(sr)%yr ) THEN
               Calib_Yield%YP%YData%calib_yield_info%crop_ptr => Calib_Crop%CP%CData%calib_crop_info 
       END IF
       CLink = LI_Get_Next(CLink)
    END DO

    YLink = TRANSFER (Calib_Yield, YLink)
    CALL LI_Add_To_Head (YLink, Calib_Yield_List)

  ! CALL print_calib_yield(6,Calib_Yield%YP%YData%calib_yield_info)

    ! Would like to print this at the "end" of each calibrate cycle only
    ! Need to catch it immediately after the last harvest so we can adjust
    ! parameter values for any crop that is "planted" in the last year of a
    ! rotation and "harvested" in the first year of the rotation prior to
    ! the next "planting"( so the next harvest yield will be "adjusted").

    IF (calibrate_rotcycles == rotation_no)  THEN   !Done with this crop, adjust bioflag

        c_no = Calib_Yield%YP%YData%calib_yield_info%crop_ptr%idx
        t_yld = Calib_Yield%YP%YData%calib_yield_info%crop_ptr%target_yield

        first_full_cycle(c_no) = .TRUE.

        ! Make a sublist for a single crop
        CALL LI_Init_List(Sub_Calib_Yield_List)
        YLink = LI_Get_Head(Calib_Yield_List)
        DO WHILE(LI_Associated(YLink))
           Calib_Yield = TRANSFER(YLink, Calib_Yield)
           IF ((Calib_Yield%YP%YData%calib_yield_info%crop_ptr%idx == c_no) .and.    &
               (Calib_Yield%YP%YData%calib_yield_info%cycle_no == calib_cycle)) THEN 
              ALLOCATE(Sub_Calib_Yield%YP)
              Sub_Calib_Yield%YP%YData => Calib_Yield%YP%YData
              Sub_YLink = TRANSFER(Sub_Calib_Yield, Sub_YLink)
              CALL LI_Add_To_Head(Sub_YLink, Sub_Calib_Yield_List)
           END IF
           YLink = LI_Get_Next(YLink)
        END DO

       ! Print out the sub-list of actual harvest yields (current calibration crop only)
       Sub_YLink = LI_Get_Head(Sub_Calib_Yield_List)
       DO WHILE (LI_Associated(Sub_YLink))
          Sub_Calib_Yield = TRANSFER(Sub_YLink, Sub_Calib_Yield)
          WRITE (6,fmt='(i5)',ADVANCE='no') Sub_Calib_Yield%YP%YData%Index
          CALL print_calib_yield(6,Sub_Calib_Yield%YP%YData%calib_yield_info)
          Sub_YLink = LI_Get_Next(Sub_YLink)
       END DO

!!! !        IF (got_all_calib_crops) THEN
!!!           print *, "Printing all calibration crop/harvest records here"
!!!       CLink = LI_Get_Head(Calib_Crop_List)
!!!       WRITE(6,*) 'Printing all "crop/harvest" records in the list'
!!!       DO WHILE (LI_Associated(CLink))
!!!          Calib_Crop = TRANSFER(CLink, Calib_Crop)
!!! ! This works 
!!!       var = Calib_Crop%CP%CData%calib_crop_info
!!!       WRITE(6,*)                                        &
!!!         var%idx, var%crop_name(1:len_trim(var%crop_name)), &
!!!         " plant(d/m/ry) ",                                 &
!!!         var%plant_day, var%plant_month, var%plant_rotyear, &
!!!         " harv(d/m/ry) ",                                  &
!!!         var%harv_day, var%harv_month, var%harv_rotyear,    &
!!!         var%bio_adj_val, var%target_yield
!!! 
!!!        Sub_YLink = LI_Get_Head(Sub_Calib_Yield_List)
!!!        DO WHILE (LI_Associated(Sub_YLink))
!!!           Sub_Calib_Yield = TRANSFER(Sub_YLink, Sub_Calib_Yield)
!!!           WRITE (6,fmt='(i5)',ADVANCE='no') Sub_Calib_Yield%YP%YData%Index
!!!           CALL print_calib_yield(6,Sub_Calib_Yield%YP%YData%calib_yield_info)
!!!           Sub_YLink = LI_Get_Next(Sub_YLink)
!!!        END DO
!!! 
!!!          CLink = LI_Get_Next(CLink)
!!!       END DO
!!!           !CALL print_all_calib_crops(6)
!!!  !       END IF


       ! Compute average yield
       n = 0
       est_adj(c_no,calib_cycle) = 0.0
       est_yield(c_no,calib_cycle) = 0.0
       Sub_YLink = LI_Get_Head(Sub_Calib_Yield_List)
       DO WHILE (LI_Associated(Sub_YLink))
          Sub_Calib_Yield = TRANSFER(Sub_YLink, Sub_Calib_Yield)
          IF (Sub_Calib_Yield%YP%YData%calib_yield_info%bio_adj_val > 0.0) THEN  ! Don't include uninitialized values
              n = n + 1
              est_adj(c_no,calib_cycle) = est_adj(c_no,calib_cycle) +         &
                      Sub_Calib_Yield%YP%YData%calib_yield_info%bio_adj_val
              est_yield(c_no,calib_cycle) = est_yield(c_no,calib_cycle) +     &
                        Sub_Calib_Yield%YP%YData%calib_yield_info%harv_yield
          END IF
          Sub_YLink = LI_Get_Next(Sub_YLink)
       END DO
       est_adj(c_no,calib_cycle) = est_adj(c_no,calib_cycle)/n
       est_yield(c_no,calib_cycle) = est_yield(c_no,calib_cycle)/n
       PRINT *, "estimated adj and yield", est_adj(c_no,calib_cycle),est_yield(c_no,calib_cycle)

       ! Quit playing around if we are within tolerance
       IF (abs(est_yield(c_no,calib_cycle) - t_yld) <= (t_yld * 0.05)) THEN
          new_adj(c_no) = est_adj(c_no,calib_cycle)
          print *, "Done calibrating Crop no: ", c_no
          crop_calib_done(c_no) = .TRUE.
          ! Check to see if all crops have been calibrated yet.  If so, set global "calib_done" flag
          calib_done = .TRUE.
          DO i = 1, calib_crop_cnt
             print *, "calib done flags (i,calib_crop_cnt,crop_calib_done(i),calib_done): ", &
                      i, calib_crop_cnt, crop_calib_done(i), calib_done
             IF (.not. crop_calib_done(i)) THEN
                 calib_done = .FALSE.
             END IF
          END DO
          IF (calib_done) write(6,*) "Done calibrating all crops!"
          RETURN 
       END IF


       ! Check to see if we have done 2 cycles (bracketed desired answer) yet
       IF (calib_cycle < 2) THEN
          IF ( (est_yield(c_no,calib_cycle) < t_yld) ) THEN
             new_adj(c_no) = est_adj(c_no,calib_cycle) * FACTOR      ! Initial guess for 2nd calibration cycle run
             print *, "Cycle 1: Crop no: ",c_no,"Est. Yield ", est_yield(c_no,calib_cycle), &
                      "is low (",t_yld,"), reset acbaf from: ", crop%database%baf,"to: ", new_adj(c_no)
          ELSE
             new_adj(c_no) = est_adj(c_no,calib_cycle) / FACTOR      ! Initial guess for 2nd calibration cycle run
             print *, "Cycle 1: Crop no: ",c_no,"Est. Yield ", est_yield(c_no,calib_cycle), &
                      "is high (",t_yld,"), reset acbaf from: ", crop%database%baf,"to: ", new_adj(c_no)
          END IF

       ELSE IF (.not. yield_bracketed(c_no)) THEN

          IF (abs(est_yield(c_no,calib_cycle) - t_yld) <= (t_yld * 0.05)) THEN
             crop_calib_done(c_no) = .TRUE.
             print *, "Already or still 'calibrated'!"
             goto 90 !already_calibrated
          ELSE
             crop_calib_done(c_no) = .FALSE.
          END IF

          IF ( ( (est_yield(c_no,calib_cycle-1) > t_yld) .and. (est_yield(c_no,calib_cycle) < t_yld) ) .or.   &
               ( (est_yield(c_no,calib_cycle-1) < t_yld) .and. (est_yield(c_no,calib_cycle) > t_yld) ) ) THEN
             yield_bracketed(c_no) = .TRUE.
             first_bisection(c_no) = .TRUE.
             print *, "Cycle ", calib_cycle,": Crop no: ", c_no, ": Yield has now been bracketed!", &
                      "[",est_yield(c_no,calib_cycle), t_yld, est_yield(c_no,calib_cycle-1),"]" 

          ELSE IF ( (est_yield(c_no,calib_cycle) < t_yld) .and. (est_yield(c_no,calib_cycle-1) < t_yld) ) THEN
             print *, "est_adj(c_no,calib_cycle) and est_adj(c_no,calib_cycle-1) are: ", &
                      est_adj(c_no,calib_cycle), est_adj(c_no,calib_cycle-1)
             IF ( est_yield(c_no,calib_cycle) > est_yield(c_no,calib_cycle-1) ) THEN
                new_adj(c_no) = est_adj(c_no,calib_cycle) * FACTOR
                print *, "Cycle ",calib_cycle,": Crop no: ", c_no,"Est. Yield (not bracketed)", &
                         est_yield(c_no,calib_cycle), "and ", &
                         est_yield(c_no,calib_cycle-1), "are low (",t_yld,"), ", &
                         "reset acbaf from: ", crop%database%baf, "to: ", new_adj(c_no)
             ELSE
                new_adj(c_no) = est_adj(c_no,calib_cycle-1) * FACTOR
                print *, "Cycle ",calib_cycle-1,": Crop no: ", c_no,"Est. Yield (not bracketed)", &
                         est_yield(c_no,calib_cycle), "and ", &
                         est_yield(c_no,calib_cycle-1), "are low (",t_yld,"), ", &
                         "reset acbaf from: ", crop%database%baf, "to: ", new_adj(c_no)
             END IF

          ELSE IF ( (est_yield(c_no,calib_cycle) > t_yld) .and. (est_yield(c_no,calib_cycle-1) > t_yld) ) THEN
             IF ( est_yield(c_no,calib_cycle) < est_yield(c_no,calib_cycle-1) ) THEN
                new_adj(c_no) = est_adj(c_no,calib_cycle) / FACTOR
                print *, "Cycle ",calib_cycle,": Crop no: ", c_no,"Est. Yield (not bracketed)", &
                         est_yield(c_no,calib_cycle), "and ", &
                         est_yield(c_no,calib_cycle-1), "are high (",t_yld,"), ", &
                         "reset acbaf from: ", crop%database%baf, "to: ", new_adj(c_no)
             ELSE
                new_adj(c_no) = est_adj(c_no,calib_cycle-1) / FACTOR
                print *, "Cycle ",calib_cycle-1,": Crop no: ", c_no,"Est. Yield (not bracketed)", &
                         est_yield(c_no,calib_cycle), "and ", &
                         est_yield(c_no,calib_cycle-1), "are high (",t_yld,"), ", &
                         "reset acbaf from: ", crop%database%baf, "to: ", new_adj(c_no)
             END IF
          END IF


       ELSE
          !We have been previously bracketed, but we need to check that we still are bracketed
          !before continuing on from here.  If we are no longer bracketed, we need to reset the
          !"yield_bracketed(c_no)" flag back to false and do whatever is required to re-bracket
          !the yield again for this crop.

          print *, "Cycle ", calib_cycle,": Yield has previously been bracketed (checking to ensure it still is)"

          IF (abs(est_yield(c_no,calib_cycle) - t_yld) <= (t_yld * 0.05)) THEN
             crop_calib_done(c_no) = .TRUE.
             print *, "Already or still 'calibrated'!"
             goto 90 !already_calibrated
          ELSE
             crop_calib_done(c_no) = .FALSE.
          END IF

! Bad test here - we should be looking at more "est_yields", e.g. the current high and low values, not just the new and previous values!!!
!          IF ( ( (est_yield(c_no,calib_cycle-1) > t_yld) .and. (est_yield(c_no,calib_cycle) < t_yld) ) .or.   &
!               ( (est_yield(c_no,calib_cycle-1) < t_yld) .and. (est_yield(c_no,calib_cycle) > t_yld) ) ) THEN
!             print *, "Cycle ", calib_cycle,": Crop no: ", c_no, ": Yield is still bracketed!", &
!                      "[",est_yield(c_no,calib_cycle), t_yld, est_yield(c_no,calib_cycle-1),"]" 


          ! We are no longer bracketed?
          IF ( ( (est_yield(c_no,calib_cycle) > t_yld) .and. &
                 (bracket_yield(c_no)%low_ptr > t_yld) .and. &
                 (bracket_yield(c_no)%high_ptr > t_yld) ) .or. &
               ( (est_yield(c_no,calib_cycle) < t_yld) .and. &
                 (bracket_yield(c_no)%low_ptr < t_yld) .and. &
                 (bracket_yield(c_no)%high_ptr < t_yld) ) ) THEN

!          ELSE
             yield_bracketed(c_no) = .FALSE.
             first_bisection(c_no) = .TRUE.
             print *, "Cycle ", calib_cycle,": Crop no: ", c_no, ": Yield is is no longer bracketed! ", &
                      "[",est_yield(c_no,calib_cycle), t_yld, est_yield(c_no,calib_cycle-1),"]" 

             IF ( (est_yield(c_no,calib_cycle) < t_yld) .and. (est_yield(c_no,calib_cycle-1) < t_yld) ) THEN
                print *, "est_adj(c_no,calib_cycle) and est_adj(c_no,calib_cycle-1) are: ", &
                         est_adj(c_no,calib_cycle), est_adj(c_no,calib_cycle-1)
                IF ( est_yield(c_no,calib_cycle) > est_yield(c_no,calib_cycle-1) ) THEN
                   new_adj(c_no) = est_adj(c_no,calib_cycle) * (FACTOR-0.55)
                   print *, "Cycle ",calib_cycle,": Crop no: ", c_no,"Est. Yield (not bracketed)", &
                         est_yield(c_no,calib_cycle), "and ", &
                         est_yield(c_no,calib_cycle-1), "are low (",t_yld,"), ", &
                         "reset acbaf from: ", crop%database%baf, "to: ", new_adj(c_no)
                ELSE
                   new_adj(c_no) = est_adj(c_no,calib_cycle-1) * (FACTOR-0.55)
                   print *, "Cycle ",calib_cycle-1,": Crop no: ", c_no,"Est. Yield (not bracketed)", &
                         est_yield(c_no,calib_cycle), "and ", &
                         est_yield(c_no,calib_cycle-1), "are low (",t_yld,"), ", &
                         "reset acbaf from: ", crop%database%baf, "to: ", new_adj(c_no)
                END IF

             ELSE IF ( (est_yield(c_no,calib_cycle) > t_yld) .and. (est_yield(c_no,calib_cycle-1) > t_yld) ) THEN
                IF ( est_yield(c_no,calib_cycle) < est_yield(c_no,calib_cycle-1) ) THEN
                   new_adj(c_no) = est_adj(c_no,calib_cycle) / (FACTOR-0.55)
                   print *, "Cycle ",calib_cycle,": Crop no: ", c_no,"Est. Yield (not bracketed)", &
                         est_yield(c_no,calib_cycle), "and ", &
                         est_yield(c_no,calib_cycle-1), "are high (",t_yld,"), ", &
                         "reset acbaf from: ", crop%database%baf, "to: ", new_adj(c_no)
                ELSE
                   new_adj(c_no) = est_adj(c_no,calib_cycle-1) / (FACTOR-0.55)
                   print *, "Cycle ",calib_cycle-1,": Crop no: ", c_no,"Est. Yield (not bracketed)", &
                         est_yield(c_no,calib_cycle), "and ", &
                         est_yield(c_no,calib_cycle-1), "are high (",t_yld,"), ", &
                         "reset acbaf from: ", crop%database%baf, "to: ", new_adj(c_no)
                END IF
             END IF
             print *, "Cycle ", calib_cycle,": Crop no: ", c_no, ": Yield is still bracketed! [low, target, high, new] yields:", &
                      "[",bracket_yield(c_no)%low_ptr, t_yld, bracket_yield(c_no)%high_ptr, est_yield(c_no,calib_cycle),"]" 
          END IF
           continue ! we "may" have been "bracketed" 
       END IF
       
       IF (yield_bracketed(c_no)) THEN
           print *, "In bracketing code"
           IF (first_bisection(c_no)) THEN
             print *, "In first_bisection"
             IF (est_yield(c_no,calib_cycle) < t_yld) THEN
                 print *, "Case A"
                 bracket_adj(c_no)%low_ptr => est_adj(c_no,calib_cycle)
                 bracket_adj(c_no)%high_ptr => est_adj(c_no,calib_cycle-1)
                 bracket_yield(c_no)%low_ptr => est_yield(c_no,calib_cycle)
                 bracket_yield(c_no)%high_ptr => est_yield(c_no,calib_cycle-1)
             ELSE
                 print *, "Case B"
                 bracket_adj(c_no)%high_ptr => est_adj(c_no,calib_cycle)
                 bracket_adj(c_no)%low_ptr => est_adj(c_no,calib_cycle-1)
                 bracket_yield(c_no)%high_ptr => est_yield(c_no,calib_cycle)
                 bracket_yield(c_no)%low_ptr => est_yield(c_no,calib_cycle-1)
             END IF
             first_bisection(c_no) = .FALSE.
           ELSE
             print *, "Not in first_bisection"
                ! Check to see if 
             IF (est_yield(c_no,calib_cycle) < t_yld) THEN !(est_yield(c_no,calib_cycle) > bracket(c_no)%low_ptr)
                 print *, "Case C"
                 bracket_adj(c_no)%low_ptr => est_adj(c_no,calib_cycle)
                 bracket_yield(c_no)%low_ptr => est_yield(c_no,calib_cycle)
             ELSE IF (est_yield(c_no,calib_cycle) > t_yld) THEN !(est_yield(c_no,calib_cycle) < bracket(c_no)%high_ptr) 
                 print *, "Case D"
                 bracket_adj(c_no)%high_ptr => est_adj(c_no,calib_cycle)
                 bracket_yield(c_no)%high_ptr => est_yield(c_no,calib_cycle)
             END IF
           END IF
           dx = bracket_adj(c_no)%high_ptr - bracket_adj(c_no)%low_ptr 
           new_adj(c_no) = bracket_adj(c_no)%low_ptr + (dx * 0.5)
           print *, "High, Low, New: ", bracket_adj(c_no)%high_ptr, &
                    bracket_adj(c_no)%low_ptr, new_adj(c_no)
       END IF      

90     continue  !If we jump here, we had a crop that is already "within tolerance", e.g. "calibrated"

    END IF

    RETURN
    END
