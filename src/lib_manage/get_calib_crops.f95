!$Author$
!$Date$
!$Revision$
!$HeadURL$

! Must be called before calibration reports
SUBROUTINE get_calib_crops(sr, crop)

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
    type(biomatter), intent(in) :: crop    ! structure containing full crop description

!   + + + ARGUMENT DEFINITIONS + + +
!   sr    - subregion number

!   + + + PARAMETERS AND COMMON BLOCKS + + +
    include 'p1werm.inc'
    include 'command.inc'

!   + + + LOCAL DECLARATIONS + + +

    LOGICAL, save :: firstime = .TRUE.  ! Initialize linked list only once

    IF (calibrate_crops == 0) RETURN  ! Calibration is not being done.

    IF (got_all_calib_crops) RETURN   ! No need to find the crops if we already have them

    IF (crop%database%baflg == 0) RETURN       ! crop not flagged for calibration

    IF (firstime) THEN
        CALL LI_Init_List(Calib_Crop_List)
        firstime = .FALSE.
    END IF

    ! Check to see if we already have this crop
    ! If so, stop looking for crops to add to calibration list (set "got_all_calib_crops" flag)
    CLink = LI_Get_Head(Calib_Crop_List)
    DO WHILE (LI_Associated(CLink))
       Calib_Crop = TRANSFER(CLink, Calib_Crop)
       IF (Calib_Crop%CP%CData%calib_crop_info%crop_name == trim(crop%bname) .and. &
           Calib_Crop%CP%CData%calib_crop_info%harv_day == lastoper(sr)%day .and.  &
           Calib_Crop%CP%CData%calib_crop_info%harv_month == lastoper(sr)%mon .and. &
           Calib_Crop%CP%CData%calib_crop_info%harv_rotyear == lastoper(sr)%yr ) THEN

             ! Print out complete list of crops to be calibrated
             CLink = LI_Get_Head(Calib_Crop_List)
             DO WHILE (LI_Associated(CLink))
                Calib_Crop = TRANSFER(CLink, Calib_Crop)
                WRITE (6,fmt='(a4,i3)',ADVANCE='no') " idx", Calib_Crop%CP%CData%Index
                CALL print_calib_crop(6,Calib_Crop%CP%CData%calib_crop_info)
                CLink = LI_Get_Next(CLink)
             END DO

             got_all_calib_crops = .TRUE.  ! Set flag to signify that we have found all crops requiring calibration
             print *, "Got all calibration crops identified (flag,cnt)", got_all_calib_crops, calib_crop_cnt
             RETURN
       END IF
       CLink = LI_Get_Next(CLink)
    END DO

    calib_crop_cnt = calib_crop_cnt + 1                  ! Must have another crop flagged for calibration

    ALLOCATE (Calib_Crop%CP); ALLOCATE (Calib_Crop%CP%CData)
    Calib_Crop%CP%CData%Index = calib_crop_cnt
    Calib_Crop%CP%CData%calib_crop_info%idx = calib_crop_cnt
    Calib_Crop%CP%CData%calib_crop_info%crop_name = trim(crop%bname)
    Calib_Crop%CP%CData%calib_crop_info%plant_day = crop%database%plant_day
    Calib_Crop%CP%CData%calib_crop_info%plant_month = crop%database%plant_month
    Calib_Crop%CP%CData%calib_crop_info%plant_rotyear = crop%database%plant_rotyr
    Calib_Crop%CP%CData%calib_crop_info%harv_day = lastoper(sr)%day
    Calib_Crop%CP%CData%calib_crop_info%harv_month = lastoper(sr)%mon
    Calib_Crop%CP%CData%calib_crop_info%harv_rotyear = lastoper(sr)%yr
    Calib_Crop%CP%CData%calib_crop_info%bio_adj_val = crop%database%baf
    Calib_Crop%CP%CData%calib_crop_info%target_yield = (crop%database%ytgt/crop%database%ycon) * (1.0-(crop%database%ywct/100.0))
    CLink = TRANSFER (Calib_Crop, CLink)
    CALL LI_Add_To_Head (CLink, Calib_Crop_List)

    ! CALL print_calib_crop(6,Calib_Crop%CP%CData%calib_crop_info)

    print *, "Found another crop to calibrate, total so far is: ", calib_crop_cnt

    RETURN
    END
