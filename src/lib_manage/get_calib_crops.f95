!$Author$
!$Date$
!$Revision$
!$HeadURL$

! Must be called before calibration reports
SUBROUTINE get_calib_crops(sr)

    USE generic_list , ONLY : Link_Ptr_Type, Link_Type, List_Type
    USE generic_list , ONLY : LI_Init_List, LI_Add_To_Head
    USE generic_list , ONLY : LI_Get_Head, LI_Remove_Head
    USE generic_list , ONLY : LI_Get_Next, LI_Associated
    USE generic_list , ONLY : LI_Get_Len

    USE calib_crop_m

    IMPLICIT NONE


!   + + + ARGUMENT DECLARATIONS + + +
    INTEGER :: sr

!   + + + ARGUMENT DEFINITIONS + + +
!   sr    - subregion number

!   + + + PARAMETERS AND COMMON BLOCKS + + +
    include 'p1werm.inc'
    include 'm1flag.inc'
    include 'main/main.inc'
    include 'c1info.inc'
    include 'c1gen.inc'
    include 'c1db1.inc'
    include 'command.inc'

!   + + + LOCAL DECLARATIONS + + +

    LOGICAL, save :: firstime = .TRUE.  ! Initialize linked list only once

    IF (calibrate_crops == 0) RETURN  ! Calibration is not being done.

    IF (got_all_calib_crops) RETURN   ! No need to find the crops if we already have them

    IF (acbaflg(sr) == 0) RETURN       ! crop not flagged for calibration

    IF (firstime) THEN
        CALL LI_Init_List(Calib_Crop_List)
        firstime = .FALSE.
    END IF

    ! Check to see if we already have this crop
    ! If so, stop looking for crops to add to calibration list (set "got_all_calib_crops" flag)
    CLink = LI_Get_Head(Calib_Crop_List)
    DO WHILE (LI_Associated(CLink))
       Calib_Crop = TRANSFER(CLink, Calib_Crop)
       IF (Calib_Crop%CP%CData%calib_crop_info%crop_name == ac0nam(sr)(1:len_trim(ac0nam(sr))) .and.    &
           Calib_Crop%CP%CData%calib_crop_info%harv_day == lopday .and.                                 &
           Calib_Crop%CP%CData%calib_crop_info%harv_month == lopmon .and.                               &
           Calib_Crop%CP%CData%calib_crop_info%harv_rotyear == lopyr ) THEN

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
    Calib_Crop%CP%CData%calib_crop_info%crop_name = ac0nam(sr)(1:len_trim(ac0nam(sr)))
    Calib_Crop%CP%CData%calib_crop_info%plant_day = aplant_day(sr)
    Calib_Crop%CP%CData%calib_crop_info%plant_month = aplant_month(sr)
    Calib_Crop%CP%CData%calib_crop_info%plant_rotyear = aplant_rotyr(sr)
    Calib_Crop%CP%CData%calib_crop_info%harv_day = lopday
    Calib_Crop%CP%CData%calib_crop_info%harv_month = lopmon
    Calib_Crop%CP%CData%calib_crop_info%harv_rotyear = lopyr
    Calib_Crop%CP%CData%calib_crop_info%bio_adj_val = acbaf(sr)
    Calib_Crop%CP%CData%calib_crop_info%target_yield = (acytgt(sr)/acycon(sr)) * (1.0-(acywct(sr)/100.0))
    CLink = TRANSFER (Calib_Crop, CLink)
    CALL LI_Add_To_Head (CLink, Calib_Crop_List)

    ! CALL print_calib_crop(6,Calib_Crop%CP%CData%calib_crop_info)

    print *, "Found another crop to calibrate, total so far is: ", calib_crop_cnt

    RETURN
    END
