!$Author$
!$Date$
!$Revision$
!$HeadURL$

SUBROUTINE set_calib(sr)

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

!   + + + LOCAL DECLARATIONS + + +

    INTEGER :: c_no = 0
    LOGICAL, save :: firstime = .TRUE.   ! Initialize linked list only once

    IF (.not. got_all_calib_crops) RETURN   ! Don't do anything if all crops not identified

    IF (acbaflg(sr) == 0) RETURN       ! crop not flagged for calibration

    ! Check to see if we already have this crop
    ! If so, stop looking for crops to add to calibration list (set "got_all_calib_crops" flag)
    CLink = LI_Get_Head(Calib_Crop_List)
    DO WHILE (LI_Associated(CLink))
       Calib_Crop = TRANSFER(CLink, Calib_Crop)
       IF (Calib_Crop%CP%CData%calib_crop_info%crop_name == ac0nam(sr)(1:len_trim(ac0nam(sr))) .and.     &
           Calib_Crop%CP%CData%calib_crop_info%plant_day == lopday .and.                                 &
           Calib_Crop%CP%CData%calib_crop_info%plant_month == lopmon .and.                               &
           Calib_Crop%CP%CData%calib_crop_info%plant_rotyear == lopyr ) THEN

             c_no = Calib_Crop%CP%CData%calib_crop_info%idx
             IF (.not. ALLOCATED (first_full_cycle)) RETURN  ! Obviously can't be done with cycle 1 yet
             IF (.not. first_full_cycle(c_no)) RETURN 

             print *, "Found calibration crop to adjust at planting time"
             print *, "Setting crop no: ",c_no,"bio_adj value from: ",acbaf(sr),"to: ",new_adj(c_no)
             acbaf(sr) = new_adj(c_no)
             RETURN
       END IF
       CLink = LI_Get_Next(CLink)
    END DO


    RETURN
    END
