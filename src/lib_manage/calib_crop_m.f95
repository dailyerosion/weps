!$Author$
!$Date$
!$Revision$
!$HeadURL$

MODULE calib_crop_m

  USE generic_list , ONLY : Link_Ptr_Type, Link_Type, List_Type

  IMPLICIT NONE

  LOGICAL :: got_all_calib_crops = .FALSE.    ! Set .TRUE. once we find all crops to calibrate

  LOGICAL, ALLOCATABLE, DIMENSION (:) :: first_full_cycle   ! Array of flags - set .TRUE once we have gotten
                                                            ! a full cycle's worth of estimated yields

  INTEGER :: calib_crop_cnt = 0               ! Count number of crops to calibrate

  REAL, TARGET, ALLOCATABLE, DIMENSION (:,:) :: est_yield          ! estimated yield for each crop (by calib_cycle)
  REAL, TARGET, ALLOCATABLE, DIMENSION (:,:) :: est_adj    ! estimated biomass adjustment for each crop (by calib_cycle)
  REAL, ALLOCATABLE, DIMENSION (:) :: new_adj              ! storage for new biomass adjustment value for each crop

  TYPE :: Array_Ptrs
     REAL, POINTER :: low_ptr
     REAL, POINTER :: high_ptr
  END TYPE Array_Ptrs

  TYPE (Array_Ptrs), ALLOCATABLE, DIMENSION (:) :: bracket_adj   ! bracket structure to hold lower/upper ptrs to adj values
  TYPE (Array_Ptrs), ALLOCATABLE, DIMENSION (:) :: bracket_yield ! bracket structure to hold lower/upper ptrs to yield values

! REAL, POINTER :: low_ptr(10)         ! Keeps track of lower bracketed bio_adj values for each calib crop
! REAL, POINTER :: high_ptr(10)        ! Keeps track of upper bracketed bio_adj values for each calib crop

  TYPE :: calib_crop_type
    CHARACTER(len=80) :: crop_name = ""
    INTEGER   :: idx = 0
    INTEGER   :: plant_day = -1
    INTEGER   :: plant_month = -1
    INTEGER   :: plant_rotyear = -1
    INTEGER   :: harv_day = -2
    INTEGER   :: harv_month = -2
    INTEGER   :: harv_rotyear = -2
    REAL      :: bio_adj_val = -1.0
    REAL      :: target_yield = -2.0 ! dry wt in WEPS metric units (kg/m^2)
  END TYPE calib_crop_type

  TYPE :: calib_yield_type
    INTEGER   :: rot_no = 0
    INTEGER   :: cycle_no = 0
    REAL      :: bio_adj_val = -1.0
    REAL      :: harv_yield = -2.0 ! dry wt in WEPS metric units (kg/m^2)
    TYPE (calib_crop_type), POINTER :: crop_ptr => NULL() 
  END TYPE calib_yield_type
  !not used? 
 !TYPE (calib_yield_type), Dimension (:), TARGET, ALLOCATABLE :: ydata

!-------------------------------
    ! My-defined list element
    ! The Link_Type field MUST be the FIRST in the my-defined list element
    ! Note pointer to data so as to easily create sublists
    TYPE My_Type
      TYPE (Link_Type) :: CLink
      TYPE (My_Data_Type), POINTER :: CData
    END TYPE My_Type

    TYPE My_Data_Type
      INTEGER :: Index
      TYPE (calib_crop_type) calib_crop_info
    END TYPE My_Data_Type

    ! Auxilliary data type required for the transfer function
    TYPE My_Ptr_Type
      TYPE (My_Type), POINTER :: CP
    END TYPE My_Ptr_Type

    TYPE (List_Type)           :: Calib_Crop_List
    TYPE (Link_Ptr_Type)       :: CLink
    TYPE (My_Ptr_Type), TARGET :: Calib_Crop

!-------------------------------
    TYPE My_Type2
      TYPE (Link_Type) :: YLink
      TYPE (My_Data_Type2), POINTER :: YData
    END TYPE My_Type2

    TYPE My_Data_Type2
      INTEGER :: Index
      TYPE (calib_yield_type) calib_yield_info
    END TYPE My_Data_Type2

    ! Auxilliary data type required for the transfer function
    TYPE My_Ptr_Type2
      TYPE (My_Type2), POINTER :: YP
    END TYPE My_Ptr_Type2

    TYPE (List_Type)      :: Calib_Yield_List, Sub_Calib_Yield_List
    TYPE (Link_Ptr_Type)  :: YLink, Sub_YLink
    TYPE (My_Ptr_Type2)   :: Calib_Yield, Sub_Calib_Yield
!-------------------------------

  CONTAINS

    SUBROUTINE print_calib_crop (unit, var)
      INTEGER,intent(in) :: unit
      TYPE (calib_crop_type),intent(in) :: var

      WRITE(unit,*)                                        &
        var%idx, var%crop_name(1:len_trim(var%crop_name)), &
        " plant(d/m/ry) ",                                 &
        var%plant_day, var%plant_month, var%plant_rotyear, &
        " harv(d/m/ry) ",                                  &
        var%harv_day, var%harv_month, var%harv_rotyear,    &
        var%bio_adj_val, var%target_yield
      RETURN
    END SUBROUTINE print_calib_crop

    SUBROUTINE print_calib_yield (unit, var)
      INTEGER,intent(in) :: unit
      TYPE (calib_yield_type),intent(in) :: var

      WRITE(unit,fmt='(2(1x,i4),2(1x,f10.6))',ADVANCE='NO')&
        var%rot_no, var%cycle_no,                          &
        var%bio_adj_val, var%harv_yield
      CALL print_calib_crop(unit,var%crop_ptr)
      RETURN
    END SUBROUTINE print_calib_yield

END MODULE calib_crop_m
