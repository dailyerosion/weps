!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine report_harvest( sr, bmrotation, mass_rem, mass_left,   &
     &                           harv_unit_flg, mandate, crop )

      use weps_interface_defs
      use mandate_mod, only: opercrop_date
      use file_io_mod, only: luoharvest_si, luoharvest_en
      use p1unconv_mod, only: KG_per_M2_to_LBS_per_ACRE
      use biomaterial, only: biomatter
      use manage_data_struct_defs, only: lastoper

!     + + + ARGUMENT DECLARATIONS + + +
      integer sr, bmrotation
      real mass_rem, mass_left
      integer harv_unit_flg
      type(opercrop_date), dimension(:), intent(inout) :: mandate
      type(biomatter), intent(inout) :: crop    ! structure containing full crop description

!     + + + ARGUMENT DEFINITIONS + + +
!     sr    - subregion number
!     bmrotation - rotation count updated in manage.for
!     mass_rem - mass removed by the harvest process
!     mass_left - mass left behind by the harvest process
!     harv_unit_flg - overide units given in crop record
!                0  - use units given in crop record
!                1  - use lb/ac or kg/m^2

!     + + + PARAMETERS AND COMMON BLOCKS + + +
      include 'p1werm.inc'
      include 'm1flag.inc'
      include 'c1gen.inc'
      include 'c1report.inc'

!     + + + LOCAL DECLARATIONS + + +
      real tot_mass, harvest_index
      logical match
      integer i

 1000 format(1x,i2,'/',i2,'/',i2,'|',a,'|',                             &
     &       f12.3,'|',a,'|',f12.3,'|',a,'|',                           &
     &       f6.3,'|',a,'|',f12.3,'|',a,'|',f5.1,'|',a,'|')
 1001 format(a)

      if( init_loop .or. calib_loop ) then  !initilizing or calibrating cycle

        ! set to the beginning of simulation
        ! to eliminate newline at beginning of file
        cprevrotation(sr) = 1

        if( init_loop ) then  !initilizing cycle
          ! attach a crop name and id as harvest operation in stir report
          call stir_crop(sr, crop%bname, 2)
        end if

      else  !done when initializing and calibrating cycle(s) are completed

        if( bmrotation .gt. cprevrotation(sr) ) then
          ! write newline
          write(unit=luoharvest_si(sr),fmt=1001) ''
          write(unit=luoharvest_en(sr),fmt=1001) ''
        end if

        tot_mass = mass_rem + mass_left
        if( tot_mass .le. 0.0 ) then
          harvest_index = 0.0
        else
          harvest_index = mass_rem/tot_mass
        end if

        write(unit=luoharvest_si(sr),fmt=1000,advance='NO')             &
     &      lastoper(sr)%day, lastoper(sr)%mon, lastoper(sr)%yr,        &
     &      trim(crop%bname),                                           &
     &      mass_rem, 'kg/m^2',                                         &
     &      mass_left, 'kg/m^2',                                        &
     &      harvest_index, "Harvest Index",                             &
     &      mass_rem / ( 1.0-acywct(sr)/100.0 ),                        &
     &      'kg/m^2',                                                   &
     &      acywct(sr), 'percent water'

        if( harv_unit_flg .eq. 0 ) then
          ! the conversion is from dry mass to wet weight
          ! and from kg/m^2 to acynmu units
          write(unit=luoharvest_en(sr),fmt=1000,advance='NO')           &
     &      lastoper(sr)%day, lastoper(sr)%mon, lastoper(sr)%yr,        &
     &      trim(crop%bname),                                           &
     &      mass_rem*KG_per_M2_to_LBS_per_ACRE, 'lb/ac',                &
     &      mass_left*KG_per_M2_to_LBS_per_ACRE, 'lb/ac',               &
     &      harvest_index, "Harvest Index",                             &
     &      mass_rem * acycon(sr) / ( 1.0-acywct(sr)/100.0 ),           &
     &      acynmu(sr)(1:len_trim(acynmu(sr))),                         &
     &      acywct(sr), 'percent water'
        else
          ! the conversion is from dry mass to wet weight
          ! and from kg/m^2 to lbs/ac units
          write(unit=luoharvest_en(sr),fmt=1000,advance='NO')           &
     &      lastoper(sr)%day, lastoper(sr)%mon, lastoper(sr)%yr,        &
     &      trim(crop%bname),                                           &
     &      mass_rem*KG_per_M2_to_LBS_per_ACRE, 'lb/ac',                &
     &      mass_left*KG_per_M2_to_LBS_per_ACRE, 'lb/ac',               &
     &      harvest_index, "Harvest Index",                             &
     &      mass_rem*KG_per_M2_to_LBS_per_ACRE/( 1.0-acywct(sr)/100.0 ),&
     &      'lb/ac',                                                    &
     &      acywct(sr), 'percent water'
        end if

! Update the mandate structure so that 'harvest' operations
! will list the 'crop name' harvested on that date
! If only one cycle is run, without an initialization cylce
! then any harvest done in the first year of a crop planted in the last
! year will not get tagged with the crop to be harvested.
! This scenario shouldn't occur in most situations since an init cycle
! is standard.  The 1st harvest in that scenario doesn't actual "harvest"
! a crop anyway.  Thus, we do 2 cycles just in case we missed it on the
! 1st cycle when no init cycles are run.

         IF (bmrotation .LE. 2) THEN   ! Need 2 cycles to get all crops
           match = .false.
           DO i = 1, size(mandate)
             IF ((mandate(i)%d == lastoper(sr)%day) .and.               &
     &           (mandate(i)%m == lastoper(sr)%mon) .and.               &
     &           (mandate(i)%y == lastoper(sr)%yr)) THEN

               IF(trim(mandate(i)%opname)==trim(lastoper(sr)%name)) THEN
                       mandate(i)%cropname = trim(adjustl(crop%bname))
                       match = .true.
               END IF
              END IF
              IF (match) THEN
                 EXIT  ! leave do loop
              END IF
           END DO
         END IF

        ! attach a crop name to the harvest operation in stir report
        call stir_crop(sr, crop%bname, 2)

        ! updated every call to get newline in right place
        cprevrotation(sr) = bmrotation

      end if

      return
      end
