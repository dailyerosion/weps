!$Author$
!$Date$
!$Revision$
!$HeadURL$

module report_harvest_mod

  integer, dimension(:), allocatable :: cprevrotation ! rotation count number previously printed in crop harvest report

  integer, dimension(:), allocatable :: cprevcalibrotation ! rotation count number previously printed in calibration crop harvest report


  contains

    subroutine report_harvest( sr, bmrotation, mass_rem, mass_left,   &
     &                           harv_unit_flg, harv_report_flg,        &
     &                           mandate, crop )

      use weps_main_mod, only: init_loop, calib_loop
      use mandate_mod, only: opercrop_date
      use file_io_mod, only: luoharvest_si, luoharvest_en
      use p1unconv_mod, only: KG_per_M2_to_LBS_per_ACRE
      use biomaterial, only: biomatter
      use manage_data_struct_defs, only: lastoper

!     + + + ARGUMENT DECLARATIONS + + +
      integer sr, bmrotation
      real mass_rem, mass_left
      integer harv_unit_flg
      integer harv_report_flg
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
!     harv_report_flg - harvest reporting flag
!                0 - do not report harvest
!                1 - report harvest

!     + + + PARAMETERS AND COMMON BLOCKS + + +
      include 'p1werm.inc'
      include 'm1flag.inc'

!     + + + LOCAL DECLARATIONS + + +
      real tot_mass, harvest_index
      logical match
      integer i

 1000 format(1x,i2,'/',i2,'/',i2,'|',i2,'|',a,'|',                      &
     &       f12.3,'|',a,'|',f12.3,'|',a,'|',                           &
     &       f6.3,'|',a,'|',f12.3,'|',a,'|',f5.1,'|',a,'|')
 1001 format(a)

      if( init_loop .or. calib_loop ) then  !initilizing or calibrating cycle

        ! set to the beginning of simulation
        ! to eliminate newline at beginning of file
        cprevrotation(sr) = 1

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
     &      harv_report_flg, trim(crop%bname),                          &
     &      mass_rem, 'kg/m^2',                                         &
     &      mass_left, 'kg/m^2',                                        &
     &      harvest_index, "Harvest Index",                             &
     &      mass_rem / ( 1.0-crop%database%ywct/100.0 ),                &
     &      'kg/m^2',                                                   &
     &      crop%database%ywct, 'percent water'

        if( harv_unit_flg .eq. 0 ) then
          ! the conversion is from dry mass to wet weight
          ! and from kg/m^2 to acynmu units
          write(unit=luoharvest_en(sr),fmt=1000,advance='NO')           &
     &      lastoper(sr)%day, lastoper(sr)%mon, lastoper(sr)%yr,        &
     &      harv_report_flg, trim(crop%bname),                          &
     &      mass_rem*KG_per_M2_to_LBS_per_ACRE, 'lb/ac',                &
     &      mass_left*KG_per_M2_to_LBS_per_ACRE, 'lb/ac',               &
     &      harvest_index, "Harvest Index",                             &
     &      mass_rem*crop%database%ycon/(1.0-crop%database%ywct/100.0), &
     &      crop%database%ynmu(1:len_trim(crop%database%ynmu)),         &
     &      crop%database%ywct, 'percent water'
        else
          ! the conversion is from dry mass to wet weight
          ! and from kg/m^2 to lbs/ac units
          write(unit=luoharvest_en(sr),fmt=1000,advance='NO')           &
     &      lastoper(sr)%day, lastoper(sr)%mon, lastoper(sr)%yr,        &
     &      harv_report_flg, trim(crop%bname),                          &
     &      mass_rem*KG_per_M2_to_LBS_per_ACRE, 'lb/ac',                &
     &      mass_left*KG_per_M2_to_LBS_per_ACRE, 'lb/ac',               &
     &      harvest_index, "Harvest Index", mass_rem *                  &
     &      KG_per_M2_to_LBS_per_ACRE/( 1.0-crop%database%ywct/100.0 ), &
     &      'lb/ac',                                                    &
     &      crop%database%ywct, 'percent water'
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

        ! updated every call to get newline in right place
        cprevrotation(sr) = bmrotation

      end if

      return
    end subroutine report_harvest

    subroutine report_calib_harvest(sr,bmrotation,mass_rem, mass_left, crop)

      use weps_main_mod, only: init_loop, report_loop
      use file_io_mod, only: luoharvest_calib, luoharvest_calib_parm
      use biomaterial, only: biomatter
      use manage_data_struct_defs, only: lastoper

!     + + + ARGUMENT DECLARATIONS + + +
      integer sr, bmrotation
      real mass_rem, mass_left
      type(biomatter), intent(in) :: crop    ! structure containing full crop description

!     + + + ARGUMENT DEFINITIONS + + +
!     sr    - subregion number
!     bmrotation - rotation count updated in manage.for
!     mass_rem - mass removed by the harvest process
!     mass_left - mass left behind by the harvest process

!     NOTE:  This routine will print out the planting date
!            of a crop first, followed by the harvest date
!            crop name and then yield and calibration info.


!     + + + PARAMETERS AND COMMON BLOCKS + + +
      include 'p1werm.inc'
      include 'm1flag.inc'

!     + + + LOCAL DECLARATIONS + + +
      real tot_mass, harvest_index

      integer, save :: prev_calib_cycle = -1

 1000 format(1x,i4,1x,i4,1x,'|')
 1001 format(1x,i4,'|')
 1015 format(1x,i2,'/',i2,'/',i2,'|',i2,'/',i2,'/',i2,'|',a,'|')
 1020 format(f12.3,'|',a,'|',f12.3,'|',a,'|')
 1030 format(f6.3,'|',a,'|',f12.3,'|',a,'|',f5.1,'|',a,'|')
 1040 format(i2,'|',g10.4,'|',f6.3,'|',f12.3,'|',a,'|')
 1041 format(g10.4,'|')
 1050 format(f12.3,'|',a,'|',f12.3,'|',a,'|')

      if (init_loop .or. report_loop) then  ! not a calibrating cycle
          ! set to the beginning of simulation
          ! to eliminate newline at beginning of file
          cprevcalibrotation(sr) = 1
          RETURN
      else if (crop%database%baflg == 0) then       ! crop not flagged for calibration
          RETURN
      endif

! We have a crop flagged for calibration and this is a calibration cycle

      !Start a new line if this is the next rotation cycle
      if (bmrotation .gt. cprevcalibrotation(sr))  then
         write(unit=luoharvest_calib(sr),fmt="(a)") ''        ! write newline
         write(unit=luoharvest_calib_parm(sr),fmt="(a)") ''   ! write newline
      end if

      !Start a new line if this is the next calib_cycle
      if ((bmrotation .eq. cprevcalibrotation(sr))                      &
     &        .and. (prev_calib_cycle .ne. calib_cycle) ) then
         if (prev_calib_cycle .ne. -1) then             ! planting operation
            write(unit=luoharvest_calib(sr),fmt="(a)") ''        ! write newline
            write(unit=luoharvest_calib_parm(sr),fmt="(a)") ''   ! write newline
         end if
         prev_calib_cycle = calib_cycle                 ! keep prev cycle
      end if

      !Update every time we have a crop flagged to get newline in the right place (hopefully)
      cprevcalibrotation(sr) = bmrotation


! Only harvest triggers the following print statements

      !Print out the "calibration cycle" and "rotation year within cycle"
      write(unit=luoharvest_calib(sr), fmt=1000,advance='NO')           &
     &      calib_cycle, bmrotation
      write(unit=luoharvest_calib_parm(sr), fmt=1001,advance='NO')      &
     &      calib_cycle

      !Print out the "planting" and "harvest" dates and "crop name"
      write(unit=luoharvest_calib(sr),fmt=1015,advance='NO')            &
     &      crop%database%plant_day, crop%database%plant_month, crop%database%plant_rotyr, &
     &      lastoper(sr)%day, lastoper(sr)%mon, lastoper(sr)%yr, trim(crop%bname)
      write(unit=luoharvest_calib_parm(sr),fmt=1015,advance='NO')       &
     &      crop%database%plant_day, crop%database%plant_month, crop%database%plant_rotyr, &
     &      lastoper(sr)%day, lastoper(sr)%mon, lastoper(sr)%yr, trim(crop%bname)

      tot_mass = mass_rem + mass_left
      if (tot_mass .le. 0.0) then
          harvest_index = 0.0
      else
          harvest_index = mass_rem/tot_mass
      end if

      !Print out "dry weight yield removed" and "residue left"
      write(unit=luoharvest_calib(sr),fmt=1020,advance='NO')            &
     &      mass_rem, 'kg/m^2', mass_left, 'kg/m^2'

      !Print out "harvest index", "wet weight yield" and "yield water content"
      write(unit=luoharvest_calib(sr),fmt=1030,advance='NO')            &
     &      harvest_index, "HI",                                        &
     &      mass_rem/(1.0-(crop%database%ywct/100.0)), "kg/m^2", &
     &      crop%database%ywct, '% H2O'

      !Print out "biomass adj factor", "yield adj factor",
      !          "target yield" and "target yield units"
      write(unit=luoharvest_calib(sr),fmt=1040,advance='NO')            &
     &    crop%database%baflg,crop%database%baf,crop%database%yraf,crop%database%ytgt,trim(crop%database%ynmu)
      write(unit=luoharvest_calib_parm(sr),fmt=1041,advance='NO')       &
     &    crop%database%baf

      !Print out "wet target yield" (metric) and "dry target yield" (metric)
      write(unit=luoharvest_calib(sr),fmt=1050,advance='NO')            &
     &    crop%database%ytgt/crop%database%ycon, 'kg/m^2', &
     &    (crop%database%ytgt/crop%database%ycon) * (1.0-(crop%database%ywct/100.0)), 'kg/m^2'


      return
    end subroutine report_calib_harvest

end module report_harvest_mod
