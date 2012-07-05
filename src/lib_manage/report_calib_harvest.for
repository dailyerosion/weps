!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine report_calib_harvest(sr,bmrotation,mass_rem, mass_left)

!     + + + ARGUMENT DECLARATIONS + + +
      integer sr, bmrotation
      real mass_rem, mass_left

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
      include 'p1const.inc'
      include 'file.inc'
      include 'm1flag.inc'
      include 'main/main.inc'
      include 'c1info.inc'
      include 'c1gen.inc'
      include 'c1db1.inc'
      include 'c1report.inc'
      include 'manage/oper.inc'

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

 1002 format(1x,i2,' ',i2,' ',i2,' ',i2,'|')

      if (init_loop .or. report_loop) then  ! not a calibrating cycle
          ! set to the beginning of simulation
          ! to eliminate newline at beginning of file
          cprevcalibrotation(sr) = 1
          RETURN
      else if (acbaflg(sr) == 0) then       ! crop not flagged for calibration
          RETURN
      endif

! We have a crop flagged for calibration and this is a calibration cycle

      !Start a new line if this is the next rotation cycle
      if (bmrotation .gt. cprevcalibrotation(sr))  then
         write(unit=luoharvest_calib,fmt="(a)") ''        ! write newline
         write(unit=luoharvest_calib_parm,fmt="(a)") ''   ! write newline
      end if

      !Start a new line if this is the next calib_cycle
      if ((bmrotation .eq. cprevcalibrotation(sr))                      &
     &        .and. (prev_calib_cycle .ne. calib_cycle) ) then
         if (prev_calib_cycle .ne. -1) then             ! planting operation
            write(unit=luoharvest_calib,fmt="(a)") ''        ! write newline
            write(unit=luoharvest_calib_parm,fmt="(a)") ''   ! write newline
         end if
         prev_calib_cycle = calib_cycle                 ! keep prev cycle
      end if

      !Update every time we have a crop flagged to get newline in the right place (hopefully)
      cprevcalibrotation(sr) = bmrotation


! Only harvest triggers the following print statements

      !Print out the "calibration cycle" and "rotation year within cycle"
      write(unit=luoharvest_calib, fmt=1000,advance='NO')               &
     &      calib_cycle, bmrotation
      write(unit=luoharvest_calib_parm, fmt=1001,advance='NO')          &
     &      calib_cycle

      !Print out the "planting" and "harvest" dates and "crop name"
      write(unit=luoharvest_calib,fmt=1015,advance='NO')                &
     &      aplant_day(sr), aplant_month(sr), aplant_rotyr(sr),         &
     &      lopday, lopmon, lopyr,                                      &
     &      ac0nam(sr)(1:len_trim(ac0nam(sr)))
      write(unit=luoharvest_calib_parm,fmt=1015,advance='NO')           &
     &      aplant_day(sr), aplant_month(sr), aplant_rotyr(sr),         &
     &      lopday, lopmon, lopyr,                                      &
     &      ac0nam(sr)(1:len_trim(ac0nam(sr)))

      tot_mass = mass_rem + mass_left
      if (tot_mass .le. 0.0) then
          harvest_index = 0.0
      else
          harvest_index = mass_rem/tot_mass
      end if

      !Print out "dry weight yield removed" and "residue left"
      write(unit=luoharvest_calib,fmt=1020,advance='NO')                &
     &      mass_rem, 'kg/m^2', mass_left, 'kg/m^2'

      !Print out "harvest index", "wet weight yield" and "yield water content"
      write(unit=luoharvest_calib,fmt=1030,advance='NO')                &
     &      harvest_index, "HI",                                        &
     &      mass_rem/(1.0-(acywct(sr)/100.0)), "kg/m^2",                &
     &      acywct(sr), '% H2O'

      !Print out "biomass adj factor", "yield adj factor",
      !          "target yield" and "target yield units"
      write(unit=luoharvest_calib,fmt=1040,advance='NO')                &
     &    acbaflg(sr),acbaf(sr),acyraf(sr),acytgt(sr),trim(acynmu(sr))
      write(unit=luoharvest_calib_parm,fmt=1041,advance='NO')           &
     &    acbaf(sr)

      !Print out "wet target yield" (metric) and "dry target yield" (metric)
      write(unit=luoharvest_calib,fmt=1050,advance='NO')                &
     &    acytgt(sr)/acycon(sr), 'kg/m^2',                              &
     &    (acytgt(sr)/acycon(sr)) * (1.0-(acywct(sr)/100.0)), 'kg/m^2'


      return
      end
