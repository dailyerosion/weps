!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!
      subroutine openfils
! ***************************************************************** wjr
! Contains init code from main
!
!       Edit History
!       10-Mar-99       wjr     created
!
      include 'file.inc'              !Logical unit numbers defined here
      include 'p1werm.inc'
      include 'wpath.inc'

      include 'm1flag.inc'
      include 'command.inc'


! + + + Called functions + + +
      integer ios

!     the main output file is opened at all times


      call fopenk (luogui1, rootp(1:len_trim(rootp)) // 'gui1_data.out',&
     &        'unknown')

      call fopenk (luomandate,rootp(1:len_trim(rootp)) // 'mandate.out',&
     &        'unknown')


!     the harvest report ouput files are opened at all times

      call fopenk (luoharvest_si,rootp(1:len_trim(rootp)) //            &
     &    'harvest_si.out', 'unknown')
      call fopenk (luoharvest_en,rootp(1:len_trim(rootp)) //            &
     &    'harvest_en.out', 'unknown')

!     the hydrobal report ouput file is opened at all times

      call fopenk(luohydrobal,rootp(1:len_trim(rootp)) //               &
     &   'hydrobal.out', 'unknown')

!     seasonal summaries of yield and biomass
      call fopenk (luoseason,                                           &
     &         rootp(1:len_trim(rootp)) // 'season.out', 'unknown')


      if (calibrate_crops .gt. 0) then
          ! calibration harvest output file
          call fopenk (luoharvest_calib,rootp(1:len_trim(rootp)) //     &
     &        'harvest_calib.out', 'unknown')

          ! calibration harvest output file for GUI
          call fopenk (luoharvest_calib_parm,rootp(1:len_trim(rootp)) //&
     &        'harvest_calib_parm.out', 'unknown')
      endif

!     open erosion output files

      if (am0efl.gt.0) then
          call fopenk (20, rootp(1:len_trim(rootp)) // 'erosion.out',   &
     &    'unknown')
          call fopenk (21, rootp(1:len_trim(rootp)) // 'eegt.out',      &
     &    'unknown')
!         open temporary file to hold accounting region erosion values
          call fopenk (40, rootp(1:len_trim(rootp)) // 'eros.tmp',      &
     &    'unknown')
      endif
!      open (unit = 22, file = outp(1:len_trim(outp)) // 'eegtss.out')
!      open (unit = 23, file = outp(1:len_trim(outp)) // 'eegt10.out')
!      call fopenk (7,rootp(1:len_trim(rootp)) // 'grid.out','unknown')

      if (btest(am0efl,0)) then
       call fopenk (luo_erod, rootp(1:len_trim(rootp)) //               &
     &              'daily_erod.out', 'unknown')
      endif
      if (btest(am0efl,1)) then
       call fopenk (luo_egrd, rootp(1:len_trim(rootp)) //               &
     &              'daily_egrd.out', 'unknown')
      endif
      if (btest(am0efl,2)) then
       call fopenk (luo_emit, rootp(1:len_trim(rootp)) //               &
     &              'subdaily_emit.out', 'unknown')
      endif
      if (btest(am0efl,3)) then
       call fopenk (luo_sgrd, rootp(1:len_trim(rootp)) //               &
     &              'subdaily_sgrd.out', 'unknown')
      endif
!     open plot data file

      if((am0hfl.gt.0).or.(am0sfl.gt.0).or.(am0tfl.gt.0)                &
     &    .or.(am0cfl.gt.0).or.(am0dfl.gt.0).or.(am0efl.gt.0)) then
          call fopenk (luoplt, rootp(1:len_trim(rootp)) // 'plot.out',  &
     &    'unknown')
      endif

!     open output file for soil conditioning index
      if( soil_cond .gt. 0 ) then
          call fopenk (luosci, rootp(1:len_trim(rootp)) //              &
     &                 'sci_energy.out', 'unknown')
          call fopenk (luostir, rootp(1:len_trim(rootp)) //             &
     &                 'stir_energy.out', 'unknown')
      end if

!     open detailed output files for hydro

      if ((am0hfl .eq. 1).or.(am0hfl .eq. 3)                            &
     &   .or. (am0hfl .eq. 5) .or. (am0hfl .eq. 7)) then
         call fopenk (luohydro, rootp(1:len_trim(rootp)) // 'hydro.out',&
     &        'unknown')
         call fopenk(luohlayers,rootp(1:len_trim(rootp))//'hlayers.out',&
     &        'unknown')
      endif

      if ((am0hfl .eq. 2).or.(am0hfl .eq. 6)                            &
     &   .or. (am0hfl .eq. 3) .or. (am0hfl .eq. 7)) then
         call fopenk (luowater, rootp(1:len_trim(rootp)) // 'water.out',&
     &        'unknown')
      end if

      if ((am0hfl .eq. 4).or.(am0hfl .eq. 5)                            &
     &   .or.(am0hfl .eq. 6).or.(am0hfl .eq. 7)) then
         call fopenk (13, rootp(1:len_trim(rootp)) // 'temp.out',       &
     &        'unknown')
      end if

! open files for outputing the crop and decomp biomass variables - LEW

      if ((am0dfl .eq. 1).or.(am0dfl.eq.3)) then
         call fopenk (luocrp1, rootp(1:len_trim(rootp)) // 'decomp.out',&
     &    'unknown')
          call fopenk (luobio1, rootp(1:len_trim(rootp)) // 'bio1.btmp',&
     &    'unknown')
          call fopenk (luod_above,                                      &
     &         rootp(1:len_trim(rootp)) // 'dabove.out', 'unknown')
      endif
      if ((am0dfl .eq. 2).or.(am0dfl.eq.3)) then
          call fopenk (luodec1, rootp(1:len_trim(rootp)) // 'dec1.btmp',&
     &    'unknown')
          call fopenk (luodec2, rootp(1:len_trim(rootp)) // 'dec2.btmp',&
     &    'unknown')
          call fopenk (luodec3, rootp(1:len_trim(rootp)) // 'dec3.btmp',&
     &    'unknown')
          call fopenk (luod_below,                                      &
     &         rootp(1:len_trim(rootp)) // 'dbelow.out', 'unknown')
      endif

      if (am0cfl .gt. 0) then
!         daily crop output of most state variables 
          call fopenk (luocrop, rootp(1:len_trim(rootp)) // 'crop.out', &
     &         'unknown')
          call fopenk (luoshoot,                                        &
     &         rootp(1:len_trim(rootp)) // 'shoot.out', 'unknown')

!         echo crop input data - AR
          call fopenk (luoinpt, rootp(1:len_trim(rootp)) // 'inpt.out', &
     &         'unknown')
      endif

!     print headings for crop output files
!     season.out, crop.out, shoot.out, inpt.out
      call cpout

      if (am0cfl .gt. 1) then
!        main crop debug file
         call fopenk (luoallcrop,                                       &
     &        rootp(1:len_trim(rootp)) // 'allcrop.prn', 'unknown')
      endif

      if ((am0sfl .eq. 1)) then
         ! soil detail output files
         ! soil surface
         call fopenk(luosoilsurf, rootp(1:len_trim(rootp)) //           &
     &   'soilsurf.out', 'unknown')
         ! soil layers
         call fopenk(luosoillay, rootp(1:len_trim(rootp)) //            &
     &   'soillay.out', 'unknown')
      endif

      if ((calc_confidence .gt. 0)) then
         ! Confidence Interval output file
         call fopenk(luoci, rootp(1:len_trim(rootp)) //                 &
     &   'ci.out', 'unknown')
      endif

      end
