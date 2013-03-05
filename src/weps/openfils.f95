!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!
      subroutine openfils(residue)
! ***************************************************************** wjr
! Contains init code from main
!
!       Edit History
!       10-Mar-99       wjr     created

      use weps_interface_defs
      use file_io_mod
      use biomaterial, only: biomatter

      include 'p1werm.inc'
      include 'wpath.inc'

      include 'm1flag.inc'
      include 'command.inc'
      include 'm1subr.inc'
      include 'm1dbug.inc'

!     + + + ARGUMENT DECLARATIONS + + +
      type(biomatter), dimension(:,:), intent(out) :: residue

!     + + +   LOCAL VARIABLES + + +
      integer idx, jdx, alloc_stat, sum_stat
      character*10 :: decfile ! decomposition detail age pool output file name
      character*20 :: subr_text(nsubr) ! subregion subdirectory text string
      character*20 :: subr_format      ! format string for creating subr_text

      ! create subregion directory names
      write( subr_format, '(i0)' ) nsubr
      idx = len_trim(subr_format)
      write( subr_format, '(a8, i0, a5)' ) '(a9, i0.', idx, ', a1)'
      do idx = 1, nsubr
          ! create the name
          write( subr_text(idx), subr_format) 'subregion', idx, '/'
          ! create the subdirectory
          call makedir(trim(rootp)//trim(subr_text(idx)) )
      end do

!     the main output file is opened at all times

      call fopenk (luogui1, rootp(1:len_trim(rootp)) // 'gui1_data.out', 'unknown')
      call fopenk (luomandate,rootp(1:len_trim(rootp)) // 'mandate.out', 'unknown')


!     the harvest report ouput files are opened at all times

      call fopenk (luoharvest_si,rootp(1:len_trim(rootp)) // 'harvest_si.out', 'unknown')
      call fopenk (luoharvest_en,rootp(1:len_trim(rootp)) // 'harvest_en.out', 'unknown')

!     the hydrobal report ouput file is opened at all times

      call fopenk(luohydrobal,rootp(1:len_trim(rootp)) // 'hydrobal.out', 'unknown')

!     seasonal summaries of yield and biomass
      call fopenk (luoseason, rootp(1:len_trim(rootp)) // 'season.out', 'unknown')

      if (calibrate_crops .gt. 0) then
          ! calibration harvest output file
          call fopenk (luoharvest_calib,rootp(1:len_trim(rootp)) // 'harvest_calib.out', 'unknown')

          ! calibration harvest output file for GUI
          call fopenk (luoharvest_calib_parm,rootp(1:len_trim(rootp)) // 'harvest_calib_parm.out', 'unknown')
      endif

!     open erosion output files

      if (am0efl.gt.0) then
          call fopenk (luo_subday, rootp(1:len_trim(rootp)) // 'subday.out', 'unknown')
      endif

      if (btest(am0efl,0)) then
       call fopenk (luo_erod, rootp(1:len_trim(rootp)) // 'daily_erod.out', 'unknown')
      endif
      if (btest(am0efl,1)) then
       call fopenk (luo_egrd, rootp(1:len_trim(rootp)) // 'daily_egrd.out', 'unknown')
      endif
      if (btest(am0efl,2)) then
       call fopenk (luo_emit, rootp(1:len_trim(rootp)) // 'subdaily_emit.out', 'unknown')
      endif
      if (btest(am0efl,3)) then
       call fopenk (luo_sgrd, rootp(1:len_trim(rootp)) // 'subdaily_sgrd.out', 'unknown')
      endif
!     open plot data file

      if((am0hfl.gt.0) .or. (am0sfl.gt.0) .or. (am0tfl.gt.0) .or. (am0cfl.gt.0) .or. (am0dfl.gt.0) .or. (am0efl.gt.0)) then
          call fopenk (luoplt, rootp(1:len_trim(rootp)) // 'plot.out', 'unknown')
      endif

!     open output file for soil conditioning index
      if( soil_cond .gt. 0 ) then
          call fopenk (luosci, rootp(1:len_trim(rootp)) // 'sci_energy.out', 'unknown')
          call fopenk (luostir, rootp(1:len_trim(rootp)) // 'stir_energy.out', 'unknown')
      end if

!     open detailed output files for hydro

      if ((am0hfl .eq. 1) .or. (am0hfl .eq. 3) .or. (am0hfl .eq. 5) .or. (am0hfl .eq. 7)) then
         call fopenk (luohydro, rootp(1:len_trim(rootp)) // 'hydro.out', 'unknown')
         call fopenk(luohlayers,rootp(1:len_trim(rootp))//'hlayers.out', 'unknown')
      endif

      if ((am0hfl .eq. 2) .or. (am0hfl .eq. 6) .or. (am0hfl .eq. 3) .or. (am0hfl .eq. 7)) then
         call fopenk (luowater, rootp(1:len_trim(rootp)) // 'water.out', 'unknown')
      end if

      if ((am0hfl .eq. 4) .or. (am0hfl .eq. 5) .or. (am0hfl .eq. 6) .or. (am0hfl .eq. 7)) then
         call fopenk(luotempsoil, rootp(1:len_trim(rootp)) // 'temp.out', 'unknown')
      end if

! open files for outputing the crop and decomp biomass variables - LEW

      if ((am0dfl .eq. 1).or.(am0dfl.eq.3)) then
         sum_stat = 0
         allocate( luocrp1(nsubr), stat=alloc_stat )
         sum_stat = sum_stat + alloc_stat
         allocate( luobio1(nsubr), stat=alloc_stat )
         sum_stat = sum_stat + alloc_stat
         allocate( luod_above(nsubr), stat=alloc_stat )
         sum_stat = sum_stat + alloc_stat
         if( sum_stat .gt. 0 ) then
            Write(*,*) 'ERROR: unable to allocate luod_above, luocrp1 or luobio1 array'
         end if

         do idx = 1, nsubr
            call fopenk (luocrp1(idx), trim(rootp) // trim(subr_text(idx)) // 'decomp.out', 'unknown')
            call fopenk (luobio1(idx), trim(rootp) // trim(subr_text(idx)) // 'bio1.btmp', 'unknown')
            call fopenk (luod_above(idx), trim(rootp) // trim(subr_text(idx)) // 'dabove.out', 'unknown')
         end do
      endif
      if ((am0dfl .eq. 2).or.(am0dfl.eq.3)) then
        ! open files to match number of biomass pools

        ! create age pool output file names, set unit numbers and open files
        ! assign first three characters to file name
        decfile(1:3) = 'dec'
        ! assign last four characters to file name
        decfile(6:10) = '.btmp'
        do idx = 1, nsubr

           do jdx = 1,mnbpls
             ! assign 4 character of file name to be the number character corresponsing to tens place of idx
             decfile(4:4) = char(48+(jdx/10))
             ! assign 5 character of file name to be the number character corresponsing to ones place of idx
             decfile(5:5) = char(48+(jdx-(jdx/10)*10))
             ! display created file
             !write(*,*) 'File name created: ', decfile(jdx)
             ! assign logical unit number of opening file to array
             call fopenk (residue(jdx,idx)%luo%dec, rootp(1:len_trim(rootp)) // subr_text(idx) // decfile, 'unknown')
           end do

            allocate( luod_below(nsubr), stat=alloc_stat )
            if( alloc_stat .gt. 0 ) then
               Write(*,*) 'ERROR: unable to allocate luod_below array'
            end if

         
            call fopenk (luod_below(idx), rootp(1:len_trim(rootp)) // subr_text(idx) // 'dbelow.out', 'unknown')
         end do
      endif

      if (am0cfl .gt. 0) then
!         daily crop output of most state variables 
          call fopenk (luocrop, rootp(1:len_trim(rootp)) // 'crop.out', 'unknown')
          call fopenk (luoshoot, rootp(1:len_trim(rootp)) // 'shoot.out', 'unknown')

!         echo crop input data - AR
          call fopenk (luoinpt, rootp(1:len_trim(rootp)) // 'inpt.out', 'unknown')
      endif

!     print headings for crop output files
!     season.out, crop.out, shoot.out, inpt.out
      call cpout

      if ((am0sfl .eq. 1)) then
         ! soil detail output files
         ! soil surface
         call fopenk(luosoilsurf, rootp(1:len_trim(rootp)) // 'soilsurf.out', 'unknown')
         ! soil layers
         call fopenk(luosoillay, rootp(1:len_trim(rootp)) // 'soillay.out', 'unknown')
      endif

      if ((calc_confidence .gt. 0)) then
         ! Confidence Interval output file
         call fopenk(luoci, rootp(1:len_trim(rootp)) // 'ci.out', 'unknown')
      endif

        if (am0hdb .eq. 1) call fopenk (luohdb,                         &
     &     rootp(1:len_trim(rootp)) // 'hdbug.out', 'unknown')
        if (am0sdb .eq. 1) call fopenk (luosdb,                         &
     &     rootp(1:len_trim(rootp)) // 'sdbug.out', 'unknown')
        if (am0tdb .eq. 1) call fopenk (luotdb,                         &
     &     rootp(1:len_trim(rootp)) // 'tdbug.out', 'unknown')
        if (am0cdb .eq. 1) call fopenk (luocdb,                         &
     &     rootp(1:len_trim(rootp)) // 'cdbug.out', 'unknown')
      if (am0ddb .eq. 1) then
         ! create arrays for subregion debug output files
         allocate( luoddb(nsubr), stat=alloc_stat )
         if( alloc_stat .gt. 0 ) then
            Write(*,*) 'ERROR: unable to allocate luoddb array'
         end if
         do idx = 1, nsubr
            call fopenk (luoddb(idx), rootp(1:len_trim(rootp)) // subr_text(idx) // 'ddbug.out', 'unknown')
         end do
      end if

!   WEPP Related files
!
       if (wepp_hydro .gt. 1) then
         call fopenk(luowepphdrive, rootp(1:len_trim(rootp)) // 'wepp_runoff.out','unknown')
         write(luowepphdrive,*) ' WEPP Flow Routing Output'
         write(luowepphdrive,*) ' # day   mon  yr     precip  runoff    peakro  effdrn    effint   effdrr/rainfall excess'
         write(luowepphdrive,*) '                      (mm)   (mm)     (mm/hr)  (min)    (mm/hr)     (min)'
       endif

       if ((run_erosion.eq.2).or.(run_erosion.eq.3)) then
         call fopenk(luowepperod,rootp(1:len_trim(rootp)) // 'wepp_eroevents.out','unknown')
         write(luowepperod,*) 'WEPP Erosion Events Output'
         write(luowepperod,*) 'day mo  year    Precp  Runoff  IR-det Av-det Mx-det  Point  Av-dep Max-dep  Point Sed.Del    ER'
         write(luowepperod,*) '--- --  ----     (mm)    (mm)  kg/m^2 kg/m^2 kg/m^2    (m)  kg/m^2  kg/m^2    (m)  (kg/m)  ----'

         call fopenk(luoweppplot,rootp(1:len_trim(rootp)) // 'wepp_eroplot.out','unknown')
     
         call fopenk(luoweppsum,rootp(1:len_trim(rootp)) // 'wepp_summary.out','unknown')
         write(luoweppsum,*) 'WEPS/WEPP Common Model'
         write(luoweppsum,*) 'March 3, 2009  (2009.3)'
         write(luoweppsum,*) '---------------------------------------'
       endif

      end
