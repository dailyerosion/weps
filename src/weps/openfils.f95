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
      use erosion_data_struct_defs, only: am0efl

      include 'p1werm.inc'
      include 'wpath.inc'

      include 'm1flag.inc'
      include 'command.inc'
      include 'm1subr.inc'
      include 'm1dbug.inc'

!     + + + ARGUMENT DECLARATIONS + + +
      type(biomatter), dimension(:,:), intent(out) :: residue

!     + + +   LOCAL VARIABLES + + +
      integer :: nsubr
      integer idx, jdx, alloc_stat, sum_stat
      character*30, dimension(:), allocatable :: subr_text ! subregion subdirectory text string
      character*30 :: dec_text ! decomposition detail age pool output file name text string

      ! use allocation of resiude array for number of subregions
      nsubr = size(residue,2)

      ! allocate the subregion name, number combination text for subregions
      allocate( subr_text(nsubr), stat=alloc_stat)
      if( alloc_stat .gt. 0 ) then
         Write(*,*) 'ERROR: unable to allocate subr_text array'
      end if

      ! create subregion directory names
      do idx = 1, nsubr
          ! create the name
          subr_text(idx) = makenamnum( 'subregion', idx, nsubr, '/' )
          ! create the subdirectory
          call makedir(trim(rootp)//trim(subr_text(idx)) )
      end do

!     these files are opened at all times
      call fopenk (luogui1, rootp(1:len_trim(rootp)) // 'gui1_data.out', 'unknown')

      sum_stat = 0
      allocate( luomandate(0:nsubr), stat=alloc_stat )
      sum_stat = sum_stat + alloc_stat
      allocate( luoharvest_si(nsubr), stat=alloc_stat )
      sum_stat = sum_stat + alloc_stat
      allocate( luoharvest_en(nsubr), stat=alloc_stat )
      sum_stat = sum_stat + alloc_stat
      allocate( luohydrobal(nsubr), stat=alloc_stat )
      sum_stat = sum_stat + alloc_stat
      allocate( luoseason(nsubr), stat=alloc_stat )
      sum_stat = sum_stat + alloc_stat
      if( sum_stat .gt. 0 ) then
         Write(*,*) 'ERROR: unable to allocate luomandate, luoharvest_, luohydrobal, luoseason arrays'
      end if
      call fopenk (luomandate(0), trim(rootp) // 'mandate.out', 'unknown')
      do idx = 1, nsubr
         call fopenk (luomandate(idx), trim(rootp) // trim(subr_text(idx)) // 'mandate.out', 'unknown')
         call fopenk (luoharvest_si(idx), trim(rootp) // trim(subr_text(idx)) // 'harvest_si.out', 'unknown')
         call fopenk (luoharvest_en(idx), trim(rootp) // trim(subr_text(idx)) // 'harvest_en.out', 'unknown')
         call fopenk (luohydrobal(idx), trim(rootp) // trim(subr_text(idx)) // 'hydrobal.out', 'unknown')
         call fopenk (luoseason(idx), trim(rootp) // trim(subr_text(idx)) // 'season.out', 'unknown')
      end do

      if (calibrate_crops .gt. 0) then
         sum_stat = 0
         allocate( luoharvest_calib(nsubr), stat=alloc_stat )
         sum_stat = sum_stat + alloc_stat
         allocate( luoharvest_calib_parm(nsubr), stat=alloc_stat )
         sum_stat = sum_stat + alloc_stat
         if( sum_stat .gt. 0 ) then
            Write(*,*) 'ERROR: unable to allocate luoharvest_calib, luoharvest_calib_parm arrays'
         end if
         do idx = 1, nsubr
            ! calibration harvest output file
            call fopenk (luoharvest_calib(idx), trim(rootp) // trim(subr_text(idx)) // 'luoharvest_calib.out', 'unknown')
            ! calibration harvest output file for GUI
            call fopenk (luoharvest_calib_parm(idx), trim(rootp) // trim(subr_text(idx)) // 'luoharvest_calib_parm.out', 'unknown')
         end do
      endif

!     open erosion output files
      if (am0efl.gt.0) then
          call fopenk (luo_subday, rootp(1:len_trim(rootp)) // 'subday.out', 'unknown')
      endif

      if (btest(am0efl,0)) then
       call fopenk (luo_erod, rootp(1:len_trim(rootp)) // 'daily_out.erod', 'unknown')
      endif

!     open plot data file
      if((am0hfl.gt.0) .or. (am0sfl.gt.0) .or. (am0tfl.gt.0) .or. (am0cfl.gt.0) .or. (am0dfl.gt.0) .or. (am0efl.gt.0)) then
         sum_stat = 0
         allocate( luoplt(nsubr), stat=alloc_stat )
         sum_stat = sum_stat + alloc_stat
         if( sum_stat .gt. 0 ) then
            Write(*,*) 'ERROR: unable to allocate luoplt array'
         end if
         do idx = 1, nsubr
            call fopenk (luoplt(idx), trim(rootp) // trim(subr_text(idx)) // 'plot.out', 'unknown')
         end do
      endif

!     open output file for soil conditioning index
      if( soil_cond .gt. 0 ) then
         sum_stat = 0
         allocate( luosci(0:nsubr), stat=alloc_stat )
         sum_stat = sum_stat + alloc_stat
         allocate( luostir(nsubr), stat=alloc_stat )
         sum_stat = sum_stat + alloc_stat
         if( sum_stat .gt. 0 ) then
            Write(*,*) 'ERROR: unable to allocate luosci, luostir arrays'
         end if
         call fopenk (luosci(0), trim(rootp) // 'sci_energy.out', 'unknown')
         do idx = 1, nsubr
            call fopenk (luosci(idx), trim(rootp) // trim(subr_text(idx)) // 'sci_energy.out', 'unknown')
            call fopenk (luostir(idx), trim(rootp) // trim(subr_text(idx)) // 'stir_energy.out', 'unknown')
         end do
      end if

!     open detailed output files for hydro

      if ((am0hfl .eq. 1) .or. (am0hfl .eq. 3) .or. (am0hfl .eq. 5) .or. (am0hfl .eq. 7)) then
         sum_stat = 0
         allocate( luohydro(nsubr), stat=alloc_stat )
         sum_stat = sum_stat + alloc_stat
         allocate( luohlayers(nsubr), stat=alloc_stat )
         sum_stat = sum_stat + alloc_stat
         if( sum_stat .gt. 0 ) then
            Write(*,*) 'ERROR: unable to allocate luohydro, luohlayers arrays'
         end if
         do idx = 1, nsubr
            call fopenk (luohydro(idx), trim(rootp) // trim(subr_text(idx)) // 'hydro.out', 'unknown')
            call fopenk (luohlayers(idx), trim(rootp) // trim(subr_text(idx)) // 'hlayers.out', 'unknown')
         end do
      endif

      if ((am0hfl .eq. 2) .or. (am0hfl .eq. 6) .or. (am0hfl .eq. 3) .or. (am0hfl .eq. 7)) then
         sum_stat = 0
         allocate( luowater(nsubr), stat=alloc_stat )
         sum_stat = sum_stat + alloc_stat
         if( sum_stat .gt. 0 ) then
            Write(*,*) 'ERROR: unable to allocate luowater array'
         end if
         do idx = 1, nsubr
            call fopenk (luowater(idx), trim(rootp) // trim(subr_text(idx)) // 'water.out', 'unknown')
         end do
      end if

      if ((am0hfl .eq. 4) .or. (am0hfl .eq. 5) .or. (am0hfl .eq. 6) .or. (am0hfl .eq. 7)) then
         sum_stat = 0
         allocate( luotempsoil(nsubr), stat=alloc_stat )
         sum_stat = sum_stat + alloc_stat
         if( sum_stat .gt. 0 ) then
            Write(*,*) 'ERROR: unable to allocate luotempsoil array'
         end if
         do idx = 1, nsubr
            call fopenk (luotempsoil(idx), trim(rootp) // trim(subr_text(idx)) // 'temp.out', 'unknown')
         end do
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

        ! create dbelow.out unit number array for subregions
        allocate( luod_below(nsubr), stat=alloc_stat )
        if( alloc_stat .gt. 0 ) then
           write(*,*) 'ERROR: unable to allocate luod_below array'
        end if
         
        do idx = 1, nsubr
           ! open dbelow.out in each subregion
           call fopenk (luod_below(idx), trim(rootp) // trim(subr_text(idx)) // 'dbelow.out', 'unknown')

           ! open files to match number of biomass pools
           ! create age pool output file names, set unit numbers and open files
           do jdx = 1,mnbpls
             ! create the name
             dec_text = makenamnum( 'dec', jdx, mnbpls, '.btmp' )
             ! display created file
             !write(*,*) 'File name created: ', dec_text
             ! assign logical unit number of opening file to array
             call fopenk (residue(jdx,idx)%luo%dec, trim(rootp) // trim(subr_text(idx)) // trim(dec_text), 'unknown')
           end do
         end do
      endif

      if (am0cfl .gt. 0) then
         sum_stat = 0
         allocate( luocrop(nsubr), stat=alloc_stat )
         sum_stat = sum_stat + alloc_stat
         allocate( luoshoot(nsubr), stat=alloc_stat )
         sum_stat = sum_stat + alloc_stat
         allocate( luoinpt(nsubr), stat=alloc_stat )
         sum_stat = sum_stat + alloc_stat
         if( sum_stat .gt. 0 ) then
            Write(*,*) 'ERROR: unable to allocate luocrop, luoshoot, luoinpt arrays'
         end if

         do idx = 1, nsubr
            ! daily crop output of most state variables 
            call fopenk (luocrop(idx), trim(rootp) // trim(subr_text(idx)) // 'crop.out', 'unknown')
            call fopenk (luoshoot(idx), trim(rootp) // trim(subr_text(idx)) // 'shoot.out', 'unknown')
            ! echo crop input data - AR
            call fopenk (luoinpt(idx), trim(rootp) // trim(subr_text(idx)) // 'inpt.out', 'unknown')
         end do
      endif

        ! print headings for crop output files
        ! season.out, crop.out, shoot.out, inpt.out
      do idx = 1, nsubr
        call cpout(idx)
      end do

      if ((am0sfl .eq. 1)) then
         ! soil detail output files
         sum_stat = 0
         allocate( luosoilsurf(nsubr), stat=alloc_stat )
         sum_stat = sum_stat + alloc_stat
         allocate( luosoillay(nsubr), stat=alloc_stat )
         sum_stat = sum_stat + alloc_stat
         if( sum_stat .gt. 0 ) then
            Write(*,*) 'ERROR: unable to allocate luosoilsurf, luosoillay arrays'
         end if
         do idx = 1, nsubr
            ! soil surface
            call fopenk(luosoilsurf(idx), trim(rootp) // trim(subr_text(idx)) // 'soilsurf.out', 'unknown')
            ! soil layers
            call fopenk(luosoillay(idx), trim(rootp) // trim(subr_text(idx)) // 'soillay.out', 'unknown')
         end do
      endif

      if (am0tfl .eq. 1) then
         sum_stat = 0
         allocate( luomanage(nsubr), stat=alloc_stat )
         sum_stat = sum_stat + alloc_stat
         if( sum_stat .gt. 0 ) then
            Write(*,*) 'ERROR: unable to allocate luomanage array'
         end if
         do idx = 1, nsubr
            call fopenk (luomanage(idx), trim(rootp) // trim(subr_text(idx)) // 'manage.out', 'unknown')
         end do
      end if

      if ((calc_confidence .gt. 0)) then
         ! Confidence Interval output file
         call fopenk(luoci, rootp(1:len_trim(rootp)) // 'ci.out', 'unknown')
      endif

         ! create arrays for subregion debug output files
      if (am0hdb .eq. 1) then
         allocate( luohdb(nsubr), stat=alloc_stat )
         if( alloc_stat .gt. 0 ) then
            Write(*,*) 'ERROR: unable to allocate luohdb array'
         end if
         do idx = 1, nsubr
            call fopenk (luohdb(idx), trim(rootp) // trim(subr_text(idx)) // 'hdbug.out', 'unknown')
         end do
      end if
      if (am0sdb .eq. 1) then
         allocate( luosdb(nsubr), stat=alloc_stat )
         if( alloc_stat .gt. 0 ) then
            Write(*,*) 'ERROR: unable to allocate luosdb array'
         end if
         do idx = 1, nsubr
            call fopenk (luosdb(idx), trim(rootp) // trim(subr_text(idx)) // 'sdbug.out', 'unknown')
         end do
      end if
      if (am0tdb .eq. 1) then
         allocate( luotdb(nsubr), stat=alloc_stat )
         if( alloc_stat .gt. 0 ) then
            Write(*,*) 'ERROR: unable to allocate luotdb array'
         end if
         do idx = 1, nsubr
            call fopenk (luotdb(idx), trim(rootp) // trim(subr_text(idx)) // 'tdbug.out', 'unknown')
         end do
      end if
      if (am0cdb .eq. 1) then
         allocate( luocdb(nsubr), stat=alloc_stat )
         if( alloc_stat .gt. 0 ) then
            Write(*,*) 'ERROR: unable to allocate luocdb array'
         end if
         do idx = 1, nsubr
            call fopenk (luocdb(idx), trim(rootp) // trim(subr_text(idx)) // 'cdbug.out', 'unknown')
         end do
      end if
      if (am0ddb .eq. 1) then
         allocate( luoddb(nsubr), stat=alloc_stat )
         if( alloc_stat .gt. 0 ) then
            Write(*,*) 'ERROR: unable to allocate luoddb array'
         end if
         do idx = 1, nsubr
            call fopenk (luoddb(idx), trim(rootp) // trim(subr_text(idx)) // 'ddbug.out', 'unknown')
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

      ! free memory from local subregion text strings
      deallocate( subr_text, stat=alloc_stat)
      if( alloc_stat .gt. 0 ) then
         Write(*,*) 'ERROR: unable to deallocate subr_text array'
      end if


      end
