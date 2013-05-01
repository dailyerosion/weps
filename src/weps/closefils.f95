!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine closefils(residue)

      use file_io_mod
      use biomaterial, only: biomatter
      use erosion_data_struct_defs, only: am0efl

      include 'p1werm.inc'
      include 'wpath.inc'

      include 'm1flag.inc'
      include 'm1dbug.inc'
      include 'command.inc'

!     + + + ARGUMENT DECLARATIONS + + +
      type(biomatter), dimension(:,:), intent(in) :: residue

      ! local variables
      integer idx, jdx

      ! files opened in inprun.for
      close(luicli)
      close(luiwin)
      close(luiwsd)
      do idx = 1, size(luoddb)
         if (am0hdb .eq. 1) close(luohdb(idx))
         if (am0sdb .eq. 1) close(luosdb(idx))
         if (am0tdb .eq. 1) close(luotdb(idx))
         if (am0cdb .eq. 1) close(luocdb(idx))
         if (am0ddb .eq. 1) close(luoddb(idx))
         if (am0tfl .eq. 1) close(luomanage(idx))
      end do

      ! files opened in cmdline.for
      close(luolog)

!     these files are opened at all times

      close(luogui1)

      close(luomandate(0))
      do idx = 1, size(luoseason)
         close(luomandate(idx))
         close(luoharvest_si(idx))
         close(luoharvest_en(idx))
         close(luohydrobal(idx))
         close(luoseason(idx))
      end do

      if (calibrate_crops .gt. 0) then
         do idx = 1, size(luoplt)
            ! calibration harvest output file
            close(luoharvest_calib(idx))
            ! calibration harvest output file for GUI
            close(luoharvest_calib_parm(idx))
         end do
      endif

!     erosion output files
      if (am0efl.gt.0) then
          close(luo_subday)
      endif

      if (btest(am0efl,0)) then
       close(luo_erod)
      endif

!     plot data file
      if((am0hfl.gt.0) .or. (am0sfl.gt.0) .or. (am0tfl.gt.0) .or. (am0cfl.gt.0) .or. (am0dfl.gt.0) .or. (am0efl.gt.0)) then
         do idx = 1, size(luoplt)
           close(luoplt(idx))
         end do
      endif

!     output file for soil conditioning index
      if( soil_cond .gt. 0 ) then
         do idx = 1, size(luosci)
            close(luosci(idx))
            close(luostir(idx))
         end do
      end if

!     detailed output files for hydro

      if ((am0hfl .eq. 1) .or. (am0hfl .eq. 3) .or. (am0hfl .eq. 5) .or. (am0hfl .eq. 7)) then
         do idx = 1, size(luohydro)
            close(luohydro(idx))
            close(luohlayers(idx))
         end do
      endif

      if ((am0hfl .eq. 2) .or. (am0hfl .eq. 6) .or. (am0hfl .eq. 3) .or. (am0hfl .eq. 7)) then
         do idx = 1, size(luowater)
            close(luowater(idx))
         end do
      end if

      if ((am0hfl .eq. 4) .or. (am0hfl .eq. 5) .or. (am0hfl .eq. 6) .or. (am0hfl .eq. 7)) then
         do idx = 1, size(luotempsoil)
            close(luotempsoil(idx))
         end do
      end if

! files for outputing the crop and decomp biomass variables - LEW

      do idx = 1, size(luocrp1)
         if ((am0dfl .eq. 1).or.(am0dfl.eq.3)) then
            close(luocrp1(idx))
            close(luobio1(idx))
            close(luod_above(idx))
         endif
         if ((am0dfl .eq. 2).or.(am0dfl.eq.3)) then
           ! files to match number of biomass pools

           do jdx = 1,mnbpls
              close(residue(jdx,idx)%luo%dec)
           end do

           close(luod_below(idx))
         endif
      end do

      if (am0cfl .gt. 0) then
        do idx = 1, size(luocrop)
!         daily crop output of most state variables 
          close(luocrop(idx))
          close(luoshoot(idx))
!         echo crop input data - AR
          close(luoinpt(idx))
        end do
      endif

      if ((am0sfl .eq. 1)) then
         ! soil detail output files
         do idx = 1, size(luocrop)
            ! soil surface
            close(luosoilsurf(idx))
            ! soil layers
            close(luosoillay(idx))
         end do
      endif

      if ((calc_confidence .gt. 0)) then
         ! Confidence Interval output file
         close(luoci)
      endif

      if (wepp_hydro .gt. 1) then
         close (luowepphdrive)
      endif
	  
      if ((run_erosion.eq.2).or.(run_erosion.eq.3)) then
         close (luowepperod)
         close (luoweppplot)
         close (luoweppsum)
      endif

      end
