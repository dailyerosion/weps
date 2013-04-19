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
      if (am0tfl .eq. 1) close(luomanage)
      if (am0hdb .eq. 1) close(luohdb)
      if (am0sdb .eq. 1) close(luosdb)
      if (am0tdb .eq. 1) close(luotdb)
      if (am0cdb .eq. 1) close(luocdb)
      do idx = 1, size(luoddb)
         if (am0ddb .eq. 1) close(luoddb(idx))
      end do

      ! files opened in cmdline.for
      close(luolog)

!     the main output file is opened at all times

      close(luogui1)
      close(luomandate)


!     the harvest report ouput files are opened at all times

      close(luoharvest_si)
      close(luoharvest_en)

!     the hydrobal report ouput file is opened at all times

      close(luohydrobal)

!     seasonal summaries of yield and biomass
      close(luoseason)

      if (calibrate_crops .gt. 0) then
          ! calibration harvest output file
          close(luoharvest_calib)

          ! calibration harvest output file for GUI
          close(luoharvest_calib_parm)
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
          close(luoplt)
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
         close(luohydro)
         close(luohlayers)
      endif

      if ((am0hfl .eq. 2) .or. (am0hfl .eq. 6) .or. (am0hfl .eq. 3) .or. (am0hfl .eq. 7)) then
         close(luowater)
      end if

      if ((am0hfl .eq. 4) .or. (am0hfl .eq. 5) .or. (am0hfl .eq. 6) .or. (am0hfl .eq. 7)) then
         close(luotempsoil)
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
!         daily crop output of most state variables 
          close(luocrop)
          close(luoshoot)

!         echo crop input data - AR
          close(luoinpt)
      endif

      if ((am0sfl .eq. 1)) then
         ! soil detail output files
         ! soil surface
         close(luosoilsurf)
         ! soil layers
         close(luosoillay)
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
