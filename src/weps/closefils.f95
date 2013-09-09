!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine closefils(residue)

      use file_io_mod
      use biomaterial, only: biomatter
      use erosion_data_struct_defs, only: am0efl
      use hydro_data_struct_defs, only: am0hfl, am0hdb
      use soil_data_struct_defs, only: am0sfl, am0sdb
      use manage_data_struct_defs, only: am0tfl, am0tdb
      use crop_data_struct_defs, only: am0cfl, am0cdb
      use decomp_data_struct_defs, only: am0dfl, am0ddb

      include 'p1werm.inc'
      include 'wpath.inc'
      include 'command.inc'

!     + + + ARGUMENT DECLARATIONS + + +
      type(biomatter), dimension(:,:), intent(in) :: residue

      ! local variables
      integer idx, jdx
      integer ::  nsubr  ! number of subregion (found from size of array always allocated, not with zero element)

      nsubr = size(luoseason)

      ! files opened in inprun.for
      close(luicli)
      close(luiwin)
      close(luiwsd)
      do idx = 1, nsubr
         if (am0hdb(idx) .eq. 1) close(luohdb(idx))
         if (am0sdb(idx) .eq. 1) close(luosdb(idx))
         if (am0tdb(idx) .eq. 1) close(luotdb(idx))
         if (am0cdb(idx) .eq. 1) close(luocdb(idx))
         if (am0ddb(idx) .eq. 1) close(luoddb(idx))
         if (am0tfl(idx) .eq. 1) close(luomanage(idx))
      end do

      ! files opened in cmdline.for
      close(luolog)

!     these files are opened at all times

      close(luogui1(0))
      close(luomandate(0))
      do idx = 1, nsubr
         close(luogui1(idx))
         close(luomandate(idx))
         close(luoharvest_si(idx))
         close(luoharvest_en(idx))
         close(luohydrobal(idx))
         close(luoseason(idx))
      end do

      if (calibrate_crops .gt. 0) then
         do idx = 1, nsubr
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
      do idx = 1, nsubr
         if(    (am0hfl(idx).gt.0) .or. (am0sfl(idx).gt.0) .or. (am0tfl(idx).gt.0) &
           .or. (am0cfl(idx).gt.0) .or. (am0dfl(idx).gt.0) .or. (am0efl.gt.0)      ) then
           close(luoplt(idx))
         endif
      end do

      ! output file for soil conditioning index
      if( soil_cond .gt. 0 ) then
         do idx = 1, nsubr
            close(luosci(idx))
            close(luostir(idx))
         end do
      end if

      ! detailed output files for hydro
      do idx = 1, nsubr
         if ((am0hfl(idx) .eq. 1) .or. (am0hfl(idx) .eq. 3) .or. (am0hfl(idx) .eq. 5) .or. (am0hfl(idx) .eq. 7)) then
            close(luohydro(idx))
            close(luohlayers(idx))
         endif
         if ((am0hfl(idx) .eq. 2) .or. (am0hfl(idx) .eq. 6) .or. (am0hfl(idx) .eq. 3) .or. (am0hfl(idx) .eq. 7)) then
            close(luowater(idx))
         end if
         if ((am0hfl(idx) .eq. 4) .or. (am0hfl(idx) .eq. 5) .or. (am0hfl(idx) .eq. 6) .or. (am0hfl(idx) .eq. 7)) then
            close(luotempsoil(idx))
         end if
      end do

      ! files for outputing the crop and decomp biomass variables - LEW
      do idx = 1, nsubr
         if ((am0dfl(idx) .eq. 1).or.(am0dfl(idx).eq.3)) then
            close(luocrp1(idx))
            close(luobio1(idx))
            close(luod_above(idx))
         endif
         if ((am0dfl(idx) .eq. 2).or.(am0dfl(idx).eq.3)) then
           ! files to match number of biomass pools

           do jdx = 1,mnbpls
              close(residue(jdx,idx)%luo%dec)
           end do

           close(luod_below(idx))
         endif

         if (am0cfl(idx) .gt. 0) then
            ! daily crop output of most state variables 
            close(luocrop(idx))
            close(luoshoot(idx))
            ! echo crop input data - AR
            close(luoinpt(idx))
         endif

         if ((am0sfl(idx) .eq. 1)) then
            ! soil detail output files
            ! soil surface
            close(luosoilsurf(idx))
            ! soil layers
            close(luosoillay(idx))
         endif
      end do

      if ((calc_confidence .gt. 0)) then
         ! Confidence Interval output file
         close(luoci)
      endif

      do idx = 1, nsubr
         if (wepp_hydro .gt. 1) then
            close (luowepphdrive(idx))
         endif
  
         if ((run_erosion.eq.2).or.(run_erosion.eq.3)) then
            close (luowepperod(idx))
            close (luoweppplot(idx))
            close (luoweppsum(idx))
         endif
      end do

      end
