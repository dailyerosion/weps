!$Author$
!$Date$
!$Revision$
!$HeadURL$

module split_layers_mod

  contains

    subroutine soil_layer_count( layer_infla, layer_scale, nnlay, fix_thick )
      integer, intent(in) :: layer_infla
      integer, intent(in) :: layer_scale
      integer, intent(out) :: nnlay    ! number of soil layers for new layering
      real, intent(in), dimension(:) :: fix_thick    ! thickness of fixed soil layers from database

      ! local variables
      integer :: nflay    ! number of layers in fix_thick array
      integer :: fdx      ! fixed layer index
      real :: mfac        ! multiplier factor
      real :: fix_depth
      real, dimension(2) :: targetthk
      real, dimension(2) :: targetdep
      real, parameter :: max_depth = 3000.0

      ! set multiplier factor
      mfac = 1.0 + layer_infla/100.0

      ! get number of fixed soil layers
      nflay = size(fix_thick)

      ! set initial layer thicknesses
      fdx = 1
      fix_depth = fix_thick(fdx)
      nnlay = 1
      targetthk(1) = layer_scale
      targetdep(1) = targetthk(1)

      ! check layers for maximum depth and number of fixed layers
      do while( targetdep(1) .lt. max_depth )
        ! estimate new target layer thickness and depth
        targetthk(2) = targetthk(1) * mfac
        targetdep(2) = targetdep(1) + targetthk(2)

        ! check for crossing layer boundaries
        if( targetdep(2) .gt. fix_depth ) then
          ! update set layer used for checking
          fdx = fdx + 1
          if( fdx .le. nflay) then
            ! add extra layer to match layer boundary
            nnlay = nnlay + 1
            ! update new set layer depth
            fix_depth = fix_depth + fix_thick(fdx)
          else
            fix_depth = max( fix_depth, max_depth )
          end if
        end if

        ! update for next step
        nnlay = nnlay + 1
        targetthk(1) = targetthk(2)
        targetdep(1) = targetdep(2)

      end do

    end subroutine soil_layer_count

    subroutine soil_layer_split( layer_infla, layer_scale, fix_thick, split_thick )
      integer, intent(in) :: layer_infla
      integer, intent(in) :: layer_scale
      real, intent(in), dimension(:) :: fix_thick    ! thickness of fixed soil layers from database
      real, intent(out), dimension(:) :: split_thick    ! thickness of new soil layers

      ! local variables
      integer :: nflay    ! number of layers in fix_thick array
      integer :: fdx      ! fixed layer index
      integer :: nslay    ! number of layers in split_thick array
      integer :: sdx      ! split layer index
      integer :: ntlay    ! number of layers in temporary split_thick array
      integer :: tdx      ! temporary split layer index
      integer :: ldx      ! local layer index
      integer :: alloc_stat, sum_stat
      real :: mfac        ! multiplier factor
      real :: add_depth   ! depth increment to be added
      integer :: dodx     ! do layers for adjustment
      real :: tgtthk      ! used in layer adjustment
      real :: totthk      ! used in layer adjustment
      real :: tinfser     ! used in layer adjustment
      real, dimension(:), allocatable :: fix_depth      
      real, dimension(:), allocatable :: targetthk
      real, dimension(:), allocatable :: targetdep
      real, dimension(:), allocatable :: tempthk
      real, dimension(:), allocatable :: tempdep
      integer, dimension(:), allocatable :: tempstat
      real, parameter :: max_depth = 3000.0

      ! set multiplier factor
      mfac = 1.0 + layer_infla/100.0

      ! get number of soil layers
      nflay = size(fix_thick)
      nslay = size(split_thick)
      ntlay = nslay

      ! allocate temporary arrays
      sum_stat = 0
      allocate(fix_depth(nflay), stat=alloc_stat)
      sum_stat = sum_stat + alloc_stat
      allocate(targetthk(nslay), stat=alloc_stat)
      sum_stat = sum_stat + alloc_stat
      allocate(targetdep(nslay), stat=alloc_stat)
      sum_stat = sum_stat + alloc_stat
      allocate(tempthk(nslay), stat=alloc_stat)
      sum_stat = sum_stat + alloc_stat
      allocate(tempdep(nslay), stat=alloc_stat)
      sum_stat = sum_stat + alloc_stat
      allocate(tempstat(nslay), stat=alloc_stat)
      sum_stat = sum_stat + alloc_stat
      if( sum_stat .gt. 0 ) then
         Write(*,*) 'ERROR: unable to allocate enough memory for soil_layer_split data arrays'
      end if

      ! set multiplier factor
      mfac = 1.0 + layer_infla/100.0

      ! alternative layering
      targetthk(1) = layer_scale
      targetdep(1) = targetthk(1)
      do sdx=2,nslay
        targetthk(sdx) = targetthk(sdx-1) * mfac
        targetdep(sdx) = targetdep(sdx-1) + targetthk(sdx)
      end do

      ! compute out depth to bottom of soil layer
      fix_depth(1) = fix_thick(1)
      do fdx = 2, nflay
        fix_depth(fdx) = fix_depth(fdx-1) + fix_thick(fdx)
      end do

      ! based on depth to impermeable, bedrock layer, increase depth 
      ! of soil. With a unit gradient at the bottom boundary, no water
      ! will move up from the lower boundary.
      if( fix_depth(nflay) .lt. max_depth ) then
          add_depth = max_depth - fix_depth(nflay)
          fix_depth(nflay) = fix_depth(nflay) + add_depth 
      end if

        ! set temporary layer thicknesses, matching input layer boundaries
        ! checking termination layer to get same total soil thickness
        ! set number of layers
        fdx = 1
        sdx = 1
        tdx = 1
        do while ( (sdx .le. nslay) .and. (fdx .le. nflay) )
          if( targetdep(sdx) .le. fix_depth(fdx) ) then
            ! totally within layer
            tempthk(tdx) = targetthk(sdx)
            tempdep(tdx) = targetdep(sdx)
            tempstat(tdx) = 0   ! target layer boundary
            tdx = tdx + 1
            sdx = sdx + 1
          else if( tempdep(tdx-1) .le. fix_depth(fdx) ) then
            ! crossed layer boundary set at layer boundary
            tempthk(tdx) = fix_depth(fdx) - tempdep(tdx-1) 
            tempdep(tdx) = fix_depth(fdx)
            ! adjust target thickness to match new layer division
            targetthk(sdx) = targetdep(sdx) - fix_depth(fdx)
            tempstat(tdx) = 1   ! input layer boundary
            ! increment counters
            tdx = tdx + 1
            fdx = fdx + 1
          end if 
        end do
        ntlay = tdx-1

        ! even out layer spacing of last surface layers
        ! search for first original layer boundary
        sdx = 1
        do while( (sdx.lt.ntlay) .and. (tempstat(sdx).eq.0) )
          sdx = sdx + 1
        end do
        ! surface layers only, average last two layers
        totthk = tempthk(sdx-1) + tempthk(sdx)
        tgtthk = totthk / 2.0
        ! redo layers
        tempthk(sdx-1) = tgtthk
        if( sdx .eq. 2 ) then
          ! only two surface layers, keep indexes in bounds
          tempdep(sdx-1) = tempthk(sdx-1)
        else
          tempdep(sdx-1) = tempdep(sdx-2) + tempthk(sdx-1)
        end if
        ! get the last layer of the interval exact
        tempthk(sdx) = tempdep(sdx) - tempdep(sdx-1)

        ! even out layer spacing between fixed layers
        fdx = 0
        sdx = 0
        do tdx = 1, ntlay
          if( tempstat(tdx) .eq. 1 ) then
            ! fixed layer found
            fdx = sdx
            sdx = tdx
          end if
          if( fdx .gt. 0 ) then
            ! below surface layers
            dodx = (sdx-fdx)
            ! add up series used to set layer adjustment series
            tinfser = 1.0
            do ldx = 1, dodx-1
              tinfser = tinfser + mfac**ldx
            end do
            totthk = tempdep(sdx) - tempdep(fdx)
            tgtthk = totthk / tinfser
            do ldx = fdx+1, sdx-1
              ! redo layers in this interval
              tempthk(ldx) = tgtthk * mfac ** (ldx - fdx - 1)
              tempdep(ldx) = tempdep(ldx-1) + tempthk(ldx)
            end do
            ! get the last layer of the interval exact
            tempthk(ldx) = tempdep(sdx) - tempdep(sdx-1)
            ! set so that adjustment not done until next permanent layer
            fdx = 0
          end if
        end do

      ! copy result into array for return
      do ldx = 1, nslay
        split_thick(ldx) = tempthk(ldx)
      end do

      ! deallocate temporary arrays
      sum_stat = 0
      deallocate(fix_depth, stat=alloc_stat)
      sum_stat = sum_stat + alloc_stat
      deallocate(targetthk, stat=alloc_stat)
      sum_stat = sum_stat + alloc_stat
      deallocate(targetdep, stat=alloc_stat)
      sum_stat = sum_stat + alloc_stat
      deallocate(tempthk, stat=alloc_stat)
      sum_stat = sum_stat + alloc_stat
      deallocate(tempdep, stat=alloc_stat)
      sum_stat = sum_stat + alloc_stat
      deallocate(tempstat, stat=alloc_stat)
      sum_stat = sum_stat + alloc_stat
      if( sum_stat .gt. 0 ) then
         Write(*,*) 'ERROR: unable to allocate enough memory for soil_layer_split data arrays'
      end if

    end subroutine soil_layer_split

    subroutine move_ave_val( nlay_old, laydepth_old, valuearr_old, nlay_new, laydepth_new, valuearr_new )
      !   + + + PURPOSE + + +
      ! averages new layer values across old layers and moves new values into the same array

!     + + + ARGUMENT DECLARATIONS + + +
      integer, intent(in) :: nlay_old    ! number of layers in old layering
      integer, intent(in) :: nlay_new    ! depth to bottom of old soil layers
      real, intent(in), dimension(:) :: laydepth_old  ! soil property variable array in old layering
      real, intent(in), dimension(:) :: valuearr_old  ! number of layers in new layering
      real, intent(in), dimension(:) :: laydepth_new  ! depth to bottom of new soil layers
      real, intent(out), dimension(:) :: valuearr_new ! soil property variable array in new layering

!     + + + LOCAL VARIABLES + + +
      integer lay
      real depth

!     + + + LOCAL DEFINITIONS + + +
!     lay     - layer index
!     depth   - depth in soil of top of layer

!     + + + FUNCTIONS CALLED + + +
      real valbydepth

!     + + + END SPECIFICATIONS + + +

      ! start from soil surface
      depth = 0.0
      do lay = 1, nlay_new
          valuearr_new(lay) = valbydepth(                               &
     &                        nlay_old, laydepth_old, valuearr_old,     &
     &                        0, depth, laydepth_new(lay) )
          depth = laydepth_new(lay)
      end do

      return
    end subroutine move_ave_val

    subroutine spllay_ifc (soil)
      ! Converts NASIS layered IFC data into thin layered data
      ! Edit History
      ! 07-Feb-01   wjr   created
      ! 10-Oct-04   lew   modified to work with "versioned" IFC files only

      use weps_main_mod, only: layer_infla, layer_scale
      use soil_data_struct_defs, only: soil_def, allocate_soil, deallocate_soil
      use soil_mod, only: depthini

!     + + + ARGUMENTS + + +
      type(soil_def), intent(inout) :: soil  ! soil for the subregion

      include 'p1werm.inc'
      include 'h1hydro.inc'
      include 'h1db1.inc'

!     + + + LOCAL VARIABLES + + +
      type(soil_def) :: soil_split

      ! copy all input values from soil to soil_split
      ! NOTE: array declared as allocatable, so arrays copied as well
      soil_split = soil
      ! deallocate arrays in new soil structure
      call deallocate_soil(soil_split)

      ! find number of soil layers in modified soil layering
      call soil_layer_count( layer_infla, layer_scale, soil_split%nslay, soil%aszlyt )

      ! allocate layer arrays for modified soil layering
      call allocate_soil(soil_split)

      ! create modified soil layering
      call soil_layer_split( layer_infla, layer_scale, soil%aszlyt, soil_split%aszlyt )

      ! recalculate  depth to bottom of soil layer for old and new layering
      call depthini( soil%nslay, soil%aszlyt, soil%aszlyd )
      call depthini( soil_split%nslay, soil_split%aszlyt, soil_split%aszlyd )

      ! average soil properties and put back into property arrays
      ! save old layer values of property before placing new values
      ! into enlarged array. All layers are averaged, allowing for
      ! new layers to be either smaller or larger than original

      ! IP soil physical properties
      call move_ave_val( soil%nslay, soil%aszlyd, soil%asfsan, soil_split%nslay, soil_split%aszlyd, soil_split%asfsan )
      call move_ave_val( soil%nslay, soil%aszlyd, soil%asfsil, soil_split%nslay, soil_split%aszlyd, soil_split%asfsil )
      call move_ave_val( soil%nslay, soil%aszlyd, soil%asfcla, soil_split%nslay, soil_split%aszlyd, soil_split%asfcla )
      call move_ave_val( soil%nslay, soil%aszlyd, soil%asvroc, soil_split%nslay, soil_split%aszlyd, soil_split%asvroc )
      call move_ave_val( soil%nslay, soil%aszlyd, soil%asfvcs, soil_split%nslay, soil_split%aszlyd, soil_split%asfvcs )
      call move_ave_val( soil%nslay, soil%aszlyd, soil%asfcs, soil_split%nslay, soil_split%aszlyd, soil_split%asfcs )
      call move_ave_val( soil%nslay, soil%aszlyd, soil%asfms, soil_split%nslay, soil_split%aszlyd, soil_split%asfms )
      call move_ave_val( soil%nslay, soil%aszlyd, soil%asffs, soil_split%nslay, soil_split%aszlyd, soil_split%asffs )
      call move_ave_val( soil%nslay, soil%aszlyd, soil%asfvfs, soil_split%nslay, soil_split%aszlyd, soil_split%asfvfs )
      call move_ave_val( soil%nslay, soil%aszlyd, soil%asdwblk, soil_split%nslay, soil_split%aszlyd, soil_split%asdwblk )
      call move_ave_val( soil%nslay, soil%aszlyd, soil%asdwsrat, soil_split%nslay, soil_split%aszlyd, soil_split%asdwsrat )

      ! IP soil chemical properties
      call move_ave_val( soil%nslay, soil%aszlyd, soil%asfom, soil_split%nslay, soil_split%aszlyd, soil_split%asfom )
      call move_ave_val( soil%nslay, soil%aszlyd, soil%as0ph, soil_split%nslay, soil_split%aszlyd, soil_split%as0ph )
      call move_ave_val( soil%nslay, soil%aszlyd, soil%asfcce, soil_split%nslay, soil_split%aszlyd, soil_split%asfcce )
      call move_ave_val( soil%nslay, soil%aszlyd, soil%asfcec, soil_split%nslay, soil_split%aszlyd, soil_split%asfcec )
      call move_ave_val( soil%nslay, soil%aszlyd, soil%asfcle, soil_split%nslay, soil_split%aszlyd, soil_split%asfcle )

      ! IC aggregate properties
      call move_ave_val( soil%nslay, soil%aszlyd, soil%aslagm, soil_split%nslay, soil_split%aszlyd, soil_split%aslagm )
      call move_ave_val( soil%nslay, soil%aszlyd, soil%as0ags, soil_split%nslay, soil_split%aszlyd, soil_split%as0ags )
      call move_ave_val( soil%nslay, soil%aszlyd, soil%aslagx, soil_split%nslay, soil_split%aszlyd, soil_split%aslagx )
      call move_ave_val( soil%nslay, soil%aszlyd, soil%aslagn, soil_split%nslay, soil_split%aszlyd, soil_split%aslagn )
      call move_ave_val( soil%nslay, soil%aszlyd, soil%asdagd, soil_split%nslay, soil_split%aszlyd, soil_split%asdagd )
      call move_ave_val( soil%nslay, soil%aszlyd, soil%aseags, soil_split%nslay, soil_split%aszlyd, soil_split%aseags )

      ! IC soil hydrologic properties
      call move_ave_val( soil%nslay, soil%aszlyd, soil%asdblk, soil_split%nslay, soil_split%aszlyd, soil_split%asdblk )
      call move_ave_val( soil%nslay, soil%aszlyd, soil%asdblk0, soil_split%nslay, soil_split%aszlyd, soil_split%asdblk0 )
      call move_ave_val( soil%nslay, soil%aszlyd, soil%ahrwc, soil_split%nslay, soil_split%aszlyd, soil_split%ahrwc )
      call move_ave_val( soil%nslay, soil%aszlyd, soil%ahrwcs, soil_split%nslay, soil_split%aszlyd, soil_split%ahrwcs )
      call move_ave_val( soil%nslay, soil%aszlyd, soil%ahrwcf, soil_split%nslay, soil_split%aszlyd, soil_split%ahrwcf )
      call move_ave_val( soil%nslay, soil%aszlyd, soil%ahrwcw, soil_split%nslay, soil_split%aszlyd, soil_split%ahrwcw )

      ! soil hydrologic (water release curve) properties
      call move_ave_val( soil%nslay, soil%aszlyd, soil%ah0cb, soil_split%nslay, soil_split%aszlyd, soil_split%ah0cb )
      call move_ave_val( soil%nslay, soil%aszlyd, soil%aheaep, soil_split%nslay, soil_split%aszlyd, soil_split%aheaep )
      call move_ave_val( soil%nslay, soil%aszlyd, soil%ahrsk, soil_split%nslay, soil_split%aszlyd, soil_split%ahrsk )

      ! New variable added that isn't read in any IFC file formats
      ! but is calculated with the -w4 cmdline option wc_type == 4
      ! before the layers are split
      call move_ave_val( soil%nslay, soil%aszlyd, soil%ahfredsat, soil_split%nslay, soil_split%aszlyd, soil_split%ahfredsat )

      ! return new layer split array
      soil = soil_split

      ! deallocate layer arrays for modified soil layering
      call deallocate_soil(soil_split)

      return

    end subroutine spllay_ifc


    subroutine spllay(soil)
      ! Converts NASIS layered IFC data into thin layered data
      ! Edit History
      ! 07-Feb-01   wjr   created

      use weps_main_mod, only: ifc_format, layer_infla, layer_scale
      use soil_data_struct_defs, only: soil_def, allocate_soil, deallocate_soil
      use soil_mod, only: depthini

!     + + + ARGUMENTS + + +
      type(soil_def), intent(inout) :: soil  ! subregion surface conditions

      include 'p1werm.inc'
      include 'h1hydro.inc'
      include 'h1db1.inc'

!     + + + LOCAL VARIABLES + + +
      type(soil_def) :: soil_split

      ! find number of soil layers in modified soil layering
      call soil_layer_count( layer_infla, layer_scale, soil_split%nslay, soil%aszlyt )

      ! copy all input values from soil to soil_split
      ! NOTE: array declared as allocatable, so arrays copied as well
      soil_split = soil
      ! deallocate arrays in new soil structure
      call deallocate_soil(soil_split)

      ! allocate layer arrays for modified soil layering
      call allocate_soil(soil_split)

      ! create modified soil layering
      call soil_layer_split( layer_infla, layer_scale, soil%aszlyt, soil_split%aszlyt )

      ! recalculate  depth to bottom of soil layer
      call depthini( soil_split%nslay, soil_split%aszlyt, soil_split%aszlyd )

      ! average soil properties and put back into property arrays
      ! save old layer values of property before placing new values
      ! into enlarged array. All layers are averaged, allowing for
      ! new layers to be either smaller of larger tha original
      call move_ave_val( soil%nslay, soil%aszlyd, soil%asfsan, soil_split%nslay, soil_split%aszlyd, soil_split%asfsan )
      call move_ave_val( soil%nslay, soil%aszlyd, soil%asfsil, soil_split%nslay, soil_split%aszlyd, soil_split%asfsil )
      call move_ave_val( soil%nslay, soil%aszlyd, soil%asfcla, soil_split%nslay, soil_split%aszlyd, soil_split%asfcla )
      call move_ave_val( soil%nslay, soil%aszlyd, soil%asvroc, soil_split%nslay, soil_split%aszlyd, soil_split%asvroc )

      ! New variable added that isn't read in "old" IFC file formats
      call move_ave_val( soil%nslay, soil%aszlyd, soil%asfvcs, soil_split%nslay, soil_split%aszlyd, soil_split%asfvcs )

      call move_ave_val( soil%nslay, soil%aszlyd, soil%asfcs, soil_split%nslay, soil_split%aszlyd, soil_split%asfcs )
      call move_ave_val( soil%nslay, soil%aszlyd, soil%asfms, soil_split%nslay, soil_split%aszlyd, soil_split%asfms )
      call move_ave_val( soil%nslay, soil%aszlyd, soil%asffs, soil_split%nslay, soil_split%aszlyd, soil_split%asffs )
      call move_ave_val( soil%nslay, soil%aszlyd, soil%asfvfs, soil_split%nslay, soil_split%aszlyd, soil_split%asfvfs )
      call move_ave_val( soil%nslay, soil%aszlyd, soil%asfwdc, soil_split%nslay, soil_split%aszlyd, soil_split%asfwdc )
      call move_ave_val( soil%nslay, soil%aszlyd, soil%asdblk, soil_split%nslay, soil_split%aszlyd, soil_split%asdblk )
      call move_ave_val( soil%nslay, soil%aszlyd, soil%asfcle, soil_split%nslay, soil_split%aszlyd, soil_split%asfcle )
      call move_ave_val( soil%nslay, soil%aszlyd, soil%asdwblk, soil_split%nslay, soil_split%aszlyd, soil_split%asdwblk )
      call move_ave_val( soil%nslay, soil%aszlyd, soil%asdwsrat, soil_split%nslay, soil_split%aszlyd, soil_split%asdwsrat )
      call move_ave_val( soil%nslay, soil%aszlyd, soil%aslagm, soil_split%nslay, soil_split%aszlyd, soil_split%aslagm )
      call move_ave_val( soil%nslay, soil%aszlyd, soil%as0ags, soil_split%nslay, soil_split%aszlyd, soil_split%as0ags )
      call move_ave_val( soil%nslay, soil%aszlyd, soil%aslagx, soil_split%nslay, soil_split%aszlyd, soil_split%aslagx )
      call move_ave_val( soil%nslay, soil%aszlyd, soil%aslagn, soil_split%nslay, soil_split%aszlyd, soil_split%aslagn )
      call move_ave_val( soil%nslay, soil%aszlyd, soil%asdagd, soil_split%nslay, soil_split%aszlyd, soil_split%asdagd )
      call move_ave_val( soil%nslay, soil%aszlyd, soil%aseags, soil_split%nslay, soil_split%aszlyd, soil_split%aseags )
      call move_ave_val( soil%nslay, soil%aszlyd, soil%ahrwc, soil_split%nslay, soil_split%aszlyd, soil_split%ahrwc )
      call move_ave_val( soil%nslay, soil%aszlyd, soil%ahrwcs, soil_split%nslay, soil_split%aszlyd, soil_split%ahrwcs )
      call move_ave_val( soil%nslay, soil%aszlyd, soil%ahrwcf, soil_split%nslay, soil_split%aszlyd, soil_split%ahrwcf )
      call move_ave_val( soil%nslay, soil%aszlyd, soil%ahrwcw, soil_split%nslay, soil_split%aszlyd, soil_split%ahrwcw )
      call move_ave_val( soil%nslay, soil%aszlyd, soil%ahrwc1, soil_split%nslay, soil_split%aszlyd, soil_split%ahrwc1 )
      call move_ave_val( soil%nslay, soil%aszlyd, soil%ah0cb, soil_split%nslay, soil_split%aszlyd, soil_split%ah0cb )
      call move_ave_val( soil%nslay, soil%aszlyd, soil%aheaep, soil_split%nslay, soil_split%aszlyd, soil_split%aheaep )
      call move_ave_val( soil%nslay, soil%aszlyd, soil%ahrsk, soil_split%nslay, soil_split%aszlyd, soil_split%ahrsk )
      call move_ave_val( soil%nslay, soil%aszlyd, soil%asfom, soil_split%nslay, soil_split%aszlyd, soil_split%asfom )
      call move_ave_val( soil%nslay, soil%aszlyd, soil%as0ph, soil_split%nslay, soil_split%aszlyd, soil_split%as0ph )
      call move_ave_val( soil%nslay, soil%aszlyd, soil%asfcce, soil_split%nslay, soil_split%aszlyd, soil_split%asfcce )
      call move_ave_val( soil%nslay, soil%aszlyd, soil%asfcec, soil_split%nslay, soil_split%aszlyd, soil_split%asfcec )
      ! This value only available in new IFC format
      if (ifc_format .gt. 1) then
        call move_ave_val( soil%nslay, soil%aszlyd, soil%asfcle, soil_split%nslay, soil_split%aszlyd, soil_split%asfcle )
      end if

      ! New variable added that isn't read in any IFC file formats
      ! but is calculated with the -w4 cmdline option wc_type == 4
      ! before the layers are split
      call move_ave_val( soil%nslay, soil%aszlyd, soil%ahfredsat, soil_split%nslay, soil_split%aszlyd, soil_split%ahfredsat )

      ! return new layer split array
      soil = soil_split

      ! deallocate layer arrays for modified soil layering
      call deallocate_soil(soil_split)

      return

    end subroutine spllay

end module split_layers_mod
