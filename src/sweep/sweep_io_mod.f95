!$Author$
!$Date$
!$Revision$
!$HeadURL$

module sweep_io_mod

   ! SWEEP routines for reading input file and creating output file
   implicit none
   private

   integer :: debugflg   ! flag to output debug data (0 = none, 1 = input, 2 = more, etc.)
   integer :: xplot      ! flag to put plot data in arrays (value>0 = no. indep input variable, 0= none)
   character*12 :: xcharin(30)   ! indep. variable name(s) used in plot
   real :: xin(30)               ! indep. variable value(s) used in plot

   integer xchl          ! length of xplot (independent variable) character strings
   parameter (xchl = 12)

   integer :: mrcl       ! length of line to be read in with getline
   parameter (mrcl = 1024)

   public :: erodin
   public :: erodout

   contains

   function erodin (input_filepath, i_unit, cmdebugflag, hagen_plot_flag) result(xmlformat)

      ! +++ PURPOSE +++
      ! Utility to read initial conditions and variables from
      ! input file (stdin or erod.in) for the standalone erosion submodel

      ! + + + Modules Used + + +
      use flib_sax
      use sweep_io_xml_defs, only: init_input_xml
      use sweep_io_xml_mod, only: sweepdata_complete, isr, treatmentdata_complete, soilstate_complete
      use sweep_io_xml_mod, only: begin_sweep_element_handler, end_sweep_element_handler, pcdata_sweep_chunk_handler
      use sweep_io_xml_mod, only: begin_treatment_element_handler, end_treatment_element_handler, pcdata_treatment_chunk_handler
      use sweep_io_xml_mod, only: begin_soilstate_element_handler, end_soilstate_element_handler, pcdata_soilstate_chunk_handler
      use sweep_io_xml_mod, only: nsubr
      use sae_in_out_mod, only: mksaeinp, subrfiles
      use grid_mod, only: gridfile, griddata_complete, init_grid_xml
      use grid_mod, only: begin_griddata_element_handler, end_griddata_element_handler, pcdata_griddata_chunk_handler

      ! +++ ARGUMENT DECLARATIONS +++
      character*1024  :: input_filepath
      integer :: i_unit
      integer :: cmdebugflag
      logical :: hagen_plot_flag
      logical :: xmlformat

      ! +++ LOCAL VARIABLES +++
      character*(mrcl) line

      type(xml_t) :: fxml   ! xml file handle structure
      integer :: iostat     ! input/output status

      ! +++ END SPECIFICATIONS +++

      ! +++ INITIALIZATION +++

      line = adjustl(getline(i_unit))
      if (line (1:8).eq.'Version: ') then
          ! interim file format, now abandoned
          write(*,*) 'ERROR: File format with Version: in header has been abandoned'
          call exit(1)
      else if (index(line, 'xml') .gt. 0) then
          if (i_unit .ne. 5) then
            close(i_unit)
          else
            write(*,*) 'ERROR: XML input file must be input using the -i flag on the command line'
            call exit(1)
          endif
          ! set file type for return value
          xmlformat = .true.

          ! open input file
          call open_xmlfile(trim(input_filepath),fxml,iostat)
          if (iostat /= 0) then
            write(*,*) "Cannot open xml input file: ", trim(input_filepath)
            stop
          end if
          ! read in xml based input file
          call init_input_xml()
          call xml_parse(fxml, &
             begin_element_handler = begin_sweep_element_handler, &
             end_element_handler = end_sweep_element_handler, &
             pcdata_chunk_handler = pcdata_sweep_chunk_handler, &
             verbose = .false.)
          call close_xmlfile(fxml)
          if (.not. sweepdata_complete) then
            write(*,*) 'Simulation run file incomplete'
            call exit(1)
          end if

          do isr = 1, nsubr

            ! open treatment file
            call open_xmlfile(trim(mksaeinp%fullpath) // trim(subrfiles(isr)%treatfil),fxml,iostat)
            if (iostat /= 0) then
              write(*,*) "Cannot open xml input file: ", trim(mksaeinp%fullpath) // trim(subrfiles(isr)%treatfil)
              stop
            end if
            ! read in xml based input file
            call init_input_xml()
            call xml_parse(fxml, &
              begin_element_handler = begin_treatment_element_handler, &
              end_element_handler = end_treatment_element_handler, &
              pcdata_chunk_handler = pcdata_treatment_chunk_handler, &
              verbose = .false.)
            call close_xmlfile(fxml)
            if (.not. treatmentdata_complete(isr)) then
              write(*,*) 'Treatment File incomplete, Subregion: ', isr
              call exit(1)
            end if

            ! open soilstate file
            call open_xmlfile(trim(mksaeinp%fullpath) // trim(subrfiles(isr)%slstfil),fxml,iostat)
            if (iostat /= 0) then
              write(*,*) "Cannot open xml input file: ", trim(mksaeinp%fullpath) // trim(subrfiles(isr)%slstfil)
              stop
            end if
            ! read in xml based input file
            call init_input_xml()
            call xml_parse(fxml, &
              begin_element_handler = begin_soilstate_element_handler, &
              end_element_handler = end_soilstate_element_handler, &
              pcdata_chunk_handler = pcdata_soilstate_chunk_handler, &
              verbose = .false.)
            call close_xmlfile(fxml)
            if (.not. soilstate_complete(isr)) then
              write(*,*) 'Soil State File incomplete, Subregion: ', isr
              call exit(1)
            end if

          end do

          ! open grid file
          call open_xmlfile(trim(mksaeinp%fullpath) // trim(gridfile),fxml,iostat)
          if (iostat /= 0) then
              write(*,*) "Cannot open xml input file: ", trim(mksaeinp%fullpath) // trim(gridfile)
              stop
            end if
          ! Read in grid subregion assignments from erod.grdx
          call init_grid_xml()
          call xml_parse(fxml, &
            begin_element_handler = begin_griddata_element_handler, &
            end_element_handler = end_griddata_element_handler, &
            pcdata_chunk_handler = pcdata_griddata_chunk_handler, &
            verbose = .false.)
          call close_xmlfile(fxml)
          if (.not. griddata_complete) then
            write(*,*) 'Grid Data File incomplete'
            call exit(1)
          end if

          return
      else
          ! set file type for return value
          xmlformat = .false.

          ! unversioned file, read old file format
          call erodin_legacy (line, i_unit, cmdebugflag, hagen_plot_flag)
          return
      end if

   end function erodin

   subroutine erodin_legacy (line, i_unit, cmdebugflag, hagen_plot_flag)

      ! +++ PURPOSE +++
      ! Utility to read initial conditions and variables from
      ! input file (stdin or erod.in) for the standalone erosion submodel

      ! + + + Modules Used + + +
      use Polygons_Mod, only: create_polygon
      use subregions_mod, only: subr_poly, acct_poly
      use erosion_data_struct_defs, only: subregionsurfacestate, create_brcdinputpools, &
                                          create_subregionsoillayers, create_subregionsurfacewet, &
                                          awzypt, awdair, anemht, awzzo, wzoflg, &
                                          ntstep, awadir, awudmx, subday, am0eif, subrsurf
      use p1erode_def, only: SLRR_MIN, SLRR_MAX, WZZO_MIN, WZZO_MAX
      use barriers_mod, only: create_barrier, barrier, barseas
      use grid_mod, only: amasim, amxsim
      use sweep_io_xml_mod, only: nsubr

      ! +++ ARGUMENT DECLARATIONS +++
      character*(mrcl), intent(inout) :: line
      integer, intent(in) :: i_unit
      integer, intent(in) :: cmdebugflag
      logical, intent(in) :: hagen_plot_flag

      ! +++ LOCAL VARIABLES +++
      integer i,j            ! do loop indices
      integer sr,ibr,a,l,h   ! do loop indices
      real :: pt_x1, pt_y1, pt_x2, pt_y2 ! the x,y coordinates for two points
      integer wflg           ! flag to determine format of wind speed data (0 = Weibull, 1 = real)
      real :: f(ntstep)      ! cumulative frequency of wind at speeds < subday(i)%awu
      real :: wu(ntstep)
      real :: wfcalm         ! wind fraction intercept (+calm, - no calm in period)
      real :: wuc            ! Weibull wind speed distribution scale factor (m/s)
      real :: w0k            ! Weibull wind speed distribution shape factor
      real :: step           ! tmp real variable for ntstep
      integer :: poly_np     ! number of points to be read in for polygon or polyline
      integer :: ipol        ! index counter for reading in polygon or polyline points
      integer :: alloc_stat  ! indicates status of memory allocation attempt
      integer :: sum_stat    ! accumulates for multiple allocations, one error statement.
      integer :: npools      ! number of brcdInput pools
      integer :: nacctr      ! number of accounting regions (read from input file)
      integer :: nbr         ! number of barriers (read from input file)

      ! variable removed from sweep input, since not used in erosion (see brcdInput pointers)
      real :: adzht_ave  ! Average residue height (m)
      real :: aczht      ! Crop height (m)
      real :: acrsai     ! Crop stem area index (m^2/m^2)
      real :: acrlai     ! Crop leaf area index (m^2/m^2)
      real :: adrsaitot  ! Residue stem area index (m^2/m^2)
      real :: adrlaitot  ! Residue leaf area index (m^2/m^2)
      real :: acxrow     ! Crop row spacing (m)
      integer :: ac0rg   ! Crop seed placement (0 - furrow, 1 - ridge)

      ! +++ FUNCTIONS CALLED +++
      ! getline

      ! +++ END SPECIFICATIONS +++

      ! +++ INITIALIZATION +++

      debugflg = 0 !needs to be initialized when using full debugging compiles
      ! Read the debug flag to specify level of debug support
      ! (currently only input file level debug supported)

      if (cmdebugflag .lt. 0) then  !commandline option not set - use input file setting
          read (line,*) debugflg
       else
          debugflg = cmdebugflag  !use commandline setting
      endif

      ! EROSION initialization flag (logical)
      line = getline(i_unit)
      read (line,*) am0eif

      ! EROSION "print" flag (integer)
      line = getline(i_unit)
      ! value no longer used

      ! +++ SIMULATION REGION +++

      ! Simulation region diagonal corners (x1,y1) and (x2,y2)
      line = getline(i_unit)
      read (line,*) amxsim(1)%x, amxsim(1)%y, amxsim(2)%x, amxsim(2)%y

      ! Simulation region orientation angle
      line = getline(i_unit)
      read (line,*) amasim

      ! +++ ACCOUNTING REGIONS +++

      ! Number of accounting regions
      line = getline(i_unit)
      read (line,*) nacctr

      ! create accounting region polygon array
      allocate(acct_poly(nacctr), stat=alloc_stat)
      if( alloc_stat .gt. 0 ) then
         Write(*,*) 'ERROR: memory allocation, acct_poly'
      end if

      do a = 1, nacctr
        ! Accounting Region diagonal corners (x1,y1) and (x2,y2)
        line = getline(i_unit)
        read (line,*) pt_x1, pt_y1, pt_x2, pt_y2

        ! create accounting region polygon from the diagonal corners
        poly_np = 4
        ! create polygon point storage
        acct_poly(a) = create_polygon(poly_np)
        ! set polygon points
        ipol = 1
        acct_poly(a)%points(ipol)%x = pt_x1
        acct_poly(a)%points(ipol)%y = pt_y1
        ipol = 2
        acct_poly(a)%points(ipol)%x = pt_x2
        acct_poly(a)%points(ipol)%y = pt_y1
        ipol = 3
        acct_poly(a)%points(ipol)%x = pt_x2
        acct_poly(a)%points(ipol)%y = pt_y2
        ipol = 4
        acct_poly(a)%points(ipol)%x = pt_x1
        acct_poly(a)%points(ipol)%y = pt_y2
      end do

      ! +++ BARRIERS +++

      ! Number of barriers
      line = getline(i_unit)
      read (line,*) nbr

      ! allocate structure for barriers (nbr .lt. 1 gives zero size array)
      allocate(barrier(nbr), stat = alloc_stat)
      if( alloc_stat .gt. 0 ) then
         Write(*,*) 'ERROR: memory alloc., barriers'
      end if
      allocate(barseas(nbr), stat = alloc_stat)
      if( alloc_stat .gt. 0 ) then
         Write(*,*) 'ERROR: memory alloc., barriers'
      end if

      ! NOTE: Barrier data must not be in the input file if "nbr = 0"
      do ibr = 1, nbr
!       Barrier linear endpoints (x1,y1) and (x2,y2)
        line = getline(i_unit)
        read (line,*) pt_x1, pt_y1, pt_x2, pt_y2

        ! number of points in barrier polyline
        poly_np = 2
        ! create storage for point and barrier data
        call create_barrier(barrier(ibr), poly_np)
        call create_barrier(barseas(ibr), poly_np,1,0)
        ! set points
        ipol = 1
        barseas(ibr)%points(ipol)%x = pt_x1
        barseas(ibr)%points(ipol)%y = pt_y1
        !  also place in fixed barrier structure
        barrier(ibr)%points(ipol)%x = pt_x1
        barrier(ibr)%points(ipol)%y = pt_y1
        ipol = 2
        barseas(ibr)%points(ipol)%x = pt_x2
        barseas(ibr)%points(ipol)%y = pt_y2
        !  also place in fixed barrier structure
        barrier(ibr)%points(ipol)%x = pt_x2
        barrier(ibr)%points(ipol)%y = pt_y2

        ! barrier height, porosity, width
        line = getline(i_unit)
        ipol = 1
        read (line,*) barseas(ibr)%param(ipol,1)%amzbr, barseas(ibr)%param(ipol,1)%ampbr, barseas(ibr)%param(ipol,1)%amxbrw
        !  also place in fixed barrier structure
        barrier(ibr)%param(ipol) = barseas(ibr)%param(ipol,1)
        ipol = 2
        read (line,*) barseas(ibr)%param(ipol,1)%amzbr, barseas(ibr)%param(ipol,1)%ampbr, barseas(ibr)%param(ipol,1)%amxbrw
        !  also place in fixed barrier structure
        barrier(ibr)%param(ipol) = barseas(ibr)%param(ipol,1)
        if( barrier(ibr)%param(ipol)%amzbr .le. 0.0 ) then
           write(*,*) 'ERROR: Barrier height must be > 0'
           write(*,FMT='(2(i0))') 'Barrier #: ', ibr, 'Point #: ', ipol
           call exit(37)
        end if
      end do

      ! +++ SUBREGION REGIONS +++

      ! Number of subregions
      line = getline(i_unit)
      read (line,*) nsubr

      if( nsubr .gt. 1 ) then
        write(*,*) 'Expected legacy file to have only one subregion. Found: ', nsubr
        write(*,*) 'Results may not be valid.'
      end if

      ! create data array to hold input and derived values for each subregion
      sum_stat = 0
      allocate(subrsurf(1, nsubr), stat=alloc_stat)
      sum_stat = sum_stat + alloc_stat
      ! create subregion polygon array
      allocate(subr_poly(nsubr), stat=alloc_stat)
      sum_stat = sum_stat + alloc_stat
      if( sum_stat .gt. 0 ) then
         Write(*,*) 'ERROR: memory allocation, subrsurf, subr_poly'
      end if

      ! Dimensions, Biomass, Soil, and Hydrology (by subregion)
      do 100  sr=1, nsubr
!       Subregion diagonal corners (x1,y1) and (x2,y2)
        line = getline(i_unit)
        read (line,*) pt_x1, pt_y1, pt_x2, pt_y2

        ! create subregion polygon from the diagonal corners
        poly_np = 4
        ! create polygon point storage
        subr_poly(sr) = create_polygon(poly_np)
        ! set polygon points
        ipol = 1
        subr_poly(sr)%points(ipol)%x = pt_x1
        subr_poly(sr)%points(ipol)%y = pt_y1
        ipol = 2
        subr_poly(sr)%points(ipol)%x = pt_x2
        subr_poly(sr)%points(ipol)%y = pt_y1
        ipol = 3
        subr_poly(sr)%points(ipol)%x = pt_x2
        subr_poly(sr)%points(ipol)%y = pt_y2
        ipol = 4
        subr_poly(sr)%points(ipol)%x = pt_x1
        subr_poly(sr)%points(ipol)%y = pt_y2

      ! +++ BIOMASS +++

      ! b1geom.inc

        ! Biomass height
        !  line = getline(i_unit)
        !  read (line,*)  abzht(sr)
        !! read (getline(i_unit),*) abzht(sr)
        ! Now reads in average "residue height" instead of "biomass height'
        ! LEW - 1/26/06
        line = getline(i_unit)
        read (line,*)  adzht_ave

        ! Crop height
        line = getline(i_unit)
        read (line,*)  aczht

        ! Crop stem area index and leaf area index
        line = getline(i_unit)
        read (line,*) acrsai, acrlai

        ! Residue stem area index and leaf area index
        line = getline(i_unit)
        read (line,*) adrsaitot, adrlaitot

        ! use crop and residue values to find the total value
        ! sum the stem area index and leaf area index values
        subrsurf(1,sr)%abrsai = acrsai + adrsaitot
        subrsurf(1,sr)%abrlai = acrlai + adrlaitot

        ! Compute the weighted average "biomass height" (residues and crop)
        ! which is used internally by the erosion code - LEW 1/26/06
        if (subrsurf(1,sr)%abrsai .le. 0.0) then
            subrsurf(1,sr)%abzht = 0.0
        else
            subrsurf(1,sr)%abzht = ( adzht_ave * adrsaitot + aczht * acrsai ) / subrsurf(1,sr)%abrsai
        endif

        ! addition to code for biodrag
        ! crop row spacing and seed location
        line = getline(i_unit)
        read (line,*) acxrow, ac0rg

        ! count number of brcdInput pools needed
        npools = 0
        if( (aczht .gt. 0.0) .and. ((acrsai .gt. 0.0) .or. (acrlai .gt. 0.0)) ) then
          npools = npools + 1
        end if
        if( (adzht_ave .gt. 0.0) .and. ((adrsaitot .gt. 0.0) .or. (adrlaitot .gt. 0.0)) ) then
          npools = npools + 1
        end if
        subrsurf(1,sr)%npools = npools
        call create_brcdinputpools( npools, subrsurf(1,sr) )

        ! Place old values for lai, sai into brcdInput
        npools = 0
        if( (aczht .gt. 0.0) .and. ((acrsai .gt. 0.0) .or. (acrlai .gt. 0.0)) ) then
          ! biodrag elements exist, add brcdInput
          npools = npools + 1
          subrsurf(1,sr)%brcdInput(npools)%bname = 'crop'
          subrsurf(1,sr)%brcdInput(npools)%rlai = acrlai
          subrsurf(1,sr)%brcdInput(npools)%rsai = acrsai
          subrsurf(1,sr)%brcdInput(npools)%rg = ac0rg
          subrsurf(1,sr)%brcdInput(npools)%xrow = acxrow
          subrsurf(1,sr)%brcdInput(npools)%zht = aczht
        end if

        if( (adzht_ave .gt. 0.0) .and. ((adrsaitot .gt. 0.0) .or. (adrlaitot .gt. 0.0)) ) then
          ! biodrag elements exist, add brcdInput
          npools = npools + 1
          subrsurf(1,sr)%brcdInput(npools)%bname = 'residue'
          subrsurf(1,sr)%brcdInput(npools)%rlai = adrlaitot
          subrsurf(1,sr)%brcdInput(npools)%rsai = adrsaitot
          subrsurf(1,sr)%brcdInput(npools)%rg = 0
          subrsurf(1,sr)%brcdInput(npools)%xrow = 0.0
          subrsurf(1,sr)%brcdInput(npools)%zht = adzht_ave
        end if

        ! These aren't used in EROSION yet
        ! Biomass stem area index by height
        ! read (getline(i_unit),*) (abrsaz(h,sr), h=1,mncz)
        ! Biomass leaf area index by height
        ! read (getline(i_unit),*) (abrlaz(h,sr), h=1,mncz)

        ! Biomass flat fraction cover, standing cover, and fraction total cover
        ! read (getline(i_unit),*) abffcv(sr), abfscv(sr), abftcv(sr)
        ! Only flat fraction cover used yet
        line = getline(i_unit)
        read (line,*) subrsurf(1,sr)%abffcv

      ! +++ SOIL +++

        ! Number of soil layers (in this subregion)
        line = getline(i_unit)
        read (line,*) subrsurf(1,sr)%nslay

        ! allocate arrays for soil layer and surface wetness values
        call create_subregionsoillayers(subrsurf(1,sr)%nslay, subrsurf(1,sr))
        call create_subregionsurfacewet(24, subrsurf(1,sr))

        ! Soil layer thickness
        line = getline(i_unit)
        read (line,*) (subrsurf(1,sr)%bsl(l)%aszlyt,l=1,subrsurf(1,sr)%nslay)

        ! Soil layer bulk density
        line = getline(i_unit)
        read (line,*) (subrsurf(1,sr)%bsl(l)%asdblk, l=1,subrsurf(1,sr)%nslay)

        ! Sand, silt, and clay fractions
        line = getline(i_unit)
        read (line,*) (subrsurf(1,sr)%bsl(l)%asfsan, l=1,subrsurf(1,sr)%nslay)

        ! read very fine sand content edit 6-9-01 LH
        line = getline(i_unit)
        read (line,*) (subrsurf(1,sr)%bsl(l)%asfvfs, l=1,subrsurf(1,sr)%nslay)

        line = getline(i_unit)
        read (line,*) (subrsurf(1,sr)%bsl(l)%asfsil, l=1,subrsurf(1,sr)%nslay)

        line = getline(i_unit)
        read (line,*) (subrsurf(1,sr)%bsl(l)%asfcla, l=1,subrsurf(1,sr)%nslay)

        ! Volume of rock fraction
        line = getline(i_unit)
        read (line,*) (subrsurf(1,sr)%bsl(l)%asvroc, l=1,subrsurf(1,sr)%nslay)

        ! Soil layer aggregate density
        line = getline(i_unit)
        read (line,*) (subrsurf(1,sr)%bsl(l)%asdagd, l=1,subrsurf(1,sr)%nslay)

        ! Soil layer aggregate stability
        line = getline(i_unit)
        read (line,*) (subrsurf(1,sr)%bsl(l)%aseags, l=1,subrsurf(1,sr)%nslay)

! Check these variables with ASD inc files and Hagen's EROSION inc files - LEW
        ! Soil layer ASD parms (gmd, min, max, gsd)
        line = getline(i_unit)
        read (line,*) (subrsurf(1,sr)%bsl(l)%aslagm, l=1,subrsurf(1,sr)%nslay)

        line = getline(i_unit)
        read (line,*) (subrsurf(1,sr)%bsl(l)%aslagn, l=1,subrsurf(1,sr)%nslay)

        line = getline(i_unit)
        read (line,*) (subrsurf(1,sr)%bsl(l)%aslagx, l=1,subrsurf(1,sr)%nslay)

        line = getline(i_unit)
        read (line,*) (subrsurf(1,sr)%bsl(l)%as0ags, l=1,subrsurf(1,sr)%nslay)

        ! Crust parms (fraction, thickness)
        line = getline(i_unit)
        read (line,*) subrsurf(1,sr)%asfcr, subrsurf(1,sr)%aszcr, &
        ! Crust parms (fraction cover of loose material, mass loose material)
             subrsurf(1,sr)%asflos, subrsurf(1,sr)%asmlos, &
        ! Crust parms (crust density and stability)
             subrsurf(1,sr)%asdcr, subrsurf(1,sr)%asecr

        ! Random Roughness
        line = getline(i_unit)
        read (line,*) subrsurf(1,sr)%aslrr

        !Lower and upper limits of grid cell RR allowed by erosion submodel
        if (subrsurf(1,sr)%aslrr < SLRR_MIN) then
           write(0,*) 'slrr: ', subrsurf(1,sr)%aslrr,' < ', SLRR_MIN
        end if
        if (subrsurf(1,sr)%aslrr > SLRR_MAX) then
           write(0,*) 'slrr: ', subrsurf(1,sr)%aslrr,' < ', SLRR_MIN
        end if

        !Lower and upper limits of grid cell aerodynamic roughness allowed
        !by erosion submodel (currently determined by equation used here)
        if (subrsurf(1,sr)%aslrr < (WZZO_MIN/0.3)) then
           write(0,*) 'slrr: ', subrsurf(1,sr)%aslrr
           write(0,*) 'wzzo < WZZO_MIN: ', subrsurf(1,sr)%aslrr*0.3,' < ', WZZO_MIN
        else if(subrsurf(1,sr)%aslrr > (WZZO_MAX/0.3)) then
           write(0,*) 'slrr: ', subrsurf(1,sr)%aslrr
           write(0,*) 'wzzo > WZZO_MAX: ', subrsurf(1,sr)%aslrr*0.3,' > ', WZZO_MAX
        end if

        ! Oriented Roughness (ridge ht, spacing, width, orientation)
        line = getline(i_unit)
        read (line,*) subrsurf(1,sr)%aszrgh, subrsurf(1,sr)%asxrgs, subrsurf(1,sr)%asxrgw, subrsurf(1,sr)%asargo

        ! Oriented Roughness ( spacing)
        line = getline(i_unit)
        read (line,*) subrsurf(1,sr)%asxdks

      ! +++ HYDROLOGY +++

        ! Snow depth
        line = getline(i_unit)
        read (line,*) subrsurf(1,sr)%ahzsnd

        ! Soil layer wilting point
        line = getline(i_unit)
        read (line,*) (subrsurf(1,sr)%bsl(l)%ahrwcw, l=1,subrsurf(1,sr)%nslay)

        ! Soil layer water content
        line = getline(i_unit)
        read (line,*) (subrsurf(1,sr)%bsl(l)%ahrwca, l=1,subrsurf(1,sr)%nslay)

        ! Soil surface hourly water content
        line = getline(i_unit)
        read (line,*) (subrsurf(1,sr)%ahrwc0(h), h=1,12)

        line = getline(i_unit)
        read (line,*) (subrsurf(1,sr)%ahrwc0(h), h=13,24)

  100 continue

      ! +++ WEATHER +++

      ! Average annual precipitation
      awzypt = 300.0

! We need to check on the units for air density - variable definition says (kg/m^3)
! Also, we need to see why it currently isn't being used - LJH said it was
      ! Air density
      line = getline(i_unit)
      read (line,*) awdair

      ! Wind Direction
      line = getline(i_unit)
      read (line,*) awadir

      ! Number of "steps" during 24 hours (96 = 15 minute intervals)
      line = getline(i_unit)
      read (line,*) ntstep

      ! allocate wind direction and speed array
      allocate(subday(ntstep), stat=alloc_stat)
      if( alloc_stat .gt. 0 ) then
         Write(*,*) 'ERROR: memory allocation, erodin wind direction and speed'
      end if

      ! anemometer height, zo at anemom, and location (station or field)
      ! note if flag=1, at field, awwzo will be changed to field value
      line = getline(i_unit)
      read (line,*) anemht, awzzo, wzoflg

      ! Weibull wind flag (0 - read Weibull parms, 1 - read wind speeds)
      line = getline(i_unit)
      read (line,*) wflg

      ! wind data inputs as the Weibull paramters
      ! (wfcalm, wuc, w0k) is indicated by code ntstep = 99
      if (wflg .eq. 0) then

        ! Weibull parms (fraction calm, c, k)
        line = getline(i_unit)
        read (line,*) wfcalm, wuc, w0k

        ! calculate daily max wind speed (99% speed)
        ! awudmx = wuc*(-log((1.0-0.99)/(1-wfcalm)))**(1.0/w0k)

        ! calculate period wind speeds
        step = ntstep
        do 198 i= 1, ntstep
          ! find center of each step and add empirical last term from file ntstep.mcd
            f(i) = (1.0/(2.0*step)) + ((i-1)/step) +0.3/(step*w0k)
          ! to prevent out-of-range
          if (f(i) .lt. wfcalm) then
            f(i) = wfcalm
          endif
          subday(i)%awu = wuc*(-log((1.0-f(i))/(1.0-wfcalm)))**(1.0/w0k)
  198   end do
        ! Use greatest interval wind speed rather than 99% speed above
        awudmx = subday(ntstep)%awu

        ! change weibull wind speed dist. to a symmetric shape similar
        ! to the daily distribution from wind gen

        ! insure that ntstep is an even no.
        ntstep = (ntstep/2)*2

        ! store wind speed in temp array
        do 110 i = 1, ntstep
          wu(i) = subday(i)%awu
  110   end do
!
        ! generate the symmetric distribution
        i = -1
        do 115 j = 1, ntstep/2
           i = i+2
           subday(j)%awu = wu(i)
  115   continue
        i = ntstep+2
        do 125 j = (ntstep/2+1),ntstep
           i = i-2
           subday(j)%awu = wu(i)
  125   continue

      else     ! when (wflg .eq. 1) input wind period data directly
        do 191 j = 1, ntstep/6
          line = getline(i_unit)
          read (line,*) (subday(i)%awu,i=(j-1)*6+1,(j-1)*6+6)
191     end do
        ! If not divisible evenly by 6, then get the remaining values
        if (mod(ntstep,6) .ne. 0) then
          line = getline(i_unit)
          read (line,*) (subday(i)%awu,i=(j-1)*6+1,(j-1)*6+mod(ntstep,6))
        endif

      ! Determine the maximum wind speed during the day
        awudmx = 0.0
        do 193 i = 1, ntstep
           if( awudmx .lt. subday(i)%awu ) then
              awudmx = subday(i)%awu
           endif
  193   end do

      endif

      if( hagen_plot_flag ) then 
         call plotin(subrsurf(1,1))
      end if

   end subroutine erodin_legacy

   subroutine erodout (hagen_plot_flag)

      ! +++  PURPOSE +++
      ! To print output desired from standalone EROSION submodel

      use erosion_data_struct_defs, only: cellsurfacestate
      use sae_in_out_mod, only: daily_erodout, aegt, aegtcs, aegtss, aegt10

      ! +++ ARGUMENT DECLARATIONS +++
      logical, intent(in) :: hagen_plot_flag

      ! ++++ LOCAL VARIABLES +++
      character*12 ycharin(30)
      integer yplot
      real yin(30)

      ! +++ SUBROUTINES CALLED+++
      ! plotout

      ! +++ END SPECIFICATIONS +++

      ! test if plot info wanted
      if (hagen_plot_flag .EQV. .true.) then

        ! test if plot info available from input files
        ! (should allow one to mix input files and get only
        ! wanted plot output)
        if (xplot > -1) then
          ! specify plotout dep variables for all values of yplot
          yplot = 4
          ycharin(1) = 'total_eros'
          ycharin(2) = 'salt/creep'
          ycharin(3) = 'suspension'
          ycharin(4) = 'PM10(kg/m^2)'
          yin(1) = aegt
          yin(2) = aegtcs
          yin(3) = aegtss
          yin(4) = aegt10

          call plotout (yplot, ycharin, yin)
        endif
      endif
      ! end of plot section

   end subroutine erodout

   subroutine plotin(subrsurf)

      ! + + +  PURPOSE + + +
      ! To read the configuration (xin data) for sweep.eplt file creation

      ! WARNING: Only plots values for subregion #1

       ! "xplot" flag for writing variables to file "sweep.eplt".
       ! -1 = write nothing
       !  0 = write erosion variables;
       ! Actual variables listed below are only written if flagged with a 1

       ! NOTE:  The SWEEP cmdline option -Eplt determines if the data
       !        specfied here is appended to the sweep.eplt file.

      use file_io_mod, only: fopenk
      use erosion_data_struct_defs, only: subregionsurfacestate
      use grid_mod, only: amxsim


      type(subregionsurfacestate), intent(in) :: subrsurf

      integer i_unit   ! logical unit input file number
      character*(mrcl) line
      integer i, xflag

      ! read sweepplot.cfg  configuration file 
      call fopenk(i_unit, 'sweepplot.cfg', 'old')

      ! + + + PLOT SECTION + + +
      ! selectively reads succesive indep variable names and values
      ! if xplot is set to zero and the xflag is set to
      !  1 for a given variable in the erodin file

      ! test if plotout file to be created
      line = getline(i_unit)
      read (line,*) xplot
      if (xplot .eq. 0) then
      ! intialize xin array
      do 194 i=1,30
         xin(i) = 0.0
  194 continue

      ! field length (good for wind parallel x-axis)
        line = getline(i_unit)
        read (line,*) xflag
        if (xflag .eq. 1) then
          xplot = xplot + 1
          line = getline(i_unit)
          xcharin(xplot) = line(1:xchl)
          xin(xplot) = amxsim(2)%y - amxsim(1)%y
        else
          line = getline(i_unit)
        endif

      ! biomass height
        line = getline(i_unit)
        read (line,*) xflag
        if (xflag .eq. 1) then
          xplot = xplot + 1
          line = getline(i_unit)
          xcharin(xplot) = line(1:xchl)
          xin(xplot) = subrsurf%abzht
        else
          line = getline(i_unit)
        endif

      ! biomass stem area index
        line = getline(i_unit)
        read (line,*) xflag
        if (xflag .eq. 1) then
          xplot = xplot + 1
          line = getline(i_unit)
          xcharin(xplot) = line(1:xchl)
          xin(xplot) = subrsurf%abrsai
        else
          line = getline(i_unit)
        endif

      ! biomass leaf area index
        line = getline(i_unit)
        read (line,*) xflag
        if (xflag .eq. 1) then
          xplot = xplot + 1
          line = getline(i_unit)
          xcharin(xplot) = line(1:xchl)
          xin(xplot) = subrsurf%abrlai
        else
          line = getline(i_unit)
        endif

      ! biomass flat cover
        line = getline(i_unit)
        read (line,*) xflag
        if (xflag .eq. 1) then
          xplot = xplot + 1
          line = getline(i_unit)
          xcharin(xplot) = line(1:xchl)
          xin(xplot) = subrsurf%abffcv
        else
          line = getline(i_unit)
        endif

      ! very fine sand
        line = getline(i_unit)
        read (line,*) xflag
        if (xflag .eq. 1) then
          xplot = xplot + 1
          line = getline(i_unit)
          xcharin(xplot) = line(1:xchl)
          xin(xplot) = subrsurf%bsl(1)%asfvfs
        else
          line = getline(i_unit)
        endif

      ! sand
        line = getline(i_unit)
        read (line,*) xflag
        if (xflag .eq. 1) then
          xplot = xplot + 1
          line = getline(i_unit)
          xcharin(xplot) = line(1:xchl)
          xin(xplot) =  subrsurf%bsl(1)%asfsan
        else
          line = getline(i_unit)
        endif

      ! silt
        line = getline(i_unit)
        read (line,*) xflag
        if (xflag .eq. 1) then
          xplot = xplot + 1
          line = getline(i_unit)
          xcharin(xplot) = line(1:xchl)
          xin(xplot) = subrsurf%bsl(1)%asfsil
        else
          line = getline(i_unit)
        endif

      ! clay
        line = getline(i_unit)
        read (line,*) xflag
        if (xflag .eq. 1) then
          xplot = xplot + 1
          line = getline(i_unit)
          xcharin(xplot) = line(1:xchl)
          xin(xplot) =  subrsurf%bsl(1)%asfcla
        else
          line = getline(i_unit)
        endif

      ! rock vol.
        line = getline(i_unit)
        read (line,*) xflag
        if (xflag .eq. 1) then
          xplot = xplot + 1
          line = getline(i_unit)
          xcharin(xplot) = line(1:xchl)
          xin(xplot) =  subrsurf%bsl(1)%asvroc
        else
          line = getline(i_unit)
        endif

      ! aggregate density
        line = getline(i_unit)
        read (line,*) xflag
        if (xflag .eq. 1) then
          xplot = xplot + 1
          line = getline(i_unit)
          xcharin(xplot) = line(1:xchl)
          xin(xplot) = subrsurf%bsl(1)%asdagd
        else
          line = getline(i_unit)
        endif

      ! aggregate stability
        line = getline(i_unit)
        read (line,*) xflag
        if (xflag .eq. 1) then
          xplot = xplot + 1
          line = getline(i_unit)
          xcharin(xplot) = line(1:xchl)
          xin(xplot) =  subrsurf%bsl(1)%aseags
        else
          line = getline(i_unit)
        endif

      ! agregate geometric mean diameter
        line = getline(i_unit)
        read (line,*) xflag
        if (xflag .eq. 1) then
          xplot = xplot + 1
          line = getline(i_unit)
          xcharin(xplot) = line(1:xchl)
          xin(xplot) =  subrsurf%bsl(1)%aslagm
        else
          line = getline(i_unit)
        endif

      ! aggreate minimum diameter
        line = getline(i_unit)
        read (line,*) xflag
        if (xflag .eq. 1) then
          xplot = xplot + 1
          line = getline(i_unit)
          xcharin(xplot) = line(1:xchl)
          xin(xplot) =  subrsurf%bsl(1)%aslagn
        else
          line = getline(i_unit)
        endif

      ! aggregate maximum diameter
        line = getline(i_unit)
        read (line,*) xflag
        if (xflag .eq. 1) then
          xplot = xplot + 1
          line = getline(i_unit)
          xcharin(xplot) = line(1:xchl)
          xin(xplot) =  subrsurf%bsl(1)%aslagx
        else
          line = getline(i_unit)
        endif

      ! aggregate geometric std dev
        line = getline(i_unit)
        read (line,*) xflag
        if (xflag .eq. 1) then
          xplot = xplot + 1
          line = getline(i_unit)
          xcharin(xplot) = line(1:xchl)
          xin(xplot) =  subrsurf%bsl(1)%as0ags
        else
          line = getline(i_unit)
        endif

      ! soil fraction crust cover
        line = getline(i_unit)
        read (line,*) xflag
        if (xflag .eq. 1) then
          xplot = xplot + 1
          line = getline(i_unit)
          xcharin(xplot) = line(1:xchl)
          xin(xplot) = subrsurf%asfcr
        else
          line = getline(i_unit)
        endif
!
      ! surface crust thickness
        line = getline(i_unit)
        read (line,*) xflag
        if (xflag .eq. 1) then
          xplot = xplot + 1
          line = getline(i_unit)
          xcharin(xplot) = line(1:xchl)
          xin(xplot) = subrsurf%aszcr
        else
          line = getline(i_unit)
        endif

      ! fraction loose material on crust
        line = getline(i_unit)
        read (line,*) xflag
        if (xflag .eq. 1) then
          xplot = xplot + 1
          line = getline(i_unit)
          xcharin(xplot) = line(1:xchl)
          xin(xplot) =  subrsurf%asflos
        else
          line = getline(i_unit)
        endif

      ! mass of loose material on crust
        line = getline(i_unit)
        read (line,*) xflag
        if (xflag .eq. 1) then
          xplot = xplot + 1
          line = getline(i_unit)
          xcharin(xplot) = line(1:xchl)
          xin(xplot) =  subrsurf%asmlos
        else
          line = getline(i_unit)
        endif

      ! soil crust stability
        line = getline(i_unit)
        read (line,*) xflag
        if (xflag .eq. 1) then
          xplot = xplot + 1
          line = getline(i_unit)
          xcharin(xplot) = line(1:xchl)
          xin(xplot) =  subrsurf%asecr
        else
          line = getline(i_unit)
        endif

      ! random roughness
        line = getline(i_unit)
        read (line,*) xflag
        if (xflag .eq. 1) then
          xplot = xplot + 1
          line = getline(i_unit)
          xcharin(xplot) = line(1:xchl)
          xin(xplot) = subrsurf%aslrr
        else
          line = getline(i_unit)
        endif

      ! ridge height
        line = getline(i_unit)
        read (line,*) xflag
        if (xflag .eq. 1) then
          xplot = xplot + 1
          line = getline(i_unit)
          xcharin(xplot) = line(1:xchl)
          xin(xplot) =  subrsurf%aszrgh
        else
          line = getline(i_unit)
        endif

      ! ridge spacing
        line = getline(i_unit)
        read (line,*) xflag
        if (xflag .eq. 1) then
          xplot = xplot + 1
          line = getline(i_unit)
          xcharin(xplot) = line(1:xchl)
          xin(xplot) =  subrsurf%asxrgs
        else
          line = getline(i_unit)
        endif

      ! ridge width
        line = getline(i_unit)
        read (line,*) xflag
        if (xflag .eq. 1) then
          xplot = xplot + 1
          line = getline(i_unit)
          xcharin(xplot) = line(1:xchl)
          xin(xplot) =  subrsurf%asxrgw
        else
          line = getline(i_unit)
        endif

      ! ridge orientation
        line = getline(i_unit)
        read (line,*) xflag
        if (xflag .eq. 1) then
          xplot = xplot + 1
          line = getline(i_unit)
          xcharin(xplot) = line(1:xchl)
          xin(xplot) =  subrsurf%asargo
        else
          line = getline(i_unit)
        endif
      endif

      close(i_unit)

   end subroutine plotin

   subroutine plotout (yplot, ycharin, yin)

      ! + + +  PURPOSE + + +
      ! 1. to create headings for sweep.eplt file
      ! 2. to store dep var (yin) and indep var (xin)
      !     and write to sweep.eplt file for each eros run

      ! plotout is called from erodout.for with yin data
      ! the xin data come from file read by plotin

      use file_io_mod, only: fopenk

      ! + + + ARGUMENT DECLARATAIONS + + +
      integer yplot             ! number of dep variables to put in sweep.eplt file
      character*12 ycharin(30)  ! name(s) of dep. variables
      real yin(30)              ! value(s) of dep. variables

      ! + + + LOCAL VARIABLES + + +
      integer :: j       ! counter, loop index
      integer :: nline   ! number of lines read in from file
      logical :: used    ! logical for presence of sweep.eplt file
      character*500 line, plotdat(500) ! string read in from file
      integer :: luo1    ! output file unit number

      ! + + + FORMATS + + +
  200 format(40f12.4)
  201 format(' file:')
  202 format('sweep.eplt')

      ! + + + END SPECIFICATIONS + + +

      ! create sweep.eplt file
      inquire (FILE='sweep.eplt',EXIST=used)
      if(.not.used) then

        ! write heading for sweep.eplt
        call fopenk(luo1, 'sweep.eplt', 'new')
        write(luo1,201)
        write(luo1,202)
       ! write(luo1,*)((ycharin(i),i=1,yplot),(xcharin(i),i=1,xplot))
        write(luo1,*)  ycharin(1:yplot), xcharin(1:xplot)
        write(luo1,*)
        close(UNIT=luo1)
      endif

      ! read current sweep.eplt file to plotdat char. array
      call fopenk(luo1, 'sweep.eplt', 'old')
      rewind (UNIT=luo1)
      j = 0
   20 read (luo1, '(a)', end=50) line
      j = j + 1
      plotdat(j) = line
      go to 20

      ! update the sweep.eplt file
   50 nline = j
      rewind (UNIT=luo1)
      do 55 j = 1, nline
         write(luo1,'(a)') trim(plotdat(j))
   55 continue
      ! change sign of erosion components (yin) and write variables
      write (luo1,200)  (-1)*yin(1:yplot), xin(1:xplot)
      close (UNIT=luo1)

   end subroutine plotout

   function getline(i_unit) result( line )
      integer, intent(in) :: i_unit
      character(len=mrcl) :: line

      integer dataline, linecount

      save dataline, linecount

1     read (i_unit, '(A)') line
      linecount = linecount + 1
      if (BTEST(debugflg,0)) then
        write (6, *) linecount, ': ', trim(line)
      endif

      if (line(1:1) .ne. '#') goto 2
      goto 1

2     dataline = dataline + 1
      if (BTEST(debugflg,1)) then
        write (6, *) linecount, ':', dataline, ': ', trim(line)
      endif

   end function getline

end module sweep_io_mod
