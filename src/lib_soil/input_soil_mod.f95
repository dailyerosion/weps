!$Author$
!$Date$
!$Revision$
!$HeadURL$

module input_soil_mod

      use soil_data_struct_defs, only: soil_def

      type(soil_def), dimension(:), allocatable :: soil_in ! structure with soil state and parameters input from ifc

  contains

    subroutine input_ifc(isr, soil)
! ***************************************************************** wjr
! reads initial field conditions (IFC) file (Version: 1.0)

!     Edit History
!     Fri Oct  8 16:54:30 CDT 2004 - LEW
!     based upon "inpsub.for" routine
          
!     + + + MODULES + + +
      use soil_data_struct_defs, only: soil_def
      use sci_soil_texture_mod, only : update_sci_soil_multiplier
      use stir_soil_texture_mod, only : update_stir_soil_multiplier
      use file_io_mod, only: fopenk
      use split_layers_mod, only: spllay_ifc

!     + + + ARGUMENTS + + +
      integer, intent(in) :: isr
      type(soil_def), intent(inout) :: soil  ! soil for this subregion

      include 'p1werm.inc'
      include 'command.inc'          !declarations for commandline args

!     + + + LOCAL VARIABLES + + +
      integer       lay
      character     line*512
      integer       lui1

!     + + + LOCAL DEFINITIONS + + +
!     lay - soil layer counter
!     line - hold input line text
!     isr - subregion counter
!     lui1 - input file unit number

      integer linnum
      data linnum /1/

      call fopenk (lui1, soil%sinfil, 'old') ! open IFC file
       
!     Check to see if this is a "versioned" IFC file
      read (lui1,'(a)',err=901) line
      if (line(1:12) .eq. 'Version: 1.0') then
         call inp_ifc_v1(isr, lui1, soil)  ! For version 1.0 IFC file format only
      else if (line(1:12) .eq. 'Version: 1.1') then
         call inp_ifc_v1_1(isr, lui1, soil)  ! For version 1.1 IFC file format only
      else  ! Assuming obsolete unversioned IFC file formats only
         close (lui1)    
         call inpsub(isr, soil)  ! For obsolete IFC file formats only
         return            ! initialization is already done in inpsub
      end if
                   
      close (lui1)    
     
!! removed code reading IFC file data - moved to inp_ifc.for
!! which now handles both version 1.0 and version 1.1 IFC file formats

      ! initialize new variables not read in from ifc file 
      do lay = 1, soil%nslay
          soil%ahfredsat(lay) = 0.0
          soil%asdwsrat(lay) = -1.0
      end do

      ! Set layer thickness of the soils as is appropriate for the simulation
      call spllay_ifc(soil)

      ! Wet Albedo (calculate from dry albedo)
      soil%asfalw = soil%asfald                                 &
     &                / ((1.33**2.)*(1-soil%asfald)+soil%asfald)

      ! Settled Bulk Density, Reference Bulk Density, and Particle Density (texture based calculation)
      call proptext(soil%nslay, soil%asfcla, soil%asfsan, soil%asfom, &
     &              soil%asdblk, soil%asdsblk, soil%asdprocblk, &
     &              soil%asdwblk, soil%asdwsrat, soil%asdpart )

      ! calculate (or recalculate) additional values from soil basic properties
      do lay=1,soil%nslay
!       command line switch, changes to IFC values
        if( wc_type.eq.0 ) then                ! This is OK, this is the way values are now read in
            continue                           ! Ifc inputs are 1/3bar(vol), 15bar(vol), convert both to (grav)

        else if( wc_type.eq.1 ) then           ! Ifc inputs are 1/3bar(vol), 15bar(grav), convert 1/3bar(vol) to (grav)
            ! Print out warning/error message about invalid commandline argument for this soil IFC file version
            write(*,*) 'Warning: -S1 (wc_type=1) is invalid commandline &
     &argument for this soil file, ignoring flag'

        else if( wc_type.eq.2 ) then           ! Ifc inputs are 1/3bar(grav), 15bar(grav), no conversion necessary
            ! Print out warning/error message about invalid commandline argument for this soil IFC file version
            write(*,*) 'Warning: -S2 (wc_type=2) is invalid commandline &
     &argument for this soil file, ignoring flag'

        else if( wc_type.eq.3 ) then           ! Default method.  It resets many input values
            ! Use texture based calculation of 1/3bar(vol), 15bar(vol) and bulk
            ! density and convert to (grav). Using Saxton Method
            call propsaxt(soil%asfsan(lay), soil%asfcla(lay), &
     &                    soil%ahrwcs(lay), &
     &                    soil%ahrwcf(lay), soil%ahrwcw(lay) )
            ! use volumetric saturation to calculate bulk density
            soil%asdwblk(lay) = (1.0-soil%ahrwcs(lay)) * soil%asdpart(lay)
            ! Returned values are 1/3bar(vol), 15bar(vol), convert both to (grav)
            soil%ahrwcf(lay) = soil%ahrwcf(lay) / soil%asdwblk(lay)
            soil%ahrwcw(lay) = soil%ahrwcw(lay) / soil%asdwblk(lay)
        end if
      end do

      do lay=1,soil%nslay
!       set saturation based on definition
!       soil%ahrwcs(lay) = 1.0/soil%asdblk(lay)-1.0/soil%asdpart(lay)   ! Is this based on gravimetric values?

        if(soil%ahrwcs(lay) .lt. soil%ahrwcf(lay)) then
            write(*,*) 'WARNING: Layer, Field Capacity > Saturation',   &  ! NOTE:  Changed to "WARNING" so message
     &                 lay, soil%ahrwcf(lay), soil%ahrwcs(lay)          !wouldn't display in GUI popup Warning dialog box
!           soil%ahrwcf(lay) = soil%ahrwcs(lay)
        endif

!      output for soil file screening
!        write(*,1000) soil%sinfil,lay, soil%aszlyt(lay),
!     &        soil%asfsan(lay), soil%asfcla(lay), soil%asfom(lay),
!     &        soil%asdwblk(lay), soil%asdblk(lay), soil%ahrwcs(lay),
!     &        soil%ahrwcf(lay), soil%ahrwcw(lay),
!     &        soil%ahrwcf(lay)-soil%ahrwcw(lay),
!     &        1.0 - soil%asdwblk(lay)/soil%asdpart(lay),
!     &        soil%ahrwcf(lay)*soil%asdwblk(lay),
!     &        soil%ahrwcw(lay)*soil%asdwblk(lay),
!     &        soil%ahrwcf(lay)*soil%asdwblk(lay)-
!     &        soil%ahrwcw(lay)*soil%asdwblk(lay)

      end do

      if( wc_type.eq.4 ) then
          ! use texture based calculations from Rawls to set all soil
          ! water properties.
          call param_prop_bc(                                           &
     &        soil%nslay, soil%aszlyd, soil%asdblk, soil%asdpart, &
     &        soil%asfcla, soil%asfsan, soil%asfom, soil%asfcec, &
     &        soil%ahrwcs, soil%ahrwcf, soil%ahrwcw, soil%ahrwcr, &
     &        soil%ahrwca, soil%ah0cb, soil%aheaep, soil%ahrsk, &
     &        soil%ahfredsat )


!         do lay=1,soil%nslay
!             ! set soil to field capacity not wilting point
!             soil%ahrwc(lay) = soil%ahrwcf(lay)
!         end do
      else
          ! set matrix potential parameters to match 1/3 bar and 15 bar water contents
          call param_pot_bc( soil%nslay, soil%asdblk, soil%asdpart, &
     &                     soil%ahrwcf, soil%ahrwcw,                &
     &                     soil%asfcla, soil%asfom,                 &
     &                     soil%ah0cb, soil%aheaep )
      end if

      ! Check if override of rock fragments are specified
      if (soil%SoilRockFragments .ge. 0.0) then
        do lay = 1, soil%nslay
          soil%asvroc(lay) = soil%SoilRockFragments
        end do
      end if
      
      !Update the stir soil texture multiplier.  This is called only once after the soil 
      !is read so layer mixing does not affect the texture multiplier.  Only the top layer used.   
      call update_sci_soil_multiplier(isr, soil%asfsan(1), soil%asfcla(1))
      call update_stir_soil_multiplier(isr, soil%asfsan(1), soil%asfcla(1))

      return

 901  write(*,9001) trim(soil%sinfil), linnum, trim(line)
9001  format(' Error in IFC file ',a,' on line #',i4,' ',a)
      call exit(1)

      stop
    end subroutine input_ifc

!      subroutine inp_ifc_v1 (isr, lui1, soil)
!      subroutine inp_ifc_v1_1 (isr, lui1, soil)
    subroutine inp_ifc_v1 (isr, lui1, soil)

      use soil_data_struct_defs, only: soil_def, allocate_soil

      include 'p1werm.inc'
      include 'h1hydro.inc'
      include 'h1db1.inc'

!     + + + Arguments + + +
      integer isr
      integer lui1
      type(soil_def), intent(inout) :: soil  ! soil structure

!     + + + LOCAL VARIABLES + + +
      integer       lay
      character     line*512
      integer       linnum, typeidx
      integer       max_typeidx  !Maximum number of lines to read in
      real          temp

!     + + + Initializations + + +

      linnum = 1
      typeidx = 0
      max_typeidx = 51  ! new ifc format (additional parms)

      ! Read "Version 1.0" IFC soil file contents     
 100  if (typeidx .eq. max_typeidx) go to 200 ! done reading IFC file
      linnum = linnum + 1
      read (lui1,'(a)',err=901) line
      ! print *, 'We are on line #', linnum, 'line = ', line
      if (line(1:1) .eq. '#') go to 100                               ! skip comment lines

!use case statement to appropriately assign values
      typeidx = typeidx + 1
        select case (typeidx)
        case (1)                                                      ! Soil ID string
          soil%am0sid = line(1:160)
        case (2)                                                      ! Local Phase string
          read(line,*,err=902) soil%am0localphase
        case (3)                                                      ! Taxonomy string
          read(line,*,err=902) soil%am0tax

        case (4)                                                      ! NRCS Soil Loss Tolerance (t/ac/yr)
          read(line,*,err=902) soil%SoilLossTol

!     read IP surface physical properties
        case (5)                                                      ! Dry soil albedo (fraction)
          read(line,*,err=902) soil%asfald
        case (6)                                                      ! Slope gradient (m/m)
          ! set default outflow height to zero (minimum depression storage)
          ahzoutflow(isr) = 0.0
          ! check value read in from weps.run
          if( soil%amrslp .lt. -1.5 ) then
             ! weps.run specifies a level basin with no runoff
             soil%amrslp = 0.0
             ! set outflow height of 1/2 meter (minimum depression storage)
             ahzoutflow(isr) = 0.5
          else if( soil%amrslp .lt. 0.0 ) then
             ! no value entered by user (from weps.run)
             read(line,*,err=902) soil%amrslp
             ! check subregion slope value for validity
             if( soil%amrslp .lt. 0.0 ) then
                ! no valid value found in IFC file either, set default value of 1%
                soil%amrslp = 0.01
             end if
          else
             ! value from weps.run being used, throw away soil value
             read(line,*,err=902) temp
          end if
        case (7)                                                      ! Surface frag cover (area fraction)
          read(line,*,err=902) soil%SFCov

        case (8)                                                      ! Depth to bedrock (mm)
          read(line,*,err=902) soil%bedrock_depth
        case (9)                                                      ! Depth to root restricting layer (mm)
          read(line,*,err=902) soil%restrict_depth

!     read IP soil layer number and thickness 
        case (10)                                                      ! Number of soil layers
          read(line,*,err=902) soil%nslay
          ! allocate layer arrays
          call allocate_soil(soil)
        case (11)                                                      ! Soil layer thickness (mm)
          read(line,*,err=902) (soil%aszlyt(lay), lay=1,soil%nslay)

!     read IP soil physical properties
        case (12)                                                     ! Sand fraction (kg/kg)
          read(line,*,err=902) (soil%asfsan(lay), lay=1,soil%nslay)
        case (13)                                                     ! Silt fraction (kg/kg)
          read(line,*,err=902) (soil%asfsil(lay), lay=1,soil%nslay)
        case (14)                                                     ! Clay fraction (kg/kg)
          read(line,*,err=902) (soil%asfcla(lay), lay=1,soil%nslay)
        case (15)                                                     ! Rock fragments fraction (m^3/m^3)
          read(line,*,err=902) (soil%asvroc(lay), lay=1,soil%nslay)
        case (16)                                                     ! Very course sand fraction (kg/kg)
          read(line,*,err=902) (soil%asfvcs(lay), lay=1,soil%nslay)
        case (17)                                                     ! Course sand fraction (kg/kg)
          read(line,*,err=902) (soil%asfcs(lay), lay=1,soil%nslay)
        case (18)                                                     ! Medium sand fraction (kg/kg)
          read(line,*,err=902) (soil%asfms(lay), lay=1,soil%nslay)
        case (19)                                                     ! Fine sand fraction (kg/kg)
          read(line,*,err=902) (soil%asffs(lay), lay=1,soil%nslay)
        case (20)                                                     ! Very fine sand fraction (kg/kg)
          read(line,*,err=902) (soil%asfvfs(lay), lay=1,soil%nslay)
        case (21)                                                     ! Bulk density [wet or 1/3 bar] (Mg/m^3)
          read(line,*,err=902) (soil%asdwblk(lay), lay=1,soil%nslay)

!     read IP soil chemical properties
        case (22)                                                     ! Organic matter (kg/kg)
          read(line,*,err=902) (soil%asfom(lay), lay=1,soil%nslay)
        case (23)                                                     ! PH (0-14)
          read(line,*,err=902) (soil%as0ph(lay), lay=1,soil%nslay)
        case (24)                                                     ! Calcium Carbonate Equiv [CaCO3] (kg/kg)
          read(line,*,err=902) (soil%asfcce(lay), lay=1,soil%nslay)
        case (25)                                                     ! Cation Exchange Capacity [CEC] (meq/100g)
          read(line,*,err=902) (soil%asfcec(lay), lay=1,soil%nslay)
        case (26)                                                     ! Linear extensibility ((Mg/m^3)/(Mg/m^3))
          read(line,*,err=902) (soil%asfcle(lay), lay=1,soil%nslay)

!     read IC aggregate properties
        case (27)                                                     ! ASD GMD (mm)
          read(line,*,err=902) (soil%aslagm(lay), lay=1,soil%nslay)
        case (28)                                                     ! ASD GSD
          read(line,*,err=902) (soil%as0ags(lay), lay=1,soil%nslay)
        case (29)                                                     ! Maximum agg. size (mm)
          read(line,*,err=902) (soil%aslagx(lay), lay=1,soil%nslay)
        case (30)                                                     ! Minimum agg. size (mm)
          read(line,*,err=902) (soil%aslagn(lay), lay=1,soil%nslay)
        case (31)                                                     ! Aggregate density (Mg/m^3)
          read(line,*,err=902) (soil%asdagd(lay), lay=1,soil%nslay)
        case (32)                                                     ! Dry aggregate stability (ln(J/m^2))
          read(line,*,err=902) (soil%aseags(lay), lay=1,soil%nslay)

!     read IC crust properties
        case (33)                                                     ! Crust thickness (mm)
          read(line,*,err=902) soil%aszcr
        case (34)                                                     ! Crust density (Mg/m^3)
          read(line,*,err=902) soil%asdcr
        case (35)                                                     ! Crust stability (ln(J/m^2))
          read(line,*,err=902) soil%asecr
        case (36)                                                     ! Crust surface frction (m^2/m^2)
          read(line,*,err=902) soil%asfcr
        case (37)                                                     ! Mass of loose material on crust (kg/m^2)
          read(line,*,err=902) soil%asmlos
        case (38)                                                     ! Fraction of loose material on crust (m^2/m^2)
          read(line,*,err=902) soil%asflos

!     read IC surface roughness properties
        case (39)                                                     ! Random roughness (mm)
          read(line,*,err=902) soil%aslrr
          soil%aslrro = soil%aslrr                            ! init after-tillage RR
        case (40)                                                     ! Ridge orientation (deg)
          read(line,*,err=902) soil%asargo
        case (41)                                                     ! Ridge height (mm)
          read(line,*,err=902) soil%aszrgh
        case (42)                                                     ! Ridge spacing (mm)
          read(line,*,err=902) soil%asxrgs
        case (43)                                                     ! Ridge width (mm)
          read(line,*,err=902) soil%asxrgw

        ! this is where dike height and spacing should be read in.
        ! they are not, but need to be initialized.
        ! case (??)
          soil%asxdks = 0.0
          soil%asxdkh = 0.0

!     read IC soil hydrologic properties
        ! All SWC values are converted to mass basis as they are the "independent variables" in WEPS
        case (44)                                                     ! Initial BD value (Mg/m^3)
          read(line,*,err=902) (soil%asdblk(lay), lay=1,soil%nslay)
          do lay=1,soil%nslay
            soil%asdblk0(lay) = soil%asdblk(lay)    ! init previous day BD
          end do
        case (45)                                                     ! Initial SWC (m^3/m^3)
          read(line,*,err=902) (soil%ahrwc(lay), lay=1,soil%nslay)
          do lay=1,soil%nslay                                         ! Convert to mass basis (kg/kg)
            soil%ahrwc(lay) = soil%ahrwc(lay) / soil%asdblk(lay)         ! (using "initial" bd value)
          end do

!     read soil hydrologic (water release curve) properties
      ! All can be overridden if "Saxton" method is specified (wc_type == 3)
        case (46)                                                     ! Saturated SWC (m^3/m^3)
          read(line,*,err=902) (soil%ahrwcs(lay), lay=1,soil%nslay)
          do lay=1,soil%nslay                                         ! Convert to mass basis (kg/kg)
            soil%ahrwcs(lay) = soil%ahrwcs(lay) / soil%asdblk(lay)       ! (using "initial" bd value)
          end do
        case (47)                                                     ! Field Capacity SWC (m^3/m^3)
          read(line,*,err=902) (soil%ahrwcf(lay), lay=1,soil%nslay)
          do lay=1,soil%nslay                                         ! Convert to mass basis (kg/kg)
            soil%ahrwcf(lay) = soil%ahrwcf(lay) / soil%asdblk(lay)       ! (using "initial" bd value)
          end do
        case (48)                                                     ! Wilting Point SWC (m^3/m^3)
          read(line,*,err=902) (soil%ahrwcw(lay), lay=1,soil%nslay)
          do lay=1,soil%nslay                                         ! Convert to mass basis (kg/kg)
            soil%ahrwcw(lay) = soil%ahrwcw(lay) / soil%asdblk(lay)       ! (using "initial" bd value)
          end do

!     read more soil hydrologic (water release curve) properties
      ! All three can be reset if "Walter Rawls" method is specified (wc_type == 4)
      ! CB and Air Entry Pot. values possibly reset if "Walter Rawls" method not specified
        case (49)                                                     ! Soil CB value
          read(line,*,err=902) (soil%ah0cb(lay), lay=1,soil%nslay)
        case (50)                                                     ! Air Entry Potential (J/kg)
          read(line,*,err=902) (soil%aheaep(lay), lay=1,soil%nslay)
        case (51)                                                     ! Saturated Hydraulic Conductivity (m/s)
          read(line,*,err=902) (soil%ahrsk(lay), lay=1,soil%nslay)

        end select
        goto 100

      ! reading of subregion IFC elements complete
  200 continue 

      return

 901  write(*,9001) trim(soil%sinfil), linnum, trim(line)
9001  format(' Error in v1 IFC file ',a,' on line #',i4,' ',a)
      call exit(1)


 902  write(*,9002) trim(soil%sinfil), linnum, typeidx, trim(line)
9002  format(' Error in v1 IFC file ',a,' on line #',i4,'(',i2,') ',a)
      call exit(1)

      stop

    end subroutine inp_ifc_v1

!-----------------------------------------------------------------------
    subroutine inp_ifc_v1_1 (isr, lui1, soil)

      ! input routine for Version 1.1 IFC file format

      ! Includes NASIS/SSURGO version number and date fields
      ! and missing soil surface initialization values
      ! dike height and spacing values

      use soil_data_struct_defs, only: soil_def, allocate_soil

      include 'p1werm.inc'
      include 'h1hydro.inc'
      include 'h1db1.inc'

!     + + + Arguments + + +
      integer isr
      integer lui1
      type(soil_def), intent(inout) :: soil  ! subregion surface conditions

!     + + + LOCAL VARIABLES + + +
      integer       lay
      character     line*512
      integer       linnum, typeidx
      integer       max_typeidx  !Maximum number of lines to read in
      real          temp

!     + + + Initializations + + +

      linnum = 1
      typeidx = 0
      max_typeidx = 55  ! new ifc format (additional parms)

      write(0,*) 'Reading Version 1.1 soil IFC file format!!!'
      ! Read "Version 1.1" IFC soil file contents
 100  if (typeidx .eq. max_typeidx) go to 200 ! done reading IFC file
      linnum = linnum + 1
      read (lui1,'(a)',err=901) line
      ! print *, 'We are on line #', linnum, 'line = ', line
      if (line(1:1) .eq. '#') go to 100                               ! skip comment lines

!use case statement to appropriately assign values
      typeidx = typeidx + 1
        select case (typeidx)
        case (1)                                                      ! NRCS SSURGO version - saversion
          !SSURGO_version(isr) = line
        case (2)                                                      ! RNCS SSURGO date - sadate?
          !SSURGO_date(isr) = line

        case (3)                                                      ! Soil ID string
          soil%am0sid = line(1:160)
        case (4)                                                      ! Local Phase string
          read(line,*,err=902) soil%am0localphase
        case (5)                                                      ! Taxonomy string
          read(line,*,err=902) soil%am0tax

        case (6)                                                      ! NRCS Soil Loss Tolerance (t/ac/yr)
          read(line,*,err=902) soil%SoilLossTol

!     read IP surface physical properties
        case (7)                                                      ! Dry soil albedo (fraction)
          read(line,*,err=902) soil%asfald
        case (8)                                                      ! Slope gradient (m/m)
          ! set default outflow height to zero (minimum depression storage)
          ahzoutflow(isr) = 0.0
          ! check value read in from weps.run
          if( soil%amrslp .lt. -1.5 ) then
             ! weps.run specifies a level basin with no runoff
             soil%amrslp = 0.0
             ! set outflow height of 1/2 meter (minimum depression storage)
             ahzoutflow(isr) = 0.5
          else if( soil%amrslp .lt. 0.0 ) then
             ! no value entered by user (from weps.run)
             read(line,*,err=902) soil%amrslp
             ! check subregion slope value for validity
             if( soil%amrslp .lt. 0.0 ) then
                ! no valid value found in IFC file either, set default value of 1%
                soil%amrslp = 0.01
             end if
          else
             ! value from weps.run being used, throw away soil value
             read(line,*,err=902) temp
          end if
        case (9)                                                      ! Surface frag cover (area fraction)
          read(line,*,err=902) soil%SFCov

        case (10)                                                      ! Depth to bedrock (mm)
          read(line,*,err=902) soil%bedrock_depth
        case (11)                                                      ! Depth to root restricting layer (mm)
          read(line,*,err=902) soil%restrict_depth

!     read IP soil layer number and thickness 
        case (12)                                                      ! Number of soil layers
          read(line,*,err=902) soil%nslay
          ! allocate soil arrays
          call allocate_soil(soil)
        case (13)                                                      ! Soil layer thickness (mm)
          read(line,*,err=902) (soil%aszlyt(lay), lay=1,soil%nslay)

!     read IP soil physical properties
        case (14)                                                     ! Sand fraction (kg/kg)
          read(line,*,err=902) (soil%asfsan(lay), lay=1,soil%nslay)
        case (15)                                                     ! Silt fraction (kg/kg)
          read(line,*,err=902) (soil%asfsil(lay), lay=1,soil%nslay)
        case (16)                                                     ! Clay fraction (kg/kg)
          read(line,*,err=902) (soil%asfcla(lay), lay=1,soil%nslay)
        case (17)                                                     ! Rock fragments fraction (m^3/m^3)
          read(line,*,err=902) (soil%asvroc(lay), lay=1,soil%nslay)
        case (18)                                                     ! Very course sand fraction (kg/kg)
          read(line,*,err=902) (soil%asfvcs(lay), lay=1,soil%nslay)
        case (19)                                                     ! Course sand fraction (kg/kg)
          read(line,*,err=902) (soil%asfcs(lay), lay=1,soil%nslay)
        case (20)                                                     ! Medium sand fraction (kg/kg)
          read(line,*,err=902) (soil%asfms(lay), lay=1,soil%nslay)
        case (21)                                                     ! Fine sand fraction (kg/kg)
          read(line,*,err=902) (soil%asffs(lay), lay=1,soil%nslay)
        case (22)                                                     ! Very fine sand fraction (kg/kg)
          read(line,*,err=902) (soil%asfvfs(lay), lay=1,soil%nslay)
        case (23)                                                     ! Bulk density [wet or 1/3 bar] (Mg/m^3)
          read(line,*,err=902) (soil%asdwblk(lay), lay=1,soil%nslay)

!     read IP soil chemical properties
        case (24)                                                     ! Organic matter (kg/kg)
          read(line,*,err=902) (soil%asfom(lay), lay=1,soil%nslay)
        case (25)                                                     ! PH (0-14)
          read(line,*,err=902) (soil%as0ph(lay), lay=1,soil%nslay)
        case (26)                                                     ! Calcium Carbonate Equiv [CaCO3] (kg/kg)
          read(line,*,err=902) (soil%asfcce(lay), lay=1,soil%nslay)
        case (27)                                                     ! Cation Exchange Capacity [CEC] (meq/100g)
          read(line,*,err=902) (soil%asfcec(lay), lay=1,soil%nslay)
        case (28)                                                     ! Linear extensibility ((Mg/m^3)/(Mg/m^3))
          read(line,*,err=902) (soil%asfcle(lay), lay=1,soil%nslay)

!     read IC aggregate properties
        case (29)                                                     ! ASD GMD (mm)
          read(line,*,err=902) (soil%aslagm(lay), lay=1,soil%nslay)
        case (30)                                                     ! ASD GSD
          read(line,*,err=902) (soil%as0ags(lay), lay=1,soil%nslay)
        case (31)                                                     ! Maximum agg. size (mm)
          read(line,*,err=902) (soil%aslagx(lay), lay=1,soil%nslay)
        case (32)                                                     ! Minimum agg. size (mm)
          read(line,*,err=902) (soil%aslagn(lay), lay=1,soil%nslay)
        case (33)                                                     ! Aggregate density (Mg/m^3)
          read(line,*,err=902) (soil%asdagd(lay), lay=1,soil%nslay)
        case (34)                                                     ! Dry aggregate stability (ln(J/m^2))
          read(line,*,err=902) (soil%aseags(lay), lay=1,soil%nslay)

!     read IC crust properties
        case (35)                                                     ! Crust thickness (mm)
          read(line,*,err=902) soil%aszcr
        case (36)                                                     ! Crust density (Mg/m^3)
          read(line,*,err=902) soil%asdcr
        case (37)                                                     ! Crust stability (ln(J/m^2))
          read(line,*,err=902) soil%asecr
        case (38)                                                     ! Crust surface frction (m^2/m^2)
          read(line,*,err=902) soil%asfcr
        case (39)                                                     ! Mass of loose material on crust (kg/m^2)
          read(line,*,err=902) soil%asmlos
        case (40)                                                     ! Fraction of loose material on crust (m^2/m^2)
          read(line,*,err=902) soil%asflos

!     read IC surface roughness properties
        case (41)                                                     ! Random roughness (mm)
          read(line,*,err=902) soil%aslrr
          soil%aslrro = soil%aslrr                            ! init after-tillage RR
        case (42)                                                     ! Ridge orientation (deg)
          read(line,*,err=902) soil%asargo
        case (43)                                                     ! Ridge height (mm)
          read(line,*,err=902) soil%aszrgh
        case (44)                                                     ! Ridge spacing (mm)
          read(line,*,err=902) soil%asxrgs
        case (45)                                                     ! Ridge width (mm)
          read(line,*,err=902) soil%asxrgw

        ! this is where dike height and spacing are now read in.
        case (46)                                                     ! Dike spacing (mm)
          read(line,*,err=902) soil%asxdks
        case (47)                                                     ! Dike height (mm)
          read(line,*,err=902) soil%asxdkh

!     read IC soil hydrologic properties
        ! All SWC values are converted to mass basis as they are the "independent variables" in WEPS
        case (48)                                                     ! Initial BD value (Mg/m^3)
          read(line,*,err=902) (soil%asdblk(lay), lay=1,soil%nslay)
          do lay=1,soil%nslay
            soil%asdblk0(lay) = soil%asdblk(lay)    ! init previous day BD
          end do
        case (49)                                                     ! Initial SWC (m^3/m^3)
          read(line,*,err=902) (soil%ahrwc(lay), lay=1,soil%nslay)
          do lay=1,soil%nslay                                         ! Convert to mass basis (kg/kg)
            soil%ahrwc(lay) = soil%ahrwc(lay) / soil%asdblk(lay)         ! (using "initial" bd value)
          end do

!     read soil hydrologic (water release curve) properties
      ! All can be overridden if "Saxton" method is specified (wc_type == 3)
        case (50)                                                     ! Saturated SWC (m^3/m^3)
          read(line,*,err=902) (soil%ahrwcs(lay), lay=1,soil%nslay)
          do lay=1,soil%nslay                                         ! Convert to mass basis (kg/kg)
            soil%ahrwcs(lay) = soil%ahrwcs(lay) / soil%asdblk(lay)       ! (using "initial" bd value)
          end do
        case (51)                                                     ! Field Capacity SWC (m^3/m^3)
          read(line,*,err=902) (soil%ahrwcf(lay), lay=1,soil%nslay)
          do lay=1,soil%nslay                                         ! Convert to mass basis (kg/kg)
            soil%ahrwcf(lay) = soil%ahrwcf(lay) / soil%asdblk(lay)       ! (using "initial" bd value)
          end do
        case (52)                                                     ! Wilting Point SWC (m^3/m^3)
          read(line,*,err=902) (soil%ahrwcw(lay), lay=1,soil%nslay)
          do lay=1,soil%nslay                                         ! Convert to mass basis (kg/kg)
            soil%ahrwcw(lay) = soil%ahrwcw(lay) / soil%asdblk(lay)       ! (using "initial" bd value)
          end do

!     read more soil hydrologic (water release curve) properties
      ! All three can be reset if "Walter Rawls" method is specified (wc_type == 4)
      ! CB and Air Entry Pot. values possibly reset if "Walter Rawls" method not specified
        case (53)                                                     ! Soil CB value
          read(line,*,err=902) (soil%ah0cb(lay), lay=1,soil%nslay)
        case (54)                                                     ! Air Entry Potential (J/kg)
          read(line,*,err=902) (soil%aheaep(lay), lay=1,soil%nslay)
        case (55)                                                     ! Saturated Hydraulic Conductivity (m/s)
          read(line,*,err=902) (soil%ahrsk(lay), lay=1,soil%nslay)

        end select
        goto 100

      ! reading of subregion IFC elements complete
  200 continue 

      return

 901  write(*,9001) trim(soil%sinfil), linnum, trim(line)
9001  format(' Error in v1.1 IFC file ',a,' on line #',i4,' ',a)
      call exit(1)


 902  write(*,9002) trim(soil%sinfil), linnum, typeidx, trim(line)
9002  format(' Error in v1.1 IFC file ',a,' on line #',i4,'(',i2,') ',a)
      call exit(1)

      stop

    end subroutine inp_ifc_v1_1

!      subroutine inpsub (isr, soil_in, soil)
    subroutine inpsub (isr, soil)
! ***************************************************************** wjr
! reads initial field conditions (IFC) file for all subregions
!
!  NOTE:  Obsolete routine.  Likely to be removed at some later date.
!         This routine is only used for "unlabeled" soil IFC files
!         (pre-version 1.0) - LEW
!
!     Edit History
!     06-Feb-99   wjr   created

      use file_io_mod, only: fopenk
      use split_layers_mod, only: spllay
      use soil_data_struct_defs, only: soil_def, allocate_soil

      include 'p1werm.inc'
      include 'h1hydro.inc'
      include 'h1db1.inc'
      include 'command.inc'          !declarations for commandline args

!     + + + Arguments + + +
      integer, intent(in) :: isr
      type(soil_def), intent(inout) :: soil  ! subregion surface conditions

!     + + + LOCAL VARIABLES + + +
      integer       lay
      character     line*256
      integer       linnum, typidx
      integer       max_typidx  !Maximum number of lines to read in
      real          temp   ! temporary variable, throw away value
      integer       lui1   ! input file unit number

!     + + + Initializations + + +
      linnum = 0
      typidx = 0

!     Old ifc file format contains 52 lines
!     New ifc file format contains 54 lines
!     To reduce the amount of code being changed,
!     "max_typidx" is set equal to 54 for both cases.
!     If (ifc_format .eq. 1) then the two additional lines
!     are skipped for "old" ifc formatted files.
!     Mon Sep 24 14:48:13 CDT 2001 - LEW

      max_typidx = 54   ! new ifc format (additional parms)

!     open simulation run file
      call fopenk (lui1, soil%sinfil, 'old')

!     read subregion information
  100   if (typidx.eq. max_typidx) go to 190  !do subregion initializations
        linnum = linnum + 1
        read (lui1,'(a)',err=81) line
!
! skip comment lines
        if (line(1:1) .eq. '#') go to 100
!
!!use case statement to appropriately assign values
        typidx = typidx + 1
        if (report_debug >= 3) write (6,*) linnum, typidx, ":", line
        select case (typidx)
        case (1)
!     read initial field conditions file
          soil%am0sid = line(1:160)
        case (2)
          read(line,*,err=82) soil%am0tax
        case (3)
          read(line,*,err=82) soil%nslay
          ! allocate soil arrays
          call allocate_soil(soil)
        case (4)
          read(line,*,err=82) (soil%aszlyt(lay), lay=1,soil%nslay)
!     read soil physical properties
        case (5)
          read(line,*,err=82) (soil%asfsan(lay), lay=1,soil%nslay)
        case (6)
          read(line,*,err=82) (soil%asfsil(lay), lay=1,soil%nslay)
        case (7)
          read(line,*,err=82) (soil%asfcla(lay), lay=1,soil%nslay)
        case (8)
          read(line,*,err=82) (soil%asvroc(lay), lay=1,soil%nslay)
        case (9)
          read(line,*,err=82) (soil%asfcs(lay), lay=1,soil%nslay)
        case (10)
          read(line,*,err=82) (soil%asfms(lay), lay=1,soil%nslay)
        case (11)
          read(line,*,err=82) (soil%asffs(lay), lay=1,soil%nslay)
        case (12)
          read(line,*,err=82) (soil%asfvfs(lay), lay=1,soil%nslay)
        case (13)
          read(line,*,err=82) (soil%asfwdc(lay), lay=1,soil%nslay)
        case (14)
          read(line,*,err=82) (soil%asdblk(lay), lay=1,soil%nslay)
          ! initialize coefficient of linear expansion, even though it isn't used with these IFC files
          do lay=1,soil%nslay
             soil%asfcle(lay) = 0.0 
          end do
        case (15)
          read(line,*,err=82) (soil%asdwblk(lay), lay=1,soil%nslay)
        case (16)
!     aggregate properties
          read(line,*,err=82) (soil%aslagm(lay), lay=1,soil%nslay)
        case (17)
          read(line,*,err=82) (soil%as0ags(lay), lay=1,soil%nslay)
        case (18)
          read(line,*,err=82) (soil%aslagx(lay), lay=1,soil%nslay)
        case (19)
          read(line,*,err=82) (soil%aslagn(lay), lay=1,soil%nslay)
        case (20)
          read(line,*,err=82) (soil%asdagd(lay), lay=1,soil%nslay)
        case (21)
          read(line,*,err=82) (soil%aseags(lay), lay=1,soil%nslay)
        case (22)
!     read crust properties
          read(line,*,err=82) soil%aszcr
        case (23)
          read(line,*,err=82) soil%asdcr
        case (24)
          read(line,*,err=82) soil%asecr
        case (25)
          read(line,*,err=82) soil%asfcr
        case (26)
!     read surface properties
          read(line,*,err=82) soil%asmlos
        case (27)
          read(line,*,err=82) soil%asflos
        case (28)
          read(line,*,err=82) soil%aslrr
          soil%aslrro = soil%aslrr
        case (29)
          read(line,*,err=82) soil%asargo
        case (30)
          read(line,*,err=82) soil%aszrgh
        case (31)
          read(line,*,err=82) soil%asxrgs
        case (32)
          read(line,*,err=82) soil%asxrgw

        ! this is where dike height and spacing should be read in.
        ! they are not, but need to be initialized.
        ! case (??)
          soil%asxdks = 0.0
          soil%asxdkh = 0.0

        case (33)
!     read soil hydrologic properties
          read(line,*,err=82) (soil%ahrwc(lay), lay=1,soil%nslay)
        case (34)
          read(line,*,err=82) (soil%ahrwcs(lay), lay=1,soil%nslay)
        case (35)
          read(line,*,err=82) (soil%ahrwcf(lay), lay=1,soil%nslay)
        case (36)
          read(line,*,err=82) (soil%ahrwcw(lay), lay=1,soil%nslay)
        case (37)
          read(line,*,err=82) (soil%ahrwc1(lay), lay=1,soil%nslay)
        case (38)
          read(line,*,err=82) (soil%ah0cb(lay), lay=1,soil%nslay)
        case (39)
          read(line,*,err=82) (soil%aheaep(lay), lay=1,soil%nslay)
        case (40)
          read(line,*,err=82) (soil%ahrsk(lay), lay=1,soil%nslay)
        case (41)
          read(line,*,err=82) temp  ! no nonger used, ah0cnp(isr)
        case (42)
          read(line,*,err=82) temp  ! no nonger used, ah0cng(isr)
        case (43)
          read(line,*,err=82) soil%asfald

!         Code added to "skip" extra parameter not available in "old" ifc files
          ! set default outflow height to zero (minimum depression storage)
          ahzoutflow(isr) = 0.0
          if (ifc_format .eq. 1) then !old ifc format (skip next typidx line)
             typidx = typidx + 1
             if( soil%amrslp .lt. -1.5 ) then
                ! weps.run specifies a level basin with no runoff
                soil%amrslp = 0.0
                ! set outflow height of 1/2 meter (minimum depression storage)
                ahzoutflow(isr) = 0.5
             else if( soil%amrslp .lt. 0.0 ) then
                ! no value entered by user (from weps.run)
                ! no valid value found in IFC file either, set default value of 1%
                soil%amrslp = 0.01
             end if
          endif

        case (44)    !This line only gets read if new ifc format is specified
          ! check value read in from weps.run
          if( soil%amrslp .lt. -1.5 ) then
             ! weps.run specifies a level basin with no runoff
             soil%amrslp = 0.0
             ! set outflow height of 1/2 meter (minimum depression storage)
             ahzoutflow(isr) = 0.5
          else if( soil%amrslp .lt. 0.0 ) then
             ! no value entered by user (from weps.run)
             read(line,*,err=82) soil%amrslp
             ! check subregion slope value for validity
             if( soil%amrslp .lt. 0.0 ) then
                ! no valid value found in IFC file either, set default value of 1%
                soil%amrslp = 0.01
             end if
          else
             ! value from weps.run being used, throw away soil value
             read(line,*,err=82) temp
          end if

        case (45)
!         read soil chemical properties
          read(line,*,err=82) (soil%asfom(lay), lay=1,soil%nslay)

        case (46)
          read(line,*,err=82) (soil%as0ph(lay), lay=1,soil%nslay)
        case (47)
          read(line,*,err=82) (soil%asfcce(lay), lay=1,soil%nslay)
!       read other soil chemical properties needed by the CROP
        case (48)
          read(line,*,err=82) (soil%asfcec(lay), lay=1,soil%nslay)
        case (49)
          read(line,*,err=82) (temp, lay=1,soil%nslay) ! asfsmb no longer used
        case (50)
          read(line,*,err=82) (temp, lay=1,soil%nslay) ! as0ec no longer used
        case (51)
          read(line,*,err=82) (temp, lay=1,soil%nslay) ! asrsar no longer used
        case (52)
          read(line,*,err=82) (temp, lay=1,soil%nslay) ! asftan no longer used
        case (53)
          read(line,*,err=82) (temp, lay=1,soil%nslay) ! asftap no longer used

!         Code added to "skip" extra parameter not available in "old" ifc files
          if (ifc_format .eq. 1) then !old ifc format (skip next typidx line)
             typidx = typidx + 1
          endif

        case (54)    !This line only gets read if new ifc format is specified
          read(line,*,err=82) (soil%asfcle(lay), lay=1,soil%nslay)

        end select
        goto 100

        ! reading of subregion IFC elements complete
  190 continue 

      ! initialize new variables not in either of these  "old" ifc file formats 
      soil%bedrock_depth = 99990.0
      soil%restrict_depth = 99990.0
      do lay = 1, soil%nslay
        soil%asfvcs(lay) = 0.0
        soil%ahfredsat(lay) = 0.0
        soil%asdwsrat(lay) = -1.0
      end do


      ! set layer thickness of the soils as is appropriate for the simulation
      call spllay(soil)

      ! calculate wet albedo from dry
      soil%asfalw = soil%asfald                                 &
     &                / ((1.33**2.)*(1-soil%asfald)+soil%asfald)

      ! texture based calculation of settled bulk density and particle density
      call proptext(soil%nslay,soil%asfcla,soil%asfsan,soil%asfom,&
     &              soil%asdsblk, soil%asdsblk, soil%asdprocblk,  &
     &              soil%asdwblk, soil%asdwsrat, soil%asdpart )

      ! calculate (or recalculate) additional values from soil basic properties
      do lay=1,soil%nslay

!       command line switch, changes to IFC values
        if( wc_type.eq.0 ) then
!           Ifc inputs are 1/3bar(vol), 15bar(vol), convert both to (grav)
            soil%ahrwcf(lay) = soil%ahrwcf(lay) / soil%asdwblk(lay)
            soil%ahrwcw(lay) = soil%ahrwcw(lay) / soil%asdwblk(lay)
        else if( wc_type.eq.1 ) then
!           Ifc inputs are 1/3bar(vol), 15bar(grav), convert 1/3bar(vol) to (grav)
            soil%ahrwcf(lay) = soil%ahrwcf(lay) / soil%asdwblk(lay)
        else if( wc_type.eq.2 ) then
!           Ifc inputs are 1/3bar(grav), 15bar(grav), no conversion necessary
        else if( wc_type.eq.3 ) then
!!          Use texture based calculation of 1/3bar(vol), 15bar(vol) and bulk
!           density and convert to (grav). Using Saxton Method
            call propsaxt(soil%asfsan(lay), soil%asfcla(lay), soil%ahrwcs(lay), &
     &                    soil%ahrwcf(lay), soil%ahrwcw(lay) )
!!          use volumetric saturation to calculate bulk density
            soil%asdwblk(lay) = (1.0-soil%ahrwcs(lay)) * soil%asdpart(lay)
!           Returned values are 1/3bar(vol), 15bar(vol), convert both to (grav)
            soil%ahrwcf(lay) = soil%ahrwcf(lay) / soil%asdwblk(lay)
            soil%ahrwcw(lay) = soil%ahrwcw(lay) / soil%asdwblk(lay)
        end if

!       set soil to field capacity not wilting point
        soil%ahrwc(lay) = soil%ahrwcf(lay)

!       make sure settled bd is greater than or equal to wet bulk density
        if( soil%asdsblk(lay) .lt. soil%asdwblk(lay) ) then
            soil%asdsblk(lay) = soil%asdwblk(lay)
        endif

!       set initial condition to wet bulk density, not dry
        soil%asdblk(lay) = soil%asdwblk(lay)
!       set previous day bulk density
        soil%asdblk0(lay) = soil%asdblk(lay)

!       set saturation based on definition
        soil%ahrwcs(lay) = 1.0/soil%asdblk(lay)-1.0/soil%asdpart(lay)
        if(soil%ahrwcs(lay) .lt. soil%ahrwcf(lay)) then
!            soil%ahrwcf(lay) = soil%ahrwcs(lay)
            write(*,*) 'Layer, Field Capacity > Saturation',            &
     &                 lay, soil%ahrwcf(lay), soil%ahrwcs(lay)
        endif

!      output for soil file screening
!        write(*,1000) soil%sinfil,lay,soil%aszlyt(lay),
!     &        soil%asfsan(lay),soil%asfcla(lay),soil%asfom(lay),
!     &        soil%asdwblk(lay),soil%asdblk(lay),soil%ahrwcs(lay),
!     &        soil%ahrwcf(lay),soil%ahrwcw(lay),
!     &        soil%ahrwcf(lay)-soil%ahrwcw(lay),
!     &        1.0 - soil%asdwblk(lay)/soil%asdpart(lay),
!     &        soil%ahrwcf(lay)*soil%asdwblk(lay),
!     &        soil%ahrwcw(lay)*soil%asdwblk(lay),
!     &        soil%ahrwcf(lay)*soil%asdwblk(lay)-
!     &        soil%ahrwcw(lay)*soil%asdwblk(lay)

      end do

      if( wc_type.eq.4 ) then
          ! use texture based calculations from Rawls to set all soil
          ! water properties.
          call param_prop_bc(                                           &
     &        soil%nslay, soil%aszlyd, soil%asdblk, soil%asdpart, &
     &        soil%asfcla, soil%asfsan, soil%asfom, soil%asfcec, &
     &        soil%ahrwcs, soil%ahrwcf, soil%ahrwcw,soil%ahrwcr, &
     &        soil%ahrwca, soil%ah0cb, soil%aheaep, soil%ahrsk, &
     &        soil%ahfredsat )

          do lay=1,soil%nslay
              ! set soil to field capacity not wilting point
              soil%ahrwc(lay) = soil%ahrwcf(lay)
          end do
      else
          ! set matrix potential parameters to match 1/3 bar and 15 bar water contents
          call param_pot_bc( soil%nslay, soil%asdblk, soil%asdpart, &
     &                     soil%ahrwcf, soil%ahrwcw,                &
     &                     soil%asfcla, soil%asfom,                 &
     &                     soil%ah0cb, soil%aheaep )
      end if

      close (lui1)

      return
   81 write(*,9001) trim(soil%sinfil), linnum, trim(line)
9001  format (' inpsub error - original format IFC file ',a,            &
     &' on line #',i4,' ',a)
      call exit(1)

   82 write(*,9002) trim(soil%sinfil), linnum, typidx, trim(line)
9002  format (' inpsub error - original format IFC file ',a,            &
     &' on line #',i4,'(',i2,')',' ',a)
      call exit(1)

      stop

    end subroutine inpsub

end module input_soil_mod
