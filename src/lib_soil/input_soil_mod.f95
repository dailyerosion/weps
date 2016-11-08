!$Author$
!$Date$
!$Revision$
!$HeadURL$

module input_soil_mod

  contains

!      subroutine input_ifc(isr, soil_in, soil)
    subroutine input_ifc(isr, subrsurf)
! ***************************************************************** wjr
! reads initial field conditions (IFC) file (Version: 1.0)

!     Edit History
!     Fri Oct  8 16:54:30 CDT 2004 - LEW
!     based upon "inpsub.for" routine
          
!     + + + MODULES + + +
      use sci_soil_texture_mod, only : update_sci_soil_multiplier
      use stir_soil_texture_mod, only : update_stir_soil_multiplier
      use file_io_mod, only: fopenk
      use split_layers_mod, only: spllay_ifc
      use erosion_data_struct_defs, only: subregionsurfacestate

!     + + + ARGUMENTS + + +
      integer, intent(in) :: isr
      type(subregionsurfacestate), intent(inout) :: subrsurf  ! subregion surface conditions

      include 'p1werm.inc'
      include 'wpath.inc'
      include 'm1subr.inc'
      include 'm1sim.inc'
      include 'm1flag.inc'
      include 's1layr.inc'
      include 's1phys.inc'
      include 's1agg.inc'
      include 's1dbh.inc'
      include 's1dbc.inc'
      include 'h1hydro.inc'
      include 'h1scs.inc'
      include 'h1db1.inc'
      include 'command.inc'          !declarations for commandline args

!     + + + LOCAL COMMON BLOCKS + + +
      include 'main/main.inc'

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

      ! temporary initialization until proper structures put this in better place
      ! these are accumulated in update_period_update_vars but are not set until
      ! erosion is called at least once (sbinit, sbpm10)
      subrsurf%acanag = 0
      subrsurf%acancr = 0

      call fopenk (lui1, sinfil(isr), 'old') ! open IFC file
       
!     Check to see if this is a "versioned" IFC file
      read (lui1,'(a)',err=901) line
      if (line(1:12) .eq. 'Version: 1.0') then
         call inp_ifc_v1(isr, lui1, subrsurf)  ! For version 1.0 IFC file format only
      else if (line(1:12) .eq. 'Version: 1.1') then
         call inp_ifc_v1_1(isr, lui1, subrsurf)  ! For version 1.1 IFC file format only
      else  ! Assuming obsolete unversioned IFC file formats only
         close (lui1)    
         call inpsub(isr, subrsurf)  ! For obsolete IFC file formats only
         return            ! initialization is already done in inpsub
      end if
                   
      close (lui1)    
     
!! removed code reading IFC file data - moved to inp_ifc.for
!! which now handles both version 1.0 and version 1.1 IFC file formats

      ! initialize new variables not read in from ifc file 
      do lay = 1, nslay(isr)
          ahfredsat(lay,isr) = 0.0
          asdwsrat(lay, isr) = -1.0
      end do

      ! Set layer thickness of the soils as is appropriate for the simulation
      call spllay_ifc(isr, subrsurf)

      ! Wet Albedo (calculate from dry albedo)
      subrsurf%asfalw = subrsurf%asfald                                 &
     &                / ((1.33**2.)*(1-subrsurf%asfald)+subrsurf%asfald)

      ! Settled Bulk Density, Reference Bulk Density, and Particle Density (texture based calculation)
      call proptext(nslay(isr),asfcla(1,isr),asfsan(1,isr),asfom(1,isr),&
     &              asdblk(1,isr), asdsblk(1,isr), asdprocblk(1,isr),   &
     &              asdwblk(1,isr), asdwsrat(1, isr), asdpart(1,isr) )

      ! calculate (or recalculate) additional values from soil basic properties
      do lay=1,nslay(isr)
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
            call propsaxt(asfsan(lay,isr), asfcla(lay,isr),             &
     &                    ahrwcs(lay,isr),                              &
     &                    ahrwcf(lay,isr), ahrwcw(lay,isr) )
            ! use volumetric saturation to calculate bulk density
            asdwblk(lay,isr) = (1.0-ahrwcs(lay,isr)) * asdpart(lay,isr)
            ! Returned values are 1/3bar(vol), 15bar(vol), convert both to (grav)
            ahrwcf(lay,isr) = ahrwcf(lay,isr) / asdwblk(lay,isr)
            ahrwcw(lay,isr) = ahrwcw(lay,isr) / asdwblk(lay,isr)
        end if
      end do

      do lay=1,nslay(isr)
!       set saturation based on definition
!       ahrwcs(lay,isr) = 1.0/asdblk(lay,isr)-1.0/asdpart(lay,isr)   ! Is this based on gravimetric values?

        if(ahrwcs(lay,isr).lt.ahrwcf(lay,isr)) then
            write(*,*) 'WARNING: Layer, Field Capacity > Saturation',   &  ! NOTE:  Changed to "WARNING" so message
     &                 lay, ahrwcf(lay,isr), ahrwcs(lay,isr)          !wouldn't display in GUI popup Warning dialog box
!           ahrwcf(lay,isr) = ahrwcs(lay,isr)
        endif

!      output for soil file screening
!        write(*,1000) sinfil,lay,aszlyt(lay,isr),
!     &        asfsan(lay,isr),asfcla(lay,isr),asfom(lay,isr),
!     &        asdwblk(lay,isr),asdblk(lay,isr),ahrwcs(lay,isr),
!     &        ahrwcf(lay,isr),ahrwcw(lay,isr),
!     &        ahrwcf(lay,isr)-ahrwcw(lay,isr),
!     &        1.0 - asdwblk(lay,isr)/asdpart(lay,isr),
!     &        ahrwcf(lay,isr)*asdwblk(lay,isr),
!     &        ahrwcw(lay,isr)*asdwblk(lay,isr),
!     &        ahrwcf(lay,isr)*asdwblk(lay,isr)-
!     &        ahrwcw(lay,isr)*asdwblk(lay,isr)

      end do

      if( wc_type.eq.4 ) then
          ! use texture based calculations from Rawls to set all soil
          ! water properties.
          call param_prop_bc(                                           &
     &        nslay(isr), aszlyd(1,isr), asdblk(1,isr), asdpart(1,isr), &
     &        asfcla(1,isr), asfsan(1,isr), asfom(1,isr), asfcec(1,isr),&
     &        ahrwcs(1,isr), ahrwcf(1,isr), ahrwcw(1,isr),ahrwcr(1,isr),&
     &        ahrwca(1,isr), ah0cb(1,isr), aheaep(1,isr), ahrsk(1,isr), &
     &        ahfredsat(1,isr) )


!         do lay=1,nslay(isr)
!             ! set soil to field capacity not wilting point
!             ahrwc(lay,isr) = ahrwcf(lay,isr)
!         end do
      else
          ! set matrix potential parameters to match 1/3 bar and 15 bar water contents
          call param_pot_bc( nslay(isr), asdblk(1,isr), asdpart(1,isr), &
     &                     ahrwcf(1,isr), ahrwcw(1,isr),                &
     &                     asfcla(1,isr), asfom(1,isr),                 &
     &                     ah0cb(1,isr), aheaep(1,isr) )
      end if

!       some soil characteristic values for crop nutirent effects 
!       were originally planned and then dropped and are not included in 
!       layer splitting. A Debug full debug compile complains
!       that these values are not initialized when they are mixed as
!       part of  management process. they are initialized here to avoid
!       removing them from mix and invert
        do lay = 1, nslay(isr)
            ! I've removed them from the mix and invert functions.  However, they might still be
            ! used in (hopefully) dead crop code.
            ascmg(lay,isr) = 0.0
            ascna(lay,isr) = 0.0
            asfesp(lay,isr) = 0.0
            asfnoh(lay,isr) = 0.0
            asfpoh(lay,isr) = 0.0
            asfpsp(lay,isr) = 0.0
            ! obsolete variables removed from "versioned" ifc files
              ! Can't not initialize them here because crop apparently is still using them somewhere
            asfsmb(lay,isr) = 0.0
            asrsar(lay,isr) = 0.0
            asftan(lay,isr) = 0.0
            asftap(lay,isr) = 0.0
  
        end do

      ! Check if override of rock fragments are specified
      if (SoilRockFragments(isr) .ge. 0.0) then
        do lay = 1, nslay(isr)
          asvroc(lay,isr) = SoilRockFragments(isr)
        end do
      end if
      
      !Update the stir soil texture multiplier.  This is called only once after the soil 
      !is read so layer mixing does not affect the texture multiplier.  Only the top layer used.   
      call update_sci_soil_multiplier(isr,asfsan(1,isr),asfcla(1,isr))
      call update_stir_soil_multiplier(isr,asfsan(1,isr),asfcla(1,isr))

      return

 901  write(*,9001) trim(sinfil(isr)), linnum, trim(line)
9001  format(' Error in IFC file ',a,' on line #',i4,' ',a)
      call exit(1)

      stop
    end subroutine input_ifc

!      subroutine inp_ifc_v1 (isr, lui1, soil)
!      subroutine inp_ifc_v1_1 (isr, lui1, soil)
    subroutine inp_ifc_v1 (isr, lui1, subrsurf)

      use erosion_data_struct_defs, only: subregionsurfacestate

      include 'p1werm.inc'
      include 'm1subr.inc'
      include 's1layr.inc'
      include 's1phys.inc'
      include 's1agg.inc'
      include 's1dbh.inc'
      include 's1dbc.inc'
      include 'h1hydro.inc'
      include 'h1scs.inc'
      include 'h1db1.inc'

!     + + + Arguments + + +
      integer isr
      integer lui1
      type(subregionsurfacestate), intent(inout) :: subrsurf  ! subregion surface conditions

!     + + + LOCAL COMMON BLOCKS + + +
      include 'main/main.inc'

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
          am0sid(isr) = line(1:160)
        case (2)                                                      ! Local Phase string
          read(line,*,err=902) am0localphase(isr)
        case (3)                                                      ! Taxonomy string
          read(line,*,err=902) am0tax(isr)

        case (4)                                                      ! NRCS Soil Loss Tolerance (t/ac/yr)
          read(line,*,err=902) SoilLossTol(isr)

!     read IP surface physical properties
        case (5)                                                      ! Dry soil albedo (fraction)
          read(line,*,err=902) subrsurf%asfald
        case (6)                                                      ! Slope gradient (m/m)
          ! set default outflow height to zero (minimum depression storage)
          ahzoutflow(isr) = 0.0
          ! check value read in from weps.run
          if( amrslp(isr) .lt. -1.5 ) then
             ! weps.run specifies a level basin with no runoff
             amrslp(isr) = 0.0
             ! set outflow height of 1/2 meter (minimum depression storage)
             ahzoutflow(isr) = 0.5
          else if( amrslp(isr) .lt. 0.0 ) then
             ! no value entered by user (from weps.run)
             read(line,*,err=902) amrslp(isr)
             ! check subregion slope value for validity
             if( amrslp(isr) .lt. 0.0 ) then
                ! no valid value found in IFC file either, set default value of 1%
                amrslp(isr) = 0.01
             end if
          else
             ! value from weps.run being used, throw away soil value
             read(line,*,err=902) temp
          end if
        case (7)                                                      ! Surface frag cover (area fraction)
          read(line,*,err=902) SFCov(isr)

        case (8)                                                      ! Depth to bedrock (mm)
          read(line,*,err=902) bedrock_depth(isr)
        case (9)                                                      ! Depth to root restricting layer (mm)
          read(line,*,err=902) restrict_depth(isr)

!     read IP soil layer number and thickness 
        case (10)                                                      ! Number of soil layers
          read(line,*,err=902) nslay(isr)
        case (11)                                                      ! Soil layer thickness (mm)
          read(line,*,err=902) (aszlyt(lay,isr), lay=1,nslay(isr))

!     read IP soil physical properties
        case (12)                                                     ! Sand fraction (kg/kg)
          read(line,*,err=902) (asfsan(lay,isr), lay=1,nslay(isr))
        case (13)                                                     ! Silt fraction (kg/kg)
          read(line,*,err=902) (asfsil(lay,isr), lay=1,nslay(isr))
        case (14)                                                     ! Clay fraction (kg/kg)
          read(line,*,err=902) (asfcla(lay,isr), lay=1,nslay(isr))
        case (15)                                                     ! Rock fragments fraction (m^3/m^3)
          read(line,*,err=902) (asvroc(lay,isr), lay=1,nslay(isr))
        case (16)                                                     ! Very course sand fraction (kg/kg)
          read(line,*,err=902) (asfvcs(lay,isr), lay=1,nslay(isr))
        case (17)                                                     ! Course sand fraction (kg/kg)
          read(line,*,err=902) (asfcs(lay,isr), lay=1,nslay(isr))
        case (18)                                                     ! Medium sand fraction (kg/kg)
          read(line,*,err=902) (asfms(lay,isr), lay=1,nslay(isr))
        case (19)                                                     ! Fine sand fraction (kg/kg)
          read(line,*,err=902) (asffs(lay,isr), lay=1,nslay(isr))
        case (20)                                                     ! Very fine sand fraction (kg/kg)
          read(line,*,err=902) (asfvfs(lay,isr), lay=1,nslay(isr))
        case (21)                                                     ! Bulk density [wet or 1/3 bar] (Mg/m^3)
          read(line,*,err=902) (asdwblk(lay,isr), lay=1,nslay(isr))

!     read IP soil chemical properties
        case (22)                                                     ! Organic matter (kg/kg)
          read(line,*,err=902) (asfom(lay,isr), lay=1,nslay(isr))
        case (23)                                                     ! PH (0-14)
          read(line,*,err=902) (as0ph(lay,isr), lay=1,nslay(isr))
        case (24)                                                     ! Calcium Carbonate Equiv [CaCO3] (kg/kg)
          read(line,*,err=902) (asfcce(lay,isr), lay=1,nslay(isr))
        case (25)                                                     ! Cation Exchange Capacity [CEC] (meq/100g)
          read(line,*,err=902) (asfcec(lay,isr), lay=1,nslay(isr))
        case (26)                                                     ! Linear extensibility ((Mg/m^3)/(Mg/m^3))
          read(line,*,err=902) (asfcle(lay,isr), lay=1,nslay(isr))

!     read IC aggregate properties
        case (27)                                                     ! ASD GMD (mm)
          read(line,*,err=902) (aslagm(lay,isr), lay=1,nslay(isr))
        case (28)                                                     ! ASD GSD
          read(line,*,err=902) (as0ags(lay,isr), lay=1,nslay(isr))
        case (29)                                                     ! Maximum agg. size (mm)
          read(line,*,err=902) (aslagx(lay,isr), lay=1,nslay(isr))
        case (30)                                                     ! Minimum agg. size (mm)
          read(line,*,err=902) (aslagn(lay,isr), lay=1,nslay(isr))
        case (31)                                                     ! Aggregate density (Mg/m^3)
          read(line,*,err=902) (asdagd(lay,isr), lay=1,nslay(isr))
        case (32)                                                     ! Dry aggregate stability (ln(J/m^2))
          read(line,*,err=902) (aseags(lay,isr), lay=1,nslay(isr))

!     read IC crust properties
        case (33)                                                     ! Crust thickness (mm)
          read(line,*,err=902) subrsurf%aszcr
        case (34)                                                     ! Crust density (Mg/m^3)
          read(line,*,err=902) subrsurf%asdcr
        case (35)                                                     ! Crust stability (ln(J/m^2))
          read(line,*,err=902) subrsurf%asecr
        case (36)                                                     ! Crust surface frction (m^2/m^2)
          read(line,*,err=902) subrsurf%asfcr
        case (37)                                                     ! Mass of loose material on crust (kg/m^2)
          read(line,*,err=902) subrsurf%asmlos
        case (38)                                                     ! Fraction of loose material on crust (m^2/m^2)
          read(line,*,err=902) subrsurf%asflos

!     read IC surface roughness properties
        case (39)                                                     ! Random roughness (mm)
          read(line,*,err=902) subrsurf%aslrr
          subrsurf%aslrro = subrsurf%aslrr                            ! init after-tillage RR
        case (40)                                                     ! Ridge orientation (deg)
          read(line,*,err=902) subrsurf%asargo
        case (41)                                                     ! Ridge height (mm)
          read(line,*,err=902) subrsurf%aszrgh
        case (42)                                                     ! Ridge spacing (mm)
          read(line,*,err=902) subrsurf%asxrgs
        case (43)                                                     ! Ridge width (mm)
          read(line,*,err=902) subrsurf%asxrgw

        ! this is where dike height and spacing should be read in.
        ! they are not, but need to be initialized.
        ! case (??)
          subrsurf%asxdks = 0.0
          subrsurf%asxdkh = 0.0

!     read IC soil hydrologic properties
        ! All SWC values are converted to mass basis as they are the "independent variables" in WEPS
        case (44)                                                     ! Initial BD value (Mg/m^3)
          read(line,*,err=902) (asdblk(lay,isr), lay=1,nslay(isr))
          do lay=1,nslay(isr)
            asdblk0(lay,isr) = asdblk(lay,isr)    ! init previous day BD
          end do
        case (45)                                                     ! Initial SWC (m^3/m^3)
          read(line,*,err=902) (ahrwc(lay,isr), lay=1,nslay(isr))
          do lay=1,nslay(isr)                                         ! Convert to mass basis (kg/kg)
            ahrwc(lay,isr) = ahrwc(lay,isr) / asdblk(lay,isr)         ! (using "initial" bd value)
          end do

!     read soil hydrologic (water release curve) properties
      ! All can be overridden if "Saxton" method is specified (wc_type == 3)
        case (46)                                                     ! Saturated SWC (m^3/m^3)
          read(line,*,err=902) (ahrwcs(lay,isr), lay=1,nslay(isr))
          do lay=1,nslay(isr)                                         ! Convert to mass basis (kg/kg)
            ahrwcs(lay,isr) = ahrwcs(lay,isr) / asdblk(lay,isr)       ! (using "initial" bd value)
          end do
        case (47)                                                     ! Field Capacity SWC (m^3/m^3)
          read(line,*,err=902) (ahrwcf(lay,isr), lay=1,nslay(isr))
          do lay=1,nslay(isr)                                         ! Convert to mass basis (kg/kg)
            ahrwcf(lay,isr) = ahrwcf(lay,isr) / asdblk(lay,isr)       ! (using "initial" bd value)
          end do
        case (48)                                                     ! Wilting Point SWC (m^3/m^3)
          read(line,*,err=902) (ahrwcw(lay,isr), lay=1,nslay(isr))
          do lay=1,nslay(isr)                                         ! Convert to mass basis (kg/kg)
            ahrwcw(lay,isr) = ahrwcw(lay,isr) / asdblk(lay,isr)       ! (using "initial" bd value)
          end do

!     read more soil hydrologic (water release curve) properties
      ! All three can be reset if "Walter Rawls" method is specified (wc_type == 4)
      ! CB and Air Entry Pot. values possibly reset if "Walter Rawls" method not specified
        case (49)                                                     ! Soil CB value
          read(line,*,err=902) (ah0cb(lay,isr), lay=1,nslay(isr))
        case (50)                                                     ! Air Entry Potential (J/kg)
          read(line,*,err=902) (aheaep(lay,isr), lay=1,nslay(isr))
        case (51)                                                     ! Saturated Hydraulic Conductivity (m/s)
          read(line,*,err=902) (ahrsk(lay,isr), lay=1,nslay(isr))

        end select
        goto 100

      ! reading of subregion IFC elements complete
  200 continue 

      return

 901  write(*,9001) trim(sinfil(isr)), linnum, trim(line)
9001  format(' Error in v1 IFC file ',a,' on line #',i4,' ',a)
      call exit(1)


 902  write(*,9002) trim(sinfil(isr)), linnum, typeidx, trim(line)
9002  format(' Error in v1 IFC file ',a,' on line #',i4,'(',i2,') ',a)
      call exit(1)

      stop

    end subroutine inp_ifc_v1

!-----------------------------------------------------------------------
    subroutine inp_ifc_v1_1 (isr, lui1, subrsurf)

      ! input routine for Version 1.1 IFC file format

      ! Includes NASIS/SSURGO version number and date fields
      ! and missing soil surface initialization values
      ! dike height and spacing values

      use erosion_data_struct_defs, only: subregionsurfacestate

      include 'p1werm.inc'
      include 'm1subr.inc'
      include 's1layr.inc'
      include 's1phys.inc'
      include 's1agg.inc'
      include 's1dbh.inc'
      include 's1dbc.inc'
      include 'h1hydro.inc'
      include 'h1scs.inc'
      include 'h1db1.inc'

!     + + + LOCAL COMMON BLOCKS + + +
      include 'main/main.inc'

!     + + + Arguments + + +
      integer isr
      integer lui1
      type(subregionsurfacestate), intent(inout) :: subrsurf  ! subregion surface conditions

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
          am0sid(isr) = line(1:160)
        case (4)                                                      ! Local Phase string
          read(line,*,err=902) am0localphase(isr)
        case (5)                                                      ! Taxonomy string
          read(line,*,err=902) am0tax(isr)

        case (6)                                                      ! NRCS Soil Loss Tolerance (t/ac/yr)
          read(line,*,err=902) SoilLossTol(isr)

!     read IP surface physical properties
        case (7)                                                      ! Dry soil albedo (fraction)
          read(line,*,err=902) subrsurf%asfald
        case (8)                                                      ! Slope gradient (m/m)
          ! set default outflow height to zero (minimum depression storage)
          ahzoutflow(isr) = 0.0
          ! check value read in from weps.run
          if( amrslp(isr) .lt. -1.5 ) then
             ! weps.run specifies a level basin with no runoff
             amrslp(isr) = 0.0
             ! set outflow height of 1/2 meter (minimum depression storage)
             ahzoutflow(isr) = 0.5
          else if( amrslp(isr) .lt. 0.0 ) then
             ! no value entered by user (from weps.run)
             read(line,*,err=902) amrslp(isr)
             ! check subregion slope value for validity
             if( amrslp(isr) .lt. 0.0 ) then
                ! no valid value found in IFC file either, set default value of 1%
                amrslp(isr) = 0.01
             end if
          else
             ! value from weps.run being used, throw away soil value
             read(line,*,err=902) temp
          end if
        case (9)                                                      ! Surface frag cover (area fraction)
          read(line,*,err=902) SFCov(isr)

        case (10)                                                      ! Depth to bedrock (mm)
          read(line,*,err=902) bedrock_depth(isr)
        case (11)                                                      ! Depth to root restricting layer (mm)
          read(line,*,err=902) restrict_depth(isr)

!     read IP soil layer number and thickness 
        case (12)                                                      ! Number of soil layers
          read(line,*,err=902) nslay(isr)
        case (13)                                                      ! Soil layer thickness (mm)
          read(line,*,err=902) (aszlyt(lay,isr), lay=1,nslay(isr))

!     read IP soil physical properties
        case (14)                                                     ! Sand fraction (kg/kg)
          read(line,*,err=902) (asfsan(lay,isr), lay=1,nslay(isr))
        case (15)                                                     ! Silt fraction (kg/kg)
          read(line,*,err=902) (asfsil(lay,isr), lay=1,nslay(isr))
        case (16)                                                     ! Clay fraction (kg/kg)
          read(line,*,err=902) (asfcla(lay,isr), lay=1,nslay(isr))
        case (17)                                                     ! Rock fragments fraction (m^3/m^3)
          read(line,*,err=902) (asvroc(lay,isr), lay=1,nslay(isr))
        case (18)                                                     ! Very course sand fraction (kg/kg)
          read(line,*,err=902) (asfvcs(lay,isr), lay=1,nslay(isr))
        case (19)                                                     ! Course sand fraction (kg/kg)
          read(line,*,err=902) (asfcs(lay,isr), lay=1,nslay(isr))
        case (20)                                                     ! Medium sand fraction (kg/kg)
          read(line,*,err=902) (asfms(lay,isr), lay=1,nslay(isr))
        case (21)                                                     ! Fine sand fraction (kg/kg)
          read(line,*,err=902) (asffs(lay,isr), lay=1,nslay(isr))
        case (22)                                                     ! Very fine sand fraction (kg/kg)
          read(line,*,err=902) (asfvfs(lay,isr), lay=1,nslay(isr))
        case (23)                                                     ! Bulk density [wet or 1/3 bar] (Mg/m^3)
          read(line,*,err=902) (asdwblk(lay,isr), lay=1,nslay(isr))

!     read IP soil chemical properties
        case (24)                                                     ! Organic matter (kg/kg)
          read(line,*,err=902) (asfom(lay,isr), lay=1,nslay(isr))
        case (25)                                                     ! PH (0-14)
          read(line,*,err=902) (as0ph(lay,isr), lay=1,nslay(isr))
        case (26)                                                     ! Calcium Carbonate Equiv [CaCO3] (kg/kg)
          read(line,*,err=902) (asfcce(lay,isr), lay=1,nslay(isr))
        case (27)                                                     ! Cation Exchange Capacity [CEC] (meq/100g)
          read(line,*,err=902) (asfcec(lay,isr), lay=1,nslay(isr))
        case (28)                                                     ! Linear extensibility ((Mg/m^3)/(Mg/m^3))
          read(line,*,err=902) (asfcle(lay,isr), lay=1,nslay(isr))

!     read IC aggregate properties
        case (29)                                                     ! ASD GMD (mm)
          read(line,*,err=902) (aslagm(lay,isr), lay=1,nslay(isr))
        case (30)                                                     ! ASD GSD
          read(line,*,err=902) (as0ags(lay,isr), lay=1,nslay(isr))
        case (31)                                                     ! Maximum agg. size (mm)
          read(line,*,err=902) (aslagx(lay,isr), lay=1,nslay(isr))
        case (32)                                                     ! Minimum agg. size (mm)
          read(line,*,err=902) (aslagn(lay,isr), lay=1,nslay(isr))
        case (33)                                                     ! Aggregate density (Mg/m^3)
          read(line,*,err=902) (asdagd(lay,isr), lay=1,nslay(isr))
        case (34)                                                     ! Dry aggregate stability (ln(J/m^2))
          read(line,*,err=902) (aseags(lay,isr), lay=1,nslay(isr))

!     read IC crust properties
        case (35)                                                     ! Crust thickness (mm)
          read(line,*,err=902) subrsurf%aszcr
        case (36)                                                     ! Crust density (Mg/m^3)
          read(line,*,err=902) subrsurf%asdcr
        case (37)                                                     ! Crust stability (ln(J/m^2))
          read(line,*,err=902) subrsurf%asecr
        case (38)                                                     ! Crust surface frction (m^2/m^2)
          read(line,*,err=902) subrsurf%asfcr
        case (39)                                                     ! Mass of loose material on crust (kg/m^2)
          read(line,*,err=902) subrsurf%asmlos
        case (40)                                                     ! Fraction of loose material on crust (m^2/m^2)
          read(line,*,err=902) subrsurf%asflos

!     read IC surface roughness properties
        case (41)                                                     ! Random roughness (mm)
          read(line,*,err=902) subrsurf%aslrr
          subrsurf%aslrro = subrsurf%aslrr                            ! init after-tillage RR
        case (42)                                                     ! Ridge orientation (deg)
          read(line,*,err=902) subrsurf%asargo
        case (43)                                                     ! Ridge height (mm)
          read(line,*,err=902) subrsurf%aszrgh
        case (44)                                                     ! Ridge spacing (mm)
          read(line,*,err=902) subrsurf%asxrgs
        case (45)                                                     ! Ridge width (mm)
          read(line,*,err=902) subrsurf%asxrgw

        ! this is where dike height and spacing are now read in.
        case (46)                                                     ! Dike spacing (mm)
          read(line,*,err=902) subrsurf%asxdks
        case (47)                                                     ! Dike height (mm)
          read(line,*,err=902) subrsurf%asxdkh

!     read IC soil hydrologic properties
        ! All SWC values are converted to mass basis as they are the "independent variables" in WEPS
        case (48)                                                     ! Initial BD value (Mg/m^3)
          read(line,*,err=902) (asdblk(lay,isr), lay=1,nslay(isr))
          do lay=1,nslay(isr)
            asdblk0(lay,isr) = asdblk(lay,isr)    ! init previous day BD
          end do
        case (49)                                                     ! Initial SWC (m^3/m^3)
          read(line,*,err=902) (ahrwc(lay,isr), lay=1,nslay(isr))
          do lay=1,nslay(isr)                                         ! Convert to mass basis (kg/kg)
            ahrwc(lay,isr) = ahrwc(lay,isr) / asdblk(lay,isr)         ! (using "initial" bd value)
          end do

!     read soil hydrologic (water release curve) properties
      ! All can be overridden if "Saxton" method is specified (wc_type == 3)
        case (50)                                                     ! Saturated SWC (m^3/m^3)
          read(line,*,err=902) (ahrwcs(lay,isr), lay=1,nslay(isr))
          do lay=1,nslay(isr)                                         ! Convert to mass basis (kg/kg)
            ahrwcs(lay,isr) = ahrwcs(lay,isr) / asdblk(lay,isr)       ! (using "initial" bd value)
          end do
        case (51)                                                     ! Field Capacity SWC (m^3/m^3)
          read(line,*,err=902) (ahrwcf(lay,isr), lay=1,nslay(isr))
          do lay=1,nslay(isr)                                         ! Convert to mass basis (kg/kg)
            ahrwcf(lay,isr) = ahrwcf(lay,isr) / asdblk(lay,isr)       ! (using "initial" bd value)
          end do
        case (52)                                                     ! Wilting Point SWC (m^3/m^3)
          read(line,*,err=902) (ahrwcw(lay,isr), lay=1,nslay(isr))
          do lay=1,nslay(isr)                                         ! Convert to mass basis (kg/kg)
            ahrwcw(lay,isr) = ahrwcw(lay,isr) / asdblk(lay,isr)       ! (using "initial" bd value)
          end do

!     read more soil hydrologic (water release curve) properties
      ! All three can be reset if "Walter Rawls" method is specified (wc_type == 4)
      ! CB and Air Entry Pot. values possibly reset if "Walter Rawls" method not specified
        case (53)                                                     ! Soil CB value
          read(line,*,err=902) (ah0cb(lay,isr), lay=1,nslay(isr))
        case (54)                                                     ! Air Entry Potential (J/kg)
          read(line,*,err=902) (aheaep(lay,isr), lay=1,nslay(isr))
        case (55)                                                     ! Saturated Hydraulic Conductivity (m/s)
          read(line,*,err=902) (ahrsk(lay,isr), lay=1,nslay(isr))

        end select
        goto 100

      ! reading of subregion IFC elements complete
  200 continue 

      return

 901  write(*,9001) trim(sinfil(isr)), linnum, trim(line)
9001  format(' Error in v1.1 IFC file ',a,' on line #',i4,' ',a)
      call exit(1)


 902  write(*,9002) trim(sinfil(isr)), linnum, typeidx, trim(line)
9002  format(' Error in v1.1 IFC file ',a,' on line #',i4,'(',i2,') ',a)
      call exit(1)

      stop

    end subroutine inp_ifc_v1_1

!      subroutine inpsub (isr, soil_in, soil)
    subroutine inpsub (isr, subrsurf)
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
      use erosion_data_struct_defs, only: subregionsurfacestate

      include 'p1werm.inc'
      include 'wpath.inc'
      include 'm1subr.inc'
      include 'm1sim.inc'
      include 's1layr.inc'
      include 's1phys.inc'
      include 's1agg.inc'
      include 's1dbh.inc'
      include 's1dbc.inc'
      include 'h1hydro.inc'
      include 'h1scs.inc'
      include 'h1db1.inc'
      include 'command.inc'          !declarations for commandline args

!     + + + Arguments + + +
      integer, intent(in) :: isr
      type(subregionsurfacestate), intent(inout) :: subrsurf  ! subregion surface conditions

!     + + + LOCAL COMMON BLOCKS + + +
      include 'main/main.inc'
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
      call fopenk (lui1, sinfil(isr), 'old')

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
          am0sid(isr) = line(1:160)
        case (2)
          read(line,*,err=82) am0tax(isr)
        case (3)
          read(line,*,err=82) nslay(isr)
        case (4)
          read(line,*,err=82) (aszlyt(lay,isr), lay=1,nslay(isr))
!     read soil physical properties
        case (5)
          read(line,*,err=82) (asfsan(lay,isr), lay=1,nslay(isr))
        case (6)
          read(line,*,err=82) (asfsil(lay,isr), lay=1,nslay(isr))
        case (7)
          read(line,*,err=82) (asfcla(lay,isr), lay=1,nslay(isr))
        case (8)
          read(line,*,err=82) (asvroc(lay,isr), lay=1,nslay(isr))
        case (9)
          read(line,*,err=82) (asfcs(lay,isr), lay=1,nslay(isr))
        case (10)
          read(line,*,err=82) (asfms(lay,isr), lay=1,nslay(isr))
        case (11)
          read(line,*,err=82) (asffs(lay,isr), lay=1,nslay(isr))
        case (12)
          read(line,*,err=82) (asfvfs(lay,isr), lay=1,nslay(isr))
        case (13)
          read(line,*,err=82) (asfwdc(lay,isr), lay=1,nslay(isr))
        case (14)
          read(line,*,err=82) (asdblk(lay,isr), lay=1,nslay(isr))
          do lay=1,nslay(isr)                    ! initialize "dry" bd, even though it isn't used with these IFC files
             asfcle(lay,isr) = 0.0 
          end do
! *** debugging write
!           write(*,*) ' inpsub: ', asdblk(1:7,isr)
! *** eodw
        case (15)
          read(line,*,err=82) (asdwblk(lay,isr), lay=1,nslay(isr))
        case (16)
!     aggregate properties
          read(line,*,err=82) (aslagm(lay,isr), lay=1,nslay(isr))
        case (17)
          read(line,*,err=82) (as0ags(lay,isr), lay=1,nslay(isr))
        case (18)
          read(line,*,err=82) (aslagx(lay,isr), lay=1,nslay(isr))
        case (19)
          read(line,*,err=82) (aslagn(lay,isr), lay=1,nslay(isr))
        case (20)
          read(line,*,err=82) (asdagd(lay,isr), lay=1,nslay(isr))
        case (21)
          read(line,*,err=82) (aseags(lay,isr), lay=1,nslay(isr))
        case (22)
!     read crust properties
          read(line,*,err=82) subrsurf%aszcr
        case (23)
          read(line,*,err=82) subrsurf%asdcr
        case (24)
          read(line,*,err=82) subrsurf%asecr
        case (25)
          read(line,*,err=82) subrsurf%asfcr
        case (26)
!     read surface properties
          read(line,*,err=82) subrsurf%asmlos
!      write(*,*) ' inpsub: asmlos(isr) ', asmlos(isr)
        case (27)
          read(line,*,err=82) subrsurf%asflos
        case (28)
          read(line,*,err=82) subrsurf%aslrr
          subrsurf%aslrro = subrsurf%aslrr
        case (29)
          read(line,*,err=82) subrsurf%asargo
        case (30)
          read(line,*,err=82) subrsurf%aszrgh
        case (31)
          read(line,*,err=82) subrsurf%asxrgs
        case (32)
          read(line,*,err=82) subrsurf%asxrgw

        ! this is where dike height and spacing should be read in.
        ! they are not, but need to be initialized.
        ! case (??)
          subrsurf%asxdks = 0.0
          subrsurf%asxdkh = 0.0

        case (33)
!     read soil hydrologic properties
          read(line,*,err=82) (ahrwc(lay,isr), lay=1,nslay(isr))
        case (34)
          read(line,*,err=82) (ahrwcs(lay,isr), lay=1,nslay(isr))
        case (35)
          read(line,*,err=82) (ahrwcf(lay,isr), lay=1,nslay(isr))
        case (36)
          read(line,*,err=82) (ahrwcw(lay,isr), lay=1,nslay(isr))
        case (37)
          read(line,*,err=82) (ahrwc1(lay,isr), lay=1,nslay(isr))
        case (38)
          read(line,*,err=82) (ah0cb(lay,isr), lay=1,nslay(isr))
        case (39)
          read(line,*,err=82) (aheaep(lay,isr), lay=1,nslay(isr))
        case (40)
          read(line,*,err=82) (ahrsk(lay,isr), lay=1,nslay(isr))
        case (41)
          read(line,*,err=82) ah0cnp(isr)
        case (42)
          read(line,*,err=82) ah0cng(isr)
        case (43)
          read(line,*,err=82) subrsurf%asfald

!         Code added to "skip" extra parameter not available in "old" ifc files
          ! set default outflow height to zero (minimum depression storage)
          ahzoutflow(isr) = 0.0
          if (ifc_format .eq. 1) then !old ifc format (skip next typidx line)
             typidx = typidx + 1
             if( amrslp(isr) .lt. -1.5 ) then
                ! weps.run specifies a level basin with no runoff
                amrslp(isr) = 0.0
                ! set outflow height of 1/2 meter (minimum depression storage)
                ahzoutflow(isr) = 0.5
             else if( amrslp(isr) .lt. 0.0 ) then
                ! no value entered by user (from weps.run)
                ! no valid value found in IFC file either, set default value of 1%
                amrslp(isr) = 0.01
             end if
          endif

        case (44)    !This line only gets read if new ifc format is specified
          ! check value read in from weps.run
          if( amrslp(isr) .lt. -1.5 ) then
             ! weps.run specifies a level basin with no runoff
             amrslp(isr) = 0.0
             ! set outflow height of 1/2 meter (minimum depression storage)
             ahzoutflow(isr) = 0.5
          else if( amrslp(isr) .lt. 0.0 ) then
             ! no value entered by user (from weps.run)
             read(line,*,err=82) amrslp(isr)
             ! check subregion slope value for validity
             if( amrslp(isr) .lt. 0.0 ) then
                ! no valid value found in IFC file either, set default value of 1%
                amrslp(isr) = 0.01
             end if
          else
             ! value from weps.run being used, throw away soil value
             read(line,*,err=82) temp
          end if

        case (45)
!         read soil chemical properties
          read(line,*,err=82) (asfom(lay,isr), lay=1,nslay(isr))

        case (46)
          read(line,*,err=82) (as0ph(lay,isr), lay=1,nslay(isr))
        case (47)
          read(line,*,err=82) (asfcce(lay,isr), lay=1,nslay(isr))
!       read other soil chemical properties needed by the CROP
        case (48)
          read(line,*,err=82) (asfcec(lay,isr), lay=1,nslay(isr))
        case (49)
          read(line,*,err=82) (asfsmb(lay,isr), lay=1,nslay(isr))
        case (50)
          read(line,*,err=82) (as0ec(lay,isr), lay=1,nslay(isr))
        case (51)
          read(line,*,err=82) (asrsar(lay,isr), lay=1,nslay(isr))
        case (52)
          read(line,*,err=82) (asftan(lay,isr), lay=1,nslay(isr))
        case (53)
          read(line,*,err=82) (asftap(lay,isr), lay=1,nslay(isr))

!         Code added to "skip" extra parameter not available in "old" ifc files
          if (ifc_format .eq. 1) then !old ifc format (skip next typidx line)
             typidx = typidx + 1
          endif

        case (54)    !This line only gets read if new ifc format is specified
          read(line,*,err=82) (asfcle(lay,isr), lay=1,nslay(isr))

        end select
        goto 100

      ! reading of subregion IFC elements complete
  190 continue 

      !initialize new variables not in either of these  "old" ifc file formats 
         bedrock_depth = 99990.0
         restrict_depth = 99990.0
        do lay = 1, nslay(isr)
            asfvcs(lay,isr) = 0.0
            ahfredsat(lay,isr) = 0.0
            asdwsrat(lay, isr) = -1.0
        end do


      ! set layer thickness of the soils as is appropriate for the simulation
      call spllay(isr, subrsurf)

      ! calculate wet albedo from dry
      subrsurf%asfalw = subrsurf%asfald                                 &
     &                / ((1.33**2.)*(1-subrsurf%asfald)+subrsurf%asfald)

      ! texture based calculation of settled bulk density and particle density
      call proptext(nslay(isr),asfcla(1,isr),asfsan(1,isr),asfom(1,isr),&
     &              asdsblk(1,isr), asdsblk(1,isr), asdprocblk(1,isr),  &
     &              asdwblk(1,isr), asdwsrat(1, isr), asdpart(1,isr) )

      ! calculate (or recalculate) additional values from soil basic properties
      do lay=1,nslay(isr)

!       command line switch, changes to IFC values
        if( wc_type.eq.0 ) then
!           Ifc inputs are 1/3bar(vol), 15bar(vol), convert both to (grav)
            ahrwcf(lay,isr) = ahrwcf(lay,isr) / asdwblk(lay,isr)
            ahrwcw(lay,isr) = ahrwcw(lay,isr) / asdwblk(lay,isr)
        else if( wc_type.eq.1 ) then
!           Ifc inputs are 1/3bar(vol), 15bar(grav), convert 1/3bar(vol) to (grav)
            ahrwcf(lay,isr) = ahrwcf(lay,isr) / asdwblk(lay,isr)
        else if( wc_type.eq.2 ) then
!           Ifc inputs are 1/3bar(grav), 15bar(grav), no conversion necessary
        else if( wc_type.eq.3 ) then
!!          Use texture based calculation of 1/3bar(vol), 15bar(vol) and bulk
!           density and convert to (grav). Using Saxton Method
            call propsaxt(asfsan(lay,isr), asfcla(lay,isr),             &
     &                    ahrwcs(lay,isr),                              &
     &                    ahrwcf(lay,isr), ahrwcw(lay,isr) )
!!          use volumetric saturation to calculate bulk density
            asdwblk(lay,isr) = (1.0-ahrwcs(lay,isr)) * asdpart(lay,isr)
!           Returned values are 1/3bar(vol), 15bar(vol), convert both to (grav)
            ahrwcf(lay,isr) = ahrwcf(lay,isr) / asdwblk(lay,isr)
            ahrwcw(lay,isr) = ahrwcw(lay,isr) / asdwblk(lay,isr)
        end if

!       set soil to field capacity not wilting point
        ahrwc(lay,isr) = ahrwcf(lay,isr)

!       make sure settled bd is greater than or equal to wet bulk density
        if( asdsblk(lay,isr).lt.asdwblk(lay,isr) ) then
            asdsblk(lay,isr) = asdwblk(lay,isr)
        endif

!       set initial condition to wet bulk density, not dry
        asdblk(lay,isr) = asdwblk(lay,isr)
!       set previous day bulk density
        asdblk0(lay,isr) = asdblk(lay,isr)

!       set saturation based on definition
        ahrwcs(lay,isr) = 1.0/asdblk(lay,isr)-1.0/asdpart(lay,isr)
        if(ahrwcs(lay,isr).lt.ahrwcf(lay,isr)) then
!            ahrwcf(lay,isr) = ahrwcs(lay,isr)
            write(*,*) 'Layer, Field Capacity > Saturation',            &
     &                 lay, ahrwcf(lay,isr), ahrwcs(lay,isr)
        endif

!      output for soil file screening
!        write(*,1000) sinfil,lay,aszlyt(lay,isr),
!     &        asfsan(lay,isr),asfcla(lay,isr),asfom(lay,isr),
!     &        asdwblk(lay,isr),asdblk(lay,isr),ahrwcs(lay,isr),
!     &        ahrwcf(lay,isr),ahrwcw(lay,isr),
!     &        ahrwcf(lay,isr)-ahrwcw(lay,isr),
!     &        1.0 - asdwblk(lay,isr)/asdpart(lay,isr),
!     &        ahrwcf(lay,isr)*asdwblk(lay,isr),
!     &        ahrwcw(lay,isr)*asdwblk(lay,isr),
!     &        ahrwcf(lay,isr)*asdwblk(lay,isr)-
!     &        ahrwcw(lay,isr)*asdwblk(lay,isr)

      end do

      if( wc_type.eq.4 ) then
          ! use texture based calculations from Rawls to set all soil
          ! water properties.
          call param_prop_bc(                                           &
     &        nslay(isr), aszlyd(1,isr), asdblk(1,isr), asdpart(1,isr), &
     &        asfcla(1,isr), asfsan(1,isr), asfom(1,isr), asfcec(1,isr),&
     &        ahrwcs(1,isr), ahrwcf(1,isr), ahrwcw(1,isr),ahrwcr(1,isr),&
     &        ahrwca(1,isr), ah0cb(1,isr), aheaep(1,isr), ahrsk(1,isr), &
     &        ahfredsat(1,isr) )

          do lay=1,nslay(isr)
              ! set soil to field capacity not wilting point
              ahrwc(lay,isr) = ahrwcf(lay,isr)
          end do
      else
          ! set matrix potential parameters to match 1/3 bar and 15 bar water contents
          call param_pot_bc( nslay(isr), asdblk(1,isr), asdpart(1,isr), &
     &                     ahrwcf(1,isr), ahrwcw(1,isr),                &
     &                     asfcla(1,isr), asfom(1,isr),                 &
     &                     ah0cb(1,isr), aheaep(1,isr) )
      end if

!       some soil characteristic values for crop nutrient effects 
!       were originally planned and then dropped and are not included in 
!       layer splitting. A Debug full debug compile complains
!       that these values are not initialized when they are mixed as
!       part of  management process. they are initialized here to avoid
!       removing them from mix and invert
        do lay = 1, nslay(isr)
            ascmg(lay,isr) = 0.0
            ascna(lay,isr) = 0.0
            asfesp(lay,isr) = 0.0
            asfnoh(lay,isr) = 0.0
            asfpoh(lay,isr) = 0.0
            asfpsp(lay,isr) = 0.0
        end do

      close (lui1)

      return
   81 write(*,9001) trim(sinfil(isr)), linnum, trim(line)
9001  format (' inpsub error - original format IFC file ',a,            &
     &' on line #',i4,' ',a)
      call exit(1)

   82 write(*,9002) trim(sinfil(isr)), linnum, typidx, trim(line)
9002  format (' inpsub error - original format IFC file ',a,            &
     &' on line #',i4,'(',i2,')',' ',a)
      call exit(1)

      stop

    end subroutine inpsub

end module input_soil_mod
