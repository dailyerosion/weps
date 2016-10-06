!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!
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

      end

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

      end
