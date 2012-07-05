
!$Author: wjr $
!$Date: 2011-11-06 $
!$Revison: 0.1 $
!$Source: grid.f95,v $
!-----------------------------------------------------------------------------------
!! Soil data -- load ifc (initial field conditions) into this structure
!!

MODULE Soil

    USE Params

    IMPLICIT NONE

!! Methods defined in this module

    PUBLIC LoadSoilFile
    PUBLIC PrintSoil
    PRIVATE ReadLine
    PRIVATE ReadRealArray
    PRIVATE ReadInt
    PRIVATE ReadReal

!! Methods to be defined in this module

!    PUBLIC :: Dummy

!! Data structures defined in this module

    integer, parameter :: MAXNUMLAYERS = 10

    TYPE, PUBLIC :: SoilRec						            ! contains ifc data
	real :: version
	character (len = 40) :: soilID					        ! 1 Soil ID string
	character (len = 20) :: localPhase				        ! 2 Local Phase string
	character (len = 20) :: soilTaxonomy				    ! 3 Taxonomy string
	real :: soilLossTolearnce					            ! 4 NRCS Soil Loss Tolerance (t/ac/yr)
	real :: drySoilAlbedo						            ! 5 Dry soil albedo (fraction)
	real :: slopeGradient						            ! 6 Slope gradient (m/m)
	real :: surfFragCover						            ! 7 Surface frag cover (area fraction)
	real :: depthBedrock						            ! 8 Depth to bedrock (mm)
	real :: depthRootRestrictLayer					        ! 9 Depth to root restricting layer (mm)
	integer :: numberLayers						            !10 Number of soil layers
	real, dimension(:), allocatable :: layerThickness		!11 Soil layer thickness (mm)
	real, dimension(:), allocatable :: sandFrac			    !12 Sand fraction (kg/kg)
	real, dimension(:), allocatable :: siltFrac			    !13 Silt fraction (kg/kg)
	real, dimension(:), allocatable :: clayFrac			    !14 Clay fraction (kg/kg)
	real, dimension(:), allocatable :: rockFragments		!15 Rock fragments fraction (m^3/m^3)
	real, dimension(:), allocatable :: sandFracVeryCoarse	!16 Very course sand fraction (kg/kg)
	real, dimension(:), allocatable :: sandFracCoarse		!17 Course sand fraction (kg/kg)
	real, dimension(:), allocatable :: sandFracMedium		!18 Medium sand fraction (kg/kg)
	real, dimension(:), allocatable :: sandFracFine			!19 Fine sand fraction (kg/kg)
	real, dimension(:), allocatable :: sandFracVeryFine		!20 Very fine sand fraction (kg/kg)
	real, dimension(:), allocatable :: bulkDensity			!21 Bulk density [wet or 1/3 bar] (Mg/m^3)
	real, dimension(:), allocatable :: organicMatter		!22 Organic matter (kg/kg)
	real, dimension(:), allocatable :: soilPH		    	!23 PH (0-14)
	real, dimension(:), allocatable :: calciumCarbEquv		!24 Calcium Carbonate Equiv [CaCO3] (kg/kg)
	real, dimension(:), allocatable :: cationExchCap		!25 Cation Exchange Capacity [CEC] (meq/100g)
	real, dimension(:), allocatable :: linearExten			!26 Linear extensibility ((Mg/m^3)/(Mg/m^3))
	real, dimension(:), allocatable :: aggregateGeoMeanDiam	!27 ASD GMD (mm)
	real, dimension(:), allocatable :: aggregateGeoSD		!28 ASD GSD
	real, dimension(:), allocatable :: aggregateMaxSize		!29 Maximum agg. size (mm)
	real, dimension(:), allocatable :: aggregateMinSize		!30 Minimum agg. size (mm)
	real, dimension(:), allocatable :: aggregateDensity		!31 Aggregate density (Mg/m^3)
	real, dimension(:), allocatable :: aggregateStability	!32 Dry aggregate stability (ln(J/m^2))n
	real :: crustThickness						            !33 Crust thickness (mm)
	real :: crustDensity						            !34 Crust density (Mg/m^3)
	real :: crustStability						            !35 Crust stability (ln(J/m^2))
	real :: crustSurfFrac						            !36 Crust surface frction (m^2/m^2)
	real :: crustLooseMaterialMass					        !37 Mass of loose material on crust (kg/m^2)
	real :: crustLooseMaterialFrac					        !38 Fraction of loose material on crust (m^2/m^2)
	real :: randomRoughness						            !39 Random roughness (mm)
	real :: ridgeOrientation					            !40 Ridge orientation (deg)
	real :: ridgeHeight						                !41 Ridge height (mm)
	real :: ridgeSpacing						            !42 Ridge spacing (mm)
	real :: ridgeWidth						                !43 Ridge width (mm)
	real, dimension(:), allocatable :: bulkDenInit			!44 Initial BD value (Mg/m^3)
	real, dimension(:), allocatable :: soilWaterContentInit	!45 Initial SWC (m^3/m^3)
	real, dimension(:), allocatable :: soilWaterContentSaturation	!46 Saturated SWC (m^3/m^3)
	real, dimension(:), allocatable :: soilWaterContentFieldCap	    !47 Field Capacity SWC (m^3/m^3)
	real, dimension(:), allocatable :: soilWaterContentWiltingPoint !48 Wilting Point SWC (m^3/m^3)
	real, dimension(:), allocatable :: soilCB				!49 Soil CB value
	real, dimension(:), allocatable :: airEntryPotential	!50 Air Entry Potential (J/kg)
	real, dimension(:), allocatable :: satHydrConduct		!51 Saturated Hydraulic Conductivity (m/s)
	type (SoilRec), pointer :: nxtSoil  					!pointer to next soil if more than 1 soil needed
    END TYPE

    CONTAINS

    FUNCTION ReadLine (lui) result (line)
        
	integer :: lui
	character (len=100) :: line
	integer :: eofFlg

	do 
	    read(lui, '(A)', iostat = eofFlg) line
	    if (eofFlg < 0) then
	        write(*,*) 'Error reading soil file'
		stop 101
	    end if
	    if (line(1:1).ne.'#') exit
	end do

    END FUNCTION


    SUBROUTINE ReadRealArray (lui, arry, numLay) 
        
	integer, INTENT (IN) :: lui 
	real, dimension(:), allocatable, INTENT (OUT) :: arry
	integer, INTENT (IN) :: numLay


	character (len = 100) :: line
	integer :: idx
	integer :: error

	allocate(arry(numLay), stat=error)

	line = ReadLine(lui)
	
	read(line, *) (arry(idx), idx=1,numLay)

    END SUBROUTINE


    FUNCTION ReadReal (lui) result (val)
        
	integer:: lui 
	real :: val

	character (len = 100) :: line

	line = ReadLine(lui)
	read(line, *) val

    END FUNCTION

    FUNCTION ReadInt (lui) result (val)
        
	integer :: lui 
	integer :: val

	character (len = 100) :: line

	line = ReadLine(lui)
	read(line, *) val

    END FUNCTION



    FUNCTION LoadSoilFile (filNam) result (soilPtr)
    
	character (len=*), intent(in) :: filNam
	type (SoilRec), pointer :: soilPtr
	integer :: eofFlg
	character (len=80) :: bufb
	integer :: linCnt
	real, dimension(:), allocatable :: tempArry
	
	write(*,*) 'input file name: ', filNam

	allocate(soilPtr)

        open(LUISOIL, file=filNam, status="old", action="read")

	bufb = ReadLine(LUISOIL)

	if (bufb.ne.'Version: 1.0') then
	     write(*,*) 'not version 1.0 file |', trim(bufb), '|'
	     stop 102
	else 
	    soilPtr%version = 1.0
	    soilPtr%soilID = ReadLine(LUISOIL)
	    soilPtr%localPhase = ReadLine(LUISOIL)
	    soilPtr%soilTaxonomy = ReadLine(LUISOIL)
	    soilPtr%soilLossTolearnce = ReadReal(LUISOIL)
	    soilPtr%drySoilAlbedo  = ReadReal(LUISOIL)
	    soilPtr%slopeGradient = ReadReal(LUISOIL)
	    soilPtr%surfFragCover = ReadReal(LUISOIL)
	    soilPtr%depthBedrock = ReadReal(LUISOIL)
	    soilPtr%depthRootRestrictLayer = ReadReal(LUISOIL)
	    soilPtr%numberLayers = ReadInt(LUISOIL)
	    call ReadRealArray(LUISOIL, soilPtr%layerThickness, soilPtr%numberLayers)
	    call ReadRealArray(LUISOIL, soilPtr%Sandfrac, soilPtr%numberLayers)
	    call ReadRealArray(LUISOIL, soilPtr%siltFrac, soilPtr%numberLayers)
	    call ReadRealArray(LUISOIL, soilPtr%clayFrac, soilPtr%numberLayers)
	    call ReadRealArray(LUISOIL, soilPtr%rockFragments, soilPtr%numberLayers)
	    call ReadRealArray(LUISOIL, soilPtr%sandFracVeryCoarse, soilPtr%numberLayers)
	    call ReadRealArray(LUISOIL, soilPtr%sandFracCoarse, soilPtr%numberLayers)
	    call ReadRealArray(LUISOIL, soilPtr%sandFracMedium, soilPtr%numberLayers)
	    call ReadRealArray(LUISOIL, soilPtr%sandFracFine, soilPtr%numberLayers)
	    call ReadRealArray(LUISOIL, soilPtr%sandFracVeryFine, soilPtr%numberLayers)
	    call ReadRealArray(LUISOIL, soilPtr%bulkDensity, soilPtr%numberLayers)
	    call ReadRealArray(LUISOIL, soilPtr%organicMatter, soilPtr%numberLayers)
	    call ReadRealArray(LUISOIL, soilPtr%soilPH, soilPtr%numberLayers)
	    call ReadRealArray(LUISOIL, soilPtr%calciumCarbEquv, soilPtr%numberLayers)
	    call ReadRealArray(LUISOIL, soilPtr%cationExchCap, soilPtr%numberLayers)
	    call ReadRealArray(LUISOIL, soilPtr%linearExten, soilPtr%numberLayers)
	    call ReadRealArray(LUISOIL, soilPtr%aggregateGeoMeanDiam, soilPtr%numberLayers)
	    call ReadRealArray(LUISOIL, soilPtr%aggregateGeoSD, soilPtr%numberLayers)
	    call ReadRealArray(LUISOIL, soilPtr%aggregateMaxSize, soilPtr%numberLayers)
	    call ReadRealArray(LUISOIL, soilPtr%aggregateMinSize, soilPtr%numberLayers)
	    call ReadRealArray(LUISOIL, soilPtr%aggregateDensity, soilPtr%numberLayers)
	    call ReadRealArray(LUISOIL, soilPtr%aggregateStability, soilPtr%numberLayers)
	    soilPtr%crustThickness = ReadReal(LUISOIL)
	    soilPtr%crustDensity = ReadReal(LUISOIL)
	    soilPtr%crustStability = ReadReal(LUISOIL)
	    soilPtr%crustSurfFrac = ReadReal(LUISOIL)
	    soilPtr%crustLooseMaterialMass = ReadReal(LUISOIL)
	    soilPtr%crustLooseMaterialFrac = ReadReal(LUISOIL)
	    soilPtr%randomRoughness = ReadReal(LUISOIL)
	    soilPtr%ridgeOrientation = ReadReal(LUISOIL)
	    soilPtr%ridgeHeight = ReadReal(LUISOIL)
	    soilPtr%ridgeSpacing = ReadReal(LUISOIL)
	    soilPtr%ridgeWidth = ReadReal(LUISOIL)
	    call Readrealarray(LUISOIL, soilPtr%bulkDenInit, soilPtr%numberLayers)
	    call ReadRealArray(LUISOIL, soilPtr%soilWaterContentInit, soilPtr%numberLayers)
	    call ReadRealArray(LUISOIL, soilPtr%soilWaterContentSaturation, soilPtr%numberLayers)
	    call ReadRealArray(LUISOIL, soilPtr%soilWaterContentFieldCap, soilPtr%numberLayers)
	    call ReadRealArray(LUISOIL, soilPtr%soilWaterContentWiltingPoint, soilPtr%numberLayers)
	    call ReadRealArray(LUISOIL, soilPtr%soilCB , soilPtr%numberLayers)
	    call ReadRealArray(LUISOIL, soilPtr%airEntryPotential, soilPtr%numberLayers)
	    call ReadRealArray(LUISOIL, soilPtr%satHydrConduct, soilPtr%numberLayers)
	endif

	close(LUISOIL)

    END FUNCTION

    SUBROUTINE PrintSoil(luo, soilPtr)
    
	integer, INTENT (IN) :: luo
	type (SoilRec), pointer, INTENT (IN) :: soilPtr
	integer :: idx
	
	character (len = 30) :: fmtLinF7_3
	character (len = 30) :: fmtLinE7_3
	character (len = 30) :: fmtLinF7_0

	write(fmtLinF7_3, '(A,I2,A)') '(A, ', soilPtr%numberLayers, 'F7.3)'
	write(fmtLinE7_3, '(A,I2,A)') '(A, ', soilPtr%numberLayers, 'E10.3)'
	write(fmtLinF7_0, '(A,I2,A)') '(A, ', soilPtr%numberLayers, 'F7.0)'

	write (luo, '(A,1X,A)') 'soilID ', soilPtr%soilID
	write (luo, '(A,1X,A)') 'localPhase ', soilPtr%localPhase
	write (luo, '(A,1X,A)') 'soilTaxonomy ', soilPtr%soilTaxonomy
	write (luo, fmtLinF7_3) 'soilLossTolearnce ', soilPtr%soilLossTolearnce
	write (luo, fmtLinF7_3) 'drySoilAlbedo ', soilPtr%drySoilAlbedo
	write (luo, fmtLinF7_3) 'slopeGradient ', soilPtr%slopeGradient
	write (luo, fmtLinF7_3) 'surfFragCover ', soilPtr%surfFragCover
	write (luo, fmtLinF7_0) 'depthBedrock ', soilPtr%depthBedrock
	write (luo, fmtLinF7_0) 'depthRootRestrictLayer ', soilPtr%depthRootRestrictLayer
	write (luo, '(A,1X,I3)') 'numberLayers ', soilPtr%numberLayers
	write (luo, fmtLinF7_0) 'layerThickness ', (soilPtr%layerThickness(idx), idx=1, soilPtr%numberLayers)
	write (luo, fmtLinF7_3) 'sandFrac ', (soilPtr%sandFrac(idx), idx=1, soilPtr%numberLayers)
	write (luo, fmtLinF7_3) 'siltFrac ', (soilPtr%siltFrac(idx), idx=1, soilPtr%numberLayers)
	write (luo, fmtLinF7_3) 'clayFrac ', (soilPtr%clayFrac(idx), idx=1, soilPtr%numberLayers)
	write (luo, fmtLinF7_3) 'rockFragments ', (soilPtr%rockFragments(idx), idx=1, soilPtr%numberLayers)
	write (luo, fmtLinF7_3) 'sandFracVeryCoarse ', (soilPtr%sandFracVeryCoarse(idx), idx=1, soilPtr%numberLayers)
	write (luo, fmtLinF7_3) 'sandFracCoarse ', (soilPtr%sandFracCoarse(idx), idx=1, soilPtr%numberLayers)
	write (luo, fmtLinF7_3) 'sandFracMedium ', (soilPtr%sandFracMedium(idx), idx=1, soilPtr%numberLayers)
	write (luo, fmtLinF7_3) 'sandFracFine ', (soilPtr%sandFracFine(idx), idx=1, soilPtr%numberLayers)
	write (luo, fmtLinF7_3) 'sandFracVeryFine ', (soilPtr%sandFracVeryFine(idx), idx=1, soilPtr%numberLayers)
	write (luo, fmtLinF7_3) 'bulkDensity ', (soilPtr%bulkDensity(idx), idx=1, soilPtr%numberLayers)
	write (luo, fmtLinF7_3) 'organicMatter ', (soilPtr%organicMatter(idx), idx=1, soilPtr%numberLayers)
	write (luo, fmtLinF7_3) 'soilPH ', (soilPtr%soilPH(idx), idx=1, soilPtr%numberLayers)
	write (luo, fmtLinF7_3) 'calciumCarbEquv ', (soilPtr%calciumCarbEquv(idx), idx=1, soilPtr%numberLayers)
	write (luo, fmtLinF7_3) 'cationExchCap ', (soilPtr%cationExchCap(idx), idx=1, soilPtr%numberLayers)
	write (luo, fmtLinF7_3) 'linearExten ', (soilPtr%linearExten(idx), idx=1, soilPtr%numberLayers)
	write (luo, fmtLinF7_3) 'aggregateGeoMeanDiam ', (soilPtr%aggregateGeoMeanDiam(idx), idx=1, soilPtr%numberLayers)
	write (luo, fmtLinF7_3) 'aggregateGeoSD ', (soilPtr%aggregateGeoSD(idx), idx=1, soilPtr%numberLayers)
	write (luo, fmtLinF7_3) 'aggregateMaxSize ', (soilPtr%aggregateMaxSize(idx), idx=1, soilPtr%numberLayers)
	write (luo, fmtLinF7_3) 'aggregateMinSize ', (soilPtr%aggregateMinSize(idx), idx=1, soilPtr%numberLayers)
	write (luo, fmtLinF7_3) 'aggregateDensity ', (soilPtr%aggregateDensity(idx), idx=1, soilPtr%numberLayers)
	write (luo, fmtLinF7_3) 'aggregateStability ', (soilPtr%aggregateStability(idx), idx=1, soilPtr%numberLayers)
	write (luo, fmtLinF7_3) 'crustThickness ', soilPtr%crustThickness
	write (luo, fmtLinF7_3) 'crustDensity ', soilPtr%crustDensity
	write (luo, fmtLinF7_3) 'crustStability ', soilPtr%crustStability
	write (luo, fmtLinF7_3) 'crustSurfFrac ', soilPtr%crustSurfFrac
	write (luo, fmtLinF7_3) 'crustLooseMaterialMass ', soilPtr%crustLooseMaterialMass
	write (luo, fmtLinF7_3) 'crustLooseMaterialFrac ', soilPtr%crustLooseMaterialFrac
	write (luo, fmtLinF7_3) 'randomRoughness ', soilPtr%randomRoughness
	write (luo, fmtLinF7_3) 'ridgeOrientation ', soilPtr%ridgeOrientation
	write (luo, fmtLinF7_3) 'ridgeHeight ', soilPtr%ridgeHeight
	write (luo, fmtLinF7_3) 'ridgeSpacing ', soilPtr%ridgeSpacing
	write (luo, fmtLinF7_3) 'ridgeWidth ', soilPtr%ridgeWidth
	write (luo, fmtLinF7_3) 'bulkDenInit ', (soilPtr%bulkDenInit(idx), idx=1, soilPtr%numberLayers)
	write (luo, fmtLinF7_3) 'soilWaterContentInit ', (soilPtr%soilWaterContentInit(idx), idx=1, soilPtr%numberLayers)
	write (luo, fmtLinF7_3) 'soilWaterContentSaturation ', (soilPtr%soilWaterContentSaturation(idx), idx=1, soilPtr%numberLayers)
	write (luo, fmtLinF7_3) 'soilWaterContentFieldCap ', (soilPtr%soilWaterContentFieldCap(idx), idx=1, soilPtr%numberLayers)
	write (luo, fmtLinF7_3) 'soilWaterContentWiltingPoint ', (soilPtr%soilWaterContentWiltingPoint(idx), idx=1, soilPtr%numberLayers)
	write (luo, fmtLinF7_3) 'soilCB ', soilPtr%soilCB
	write (luo, fmtLinF7_3) 'airEntryPotential ', soilPtr%airEntryPotential
	write (luo, fmtLinE7_3) 'satHydrConduct ', soilPtr%satHydrConduct
	
    END SUBROUTINE  

END MODULE Soil


!-----------------------------------------------------------------------------------
!Sample IFC file

!Version: 1.0
!#
!# Soil ID
!NA-NA-Clay Loam-100-CL-NA-NA-NA
!#
!# Local Phase
!unknown
!# Soil Order
!NA
!# Soil Loss Tolerance (tons/acre/year)
!5
!# Dry soil albedo (fraction)
!0.329
!# Slope gradient (fraction)
!0.010
!# Surface fragment cover or surface layer fragments (area fraction)
!0.000
!#
!# Depth to bedrock (mm)
!99990
!# Depth to root restricting layer (mm)
!99990
!#
!# Number of layers
!1
!# Layer thickness (mm)
!1500     
!#
!# Sand fraction
!0.330     
!# Silt fraction
!0.340     
!# Clay fraction
!0.330     
!# Rock fragments
!0.000     
!# Sand fraction very coarse
!0.060     
!# Sand fraction coarse
!0.060     
!# Sand fraction medium
!0.070     
!# Sand fraction fine
!0.070     
!# Sand fraction very fine
!0.070     
!#
!# Bulk Density (1/3 bar)(Mg/m^3)
!1.320     
!# Organic matter (kg/kg)
!0.0150     
!# Soil PH (0-14)
!7.00     
!# Calcium carbonate equivalent (CaCO3)
!0.00     
!# Cation exchange capacity (CEC) (meq/100g)
!19.50     
!# Linear extensibility
!0.374     
!#
!# Aggregate geometric mean diameter (mm)
!19.519     
!# Aggregate geometric standard deviation
!12.243     
!# Maximum aggregate size (mm)
!53.209     
!# Minimum aggregate size (mm)
!0.010     
!# Aggregate density (Mg/m^3)
!1.800     
!# Aggregate stability (ln(J/m^2))
!3.419     
!#
!# Crust thickness (mm)
!0.010
!# Crust density (Mg/m^3)
!1.800
!# Crust stability (ln(J/m^2))
!3.42
!# Crust surface fraction (m^2/m^2)
!0.00
!# Mass of loose material on crust (kg/m^2)
!0.00
!# Fraction of loose material on crust (m^2/m^2)
!0.00
!#
!# Random roughness (mm)
!4.00
!# Ridge orientation (deg)
!0.00
!# Ridge height (mm)
!0.00
!# Spacing between ridge tops (mm)
!10.00
!# Ridge width (mm)
!10.00
!#
!# Initial Bulk Density (1/3 bar)(Mg/m^3)
!1.230     
!# Initial soil water content (m^3/m^3)
!0.253     
!# Saturation soil water content (m^3/m^3)
!0.502     
!# Field capacity water content (m^3/m^3)
!0.322     
!# Wilting point water content (m^3/m^3)
!0.184     
!#
!# Soil CB value (exponent to Campbell's SWRC)
!6.810     
!# Air entry potential (J/kg)
!-1.606     
!# Saturated hydraulic conductivity (m/s)
!2.759E-5     
!#
!# Notes:
!# # This file can be used when USDA NRCS soils information is not available.
!# # The users assume all responsiblity to estimate the correct texture of the unknow soil.
!# # The minimum data fields were set using the mid values of the USDA NRCS textural triangle.
!# # The remaining values were estimated using the WEPS soil estimation routines.
!-----------------------------------------------------------------------------------
