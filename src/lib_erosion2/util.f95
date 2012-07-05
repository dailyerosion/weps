
!$Author: wjr $
!$Date: 2011-11-06 $
!$Revison: 0.1 $
!$Source: testerode.f95,v $
!-----------------------------------------------------------------------------------
!! Utility (used in more than one sub-model) functions and parameters
!!

MODULE Util

    public :: calcBioDrag
    public :: calcSoilMassFrac

      real, parameter :: mtomm  = 1000.0		! meters to mm
      real, parameter :: mmtom  = 0.001			! mm to meters
      integer, parameter :: hrday  = 24			! hours per day
      real, parameter :: hrtosec = 3600.0		! seconds per hour
      real, parameter :: daytosec = 86400.0		! seconds per day
      real, parameter :: degtorad = 0.017453293 !pi/180	! degrees to radians
      real, parameter :: radtodeg = 57.2957795 !180/pi	! radians to degrees
      real, parameter :: hatom2 = 10000.0		! hectare to square meters
      real, parameter :: mgtokg = 0.000001		! milligram to kilogram
      real, parameter :: fractopercent = 100.0		! fraction to percent
      real, parameter :: percenttofrac = 0.01		! percent to fraction

CONTAINS

    FUNCTION calcBioDrag (resiLAI, resiSAI, cropLAI, cropSAI, furrFlg, rowSpac, &
        cropHght, ridgHght) result (bioDrag)

!     + + + PURPOSE + + +
!     calcBioDrag: combine effects of leaves and stems on drag coef.
!     Calling subroutine needs b1glob.inc, c1gen.inc s1sgeo.inc

!     Leaves are less effective at reducing the wind speed than
!     stems.  Three effects are simulated: 1. streamlining of leaves,
!     2. leaf sheltered in furrow, and
!     3.leaf area confined in wide rows that act as wind barriers.
!     This function combines these effects into a single
!     value for use by other routines. May still be too large.

!     + + + KEYWORDS + + +
!     biodrag

!     + + + ARGUMENT DECLARATIONS + + +
      real    :: resiLAI, resiSAI, cropLAI, cropSAI
      integer :: furrFlg
      real    :: rowSpac, cropHght, ridgHght
      
      real :: bioDrag
!     + + + ARGUMENT DEFINITIONS + + +
!     calcBioDrag  - drag coefficient (no units)
!     resiLAI   - residue leaf area index (sum of all pools)(m^2/m^2)
!     resiSAI   - residue stem silhouette area index (sum of all pools)(m^2/m^2)
!     cropLAI   - crop leaf area index (m^2/m^2)
!     cropSAI   - crop stem silhouette area index (m^2/m^2)
!     furrFlg    - crop seed location flag (0= in furrow, 1=on ridge)
!     rowSpac   - crop row spacing (m)(0 = broadcast)
!     cropHght    - crop biomass height (m)
!     ridgHght   - ridge height (mm)

!     + + + PARAMETERS + + +
      real, parameter :: furrDisc = 0.5
!     furrDisc  - coefficient for discounting drag of plant in furrow bottom

!     + + + LOCAL VARIABLES + + +
      real :: reduLAI, reduSAI, reduFact

!     + + + LOCAL VARIABLE DEFINITIONS + + +
!     reduLAI     - reduced leaf area index (m^2/m^2)
!     reduSAI     - reduced stem area index (m^2/m^2)
!     reduFact     - reduction factor

!     + + + END SPECIFICATIONS + + +

      ! place crop values in temporary variables
      reduLAI = cropLAI
      reduSAI = cropSAI

      ! check for crop biomass position with respect to the ridge
      if(furrFlg .eq. 0) then
          ! biomass in furrow
          ! test plant height and ridge height for minimums
          if( cropHght .gt. (furrDisc * ridgHght * mmtom) ) then
              ! sufficient height for some effect
              reduFact = (1.0 - furrDisc * ridgHght * mmtom / cropHght)
              reduLAI = reduLAI * reduFact
              reduSAI = reduSAI * reduFact

              ! check for row width effect
              if( rowSpac .gt. cropHght*5.0 ) then						!eq. OE-65
                  reduFact = 1.0/(0.92 + 0.021 * rowSpac / (cropHght - furrDisc * ridgHght * mmtom) )
                  reduLAI = reduLAI * reduFact
              end if

          else
              ! not tall enough to do anything
              reduLAI = 0.0
              reduSAI = 0.0
          endif
      else
          ! biomass not in furrow
          ! test plant height and ridge height for minimums
          if( cropHght .gt. 0.0 ) then
              ! check for row width effect
              if( rowSpac .gt. cropHght*5.0 ) then
                  reduFact = 1.0 / (0.92 + 0.021 * rowSpac / cropHght)				!eq OE-65
                  reduLAI = reduLAI * reduFact
              end if
          else
              ! not tall enough to do anything
              reduLAI = 0.0
              reduSAI = 0.0
          endif
      end if

      ! add discounted crop values to biomass values
      reduLAI = reduLAI + resiLAI
      reduSAI = reduSAI + resiSAI

      ! streamline effect for total leaf area
      reduLAI = reduLAI * 0.2 * (1.0 - exp(-reduLAI))						!eq OE-67

      ! final result
      bioDrag = reduLAI + reduSAI

      END FUNCTION

      !subroutine sbsfdi (slagm, s0ags, slagn, slagx, sldi, sfdi)
      FUNCTION calcSoilMassFrac(aggrDistGMD, aggrDistGSD, aggrDistMin, aggrDistMax, soilDiamB) &
        	result(soilMassFrac)

!     +++ PURPOSE +++
!     calc soil mass fraction (soilMassFrac) < diameter (soilDiam)
!     given modified lognormal distribution parameters

!     +++ ARGUMENT DECLARATIONS +++
      real aggrDistGMD, aggrDistGSD, aggrDistMin, aggrDistMax, soilDiam, soilMassFrac
      
!     +++  ARGUMENT DEFINITIONS +++
!     aggrDistGMD - aggregate distribution geometric mean diameter (mm).
!     aggrDistGSD - aggregate distribution geometric standard deviation.
!     aggrDistMin - aggregate distribution lower limit (mm).
!     aggrDistMax - aggregate distribution upper limit (mm).
!     soilDiam  - soil diameter in distribution (mm)
!     soilMassFrac  - soil mass fraction < soilDiam

!     +++ LOCAL VARIABLES +++
      real slt

!     +++ FUNCTIONS CALLED+++
      real erf

!     +++ END SPECIFICATIONS +++

!     calc soil mass < soilDiam

      if (soilDiam .lt. aggrDistMax .and. soilDiam .gt. aggrDistMin) then			!eq. OE-81
        slt = ((soilDiam - aggrDistMin)*(aggrDistMax - aggrDistMin))/((aggrDistMax - soilDiam)*aggrDistGMD)
        soilMassFrac = 0.5*(1 + erf(alog(slt)/(sqrt(2.0)*alog(aggrDistGSD))))			!eq. OE-82
      elseif (soilDiam .ge. aggrDistMax) then
        soilMassFrac = 1.0
      else
        soilMassFrac = 0.0
      endif

      end function

      FUNCTION calcFricVelc (anemHght, aeroRougWindStat, windSped, aeroRoug, bioDrag) &
    	result (thrsFricVel)
!
!     +++ PURPOSE +++
!     To calculate subregion, friction velocity, given station
!     anemometer height, surface roughness, wind speed; and subregion
!     aerodynamic roughness.
!
!     if standing biomass present, then calculate friction velocity
!     at surface below the canopy (wus).
!
!     +++ ARGUMENT DECLARATIONS +++
      real  :: anemHght, aeroRougWindStat, windSped, aeroRoug
      real  :: bioDrag, thrsFricVel
!
!     +++ ARGUMENT DEFINITIONS +++
!
!     anemHght - parameter, anemometer height of input wind speed (m).
!     aeroRougWindStat - parameter, surface aerodynamic roughness at input wind
!             speed location (mm).
!     windSped - input wind speed driving EROSION submodel (m/s).
!     aeroRoug - subregion aerodynamic roughness (mm).
!     bioDrag - biomass drag coefficient
!     wus - subregion soil surface friction velocity (m/s)
!           i.e. below canopy, if one exists.
!
!     +++ LOCAL VARIABLES +++
      real thrsFricVelst, thrsFricVelv
!
!     +++ END SPECIFICATIONS +++
!     note:  in BLOCK.FOR wzoflg should be set to 1 and anemomht
!             set to correct height if anemometer is at field site
!             to obtain correct values from SBWUS or read as
!             input data in stand-alone EROSION.
!
!     Calc station (input wind speed location) friction velocity
      thrsFricVelst = windSped*0.4/alog(anemHght*1000./aeroRougWindStat)
!
!     calc subregion friction velocity
      thrsFricVel = thrsFricVelst * (aeroRoug/aeroRougWindStat)**0.067
!
!     if standing biomass, calculate wus below canopy
      if (bioDrag .gt. 0.0001 ) then
         thrsFricVelv = thrsFricVel
!
!        calculate friction velocity below canopy

        if( bioDrag.gt.2.56) then       !check to avoid underflow
            thrsFricVel = thrsFricVelv * 0.25*exp(-bioDrag/0.356)
        else
            thrsFricVel = thrsFricVelv*(0.86*exp(-bioDrag/0.0298)+0.25*exp(-bioDrag/0.356))
        endif
         thrsFricVel = amin1(thrsFricVel,thrsFricVelv)
      endif
!
      return
      end function


END MODULE Util