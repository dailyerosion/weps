!$Author$
!$Date$
!$Revision$
!$HeadURL$

module wind_mod

  contains

    subroutine sbzo (sxprg, szrgh, slrr, bbzht, brcd, wzoflg, wzorg, wzorr, wzzo, wzzov, awzzo)

!     +++ PURPOSE +++
!     Calc. aerodynamic roughness parm., wzzo, with no standing biomass
!           wzzo is used by sbwust

!     Calc. aerodynamic roughness parm. as wzzov, if standing biomass
!              else let wzzov = wzzo
!         wzzov is used by sbwus

!     set anem aero. roughness and field roughness equal when anem. at
!         the field site, ie. wzoflg = 1
!     to calculate aerodynamic roughness of vegetation canopy.
!     Ref. Trans ASAE 31(3):769-775, Armbrust and Bilbro, 1995

      use p1erode_def, only: WZZO_MIN, WZZO_MAX
      use p1unconv_mod, only: mtomm

!     +++ ARGUMENT DECLARATIONS +++
      real, intent(in) :: sxprg  ! row/dike spacing parallel the wind (mm)
      real, intent(in) :: szrgh  ! ridge height (mm)
      real, intent(in) :: slrr   ! random roughness (mm)
      real, intent(in) :: bbzht  ! composite average biomass height (m)
      real, intent(in) :: brcd   ! biomass drag coefficient
      integer, intent(in) :: wzoflg ! flag=0 - anemometer at station
                                    ! flag=1 - anemometer at field
      real, intent(out) :: wzorg  ! aerodynamic roughness of ridge
      real, intent(out) :: wzorr  ! aerodynamic roughness of random roughness
      real, intent(out) :: wzzo   ! aerodynamic roughness of surface below canopy (mm)
      real, intent(out) :: wzzov  ! aerodynamic roughness length of canopy (mm)
      real, intent(inout) :: awzzo  ! aerodynamic roughness at anemom. site (mm)

!     +++ LOCAL VARIABLES +++
      real :: hl    ! ratio of ridge height to parallel ridge spacing
      real :: bht   ! biomass height (mm)

!     +++ END SPECIFICATIONS +++
      ! Note: wzoflg should be set to 1 and anemomht changed if the anemomenter is at the field site
      ! to obtain correct values from SBZO

      ! calc. for ridge aerodynamic roughness
      if (szrgh .gt. 5.0) then
        hl   = szrgh / sxprg
        ! winds are never continually normal to ridges, so restrict hl.
        hl = min(0.20,hl)
        wzorg = szrgh * 1/(-64.1+135.5*hl+(20.84/sqrt(hl)))
      else
        wzorg = 0.
      endif

      ! calculation for random aerodynamic roughness
      wzorr = slrr*0.3
      !set upper and lower limits on aerodynamic roughness
      wzorr = min(WZZO_MAX, wzorr)   ! RR <= ~100.0mm
      wzorr = max(wzorr, WZZO_MIN)   ! RR >= ~1.67mm

      ! estimate combined ridge and random aerodynamic roughness
      ! (later- no data sets at present) chose the largest of the two.
      wzzo = max (wzorg, wzorr)

      ! calculate aerodynamic roughness of vegetation, if present

      ! convert biomass height to mm
      bht = bbzht * mtomm

      ! calculate roughness length of canopy ( in mm)
      if (brcd .gt. 0.1) then
        wzzov = bht * 1/(17.27-(1.254*alog(brcd)/brcd)-(3.714/brcd))
      else if( (bht .gt. 5.0) .and. (brcd .gt. 0.001) ) then
          ! wzzov = bht*exp(alog(wzzo/bht) + (alog(0.11*bht/wzzo) * alog(brcd/0.01))/2.3) ! caused Simon's instability
        wzzov = bht*(wzzo/bht+((0.11-wzzo/bht)/4.60517)*alog(brcd/0.001))
      else
          wzzov = 0.0
      endif

      ! choose the maximum of canopy or surface roughness
      wzzov = max(wzzov, wzzo)

      if (wzoflg .eq. 1) then
         ! anemom. in field set awzzo to wzzov
         awzzo = wzzov
      endif

    end subroutine sbzo

    function sbwus( anemht, awzzo, awu, wzzov, brcd ) result( wus )

!     +++ PURPOSE +++
!     To calculate subregion, friction velocity, given station
!     anemometer height, surface roughness, wind speed; and subregion
!     aerodynamic roughness.

!     if standing biomass present, then calculate friction velocity
!     at surface below the canopy (wus).

!     +++ ARGUMENT DECLARATIONS +++
      real, intent(in) :: anemht ! parameter, anemometer height of input wind speed (m).
      real, intent(in) :: awzzo  ! parameter, surface aerodynamic roughness at input wind speed location (mm).
      real, intent(in) :: awu    ! input wind speed driving EROSION submodel (m/s).
      real, intent(in) :: wzzov  ! subregion aerodynamic roughness (mm).
      real, intent(in) :: brcd   ! biomass drag coefficient
      real :: wus    ! subregion soil surface friction velocity (m/s) i.e. below canopy, if one exists.

!     +++ LOCAL VARIABLES +++
      real :: wusst  ! station (ie. anemomter location friction velocity)
      real :: wusv   ! friction veolocity value retained for check against below canopy value

!     +++ END SPECIFICATIONS +++

!     note:  wzoflg should be set to 1 and anemht set to correct height if anemometer is at field site
!             to obtain correct values from SBWUS or read as input data in stand-alone EROSION.

      ! Calc station (input wind speed location) friction velocity
      wusst = awu*0.4/alog(anemht*1000./awzzo)

      ! calc subregion friction velocity
      wus = wusst * (wzzov/awzzo)**0.067

      ! if standing biomass, calculate wus below canopy
      if (brcd .gt. 0.0001 ) then
         wusv = wus

        ! calculate friction velocity below canopy
        if( brcd.gt.2.56) then       !check to avoid underflow
            wus = wusv * 0.25*exp(-brcd/0.356)
        else
            wus = wusv*(0.86*exp(-brcd/0.0298)+0.25*exp(-brcd/0.356))
        endif
        wus = min(wus,wusv)
      endif

    end function sbwus

    subroutine sbzdisp( wzoflg, brcd, bbzht, bczht, awzdisp, wzdisp )

!     +++ PURPOSE +++
!     Calc. zero plane displacement (mm)

!     set field zero plane displacement equal to Anemometer zero plane
!     displacement when anem. at the field site, ie. wzoflg = 1

      ! using equation from: Raupach, M.R. 1994. Simplified Expressions for
      ! Vegetation Roughness Length and Zero-Plane Displacement as functions
      ! of Canopy Height and Area index. Boundary Layer meteorology 71:211-216.

      use p1unconv_mod, only: mtomm

!     +++ ARGUMENT DECLARATIONS +++
      integer, intent(in) :: wzoflg ! flag=0 - weather measurements are from a distant station
                                    ! flag=1 - weather measurements are in field
      real, intent(in) :: brcd      ! biomass drag coefficient (or "effective" biomass silhouette area index)
      real, intent(in) :: bbzht     ! composite average residue height (m)
      real, intent(in) :: bczht     ! crop height (m)
      real, intent(inout) :: awzdisp  ! zero plane displacement at weather location (mm)
      real, intent(out) :: wzdisp   ! zero plane displacement at location (mm)

!     +++ LOCAL VARIABLES +++
      real  bht

!     +++ LOCAL VARIABLE DEFINITIONS +++
!     bht   - biomass height (mm)

!     +++ END SPECIFICATIONS +++

      ! find maximum biomass height and convert to mm
      bht = max(bbzht, bczht) * mtomm

      ! use silhouette area index and biomass height to find zero plane displacement
      if( brcd .gt. 1.0e-10 ) then
          wzdisp = bht * (1.0 - (1.0 - exp( -(15.0*brcd)**0.5) ) / (15.0*brcd)**0.5)
      else
          wzdisp = 0.0
      end if

      if (wzoflg .eq. 1) then
         ! anemom. in field, set weather displacement to field displacement
         awzdisp = wzdisp
      endif

    end subroutine sbzdisp

    function biodrag (bdrlai, bdrsai, bcrlai, bcrsai, bc0rg, bcxrow, bczht, bszrgh) result( bio_drag )

!     + + + PURPOSE + + +
!     BIODRAG: combine effects of leaves and stems on drag coef.

!     Leaves are less effective at reducing the wind speed than
!     stems.  Three effects are simulated: 1. streamlining of leaves,
!     2. leaf sheltered in furrow, and
!     3.leaf area confined in wide rows that act as wind barriers.
!     This function combines these effects into a single
!     value for use by other routines. May still be too large.

      use p1unconv_mod, only: mmtom

!     + + + ARGUMENT DECLARATIONS + + +
      real, intent(in) :: bdrlai   ! residue leaf area index (sum of all pools)(m^2/m^2)
      real, intent(in) :: bdrsai   ! residue stem silhouette area index (sum of all pools)(m^2/m^2)
      real, intent(in) :: bcrlai   ! crop leaf area index (m^2/m^2)
      real, intent(in) :: bcrsai   ! crop stem silhouette area index (m^2/m^2)
      integer, intent(in) :: bc0rg    ! crop seed location flag (0 = in furrow, 1 = on ridge)
      real, intent(in) :: bcxrow   ! crop row spacing (m)(0 = broadcast)
      real, intent(in) :: bczht    ! crop biomass height (m)
      real, intent(in) :: bszrgh   ! ridge height (mm)

      real :: bio_drag  ! drag coefficient (no units)

!     + + + PARAMETERS + + +
      real fur_dis      ! coefficient for discounting drag of plant in furrow bottom
      parameter( fur_dis = 0.5 )

!     + + + LOCAL VARIABLES + + +
      real :: red_lai     ! reduced leaf area index (m^2/m^2)
      real :: red_sai     ! reduced stem area index (m^2/m^2)
      real :: red_fac     ! reduction factor

!     + + + END SPECIFICATIONS + + +

      ! place crop values in temporary variables
      red_lai = bcrlai
      red_sai = bcrsai

      ! check for crop biomass position with respect to the ridge
      if(bc0rg .eq. 0) then
          ! biomass in furrow
          ! test plant height and ridge height for minimums
          if( bczht .gt. (fur_dis * bszrgh * mmtom) ) then
              ! sufficient height for some effect
              red_fac = (1.0 - fur_dis * bszrgh * mmtom / bczht)
              red_lai = red_lai * red_fac
              red_sai = red_sai * red_fac

              ! check for row width effect
              red_fac = min( 1.0, 1.0/(0.92 + 0.021 * bcxrow / (bczht - fur_dis * bszrgh * mmtom) ) )
              red_lai = red_lai * red_fac

          else
              ! not tall enough to do anything
              red_lai = 0.0
              red_sai = 0.0
          endif
      else
          ! biomass not in furrow
          ! test plant height and ridge height for minimums
          if( bczht .gt. 0.0 ) then
              ! check for row width effect
              red_fac = min( 1.0, 1.0 / (0.92 + 0.021 * bcxrow / bczht))
              red_lai = red_lai * red_fac
          else
              ! not tall enough to do anything
              red_lai = 0.0
              red_sai = 0.0
          endif
      end if

      ! add discounted crop values to biomass values
      red_lai = red_lai + bdrlai
      red_sai = red_sai + bdrsai

      ! streamline effect for total leaf area
      red_lai = red_lai * 0.2 * (1.0 - exp(-red_lai))

      ! final result
      bio_drag = red_lai + red_sai

    end function biodrag

    subroutine anemometer_init

! + + +  PURPOSE + + +
!     To provide initial default values to wx station variables

!     The anemom. ht. and awwzo may be changed by read inputs in the
!     stand-alone erosion code. If anem. at the field  i.e flag =1,
!     then awwzo is set equal to the field zo in sbwus.

! + + + VARIABLE DEFINITIONS + + +
!     anemht = anemometer height (m)
!     awzzo  = aerodynamic roughness at anemometer (mm)
!     awzdisp - Weather station zero plane displacement height (mm)
!     wzoflg = flag = 0 for anem. and fixed awwzo at wx station
!              flag = 1 for anem. and variable awzzo at field

! + + + END SPECIFICATIONS + + +

      use erosion_data_struct_defs, only: anemht, awzzo, awzdisp, wzoflg

      ! set the default data values
      anemht =  10.0
      awzzo = 25.0
      awzdisp = 0.0
      wzoflg = 0

   end subroutine anemometer_init

end module wind_mod

