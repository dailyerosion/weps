!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine transp (layrsn, actflg, bszlyd, bszlyt, rootd,         &
     &                   theta, thetas, thetaf, thetaw,                 &
     &                   theta80rh, thetar, airentry, lambda,           &
     &                   ksat, soiltemp, potwu, actwu, wsf)
!     + + + PURPOSE + + +
!     This subroutine determines the actual plant transpiration first
!     by distributing the potential rate of plant transpiration
!     throughout the root zone.  the actual plant transpiration is
!     then obtained by adjusting the potential plant traspiration
!     on the basis of soil water availability.
!     DATE:  09/22/93
!     MODIFIED:  10/06/93
!     MODIFIED:  08/03/95
!     MODIFIED:  05/01/99 - LEW

!     + + + KEYWORDS + + +
!     transpiration

!     + + + ARGUMENT DECLARATIONS + + +
      integer layrsn, actflg
      real bszlyd(*), bszlyt(*), rootd
      real theta(0:*), thetas(*), thetaf(*), thetaw(*)
      real theta80rh(*), thetar(*), airentry(*), lambda(*)
      real ksat(*), soiltemp(*), potwu, actwu, wsf

!     + + + ARGUMENT DEFINITIONS + + +
!     layrsn - Number of soil layers used in simulation
!     actflg - flag to trigger removal of soil water
!              0 no soil water removal
!              1 soil water is acturally removed 
!     bszlyd  - Depth to bottom of soil layer from surface (mm)
!     bszlyt - Layer thickness (mm)
!     rootd   - Plant root depth (converted to mm)
!     theta      - present volumetric water content
!     thetas     - saturated volumetric water content
!     thetaf     - field capacity volumetric water content
!     thetaw     - volumetric water content at wilting (15 bar or 1.5 MPa)
!     theta80rh  - volumetric water content at %80 relative humidity (300 bar or 30 MPa)
!     thetar     - volumetric water content where hydraulic conductivity becomes zero
!     airentry   - Brooks/Corey air entry potential (1/pressure) (modify to set units returned)
!     lambda     - Brooks/Corey pore size interaction parameter 
!     ksat       - Saturated hydraulic conductivity (m/s)
!     soiltemp   - soil temperature (C)
!     potwu      - potential use of water by plant (mm)
!     actwu      - actual soil water use (estimated or actucally removed (mm)
!     wsf    - Plant growth water stress factor (unitless)

!     + + + PARAMETERS + + +
      real   wud
      parameter   (wud = 3.0650)
      real   wuc
!      parameter   (wuc = 1.0)
      parameter   (wuc = 0.8)
      real cond_crit
      parameter   (cond_crit = 1.0e-12)      !(m/s) = 8.64*10^-5 mm/day

!     wud - Water use distribution, a depth parameter of 3.065 is
!           used in weps assuming about 30 percent of the total
!           water use comes from the top 10 percent of the root
!           zone
!     wuc - Water use compensation factor, determines how much a plant
!           can draw water from lower soil layers when higher soil layers
!           are dry. wuc = 0 means water will be withdrawn according to
!           the wud distribution without compensation if upper layers are 
!           dry. wuc = 1 means that if water is available in lower layer, 
!           more water than indicated by wud will be withdrawn.
!     cond_crit = critical soil water conductivity where water stress occurs
!           Taken from Gardner, C.M.K., K.B. Laryea and P.W. Unger. 1999.
!           Soil Physical Constraints to Plant Growth and Crop Production.
!           AGL/MISC/24/99, Land and Water Development Division, Food and
!           Agriculture Organization of the United Nations, Rome, page 24

!     + + + FUNCTIONS CALLED + + +

      real availwc
      real acplwu
      real unsatcond_bc

!     availwc - Available water content (mm/mm)
!     acplwu - Actual water use rate from soil layer (mm/day)
!     unsatcond_bc - unsaturated hydraulic conductivity (same units as ksat)

!     + + + LOCAL COMMON BLOCKS + + +
!     include 'hydro/vapprop.inc'

!     + + + LOCAL VARIABLES + + +

      integer k
      real depth
      real awcr, awcr_crit
      real wua, wup
      real wup_fac(0:layrsn)
      real wu_bal
      real potm, soilrh, cond

!     + + + LOCAL DEFINITIONS + + +

!     k      - Local loop variable
!     awcr   - Relative available soil water content (or potential), fraction (0-1.0)
!     awcr_crit - Critical relative available soil water content (or potential)
!     wua    - Actual water use rate from soil layer (mm/day)
!     wup    - Potential water use rate from soil layer (mm/day)
!     wup_fac - water uptake factor with depth in soil
!     wu_bal  - used to store and test layer water balance after removal
!     potm    - soil water potential (meters of water at max density)
!     soilrh  - soil air relative humidity
!     cond    - soil hydraulic conductivity (m/s)

!     + + + END SPECIFICATIONS + + +

      !handle special case of no roots yet
      if (rootd .le. 0.0) then
          actwu = 0.0
          goto 999  !finish up
      endif

      actwu = 0.0
      wup_fac(0) = 0.0
      do 100 k = 1, layrsn
         !compute transpiration in root zone only
         if (rootd .ge. bszlyd(k)) then
             depth = bszlyd(k)
         else if (rootd .gt. (bszlyd(k)-bszlyt(k))) then
             depth = rootd
         else
             goto 999  !we are done with layers having root mass
         endif
         wup_fac(k) = (1.0 - exp(-wud*depth/rootd)) / (1.0 - exp(-wud))
         wup = potwu * (wup_fac(k)-(1.0-wuc)*wup_fac(k-1)) - wuc*actwu

         ! volumetric soil water content based approach
         awcr = availwc(theta(k), thetaw(k), thetaf(k))
         ! critical value for using fraction of available water content
         awcr_crit = 0.3

         !soil water potential based approach
         ! Feddes,R.A., H. Hoff, M. Bruen, T. Dawson,d P.Rosnay, P. Dirmeyer,
         ! R.B. Jackson, P. Kabat, A. Kleidon, A. Lilly, and A.J. Pitman.
         ! 2001. Modeling Root Water Uptake in Hydrological and Climate Models.
         ! Bulletin of the American Meteorological Society. Vol. 82, No. 12, Pg. 2800
!         call matricpot_bc(theta(k), thetar(k), thetas(k),              &
!     &                     airentry(k), lambda(k), thetaw(k),           &
!     &                     theta80rh(k), soiltemp(k), potm, soilrh )

         cond = unsatcond_bc(theta(k), thetar(k),                       &
     &                       thetas(k), ksat(k), lambda(k))
!         awcr = min(1.0, max(0.0, (potm - potwilt) / (potfc - potwilt)))
!         awcr_crit = 0.95  ! this corresponds to -1bar water stress level

         ! test for critical conductivity value
         ! from Gardner, C.M.K., K.B. Laryea, P.W. Unger. 1999. Soil Physical
         ! Constraints to Plant Growth and Crop Production. Land and Water
         ! Development Division, Food and Agriculture Organization of the
         ! United nations, Rome
         if( cond .lt. cond_crit ) then
             wup = 0.0
         end if

         ! Bristow, K.L., G.S. Campbell, and C. Calissendorff. 1984. The effects
         ! of texture on the resistance to water movement within the
         ! rhizosphere. Soil Sci. Soc. Am. J. 48:266-270.
         ! test for critical root interface conductivity
         if( (theta(k) / thetas(k)) .lt. 0.25 ) then
             wup = 0.0
         end if

         ! get acutal layer water use
         wua = acplwu(awcr, awcr_crit, wup)
         ! update soil water content in layer
         ! prevent going beyond wilting point
         if( wua .gt. 0.0 ) then
             wu_bal = theta(k) - wua / bszlyt(k)
             if( wu_bal .lt. thetaw(k) ) then
                 wua = (theta(k) - thetaw(k)) * bszlyt(k)
                 if( actflg .eq. 1 ) then
                     ! water content is changed
                     theta(k) = thetaw(k)
                 end if
             else
                 if( actflg .eq. 1 ) then
                     ! water content is changed
                     theta(k) = wu_bal
                 end if
             end if
             actwu = actwu + wua
          end if
100   continue

999   continue

      !Since potwu is set to zero when a crop is in dormancy, eg.,
      !winter wheat, we check that condition here.
      !However, this routine really shouldn't be called if a crop
      !is in dormancy because it isn't using any water - LEW 5/1/99
      if (potwu .eq. 0.0) then
         wsf = 1.0              !set it to not negatively influence growth
      else
         wsf = actwu / potwu  !set water stress factor
      endif

      return
      end
