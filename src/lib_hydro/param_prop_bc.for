!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine param_prop_bc( nlay, bszlyd, bsdblk, bsdpart,          &
     &                          bsfcla, bsfsan, bsfom, bsfcec,          &
     &                          bhrwcs, bhrwcf, bhrwcw, bhrwcr,         &
     &                          bhrwca, bh0cb, bheaep, bhrsk,           &
     &                          bhfredsat )

!     + + + PURPOSE + + +
!     
!     This subroutine calculates the full range of matric potential
!     parameters and properties from given values of bulk density, cation
!     exchange capacity, sand, clay and organic matter fractions
!     Equations taken from: Rawls, D.L. and D.L. Brakensiek. 1989.
!     Estimation of soil water retention and hydraulic properties, H.J.
!     Morel-Seytoux (ed.), Unsaturated flow in hydraulic modeling theory
!     and practice. 275-300, NATO ASI Series. Series c: Mathematical and
!     physical science, vol. 275

!     + + + KEYWORDS + + +
!     matric potential parameters

      use weps_interface_defs
      use p1unconv_mod, only: fractopercent, hrtosec, mmtom

!     + + + ARGUMENT DECLARATIONS + + +
      integer nlay
      real bszlyd(*), bsdblk(*), bsdpart(*)
      real bsfcla(*), bsfsan(*), bsfom(*), bsfcec(*)
      real bhrwcs(*), bhrwcf(*), bhrwcw(*), bhrwcr(*)
      real bhrwca(*), bh0cb(*), bheaep(*), bhrsk(*)
      real bhfredsat(*)

!     + + + ARGUMENT DEFINITIONS + + +
!     nlay     - number of soil layers to be updated
!     bszlyd - Depth to bottom of each soil layer for each subregion (mm)
!     bsdblk   - bulk density (Mg/m^3) = (g/cm^3)
!     bsdpart  - particle density (Mg/m^3)
!     bsfcla   - fraction of soil mineral portion which is clay
!     bsfsan   - fraction of soil mineral portion which is sand
!     bsfom    - fraction of total soil mass which is organic matter
!     bsfcec   - Soil layer cation exchange capacity (cmol/kg) (meq/100g)
!     bhrwcs   - gravimetric saturated water
!     bhrwcf   - gravimetric 1/3 bar water
!     bhrwcw   - gravimetric 15 bar water
!     bhrwcr   - gravimetric residual water
!     bhrwca   - gravimetric plant available water
!     bh0cb    - Brooks and Corey pore size interation exponent b
!     bheaep   - Brooks and Corey air entry potential (J/kg)
!     bhrsk    - saturated hydraulic conductivity (m/s)
!     bhfredsat - fraction of soil porosity that will be filled with water
!                 while wetting under normal field conditions due to entrapped air

!     + + + LOCAL COMMON BLOCKS + + +
      include 'hydro/vapprop.inc'

!     + + + FUNCTION DECLARATIONS + + +
!      real volwat_matpot_bc

!     + + + LOCAL VARIABLES + + +
      integer lay
      real porosity, adj_por, eff_por, depth
      real cec_clay, cec_rat, trap_air_frac
      real per_clay, per_sand, per_om
      real thetas, thetaf, thetaw, thetar
      real airentry, lambda, k_sat
      real thetaf_10, thetaf_3

!     + + + LOCAL VARIABLE DEFINITIONS + + +

!     + + + PARAMETERS + + +
      real   sand_hi
      parameter   (sand_hi = 0.85)
      real   sand_lo
      parameter   (sand_lo = 0.60)

!     + + + END SPECIFICATIONS + + + 

      do lay=1,nlay

          ! indicated range for relationships is enforced here
          per_clay = min(60.0, max(5.0, bsfcla(lay) * fractopercent))
          per_sand = min(70.0, max(5.0, bsfsan(lay) * fractopercent))
          per_om = bsfom(lay) * fractopercent

          porosity = 1 - bsdblk(lay) / bsdpart(lay)

          ! Taken from WEPP tech Doc, 1995, eq 7.7.3
          ! using the botom of the layer for a first approximation
          depth = bszlyd(lay) * mmtom
          cec_clay = bsfcec(lay) - per_om * (1.42 + 1.7 * depth)

          ! WEPP documentation is not clear but implies clay fraction.
          ! Rawls, in the NATO pub indicates that it is percent clay
          ! and gives the range of 0.1 to 0.9 which works best with
          ! percent clay when CEC is in meq/100g
          ! indicated range for relationship is enforced here
          cec_rat = min(0.9, max(0.1, cec_clay / per_clay))

          ! the NATO reference has a misprint. Correct equation was found
          ! in WEPP tech DOC, 1995 based on personal communication with
          ! Baumer. This is actually the porosity where water can enter
          trap_air_frac = 1.0 - (3.8 + 0.00019 * per_clay * per_clay    &
     &        - 0.03365 * per_sand + 0.126 * cec_rat * per_clay         &
     &        + per_om * (per_sand / 200.0)**2 ) / 100.0

          ! can we assume here that the curves that were fit are laboratory
          ! moisture release curves, and would have been done at full saturation?
          adj_por = porosity
          ! adj_por = porosity * trap_air_frac

          ! Brooks-Corey air entry potential
          ! original expression is in cm, multiply by 0.01 to get meters
          airentry = - 0.01 * exp( 5.3396738                            &
     &        + 0.1845038  * per_clay                                   &
     &        - 2.48394546 * adj_por                                    &
     &        - 0.00213853 * per_clay * per_clay                        &
     &        - 0.04356349 * per_sand * adj_por                         &
     &        - 0.61745089 * per_clay * adj_por                         &
     &        + 0.00143598 * per_sand * per_sand * adj_por * adj_por    &
     &        - 0.00855375 * per_clay * per_clay * adj_por * adj_por    &
     &        - 0.00001282 * per_sand * per_sand * per_clay             &
     &        + 0.00895359 * per_clay * per_clay * adj_por              &
     &        - 0.00072472 * per_sand * per_sand * adj_por              &
     &        + 0.00000540 * per_clay * per_clay * per_sand             &
     &        + 0.50028060 * adj_por * adj_por * per_clay)

          ! Brooks-Corey pore size interaction paramter
          lambda = exp( -0.7842831                                      &
     &        + 0.0177544  * per_sand                                   &
     &        - 1.062498   * adj_por                                    &
     &        - 0.00005304 * per_sand * per_sand                        &
     &        - 0.00273493 * per_clay * per_clay                        &
     &        + 1.11134946 * adj_por * adj_por                          &
     &        - 0.03088295 * per_sand * adj_por                         &
     &        + 0.00026587 * per_sand * per_sand * adj_por * adj_por    &
     &        - 0.00610522 * per_clay * per_clay * adj_por * adj_por    &
     &        - 0.00000235 * per_sand * per_sand * per_clay             &
     &        + 0.00798746 * per_clay * per_clay * adj_por              &
     &        - 0.00674491 * adj_por * adj_por * per_clay)

!          thetar = (0.2+ 0.1*per_om + 0.25*per_clay*cec_rat**0.45)      &
!     &        * bsdblk(lay) / 100.0

          thetar = -0.0182482 + 0.00087269 * per_sand                   &
     &        + 0.00513488 * per_clay + 0.02939286 * adj_por            &
     &        - 0.00015395 * per_clay * per_clay                        &
     &        - 0.0010827 * per_sand * adj_por                          &
     &        - 0.00018233 * per_clay * per_clay * adj_por * adj_por    &
     &        + 0.00030703 * per_clay * per_clay * adj_por              &
     &        - 0.0023584 * adj_por * adj_por * per_clay

          thetas = adj_por                          ! saturation

          ! find 1/10th bar soil water content
          thetaf_10 = volwat_matpot_bc( potfcs, thetar, thetas,         &
     &        airentry, lambda )

          ! find 1/3rd bar soil water content
          thetaf_3 = volwat_matpot_bc(potfc, thetar, thetas,            &
     &        airentry, lambda)

          ! set field capacity based on texture
!          if( bsfsan(lay) .gt. sand_hi ) then
!              thetaf = thetaf_10
!          else if( bsfsan(lay) .gt. sand_lo ) then
!              thetaf = thetaf_3 + (thetaf_10-thetaf_3)                  &
!     &            * ( (bsfsan(lay)-sand_lo) / (sand_hi-sand_lo) )
!          else
              thetaf = thetaf_3
!          end if

          thetaw = volwat_matpot_bc(potwilt, thetar, thetas,            &
     &        airentry, lambda)

          ! Talked with Walter Rawls on September 2, 2004 and he recommended
          ! k_sat taken from: Rawls, W.J., D. Gimenez, R. Grossman.
          ! 1998. Use of soil texture, bulk density and slope of the water
          ! retention curve to predict saturated hydraulic conductivity.
          ! Trans. of ASAE, Vol. 41(4):983-988

          ! note that the use of adj_por to get the effective porosity
          ! is not specifically mentioned in this article, but it is
          ! mentioned in the NATO article.
          eff_por = adj_por - thetaf

          ! result is in mm/hour
          k_sat = 1930.0 * eff_por ** (3.0 - lambda)

          ! update values for global variables
          bhrwcs(lay) = thetas / bsdblk(lay)        ! saturation
          bhrwcf(lay) = thetaf / bsdblk(lay)        ! field capacity
          bhrwcw(lay) = thetaw / bsdblk(lay)        ! wilting point
          bhrwcr(lay) = thetar / bsdblk(lay)        ! residual

          bhrwca(lay) = bhrwcf(lay) - bhrwcw(lay)   ! plant available capacity

          bh0cb(lay) = 1.0 / lambda                 ! Brooks and Corey B
          ! multiply meters by the gravitational constant to get J/kg
          bheaep(lay) = airentry * gravconst        ! Brooks and Corey air entry potential

          bhrsk(lay) = k_sat * mmtom / hrtosec      ! saturated hydraulic conductivity
          bhfredsat(lay) = trap_air_frac
      end do

      end
