!$Author$
!$Date$
!$Revision$
!$HeadURL$
module hydro_util_mod

  contains

    subroutine param_blkden_adj( nlay, bsdblk, bsdblk0, &
                               bsdpart, bhrwcf, bhrwcw, bhrwca, &
                               bsfcla, bsfom, bh0cb, bheaep, bhrsk )

      ! + + + PURPOSE + + +
      ! 
      ! This subroutine adjusts the air entry potential and saturated hydraulic
      ! for changes in bulk density

      ! + + + KEYWORDS + + +
      ! bulk density adjustment hydraulic parameters

      use hydro_data_struct_defs, only: claygrav80rh, orggrav80rh

      ! + + + ARGUMENT DECLARATIONS + + +
      integer nlay
      real bsdblk(*), bsdblk0(*)
      real bsdpart(*), bhrwcf(*), bhrwcw(*), bhrwca(*)
      real bsfcla(*), bsfom(*)
      real bh0cb(*), bheaep(*), bhrsk(*)

      ! + + + ARGUMENT DEFINITIONS + + +
      ! nlay     - number of soil layers to be updated
      ! bsdblk   - bulk density (Mg/m^3)
      ! bsdblk0  - previous day bulk density (Mg/m^3)
      ! bsdpart  - particle density (Mg/m^3)
      ! bhrwcf   - gravimetric 1/3 bar water
      ! bhrwcw   - gravimetric 15 bar water
      ! bhrwca   - gravimetric plant available water
      ! bsfcla   - fraction of soil mineral portion which is clay
      ! bsfomf   - fraction of total soil mass which is organic matter
      ! bh0cb    - Brooks and Corey pore size interation exponent b
      ! bheaep   - Brooks and Corey air entry potential
      ! bhrsk    - Saturated hydraulic conductivity (m/s)

      ! + + + LOCAL VARIABLES + + +
      integer lay
      real thetas, thetaf, thetaw, thetar
      real temp, temp1

      ! + + + LOCAL VARIABLE DEFINITIONS + + +

      ! + + + END SPECIFICATIONS + + + 

      do lay=1,nlay
          ! adjust air entry potential from Campbell (1985) pg 46
          bheaep(lay) = bheaep(lay) * (bsdblk(lay)/bsdblk0(lay))**(0.67*bh0cb(lay))
          ! adjust saturated hydraulic conductivity based on Campbell (1985) pg 54
          bhrsk(lay) = bhrsk(lay) * (bsdblk0(lay)/bsdblk(lay))**(1.34*bh0cb(lay))
          ! update previous day bulk density since changes done
          bsdblk0(lay) = bsdblk(lay)

          ! reverse calculation of field capacity and wilting point
          thetas = 1 - bsdblk(lay) / bsdpart(lay)   ! saturation

          ! use theta corresponding to 80% relhum in soil for thetar
          temp = bsdblk(lay)*1000.0  !convert Mg/m^3 to kg/m^3
          thetar = volwatadsorb( temp, bsfcla(lay), bsfom(lay), claygrav80rh, orggrav80rh )

          temp = -33.33
          temp1 = 1.0 / bh0cb(lay)
          thetaf = volwat_matpot_bc(temp, thetar, thetas, bheaep(lay), temp1)
          temp = -1500.0
          thetaw = volwat_matpot_bc(temp, thetar, thetas, bheaep(lay), temp1)

          ! update gravimetric values for these properties
          bhrwcf(lay) = thetaf / bsdblk(lay)        ! field capacity
          bhrwcw(lay) = thetaw / bsdblk(lay)        ! wilting point
          bhrwca(lay) = bhrwcf(lay) - bhrwcw(lay)   ! plant available capacity
      end do

    end subroutine param_blkden_adj

    subroutine param_pot_bc( nlay, bsdblk, bsdpart, bhrwcf, bhrwcw, &
                               bsfcla, bsfom, bh0cb, bheaep )

      ! + + + PURPOSE + + +
      ! 
      ! This subroutine calculates matric potential parameters from given
      ! values of bulk density, gravimetric 1.3 bar water, 15 bar water and
      ! clay and organic matter fraction

      ! + + + KEYWORDS + + +
      ! matric potential parameters

      use hydro_data_struct_defs, only: claygrav80rh, orggrav80rh

      ! + + + ARGUMENT DECLARATIONS + + +
      integer nlay
      real bsdblk(*)
      real bsdpart(*), bhrwcf(*), bhrwcw(*)
      real bsfcla(*), bsfom(*)
      real bh0cb(*), bheaep(*)

      ! + + + ARGUMENT DEFINITIONS + + +
      ! nlay     - number of soil layers to be updated
      ! bsdblk   - bulk density (Mg/m^3)
      ! bsdpart  - particle density (Mg/m^3)
      ! bhrwcf   - gravimetric 1/3 bar water
      ! bhrwcw   - gravimetric 15 bar water
      ! bsfcla   - fraction of soil mineral portion which is clay
      ! bsfomf   - fraction of total soil mass which is organic matter
      ! bh0cb    - Brooks and Corey pore size interation exponent b
      ! bheaep   - Brooks and Corey air entry potential

      ! + + + FUNCTION DECLARATIONS + + +
      !  real volwatadsorb

      ! + + + LOCAL VARIABLES + + +
      integer lay
      real thetas, thetaf, thetaw, thetar
      real temp

      ! + + + LOCAL VARIABLE DEFINITIONS + + +

      ! + + + END SPECIFICATIONS + + + 

      do lay=1,nlay
          thetaf = bhrwcf(lay) * bsdblk(lay)        ! field capacity
          thetaw = bhrwcw(lay) * bsdblk(lay)        ! wilting point
          thetas = 1 - bsdblk(lay) / bsdpart(lay)   ! saturation

          ! use theta corresponding to 80% relhum in soil for thetar
          temp = bsdblk(lay)*1000.0  !convert Mg/m^3 to kg/m^3
          thetar = volwatadsorb( temp, bsfcla(lay), bsfom(lay), claygrav80rh, orggrav80rh )

          ! error check and adjustments to prevent numerical problems
          ! with curve fit
          if( thetas.le.thetaf ) then
              write(0,*) &
               'Error: saturation less than field capacity, layer', lay
              call exit(1)
          else if( thetaf.le.thetaw ) then
              write(0,*) 'field capacity less than wilting point, layer', lay
              call exit(1)
          end if
          thetar = min( thetar, 0.8 * thetaw )

          ! Calculate air entry and b to match saturated, field
          ! capacity, permanent wilting point and residual water
          ! as calcuated above 
          bh0cb(lay) = -(log(33.333)-log(1500.0)) &
                  / (log((thetaf-thetar)/(thetas-thetar)) &
                  - log((thetaw-thetar)/(thetas-thetar)))
          bheaep(lay)=-exp(log(1500.0)+bh0cb(lay) &
                   *log((thetaw-thetar)/(thetas-thetar)))

          ! error check brooks and corey b value to keep in range and
          ! prevent later numberical problems
          if( bh0cb(lay).gt.50.0 ) then
              write(0,*) 'Derived Brooks and Corey b too large, layer', lay
              call exit(1)
          endif

      end do

    end subroutine param_pot_bc

    subroutine param_prop_bc( nlay, bszlyd, bsdblk, bsdpart, &
                                bsfcla, bsfsan, bsfom, bsfcec, &
                                bhrwcs, bhrwcf, bhrwcw, bhrwcr, &
                                bhrwca, bh0cb, bheaep, bhrsk, &
                                bhfredsat )

      ! + + + PURPOSE + + +
      ! 
      ! This subroutine calculates the full range of matric potential
      ! parameters and properties from given values of bulk density, cation
      ! exchange capacity, sand, clay and organic matter fractions
      ! Equations taken from: Rawls, W.J. and D.L. Brakensiek. 1989.
      ! Estimation of soil water retention and hydraulic properties, H.J.
      ! Morel-Seytoux (ed.), Unsaturated flow in hydraulic modeling theory
      ! and practice. 275-300, NATO ASI Series. Series c: Mathematical and
      ! physical science, vol. 275

      ! + + + KEYWORDS + + +
      ! matric potential parameters

      use p1unconv_mod, only: fractopercent, hrtosec, mmtom
      use hydro_data_struct_defs, only: gravconst, potfc, potfcs, potwilt

      ! + + + ARGUMENT DECLARATIONS + + +
      integer nlay
      real bszlyd(*), bsdblk(*), bsdpart(*)
      real bsfcla(*), bsfsan(*), bsfom(*), bsfcec(*)
      real bhrwcs(*), bhrwcf(*), bhrwcw(*), bhrwcr(*)
      real bhrwca(*), bh0cb(*), bheaep(*), bhrsk(*)
      real bhfredsat(*)

      ! + + + ARGUMENT DEFINITIONS + + +
      ! nlay     - number of soil layers to be updated
      ! bszlyd - Depth to bottom of each soil layer for each subregion (mm)
      ! bsdblk   - bulk density (Mg/m^3) = (g/cm^3)
      ! bsdpart  - particle density (Mg/m^3)
      ! bsfcla   - fraction of soil mineral portion which is clay
      ! bsfsan   - fraction of soil mineral portion which is sand
      ! bsfom    - fraction of total soil mass which is organic matter
      ! bsfcec   - Soil layer cation exchange capacity (cmol/kg) (meq/100g)
      ! bhrwcs   - gravimetric saturated water
      ! bhrwcf   - gravimetric 1/3 bar water
      ! bhrwcw   - gravimetric 15 bar water
      ! bhrwcr   - gravimetric residual water
      ! bhrwca   - gravimetric plant available water
      ! bh0cb    - Brooks and Corey pore size interation exponent b
      ! bheaep   - Brooks and Corey air entry potential (J/kg)
      ! bhrsk    - saturated hydraulic conductivity (m/s)
      ! bhfredsat - fraction of soil porosity that will be filled with water
      !             while wetting under normal field conditions due to entrapped air

      ! + + + LOCAL VARIABLES + + +
      integer lay
      real porosity, adj_por, eff_por, depth
      real cec_clay, cec_rat, trap_air_frac
      real per_clay, per_sand, per_om
      real thetas, thetaf, thetaw, thetar
      real airentry, lambda, k_sat
      real thetaf_10, thetaf_3

      ! + + + LOCAL VARIABLE DEFINITIONS + + +

      ! + + + PARAMETERS + + +
      !  real   sand_hi
      !  parameter   (sand_hi = 0.85)
      !  real   sand_lo
      !  parameter   (sand_lo = 0.60)

      ! + + + END SPECIFICATIONS + + + 

      do lay=1,nlay

          ! indicated range for relationships is enforced here
            !  per_clay = min(60.0, max(5.0, bsfcla(lay) * fractopercent))
            !  per_sand = min(70.0, max(5.0, bsfsan(lay) * fractopercent))
          ! testing shows that equations are well behaved outside of range
          per_clay = bsfcla(lay) * fractopercent
          per_sand = bsfsan(lay) * fractopercent
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
          trap_air_frac = 1.0 - (3.8 + 0.00019 * per_clay * per_clay &
              - 0.03365 * per_sand + 0.126 * cec_rat * per_clay &
              + per_om * (per_sand / 200.0)**2 ) / 100.0

          ! can we assume here that the curves that were fit are laboratory
          ! moisture release curves, and would have been done at full saturation?
          adj_por = porosity
          ! adj_por = porosity * trap_air_frac

          ! Brooks-Corey air entry potential
          ! original expression is in cm, multiply by 0.01 to get meters
          airentry = - 0.01 * exp( 5.3396738 &
              + 0.1845038  * per_clay &
              - 2.48394546 * adj_por &
              - 0.00213853 * per_clay * per_clay &
              - 0.04356349 * per_sand * adj_por &
              - 0.61745089 * per_clay * adj_por &
              + 0.00143598 * per_sand * per_sand * adj_por * adj_por &
              - 0.00855375 * per_clay * per_clay * adj_por * adj_por &
              - 0.00001282 * per_sand * per_sand * per_clay &
              + 0.00895359 * per_clay * per_clay * adj_por &
              - 0.00072472 * per_sand * per_sand * adj_por &
              + 0.00000540 * per_clay * per_clay * per_sand &
              + 0.50028060 * adj_por * adj_por * per_clay)

          ! Brooks-Corey pore size interaction paramter
          lambda = exp( -0.7842831 &
              + 0.0177544  * per_sand &
              - 1.062498   * adj_por &
              - 0.00005304 * per_sand * per_sand &
              - 0.00273493 * per_clay * per_clay &
              + 1.11134946 * adj_por * adj_por &
              - 0.03088295 * per_sand * adj_por &
              + 0.00026587 * per_sand * per_sand * adj_por * adj_por &
              - 0.00610522 * per_clay * per_clay * adj_por * adj_por &
              - 0.00000235 * per_sand * per_sand * per_clay &
              + 0.00798746 * per_clay * per_clay * adj_por &
              - 0.00674491 * adj_por * adj_por * per_clay)

      !      thetar = (0.2+ 0.1*per_om + 0.25*per_clay*cec_rat**0.45) &
      ! &        * bsdblk(lay) / 100.0

          thetar = -0.0182482 + 0.00087269 * per_sand &
              + 0.00513488 * per_clay + 0.02939286 * adj_por &
              - 0.00015395 * per_clay * per_clay &
              - 0.0010827 * per_sand * adj_por &
              - 0.00018233 * per_clay * per_clay * adj_por * adj_por &
              + 0.00030703 * per_clay * per_clay * adj_por &
              - 0.0023584 * adj_por * adj_por * per_clay

          thetas = adj_por                          ! saturation

          ! find 1/10th bar soil water content
          thetaf_10 = volwat_matpot_bc( potfcs, thetar, thetas, airentry, lambda )

          ! find 1/3rd bar soil water content
          thetaf_3 = volwat_matpot_bc(potfc, thetar, thetas, airentry, lambda)

      ! set field capacity based on texture
      !      if( bsfsan(lay) .gt. sand_hi ) then
      !          thetaf = thetaf_10
      !      else if( bsfsan(lay) .gt. sand_lo ) then
      !          thetaf = thetaf_3 + (thetaf_10-thetaf_3) &
      ! &            * ( (bsfsan(lay)-sand_lo) / (sand_hi-sand_lo) )
      !      else
              thetaf = thetaf_3
      !      end if

          thetaw = volwat_matpot_bc(potwilt, thetar, thetas, airentry, lambda)

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

    end subroutine param_prop_bc

    real function albedo (bcrlai, snwc, sndp, soil)

      ! + + + purpose + + +
      ! this subroutine estimates the net radiation for a given area
      ! using known solar radiation, air temperature, and vapor pressure
      ! according to wright's (1982) modified version of penman's (1948)
      ! relationship.

      ! + + + key words + + +
      ! radiation, hydrology, weps
      use p1unconv_mod, only: mtomm
      use hydro_heat_mod, only: max_snow_den, min_snow_den
      use soil_data_struct_defs, only: soil_def

      ! + + + argument declarations + + +
      real bcrlai
      real snwc
      real sndp
      type(soil_def), intent(inout) :: soil  ! soil for this subregion

      ! + + + argument definitions + + +
      ! bcrlai   - plant leaf area index
      ! snwc     - water content of snow, mm
      ! sndp     - depth of snow, mm

      ! + + + local variables + + +
      real snow_den
      real albs
      real albsn
      real sci
      real snci
      real pci

      ! + + + local definitions + + +
      ! snow_den - density of the snow
      ! albs   - soil albedo
      ! albsn  - snow albedo
      ! sci    - soil albedo fraction
      ! snci   - snow albedo fraction
      ! pci    - plant albedo fraction

      ! + + + parameters + + +
      real albp
      real alb_snow_max
      real alb_snow_min
      parameter   (albp = 0.23)            ! albedo of plants
      parameter   (alb_snow_max = 0.6)     ! albedo of new snow
      parameter   (alb_snow_min = 0.2)     ! albedo of fully dense snow

      ! + + + data initialization + + +

      ! + + + end specifications + + +

      ! estimate snow albedo

      ! using mass of existing snow (snwc)
      ! units: mm * (1m/1000mm) * 1000 kg/m^3 = kg/m^2
      ! and physical depth of snow (sndp)
      ! units: 1000 mm/m * kg/m^2 / mm = kg/m^3
      if( sndp .gt. 0.0 ) then
          snow_den = mtomm * snwc / sndp
          albsn = alb_snow_min + (alb_snow_max - alb_snow_min) * (max_snow_den - snow_den)/(max_snow_den - min_snow_den)
      else
          albsn = alb_snow_max
      end if

      ! estimate the surface albedo
      if ( snwc .ge. 5.0 ) then
        albedo = albsn                          !snow covers surface & plants
      else
        snci = snwc / 5.0                       !coverage factor for snow
        pci = min(bcrlai/3, 1.0)                !coverage factor for plants based upon leaf area index
        if (pci + snci .gt. 1.0) pci = 1.0 - snci  !make sure factors sum to 1
        sci = 1.0 - (pci + snci)                !soil albedo factor is what is left over
        if (sci.gt.0.0) then                    !need to calc soil albedo
          albs = soil%asfald + (soil%asfalw - soil%asfald) &
               * (soil%theta(0) - soil%thetaw(1)) / (soil%thetaf(1) - soil%thetaw(1))
          albs = max(albs, soil%asfalw)   !no less than wet  (wet is less than dry)
          albs = min(albs, soil%asfald)   !no greater than dry
          albedo = snci*albsn + pci * albp + sci * albs
        else
          albedo = snci*albsn + pci * albp
        endif
      endif

      return
    end function albedo

    real function radnet( bcrlai, bweirr, snwc, sndp, bwtdmx, bwtdmn, &
                          bmalat, idoy, bwtdpt, soil )

      ! + + + purpose + + +
      ! this function estimates the net radiation for a given area (Mj/m^2/day)
      ! using known solar radiation, air temperature, and vapor pressure
      ! according to wright's (1982) modified version of penman's (1948)
      ! relationship.

      ! + + + key words + + +
      ! radiation, hydrology, weps

      use solar_mod, only: radext
      use soil_data_struct_defs, only: soil_def

      ! + + + argument declarations + + +
      real bcrlai, bweirr, snwc, sndp, bwtdmx, bwtdmn
      real bmalat
      integer idoy
      real bwtdpt
      type(soil_def), intent(inout) :: soil  ! soil for this subregion

      ! + + + argument definitions + + +
      ! bcrlai - plant leaf area index
      ! bweirr - solar radiation, Mj/m^2/day
      ! snwc   - water content of snow, mm
      ! sndp   - actual depth of snow, mm
      ! bwtdmx - maximum air temperature, c
      ! bwtdmn - minimum air temperature, c
      ! bmalat - latitude of the site, degrees
      ! idoy   - julian day of year, 1-366
      ! bwtdpt - dew point temperature, c

      ! + + + local variables + + +
      real albt
      real tmink
      real tmaxk
      real rna
      real rnb
      real ra
      real rso
      real a
      real a1
      real b
      real e
      real rno

      ! + + + local definitions + + +
      ! albt - composite albedo value (snow, plant, soil)
      ! tmink, tmaxk - temperatures converted from degrees C to degrees K
      ! rna  - net radiation term a
      ! rnb  - net radiation term b
      ! ra   - extraterrestrial radiation
      ! rso  - terrestrial clear sky radiation
      ! a, b - coefficients relating proportioning long wave and short wave
      !        radiation exchange based on actual to clear sky ratio
      ! a1, e - intermediate calculations
     
      ! + + + parameters + + +

      real coeff,sbc

      parameter   (coeff= -2.9e-5, sbc = 4.903e-9)

      ! coeff  - coefficient in the equation to estimate soil cover index
      ! sbc    - stefan-boltzmann constant, mj/m^2/day

      ! + + + FUNCTION DECLARATIONS + + +
      ! real albedo

      ! + + + end specifications + + +

      tmaxk = bwtdmx + 273.15         !prereq h-17
      tmink = bwtdmn + 273.15         !prereq h-17

      ra = radext(idoy, bmalat)
      rso = 0.75*ra            !h-19

      if( rso.gt.1.0e-36 ) then
          if ((bweirr/rso).gt.(0.7)) then
            a = 1.126
            b = -0.07
          else
            a = 1.017
            b = -0.06
          end if
          a1 = 0.26 + 0.1*exp(-((0.0154*(idoy - 180))**2))   !h-18
          e = exp((16.78*bwtdpt - 117)/(bwtdpt + 237.3))
          rno = (sbc*(tmaxk**4+tmink**4)/2)*(a1 - 0.139 * sqrt(e)) !h-17(b)

          albt = albedo (bcrlai, snwc, sndp, soil)

          rna = (1-albt)*bweirr          !h-17(a)

          rnb = (a*(bweirr/rso) + b)        !h-17(c)

          radnet = rna-(rno*rnb)         !h-17
      else
          radnet = 0.0
      end if

      return
    end function radnet

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
!              1 soil water is actually removed 
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
!      real availwc
!      real acplwu
!      real unsatcond_bc

!     availwc - Available water content (mm/mm)
!     acplwu - Actual water use rate from soil layer (mm/day)
!     unsatcond_bc - unsaturated hydraulic conductivity (same units as ksat)

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

         ! get actual layer water use
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
    end subroutine transp

    subroutine matricpot_bc(theta, thetar, thetas, airentry, lambda,  &
     &                        thetaw, theta80rh, soiltemp,              &
     &                        matricpot, soilrh )

!     returns: matricpot, soilrh
!     returns the matric potential in meters of water as defined by the 
!     Brooks and Corey function down to wilting point. Below wilting point,
!     the calculation is done based on clay and organic matter adsorption
!     isotherms. Coincidentally, the soil relative humidity is used to find 
!     the matric potential from the clay isotherms and is also returned for
!     potentials in the wetter range.

!*** Argument declarations ***
      real  theta, thetar, thetas, airentry, lambda
      real  thetaw, theta80rh, soiltemp
      real  matricpot, soilrh
!     theta      - present volumetric water content
!     thetar     - volumetric water content where hydraulic conductivity becomes zero
!     thetas     - saturated volumetric water content
!     airentry   - Brooks/Corey air entry potential (1/pressure) (modify to set units returned)
!     lambda     - Brooks/Corey pore size interaction parameter 
!     thetaw     - volumetric water content at wilting (15 bar or 1.5 MPa)
!     theta80rh  - volumetric water content at %80 relative humidity (300 bar or 30 MPa)
!     soiltemp   - soil temperature (C)
!     matricpot  - matric potential (meters of water)
!     soilrh     - relative humidity of soil air (fraction)

!*** function declarations ***
!      real soilrelhum, matricpot_from_rh

!*** Local variable declarations ***
      real  satrat
!     satrat     - conductivity relative saturation ratio

      if( theta .ge. thetaw ) then
          satrat = (theta-thetar)/(thetas-thetar)
          if( satrat .le. 0.0 ) then
              write(0,*) 'matricpot_bc: thetar= ',thetar,               &
     &                   ' thetaw= ', thetaw
              write(0,*)                                                &
     &  'Error: residual water content is greater than wilting point'
              call exit(1)
              !stop
          else if( satrat .ge. 1.0 ) then
              matricpot = airentry
          else
              matricpot = airentry*satrat**(-1.0/lambda)
          end if
          soilrh = soilrelhum(theta, thetaw, theta80rh, soiltemp,       &
     &                        matricpot)
      else
          soilrh = soilrelhum(theta, thetaw, theta80rh, soiltemp,       &
     &                        matricpot)
          matricpot = matricpot_from_rh( soilrh, soiltemp )
      end if
      return
    end subroutine matricpot_bc

    real function soilrelhum(theta, thetaw, theta80rh, soiltemp,      &
     &                           matricpot)
!     returns the soil relative humidity using approximation of water
!     adsorption isotherms on clay minerals by Berge, H.F.M. ten, 1990

      use hydro_data_struct_defs, only: gravconst, molewater, potwilt, rgas, zerokelvin

!*** Argument declarations ***
      real theta, thetaw, theta80rh, soiltemp, matricpot
!     theta      - present volumetric water content
!     thetaw     - volumetric water content at wilt (15 bar or 1.5 MPa)
!     theta80rh  - volumetric water content at %80 relative humidity (300 bar or 30 MPa)
!     soiltemp   - soil temperature (C)
!     matricpot  - matric potential (meters of water) corresponding to theta
!                  only used if theta greater than thetaw

!*** local declarations ***
      real relhumwilt, mintheta
      parameter(mintheta = 1.0e-37)

      if( theta .le. mintheta ) then
          soilrelhum = 0.8*mintheta/theta80rh
      else if( theta .lt. theta80rh ) then
          soilrelhum = 0.8*theta/theta80rh
      else if( theta .le. thetaw ) then
!         find the relative humidity corresponding to thetaw (15 bar)
          relhumwilt = exp( (potwilt * molewater * gravconst)           &
     &               / (rgas * (soiltemp + zerokelvin)) )
          soilrelhum = 0.8+(relhumwilt - 0.8)                           &
     &               * ( (theta-theta80rh)/(thetaw-theta80rh) )
      else if( matricpot.le.0.0) then
          soilrelhum = exp( (matricpot * molewater * gravconst)         &
     &               / (rgas * (soiltemp + zerokelvin)) )
      else
          soilrelhum = 1.0
      endif
      return
    end function soilrelhum

    real function volwatadsorb(bulkden, clayfrac, orgfrac,            &
     &                             claygrav80rh, orggrav80rh )
!     computes the volumetric water content of the soil at 80 percent
!     relative humidity based on basic soil properties and the clay
!     adsorption isotherms on clay minerals by Berge, H.F.M. ten, 1990
!     with the addition of the organic matter isotherm from Rutherford
!     and Chlou, 1992

      use hydro_data_struct_defs, only: denwat

!*** Argument declarations ***
      real bulkden, clayfrac, orgfrac, claygrav80rh, orggrav80rh
!     bulkden        - bulk density of the soil (kg/m^3)
!     clayfrac       - fraction of the mineral soil which is clay (kg/kg)
!     orgfrac        - fraction of the total soil which is organic (kg/kg)
!     claygrav80rh   - Gravimetric water content of clay at 0.8 relative
!                      humidity (parameter A in reference)
!     orggrav80rh    - Gravimetric water content of organics at 0.8 relative
!                      humidity (parameter A in reference)

      volwatadsorb = (bulkden  / denwat)                                &
     &             * ( clayfrac * (1-orgfrac)*claygrav80rh              &
     &             + orgfrac * orggrav80rh )
      return
    end function volwatadsorb

!     not used but retained if needed

!      real function volwat_rh( relhum, theta80rh, thetaw, soiltemp )
!     returns the volumetric water content of the soil based on
!     the relative humidity using the approximation of water adsorption
!     isotherms on clay minerals by Berge, H.F.M. ten, 1990

!*** Argument declarations ***
!      real relhum, theta80rh, thetaw, soiltemp
!     theta      - present volumetric water content
!     thetaw     - volumetric water content at wilt (15 bar or 1.5 MPa)
!     theta80rh  - volumetric water content of soil at 0.8 relative humidity
!     soiltemp   - soil temperature (C)

!*** local declarations ***
!      real relhumwilt

!      if( relhum .le. 0.8 ) then
!          volwat_rh = theta80rh*relhum/0.8
!      else if( relhum .lt. 1.0 ) then
!          relhumwilt = exp( (potwilt * molewater * gravconst)
!     &               / (rgas * (soiltemp + zerokelvin)) )
!          if( relhum.lt.relhumwilt ) then
!              volwat_rh = theta80rh+(thetaw-theta80rh)
!     &                  * (relhum-0.8)/(relhumwilt-0.8)
!          else
!              volwat_rh = thetaw
!          endif
!      endif
!      return
!     end

    real function matricpot_from_rh( soilrh, soiltemp )

!     returns: matricpot
!     returns the matric potential in meters of water as defined by the 
!     clay and organic matter adsorption isotherms.

      use hydro_data_struct_defs, only: gravconst, molewater, rgas, zerokelvin

!*** Argument declarations ***
      real  soilrh, soiltemp
!     soilrh     - relative humidity of soil air (fraction)
!     soiltemp   - soil temperature (C)

      matricpot_from_rh = rgas*(soiltemp+zerokelvin)*(log(soilrh))      &
     &                  / (molewater * gravconst)

      return
    end function matricpot_from_rh

    real function acplwu (awcr, awcr_crit, wup)

!     + + + PURPOSE + + +

!     acplwu - Actual water use rate from soil layer 
!              same units as wup

!     + + + ARGUMENT DECLARATIONS + + +

      real awcr
      real awcr_crit
      real wup

!     + + + ARGUMENT DEFINITIONS + + +

!     awcr   - Relative soil water availability, fraction (0-1.0)
!     awcr_crit	- soil water availability ratio below which
!                 plant transpiration is reduced 
!     wup    - Potential water use rate from soil layer (mm/day)

      real str_fac
      parameter( str_fac = 100 )

!     + + + END SPECIFICATIONS + + +

!      if (awcr .ge. awcr_crit) then
!         acplwu = wup
!      else if (awcr .gt. 0.0) then
!         acplwu = wup * awcr/awcr_crit
!      else
!         acplwu = 0.0
!      endif

      if (awcr .ge. 1.0) then
         acplwu = wup
      else if (awcr .gt. 0.0) then
         acplwu = wup * log10( str_fac + 1.0 - str_fac*(1.0-awcr) )     &
     &          / log10( str_fac + 1.0 )
      else
         acplwu = 0.0
      endif

    end function acplwu

    real function availwc (theta, thetaw, thetaf)

!     + + + PURPOSE + + +

!     availwc - Available water content ratio (mm/mm)
!     returns a linear function from thetaw=0 to thetaf=1

!     + + + ARGUMENT DECLARATIONS + + +

      real theta, thetaw, thetaf

!     + + + ARGUMENT DEFINITIONS + + +

!     theta  - actual water content (mm/mm)
!     thetaw - water content at wilting point (mm/mm)
!     thetaf - water content at field capacity (mm/mm)

!     + + + END SPECIFICATIONS + + +

      if (theta .le. thetaw) then
         ! can't be negative value
         availwc = 0.0
      else if (theta .ge. thetaf) then
         ! can't be greater than 1
         availwc = 1.0
      else
         availwc = (theta - thetaw) / (thetaf - thetaw)
      endif

    end function availwc

    real function depstore( ranrough, soilslope, bhzoutflow )

!     + + + PURPOSE + + +
!     returns the maximum depression storage depth (m)
!     equation from WEPP 4.3.4 reference to 

!     + + + KEY WORDS + + +
!     hydrology

!     + + + ARGUMENT DECLARATIONS + + +
      real ranrough, soilslope, bhzoutflow

!     + + +  ARGUMENT DEFINITIONS + + +
!     ranrough   - random roughness of soil surface (m)
!     soilslope  - slope of soil surface (m/m)
!     bhzoutflow - height of runoff outlet above field surface (m)

!     + + + LOCAL VARIABLES + + +
      real coef_a, coef_b, coef_c
      parameter( coef_a = 0.112 )
      parameter( coef_b = 3.1 )
      parameter( coef_c = -1.2 )

      depstore = max( bhzoutflow, ranrough                              &
     &         * (coef_a + coef_b * ranrough + coef_c * soilslope))

      return
    end function depstore

    real function unsatcond_bc(theta, thetar, thetas, ksat, lambda)
!     returns the unsaturated hydraulic conductivity in same units as ksat as 
!     defined by the Books and Corey function and the Mualem conductivity model

!*** Argument declarations ***
      real  theta, thetar, thetas, ksat, lambda

!     theta      - present volumetric water content
!     thetar     - volumetric water content where hydraulic conductivity becomes zero
!     thetas     - saturated volumetric water content
!     ksat       - Saturated hydraulic conductivity (L/T) (modify to set units returned)
!     lambda     - Brooks adn Corey pore size interaction parameter 


!*** Local variable declarations ***
      real  satrat, minsatrat
      parameter( minsatrat = 1.0e-2 )
!     satrat     - conductivity relative saturation ratio
!     minsatrat  - used to clamp unsatcond_bc to zero and prevent underflow

      satrat = min(1.0,(theta-thetar)/(thetas-thetar))
      if( satrat.lt.minsatrat ) then
          unsatcond_bc = 0.0
      else
          unsatcond_bc = ksat*satrat**(2.5+2.0/lambda)
      endif
      return
    end function unsatcond_bc

    real function volwat_matpot_bc(matricpot,thetar,thetas,           &
     &                                 airentry,lambda)
!     computes the volumetric water content at a matric potential

!*** Argument declarations ***
      real matricpot, thetar, thetas, airentry, lambda
!     matricpot  - soil water matric potential (pressure units of airentry)
!     thetar     - volumetric water content where hydraulic conductivity becomes zero
!     thetas     - saturated volumetric water content
!     airentry   - Van Genuchten parameter (1/pressure) (modify to set units returned)
!     lambda     - Brooks adn Corey pore size interaction parameter 

!*** Local variable declarations ***
      real  satrat
!     satrat     - conductivity relative saturation ratio

      satrat = (airentry/matricpot)**lambda
      volwat_matpot_bc = (thetas-thetar)*satrat + thetar
      return
    end function volwat_matpot_bc

end module hydro_util_mod

