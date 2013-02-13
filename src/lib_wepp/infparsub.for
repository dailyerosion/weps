!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine infparsub( nsl, ssc, sscv, dg, cec1, st, ul, frzw,     &
     &                      avclay, avsand, avbdin, avporin, avrocvol,  &
     &                      avsatin, rescov, cancov, canhgt,            &
     &                      rrc, dsnow, prcp, rkecum, bcdayap,          &
     &                      ks, sm, frdp )

!     + + + ARGUMENT DECLARATIONS + + +
      integer, intent(in) :: nsl
      real, intent(in) :: ssc(*), sscv(*), dg(*), cec1(*), st(*), ul(*)
      real, intent(in) :: avclay, avsand, avbdin, avporin, avrocvol
      real, intent(in) :: avsatin, rescov, cancov, canhgt
      real, intent(in) :: rrc, dsnow, prcp, rkecum
      integer, intent(in) :: bcdayap
      real, intent(inout) :: ks, sm
      real, intent(in) :: frzw(*),frdp

!     temporary declarations
      integer lanuse, ksflag

!     + + + ARGUMENT DEFINITIONS + + +
!     nsl    - number of soil layers (of interest, ie surface layers)
!     ssc    - soil saturated hydraulic conductivity by layer (m/s)
!     dg     - thickness of individual soil layers (m)
!     cec1   - cation exchange capacity (meq/100g)
!     st     - current available water content per soil layer (m)
!     ul     - upper limit of water content per soil layer (m)
!     avclay - average clay content of surface layers (mineral fraction)
!     avsand - average sand content of surface layers (mineral fraction)
!     avbdin   - average bulk density of surface layers (kg/m^3)
!     avporin  - average porosity of surface layers (m^3/m^3)
!     avrocvol - average rock fragment content of surface layers (m^3/m^3)
!     avsatin  - average water content of surface layers (m^3/m^3)
!     rescov - residue cover (0-1)
!     cancov - canopy cover (0-1, unitless)
!     canhgt - canopy height (m)
!     rrc    - random roughness (m)
!     dsnow  - depth of snow on the soil surface (m)
!     prcp   - precipitation including all irrigation water applied above the canopy (m)
!     rkecum - cumulative kinetic energy since last tillage (J/m2)
!     bcdayap - number of days of growth completed since crop planted
!     ks     - effective saturated hydraulic conductivity (m/s)
!     sm     - effective soil matic potential (m)

!     + + + PURPOSE + + +
!     Calculates effective sat. hydraulic conductivity and effective
!     matric potential for Green Ampt infiltration from :
!      1) bare soil hyd. cond.
!      2) avg. potential across wetting front
!      3) effective porosity
!      4) percent ground cover
!      5) percent canopy cover
!      6) relative effective saturation

!     Called from SOIL
!     Author(s): Savabi,Risse,Zhang
!     Reference in User Guide: Chapter 4

! NOTE: Computation of the fraction of soil surface covered by both
!       canopy and ground cover (COVU) assumes location of ground
!       cover and canopy cover are independent.  Reza Savabi says
!       this is correct.  I would expect the location of the residue
!       to be somewhat dependent on the location of the plants that
!       generated it.  -- CRM (9/14/92 conversation with R. Savabi)

!     Changes:
!           1) Common block SOLVAR was not used.  It was de-referenced.
!           2) The generic "SAVE", which saves ALL local variables,
!              was eliminated.
!           3) Eliminated local variables TOTADJ, IPLUG, & NCOUNT.
!           4) Introduced intermediate local variables TMPVR1
!              to TMPVR4 to make calculations more efficient.
!           5) The statement:
!                   if (wetfrt.eq.tc) wetfrt=tc+.00001
!              was changed to:
!                   if(abs(wetfrt-tc) .lt. 0.00001) wetfrt=tc+.00001
!           6) Local variable SAT11 was computed, but the result was
!              never used.  It was eliminated.
!           7) Added local variable RFCUMX so INFPAR could tell if
!              rainfall had occurred.  In my test data sets, this
!              eliminated about 80% of the executions of the code for
!              soil crusting adjustments.
!           8) Changed statement at end of subroutine to prevent a
!              divide by zero occurring in SR ROCHEK - prevents the
!              value of sm from becoming zero.  dcf  8/16/93
!           9) Moved RFCUMX to common block cifpar.inc jca2 8/31/93
!          10) Added new Ksat adjustments from Risse and Zhang
!              dcf  1/11/94
!          11) Changes made to Ksat adjustments of Risse and
!              Zhang -  dcf     2/4/94
!          12) Changes made to Ksat adjustment equations again
!              by Nearing - dcf  3/8/94
!          13) Change to Ksat adjustment equations for established
!              perennials from John Zhang - dcf  5/26/94
!          14) Change to Ksat adjustment equations for surface
!              cover(both residue and canopy) adjustments to
!              hydraulic conductivity and also for first year
!              of perennial growth.   dcf  12/14/94
!          15) Change to exclude canopy cover factor in the
!              adjustment for hydraulic conductivity for the case
!              of furrow irrigation water addition.  dcf  12/14/94
!          16) Change statement which checks for water content to
!              be above upper limit so that saturation can occur for
!              a single storm simulation.  savabi and dcf  4/95
!               FROM:     if (st(i).ge.ul(i)) then
!                 TO:     if (st(i).ge.0.95*ul(i)) then

!     Version: This module recoded from WEPP Version 92.25.
!     Date recoded: 09/03/92 & 10/20/92.
!     Recoded by: Charles R. Meyer.

!     + + + LOCAL VARIABLES + + +
      integer idx
      real avbd, avpor, avsat, avcpm
      real tmpvr2, tmpvr3, tmpvr4
      real solthk
      real avks, sf, wetfrt, a, rra, bbbb, tc, crust, ffi
      real eke, sc, cke, crstad, kbare, ktmp, ccovef, scovef, kcov
      real dtheta,fzul

!     + + + LOCAL DEFINITIONS + + +
!     idx    - array index
!     avbd   - locally adjusted average bulk density of surface layers (kg/m^3)
!     avpor  - locally adjusted average porosity of surface layers (m^3/m^3)
!     avsat  - locally adjusted average water content of surface layers (m^3/m^3)
!     avcpm  - rock fragment correction factor
!     tmpvr2-4 - temporary variables to hold intermediate calculations for multiple reuse
!     solthk - depth from surface to bottom of indexed soil layers (m)
!     avsm15 - average 1500 KPa (15 bar) soil water content
!     avks   - average saturated hydralic conductivity for the tillage
!              layer
!     sf     - matric potential across wetting front (m)
!     wetfrt - average depth of wetting front (m)
!     cf     - canopy cover adjustment for saturated hydraulic
!              conductivity
!     a      - macroporosity adjustment for saturated hydraulic
!              conductivity
!     bareu  - bare area under canopy (fraction)
!     bareo  - bare area outside canopy (fraction)
!     covu   - ground cover under canopy (fraction)
!     covo   - ground cover outside canopy (fraction)
!     tc     - crust thickness
!     crust  - crust adjustment for saturated hydraulic conductivity
!     eke    - effective hydraulic conductivity in fill layer
!              (m/sec)
!     sc     - reduction factor for subcrust hydraulic conductivity
!     avcpm  - average rock fragment correction factor for the tillage
!              layer
!     grdcov - ground cover value used in macroporosity calculations
!              for cropland annuals - assigned a value on date of
!              last planting.  For perennials and range - use actual
!              cover values
!     cke    - coefficient relating amount of kinetic energy since last
!              tillage to the speed of crust formation
!     rtmt   - total dead and live root mass in top 15 cm of soil
!              (kg/m**2) (NOT USED)
!c    rtmtef - transformed (live plus dead) root mass
!     ccovef - effective canopy cover corrected for the effect of
!              canopy height
!     scovef - effective total surface cover
!     ktmp   - same as AVKS, only in units of (mm/hr)
!     kbare  - effective AVKS after adjustment for crusting/tillage
!     kcov   - portion of equation to compute effective AVKS
!              (surface cover adjustment)

!     + + + DATA INITIALIZATIONS + + +

!     + + + END SPECIFICATIONS + + +

!      The layers assumed to affect infiltration are the
!      primary (deepest), and [the average of] the secondary tillage
!      layers.)

      lanuse = 1
      ksflag = 1

!     Range checks:
      avpor = min( 1.0, max( 0.0, avporin ) )
      avbd = min( 2200.0, max( 800.0, avbdin ) )
! ---- Calculate average water content in tillage layer (AVSAT)
!      for the infiltration routine.  (ST is constant > 15 bars)
!     avsm15 - average 15 bar water content of surface layers (m^3/m^3)
!      avsat = (st(1)+st(2)) / tillay + avsm15
      avsat = min( avpor*0.98, avsatin )
      avcpm = 1.0 - avrocvol

! ---- Calculate the harmonic mean of Ks in the tillage layer (AVKS)
!      for the infiltration routine.
      ! modified to use multiple layers in place of primary, secondary tillage layers
      avks = ssc(1)
      solthk = dg(1)
      do idx = 2, nsl
          tmpvr2 = solthk + dg(idx)
          avks = tmpvr2 / (solthk/avks + dg(idx)/ssc(idx))
          solthk = tmpvr2 ! update thickness for next step
      end do

! ---- Compute the matric potential of the infiltration zone (SF)
!     (WEPP Equation 4.3.2 ff)
! -- XXX -- This equation needs a *number* in the User Document!
!     (See top of p 4.5) -- CRM -- 9/14/92.
      tmpvr2 = avclay ** 2
      tmpvr3 = avsand ** 2
      tmpvr4 = avpor ** 2

      sf = 0.01 * exp( 6.531 - 7.33*avpor + 15.8*tmpvr2 + 3.81*tmpvr4   &
     &   + avsand * (3.4*avclay - 4.98*avpor)                           &
     &   + tmpvr4 * (16.1*tmpvr3 + 16.0*tmpvr2) - 14.0*tmpvr3*avclay    &
     &   - avpor * (34.8*tmpvr2 + 8.0*tmpvr3) )
      if (sf.gt.0.5) sf = 0.5

!     *** L0 IF ***
!     CROPLAND
      if (lanuse.eq.1) then

! ------ Compute average depth of the wetting front (meters)
!       (WEPP Equation 4.3.11)
        wetfrt = 0.147 - 0.15 * tmpvr3 - (0.0003 * avclay * avbd)
        if (wetfrt.lt.0.005) wetfrt = 0.005

! ------ crust thickness
        tc = 0.005

!     *** L0 ELSE-IF ***
!     RANGELAND
      else if (lanuse.eq.2) then

! ------ average wetting front depth (meters)
!       (WEPP Equation 4.3.11)
        wetfrt = 0.147 - 0.15 * tmpvr3 - (0.0003 * avclay * avbd)
        if (wetfrt.lt.0.01) wetfrt = 0.01

! ------ crust thickness
        tc = 0.01

! ------ canopy cover adjustment factor for sat. hydraulic conductivity
!       (WEPP Equation 4.3.12)
!       cf = 1.0 + cancov

! ------ macroporosity adjustment factor for sat. hydraulic conductivity
        a = exp(6.10-(10.3*avsand)-(3.7*avclay))
        if (a.lt.1.0) a = 1.0
        if (a.gt.10.0) a = 10.0

!     *** L0 ELSE ***
!     FOREST
      else

!     (This branch left intentionally blank.)

!     *** L0 ENDIF ***
      end if


!     *** M0 IF ***
!     If rainfall or tillage has occurred, compute saturated
!     hydraulic conductivity adjustment for crusted soil surface.
!     if (rfcum.ne.rfcumx) then

! ------ correction factor for partial saturation of the sub-crust layer
!     (WEPP Equation 4.3.8)
      sc = 0.736 + (0.19*avsand)

! ------ compute maximum potential crust adjustment fraction
!     (WEPP Equation 4.3.7)
      if (abs(wetfrt-tc).lt.0.00001) wetfrt = tc + .00001

!     Use new equation for forming crust - see Rawls et al. 1989
!     Note: This is now maximum adjustment
      ffi = 45.19 - (46.68*sc)
      crust = sc / (1.0+(ffi/(wetfrt*100)))
      if (crust.lt.0.20) crust = 0.20

!     ************************************************************
!     * Note: CRUST is multiplied times Ksat.  When there is no  *
!     *       crust, Ksat is not adjusted; ie, CRUST = 1.  CRUST *
!     *       is computed in 2 parts.  The second part is the    *
!     *       adjustment for cumulative rainfall since tillage.  *
!     *       It is performed ONLY if RF since tillage is less   *
!     *       than 0.1 m                                         *
!     ************************************************************

!-----for CROPLAND, if cumulative RF since tillage is less than
!     1/10 meter, update crust reduction factor for rainfall.
!     New equation inserted Risse
!     crust adjustment=maxadj+(1-maxadj) exp(-C kecum (1-rr/4))
!     where maxadj is the same as it was in previous version with
!     correction for units(crust), C is calculated based on analysis
!     by Risse, and instead of using a linear relationship between 0
!     and 100mm of rfcum we used the exponential relationship of
!     Brakensiek and Rawls, 1983
!     (WEPP Equation 4.3.10)
!     if(lanuse.eq.1) then
!     Original Code:
!     1                            crust=1.-(((1.-crust)/0.1)*rfcum)
!     1                            crust = 1.0 - (1.0-crust)*10.0*rfcum
!     cke is coeffient relating to speed with which crust forms
!     based on analysis of natural runoff plot data by Risse

!-----for CROPLAND, if cumulative RF since tillage is less than
!     1/10 meter, update crust reduction factor for rainfall.

      if (lanuse.eq.1) then
        cke = -0.0028 + 0.0113 * avsand + 0.125 * avclay / cec1(1)
        if (cke.gt.0.01) cke = 0.01
        if (cke.lt.0.0001) cke = 0.0001
!       make sure random roughness does not cause positive exponent
        if (rrc.le.0.04) then
          rra = rrc
        else
          rra = 0.04
        end if

        bbbb = -cke * rkecum * (1-rra/0.04)
        if (bbbb.lt.-25.0) bbbb = -25.0
!       Calculate crusting/tillage adjustment
        crstad = crust + (1-crust) * exp(bbbb)
      else
        crstad = 1.0
      end if

!     *** M0 ENDIF ***
!     endif

!     Adjust AVKS for soil surface characteristics (canopy & cover)
!     and for dead and live roots.    (from zhang 1/94)   dcf

        kbare = avks * crstad

!     EXCLUDE the adjustment for canopy cover for the case of
!     snow melt or furrow irrigation.   dcf  12/15/94

      if( (dsnow .gt. 0.001) .or. (prcp .lt. 0.001) )then
        ! snow shields the surface, or no water, no canopy effect
        scovef = rescov
      else
        ! Adjust the effectiveness of canopy cover by canopy height
        ccovef = cancov * exp(-0.3358*canhgt/2.0)

        ! Calculate the total effective surface cover
        scovef = ccovef + rescov - ccovef * rescov
      endif

!       Calculate the final effective conductivity

!     IF the user has indicated that he/she wants the internal
!     Ksat adjustments used in the SOIL input file - then
!     adjust final effective conductivity for crusting/tillage/
!     crop/rainfall                dcf  1/11/94

      if (ksflag.eq.1) then

        if(dsnow .lt. 0.001)then

! XXX     NOTE - We should really add in the amount of sprinkler
! XXX            irrigation water here (if any exists for the day)
! XXX            but unfortunately at this point in the program we
! XXX            do not yet know what this amount will be (have not
! XXX            call subroutine IRRIG yet).    dcf  12/15/94

          ! sprinkler water included in input for WEPS
          ! convert from m/sec to mm/hr
          ktmp = avks * 3.6e6
          ! this equation (7.9.12) is specified with ktmp in mm/h and prcp*1000 yields rain in mm
          kcov = (0.0534 + 0.01179*ktmp) * prcp * 1000.0 / 3.6e6
        else
          kcov = 0.0
        endif

!       Zhang change 12/9/94
!       if(kcov .lt. avks)kcov = avks
        if(kcov .lt. 0.5*avks)kcov = 0.5*avks

        eke = kbare*(1.0 - scovef) + kcov*scovef

!       If crop adjusted eke is smaller than that of crust adjusted
!       set it back to crust adjusted value

        if (eke.lt.kbare) eke = kbare


!       ADJUST FOR ESTABLISHED PERENNIAL CROP (meadows, etc.)

      ! use day after planting as a substitute for land use flag and
      ! rootmass to maximum root mass ratio. (empericism at it's best)

! Note - Changed coefficient in equation below from 1.7965 to 1.81
!        when given change from Zhang.   7/1/94   dcf

        if( bcdayap .ge. 270 )then

!         Changes to include perennial adjustment for first
!         year of perennial growth when plant is sufficiently
!         developed.   dcf  12/9/94

            if( bcdayap .ge. 365)then
                ! plant in place for a full year, call it a developed perennial
                eke = 1.81 * eke
            else
                ! increase adjustment linearly as full development sets in
                eke = eke * (1.0 + 0.81 *((bcdayap-270)/(365-270)))
            endif
        endif
      else
        eke = avks
      end if

!d    Modified by S. Dun 06/20/2002
!     LIMIT MINIMUM KSAT TO 1.94E-08 m/s (0.07 mm/h)

      if (eke.le.1.94e-08) eke = 1.94e-08
!d    adjust the lower limit to e-14 m/s 
!d    (the reference we are using is "Physical and Chemical Hydrogeology"
!d     by P.A. Domenico and F.W. Schwartz)
!l      if (eke.le.1.0e-14) eke = 1.0e-14
!d    end modifying.


!     *** BEGIN N0 LOOP ***

!     In case a restricted soil layer controls percolation and
!     infiltration....

      idx = 0
   20 continue
  
!
!d    Modified by S. Dun, April 17, 2008
!      for frozen soil effect   
      idx = idx + 1
      fzul =ul(idx) - frzw(idx)
! ---- If the water content is above the upper limit for this layer....
      if (st(idx).ge.0.95*fzul) then
! ------ If this layer's Ksat is less than the average Ksat for the
!        plow layer ....
        if (ssc(idx).le.eke) eke = ssc(idx)
!
!       Frost check - jrf 2/20/2009        
        if ((frdp .gt. 0.0).and. (sscv(idx).gt.0.0)                     &
     &     .and. (sscv(idx).le.eke) ) then
            eke = sscv(idx)
        endif
      else
! ------ (force exit from loop)
        idx = nsl
      end if
!     *** END N0 LOOP ***
      if (idx.lt.nsl) go to 20

      ks = eke

!     Compute effective matric potential (SM), correcting for rock
!     fragments (using AVCPM).

      if (avsat.ge.(avpor*avcpm)) avsat = (avpor*avcpm) * 0.99
! ---- compute water above field capacity
      dtheta = avpor * avcpm - avsat
! ---- compute effective matric potential (SM)
      sm = dtheta * sf

      return
      end
