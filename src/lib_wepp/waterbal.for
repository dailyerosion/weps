!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine waterbal(layrsn, thetas, thetes, thetaf, thetaw,       &
     &                   bszlyt, bszlyd, satcond,                       &
     &                   dprecip, bwdurpt, bwpeaktpt, bwpeakipt,        &
     &                   dirrig, bhdurirr, bhlocirr, bhzoutflow,        &
     &                   bhzsno, bslrr, bmrslp, bsfsan, bsfcla,         &
     &                   bsvroc, bsdblk, bsfcec,                        &
     &                   bbffcv, bbfcancov, bbzht, bcdayap,             &
     &                   bhzep, theta, thetadmx, bhrwc0,                &
     &                   bhzea, bhzper, bhzrun, bhzinf, bhzwid,         &
     &                   rkecum )

!     + + + PURPOSE + + +
!     Implements soil water balance using routines from WEPP

!     + + + ARGUMENT DECLARATIONS + + +
      integer, intent(in) :: layrsn
      real, intent(in) :: thetas(*), thetes(*), thetaf(*), thetaw(*)
      real, intent(in) :: bszlyt(*), bszlyd(*), satcond(*)
      real, intent(in) :: dprecip, bwdurpt, bwpeaktpt, bwpeakipt
      real, intent(in) :: dirrig, bhdurirr, bhlocirr, bhzoutflow
      real, intent(in) :: bhzsno, bslrr, bmrslp, bsfsan(*), bsfcla(*)
      real, intent(in) :: bsvroc(*), bsdblk(*), bsfcec(*)
      real, intent(in) :: bbffcv, bbfcancov, bbzht
      integer, intent(in) :: bcdayap
      real, intent(in) :: bhzep
      real, intent(inout) :: theta(0:*), thetadmx(*), bhrwc0(*)
      real, intent(inout) :: bhzea, bhzper, bhzrun, bhzinf, bhzwid
      real, intent(inout) :: rkecum

!     + + + ARGUMENT DEFINITIONS + + +
!     layrsn - number of soil layers
!     thetas - saturated volumetric water content (= porosity)
!     thetes - reduced saturated volumetric water content
!     thetaf - field capacity volumetric water content
!     thetaw - wilting point volumetric water content
!     bszlyt - soil layer thinkness array (mm)
!     bszlyd   - Depth to bottom of soil layers (mm)
!     satcond - saturated hydraulic conductivity (m/s)
!     dprecip  - rainfall depth after snow filter (mm)
!     bwdurpt  - Duration of Daily precipitation (hours)
!     bwpeaktpt - Normalized time to peak of Daily precipitation (time to peak/duration)
!     bwpeakipt - Normalized intensity of peak Daily precipitation (peak intensity/average intensity)
!     dirrig   - Daily irrigation (mm)
!     bhdurirr - duration of irrigation water application (hours)
!     bhlocirr - emitter location point (mm)
!                positive is above the soil surface
!                negative is below the soil surface
!     bhzoutflow - height of runoff outlet above field surface (m)
!     bhzsno   - depth of water in snow layer (mm)
!     bslrr    - Allmaras random roughness parameter (mm)
!     bmrslp   - Average slope of subregion (mm/mm)
!     bsfsan   - Fraction of soil mineral which is sand
!     bsfcla   - Fraction of soil mineral which is clay
!     bsvroc   - Soil layer coarse fragments, rock (m^3/m^3)
!     bsdblk   - soil bulk density (Mg/m^3)
!     bsfcec - Soil layer cation exchange capacity (cmol/kg) (meq/100g)
!     bbffcv - Biomass cover - flat  (m^2/m^2)
!     bbfcancov - Biomass canopy cover (m^2/m^2)
!     bbzht  - composite average residue height (m)
!     bcdayap - number of days of growth completed since crop planted
!     bhzep    - potential soil evaporation (mm/day)
!     theta(*) - present volumetric water content
!     thetadmx(*) - daily maximum volumetric water content (m^3/m^3)
!     bhrwc0(*)  - Hourly values of surface soil water content (not as in soil)
!     bhzea      - accumulated daily evaporation (mm) (comes in with a value set from snow evap)
!     bhzper     - accumulated daily drainage (deep percolation) (mm)
!     bhzrun     - accumulated daily runoff (mm)
!     bhzinf     - depth of water infiltrated (mm)
!     bhzwid     - Water infiltration depth into soil profile (mm)
!     rkecum     - cumulative kinetic energy since last tillage (J/m2)

!     + + + PARAMETERS + + +
      integer mxtime, mxpond, nr
      parameter (mxtime = 1000, mxpond = 1000, nr = 11)

      real mmtom, mtomm
      parameter (mmtom = 0.001, mtomm = 1000.0)

      real hrtosec, sectohr
      parameter (hrtosec = 3600.0, sectohr = 1.0/3600.0)

      real surflay, eflim, eflay
      parameter (surflay = 0.2, eflim = 0.2, eflay = 0.3)

      real weightsurf
      parameter (weightsurf = 2.0)

!     + + + PARAMETER DEFINITIONS + + +
!     mxtime - maximum array dimension
!     mxpond - maximum array dimension
!     nr - number of breakpoint intervals to be created (no less than three or disag croaks)
!     mmtom  - Unit conversion constant (m/mm)
!     mtomm  - Unit conversion constant (mm/m)
!     hrtosec - Unit conversion constant (seconds/hour)
!     sectohr - Unit conversion constant (hours/seconds)
!     surflay - the depth of soil considered to be the surface layer (m)
!               soil properties are averaged across this layer for
!               infiltration and evaporation
!     eflim - evaporation fraction (of wilting point moisture) limit (minimum water content)
!     eflay - extraction fraction for each layer (changed so that water content
!             less than readily available water not a factor)
!     weightsurf - summation of surface layer numbers, to form series that sums to 1

!     + + + LOCAL VARIABLES + + +
      integer idx, nrain, ninf, ns
      integer locidx, nextidx, evapidx
      integer hrpond, hrend
      real st(layrsn), ul(layrsn), fc(layrsn), hk(layrsn)
      real layth(layrsn), laydp(layrsn)
      real trf(nr), rf(nr), train(mxtime), rrain(mxtime)
      real precip, irrig, durpre, durirr, dsnow, prcp
      real dtinf, rr(mxtime), ranrough !, dsat
      real sand, clay, ks, sm, depsto, wcon !, wsat
      real avbulkd, avporos, avrockvol
      real tf(mxtime), rcum(mxtime), f(mxtime)
      real ff(mxtime), re(mxtime), recum(mxtime), tp(mxpond)
      real rprint(mxtime), ddepsto(mxtime)
      real seepage, runoff, durexr, effint, effdrr
      real xfin, epdp, parthk, potenevap
      real evap, evaprem, evapremprev, evaplay
      real tew(layrsn), rew(layrsn), wfevp(layrsn), evapfdp(layrsn)
      real nrew(layrsn)
      real tottew, totrew, totwfevp, dval
      real settle_seep, inf_seep, irr_seep
      integer nsl
      real surfcap, temp, laycap(layrsn)
!      real ksold, smold

!     + + + LOCAL DEFINITIONS + + +
!     idx - array indexing variable
!     nrain - number of breakpoint array values used to represent rainfall
!     ninf - number of times steps for infiltration
!     ns - number of element for end of runoff arrays
!     locidx - layer index for location of irrigation insertion
!     nextidx - reference layer index around locidx
!     evapidx - index of last layer in evaporative zone
!     hrpond - hour that ponding starts
!     hrend - hour that ponding ends
!     st - current available water content per soil layer (m)
!     ul - upper limit of water content per soil layer (m)
!     fc - soil field capacity (m)
!     hk - a parameter that causes SC approach zero as soil water approaches FC
!     layth - layer thickness (meters)
!     laydp - depth to bottom of soil layer (m)
!     trf - rainfall breakpoint time array (seconds)
!     rf - rainfall rate array (m/sec)
!     train - (tr) rainfall time array with infiltration time step inserted
!     rrain - (r) rainfall rate array with infiltration time step inserted
!     precip - total storm precipitation amount (m)
!     irrig - total irrigation application amount (m)
!     durpre - duration of storm (seconds)
!     durirr - duration of irrigation (seconds)
!     dsnow - depth of snow on surface (m)
!     prcp  - depth of rain and irrigation above the canopy (m)
!     dtinf - time step for infiltration (seconds)
!     rr - cumulative depth array of rainfall (m)
!     ranrough - soil random roughness (m)
!     dsat - degree of saturation for soil for infiltration
!     sand - effective soil sand content (fraction)
!     clay - effective soil clay content (fraction)
!     ks - effective saturated hydraulic conductivity (m/s)
!     sm - effective soil matic potential (m)
!     depsto - effective depression storage (m)
!     wsat - surface layers average saturated water content (m^3/m^3)
!     wcon - actual water content (m^3/m^3)
!     avbulkd - effective soil bulk density (kg/m^3)
!     avporos - surface layers average porosity (m^3/m^3)
!     avrockvol - surface layers average rock fragment content (m^3/m^3)
!     tf      - time values in runoff array (T)
!     rcum    - accumulated rainfall depth (L)
!     f       - infiltration rate (L/T)
!     ff      - accumulated infiltration depth (L)
!     re      - rainfall excess rate (L/T)
!     recum   - accumulated rainfall excess depth (L)
!     tp      - time of ponding (T)
!     rprint  -
!     ddepsto - depth of depression storage at infiltration timestep (L)
!     seepage - seepage depth (m)
!     runoff  - runoff depth (m)
!     durexr  - duration of rainfall excess (sec)
!     effint  - effective rainfall intensity
!     effdrr  - effective rainfall duration
!     xfin    - infiltration amount to be inserted into a soil layer
!     epdp    - depth of evaporation effect based on soil texture
!     parthk  - thickness of partial soil layer
!     potenevap - local value of potential total evaporation
!     evap    - actual evaporation based on water available in evap zone
!     evaprem - evaporation left to be removed from the evap zone
!     evapremprev - set reference value for convergence check
!     evaplay - evaporation to be removed from the layer
!     tew     - by layer Total Evaporable Water
!     rew     - by layer Readily Evaporable Water
!     wfevp   - by layer Water for Evaporation
!     evapfdp - depth factor, reduces fraction of remaining water for
!               evaporation removed as evaporation goes deeper
!     nrew    - (tew-rew) non readily evaporable water
!     tottew  - Total Evaporable Water in total evaporable depth
!     totrew  - Readily Evaporable Water in total evaporable depth
!     totwfevp -  Water for Evaporation in total evaporable depth
!     dval    - delta value for linear interpolation and
!               used as temporary value in soil water content adjustement
!     settle_seep - depth of water found in excess of saturation from 
!               soil settling/consolidation outside of hydro. (m)
!     inf_seep - depth of infiltration water in excess of saturation for entire profile 
!     irr_seep - depth of subsurface irrigation water in excess of saturation for entire profile 
!     nsl     - number of soil layers considered to be "surface layers"
!     surfcap - available water holding capacity of "surface layer"
!     temp    - temporary variable
!     laycap  - available water holding capacity of individual layers

!     + + + SUBROUTINES CALLED + + +

!     + + + FUNCTION DECLARATIONS + + +
      real depstore, valbydepth, rainenergy

!     + + + DATA INITIALIZATIONS + + +
      dtinf = 180.0 !in seconds = three minutes)
      tf(1) = 0.0

!     + + + OUTPUT FORMATS + + +

!     + + + END SPECIFICATIONS + + +

      ! zero out seepage accumulators
      settle_seep = 0.0
      inf_seep = 0.0
      irr_seep = 0.0

      ! convert to WEPP style variables, check for seepage from settling
      do idx = 1, layrsn
          ! set values used in WEPP water balance routines
          ! layer thickness (m)
          layth(idx) = bszlyt(idx) * mmtom
          ! layer depth (m)
          laydp(idx) = bszlyd(idx) * mmtom
          ! present water content for current layer (m)
          st(idx) = (theta(idx) - thetaw(idx)) * layth(idx)
          ! upper limit of water content for current layer (m)
          ul(idx) = (thetes(idx) - thetaw(idx)) * layth(idx)
          ! field capacity for current layer (m)
          fc(idx) = (thetaf(idx) - thetaw(idx)) * layth(idx)
          ! Used in PERC to adjust sat. hyd. cond. on non-saturated soils.
          hk(idx) = -2.655 / alog10(fc(idx) / ul(idx))
          ! check for increase above saturation from settling outside hydro
          if( st(idx) .gt. ul(idx) ) then
               ! accumulate the excess water to be inserted lower in soil
               settle_seep = settle_seep + st(idx) - ul(idx)
               ! set layer water content to upper limit
               st(idx) = ul(idx)
               ! reset theta to adjusted value
               theta(idx) = ( st(idx)/layth(idx) ) + thetaw(idx)
          else if( st(idx) .lt. ul(idx)                                 &
     &     .and. (settle_seep .gt. 0.0) ) then
               ! capacity available and water available to be inserted
               dval = ul(idx) - st(idx)
               if( dval .ge. settle_seep ) then
                   ! insert all extra into this layer
                   st(idx) = st(idx) + settle_seep
                   settle_seep = 0.0
               else
                   ! insert some into this layer
                   st(idx) = ul(idx)
                   settle_seep = settle_seep - dval
               end if
               theta(idx) = ( st(idx)/layth(idx) ) + thetaw(idx)
          end if
          ! set daily maximum value to incoming theta value
          thetadmx(idx) = theta(idx)
      end do

      ! zero out surface water content array
      do idx = 1, 24
          bhrwc0(idx) = 0.0
      end do
      ! set first value of hourly surface water content from top layer
      bhrwc0(1) = theta(1)
    
      ! find representative texture values using surflay thickness
      sand = valbydepth(layrsn, laydp, bsfsan, 0, 0.0, surflay)
      clay = valbydepth(layrsn, laydp, bsfcla, 0, 0.0, surflay)

      ! input water to soil

      ! convert values to meters and seconds in preparation for call
      precip = dprecip * mmtom
      durpre = bwdurpt * hrtosec
      irrig = dirrig * mmtom
      durirr = bhdurirr * hrtosec

      if(    (precip .gt. 0.0)                                          &
     &  .or. ((bhlocirr .ge. 0.0) .and. (irrig .gt. 0.0)) ) then

          ! create breakpoint data from cligen storm input
          nrain = nr
          call disag(nrain,trf, rf, precip, durpre, bwpeaktpt,bwpeakipt)

          ! accumulate rainfall kinetic energy for today
          rkecum = rkecum + rainenergy(nrain, trf, rf)

          ! merge infiltration, rainfall and irrigation "arrays"
          ! returns infiltation array dimension
          if( bhlocirr .ge. 0.0 ) then
              ! add sprinkler or surface irrigation to water application array
              call arraymerge( nrain, dtinf, trf, rf, irrig, durirr,    &
     &                         ninf, train, rrain, rr)
          else
              ! assume that subsurface irrigation does not interact with infiltration directly
              call arraymerge( nrain, dtinf, trf, rf, 0.0, 0.0,         &
     &                         ninf, train, rrain, rr)
          end if

          ! estimate infiltration parameters
          ! depression storage
          ranrough = bslrr * mmtom 
          depsto = depstore(ranrough, bmrslp, bhzoutflow)
!          wsat = valbydepth(layrsn, laydp, thetes, 0, 0.0, surflay)
          wcon = valbydepth(layrsn, laydp, theta(1), 0, 0.0, surflay)
!          dsat = wcon / wsat
!       call parestsub(sand, clay, dsat, bbfcancov, bbffcv, ksold, smold)

          do idx = 1, layrsn
              ! find number of layers that are considered surface layers
              if( laydp(idx) .le. surflay ) then
                  ! leaves nsl at last surface layer value
                  nsl = idx
              else
                  ! no more surface layers
                  exit
              end if
          end do

          ! representative bulk density converted to kg/m^3
          avbulkd = 1000.0*valbydepth(layrsn,laydp,bsdblk,0,0.0,surflay)
          ! porosity is equivalent to saturated water content (not reduced saturation)
          avporos = valbydepth(layrsn, laydp, thetas, 0, 0.0, surflay)
          ! rock fragment volume
          avrockvol = valbydepth(layrsn, laydp, bsvroc, 0, 0.0, surflay)
          ! snow depth
          dsnow = bhzsno * mmtom
          ! water above the canopy
          if( bhlocirr * mmtom .ge. bbzht ) then
              ! irrigation is applied above the canopy
              prcp = precip + irrig
          else
              ! irrigation is below the canopy, do not include
              prcp = precip
          end if
          call infparsub( nsl, satcond, layth, bsfcec, st, ul,          &
     &                    clay, sand, avbulkd, avporos, avrockvol,      &
     &                    wcon, bbffcv, bbfcancov, bbzht,               &
     &                    ranrough, dsnow, prcp, rkecum, bcdayap,       &
     &                    ks, sm )

          ! call infiltration
          call grna( ninf, depsto, train, rrain, rr, ks, sm,            &
     &         ns, tf, rcum, f, ff, re, recum, tp,                      &
     &         rprint, ddepsto, runoff, durexr, effint, effdrr )

          ! set return values
          bhzrun = runoff * mtomm
          bhzinf = ff(ns) * mtomm

          ! set infiltration water amount
          xfin = ff(ns)

          ! insert infiltration water into soil
          if (xfin.gt.0.0) then
              ! check available storage in surface layers
              surfcap = 0.0
              do idx = 1, nsl
                  laycap(idx) = ul(idx) - st(idx)
                  surfcap = surfcap + laycap(idx)
              end do
              ! Starting at top, infiltrate water into each layer.
              do idx = 1, layrsn
                  ! check for layer being in surface layer and having capacity
                  if( (idx .le. nsl) .and. (surfcap .gt. 0.0) ) then
                      ! prorate infiltration water into surface layers
                      temp = xfin * laycap(idx) / surfcap               & ! weight by available capacity
     &                     * weightsurf                                   ! skew toward surface
                      temp = min( temp, xfin )                            ! can't put in more than ya' got
                      surfcap = surfcap - laycap(idx)                     ! adjust so next layer gets enough
                      st(idx) = st(idx) + temp                            ! adding the water to the layer
                      xfin = xfin - temp                                  ! infiltrated amount remaining for next layer
                  else
                      ! fill layer to capacity
                      st(idx) = st(idx) + xfin
                      xfin = 0.0
                  end if
                  if( st(idx) .gt. ul(idx) ) then
                      ! this is more water than this layer can hold
                      xfin = xfin + st(idx) - ul(idx)
                      st(idx) = ul(idx)
                  end if
                  if( xfin .le. 0.0 ) then
                      ! use bottom of this layer as infiltration depth
                      bhzwid = bszlyd(idx)
                      ! no more water to be inserted, stop looping
                      exit
                  end if
              end do
              ! check for excess water at bottom
              if( xfin .gt. 0.0 ) then
                  ! add excess to drainage
                  inf_seep = xfin
                  xfin = 0.0
                  ! use bottom of profile as infiltration depth
                  bhzwid = bszlyd(layrsn)
              end if
          end if
      else
          ! set return values
          bhzrun = 0.0
          bhzinf = 0.0
          bhzwid = 0.0

          ! set infiltration water amount
          xfin = 0.0
          tp(1) = 0.0
      end if

      ! check for subsurface irrigation
      if( (bhlocirr .lt. 0.0) .and. (irrig .gt. 0.0) ) then
          ! insert subsurface irrigation water into soil
          ! find insertion layer
          do idx = 1, layrsn
              if( -bhlocirr .le. bszlyd(idx) ) then
                  locidx = idx
                  exit
              end if
          end do
          ! insert water above and below insertion point
          xfin = irrig
          idx = locidx
          do while( (xfin .gt. 0.0)                                     &
     &        .and. (idx .le. layrsn) .and. (idx .ge. 1) )
              ! insert water into layer
              st(idx) = st(idx) + xfin
              if( st(idx) .gt. ul(idx) ) then
                  ! this is more water than this layer can hold
                  xfin = st(idx) - ul(idx)
                  st(idx) = ul(idx)
              else
                  ! all remaining water placed in this layer
                  xfin = 0.0
              end if
              ! select next insert layer, insert alternately below then above
              if( idx .eq. locidx ) then
                  ! do layer below first
                  nextidx = locidx + 1
              else if( idx .gt. locidx ) then
                  ! if below bounce to symetrical layer above
                  nextidx = locidx - (idx - locidx)
              else if( idx .lt. locidx ) then
                  ! if above bounce to symetrical layer below + 1
                  nextidx = locidx + (locidx - idx) + 1
              end if
              ! check for out of range and bounce to valid layer index
              if( nextidx .lt. 1 ) then
                  ! out the top of the soil, go lower
                  idx = idx + 1
              else if( nextidx .gt. layrsn ) then
                  ! out of bottom of the soil, go higher
                  idx = idx - 1
              else
                  idx = nextidx
              end if
          end do
          ! check for excess water at bottom
          if( xfin .gt. 0.0 ) then
              ! add excess to drainage
              irr_seep = xfin
              xfin = 0.0
              ! use bottom of profile as infiltration depth
              bhzwid = bszlyd(layrsn)
          end if
          
      end if

      ! check for daily maximum soil layer water content
      do idx = 1, layrsn
          theta(idx) = ( st(idx)/layth(idx) ) + thetaw(idx)
          thetadmx(idx) = max( thetadmx(idx), theta(idx) )
      end do

      ! set hourly value for surface water content
      if( tp(1) .gt. 0.0 ) then
          hrpond = max(1, min(24, int(tp(1) * sectohr)))
          hrend = max(1, min(24, int( (tp(1) + durexr) * sectohr )))
          do idx = hrpond, hrend
              bhrwc0(idx) = thetes(idx)
          end do
      else
          ! no ponding occured, use these values at end, and interpolate 1 to 24
          hrpond = 24
          hrend = 1
      end if

      ! redistribute water in soil
      call purk(layrsn, st, fc, ul, hk, satcond, seepage)

      bhzper = (settle_seep + inf_seep + irr_seep + seepage) * mtomm

      ! check for daily maximum soil layer water content
      do idx = 1, layrsn
          theta(idx) = ( st(idx)/layth(idx) ) + thetaw(idx)
          thetadmx(idx) = max( thetadmx(idx), theta(idx) )
      end do

      ! find soil surface evaporation
!    calculation of soil evaporation reduction coefficient 
!    In the TEW formula 0.1m is the depth of the surface soil layer 
!        that is subject to drying by way of evaporation 0.10~0.15m.
!    REW is a formula from linear regression of Table 19 (FAO56 p144)
!    Notes for previous day water depletion calculation:
!        st(mxnsl,mxplan) is available water content per soil layer(m) 
!        at the end of previous day. This value is from the difference 
!        between soil water content and wilting point soil water 
!        content. Because the available water for evaporation is from
!        the difference between water content and half of wilting 
!        point water content. So we add half of wilting point water
!        content to calculate available water for using st value. 
                        
      ! NOTE, this is the place to insert depth of disturbance (tillage)
      ! that breaks cappillary upflow.
      ! evaporable depth based on soil texture in top 15 cm (see above)
      epdp = 0.09 - 0.077 * clay + 0.0006 * sand
            
      ! sum Total Evaporable Water and Readily Evaporable Water in evaporable depth
      tottew = 0.0
      totrew = 0.0
      totwfevp = 0.0
      do idx = 1, layrsn
        if (laydp(idx).lt.epdp) then  
            tew(idx) = (thetaf(idx) - eflim * thetaw(idx)) * layth(idx)
            rew(idx) = ( 0.057856*(thetaf(idx)-thetaw(idx)) + 0.000280 )&
     &               * layth(idx) / epdp
            nrew(idx) = tew(idx) - rew(idx)
            wfevp(idx) = st(idx) + (1.0-eflim) * thetaw(idx) *layth(idx)
            tottew = tottew + tew(idx)
            totrew = totrew + rew(idx)
            totwfevp = totwfevp + wfevp(idx)
        else
            if( idx .eq. 1 ) then
                parthk = epdp
            else
                parthk = epdp - laydp(idx-1)
            end if
            tew(idx) = ( thetaf(idx) - eflim * thetaw(idx) ) * parthk
            rew(idx) = ( 0.057856*(thetaf(idx)-thetaw(idx)) + 0.000280 )&
     &               * parthk / epdp
            nrew(idx) = tew(idx) - rew(idx)
            wfevp(idx) = st(idx) * parthk / layth(idx) +                &
     &                   (1.0-eflim) * thetaw(idx) * parthk
            tottew = tottew + tew(idx)
            totrew = totrew + rew(idx)
            totwfevp = totwfevp + wfevp(idx)
            evapidx = idx
            exit
        endif
      end do

      ! find actual evaporation amount
      potenevap = bhzep * mmtom
      if( (tottew - totwfevp + potenevap) .le. totrew ) then
          ! all evaporative demand satisfied by Readily Available Water
          evap = potenevap
      else if( (tottew - totwfevp) .le. totrew ) then
          ! part of evaporative demand satisfied by Readily Available Water
          ! this is satisfied by readily available water
          evap = totrew - (tottew - totwfevp)
          ! potential is reduced by this amount
          potenevap = potenevap - evap
          ! the reaminder is evaporated at the reduced rate
          evap = evap + potenevap                                       &
     &         * ( (totwfevp-evap-potenevap/2.0) / (tottew-totrew) )**2
      else
          ! evaporative demand limited
          evap = potenevap*((totwfevp-potenevap/2.0)/(tottew-totrew))**2
      endif

      ! set evaporation factor by depth for evaporation layers
      evapfdp(1) = 1.0
      do idx = 2, evapidx
          evapfdp(idx) = 1.0 - laydp(idx-1) / epdp
      end do

      ! remove evaporation water from soil layers
      idx = 1
      evaprem = evap
      ! set reference value for convergence check
      evapremprev = evaprem
      do while( evaprem .gt. 0.0 )
!          if( wfevp(idx) .gt. nrew(idx) ) then
              ! remove readily available water from layer
!              evaplay = min( evaprem, wfevp(idx) - nrew(idx) )
!              st(idx) = st(idx) - evaplay
!              evaprem = evaprem - evaplay
!              wfevp(idx) = wfevp(idx) - evaplay
              ! restart at first layer
!              idx = 1
              ! set reference value for convergence check
!              evapremprev = evaprem
!          else
              ! remove an additional fraction from the layer
              evaplay = min( evaprem, eflay * evapfdp(idx) * wfevp(idx))
              st(idx) = st(idx) - evaplay
              evaprem = evaprem - evaplay
              wfevp(idx) = wfevp(idx) - evaplay
              ! cascade to the next layer
              idx = idx + 1
!          end if
          if( idx .gt. evapidx ) then
              ! all readily available water used up. Repeat fraction removal from layer one
              idx = 1
              if( evaprem .eq. evapremprev ) then
                  ! no more water available at all for evaporation (within machine precision)
                  exit
              end if
              ! set reference value for convergence check
              evapremprev = evaprem
          end if
      end do

      bhzea = ( evap - evaprem ) * mtomm

      ! revert water contents back to volumetric basic (m^3/m^3)
      do idx = 1, layrsn
          theta(idx) = ( st(idx)/layth(idx) ) + thetaw(idx)
          thetadmx(idx) = max( thetadmx(idx), theta(idx) )
      end do

      ! set last value of hourly surface water content
      bhrwc0(24) = theta(1)
      ! interpolate surface layer hourly water contents
      if( hrend .lt. hrpond ) then
          ! no ponding occured, interpolate hour 1 to 24
          dval = (bhrwc0(24) - bhrwc0(1)) / 23
          do idx = 2, 23
              bhrwc0(idx) = bhrwc0(idx-1) + dval
          end do
      else
          if( hrpond .gt. 2 ) then
              ! linearly interpolate from hour 1 to hrpond
              dval = (bhrwc0(hrpond) - bhrwc0(1)) / (hrpond-1)
              do idx = 2, hrpond-1
                  bhrwc0(idx) = bhrwc0(idx-1) + dval
              end do
          end if
          if( hrend .lt. 23 ) then
              ! linearly interpolate from hour hrend to 24
              dval = (bhrwc0(24) - bhrwc0(hrend)) / (24-hrend)
              do idx = hrend+1, 23
                  bhrwc0(idx) = bhrwc0(idx-1) + dval
              end do
          end if
      end if

      return
      end
