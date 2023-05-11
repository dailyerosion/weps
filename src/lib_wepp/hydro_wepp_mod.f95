!$Author$
!$Date$
!$Revision$
!$HeadURL$

module hydro_wepp_mod

  contains

    subroutine waterbal( layrsn, thetas, thetes, thetaf, thetaw, &
     &                   bszlyt, bszlyd, satcond,                       &
     &                   dprecip, bwdurpt, bwpeaktpt, bwpeakipt,        &
     &                   dirrig, bhdurirr, bhlocirr, bhzoutflow,        &
     &                   bhzsno, bslrr, bmrslp, bsfsan, bsfcla,         &
     &                   bsfcr, bsvroc, bsdblk, bsfcec,                 &
     &                   bbffcv, bbfcancov, bbzht, bcdayap,             &
     &                   bhzep, theta, thetadmx, bhrwc0,                &
     &                   bhzea, bhzper, bhzrun, bhzinf, bhzwid,         &
     &                   slen, cd, cm, cy, isr,                         &
     &                   wepp_hydro, init_loop, calib_loop, bhfice, wp)

!     + + + PURPOSE + + +
!     Implements soil water balance using routines from WEPP

      use file_io_mod, only: luowepphdrive
      use wepp_param_mod, only: wepp_param
      use hydro_util_mod, only: depstore
      use hydro_wepp_util_mod, only: disag, frsoil, effksat, purk, rainenergy
      use green_ampt_mod, only: grna
      use soillay_mod, only: valbydepth

      implicit none

!     + + + ARGUMENT DECLARATIONS + + +
      integer, intent(in) :: layrsn
      real, intent(in) :: thetas(*), thetes(*), thetaf(*), thetaw(*)
      real, intent(in) :: bszlyt(*), bszlyd(*), satcond(*)
      real, intent(in) :: dprecip, bwdurpt, bwpeaktpt, bwpeakipt
      real, intent(in) :: dirrig, bhdurirr, bhlocirr, bhzoutflow
      real, intent(in) :: bhzsno, bslrr, bmrslp, bsfsan(*), bsfcla(*)
      real, intent(in) :: bsfcr, bsvroc(*), bsdblk(*), bsfcec(*)
      real, intent(in) :: bbffcv, bbfcancov, bbzht
      integer, intent(in) :: bcdayap
      real, intent(in) :: bhzep
      real, intent(inout) :: theta(0:*), thetadmx(*), bhrwc0(*)
      real, intent(inout) :: bhzea, bhzper, bhzrun, bhzinf, bhzwid
      logical, intent(in) :: init_loop,calib_loop
      integer, intent(in) :: cd, cm, cy, isr, wepp_hydro
      real, intent(inout) :: slen
      real, intent(in) :: bhfice(*)
      type(wepp_param), intent(inout) :: wp
      
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
!     bsfcr    - Fraction of soil surface that is crusted
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
!     slen       - field length(m)
!     cd        - day of simulation
!     cm        - month of simulation
!     cy         - year of simulation

!     + + + PARAMETERS + + +
      integer mxtime, mxpond, nr
      parameter (mxtime = 1500, mxpond = 1000, nr = 11)

      real mmtom, mtomm
      parameter (mmtom = 0.001, mtomm = 1000.0)

      real hrtosec, sectohr
      parameter (hrtosec = 3600.0, sectohr = 1.0/3600.0)
      
      real hrtomin
      parameter (hrtomin = 60.0)

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
      integer idx, nrain, ninf, ns, i
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
      real peakro, durrun, precipmm, effintmm, effdrr_min
      integer nsl
      real surfcap, temp, laycap(layrsn)
      real effdrn
      real ssc(layrsn), sscv(layrsn), sscunf(layrsn), dg(layrsn)
      real kfactor,frdp,bottom,slsic(layrsn)
      integer LNfrst
      real rkecum_update
      
!      real ksold, smold
      integer it

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
!     peakro  - peak runoff
!     durrun  - runoff duration
!     effdrn  - effective runoff duration

!     rkecum_update - wp%rkecum value to set for next day

!     + + + FUNCTION DECLARATIONS + + +
!      real depstore, valbydepth, rainenergy, effksat

!     + + + DATA INITIALIZATIONS + + +
      dtinf = 180.0 !in seconds = three minutes)
      tf(1) = 0.0
      kfactor = 1e-5
      frdp = 0
      bottom = 0
      LNfrst = 0

!     + + + OUTPUT FORMATS + + +

!     + + + END SPECIFICATIONS + + +


!     zero out any hydrology variables that will be computed
      runoff = 0.0
      peakro = 0.0
      effdrn = 0.0
      effint = 0.0
      effdrr = 0.0

      wp%runoff = runoff
      wp%peakro = peakro
      wp%effdrn = effdrn
      wp%effint = effint
      wp%effdrr = effdrr
      wp%qout = 0.0
      wp%qin = 0.0
      wp%qsout = 0.0

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
          hk(idx) = -2.655d0 / log10( dble(fc(idx)) / dble(ul(idx)) )
          ! check for increase above saturation from settling outside hydro
          if( st(idx) .gt. ul(idx) ) then
               ! accumulate the excess water to be inserted lower in soil
               settle_seep = settle_seep + st(idx) - ul(idx)
               ! set layer water content to upper limit
               st(idx) = ul(idx)
               ! reset theta to adjusted value
               theta(idx) = ( st(idx)/layth(idx) ) + thetaw(idx)
          else if( st(idx) .lt. ul(idx)                                 &
     &         .and. (settle_seep .gt. 0.0) ) then
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

      ! factors for crust effect on infiltration
      if( bsfcr .lt. wp%prev_crust_frac ) then
          ! surface was disturbed, reset
          wp%rkecum = 0.0
      end if

      if(    (precip .gt. 0.0)                                          &
     &  .or. ((bhlocirr .ge. 0.0) .and. (irrig .gt. 0.0)) ) then

          ! create breakpoint data from cligen storm input
          nrain = nr
          call disag(nrain,trf, rf, precip, durpre, bwpeaktpt,bwpeakipt)

          ! accumulate rainfall kinetic energy for today
          rkecum_update = wp%rkecum + rainenergy(nrain, trf, rf)

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
          
!         Adjust conductivity (ks) for frozen soil in top layers
          do i = 1, nsl
             slsic(i) = 0
             ! only implemented for landuse = 1, croplands. Rangeland not implemented.
             ssc(i) = effksat(1, bsfcla(i),  bsfsan(i), bsfcec(i), 0.0, &
     &                0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0)
             sscv(i) = ssc(i)
             sscunf(i) = ssc(i)
             dg(i) = bszlyt(i) * mmtom
             bottom = bottom + dg(i)
             if (bhfice(i).ge.0.5) then
                    frdp = bottom
                    LNfrst = i
                    slsic(i) = (theta(i) - thetaw(i)) * dg(i)
             endif
          end do
          
          if (frdp.gt.0.0) then
             call frsoil(nsl,sscunf,LNfrst,ssc,sscv,dg,kfactor,slsic,   &
     &         wp%saxfc,wp%saxwp,wp%saxA,wp%saxB,wp%saxpor,wp%saxenp,   &
     &         wp%saxks)
          endif
            
          call infparsub( nsl, ssc, sscv, layth, bsfcec, st,ul,slsic,   &
     &                    clay, sand, avbulkd, avporos, avrockvol,      &
     &                    wcon, bbffcv, bbfcancov, bbzht,               &
     &                   ranrough, dsnow, prcp, wp%rkecum, bcdayap,     &
     &                    ks, sm, frdp )
   
          ! debugging output
!          if (init_loop.eqv..false.) then
!              write(63,1500) cy, cm, cd, wepp_hydro, prcp, ks, sm,      &
!     &                       depsto, ranrough, bmrslp, wcon
!1500          format(1x, 4i6, 7E12.3)
!              write(163,1510) cy, cm, cd, wepp_hydro, ks, wp%rkecum
!1510          format(1x, 4i6, 2E12.3)
!          end if

          ! call infiltration
          call grna( ninf, depsto, train, rrain, rr, ks, sm, &
               ns, tf, rcum, f, ff, re, recum, tp, &
               rprint, ddepsto, runoff, durexr, effint, effdrr, it )

!      write(*,*) 'it', it
!      if( it .gt. 60 ) then
!        do idx = 1, nrain
!          write(*,*) 'idx,trf,rf', idx, trf(idx), rf(idx)
!        end do
!        do idx = 1, it
          ! write out ponding array
!          write(*,*) 'idx,tp', idx, tp(idx)
!        end do
!        do idx = 1, ns
          ! write out infiltration array
!          write(*,*) 'idx,tf,ddepsto,re,f,rprint', idx, tf(idx),        &
!     &               ddepsto(idx), re(idx), f(idx), rprint(idx)
!        end do
!      end if

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
               temp = xfin * laycap(idx) / surfcap  * weightsurf   ! weight by available capacity
                                                               ! skew toward surface
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

         ! rainfall kinetic energy for today unchanged
         rkecum_update = wp%rkecum
       end if

       if ((runoff .gt. 0.0) .and. (wepp_hydro .gt. 1)) then
           ! call flow routing hdrive
           ! write (*,*) 'runoff=', runoff
           call hdriveflow(ns, ninf, recum, slen, bmrslp, durexr, dtinf,&
     &         tf, re, bbffcv, peakro, durrun)

           if (peakro.gt.0.0) then
               effdrn = ((runoff*mtomm)/peakro) * 60.
           else
               effdrn = 0.0
           endif
       else
           peakro = 0.0
           durrun = 0.0
           effdrn = 0.0
       endif

       precipmm = precip * mtomm
       effintmm = effint * mtomm * 3600


       effdrr_min = effdrr / 60.

      !  write flow routing results to wepp_hdrive.out
      !  precipmm - ok
      !  bhzrun - ok (runoff)
      !  peakro - maybe
      !  effdrn - maybe
      !  effint - maybe
      !  effdrr - maybe (effective duration)
      !
        if ((wepp_hydro .gt. 1).and.(init_loop.eqv..false.).and.        &
     &      (calib_loop .eqv. .false.)) then
            write(luowepphdrive(isr),                                   &
     &      fmt="(1X,i4,4X,i4,1X,i3,5(3X,f6.1),3X,f8.2)")               &
     &      cd,cm,cy,precipmm, bhzrun, peakro, effdrn,                  &
     &      effintmm,  effdrr_min
        endif
        

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

!     set return values used by the WEPP erosion code
      wp%runoff = runoff
      wp%peakro = peakro/(mtomm * hrtosec)
      wp%effdrn = effdrn*hrtomin
      wp%effint = effint
      wp%effdrr = effdrr*hrtomin
      
      ! update crust fraction value for next day
      wp%prev_crust_frac = bsfcr
      ! set cumulative rainfall energy for next day
      wp%rkecum = rkecum_update

      ! surface water content is gravimetric (kg/kg)
      do idx = 1, 24
          bhrwc0(idx) = bhrwc0(idx) / bsdblk(1)
      end do

      return

    end subroutine waterbal

    subroutine arraymerge( nr, dt, trf, rf, irrig, durirr,            &
     &                       nf, tr, r, rr)

!     + + + PURPOSE + + +
!     merges rainfall, infiltration step and irrigation arrays

!     + + + ARGUMENT DECLARATIONS + + +
      integer, intent(in) :: nr
      real, intent(in) :: dt, trf(*), rf(*), irrig, durirr
      integer, intent(inout) :: nf
      real, intent(inout) :: tr(*), r(*), rr(*)

!     + + + ARGUMENT DEFINITIONS + + +
!     nr - number of points in the rainfall breakpoint representation
!     dt - infiltration array time step
!     trf - time values in the rainfall breakpoint representation (T)
!     rf - rate values in the rainfall breakpoint representation (L/T)
!     irrig - daily irrigation depth (L)
!     durirr - daily irrigation duration (T)
!     nf - number of values in the infiltration array
!     tr - time value in infiltration input array (T)
!     r - depth value in infiltration input array (rainfall) (L)
!     rr - cumulative depth for infiltration array (L)

!     + + + PARAMETERS + + +
      integer mxtime
      parameter (mxtime = 1500)

!     + + + PARAMETER DEFINITIONS + + +

!     + + + LOCAL VARIABLES + + +
      integer idx, jdx, ip, nri
      real*8 xx, test
      real dtc, rateirr, trfi(mxtime), rfi(mxtime)

!     + + + LOCAL DEFINITIONS + + +
!     idx - loop variable
!     jdx - loop variable
!     xx - cumulative sum value
!     test - intermediate value
!     dtc - cumulative time for infiltration (T)
!     rateirr - irrigation rate (L/T) (constant for irrigation duration)
!     trfi - time values in rainfall array with irrigation rate and duration added
!     rfi - rate values in rainfall array with irrigation rate and duration added

!     + + + DATA INITIALIZATIONS + + +

!     + + + END SPECIFICATIONS + + +

      if( (irrig .gt. 0.0) .and. (durirr .gt. 0.0) ) then
         ! compute irrigation rate
         rateirr = irrig / durirr
         ! add point for time irrigation ends
         ! find insertion point starting at end
         ip = 0
         do jdx = nr, 1, -1
            if (durirr.ge.trf(jdx)) then
               ! insertion point found
               ip = jdx
               test = abs(durirr-trf(jdx))
               if (test.gt.0.015d0) then
                  ! irrigation termination is different from this rainfall time
                  ! new time will be added
                  nri = nr + 1
               else
                  ! irrigation termination time is same as rainfall breakpoint
                  ! no insertion done, breakpoint used
                  nri = nr
               end if
               exit
            end if
         end do
      else
         nri = nr
         ip = 0
      end if

      ! add irrigation rate to rainfall rate.
      idx = 0
      do jdx = 1, nr
         if( ip .gt. jdx ) then
            ! irrigation being applied
            trfi(jdx) = trf(jdx)
            rfi(jdx) = rf(jdx) + rateirr
         else if( ip .eq. jdx ) then
            if( nri .gt. nr ) then
               ! point inserted
               idx = jdx + 1
               trfi(jdx) = trf(jdx)
               rfi(jdx) = rf(jdx)+ rateirr
               ! set irrigation termination breakpoint
               trfi(idx) = durirr
               rfi(idx) = rf(jdx)
            else
               ! no point inserted
               idx = jdx
               trfi(idx) = trf(jdx)
               rfi(idx) = rf(jdx)
            end if
         else ! ip .lt. jdx
            ! no irrigation being applied
            idx = idx + 1
            trfi(idx) = trf(jdx)
            rfi(idx) = rf(jdx)
         end if
      end do
      if( ip .eq. nri ) then
         trfi(nri) = durirr
         rfi(nri) = 0.0
      end if

      ! remove any zero entries in the beginning of the array

      ! search up the array for multiple zero time entries
      idx = nri
      do jdx = 2, idx
         if( trfi(jdx) .le. 0.0 ) then
             nri = nri - 1
         end if
      end do
      ! remove zero entries by shifting the array down
      ! set idx to number of values to be removed
      idx = idx - nri
      do jdx = 1, nri
         trfi(jdx) = trfi(jdx+idx)
         rfi(jdx) = rfi(jdx+idx)
      end do

      ! using modified rainfall array, merge with infiltration timestep array
      xx = 0.d0
      idx = 2
      dtc = dt
      tr(1) = trfi(1)
      r(1) = rfi(1)
      rr(1) = 0.0
     
      do jdx = 2, nri
  110    test = abs(dtc-trfi(jdx))
         if (idx.gt.2) then
            xx = xx + r(idx-2) * (tr(idx-1)-tr(idx-2))
            rr(idx-1) = xx
         end if
         if (test.gt..015d0) then
           
            if (dtc.lt.trfi(jdx)) then
               r(idx) = rfi(jdx-1)
               tr(idx) = dtc
               dtc = dtc + dt
               idx = idx + 1
               go to 110
            else
               tr(idx) = trfi(jdx)
               r(idx) = rfi(jdx)
               idx = idx + 1
            end if
        
         else
            tr(idx) = trfi(jdx)
            r(idx) = rfi(jdx)
            idx = idx + 1
            dtc = dtc + dt
         end if
      end do

      nf = idx - 1
      if( nf .lt. 3 ) then
         do jdx = 1, nf
            rr(jdx) = 0.0
         end do
      else
         rr(nf) = rr(nf-1) + r(nf-1) * (tr(nf)-tr(nf-1))
      end if

      return
    end subroutine arraymerge

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
      real, intent(out) :: ks, sm
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

      sf = 0.01 * exp( 6.531d0 - 7.33d0*avpor + 15.8d0*tmpvr2 + 3.81d0*tmpvr4 &
         + avsand * (3.4d0*avclay - 4.98d0*avpor) &
         + tmpvr4 * (16.1d0*tmpvr3 + 16.0d0*tmpvr2) - 14.0d0*tmpvr3*avclay &
         - avpor * (34.8d0*tmpvr2 + 8.0d0*tmpvr3) )

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
        a = exp(6.10d0 - (10.3d0*avsand) - (3.7d0*avclay))
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
        crstad = crust + (1-crust) * exp(dble(bbbb))
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
        ccovef = cancov * exp(-0.3358d0*canhgt/2.0d0)

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
    end subroutine infparsub

end module hydro_wepp_mod
