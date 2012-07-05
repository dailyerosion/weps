!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine darcy(daysim, numeq, bszlyt, bszlyd, bulkden,          &
     &       theta, thetadmx, bthetas, bthetaf, bthetaw, bthetar,       &
     &       bhrsk, bheaep, bh0cb, bsfcla, bsfom, bhtsav,               &
     &       bwtdmxprev, bwtdmn, bwtdmx, bwtdmnnext, bwtdpt,            &
     &       rise, daylength, bhzep, dprecip, bwdurpt, bwpeaktpt,       &
     &       dirrig, bhdurirr, bhlocirr, bhzoutflow,                    &
     &       bbdstm, bbffcv, bslrro, bslrr, bmzele, bhrwc0,             &
     &       bhzea, bhzper, bhzrun, bhzinf, bhzwid,                     &
     &       bhzeasurf, evaplimit, vaptrans, bmrslp )

!     + + + PURPOSE + + +
!     This subroutine predicts on an hourly basis soil water profile,
!     soil water content at the soil-air interface, potential and
!     actual soil evaporation, runoff, ponding and deep percolation.

!     + + + KEYWORDS + + +
!     soil water redistribution, evaporation, runoff, deep percolation

!     + + + ARGUMENT DECLARATIONS + + +
      integer daysim, numeq
      real bszlyt(*), bulkden(*), bszlyd(*), theta(0:*)
      real thetadmx(*), bthetas(*), bthetaf(*), bthetar(*), bthetaw(*)
      real bhrsk(*), bheaep(*), bh0cb(*), bsfcla(*), bsfom(*), bhtsav(*)
      real bwtdmxprev, bwtdmn, bwtdmx, bwtdmnnext, bwtdpt
      real rise, daylength, bhzep, dprecip, bwdurpt, bwpeaktpt
      real dirrig, bhdurirr, bhlocirr, bhzoutflow
      real bbdstm, bbffcv, bslrro, bslrr, bmzele, bhrwc0(*)
      real bhzea, bhzper, bhzrun, bhzinf, bhzwid
      real bhzeasurf, evaplimit, vaptrans, bmrslp

! intent(in)
! daysim, numeq, bszlyt, bszlyd, bulkden,
! bthetas, bthetaf, bthetar, bhrsk,
! bheaep, bh0cb, bsfcla, bsfom, bhtsav,
! bwtdmxprev, bwtdmn, bwtdmx, bwtdmnnext, bwtdpt,
! rise, daylength, bhzep,
! dprecip, bwdurpt, bwpeaktpt,
! dirrig, bhdurirr, bhlocirr,
! bbdstm, bbffcv, bslrro, bslrr, bmzele, 
! bhzeasurf, evaplimit, vaptrans

! intent(inout)
! theta, thetadmx, bhrwc0,
! bhzea, bhzper, bhzrun, bhzinf, bhzwid

!     + + + ARGUMENT DEFINITIONS + + +
!     daysim     - day of the simulation (very useful for debugging, not necessary otherwise)
!     numeq      - number of equations to be solved = layrsn + 6
!     bszlyt(*)  - thickness of the soil layer (mm)
!     bszlyd(*)  - depth to bottom of the soil layer (mm)
!     theta(*)   - volumetric water content (m^3/m^3)
!     thetadmx(*)- daily maximum volumetric water content (m^3/m^3)
!     bulkden(*) - soil bulk density Mg/m^3)
!     bthetas(*) - saturated volumetric water content (m^3/m^3)
!     bthetaf(*) - field capacity volumetric water content (m^3/m^3)
!     bthetar(*) - residual (conductivity) volumetric water content (m^3/m^3)
!     bhrsk(*)   - saturated hydraulic conductivity (m/s)
!     bheaep(*)  - air entry potential (J/kg)
!     bh0cb(*)   - exponent of Campbell soil water release curve (unitless)
!     bsfcla(*)  - fraction of soil mineral content which is clay (unitless)
!     bsfom(*)   - fraction of total soil which is organic (unitless)
!     bhtsav(*)  - daily average soil temperature (C)
!     bwtdmxprev - maximum air temperature of previous day (C)
!     bwtdmn     - minimum air temperature (C)
!     bwtdmx     - maximum air temperature (C)
!     bwtdmnnext - minimum air temperature of next day (C)
!     bwtdpt     - dew point temperature (C)
!     rise       - time of the sunrise (hour of day)
!     daylength  - time of daylight (hours)
!     bhzep      - potential soil evaporation (mm/day)
!     dprecip     - rainfall depth after snow filter (mm)
!     bwdurpt    - duration of precipitation (hours)
!     bwpeaktpt  - normalized time to peak of precipitation (time to peak/duration)
!     dirrig   - Daily irrigation (mm)
!     bhdurirr - duration of irrigation water application (hours)
!     bhlocirr - emitter location point (m)
!                positive is above the soil surface
!                negative is below the soil surface
!     bhzoutflow - height of runoff outlet above field surface (m)
!     bbdstm     - total number of stems (#/m^2)
!     bslrro     - original random roughness height, after tillage, mm
!     bslrr      - Allmaras random roughness parameter (mm) 
!     bmzele     - Average site elevation (m)
!     bhrwc0(*)  - Hourly values of surface soil water content (not as in soil)
!     bhzea      - accumulated daily evaporation (mm) (comes in with a value set from snow evap)
!     bhzper     - accumulated daily drainage (deep percolation) (mm)
!     bhzrun     - accumulated daily runoff (mm)
!     bhzinf     - depth of water infiltrated (mm)
!     bhzwid     - Water infiltration depth into soil profile (mm)
!     bhzeasurf - accumulated surface evaporation since last complete rewetting (mm)
!     evaplimit - accumulated surface evaporation since last complete rewetting
!                 defining limit of stage 1 (energy limited) and start of 
!                 stage 2 (soil vapor transmissivity limited) evaporation (mm)
!     vaptrans  - vapor transmissivity (mm/d^.5)
!     bmrslp   - Average slope of subregion (mm/mm)

!     + + + PARAMETERS + + +
      real   pi
      parameter( pi = 3.1415927 )

!     + + + COMMON BLOCKS + + +
      include 'p1werm.inc'
      include 'p1unconv.inc'
      include 'm1flag.inc'
      include 'm1subr.inc'
      include 'file.inc'

!     + + + LOCAL COMMON BLOCKS + + +
      include 'hydro/lsoda.inc'
      include 'hydro/dvolwparam.inc'
      include 'hydro/vapprop.inc'
      include 'hydro/clayomprop.inc'

!     + + + LOCAL VARIABLES + + +
      integer    itol, itask, jt, iopt, neq(2)
      real       tday,tout,relerr(1),abserr(numeq)
      real       volw(numeq)
      integer    kindex, hourstep, day, mo, yr
      real       swc, swci, ref_ranrough, ranrough
!      integer    lstep,lfunc,ljac
      real       temp, netflux(numeq)
      real       evap, runoff, drain, infil
      integer    iminlay, imaxlay
      real       evapratio, surf_cum
      integer    idoy
      real       thetaf, availwat
      real       unsatcond, matricpot
      real       laycenter, sat_rat, irrigstart, irrigmid

      real       delta_drip
      parameter  (delta_drip = 0.005) ! use to avoid flooding a thin 
                                      ! soil layer with drip irrigation 
!      real       temp_sat

!     + + + LOCAL DEFINITIONS + + +
!     itol     - lsoda: setting to make relerr and/or abserr scalar or array
!     itask    - lsoda: stepping mode
!     jt       - lsoda: flag for selection of jacobian matrix
!     iopt     - lsoda: flag for use of extra inputs
!     neq(1)   - lsoda: number of equations to be solved
!     tday     - time of day (seconds)
!     tout     - time of day for end of integration step (seconds)
!     relerr(1) - lsoda: relative error value to control integration accuracy
!     abserr(numeq) - lsoda: absolute error value to control integration accuracy
!     volw(1-5) - accumulated values integrate in time
!                   (1) accumulated rainfall depth (total surface water application)(m)
!                   (2) runoff depth (m)
!                   (3) evaporation depth (m)
!                   (4) depth of water infiltrated (m)
!                   (5) ponded water depth (m)
!     volw(numeq) - (iminlay:imaxlay) volume of water in a soil layer (m)
!                   (imaxlay+1) depth of water drained (m)
!     kindex   - array index for loops
!     hourstep - step counter for 24 hourly steps
!     day      - day of month
!     mo       - month of year
!     yr       - year of simulation
!     swc      - total depth of water in soil profile (mm)
!     swci     - total depth of water in soil profile a start of day (mm)
!     ref_ranrough - random roughness after last tillage (m)
!     ranrough - present random roughness (m)
!     lstep    - preserve number of steps from previous lsoda call
!     lfunc    - preserve number of function evaluations from previous lsoda call
!     ljac     - preserve number of jacobian evaluations from previous lsoda call
!     temp     - temporary value
!     prevevap - previous hour accumulated soil surface evaporation depth (mm)
!     evap     - accumulated soil surface evaporation depth (mm)
!     prevrunoff - previous hour accumulated soil surface runoff (mm)
!     runoff   - accumulated soil surface runoff (mm)
!     prevdrain - previous hour accumulated soil drainage (mm)
!     drain    - accumulated soil drainage (mm)
!     iminlay  - minimum index for placement of soil layers in volw array
!     imaxlay  - maximum index for placement of soil layers in volw array
!     evapratio - ratio reduction in evaporation rate due to soil dryness
!     surf_cum  - updated hourly cummulative surface evaporation (mm)
!     thetaf    - field capacity water content (for output)
!     availwat  - soil plant availale water content (for output)
!     unsatcond - unsaturated hydraulic conductivity (m/s) (for output)
!     matricpot - soil matric potential (m) (for output)
!     layercenter - depth to the center of a soil layer (mm) (for output)
!     sat_rat - saturation ratio (for output)
!     irrigstart - time of start point of irrigation (surface or subsurface) event (seconds)
!     irrigmid  - time of midpoint of irrigation (surface or subsurface) event (seconds)

!     + + + SUBROUTINES CALLED + + +
!     slsoda - livermore solver for ordinary differential equations

!     + + + FUNCTION DECLARATIONS + + +
      external   dvolw, jac
      real   volwatadsorb
      real   volwat_matpot_bc
      real   atmpreselev, depstore, fricfact, store
      real   calctht0
      real   evapredu
      integer dayear
      real   availwc
      real   unsatcond_bc
      real   intersect

!     + + + DATA INITIALIZATIONS + + +

!     + + + OUTPUT FORMATS + + +
 2000   format('*','Time of Sunrise: ',f10.2,/,'*','Length of Day: '    &
     &,f10.2,/,'*','Time of Sunset: ',f10.2)
 2009   format ('*',' Hourly HYDROLOGY output ')
 2010   format('*','date: ',i2,'/',i2,'/',i4,'          Subregion # '   &
     &         ,i4)
 2020   format ('*',22x,3('*'),' hourly soil water data ',3('*'))
 2030   format ('*',79('-')/'*','hr',t7,'evap',t13,'run',t17,'dper',t24,&
     &  'swc',t28,'theta(0)',1x,11('-'),'water content of soil layers', &
     &  10('-')/,'*',t7,'--------mm--------',2x,25('-'),                &
     &  'm^3/m^3',24('-'))
 2040    format('*',18x,f8.3,1x,f6.3,1x,10(1x,f6.3))
 2050    format(1x,i2,2f6.3,f4.1,f8.3,1x,f6.3,1x,10(1x,f6.3))
 2060    format('*',79('-'))
 2065    format('*','average daily wetness',6x,f6.3,1x,10(1x,f6.3))
 2070    format('*','ep=',f7.2,' mm',' (cumulative amount of daily',    &
     &         ' potential soil evaporation)')
 2080    format('*','ea=',f7.2,' mm',' (cumulative amount of daily',    &
     &         ' actual soil evaporation)')
 2090    format('*','bhzper=',f6.2,' mm',' (cumulative amount of daily',&
     &         ' deep percolation)')
 2100    format('darcy: lrx, wc, wct, wfluxn, deltim, wfluxr(top), wflu &
     &xr(bottom)',/,i2,7f11.3,/,7f11.3)
 3000 format('# sec daysim doy yr var depth volw netflux numeq = ',i3)
 3010 format(1x,f8.1,1x,i5,1x,i3,1x,i4,1x,i3,4(1x,g11.4))

!     + + + END SPECIFICATIONS + + +

! ***      write(*,*) 'in darcy'

      if( (am0ifl .eqv. .true.) .and. ((am0hfl .eq. 2)                  &
     & .or. (am0hfl .eq. 3) .or. (am0hfl .eq. 6)                        &
     & .or. (am0hfl .eq. 7)) ) then
          ! write header information to hourly (or sub-hourly output file
         write(luowater, 3000) numeq
      end if
!     initialize step reporting counters
!      lstep = 0
!      lfunc = 0
!      ljac = 0

!     bhzea not zeroed since snow evap must be included
      bhzper = 0.0
      bhzrun = 0.0
      bhzinf = 0.0

!     initialize LSODA options
      call xsetf(0)   !do not print internal error messages

!     initialize values for the DVOLWPARAM common block
!     values are place in volw array position, which is structured with
!     surface variables first, then the soil surface to depth and 
!     then drainage at depth.
      layrsn = numeq - 6
      iminlay = 6
      imaxlay = layrsn + 5
      do kindex = 1, layrsn
          tlay(kindex) = bszlyt(kindex) *  mmtom
          thetas(kindex) = bthetas(kindex)
          thetaw(kindex) = bthetaw(kindex)
          thetar(kindex) = bthetar(kindex)
          ksat(kindex) = bhrsk(kindex)
          airentry(kindex) = bheaep(kindex) / gravconst
          lambda(kindex) = 1.0 / bh0cb(kindex)
          temp = bulkden(kindex)*1000.0  !convert Mg/m^3 to kg/m^3
          theta80rh(kindex) = volwatadsorb( temp,                       &
     &                        bsfcla(kindex), bsfom(kindex),            &
     &                        claygrav80rh, orggrav80rh )
!          thetaw(kindex) = volwat_matpot_bc(potwilt, thetar(kindex),    &
!     &               thetas(kindex), airentry(kindex), lambda(kindex))
          soiltemp(kindex) = bhtsav(kindex)
      end do
      dist(1) = tlay(1) / 2.
      depth(1) = dist(1)
      do kindex = 2, layrsn
        dist(kindex) = (tlay(kindex) + tlay(kindex-1))/2.
        depth(kindex) = depth(kindex-1) + dist(kindex)
      end do
      airtmaxprev = bwtdmxprev
      airtmin = bwtdmn
      airtmax = bwtdmx
      airtminnext = bwtdmnnext
      tdew = bwtdpt
      lenday = daylength * hrtosec
      sunrise = rise * hrtosec
      sunset = sunrise + lenday

!     evaporation limiting parameters for dvolw (convert all to m)
      evapendconstant = evaplimit * mmtom
      evapdaypot = bhzep * mmtom
      evaptrans = vaptrans * mmtom
      ! partition potential evaporation over entire day
      ! make 10% apply all day, the rest to the sine curve
      if( lenday.gt.0.0 ) then
          evapamp = pi * 0.9 * bhzep * mmtom / lenday / 2.0
      else
          evapamp = 0.0
      end if

      ! set passed rain parameters
      raindepth = dprecip * mmtom  !storm total depth (m)
      rainstart = 1.0
      if( (bwdurpt.lt.0.08333333).and.(dprecip.gt.0.001) ) then !0.0833333 = 5 minutes = 300sec
          !enforce minimum time so solver does not miss rainfall. The
          !minimum is really between 60 and 120 for the example run, but to make sure used 300
          !add sanity check for duration amount relationship from
          !Linsley,R.K., M.A. Kohler, J.L.H. Paulhus. 1982. Hydrology for Engineers. p80.
          rainend = rainstart                                           &
     &            + max(300.0,10.0**(log10(dprecip)/0.48-1.90231428))
          rainmid = rainstart                                           &
     &        + max(rainstart,min(rainend,bwpeaktpt*300.0))
      else
          rainend = rainstart + max(300.0, bwdurpt * hrtosec)
          rainmid = rainstart                                           &
     &        + max(rainstart,min(rainend,bwpeaktpt*bwdurpt*hrtosec))
      end if
      ref_ranrough = bslrro * mmtom
      ranrough = bslrr * mmtom
      soilslope = bmrslp
      pondmax = depstore( ranrough, soilslope, bhzoutflow )!maximum ponding depth in meters
      dw_friction = fricfact( ref_ranrough, ranrough, bbdstm, bbffcv )
      slopelength = 50.0  !temporarily set since value is not supplied
      atmpres = atmpreselev( bmzele )

      ! set passed irrigation parameters
      ! zero out all input parameters
      surface_rate = 0.0
      surface_start = 0.0
      surface_end = 0.0
      do kindex=1,layrsn
          source_rate(kindex) = 0.0
          source_start(kindex) = 0.0
          source_end(kindex) = 0.0
      end do
      ! set irrigmid for later use
      irrigmid = 0.0
      ! reset parameters if irrigation is applied
      if( dirrig .gt. 0.0 ) then
          if( bhlocirr .ge. 0.0 ) then
              ! apply irrigation water as surface water
              surface_rate = dirrig * mmtom / (bhdurirr * hrtosec)
              surface_start = 0.0
              surface_end = bhdurirr * hrtosec
              ! set time parameters for use in setting time step
              irrigstart = surface_start
              irrigmid = (surface_start + surface_end) / 2.0
          else
              ! add within layer source term to layers
              ! uses a finite interval to avoid overloading a thin layer
              call distriblay( layrsn, bszlyd, bszlyt, source_rate,     &
     &                         dirrig * mmtom / (bhdurirr * hrtosec),   &
     &                         max(0.0,-bhlocirr-delta_drip),           &
     &                         -bhlocirr+delta_drip )
              temp = 0.0
              do kindex=1,layrsn
                  if( intersect( temp, bszlyd(kindex),                  &
     &                          max(0.0,-bhlocirr-delta_drip),          &
     &                         -bhlocirr+delta_drip ) .gt. 0.0 ) then
                      source_end(kindex) = max( source_end(kindex),     &
     &                                          bhdurirr * hrtosec )
                      ! set time parameters for use in setting time step
                      irrigstart = source_start(kindex)
                      irrigmid = (source_start(kindex)                  &
     &                         + source_end(kindex)) / 2.0
                  end if 
              end do
          end if
      end if

!     initialize state of soil
      do kindex=1,layrsn
          if(theta(kindex).le.0.0) then
             write(0,*)                                                 &
     &        'Error: darcy:begin theta<0',kindex,theta(kindex),daysim
             call exit (1)
             !stop
          endif
          volw(kindex+5) = theta(kindex)*tlay(kindex)
          lastvolw(kindex+5) = -1.0e15
      end do
      do kindex=iminlay,imaxlay
          prevvolw(kindex) = volw(kindex)
      end do
      bhzwid = 0.0

!     initialize auxiliary variables for integration
!      if( daysim .eq. 1 ) then   !commented out to initialized every day anew
          do kindex=1,5
              prevvolw(kindex) = 0.0
          end do
          prevvolw(numeq) = 0.0
!      endif
      ! start surface evap at the daily acumulated total
      prevvolw(3) = bhzeasurf * mmtom
      do kindex=1,5
          volw(kindex) = prevvolw(kindex)
      end do
      volw(numeq) = prevvolw(numeq)

      neq(1) = numeq !can't declare parameter as array, but must be passed as an array
      neq(2) = 0 !this flag is used to activate printing internal to dvolw
!      if(daysim.eq.48) then
!          neq(2) = 1
!      else
!          neq(2) = 0 !this flag is used to activate printing internal to dvolw
!      endif
      itol = 2   !lsoda relerr is scalar and abserr is an array
!      itol = 1   !lsoda relerr is scalar and abserr is scalar
      relerr(1) = 1.0e-4  ! relative error tolerance
      do kindex=1,numeq  
          abserr(kindex) = 1.0e-6   ! absolute error tolerance
      end do
      itask = 1  !lsoda returns value at tout
!      itask = 2    !put in single step mode for more error reporting
!      iopt = 0   !lsoda no extra inputs
      iopt = 1   !lsoda using optional inputs on rwork and iwork(5-10)
      do kindex=5,10
          rwork(kindex) = 0.0
          iwork(kindex) = 0
      end do
      rwork(6) = 7200  !maximum allowed step size (seconds)
!      jt = 2     !lsoda internally generated Jacobian matrix
      jt = 5     !lsoda internally generated banded Jacobian matrix
      iwork(1) = 3  !ml, lower half band width of jacobian matrix
      iwork(2) = 1  !mu, upper half band width of jacobian matrix
      iwork(6) = 5000 !maximum number of steps allowed before error generated

!      if( daysim .eq. 1 ) then    !commented out to initialized every day anew
          istate = 1
          beginday = 0.0
          t = beginday
!      endif
      hourstep = 1

!  print out zero hour initialization values
      if ((am0hfl .eq. 2) .or. (am0hfl .eq. 3) .or.                     &
     &    (am0hfl .eq. 6) .or. (am0hfl .eq. 7)) then
         call caldatw(day,mo,yr)
         idoy = dayear(day, mo, yr)
         if( idoy .eq. 1 ) then
             write(luowater,*)
             write(luowater,*)
         else
             ! print a blank line to separate layer blocks
             write(luowater,*)
         end if
         ! output from differencing array for above ground phenomena
         call dvolw(neq, t, volw, netflux)
         do kindex=1,5
             write(luowater,3010) t, daysim, idoy, yr, kindex,          &
     &             0.0, volw(kindex), netflux(kindex), 0.0
         end do
         ! set surface water content value and output above thetat(1)
         evapratio = evapredu(bhzeasurf, evaplimit, vaptrans, bhzep)
         theta(0) = calctht0(bszlyd, theta, thetaw, evapratio)      !H-64,65,66
         write(luowater,3010) t, daysim, idoy, yr, 0,                   &
     &         0.0, 0.0, evapratio, theta(0)
         ! output from differencing array continued into soil layers
         do kindex=6,numeq-1
             laycenter = bszlyd(kindex-5)-0.5*bszlyt(kindex-5)
             write(luowater,3010) t, daysim, idoy, yr, kindex,          & 
     &            laycenter,volw(kindex),netflux(kindex),theta(kindex-5)
         end do
         ! output from differencing array for drainage value
         kindex = numeq
         write(luowater,3010) t, daysim, idoy, yr, kindex,              &
     &         0.0, volw(kindex), netflux(kindex), 0.0
      end if

!     start loop in time
30    continue

      ! set initial time step to make sure water applications are found
      tday = t - beginday
      tout = beginday + hourstep*hrtosec
      ! rainfall event
      temp = (rainstart + rainend) / 2.0
      if(      (raindepth .gt. 0.0)                                     &
     &   .and. (tday .le. rainstart)                                    &
     &   .and. (tout .gt. rainstart)                                    &
     &   .and. (tout .gt. temp) ) then
          ! initializes solution routines
          istate = 1
          ! guarantees that integration will find event
          tout = min(temp, hourstep*hrtosec)
          ! reset multiday tracking values
          beginday = 0.0
          t = tday
!          lstep = 0    !step reporting counters
!          lfunc = 0
!          ljac = 0
!          itask = 2    !put in single step mode for more error reporting
      end if
      ! irrigation event
      temp = min( tout, irrigmid )
      if(      (dirrig .gt. 0.0)                                        &
     &   .and. (tday .le. irrigstart)                                   &
     &   .and. (tout .gt. irrigstart)                                   &
     &   .and. (tout .gt. temp) ) then
          ! initializes solution routines
          istate = 1
          ! guarantees that integration will find event
          tout = min(temp, hourstep*hrtosec)
          ! reset multiday tracking values
          beginday = 0.0
          t = tday
!          lstep = 0    !step reporting counters
!          lfunc = 0
!          ljac = 0
!          itask = 2    !put in single step mode for more error reporting
      end if
!     settings for single step mode, retaining values on the hour
!      itask = 5
!      rwork(1) = tout

40    continue
      call slsoda(dvolw,neq,volw,t,tout,itol,relerr,abserr,itask,       &
     &           istate,iopt,rwork,lrw,iwork,liw,jac,jt)

!      if( daysim.eq.6321 ) then
!          write(*,*) 'steps, functions, jacobians, order:',
!     &    iwork(11)-lstep,
!     &    iwork(12)-lfunc,iwork(13)-ljac,iwork(14)
!      end if
!      lstep = iwork(11)
!      lfunc = iwork(12)
!      ljac = iwork(13)

!-------------------------------------------------------------
!  Was the step successful?  If not, quit with an explanation.
!-------------------------------------------------------------
      if(istate .lt. 0) then
          if(istate.eq.-1) then
              istate=2
              write(*,*) 'day',daysim,'time',t,'5k steps ',             &
     &                  'infil=',( volw(4) - prevvolw(4) ) * mtomm,     &
     &                  'drain=',(volw(numeq)-prevvolw(numeq))*mtomm
!              do kindex=1,layrsn
!                  write(*,*) 'darcy:s,r,e,b',bthetas(kindex),
!     &            bthetar(kindex),bheaep(kindex),bh0cb(kindex)
!                  theta(kindex) = volw(kindex+5)/tlay(kindex)
!              end do
!              write(*,*) 'darcy:theta',(theta(kindex),kindex=1,layrsn)
!              call dvolw(neq,t,volw,netflux)
!              write(*,*) 'darcy:netflux',
!     &                  (netflux(kindex+5),kindex=1,layrsn)
          else
             write(0,*)                                                 &
     &       "Error: Failed day:",daysim," time:",t," istate:",istate
              call exit(1)
          end if
          goto 40
      end if

!-------------------------------------
!     step was sucessful, continue on
!-------------------------------------
      if ((am0hfl .eq. 2) .or. (am0hfl .eq. 3) .or. (am0hfl .eq. 6) .or.&
     &   (am0hfl .eq. 7)) then
         ! blank line to separate each layer block
         write(luowater,*)
         ! other values
         do kindex=1,layrsn
           theta(kindex) = volw(kindex+5)/tlay(kindex)
         end do
         ! output from differencing array for above ground phenomena
         call dvolw(neq, t, volw, netflux)
         do kindex=1,5
             write(luowater,3010) t, daysim, idoy, yr, kindex,          &
     &         0.0, volw(kindex), netflux(kindex), 0.0
         end do
         ! set surface water content value and output above thetat(1)
         surf_cum = bhzeasurf + bhzea + (volw(3) - prevvolw(3)) * mtomm
         evapratio = evapredu( surf_cum, evaplimit, vaptrans, bhzep )
         theta(0) = calctht0(bszlyd, theta, thetaw, evapratio)      !H-64,65,66
         write(luowater,3010) t, daysim, idoy, yr, 0,                   &
     &         0.0, 0.0, evapratio, theta(0)
         ! output from differencing array continued into soil layers
         do kindex=6,numeq-1
             laycenter = bszlyd(kindex-5) - 0.5*bszlyt(kindex-5)
             write(luowater,3010) t, daysim, idoy, yr, kindex,          &
     &         laycenter, volw(kindex), netflux(kindex), theta(kindex-5)
         end do
         ! output from differencing array for drainage value
         kindex = numeq
         write(luowater,3010) t, daysim, idoy, yr, kindex,              &
     &     0.0, volw(kindex), netflux(kindex), 0.0
      end if

      if( t.lt.(hourstep*hrtosec)) goto 30

!-------------------------------------
! completed the hour, sum up
!-------------------------------------
      do kindex=1,layrsn
          theta(kindex) = volw(kindex+5)/tlay(kindex)
          thetadmx(kindex) = max( thetadmx(kindex),theta(kindex) )
      end do

      swci = sum(volw(6:layrsn+5)) * mtomm

!     create hourly and daily output values
      runoff = ( volw(2) - prevvolw(2) ) * mtomm
      bhzrun = bhzrun + runoff
      evap = ( volw(3) - prevvolw(3) ) * mtomm
      bhzea = bhzea + evap
      infil = ( volw(4) - prevvolw(4) ) * mtomm
      bhzinf = bhzinf + infil
      drain = ( volw(numeq) - prevvolw(numeq) ) * mtomm
      bhzper = bhzper + drain

!     evaporation ratio based on accumulation for this hour
      surf_cum = bhzeasurf + bhzea
      evapratio = evapredu( surf_cum, evaplimit, vaptrans, bhzep )

!      theta(0) = theta(1)
      theta(0) = calctht0(bszlyd, theta, thetaw, evapratio)      !H-64,65,66

      bhrwc0(hourstep) = theta(0)/bulkden(1)

!     update prevvolw to carry over to next hour or day
      do kindex=1,5
          prevvolw(kindex) = volw(kindex)
      end do
      prevvolw(numeq) = volw(numeq)

!    output the hourly info here
!      if ((am0hfl .eq. 2) .or. (am0hfl .eq. 3) .or. (am0hfl .eq. 6) .or.&
!     &   (am0hfl .eq. 7)) then
!          swc = sum(volw(iminlay:imaxlay)) * mtomm
!          write(12,2050) hourstep,evap, runoff, drain,                  &
!     &       swc,theta(0),(theta(kindex), kindex=1,layrsn)
!      end if

!    Accumulating hourly soil wetness values
      do  kindex=0,layrsn
          if( (theta(kindex).le.0.001) .and. (kindex.gt.0) ) then
              write(*,*)'darcy:end theta<0.001',kindex,theta(kindex)
              call dvolw(neq,t,volw,netflux)
              write(*,*) 'lay',kindex,':',t,netflux(kindex+5),          &
     &           netflux(kindex+6), netflux(3)
              write(*,*) 'tcur,hu:',rwork(13), rwork(11)
!              stop
          endif
      end do

      if(     (raindepth.gt.0.0)                                        &
     &   .and.(tout.ge.rainend)                                         &
     &   .and.(bhzwid.le.0.0) ) then
          bhzwid = store( iminlay, imaxlay, prevvolw, volw, bszlyd )
      endif

      hourstep = hourstep + 1
!-------------------------------------
!  If not done yet, take another step.
!-------------------------------------      
      if(hourstep.le.24) goto 30

! completed the day

!     this section should be enabled to extend solution over
!     multiple days when no outside process changes water contents
!      if( beginday .lt. 100*daytosec )  then
!          beginday = beginday + daytosec
!      else
!          istate = 1   !-- initializes solution routines
!          beginday = 0.0
!          t = beginday
!          do kindex=1,5
!              prevvolw(kindex) = 0.0
!          end do
!          prevvolw(numeq) = 0.0
!      end if

      return
      end
