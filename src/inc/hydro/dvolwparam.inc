!$Author$
!$Date$
!$Revision$
!$HeadURL$
!     defines for soil water properties
!     number of layers plus six variables
!     ponded depth, cumulative evaporation, cumulative runoff,
!     cumulative infiltration, cumulative drainage,
!     and cumulative rainfall
!     iwork and rwork arrays set for max 100 soil layers
!     plus 6 auxilary variables (neq = 106, ml=3, mu=1)
!     lrw = 22 + 10*NEQ + (2*ML+MU)*NEQ   if JT = 4 or 5.
!     liw = 20 + NEQ
      integer  lrw, liw
      parameter (liw = 126)
      parameter (lrw = 1824)
      integer layrsn, istate, iwork(liw)
      real t, rwork(lrw), tlay(mnsz),dist(mnsz),depth(mnsz),            &
     &       thetas(mnsz), thetar(mnsz), ksat(mnsz),                    &
     &       airentry(mnsz), lambda(mnsz),                              &
     &       theta80rh(mnsz), thetaw(mnsz),                             &
     &       soiltemp(mnsz),                                            &
     &       airtmaxprev, airtmin, airtmax, airtminnext, tdew,          &
     &       beginday, lenday, sunrise, sunset,                         &
     &       evapendconstant, evapdaypot, evaptrans, evapamp,           &
     &       raindepth, rainstart, rainend, rainmid,                    &
     &       soilslope, pondmax, dw_friction, slopelength,              &
     &       atmpres,                                                   &
     &       surface_rate, surface_start, surface_end,                  &
     &       source_rate(mnsz), source_start(mnsz), source_end(mnsz),   &
     &       lastvolw(mnsz+7), cond(mnsz), swm(mnsz),                   &
     &       soilrh(mnsz), soilvapden(mnsz), soildiffu(mnsz),           &
     &       prevvolw(mnsz+7)


      common / dvolwparam /                                             &
     & layrsn, istate, iwork,                                           &
     & t, rwork, tlay, dist, depth,                                     &
     & thetas, thetar, ksat,                                            &
     & airentry, lambda,                                                &
     & theta80rh, thetaw,                                               &
     & soiltemp,                                                        &
     & airtmaxprev, airtmin, airtmax, airtminnext, tdew,                &
     & beginday, lenday, sunrise, sunset,                               &
     & evapendconstant, evapdaypot, evaptrans, evapamp,                 &
     & raindepth, rainstart, rainend, rainmid,                          &
     & soilslope, pondmax, dw_friction, slopelength,                    &
     & atmpres,                                                         &
     & surface_rate, surface_start, surface_end,                        &
     & source_rate, source_start, source_end,                           &
     & lastvolw, cond, swm,                                             &
     & soilrh, soilvapden, soildiffu,                                   &
     & prevvolw

      save :: /dvolwparam/

!     lrw             - lsoda: length of real work array
!     liw             - lsoda: length of integer work array
!     layrsn          - number of soil layers
!     istate          - LSODA input control and result flag, preserved between days
!     iwork(20+numeq) -  lsoda: integer work array
!     t               - time since last LSODA initialization, preserved between days
!     rwork(22+10*numeq+(2*ml+mu)*numeq - lsoda: real work array
!     tlay(mnsz)      - soil layer thickness (m)
!     dist(mnsz)      - distance from center of soil layer(index-1)
!                       to soil layer(index) (m)
!     depth(mnsz)     - distance from surface to center of soil layer (m)
!     thetas(mnsz)    - saturated volumetric water content
!     thetar(mnsz)    - volumetric water content where conductivity ceases (residual)
!     ksat(mnsz)      - saturated hydraulic conductivity (m/s)
!     airentry(mnsz)  - Brooks and Corey air entry potential (m)
!     lambda(mnsz)    - Brooks and Corey pore interaction exponent
!     theta80rh(mnsz) - volumetric water content when soil air is 80 percent
!                       relative humidity (obtained from soil adsorption isotherm estimates)
!     thetaw(mnsz)    - wilting point (15 bar) volumetric water content
!     soiltemp(mnsz)  - soil temperature (C)
!     airtmaxprev     - maximum air temperature of the previous day (C)
!     airtmin         - minimum air temperature today (C)
!     airtmax         - maximum air temperature today (C)
!     airtminnext     - minimum air temperature of the next day (C)
!     tdew            - dewpoint temperature (C)
!     beginday        - time of the beginning of the day (seconds)
!     lenday          - length of the daylight (seconds)
!     sunrise         - time of sunrise (seconds)
!     sunset          - time of sunset (seconds)
!     evapendconstant - cumulative evaporation where falling rate stage begins (m)
!     evapdaypot      - potential evaporation for day (m)
!     evaptrans       - soil vapor transmissivity (m/d^0.5)
!     evapamp         - amplitude of the daily evaporation curve (m)
!                       calculated from pi*dailydepth(m)/lenday/2.0
!     raindepth       - total depth of a rainfall event (m)
!     rainstart       - time of the start of a rainfall event (seconds)
!     rainend         - time of the end of a rainfall event (seconds)
!     rainmid         - time of the peak of a rainfall event (seconds)
!     soilslope       - slope of the soil surface (m/m)
!     pondmax         - maximum depth of soil surface ponding before runoff (m)
!     dw_friction     - darcy weisbach friction factor for runoff flow rate calculation
!     slopelength     - length of sheet flow runoff (overland flow element) (m)
!     atmpres         - pressure of the ambient atmosphere (kPa)
!     surface_rate    - rate of surface water flow into the soil (m/sec)
!     surface_start   - time of the start of surface flow rate (seconds)
!     surface_end     - time of the end of surface flow rate (seconds)
!     source_rate     - rate of water source flow into the soil (m/sec)
!     source_start    - time of the start of source flow rate (seconds)
!     source_end      - time of the end of source flow rate (seconds)
!     lastvolw(mnsz+6) - value saved from previous entry into dvolw
!     cond(mnsz)      - value used from previous entry into dvolw
!     swm(mnsz)       - value used from previous entry into dvolw
!     soilrh(mnsz)    - value used from previous entry into dvolw
!     soilvapden(mnsz) - value used from previous entry into dvolw
!     soildiffu(mnsz) - value used from previous entry into dvolw
!     prevvolw(numeq) - initial values of water content at beginning of hour
!                       indexes greater than layrsn may be updated hourly
