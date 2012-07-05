!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine dvolw(neqn,tsec,volw,wfluxn)

!     + + + PURPOSE + + +
!     Returns the values of net flux when given a simulation time and
!     state of the system. It is used by LSODA to integrate forward in time

!     + + + KEYWORDS + + +
!     soil water redistribution, evaporation, deep percolation, runoff

!     + + + ARGUMENT DECLARATIONS + + +
      integer neqn(*)
      real tsec, volw(*), wfluxn(*)

!     + + + ARGUMENT DEFINITIONS + + +
!     neqn(1) - number of Ordinary Differential Equations to be integrated
!     neqn(2) - flag to activate internal printing
!     tsec    - time in seconds since lsoda was initialized
!     volw(*) - array of state variables
!               (1-5) - rain, runoff, evap, infil, pond (m)
!               (6-imaxlay) - volume of water in each soil layer (m)
!               (imaxlay+1) - drainage (m)
!     wfluxn(*) - net flux rates corresponding to volw(*) (m/s)

!*** FUNCTION DECLARATIONS ***
      real unsatcond_bc
      real internode_wt_bc
      real vaporden, diffusive
      real airtempsin, satvappres
      real evapredu, matricpot_from_rh

!*** INCLUDED COMMON BLOCKS ***
      include 'p1werm.inc'
      include 'command.inc'
      include 'hydro/dvolwparam.inc'
      include 'hydro/vapprop.inc'

!*** LOCAL DECLARATIONS ***
      integer     lrx, imaxlay
      real theta(layrsn), wfluxr(layrsn+1)
      real conda(layrsn), potm(layrsn), cond_wt
      real intenspeak, diffua(layrsn), fluxv(layrsn), fluxw(layrsn)
      real airtemp, airvappres, airsatvappres, airhumid, airvapden
      real rainduration, tday, max_infil_rate, frac_pond_area
      real max_evap_rate, soil_evap_rate, pond_evap_rate
      real surface_vapor_rate, surface_capil_rate, air_mat_pot
      real pond_infil, rain_infil, rain_pond
      real lay_source(layrsn)
!     saved between calls
!     cond(layrsn), swm(layrsn),
!     soilrh(layrsn), soilvapden(layrsn), soildiffu(layrsn)

      real pi
      parameter( pi = 3.1415927 )

!*** LOCAL DEFINITIONS ***
!     locally, the soil layers go from 1 to layrsn. In the passed arrays,
!     the soil layers go from 6 to layrsn+5

!     set time of the beginning of the day
      tday = tsec - beginday

!     soil state
      imaxlay = layrsn + 5
      do lrx = 1,layrsn
          if( volw(lrx+5).ne.lastvolw(lrx+5) ) then
              theta(lrx) = volw(lrx+5)/tlay(lrx)
!             Brooks and Corey soil properties
              call matricpot_bc(theta(lrx), thetar(lrx), thetas(lrx),   &
     &                      airentry(lrx), lambda(lrx), thetaw(lrx),    &
     &                      theta80rh(lrx), soiltemp(lrx),              &
     &                      potm(lrx), soilrh(lrx) )
              swm(lrx) = potm(lrx) - depth(lrx)
              soilvapden(lrx) = vaporden( soiltemp(lrx), soilrh(lrx) )
              soildiffu(lrx) = diffusive(theta(lrx), thetas(lrx),       &
     &                               soiltemp(lrx), atmpres )
              cond(lrx) = unsatcond_bc(theta(lrx),thetar(lrx),          &
     &                    thetas(lrx), ksat(lrx),lambda(lrx))
          end if
      end do

!     calc the conductivity and rate of flow in for each layer
      do lrx = 2,layrsn
       if (layer_weighting == 2) then
          ! internodal conductivity, darcian means
          cond_wt = internode_wt_bc( cond(lrx-1), cond(lrx),            &
     &        ksat(lrx-1), ksat(lrx), lambda(lrx-1), lambda(lrx),       &
     &        tlay(lrx-1), tlay(lrx), airentry(lrx-1), airentry(lrx) )
          conda(lrx) = cond_wt*cond(lrx-1) + (1.0-cond_wt)*cond(lrx)
       else if (layer_weighting == 1) then
          ! proportional layer thickness weighted internodal conductivity
          conda(lrx) = (cond(lrx-1)*tlay(lrx-1)+cond(lrx)*tlay(lrx))    &
     &               / (2*dist(lrx))
       else ! if (layer_weighting == 0) then !Currently the default
          ! unweighted arithmetic average internodal conductivity
          conda(lrx) = 0.5 * ( cond(lrx-1) + cond(lrx) )
       end if
          diffua(lrx) = (soildiffu(lrx-1)*tlay(lrx-1)                   &
     &                + soildiffu(lrx)*tlay(lrx)) / (2*dist(lrx))
          fluxw(lrx) = (swm(lrx-1)-swm(lrx)) * conda(lrx)               &
     &                / dist(lrx)
          fluxv(lrx) = ((soilvapden(lrx-1)-soilvapden(lrx))             &
     &              * diffua(lrx) / denwat) / dist(lrx)
          wfluxr(lrx) = fluxw(lrx) + fluxv(lrx)
      end do
      wfluxr(layrsn+1) = cond(layrsn)
!      wfluxr(layrsn+1) = 0.0

!     boundary fluxes
!     rainfall intensity = wfluxn(1)
!     runoff rate = wfluxn(2)
!     evaporation rate = wfluxn(3)
!     infiltration rate = wfluxn(4)
!     change in ponded depth = wfluxn(5)
!     drainage rate = wfluxn(imaxlay+1)

!     rainfall intensity
      if(     (tday.ge.rainstart)                                       &
     &   .and.(tday.le.rainend)                                         &
     &   .and.(raindepth.gt.0.0) ) then
          rainduration = rainend - rainstart
          intenspeak = 2.0*raindepth/rainduration
          if( tday.lt.rainmid ) then
              wfluxn(1) = intenspeak                                    &
     &                  * (tday-rainstart)/(rainmid-rainstart)
          else if( tday.gt.rainmid ) then
              wfluxn(1) = intenspeak                                    &
     &                         * (tday-rainend)/(rainmid-rainend)
          else
              wfluxn(1) = intenspeak
          end if
      else
          wfluxn(1) = 0.0
      end if

      ! surface irrigation intensity
      if(      (tday .ge. surface_start)                                &
     &   .and. (tday .le. surface_end)                                  &
     &   .and. (surface_rate .gt. 0.0) ) then
          wfluxn(1) = wfluxn(1) + surface_rate
      end if

      pondmax = max(0.001, pondmax) !avoid div by zero
!     generate runoff if above retention limit (kind of like WEPP)
      if(volw(5).ge.pondmax) then
          wfluxn(2) = ((volw(5)-pondmax)**1.5)                          &
     &              * (8.0*gravconst*soilslope/dw_friction)**0.5        &
     &              /  slopelength
      else
          wfluxn(2) = 0.0   ! no runoff
      end if

!     determine fraction of soil area covered by ponding
      if( volw(5).ge.0.0 ) then
          frac_pond_area = min(1.0,(volw(5)/pondmax)**0.5)
      else
          frac_pond_area = 0.0
      end if

!     calculate maximum evaporation rate
!     this method using a lenday that is 12 hours (43200 sec) allows
!     running time continuously through many days
!      wfluxn(3) = max(0.0,evapamp * sin(pi*(tday-sunrise)/lenday))
!     this method assumes that tday and sunrise and sunset are in the
!     same day and lenday can be actual daylight hours
      ! evapamp accounts for 90% of daily evaporation
      ! 10% remaining is distributed over the remainder of the day
      max_evap_rate = evapdaypot * 0.1 / 86400.0
      if( tday.gt.sunrise.and.tday.lt.sunset ) then
          ! evaporation ratio based on accumulation
          ! (volw(3) must be maintained over multiple days, not zeroed daily
!          max_evap_rate = evapamp * sin(pi*(tday-sunrise)/lenday)       &
!     &       * evapredu(volw(3), evapendconstant, evaptrans, evapdaypot)
          max_evap_rate = max_evap_rate                                 &
     &                  + evapamp * sin(pi*(tday-sunrise)/lenday)
      endif

!     find air relative humidity from diurnal air temperature
!     and dewpoint temperature
      if( tday .lt. 21600 ) then
          airtemp = airtempsin( tday, airtmaxprev, airtmin )
      else if( tday .lt. 64800 ) then
          airtemp = airtempsin( tday, airtmax, airtmin )
      else
          airtemp = airtempsin( tday, airtmax, airtminnext )
      end if

      airvappres = satvappres(tdew)
      airsatvappres = satvappres(airtemp)
      airhumid = airvappres/airsatvappres
      airvapden = vaporden( airtemp, airhumid )

!      if( (soilrh(1).ge.airhumid).and.(airhumid.le.0.99) ) then
!      if( (airhumid.le.0.99) ) then
          ! calculate reduction in evaporation rate due to dry soil
!          soil_evap_rate = max_evap_rate                                &
!     &                   * (soilrh(1)-airhumid)/(1.0-airhumid)
!      else
!          soil_evap_rate = 0.0
!      end if

      surface_vapor_rate = ((soilvapden(1)-airvapden)                   &
     &                 * soildiffu(1) / denwat) / dist(1)

      air_mat_pot = matricpot_from_rh( airhumid, airtemp )
      surface_capil_rate = (swm(1) - air_mat_pot) *0.5*cond(1) / dist(1)

      soil_evap_rate = surface_vapor_rate + surface_capil_rate
      soil_evap_rate = max( -max_evap_rate, soil_evap_rate )
      soil_evap_rate = min( max_evap_rate, soil_evap_rate )

!      fluxw(1)
!      fluxv(1)
!      matricpot_from_rh( airhumid, airtemp )
!*** try this
!      soil_evap_rate = min( max_evap_rate,                              &
!     &                 ( (soilvapden(1)-airvapden)                      &
!     &                 * soildiffu(1) / denwat) / dist(1))
!
!      if( neqn(2).gt.0 ) then
!          write(*,*)'dvolw:tcddf',
!     &    tday, airtemp, soilvapden(1), airvapden, wfluxn(3)
!      endif
!      if( neqn(2).gt.0 ) then
!          if(max_evap_rate.gt.0.0)
!     &    write(*,*)'dvolw:evap',soilrh(1),airhumid, max_evap_rate
!      endif
!*** end try this

!     split evaporation between soil and pond
!     fluxv(1) is negative since this is a loss to the soil layer
!     consequently it mus be subtracted to make it an addition to evap.
      fluxv(1) = -soil_evap_rate * (1.0-frac_pond_area)
      pond_evap_rate = max_evap_rate * frac_pond_area
      wfluxn(3) = -fluxv(1) + pond_evap_rate

!     calculate max infiltration rate !note, the potential difference
!     should be (0.0-swm(1)), but this only works if we account for
!     hysteresis. As it is written, infiltration will not exceed
!     saturation. When hysteresis is incorporated, sorbing soil will
!     have an airentry potential of 0.0 again making them match.
      max_infil_rate = (airentry(1) - swm(1))                           &
     &               * 0.5 * (ksat(1)+cond(1))/dist(1)

!     add infiltration from both ponded area and rainfall area
      pond_infil = max_infil_rate * frac_pond_area
      rain_infil = min(wfluxn(1),max_infil_rate) * (1.0-frac_pond_area)
      wfluxn(4) = pond_infil + rain_infil
      fluxw(1) = wfluxn(4)

!     rate rain is added to the pond
      rain_pond = wfluxn(1) - rain_infil

!     rate change in pond depth (rain - infil - evap - runoff)
      wfluxn(5) = rain_pond - pond_infil - pond_evap_rate - wfluxn(2)

!     resultant flux at surface layer
      wfluxr(1) = fluxw(1) + fluxv(1)

!     drainage
      wfluxn(imaxlay+1) = wfluxr(layrsn+1)

!     calc the net flow into (+) or out of (-) each layer
      do lrx = 1, layrsn
          ! turn within layer source terms on with time
          if(   (tday.ge.source_start(lrx))                             &
     &    .and. (tday.le.source_end(lrx))) then
              lay_source(lrx) = source_rate(lrx)
          else
              lay_source(lrx) = 0.0
          end if
          ! find net flow
          wfluxn(lrx+5) = wfluxr(lrx) - wfluxr(lrx+1) + lay_source(lrx)
!          write(*,*) 'lay ', lrx, wfluxr(lrx), wfluxn(lrx)
          lastvolw(lrx+5) = volw(lrx+5)
      end do

!      if( neqn(2).gt.0 ) then
!          if( (wfluxr(1).lt.0.0).or.(wfluxr(2).gt.0.0) )
!     &    write(*,*) 'flx 1:',wfluxr(1), wfluxr(2)
!      endif

      return
      end

!     dummy jacobian matrix
      subroutine jac (neq, t, y, ml, mu, pd, nrowpd)
      integer neq, ml, mu, nrowpd
      real*4 t, y, pd
      dimension y(*), pd(nrowpd,*)
!     the full matrix case (jt = 1), ml and mu are ignored,
!     and the jacobian is to be loaded into pd in columnwise manner,
!     with df(i)/dy(j) loaded into pd(i,j).
!     in this case, dwfluxn(i)/dvolw(j) in pd(i,j)
      return
      end

! table lookup function archived here for reference
!      real*4 function afgen(numpts, table, xval)
!     Finds two entries in table( ,1) which bracket the value of the independent
!     variable, xval, and then linearly interpolates between the two corresponding
!     entries in Table( ,2) to find the corresponding value of the dependent
!     variable.
!
!      integer   numpts
!      real*4    table(numpts,2), xval
!
!      integer maxindex, minindex, midindex
!
!      maxindex = numpts
!      minindex = 0  !Place limit on one end of output.
!	  Set limits on values the function may take.
!	  if( xval.lt.table(1,1)) then
!          afgen = table(1,2)
!      else if( xval.gt.table(numpts,1)) then
!          afgen = table(numpts,2)
!      else
!          do while((maxindex-minindex).gt.1)
!              midindex = (maxindex+minindex)/2
!              if((table(numpts,1).gt.table(1,1))
!     &            .eqv.(xval.gt.table(midindex,1))) then
!                  minindex = midindex
!              else
!                  maxindex = midindex
!              end if
!          end do
!          afgen = (xval-table(minindex,1))
!     &          * (table(minindex+1,2)-table(minindex,2))
!     &          / (table(minindex+1,1)-table(minindex,1))
!     &          + table(minindex,2)
!          write(*,*) 'afgen int:', afgen
!      end if
!
!      return
!      end
!
