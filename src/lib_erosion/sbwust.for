!$Author$
!$Date$
!$Revision$
!$HeadURL$
!**********************************************************************
!      subroutine sbwust
!**********************************************************************
      subroutine sbwust (sf84, sdagd, sfcr, svroc, sflos, bffcv,        &
     &  wzzo, hrwc, hrwcw, wus, sf84ic, asvroc, dmlos,                  &
     &  wust, wusp, wusto, sf84mn, smaglos, smaglosmx,                  &
     &  wubsts, wucsts, wucwts, wucdts, sfcv)

!     + + + PURPOSE + + +
!     Calculate threshold soil surface friction velocity
!     as a function of ag size dist., aerodynamic roughness,
!   	  crust, rock, & flat biomass cover,and soil surface wetness

!     + + + ARGUMENT DECLARATIONS + + +
      real sf84, sdagd, sfcr, svroc, sflos, bffcv
      real wzzo, hrwc, hrwcw, wus, sf84ic, asvroc, dmlos
      real wust, wusp, sf84mn, smaglos
      real smaglosmx, wusto
      real wubsts, wucsts, wucwts, wucdts, sfcv

!     + + +  ARGUMENT DEFINITIONS + + +
!     sf84   - soil mass fraction in surface layer < 0.84 mm
!     sdagd - aggregate density (Mg/m^3)
!     sfcr  - fraction of crust cover.
!     svroc - updated surface vol. rock > 2.0 mm (m^3/m^3).
!     sflos - soil fraction loose material cover on crust (m^3/m^3)
!     bffcv - biomass fraction of flat cover (m^2/m^2)
!     wzzo  - aerodynamic roughness length of surface below canopy(mm)
!     hrwc  - soil water content on mass basis (at surface) (kg/kg).
!     hrwcw - soil water content on mass basis, at -1.5 MPa (kg/kg)
!     wus    - Soil surface friction velocity (m/s)
!     sf84ic- surface soil fraction <0.84 mm initial condition
!     asvroc- initial surface soil volume roc fraction
!     dmlos - mobile soil mass change from erosion of aggregated
!               surface (kg/m^2) (+)= gain, (-)= loss from surface.
!     wust  - friction velocity theshold for en (m/s)
!     wusp  - friction velocity threshold of tp and trans. cap.(m/s)
!     sf84mn- surface soil fraction <0.84 mm where wust= wus of ag.sfc.
!     smaglos- mobile soil reservoir of initial aggregated sfc.(kg/m^2)
!     smaglosmx - max mobile soil reservoir of aggregateed sfc.(kg/m^2)
!     wubsts - bare soil threshold friction velocity
!     wucsts - surface cover addition to bare soil threshold friction velocity
!     wucwts - surface wetness addition to bare soil threshold friction velocity
!     wucdts - aggregate density addition to bare soil threshold friction velocity
!     sfcv - fraction bare surface that does not emit

!     + + + LOCAL VARIABLES + + +
      real  b1, b2, wet_rat

!     + + + END SPECIFICATIONS + + +

!     calculate threshold (wusto) of bare, smooth surface with sf84ic 
!     for use in sbaglos edit 6-27-06 LH
      b1 = -0.179 + 0.225*(log(1.5))**0.891  ! approx -0.078337
      b2 = 0.3 + 0.06*0.5**1.2               ! approx  0.3261165
      wusto =1.7-1.35*exp(-b1-b2*((1-sf84ic)*(1-asvroc)+asvroc)**2)

!     calc fraction bare surface that does not emit
      sfcv = ((1 - sfcr)*(1 - sf84) + sfcr - sfcr*sflos)*(1 - svroc)    &
     &     + svroc

!     to avoid a zero value
      sfcv = sfcv + 0.0001
!     check for total cover.
!      if (sfcv < 1.0) then
!       calculate bare surface static threshold friction velocity
        b1 = -0.179 + 0.225*(alog(1 + wzzo))**0.891
        b2 = 0.3 + 0.06*wzzo**1.2
        wubsts = 1.7 - 1.35*exp(-b1-b2*sfcv**2)
        wusp   = 1.7 - 1.35*exp(-b1-b2*0.4**2)
 !     else
 !        wubsts = 1.85
 !        wusp   = 1.80
 !     endif

!     edit 07-17-01
!     calc change in threshold with flat cover
      if (bffcv .gt. 0) then
        wucsts = (1 - exp(-1.2*bffcv))*(exp(-0.3*sfcv))
      else
         wucsts = 0.
      endif

!     calc change in threshold vel with wetness
      wet_rat = hrwc / hrwcw
!      if ( wet_rat .gt. 0.3) then
!      if ( wet_rat .gt. 0.25) then  ! triggers at 1/4 of the wilting pt
      if ( wet_rat .ge. 0.0) then
          ! wucwts = 0.48 * wet_rat
          ! this modified curve closely matches the previous linear realationship
          ! in the 0.25 to 0.8 range, which is where the measured data are.
          ! (make sure the reference to the measured data is in the Tech Doc.)
          ! It is apparently not possible to measure threshold effects below
          ! 0.25 wetness ratio so whether the relationship should go smoothly
          ! through zero for a wetness ratio below 0.25 has not been determined.
          !!!!!! this function has a singularity and goes negative for
          ! values of wet_rat > 1.307674238
          ! wucwts = 1.0 / (11.906541 - 10.41204 * wet_rat**0.5)
          !!!! this function is better behaved
          wucwts = 0.58 * (exp(wet_rat) - 1.0 - 0.7*wet_rat*wet_rat)
      else
          wucwts = 0.
      endif

      ! After consultation with LH, it was decided that the adjustment
      ! to the friction velocity terms due to Agg. Density would be
      ! retained in the erosion code, but WEPS would default it to a
      ! value of 1.8 at this time.  The standalone code would then
      ! be able to modify that value if desired (with 1.8 being the
      ! suggested default value) - Mar. 15, 2006 - LEW

      !correct for ag density, (use constant sdagd=1.8, 5/28/03 LH)
      ! wucdts = -0.05275  !adjustment value if sdagd == 1.8

      wucdts = 0.3*(sqrt(sdagd/2.65)-1.0)


!     calc final static threshold friction velocity
      wust = wubsts + wucsts + wucwts + wucdts
      wusp = wusp   + wucsts + wucwts + wucdts

!     calculate: smaglosmx; update: smaglos, sf84mn
          call sbaglos (wus, wust, wusto, sf84ic, asvroc,               &
     &                      smaglosmx, smaglos, sf84mn, sf84)

      return
      end
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
