!$Author$
!$Date$
!$Revision$
!$HeadURL$

module process_mod

  contains

    subroutine sbqout (SURF_UPD_FLG, wus, wust, wusp, sf10, sf84, sf200, &
                       szcr, sfcr, sflos, smlos, szrgh, sxrgs, sxprg, slrr, &
                       sfcla, sfsan, sfvfs, svroc, brsai, bzht, bffcv, time, &
                       canag, cancr, sf10an, sf10en, sf10bk, lx, qi, qssi, q10i, &
                       dmlos, sf84mn, sf84ic, sf10ic, asvroc, smaglosmx, qo, qsso, q10o )

!     +++ PURPOSE +++
!     calculate the saltation/creep, suspension, and PM-10 discharge
!     from each control volume

!     +++ ARGUMENT DECLARATIONS +++
      integer, intent(in) :: SURF_UPD_FLG    !Surface update flag (1=on, 0=off)
      real, intent(in) :: wus          ! friction velocity (m/s)
      real, intent(in) :: wust         ! friction velocity threhold at emission (m/s)
      real, intent(in) :: wusp         ! friction velocity threshold at transport cap.(m/s)
      real, intent(inout) :: sf10         ! soil fraction less than 0.010 mm (PM10)
      real, intent(inout) :: sf84         ! soil fraction less than 0.84 mm
      real, intent(inout) :: sf200        ! soil fractions less than 2.0 mm
      real, intent(inout) :: szcr         ! soil crust thickness
      real, intent(inout) :: sfcr         ! soil fraction  area crusted
      real, intent(inout) :: sflos        ! soil fraction area of loose soil (only on crust)
      real, intent(inout) :: smlos        ! soil mass of loose soil (only on crust)
      real, intent(inout) :: szrgh        ! soil ridge height (mm)
      real, intent(in) :: sxrgs        ! plant stem row spacing (mm)
      real, intent(in) :: sxprg        ! soil ridge spacing parallel wind direction (mm)
      real, intent(inout) :: slrr         ! soil random roughness (mm)
      real, intent(in) :: sfcla        ! soil fraction clay by mass
      real, intent(in) :: sfsan        ! soil fraction sand by mass
      real, intent(in) :: sfvfs        ! soil fraction very fine sand by mass (0.05-0.1 mm)
      real, intent(inout) :: svroc        ! soil fraction rock >2.0 mm by volume
      real, intent(in) :: brsai        ! biomass stem area index
      real, intent(in) :: bzht         ! biomass height (m)
      real, intent(in) :: bffcv        ! biomass fraction flat cover
      real, intent(in) :: time         ! time step (seconds)
      real, intent(in) :: canag        ! coeficient of aggregate abrasion (1/m)
      real, intent(in) :: cancr        ! coefficient of crust abrasion (1/m)
      real, intent(in) :: sf10an       ! soil fraction of pm10 in abraded suspension size
      real, intent(in) :: sf10en       ! soil fraction of pm10 in emitted suspension size
      real, intent(in) :: sf10bk       ! soil fraction of pm10 in saltion/creep breakage
      real, intent(in) :: lx           ! "effective" length of erosion cell (m)
      real, intent(in) :: qi           ! input to C.V. of saltation, creep (kg/m*s)
      real, intent(in) :: qssi         ! input to C.V. of suspension (kg/m*2)
      real, intent(in) :: q10i         ! input to C.V. of pm-10 (kg/m*2)
      real, intent(inout) :: dmlos        ! change in loose mass on aggregated sfc. (kg/m^2) (from the beginning of the erosion event)
      real, intent(in) :: sf84mn       ! soil surface fraction less than 0.84 below which no emission occurs
      real, intent(in) :: sf84ic       ! soil surface fraction less than 0.84 initially
      real, intent(in) :: sf10ic       ! soil surface fraction less than 0.10 initially
      real, intent(in) :: asvroc       ! soil surface volume rock at start of event
      real, intent(in) :: smaglosmx    ! max mobile soil reservoir of aggregated sfc.(kg/m^2)
      real, intent(out) :: qo           ! output from C.V. of saltation, creep (kg/m*s)
      real, intent(out) :: qsso         ! output from C.V. of suspension (kg/m*s)
      real, intent(out) :: q10o         ! output from C.V. of pm-10 (kg/m*s)

!     +++ PARAMETERS +++
      real, parameter :: cmp = 0.0001  ! mixing parameter coef.
      real, parameter :: ctf = 1.2     ! ridge trapping fraction
      real, parameter :: cdp = 0.005   ! deposition coef. for suspension
      real, parameter :: c10dp = 0.001 ! deposition coef. for pm10

!     +++ LOCAL VARIABLES +++
      real :: cen      ! coef. of emission (1/m)
      real :: sfa12    ! soil fraction area with shelter angle > 12 deg.
      real :: sfcv     ! soil fraction clod & crust cover
      real :: srrg     ! ratio of ridge spacing to ridge spacing parallel wind direction
      real :: qen      ! transport capacity using emission threshold (kg/m*s)
      real :: qcp      ! transport capacity using trans. cap. threshold (kg/m*s)
      real :: qi_tcap  ! input limited by transport capacity (kg/m*s)
      real :: ci       ! coef. of saltation interception by biomass stems (1/m) edit LH 8-29-05
      real :: sfsn     ! fraction of soil saltation and creep in saltation
      real :: fan      ! fraction saltation impacting clods & crust
      real :: fanag    ! fraction saltation impacting clods
      real :: fancr    ! fraction saltation impacting crust
      real :: fancan   ! product of fan & can ie. effective abrasion coef. (1/m)
      real :: sfssen   ! soil fraction to suspension from emission
      real :: sfssan   ! soil fraction from abrasion to emission
      real :: cbk      ! suspension breakage coefficient from saltation
      real :: a,b,c    ! composite parameters for saltation discharge soln eq.
      real :: f, g     ! composite parameters for suspension discharge soln eq.
      real :: h, k     ! composite parameter for pm-10 discharge eq.
      real :: s, p     ! intermediate calc. param. for saltation discharge soln.
      real :: t1,t2    ! intermediate calc. param. for salt. and susp. soln eq.
      real :: ct       ! total trap coefficient
      real :: cm       ! soil mixing parameter for suspension
      real :: mntime   ! smallest time step where one of the surface updates reaches a limit

!     +++ END SPECIFICATIONS +++

      ! calc. fraction of area with shelter > 12 degrees
      sfa12 = frac_area_sheltered( slrr, sxprg, szrgh )

      ! The following changes were made based upon Larry Hagen's email
      ! me on Fri, Jan. 31, 2003 to represent dynamic threshold instead
      ! assuming a static threshold - LEW

      ! calc. transport cap. for emission edit 5/30/01 LH
      qen = transport_capacity( wus, wust, sfa12 )
      ! calc. transport cap. for trapping
      qcp = transport_capacity( wus, wusp, sfa12 )
      if (qcp .lt. 0.0) then
         qcp = 0.001
      endif

      ! test for trap region with both saltation & suspension deposition
      if (qen < 0.0001) then                 !edit ljh 1-24-05
        ! this is a trapping only region
        qen  = 0.00001
        qo   = 0.0
        qi_tcap   = qo          !no abrasion when no transport edit 8-30-06
        ct   = 1.0         !traps all incoming saltation discharge
        fancan = 0.0
        qsso = qssi*exp(-cdp*lx)
        q10o = q10i*exp(-c10dp*lx)
        go to 90
      elseif (qen .le. qi) then
        ! this is a saltation trapping region with suspension emission
        ! We have set qi_tcap to qen to calculate suspension and abrasion
        ! and then used the real qi value to calc. the deposition. 12-5-2000 LH
        qi_tcap = qen
      else
        qi_tcap = qi
      endif

      ! fraction emission coef. reduction by roughness and area not emitting
      if( sf84 .le. sf84mn) then        ! edit LH 6-27-06
         sfcv = fraction_area_noemit( 0.0, sfcr, sflos, svroc )
      else
         sfcv = fraction_area_noemit( sf84, sfcr, sflos, svroc )
      endif

      ! Calc. emission params for saltation/creep

      ! coefficient of emission
      cen = emission_coef( sfcv, sfa12, bffcv )

      ! soil fraction suspension in emitted soil; lower for loose on cr
      sfssen = (sf10/(sf200+0.001))*(1. - sfcr*0.8)
      ! soil mixing parameter for suspension
      cm = cmp*sfssen

      ! calc. trap params. for saltation/creep
      ! calc. coef of stem interception for plants above 1 mm height  edited 3-29-01 LH
      if (bzht .lt. 0.001) then
        ci = 0.
      else
        if (sxrgs .gt. 10) then
          srrg = sqrt(sxrgs/sxprg)
        else
          srrg = 1.0
        endif
        ci = 0.005*srrg*(1 - exp(-0.5*brsai/bzht))*(1 - exp(-50*bzht)) !LH 8-29-05
      endif

      ! Calc. abrasion params. for saltation/creep
      ! soil fraction saltation/creep in saltation
      sfsn = 1

      ! fraction of saltation abrading clods, crust and loose
      fan = (exp(- 4.0*bffcv))*(exp(-3*svroc))*sfsn
      fan = max(fan,0.)
      ! fraction abrading aggregates
      fanag = (1. - sf84)*(1. - sfcr)*fan
      ! fraction abrading crust
      fancr = (sfcr - sflos*sfcr)*fan
      ! sum
      fancan = fancr*cancr + fanag*canag
                                    !test3
      if (fancan .lt. 0.0001) fancan = 0.0001
      ! calc fraction of abraded of suspension size
      sfssan =  (0.4 - 4.83*sfcla + 27.18*sfcla**2 - 53.7*sfcla**3 + 42.25*sfcla**4 - 10.7*sfcla**5)

      ! set upper limit on sfssan for high sand content
      sfssan = min(sfssan, (1.0 - (sfsan-sfvfs)))

      ! calc suspension breakage coef. for saltation creep edit 6-9-01 LH
      cbk = 0.11*canag*(1. - (sfsan-sfvfs))*sfsn

      ! calc total trap coef.
      if ((qcp .lt. qen) .and. ((slrr .gt. 10.) .or. (szrgh .gt. 50.))) then
        ct = (1. - sfssen)*cen*ctf*(qen - qcp)/qen
      else
        ct = 0.
      endif

      ! assemble composite params. for saltation/creep
      a = cen *(1. - sfssen)*qen
      b = (1. - sfssan)*fancan - (1. - sfssen)*cen - cbk - ci - ct
      c = (1. - sfssan)*fancan*1./qen

      ! solve for saltation/creep out
      ! collect variables
      s = sqrt(4.*a*c + b**2.)
      p = (-2.*c*qi_tcap + b)/s
      ! change p-values that are out of range, math range restriction: (-1 < p < 1) edit 7-18-01 LH
      if (p .le. -1.) then
         t1 = -20
      elseif (p .ge. 1.) then
         t1 = 20
      else
         t1 = (s/2.0)*(-lx) + 0.5*alog((1. + p)/(1. - p))
      endif
      ! calculate atanh
      qo = (s/(2.*c))*(-tanh(t1) + b/s)

      ! assemble composite params. for suspension
      f = sfssen*cen*qen*(1.0 - 0.1*brsai)    !edit 8-29-05 for suspension interception
      ! added last term to intercept some ss LH edit  6-11-01, changed 8-29-05
      g = (sfssan*fancan - sfssen*cen + cbk + cm)*(1 - 0.1*brsai)

      ! trap to prevent over range of exp
      if ((s*lx) .gt. 40.0) then
         s = 40.0/lx
      endif

      ! solve for suspension out
      ! collect variables
      if( p .gt. 1.0 ) then
         t2 = alog(2.0)
      else if( p .lt. -1 ) then
         t2 = alog(exp(s*lx)*2.0)
      else
         t2 = alog(exp(s*lx)*(1.-p)+ p + 1.)
      end if
      qsso=qssi+(1./(2.*c))*((-g*s+g*b+2.*f*c)*lx+2.*g*(-alog(2.0)+t2))

      ! assemble composite params. for PM-10
      h = sf10en*f
      k = sf10an*sfssan*fancan - sf10en*sfssen*cen + sf10bk*cbk + sf10en*cm

      ! solve for PM-10 out (similar to suspension out)
      q10o=q10i+(1./(2.*c))*((-k*s+k*b+2.*h*c)*lx+2.*k*(-alog(2.0)+t2))

      ! SURFACE UPDATE EQUATIONS

      ! now added as a commandline argument to tsterode - LEW
      ! execute this section if flag is set
   90 if (SURF_UPD_FLG .eq. 1) then

        mntime = update_surface( time, qi, qi_tcap, qssi, qo, qsso, lx, fan, fancr, cancr, fancan, sf84mn, sf84ic, sf10ic, asvroc, &
                             smaglosmx, ct, szcr, sfcr, sflos, smlos, dmlos, sf10, sf84, sf200, svroc, szrgh, slrr )
      endif

    end subroutine sbqout

    subroutine sbwust (sf84, sdagd, sfcr, svroc, sflos, bffcv, wzzo, hrwc, hrwcw, sf84ic, asvroc, &
                       wust, wusp, wusto, wubsts, wucsts, wucwts, wucdts, sfcv)

!     + + + PURPOSE + + +
!     Calculate threshold soil surface friction velocity
!     as a function of ag size dist., aerodynamic roughness,
!   	  crust, rock, & flat biomass cover,and soil surface wetness

!     + + + ARGUMENT DECLARATIONS + + +
      real, intent(in) :: sf84   ! soil mass fraction in surface layer < 0.84 mm
      real, intent(in) :: sdagd  ! aggregate density (Mg/m^3)
      real, intent(in) :: sfcr   ! fraction of crust cover.
      real, intent(in) :: svroc  ! updated surface vol. rock > 2.0 mm (m^3/m^3).
      real, intent(in) :: sflos  ! soil fraction loose material cover on crust (m^3/m^3)
      real, intent(in) :: bffcv  ! biomass fraction of flat cover (m^2/m^2)
      real, intent(in) :: wzzo   ! aerodynamic roughness length of surface below canopy(mm)
      real, intent(in) :: hrwc   ! soil water content on mass basis (at surface) (kg/kg).
      real, intent(in) :: hrwcw  ! soil water content on mass basis, at -1.5 MPa (kg/kg)
      real, intent(in) :: sf84ic ! surface soil fraction <0.84 mm initial condition
      real, intent(in) :: asvroc ! initial surface soil volume roc fraction
      real, intent(out) :: wust   ! friction velocity theshold for en (m/s)
      real, intent(out) :: wusp   ! friction velocity threshold of tp and trans. cap.(m/s)
      real, intent(out) :: wusto  ! friction velocity threshold of bare smooth surface with sf84ic (for in sbaglos) (m/s)
      real, intent(out) :: wubsts ! bare soil threshold friction velocity
      real, intent(out) :: wucsts ! surface cover addition to bare soil threshold friction velocity
      real, intent(out) :: wucwts ! surface wetness addition to bare soil threshold friction velocity
      real, intent(out) :: wucdts ! aggregate density addition to bare soil threshold friction velocity
      real, intent(out) :: sfcv   ! fraction bare surface that does not emit

!     + + + LOCAL VARIABLES + + +
      real  b1, b2, wet_rat

!     + + + END SPECIFICATIONS + + +

      ! calculate threshold (wusto) of bare, smooth surface with sf84ic for use in sbaglos edit 6-27-06 LH
      b1 = -0.179 + 0.225*(log(1.5))**0.891  ! approx -0.078337
      b2 = 0.3 + 0.06*0.5**1.2               ! approx  0.3261165
      wusto = 1.7 - 1.35 * exp( -b1 - b2*( (1-sf84ic)*(1-asvroc) + asvroc )**2 )

      ! calc fraction bare surface that does not emit
      sfcv = fraction_area_noemit( sf84, sfcr, sflos, svroc )

      ! to avoid a zero value
      sfcv = sfcv + 0.0001
      ! check for total cover.
      !  if (sfcv < 1.0) then
      !   calculate bare surface static threshold friction velocity
      b1 = -0.179 + 0.225*(alog(1 + wzzo))**0.891
      b2 = 0.3 + 0.06*wzzo**1.2
      wubsts = 1.7 - 1.35*exp(-b1-b2*sfcv**2)
      wusp   = 1.7 - 1.35*exp(-b1-b2*0.4**2)
      ! else
      !    wubsts = 1.85
      !    wusp   = 1.80
      ! endif

      ! edit 07-17-01
      ! calc change in threshold with flat cover
      if (bffcv .gt. 0) then
         wucsts = (1 - exp(-1.2*bffcv))*(exp(-0.3*sfcv))
      else
         wucsts = 0.
      endif

      ! calc change in threshold vel with wetness
      wet_rat = min(88.721, hrwc / hrwcw)
      !  if ( wet_rat .gt. 0.3) then
      !  if ( wet_rat .gt. 0.25) then  ! triggers at 1/4 of the wilting pt
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


      ! calc final static threshold friction velocity
      wust = wubsts + wucsts + wucwts + wucdts
      wusp = wusp   + wucsts + wucwts + wucdts

    end subroutine sbwust

    subroutine sbpm10 (seags, secr, sfcla, sfsan, awzypt, canag, cancr, sf10an, sf10en, sf10bk)

!     + + + PURPOSE + + +
!     Calculates abrasion coefficients and PM10 fractions in
!       sources of suspended soil

!     + + + ARGUMENTS + + +   
      real, intent(in) :: seags  ! aggreg. stability [Ln(J/Kg)]
      real, intent(in) :: secr   ! crust stabitlity [Ln(J/Kg)]
      real, intent(in) :: sfcla  ! soil surface fraction clay
      real, intent(in) :: sfsan  ! soil surface faction sand
      real, intent(in) :: awzypt ! annual average precipitation (mm)
      real, intent(out) :: canag  ! coefficent of abrasion of aggregates (1/m)
      real, intent(out) :: cancr  ! coefficient of abrasion of crust (1/m)
      real, intent(out) :: sf10an ! fraction pm10 in abraded supension size soil
      real, intent(out) :: sf10en ! fraction pm10 in emitted suspension size soil
      real, intent(out) :: sf10bk ! fraction pm10 in breakage from saltion size soil

!     + + + LOCAL VARIABLES + + +
      real :: ratio ! silt/clay^2
      real :: cla   ! fraction clay with restricted range
      real :: lnc   ! alog (cla)
      real :: sfsil ! soil fraction silt
      real :: ppt   ! annual average precip restricted to 100-800 mm

!     + + +  END SPECIFICATIONS + + +

!       calc. abrasion coefficients
        canag = exp(-2.07-0.077*seags**2.5-0.119*alog(seags))
        if (secr .eq. 0.0) then  ! No crust stability value specified
          cancr = 0.0
          write(0,*) "Warning:  Crust stability value is set to zero"
        else
          cancr = exp(-2.07-0.077*secr**2.5-0.119*alog(secr))
        end if

!       calc. pm10 fractions in suspended soil
        sf10an = 0.0116 + 0.00025/(canag+0.001)

        sfsil = 1 - sfsan - sfcla
        ratio = sfsil/(sfcla + 0.0001)**2
        ratio = min(300.0, ratio)
        sf10en = 0.0067 + 0.0000487*ratio - 0.0000044*awzypt

        cla = min(0.42,max(0.017,sfcla))  !restrict clay range
        lnc = alog(cla)
        ppt = min(800.0,max(100.0,awzypt))    !restrict precip range
        sf10bk = -0.201-(0.52+(0.422+(0.1395+0.0156*lnc)*lnc)*lnc)*lnc + 0.131*exp(-ppt/175.6)

    end subroutine sbpm10

    subroutine sbaglos (wus, wust, wusto, sf84ic, asvroc, smaglosmx, smaglos, sf84mn, sf84)

!     + + + PURPOSE + + +
!     calc. minimum erodible fraction (sf84mn) needed to stop emission
!     on aggregated portion of surface at current threshold friction velocity
!     that occurs when smaglos is removed.

!     calc. potential mobile soil reservoir (smaglosmx) for a smooth
!     surface 0.8 m/s friction velocity and sf84ic that armors clod surface.
!
!     calc. available mobile reservoir (smaglos) of sf84 for current surface 
!     based on wus-wust ratio for 

!     + + + ARGUMENT DECLARATIONS + + +
      real, intent(in) :: wus       ! friction velocity (m/s)
      real, intent(in) :: wust      ! friction velocity theshold for en (m/s)
      real, intent(in) :: wusto     ! threhold friction velocity = wus minus flat biomass and wetness effects (m/s)
      real, intent(in) :: sf84ic    ! surface soil fraction <0.84 mm initial condition
      real, intent(in) :: asvroc    ! surface soil volume rock (m^3/m^3)
      real, intent(out) :: smaglosmx ! max mobile soil reservoir of aggregated sfc.(kg/m^2)
      real, intent(out) :: smaglos   ! potential mobile soil reservoir of aggregated sfc.(kg/m^2)
      real, intent(out) :: sf84mn    ! surface soil fraction <0.84 mm where wust= wus of ag.sfc.
      real, intent(in) :: sf84      ! soil mass fraction in surface layer < 0.84 mm

!     + + + END SPECIFICATIONS + + +

      ! edit LH 6-26-06
      ! calc. max mobile soil at wus = 0.75 m/s for bare, smooth surface
      ! sf84 is assumed = 0 after this mass removal.
      smaglosmx = exp(2.708 - 7.603*((1-sf84ic)*(1-asvroc) + asvroc))

      ! reduce max mobile soil for roughness, cover, etc.
      smaglos = smaglosmx * (wus - wust) / (0.75 - min(wusto,wust))
      smaglos = max(0.0, smaglos)
      !smaglos = min(smaglosmx, smaglos)

      ! find sf84 when smaglos has been removed from control volume mass(denominator).
      ! in sbqout emission from cloddy surface goes to zero at sf84mn
      if ((smaglos .eq. 0.0) .or. (sf84 .eq. 0.0)) then
          sf84mn = sf84
      !elseif (smaglosmx .le. smaglos) then
      !    sf84mn = 0.05
      else
       sf84mn=(smaglosmx - smaglos)/(smaglosmx/(sf84ic*(1.001-asvroc)))
       sf84mn = max(0.0, sf84mn)
      endif

    end subroutine sbaglos

    subroutine sbsfdi (slagm, s0ags, slagn, slagx, sldi, sfdi)

!     +++ PURPOSE +++
!     calc soil mass fraction (sfdi) < diameter (sldi)
!     given modified lognormal distribution parameters

!     +++ ARGUMENT DECLARATIONS +++
      real, intent(in) :: slagm ! aggregate distribution geometric mean diameter (mm).
      real, intent(in) :: s0ags ! aggregate distribution geometric standard deviation.
      real, intent(in) :: slagn ! aggregate distribution lower limit (mm).
      real, intent(in) :: slagx ! aggregate distribution upper limit (mm).
      real, intent(in) :: sldi  ! soil diameter in distribution (mm)
      real, intent(out) :: sfdi  ! soil mass fraction < sldi

!     +++ LOCAL VARIABLES +++
      real slt

!     +++ FUNCTIONS CALLED+++
      real erf

!     +++ END SPECIFICATIONS +++

!     calc soil mass < sldi

      if (sldi .lt. slagx .and. sldi .gt. slagn) then
        slt = ((sldi - slagn)*(slagx - slagn))/((slagx - sldi)*slagm)
        sfdi = 0.5*(1 + erf(alog(slt)/(sqrt(2.0)*alog(s0ags))))
      elseif (sldi .ge. slagx) then
        sfdi = 1.0
      else
        sfdi = 0.0
      endif

    end subroutine sbsfdi

    function frac_area_sheltered( slrr, sxprg, szrgh ) result( sfa12 )
      real, intent(in) :: slrr         ! soil random roughness (mm)
      real, intent(in) :: sxprg        ! soil ridge spacing parallel wind direction (mm)
      real, intent(in) :: szrgh        ! soil ridge height (mm)
      real :: sfa12    ! soil fraction area with shelter angle > 12 deg.

      real :: sargc    ! soil angle of ridge shelter weibull 'c' (deg.)
      real :: sarrc    ! soil angle of random roughness weibull 'c'(deg.)
      double precision :: sfrg12   ! sheltered soil fraction for ridge
      double precision :: sfrr12   ! sheltered soil fraction for random roughness

      sarrc = 2.3 * sqrt (slrr)
      sfrr12 = exp(-(12.0/sarrc)**0.77)
      if ((sxprg .gt. 10.0) .and. (szrgh .gt. 1.0)) then
         sargc = 65.4*(szrgh/sxprg)**0.65
         sfrg12 = exp(-(12.0/sargc)**0.77)
      else
          sfrg12 = 0.0
      endif
      sfa12 = (1.0 - sfrg12)*(sfrr12) + sfrg12

    end function frac_area_sheltered

    function cover_loose_mass( smlos, szrgh, slrr ) result( sflos )
      real, intent(in) :: smlos        ! soil mass of loose soil (only on crust)
      real, intent(in) :: szrgh        ! soil ridge height (mm)
      real, intent(in) :: slrr         ! soil random roughness (mm)
      real :: sflos        ! soil fraction area of loose soil (only on crust)

      real :: sz       ! tmp variable for roughness
      real :: crlos    ! factor that decreases loose cover with roughness
      real :: tmp      ! intermediate calculation value

      if (smlos > 0) then
        ! coef. for cover of loose mass (same as SOIL eq. s-24,2-25)
        sz = max(szrgh, 4.0*slrr)
        crlos = exp(-0.08*sz**0.5)

        tmp = 3.5*smlos**1.5
        if(tmp.gt.80.) then       !test and prevent underflow condition
          sflos = crlos
        else
          sflos = (1.0 - exp(-tmp))*crlos
        endif
      else
        sflos = 0
      endif

    end function cover_loose_mass

    function fraction_area_noemit( sf84, sfcr, sflos, svroc ) result( sfcv )
      real, intent(in) :: sf84         ! soil fraction less than 0.84 mm
      real, intent(in) :: sfcr         ! soil fraction  area crusted
      real, intent(in) :: sflos        ! soil fraction area of loose soil (only on crust)
      real, intent(in) :: svroc        ! soil fraction rock >2.0 mm by volume
      real :: sfcv     ! soil fraction clod & crust cover

      sfcv = ((1.-sfcr)*(1.-sf84) + sfcr - sflos*sfcr)*(1.-svroc) + svroc

    end function fraction_area_noemit

    function emission_coef( sfcv, sfa12, bffcv ) result( cen )
      real, intent(in) :: sfcv         ! soil fraction clod & crust cover
      real, intent(in) :: sfa12        ! soil fraction area with shelter angle > 12 deg.
      real, intent(in) :: bffcv        ! biomass fraction flat cover
      real :: cen      ! coef. of emission (1/m)

      real :: ceno     ! Coef. of emission for bare, loose, erodible soil (1/m)
      real :: renb     ! reduction in emission on bare soil by cover and rough.
      real :: renv     ! reduction in emission on bare soil by flat biomass

      ceno = 0.06
      ! fraction emission coef. reduction by flat biomass
      renv = 0.075 + 0.934*exp(-bffcv/0.149)
      ! edit 7-17-01  LH
      renb = (-0.051 + 1.051*exp(-sfcv/0.33050512))*(1 - sfa12)
      cen = ceno*renv*renb
      ! if (cen .lt. 0.0001) cen = 0.0

    end function emission_coef

    function transport_capacity( wus, wusthr, sfa12 ) result( qcap )
      real, intent(in) :: wus          ! friction velocity (m/s)
      real, intent(in) :: wusthr       ! friction velocity threshold (m/s)
      real, intent(in) :: sfa12        ! soil fraction area with shelter angle > 12 deg.
      real :: qcap      ! transport capacity using threshold (kg/m*s)

      real :: fracd    ! dynamic threshold adjustment - added per LH's suggestions
      real, parameter :: cs = 0.3      ! saltation transport coef. (kg*s^2/m^4)

      ! calc. change to dynamic transport ! edit ljh 1-24-05
      fracd = 0.05 * (1.0 - sfa12)

      ! calc. transport cap. for emission edit 5/30/01 LH
      qcap = cs * wus * wus * (wus - (wusthr - fracd))

    end function transport_capacity

    function update_surface( time, qi, qi_tcap, qssi, qo, qsso, lx, fan, fancr, cancr, fancan, sf84mn, sf84ic, sf10ic, asvroc, &
                             smaglosmx, ct, szcr, sfcr, sflos, smlos, dmlos, sf10, sf84, sf200, svroc, szrgh, slrr ) &
                             result( mntime )

      use p1erode_def, only: SLRR_MIN

      real, intent(in) :: time         ! time step (seconds)
      real, intent(in) :: qi           ! input to C.V. of saltation, creep (kg/m*s)
      real, intent(in) :: qi_tcap      ! input limited by transport capacity (kg/m*s)
      real, intent(in) :: qssi         ! input to C.V. of suspension (kg/m*2)
      real, intent(in) :: qo           ! output from C.V. of saltation, creep (kg/m*s)
      real, intent(in) :: qsso         ! output from C.V. of suspension (kg/m*s)
      real, intent(in) :: lx           ! "effective" length of erosion cell (m)
      real, intent(in) :: fan          ! fraction saltation impacting clods & crust
      real, intent(inout) :: fancr        ! fraction saltation impacting crust
      real, intent(in) :: cancr        ! coefficient of crust abrasion (1/m)
      real, intent(in) :: fancan       ! product of fan & can ie. effective abrasion coef. (1/m)
      real, intent(in) :: sf84mn       ! soil surface fraction less than 0.84 below which no emission occurs
      real, intent(in) :: sf84ic       ! soil surface fraction less than 0.84 initially
      real, intent(in) :: sf10ic       ! soil surface fraction less than 0.10 initially
      real, intent(in) :: asvroc       ! soil surface volume rock at start of event
      real, intent(in) :: smaglosmx    ! max mobile soil reservoir of aggregated sfc.(kg/m^2)
      real, intent(in) :: ct           ! total trap coefficient
      real, intent(inout) :: szcr         ! soil crust thickness
      real, intent(inout) :: sfcr         ! soil fraction  area crusted
      real, intent(inout) :: sflos        ! soil fraction area of loose soil (only on crust)
      real, intent(inout) :: smlos        ! soil mass of loose soil (only on crust)
      real, intent(inout) :: dmlos        ! change in loose mass on aggregated sfc. (kg/m^2) (from the beginning of the erosion event)
      real, intent(inout) :: sf10         ! soil fraction less than 0.010 mm (PM10)
      real, intent(inout) :: sf84         ! soil fraction less than 0.84 mm
      real, intent(inout) :: sf200        ! soil fractions less than 2.0 mm
      real, intent(inout) :: svroc        ! soil fraction rock >2.0 mm by volume
      real, intent(inout) :: szrgh        ! soil ridge height (mm)
      real, intent(inout) :: slrr         ! soil random roughness (mm)
      real :: mntime               ! smallest time step where one of the surface updates reaches a limit
      
      real :: dmt      ! soil loss total in time-step (+ = deposition) (kg/m^2)
      real :: dmtlos   ! soil loss/dep. for each time-step (+ = deposition)
      real :: fdm      ! increase of dmtlos mass to agg. reservoir when loss exceeds smlos from crust
      real :: szc      ! tmp variable for prior crust thickness (mm)
      real :: szv      ! change in height based on volume change (mm)
      real :: smasstot ! total soil reservior mass (kg/m^2)

      ! set mntime since not yet checking
      mntime = time

      ! calc change in loose surface soil for time step (+ = surface gain)
      ! terms: - total loss,  + created by abrasion
      dmt = ((-(qo-qi) - (qsso-qssi))/lx)*time
      dmtlos = dmt + (fancan*qi_tcap)*time      !edit LH 3-1-05

      ! set initial to zero
      fdm = 0.0

      ! execute this section if crust present
      if (sfcr .gt. 0.01) then
        ! increase loss per unit area crust if non-emitting aggregate sfc.
        if (dmtlos .lt. 0 .and. sf84 .le. sf84mn) then   !edit LH 2-28-05
           dmtlos = dmtlos*(1.0/sfcr)
        endif

        ! update loose on crust           ! edit LH 2-24-05
        smlos = smlos + dmtlos
        if (smlos < 0) then           ! loose removal exceeded
          fdm = smlos* sfcr/(1.0001-sfcr) !remove extra from ag. area
          smlos = 0.0
        endif

        ! update cover of loose mass on crust
        sflos = cover_loose_mass( smlos, szrgh, slrr )

        ! in calling routine, fancan is always greater than 0.0
        if (fancan > 0.0) then    ! abrasion occurs edit LH 1-9-07
          ! update fancr
          if (dmtlos > 0.0 )then
            fancr = (sfcr - sflos*sfcr)*fan  !edit LH 2-16-05
          endif

          ! update crust thickness
          szc = max(0.001, szcr)
          szcr = szcr - (fancr*cancr*qi_tcap/(1.4*sfcr))*time !edit LH 3-1-05
          szcr = max(szcr, 0.00)

          ! update crust (consolidated) zone cover
          sfcr = sfcr*szcr/szc
          sfcr = max(0.0, sfcr)
        endif
      else
        smlos = 0.
        sflos = 0.
      endif

      ! execute this section if clods are present
      if (sfcr .lt. 0.99) then

        ! change in loose mass on aggregated and rock surface,(+)=increase
        dmlos = dmlos + dmtlos + fdm

        ! update sf84 when net loose soil gain on agg. & rock
        ! added smasstot to simplify SF84 & SF10 equations
        smasstot= smaglosmx/(sf84ic*(1.001-asvroc))   !edit LH 3-26-07
        if (dmlos .lt. 0.0 ) then
          sf84 = (smaglosmx + dmlos)/smasstot
        else
          sf84=(smaglosmx+dmlos)/(smasstot + dmlos)
          ! bad LH 3-24-07        sf84 = min(sf84,(dmlos/0.4))
        endif

        ! set limits on sf84
        sf84 = max(0.0, sf84)
        sf84 = min(0.9999, sf84)

        ! update rock cover based on dmt (soil loss(-) or gain(+))
        if (asvroc .gt. 0.0 .and. asvroc .lt. 1.0) then
          svroc = svroc - 7.5*((1-svroc)/(1-asvroc)) * (dmt/(1200*(1.001-svroc)))
          svroc = min( 1.0, max(svroc, 0.0) )
        endif

        ! update sf200
        sf200 = (2 - sf84)*sf84
        ! sf84 limited above, so these limits not needed. FAF
        sf200 = min(sf200, 1.0)
        sf200 = max(0.001,sf200)

        ! update sf10             edit LH 3-9-05
        ! if (dmtlos > 0 ) then
          ! sf10 = sf10 - sf10*dmtlos
          ! sf10 = max(.01,sf10)
        ! endif
        if (dmlos < 0.0) then
          sf10 = sf10ic*sf84/sf84ic
        else
          sf10 = sf10ic*smasstot/(smasstot+dmlos) !edit 3-26-07 LH
        endif
        ! sf10 = sf84*0.01

        ! check that cumulative distribution points are always rational
        ! Not necessary, since sf84 = 1 (max value) means sf200 = 1 and all other values less than 1, sf200 is alway greater than sf84
        if( sf84 .ge. sf200 ) then
          sf84 = min( sf84, 0.9999999*sf200)
          !write(*,*) 'sbqout: sf84 >= sf200'
        end if
        if( sf10 .ge. sf84 ) then
          sf10 = min( sf10, 0.9999999*sf84)
          !write(*,*) 'sbqout: sf10 >= s84'
        end if

        ! fanag = (1-sf84)*(1-sfcr)*fan

      endif

      ! update surface roughness

      ! rate of volume change caused by emission
      ! (used bulk densities of 1.2 for loose and 1.4 for crust to calc. mm depth per m^2 of area)
      ! szv= cen*(qen - qi_tcap)/1.2

      ! if trapping then emission lowers roughness, i.e. it comes from highest areas.
      ! If (ct .gt. 0.) then
        ! szv= -szv
      ! endif
      ! calc. change in mean surface depth
      ! terms: emission, trapped, abrasion, & abrasion not emitted
      ! szv = (szv - ct*qi_tcap/1.2 - fancan*qi_tcap/1.4 - (1.0 - sfssan)*fancan*qi_tcap*qi_tcap/(qen*1.2))*time
      ! if (szv .gt. 0.0) then  ! slow roughness increase edit LH 8-30-06
        ! szv = szv*0.5
      ! endif

      ! new trial roughness update edit LH 8-31-06
      if (ct .gt. 0.0) then                 !trapping
        if (dmtlos .gt. 0.0) then            !deposition (-roughness)
          szv = -2.0*dmtlos/1.2
        else
          szv =  dmtlos/1.2                  !emission (-roughness)
        endif
      else
        if (dmtlos .gt. 0.0) then
          szv = - dmtlos/1.2                 !deposition (-roughness)
        else
          szv = 0.5*dmtlos/1.2               ! emission (+ roughness)
        endif
      endif
      szv = szv-(2.0*(fancan*qi_tcap/1.4))*time ! abrasion (- roughness)

      ! update ridge height
      if (szrgh .gt. 10.) then
        szrgh = szrgh + szv
        ! set lower limit on ridge height
        szrgh = max(szrgh, 0.0)
      endif

      ! update random roughness
      slrr = slrr + szv/4.0
      ! set lower limit on slrr
      slrr = max(slrr, SLRR_MIN)

    end function update_surface

end module process_mod

