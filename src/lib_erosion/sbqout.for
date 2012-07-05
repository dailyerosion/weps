!$Author$
!$Date$
!$Revision$
!$HeadURL$
!**********************************************************************
!     subroutine sbqout    ver 9/2/98 file name sbqout.for
!**********************************************************************
      subroutine sbqout                                                 &
     & (flg, wus, wust, wusp, sf10, sf84,                               &
     & sf200, szcr, sfcr, sflos, smlos,                                 &
     & szrgh, sxrgs, sxprg, slrr,                                       &
     & sfcla, sfsan,                                                    &
     & sfvfs, svroc, brsai, bzht,                                       &
     & bffcv, time,                                                     &
     & canag, cancr, sf10an, sf10en, sf10bk,                            &
     & lx, qi, qssi, q10i, i, j, imax, jmax,                            &
     & smaglos, dmlos, sf84mn, sf84ic, sf10ic, asvroc,smaglosmx,        &
     & qo, qsso, q10o )
!
!     +++ PURPOSE +++
!     calculate the saltation/creep, suspension, and PM-10 discharge
!     from each control volume
!
!     +++ ARGUMENT DECLARATIONS +++
      integer   flg    !Surface update flag (1=on, 0=off)
      real wus, wust, wusp, sf10, sf84
      real sf200, szcr, sfcr, sflos, smlos
      real szrgh, sxrgs, sxprg, slrr
      real sfcla, sfsan,sfvfs
      real svroc, brsai, bzht, bffcv, time
      real canag, cancr, sf10an, sf10en, sf10bk
      real lx, qi, qssi, q10i, qo, qsso, q10o
      real smaglos, dmlos, sf84mn, sf84ic, sf10ic, asvroc, smaglosmx
      integer i, j, imax, jmax

      include 'erosion/p1erode.inc' !Parameters defining scope of erosion submodel

!     +++ ARGUMENT DEFINITIONS +++
!     wus    =       friction velocity (m/s)
!     wust   =       friction velocity threhold at emission (m/s)
!     wusp   =       friction velocity threshold at transport cap.(m/s)
!     sf10   =       soil fractions less than 0.010 mm (PM10)
!     sf84, sf200    soil fractions less than 0.84 and 2.0 mm
!     sf84ic =       soil surface fraction less than 0.84 initially
!     sf10ic =       soil surface fraction less than 0.10 initially
!     asvroc =       soil surface volume rock at start of event
!     sfcr   =       soil fraction  area crusted
!     sflos  =       soil fraction area of loose soil (only on crust)
!     sarrc  =       soil angle r. roughness weibull c parm
!     szrgh  =       soil ridge height (mm)
!     sxprg  =       soil ridge spacing parallel wind direction (mm)
!     sxrg   =       soil ridge spacing (mm)
!     seags  =       soil aggregate stability (ln(J/kg))
!     secr   =       soil crust stability (ln(j/kg))
!     sfcla  =       soil fraction clay by mass
!     sfsan  =       soil fraction sand by mass
!     sfvfs  =       soil fraction very fine sand by mass (0.05-0.1 mm)
!     svroc  =       soil fraction rock >2.0 mm by volume
!     brsai  =       biomass stem area index
!     bzht   =       biomass height (m)
!     bffcv  =       biomass fraction flat cover
!     canag  =       coeficient of aggregate abrasion (1/m)
!     cancr  =       coefficient of crust abrasion (1/m)
!     sf10an =       soil fraction of pm10 in abraded suspension size
!     sf10en =       soil fraction of pm10 in emitted suspension size
!     sf10bk =       soil fraction of pm10 in saltion/creep breakage
!     lx     =       node spacing in x-direction (m)
!     qi, qssi, q10i=input to C.V. of saltion, creep, pm-10 (kg/m*2)
!     qo, qsso, q10o=output from C.V. of saltion, creep, pm-10 (kg/m*s)
!     szc    =       tmp variable for prior crust thickness (mm)
!     sz     =       tmp variable for roughness (mm)
!     crlos  =       factor that decreases loose cover with roughness
!     dmlos  =       change in loose mass on aggregated sfc. (kg/m^2)
!     dmcld  =       change in clod mass on aggregated sfc. (kg/m^2)
!     szv    =       change in height based on volume change (mm)
!     sacd   =       specific area of clods per unit volume (mm^2/mm^3)
!
!     +++ PARAMETERS +++
      real  cs, cmp, ctf, cdp, c10dp
      parameter (cs=0.3, cmp=0.0001, ctf=1.2, cdp=0.02,                 &
     &           c10dp=0.001)

!     +++ PARAMETER DEFINITIONS +++
!     cs    = saltation transport coef. (kg*s^2/m^4)
!     cmp   = mixing parameter coef.
!     ct    = ridge trapping coef.
!     ctf   = ridge trapping fraction
!     cdp   = deposition coef. for saltation
!     c10dp = deposition coef. for pm10
!
!     +++ LOCAL VARIABLES +++
!
      real fracd
      real cen, ceno, renb, renv, qen, qcp, ci
      real sfa12, sfcv, sargc, sarrc, sfsn
      real fan, fanag, fancr, fancan, sfssen
      real sfssan, cbk,a,b,c,f,g,h,k
      real s, p, t1,t2, ct
      real cm, tmp, srrg, dmtlos, fdm
      real szc, sz, crlos
      real szv, qitmp, dmt, smasstot
!      real lnslp
!      integer m, n

!     +++ LOCAL VARIABLE DEFINITIONS +++
!     fracd  =  dynamic threshold adjustment - added per LH's suggestions
!     cen    =  coef. of emission (1/m)
!     ceno   =  Coef. of emission for bare, loose, erodible sfc.
!     renb   =  reduction in emission on bare soil by cover and rough.
!     sargc  =  soil angle of ridge shelter weibull 'c' (deg.)
!     sarrc  =  soil angle of random roughness weibull 'c'(deg.)
!     sfa12  =  soil fraction area with shelter angle > 12 deg.
!     sfcv   =  soil fraction clod & crust cover
!     srrg   =  ratio of ridge spacing to ridge spacing parallel wind direction
!     renv   =  reductin in emission on bare soil by flat biomass
!     qen    =  transport capacity using emission threshold (kg/m*s)
!     qcp    =  transport capacity using trans. cap. threshold (kg/m*s)
!     ci     =  coef. of saltation interception by biomass stems (1/m)
!                 edit LH 8-29-05
!     a2     =  intermediate calc. parameter
!     fan    =  fraction saltation impacting clods & crust
!     fanag  =  fraction saltation impacting clods
!     fancr  =  fraction saltation impacting crust
!     fancan =  product of fan & can ie. effective abrasion coef. (1/m)
!     sfssen =  soil fraction to suspension from emission
!     sfssan =  soil fraction from abrasion to emission
!    a,b,c  =  composite parameters for saltation discharge soln eq.
!     f, g   =  composite parameters for suspension discharge soln eq.
!     h, k   =  composite parameter for pm-10 discharge eq.
!     s, p   =  intermediate calc. param. for saltation discharge soln.
!     t1,t2  =  intermediate calc. param. for salt. and susp. soln eq.
!     tmp    =  intermdediate calc. param. for transport cap. soln eq.
!     dmt    =  soil loss total in time-step (+ = depostion) (kg/m^2)
!     dmtlos =  soil loss/dep. for each time-step (+ = deposition)
!     fdm    =  increase of dmtlos mass to agg. reservoir when loss
!               exceeds smlos from crust
!     lnslp  =  ln of smaglos_sf84 slope, intermediate variable
!     smasstot = total soil reservior mass (kg/m^2)
!     +++ END SPECIFICATIONS +++
!
!     calc. fraction of area with shelter > 12 degrees
      sarrc = 2.3 * sqrt (slrr)
      if ((sxprg .gt. 10.0) .and. (szrgh .gt. 1.0)) then
         sargc = 65.4*(szrgh/sxprg)**0.65
         sfa12 =(1.- exp(-(12./sargc)**0.77))*(exp(-(12./sarrc)**0.77)) &
     &             + exp(-(12./sargc)**0.77)
      else
          sfa12 = exp(-(12./sarrc)**0.77)
      endif


!
!
!     The following changes were made based upon Larry Hagen's email
!     me on Fri, Jan. 31, 2003 to represent dynamic threshold instead
!     assuming a static threshold - LEW

!     calc. change to dynamic transport ! edit ljh 1-24-05
        fracd = 0.05 * (1.0 - sfa12)
!
!     calc. transport cap. for emission edit 5/30/01 LH
        qen = cs * wus * wus * (wus - (wust-fracd))
!     calc. transport cap. for trapping
        qcp = cs * wus * wus * (wus - (wusp-fracd))
        if (qcp .lt. 0.0) then
           qcp = 0.001
        endif
!
!     store tmp qi
      qitmp = qi
!     test for trap region with both saltation & suspension deposition
      if (qen < 0.0001) then                 !edit ljh 1-24-05
        qen  = 0.00001
        qo   = 0.0
        qi   = qo          !no abrasion when no transport edit 8-30-06
        ct   = 1.0         !traps all incoming saltation discharge
        fancan = 0.0
        qsso = qssi*exp(-cdp*lx)
        q10o = q10i*exp(-c10dp*lx)
        go to 90
!     test for saltation deposition region with suspension emission
!        We have set qi to qen to calc. suspension and abrasion
!        and then used the real qi value to calc. the deposition.
!         12-5-2000 LH)
!
      elseif (qen .le. qi) then
        qi = qen
      endif
!
!     Calc. emission params for saltation/creep
!     emission coef. for bare, loose, erodible soil (1/m)
      ceno = 0.06
!     fraction emission coef. reduction by flat biomass
      renv = 0.075 + 0.934*exp(-bffcv/0.149)
!     fraction emission coef. reduction by roughness and area not emitting

      if( sf84 .le. sf84mn) then        ! edit LH 6-27-06
         sfcv = ((1.-sfcr)*(1.- 0.) + sfcr - sflos*sfcr)*(1.-svroc)     &
     &          + svroc
      else
         sfcv = ((1.-sfcr)*(1.-sf84) + sfcr - sflos*sfcr)*(1.-svroc)    &
     &          + svroc
      endif
!
!         edit 7-17-01  LH
         renb = (-0.051 + 1.051*exp(-sfcv/0.33050512))*(1 - sfa12)
         cen = ceno*renv*renb
!        if (cen .lt. 0.0001) cen = 0.0
!     soil fraction suspension in emitted soil; lower for loose on cr
        sfssen = (sf10/(sf200+0.001))*(1. - sfcr*0.8)
!     soil mixing parameter for suspension
        cm = cmp*sfssen
!
!     calc. trap params. for saltation/creep
!       calc. coef of stem interception
!       for plants above 1 mm height  edited 3-29-01 LH
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
!
!     Calc. abrasion params. for saltation/creep
!       soil fraction saltation/creep in saltation
        sfsn = 1
!
!     fraction of saltation abrading clods, crust and loose
        fan = (exp(- 4.0*bffcv))*(exp(-3*svroc))*sfsn
        fan = amax1(fan,0.)
!     fraction abrading aggregates
        fanag = (1. - sf84)*(1. - sfcr)*fan
!     fraction abrading crust
        fancr = (sfcr - sflos*sfcr)*fan
!     calc. coef. of abrasion
!      canag = exp(-2.07-0.077*seags**2.5-0.119*alog(seags))
!      cancr = exp(-2.07-0.077*secr**2.5-0.119*alog(secr))
!     sum
      fancan = fancr*cancr + fanag*canag
                                    !test3
      if (fancan .lt. 0.0001) fancan = 0.0001
!       calc fraction of abraded of suspension size
      sfssan =  (0.4 - 4.83*sfcla + 27.18*sfcla**2                      &
     &         - 53.7*sfcla**3 + 42.25*sfcla**4 - 10.7*sfcla**5)
!
!     set upper limit on sfssan for high sand content
      sfssan = min(sfssan, (1.0 - (sfsan-sfvfs)))
!
!       calc suspension breakage coef. for saltation creep
!       edit 6-9-01 LH
      cbk = 0.11*canag*(1. - (sfsan-sfvfs))*sfsn
!
!       calc total trap coef.
      if ((qcp .lt. qen)                                                &
     &  .and. ((slrr .gt. 10.) .or. (szrgh .gt. 50.))) then
            ct = (1. - sfssen)*cen*ctf*(qen - qcp)/qen
         else
         ct = 0.
      endif
!
!     assemble composite params. for saltation/creep
      a = cen *(1. - sfssen)*qen
      b = (1. - sfssan)*fancan - (1. - sfssen)*cen - cbk - ci - ct
      c = (1. - sfssan)*fancan*1./qen

!
!     solve for saltation/creep out
!        collect variables
         s = sqrt(4.*a*c + b**2.)
         p = (-2.*c*qi + b)/s
!        change p-values that are out of range
!            math range restriction: (-1 < p < 1) edit 7-18-01 LH
         if (p .le. -1.) then
            t1 = -20
         elseif (p .ge. 1.) then
            t1 = 20
         else
            t1 = (s/2.0)*(-lx) + 0.5*alog((1. + p)/(1. - p))
         endif
!        calculate atanh

      qo = (s/(2.*c))*(-tanh(t1) + b/s)
!
!     assemble composite params. for suspension
      f = sfssen*cen*qen*(1.0 - 0.1*brsai)    !edit 8-29-05 for suspension interception
!     added last term to intercept some ss LH edit  6-11-01, changed 8-29-05
      g = (sfssan*fancan - sfssen*cen + cbk + cm)*(1 - 0.1*brsai)
!
! ^^^ tmp out
!     set counter
!      m = (imax-1)/8
!      m = max(1,m)

!^^^ tmp output
!      if (wus .gt. 1.1) then
!        if (j .eq. 10) then
!          do 2 i=1,10
!           write (*,*)
!           write (*,*) 'output from sbqout: i=',i,'j=',j
!           write (*,*) 'a=', a, 'b=', b, 'c=',c
!           write (*,*) 'g=', g, 'f=', f
!           write (*,*) 'cen=', cen, 'qen=', qen
!           write (*,*) 'sfssen=', sfssen, 'sfssan=',sfssan,
!     &      'fancan=',fancan
!           write (*,*) 'cbk=',cbk, 'ci=',ci,'ct=',ct,'cm=',cm
!    2 continue
!        endif
!      endif
! ^^^ end tmp out
!
!     trap to prevent over range of exp
      if ((s*lx) .gt. 40.0) then
           s = 40.0/lx
      endif
!     solve for suspension out
!        collect variables
      if( p .gt. 1.0 ) then
         t2 = alog(2.0)
      else if( p .lt. -1 ) then
         t2 = alog(exp(s*lx)*2.0)
      else
         t2 = alog(exp(s*lx)*(1.-p)+ p + 1.)
      end if
      qsso=qssi+(1./(2.*c))*((-g*s+g*b+2.*f*c)*lx+2.*g*(-alog(2.0)+t2))
!

!     assemble composite params. for PM-10
      h = sf10en*f
      k = sf10an*sfssan*fancan - sf10en*sfssen*cen + sf10bk*cbk         &
     &   + sf10en*cm
!
!     solve for PM-10 out (similar to suspension out)
      q10o=q10i+(1./(2.*c))*((-k*s+k*b+2.*h*c)*lx+2.*k*(-alog(2.0)+t2))

!     SURFACE UPDATE EQUATIONS

      !now added as a commandline argument to tsterode - LEW
!     execute this section if flag is set
   90  if (flg .eq. 1) then
!
!     calc change in loose surface soil for time step (+ = surface gain)
!        terms: - total loss,  + created by abrasion
        dmt = ((-(qo-qitmp) - (qsso-qssi))/lx)*time
        dmtlos = dmt                                                    &
     &   + (fancan*qi)*time      !edit LH 3-1-05

!       set initial to zero
	fdm = 0.0
!       coef. for cover of loose mass (same as SOIL eq. s-24,2-25)
        sz = amax1(szrgh, 4.0*slrr)
        crlos = exp(-0.08*sz**0.5)
!
!     execute this section if crust present
      if (sfcr .gt. 0.01) then
!       increase loss per unit area crust if non-emitting aggregate sfc.
        if (dmtlos .lt. 0 .and. sf84 .le. sf84mn) then   !edit LH 2-28-05
           dmtlos = dmtlos*(1.0/sfcr)
        endif
!
!     update loose on crust           ! edit LH 2-24-05
           smlos = smlos + dmtlos
        if (smlos < 0) then           ! loose removal exceeded
            fdm = smlos* sfcr/(1.0001-sfcr) !remove extra from ag. area
            smlos = 0.0
        endif
!
!     update cover of loose mass
        if (smlos > 0) then
          tmp = 3.5*smlos**1.5
          if(tmp.gt.80.) then       !test and prevent underflow condition
            sflos = crlos
           else
            sflos = (1.0 - exp(-tmp))*crlos
           endif
        else
          sflos = 0
        endif

!
        if (fancan > 0.0) then    ! abrasion occurs edit LH 1-9-07
!       update fancr
          if (dmtlos > 0.0 )then
          fancr = (sfcr - sflos*sfcr)*fan  !edit LH 2-16-05
          endif
!
!       update crust thickness
          szc = amax1(0.001, szcr)
          szcr = szcr - (fancr*cancr*qi/(1.4*sfcr))*time !edit LH 3-1-05
          szcr = amax1(szcr, 0.00)
!
!       update crust (consolidated) zone cover
          sfcr = sfcr*szcr/szc
          sfcr = amax1(0.0, sfcr)
        endif
      else
        smlos = 0.
        sflos = 0.
      endif
!
!     execute this section if clods are present
      if (sfcr .lt. 0.99) then
!
!       change in loose mass on aggregated and rock surface,(+)=increase
        dmlos = dmlos + dmtlos + fdm

!     update sf84 when net loose soil gain on agg. & rock
!     added smasstot to simplify SF84 & SF10 equations
       smasstot= smaglosmx/(sf84ic*(1.001-asvroc))   !edit LH 3-26-07
       if (dmlos .lt. 0.0 ) then
        sf84 = (smaglosmx + dmlos)/smasstot
       else
        sf84=(smaglosmx+dmlos)/(smasstot + dmlos)
!    bad LH 3-24-07        sf84 = min(sf84,(dmlos/0.4))
       endif
!
!
!       set limits on sf84
         sf84 = max(0.0, sf84)
         sf84 = min(0.9999, sf84)

!
!       update rock cover based on dmt (soil loss(-) or gain(+))
          if (asvroc .gt. 0.0 .and. asvroc .lt. 1.0) then
            svroc = svroc - 7.5*((1-svroc)/(1-asvroc))                  &
     &     *(dmt/(1200*(1.001-svroc)))
           svroc = min( 1.0, max(svroc, 0.0) )
          endif
!
!       update sf200
        sf200 = (2 - sf84)*sf84
        sf200 = amin1(sf200, 1.0)
        sf200 = amax1(0.001,sf200)

!       update sf10             edit LH 3-9-05
!        if (dmtlos > 0 ) then
!          sf10 = sf10 - sf10*dmtlos
!          sf10 = amax1(.01,sf10)
!        endif
        if (dmlos < 0.0) then
          sf10 = sf10ic*sf84/sf84ic
        else
          sf10 = sf10ic*smasstot/(smasstot+dmlos) !edit 3-26-07 LH
        endif
!        sf10 = sf84*0.01

        ! check that cumulative distribution points are always rational
        if( sf84 .ge. sf200 ) then
          sf84 = min( sf84, 0.9999999*sf200)
          !write(*,*) 'sbqout: sf84 >= sf200'
        end if
        if( sf10 .ge. sf84 ) then
          sf10 = min( sf10, 0.9999999*sf84)
          !write(*,*) 'sbqout: sf10 >= s84'
        end if

!        fanag = (1-sf84)*(1-sfcr)*fan

      endif
!
!     update surface roughness
!
!     rate of volume change caused by emission
!        (used bulk densities of 1.2 for loose and 1.4 for crust
!         to calc. mm depth per m^2 of area)
!        szv= cen*(qen - qi)/1.2
!
!     if trapping then emission lowers roughness,
!          i.e. it comes from highest areas.
!      If (ct .gt. 0.) then
!        szv= -szv
!      endif
!     calc. change in mean surface depth
!     terms: emission, trapped, abrasion, & abrasion not emitted
!        szv = (szv - ct*qi/1.2 - fancan*qi/1.4                         &
!    &      -(1.0 - sfssan)*fancan*qi*qi/(qen*1.2))*time
!      if (szv .gt. 0.0) then  ! slow roughness increase edit LH 8-30-06
!        szv = szv*0.5
!      endif
!
!    new trial roughness update edit LH 8-31-06
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
         szv = szv-(2.0*(fancan*qi/1.4))*time ! abrasion (- roughness)
!
!     update ridge height
      if (szrgh .gt. 10.) then
          szrgh = szrgh + szv
!     set lower limit on ridge height
          szrgh = amax1(szrgh, 0.0)
      endif
!     update random roughness
         slrr = slrr + szv/4.0
!    set lower limit on slrr
         slrr = max(slrr, SLRR_MIN)
!
      endif
  100 continue
      qi = qitmp
      return
      end
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

