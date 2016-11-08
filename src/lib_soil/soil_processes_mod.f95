!$Author$
!$Date$
!$Revision$
!$HeadURL$

module soil_processes_mod

  contains

    subroutine updlay(daysim, szlyd,                                  &
     &  bhrwc0, bhrwc, bhrwcdmx,                                        &
     &  bseagmx, bseagmn, bseags,                                       &
     &  bhrwca, bhrwcw, bhrwcs,                                         &
     &  bhtsmn, bhtmx0, bhtsmx,                                         &
     &  bsk4d, bslmin, bslmax,                                          &
     &  bslagm,                                                         &
     &  bs0ags, bslagx, bsdblk,                                         &
     &  bszlyt, bsdagd, bslay,                                          &
     &  bsdsblk, bsdwblk,                                               &
     &  bhzinf, bhzwid, trigger)

!     + + + GLOBAL COMMON BLOCKS + + +
      include 'p1werm.inc'

!     + + + ARGUMENT DECLARATIONS + + +

      integer daysim
      real szlyd(mnsz)
      real  bhrwc0(mnsz), bhrwc(mnsz), bhrwcdmx(mnsz)
      real  bseagmx(mnsz), bseagmn(mnsz), bseags(0:mnsz)
      real  bhrwca(mnsz), bhrwcw(mnsz),bhrwcs(mnsz)
      real  bhtsmn(mnsz), bhtmx0(mnsz), bhtsmx(mnsz)
      real bsk4d(mnsz), bslmin(mnsz), bslmax(mnsz)
      real bslagm(0:mnsz)
      real bs0ags(0:mnsz), bslagx(0:mnsz)
      real bsdblk(0:mnsz), bhzinf
      real bszlyt(mnsz), bsdagd(0:mnsz)
      real bsdsblk(mnsz), bsdwblk(mnsz)
      real bhzwid

      integer bslay, trigger(bslay)

!     + + + LOCAL VARIABLES + + + 
      real k4f, k4fs, k4fd, k4td, k4w,k4d
      parameter(k4f=1.4, k4fs=4.25, k4fd=5.08, k4w=1)
      real se0, se1
      integer ldx

!     + + + LOCAL DEFINITIONS + + +
!   k4f  - water migration & freezing expansion coef. for agg. stab.
!   k4fs - freezing solidification coef. for agg. stability
!   k4fd - drying while frozen coef. for agg. stability
!   k4td - after thaw drying coef. a fn of depth in soil
!   k4w  - wetting process coef. modified by current dry stability
!   k4d  - drying coef. a fn of depth in soil
!   se0  - relative aggregate stability prior to SOIL update
!   se1  - relative aggregate stability after SOIL update

!  + + +  UPDATE LAYERS: + + +

      do ldx = 1, bslay
!        check for dry soil - then no changes
         if ((bhrwc(ldx) .lt. bhrwcw(ldx)) .and.                        &
     &       (bhrwcdmx(ldx) .lt. bhrwcw(ldx)) .and.                     &
     &       (bhrwc0(ldx) .lt. bhrwcw(ldx))) go to 90
!
       !estimate parameters as fn of depth
        k4td = 0.4*(1+0.00333*szlyd(ldx))
        k4td = min(k4td,0.667)
        k4d  = 0.6*(1+0.00333*szlyd(ldx))
        k4d  = min(k4d, 1.0)
!Tmp ####
!       write (*,*)'ldx=',ldx,'szlyd=',szlyd(ldx),'k4d=',k4d
!
         call aggsta(daysim, bseags(ldx), bseagmn(ldx), bseagmx(ldx),   &
     &    bhrwc0(ldx), bhrwc(ldx), bhrwcdmx(ldx),                       &
     &    bhrwcw(ldx), bhrwca(ldx),bhrwcs(ldx),                         &
     &    bhtmx0(ldx), bhtsmn(ldx), bhtsmx(ldx), bsk4d(ldx),            &
     &    se0,se1,  trigger(ldx),                                       &
     &    k4f, k4fs, k4fd, k4td, k4w, k4d)

          call asd(bslagm(ldx), bslmin(ldx),                            &
     &    bslmax(ldx), bhtsmx(ldx), bhtmx0(ldx), bs0ags(ldx),           &
     &    bslagx(ldx), se0, se1)
!
         call den(bsdblk(ldx), bsdsblk(ldx), bsdwblk(ldx),              &
     &    bszlyt(ldx), bsdagd(ldx), bhrwc0(ldx), bhrwc(ldx),            &
     &    bhrwca(ldx), bhrwcw(ldx), bhzinf, bhzwid, trigger(ldx))
   90    continue

      end do

    end subroutine updlay

    subroutine aggsta(daysim,                                         &
     &  cseags, cseagmn, cseagmx,                                       &
     &  cbhrwc0, cbhrwc, cbhrwcdmx,                                     &
     &  chrwcw, chrwca,chrwcs,                                          &
     &  chtmx0, chtsmn, chtsmx, ck4d,                                   &
     &  se0, se1, trigger,                                              &
     &  k4f, k4fs, k4fd, k4td, k4w, k4d)

!     + + + ARGUMENT DECLARATIONS + + +
      integer daysim
      real cseags, cseagmn, cseagmx
      real  cbhrwc0, cbhrwc, cbhrwcdmx
      real  chrwcw, chrwca,chrwcs
      real  chtmx0, chtsmn, chtsmx, ck4d
      real  se0, se1
      integer trigger
      real k4f, k4fs, k4fd, k4td, k4w, k4d

!     + + + LOCAL VARIABLES + + +
      real se, minse, maxse, hrwc0, hrwc1, hrwcdmx
      parameter (minse = 0.01)
      parameter (maxse = 1.0)
      real se00

!     + + + GLOBAL INCLUDES + + +
      include 'precision.inc' !declaration for portable math range checking
      include 'command.inc'   ! command line argument for new puddling option

!     + + + LOCAL DEFINITIONS + + +
!     se        - relative aggregate stability with partial update
!     minse     - minimum value allowed for se
!     maxse     - maximum value allowed for se
!     hrwc0     - relative water content on prior day of each layer
!     hrwc1     - relative water content on current day of each layer
!     hrwcdmx   - maximum relative water content on current day of each layer

!     AGGREGATE STABILITY SECTION:

!     relative agg stability for prior day
      se0 = (cseags - cseagmn)/(cseagmx - cseagmn)
      se00 = se0   !preserve relative agg stability from prior day
!tmp ####
!      write (*,*) 'k4w=',k4w, 'kfd=', k4d
!      write (*,*)'se0=',se0,'cseags=',cseags,'csagmx=',cseagmx
!      write (*,*)'se1=',se1
!     relative water content for prior day
      hrwc0 = (cbhrwc0 - chrwcw)/(chrwcs-chrwcw)
      if (hrwc0.lt.0.0) hrwc0 = 0.0
      if (hrwc0.gt.1.0) hrwc0 = 1.0
!     relative water content for current day
      hrwc1 = (cbhrwc - chrwcw) / (chrwcs-chrwcw)
      if (hrwc1 .lt. 0.0) hrwc1 = 0.0
      if (hrwc1 .gt. 1.0) hrwc1 = 1.0
!     daily maximum relative water content for current day
      hrwcdmx = (cbhrwcdmx - chrwcw) / (chrwcs - chrwcw)
      if (hrwcdmx .lt. 0.0) hrwcdmx = 0.0

!     check for two days unfrozen
      if((chtmx0.gt.0.0).and.(chtsmn.gt.0.0))then                            
         go to 70
      else
!        check for two dayscontinuous frozen         
         if ((chtmx0.lt.0.0) .and. (chtsmx .lt.0.0)) then
!          Trap for wrong initial unfrozen stability when frozen
           if (daysim .eq. 2) then
           if ( se0 .lt. (k4fd*k4f*hrwc0+0.5)) then   !freeze
!          Freeze process with prior day water content
            se = se0*(1.0001-k4w*k4f*hrwc0)/(1.0001-k4w*hrwc0)
            se = max(0.0,se)      !set lower limit
            se0 = se + k4fs*k4f*hrwc0 + 0.5 
           endif 
           endif

!          check for frozen drying or wetting
!           assumes water migration to frozen area(k4f term)
           if (hrwc1 .lt. hrwc0) then
             trigger = ibset (trigger, 5)  !frozen drying
!            frozen drying
             se1 = se0 + k4fd*k4f*(hrwc1-hrwc0)
             se1 = max(se1,0.0)
           elseif (hrwc1 .gt. hrwc0) then
             trigger = ibset(trigger, 4)   !frozen wetting
!            frozen wetting
             se1 = se0 + k4fs*k4f*(hrwc1-hrwc0) 
           else
             se1 = se0                     !no change 
           endif
!###tmp
!      write (*,*) 'frozen solid'
!       write (*,*)'hrwc0=',hrwc0,'hrwc1=',hrwc1,'se0=',se0,'se1=',se1
!      write (*,*)'k4fd=',k4fd,'k4f=',k4f,'k4fs=',k4fs
         go to 80
         endif

!     check for freeze/thaw
       if((chtmx0 .gt. 0.0).and.(chtsmn.lt.0.0)                         &
     &                         .and.(chtsmx.gt.0.0)) then 
          trigger = ibset(trigger, 1)  !freeze_thaw
!         Freeze process with prior day water content
          se = se0*(1.0001-k4w*k4f*hrwc0)/(1.0001-k4w*hrwc0)
          se = max(0.0,se)      !set lower limit
          se0 = se + k4fs*k4f*hrwc0 + 0.5
      endif 

!     Check for thaw process
       if(((chtsmn .lt. 0.0) .and. (chtsmx .gt. 0.0)) .or.              &
     &    ((chtmx0 .lt. 0.0) .and. (chtsmx .gt. 0.0 ))) then
           trigger = ibset (trigger, 2)     !thaw
!       thaw process with prior day water content
           if (hrwc0*k4f .gt. 1.0) then       !soil puddling 
             se0 = max(minse,0.999 - k4td*hrwc0)
           else
             se = se0 - k4fs*k4f*hrwc0 - 0.5  !thaw
             se = max(se, 0.0)
             se0 = se + k4td*hrwc0*(k4f-1)    !shrink
           endif
        endif
       endif
     
!    check for unfrozen drying or wetting 
   70  If (hrwc1 .lt. hrwc0) then
        trigger = ibset(trigger, 5)  !drying
!       drying process
        se1 = se0 + k4d*(hrwc0-hrwc1)
      else
         trigger = ibset(trigger, 4) !wetting
!        wetting process
         se1 = se0*(1.0001 - k4w*hrwc1)/(1.0001-k4w*hrwc0)
      endif
     

!    check for freeze process after wet/dry
       if( chtmx0 .gt. 0.0 .and. chtsmx .lt. 0) then
         trigger = ibset(trigger, 0)   !freeze
!        freeze process today
         se = se1*(1.0001-k4w*k4f*hrwc1)/(1.0001-k4w*hrwc1)
         se = max(0.0,se)        !set lower limit
         se1 = se + k4fs*k4f*hrwc1 + 0.5
!tmp ####
!         write (*,*) 'fz after w/d se=',se,'se1=',se1
!         write (*,*) 'chtmx0=',chtmx0,'chtsmx=',chtsmx
!         write (*,*) 'hrwc1=',hrwc1,'k4f=',k4f,'k4fs=',k4fs
       endif

                                   
   80 if (se1.lt.minse) then
         se1 = minse
      endif
      
!     size limits based on frozen status
      if( chtsmx .gt. 0.0 ) then
         ! if not frozen, don't allow over max
         se1 = min( se1, maxse )
      elseif (chtsmx .le. 0.0) then
         ! if frozen, allow greater stability but limit to prevent
         ! out of range asd calculation 
         se1 = min(se1, 10.0)
      endif

!     calc. new agg. stability
!     set resulting aggregate stability based on range limited se1
      cseags = se1*(cseagmx - cseagmn) + cseagmn

!    
!        (may want to use today values for asd ex.after freeze)
!     set se0 and se1 at wilting point for pass to asd subroutine
      se0 = se00 !use relative agg stability for prior day
        if (chtmx0 .gt. 0.0) then !not frozen soil prior day
          se0 = se0 + k4d*hrwc0  !dried
          se0 = min(maxse, se0)  !set upper limit
        endif
!
        if (chtsmx .gt. 0.0 ) then !not frozen today
          se1 = se1 + k4d*hrwc1  !dried
          se1 = min(maxse,se1)   !set upper limit
        endif
!
!      if (chtsmx .gt. 0.0) then !not frozen soil
!        if (ck4d .lt. k4w) then
!          slpd = (ck4d - 0.4*k4w)/0.6
!          intd = ck4d - slpd
!          se1 = (se1 + intd*hrwc1)/(1 - slpd*hrwc1) !dried
!        else
!          se1 = se1 + k4d*hrwc1 !dried
!        endif
!        se1 = min(maxse, se1) !set upper limit
!      endif

!     Can't have a negative value - yet in some cases we get them
!     So, we've put the following checks in here to trap them
!     The question is when the invalid (negative) values occur,
!     should they be set to the minimum or the maximum boundary
!     condition?  For now they are set to the minimum value.
      se0 = max(minse, se0) !set lower limit
      se1 = max(minse, se1) !set lower limit
      return      
    end subroutine aggsta

    subroutine asd( cslagm, cslmin, cslmax, chtsmx, chtmx0, cs0ags,   &
     &  cslagx, se0, se1)

!asd = aggregate size distribution
!this subroutine calculates:
!aggregate geometric mean diameter (cslagm)
!aggregate geometric standard deviation (cs0ags)
!max. aggregate diameter (cslagx)

!     + + + ARGUMENT DECLARATIONS + + +
      real cslagm, cslmin
      real cslmax, chtsmx, chtmx0, cs0ags
      real cslagx, se0, se1

!     + + + LOCAL VARIABLES + + +
      real c4p, c4f
      real gmd1, gmd0, gmd_avg0, gmd_avg1
      real slp0, slp_avg, slp
!     + + + ARGUMENT DEFINITIONS + + +
!     cs0ags    - aggregate geometric standard deviation
!     cslagm    - aggregate geometric mean diameter
!     cslmin    - min value of aggregate gmd
!     cslmax    - max value of aggregate gmd
!     chtsmx    - max temperature (C) of layer for the day
!     cslagx    - max value of aggregate size (mm)
!     se0       - relative agg stability at WP prior to SOIL update
!     se1       - relative agg stability at WP after SOIL update

!     + + + LOCAL VARIABLE DEFINITIONS + + +
!     c4p    -  scale coefficent in weibull gmd distribution
!     c4f    -  intercept coeffient in weibull gmd distribution
!     gmd0 - dimensionless geometric mean agg. diameter,prior day
!     gmd1 - dimensionless geometric mean agg. diameter, today
!     gmd_avg0 - dimesionless average gmd at se0 on prior day
!     gmd_avg1 - dimensionless avrage gmd at se1 today 

!     + + + END SPECIFICATIONS + + +
!         determine gmd_avg increase by root fibers 
!         by changing cofficients c4p, and c4f
!          fr_tot = fiber_roots + fiber_roots_dead ??????
!         if(fr_tot .lt. mass1/z1) then

!          elseif ( 
    
!         elseif (

!         else

!          end
!### tmp
!	write (*,*) 'start asd'
 !       write (*,*) 'se0=',se0, 'se1=',se1
       if (se0 .gt. 1.0 .and. se1 .gt. 1.0 ) then         
!	    gmd1 = gmd0         !no change or  all frozen
            go to 100
       endif
!         temp coef. values
          c4p = 0.6
          c4f = 0.0

!     calculate geometric mean diameter using prior geometric mean diameter
      if (chtmx0 .gt. 0.0) then
      cslagm = max (cslmin, cslagm)   !error trap
      endif
      gmd0 = (cslagm - cslmin)/(cslmax - cslmin)    !dimensionless
!
     
      if ((se0 .lt. 1.0) .and. (se1 .gt. 1.0)) then   !freeze
          gmd1 = gmd0 + se1
      elseif ((se0 .gt. 1.0) .and. (se1 .le. 1.0)) then   !thaw
!         se1 may be puddled, all freeze dried or between these states
          gmd1 = 1 - exp(-(se1/c4p)**2)
!
!        no freeze; calculate gmd1
      elseif ((se0 .eq. 1.0).and.(se1 .eq. 1.0) ) then 
          gmd_avg1 = (1 - exp(-(se1/c4p)**2))*(1-c4f) + c4f
          gmd1 = (gmd_avg1 + gmd0)/2.0

      elseif (se0 .eq. se1) then
           gmd_avg1 = (1 - exp(-(se1/c4p)**2))*(1-c4f) + c4f
           gmd1 = gmd_avg1*0.2 + gmd0*0.8
      else                              
          gmd_avg0 = (1 - exp(-(se0/c4p)**2))*(1-c4f) + c4f
          gmd_avg1 = (1 - exp(-(se1/c4p)**2))*(1-c4f) + c4f

          slp0 = (gmd_avg1 - gmd0)/(se1 - se0)
          slp_avg = (gmd_avg1 - gmd_avg0)/(se1 - se0)
          slp = (slp0 + slp_avg)/2.0
          gmd1 = gmd0 + slp*(se1 - se0)
      endif
      
        cslagm = (cslmax - cslmin) * gmd1 + cslmin  !dimensioned gmd      
 
!     restrict upper size if not frozen
      if ((chtsmx .ge. 0.0) .and. (cslagm .gt. cslmax)) then
          cslagm = cslmax
      endif

      ! restrict lower size unconditionally
      cslagm = max(cslmin, cslagm)

!     calculate geometric standard deviation (eq. S-??)
!     this equation is asmytotic to zero at zero and +infinity
!     Based on the definition of Geometric Standard Deviation this
!     should be asmototic to 1
!      cs0ags = 1.0 / (0.0203 + 0.00193  *cslagm +
!     &             0.074 / sqrt(cslagm))
!     this replacement equation is asmytotic to 1 and is very close
!     to the original where the gsd was greater than 1
      cs0ags = 1.0 + 1.0                                                &
     &       / (0.012448 + 0.002463*cslagm + 0.093467/sqrt(cslagm))

!     calculate max. aggregate diameter (cslagx)
      c4p = 1.52 * cslagm**(-0.449)
      cslagx = (cs0ags**c4p) * cslagm

  100 return
    end subroutine asd

    subroutine den(                                                   &
     &  csdblk, csdsblk, csdwblk, cszlyt, csdagd,                       &
     &  chrwc0, chrwc, chrwca, chrwcw,                                  &
     &  bhzinf, chzwid, trigger)

!     + + + ARGUMENT DECLARATIONS + + +

      real csdblk, csdsblk, csdwblk, cszlyt, csdagd
      real chrwc0, chrwc, chrwca, chrwcw
      real bhzinf, chzwid
      integer trigger

!     + + + ARGUMENT DEFINITIONS + + +

!     csdblk - present soil bulk density (Mg/m^3)
!     csdsblk - Settled soil bulk density (Mg/m^3)
!     csdwblk - 1/3 bar soil bulk density (Mg/m^3)
!     cszlyt - Soil layer thickness (mm)
!     csdagd - Soil aggregate density (Mg/m^3)
!     chrwc0 - Soil water content from previous day (g/g)
!     chrwc  - Soil water content on present day (g/g)
!     chrwca - Available soil water content (g/g)
!     chrwcw - Wilting point soil water content (g/g)
!     bhzinf - daily water infiltration depth (mm)
!     chzwid - water infiltration depth (mm)

!     + + + LOCAL VARIABLES + + + 

      real bsdbk0
      integer j, nj
      real wsdblk, dsdblk
      real wszlyt

!     + + + LOCAL DEFINITIONS + + +

!   bsdbk0    - bulk density prior to update by SOIL, Mg/m^3
!   wsdblk    - bulk density of wet soil
!   dsdblk    - bulk density of dry soil
!   wszlyt    - depth of wetness in this layer

!  DENSITY SECTION:
!     update bulk density

!     store initial value of layer density
      bsdbk0 = csdblk

!     daily update density for other forces -- long term
!     only if current bulk density is less than settled bd
      ! removed restriction to allow compaction condition with slow amelioration
      !if (csdblk.lt.csdsblk) then
        dsdblk = csdblk + 0.01*(csdsblk - csdblk)
      !else
      ! dsdblk = csdblk
      !endif

! if water has infiltrated into the layer
      if (chzwid .gt. 0) then
        trigger = ibset(trigger, 7) !wet_bulk_den

!     update for infiltrated water additions in 5 mm increments
        nj =nint(bhzinf/5.)
        wsdblk = csdblk
        do j = 1,nj
          if (wsdblk .lt. 0.97 * csdsblk) then
            wsdblk = min( csdsblk,                                      &
     &               wsdblk+0.75 * (1-(wsdblk/(0.97 * csdsblk)))**1.5 )
          else
            exit
          endif
        enddo
      else 
! set wsdblk to something so that it won't bomb out as uninit'd later on
        wsdblk = dsdblk
      endif

! get weighted average of wet and dry densities
      wszlyt = min(cszlyt, chzwid)
      csdblk = (wsdblk*wszlyt + dsdblk*(cszlyt-wszlyt)) / cszlyt

! reduce wet depth by this layer's thickness or wet depth
      chzwid = chzwid - wszlyt
   
!     update layer thickness
      cszlyt = cszlyt * bsdbk0 / csdblk
      
!     update aggregate density
      ! After consultation with LH, it was decided that Agg. Density
      ! should be set to a constant in WEPS for now.  The erosion submodel
      ! code has been changed to allow friction velocity values to be adjusted
      ! based upon surface agg. density value.  Since the settled bulk
      ! density value is not a good alternative value to set the agg. density
      ! value too, it has been changed to 1.8.  This needs to be moved
      ! (probably removed entirely) once the WEPS interface code has been
      ! changed to default the incoming agg. density values to 1.8
      ! Mar. 15, 2006 - LEW

      ! csdagd = csdsblk
      csdagd = 1.8  ! Set to constant value in WEPS for now - LEW
!
    end subroutine den

    subroutine cru( bszcr,cumpa,csfcla,dcump,bsfcr,bhzsmt,            &
     &  bsmlos,csfom,csfcce,csfsan,bsmls0,bszrgh,bszrr,bsflos)

!calculates 4 crust variables:
!crust thickness, mm
!fraction of soil crust cover, m2/m2
!mass of loose erodible material on crust, kg/m2
!fraction cover of loose erodible material, m2/m2 

!     + + + ARGUMENT DECLARATIONS + + +
      real bszcr,cumpa,csfcla,dcump,bsfcr,bhzsmt,bsmlos,csfom
      real csfcce,csfsan,bsmls0,bszrgh,bszrr,bsflos 

!     + + + LOCAL VARIABLES + + +
      real sz, cflos
      real temp

!     + + + LOCAL DEFINITIONS + + +
!   sz        - maximum of ridge height and 4 times random roughness
!   cflos     - correction factor for decease of fraction loose cover
!               area on crust caused by roughness

!     calc. apparent precip. (eq. S-14)
      if (bszcr .ge. 7.6) bszcr = 7.599
      cumpa = -(alog(1.0-bszcr/7.6)) / (0.0705-0.0687*csfcla**0.146)
!     check for threshold precip.
!     ie. check to see if a H2O addition exceeding 10mm has been made
! *** threshold is not noted for S-15, this test should go later
!      write(*,*) '*******cumpa + dcump<10.? ',cumpa,dcump
      if((cumpa + dcump) .lt. 10. ) go to 12
! ***
!     calc. crust thickness (eq. S-16, *** sb S-15)
      temp = (0.0705 - 0.0687*csfcla**0.146)*(cumpa + dcump)
      if (temp.gt.20.0) then                !check to avoid underflow
          bszcr = 7.6
      else
          bszcr = 7.6*(1.0 - exp(-temp))
      endif

!     calc. apparent precip (eq. S-17 *** sb S-16)
      if( bsfcr .lt. 1.0 ) then
          cumpa = -(alog(1.0 - bsfcr))/0.045
          ! calc. crust cover fraction (eq. S-18, *** sb S-17)
          bsfcr = 1.0 - exp(- 0.045*(cumpa + dcump))
      end if

!  loose erodible material on crust
!     set max loose mass (eq S-20, *** sb S-19)
      if (bhzsmt .eq. 0.0) then !if no snow melt
         if (csfcla .eq. 0.0) then
            bsmlos = 0.1*exp(-0.57 + 0.22 * 999. + 7.0 * csfcce - csfom)
         else
            bsmlos = 0.1*exp(-0.57 + 0.22 * csfsan / csfcla             &
     &               + 7.0 * csfcce - csfom)
         end if
!        set upper limit on loose mass (eq. S-21, *** sb S-20)
         if (bsmlos .gt. 3.0) bsmlos = 3.0
      else
!        check if water is from snowmelt (eq. S-22, *** sb S-21,22)
         if (bhzsmt .gt. 0.0) then !if snow melt
             bsmls0 = bsmlos
             bsmlos = bsmlos * (1.0 - 0.1 * bhzsmt)
             if (bsmlos .lt. bsmls0*0.1) bsmlos = bsmls0 * 0.1
         else
!             bsmlos = bsmlos * (1.0 - 0.0053 * dcump)
             bsmlos = bsmlos
         endif
      endif

!     fraction cover of loose erodible material (eq. S-24, S-25, sb S-23,24)
      sz = amax1(4.0*bszrr, bszrgh)
! ***      cflos = sqrt(bsmlos)/(0.24*sz)
! *** debugging fix
      cflos = exp(-0.08*sz**0.5)
! *** eodf
      if (cflos .gt. 1.0) cflos = 1.0
      bsflos = (1.0 - exp(-3.5*bsmlos**1.5))*cflos

   12 continue

    end subroutine cru

    subroutine ranrou(                                                &
     &  csfsil, csfsan, bszrr, bszrro, cumpa, dcump, cf2cov, csvroc)

!     + + + ARGUMENT DECLARATIONS + + +
      real csfsil, csfsan
      real bszrr, bszrro
      real cumpa, dcump, cf2cov, csvroc

!     + + + LOCAL VARIABLES + + +

      real arr, crr

!     + + + LOCAL DEFINITIONS + + +
!
!   arr       - regression coef. to calc. random roughness
!   crr       - regression coefficient for random roughness decrease
!   csfsan    - top layer fraction of sand.
!   csfsil    - top layer fraction of silt.
!   csvroc    - soil volume fraction of rock in top layer

!  RANDOM ROUGHNESS SECTION:
!     calc. reg. coefficients (eq. S-12, S-13)
      arr = 91.08 + 765.8 * csfsil
      crr = 0.53 + 4.66 * csfsan - 3.8 * csfsan**1.5-1.22*(csfsan)**0.5
!     calc. apparent precip. (eq. S-11 is S-14 solved for a bare surface)
!     changed * to ** to conform to equ S-10
!     erosion could make bszrr > bszrro so insert fix - LH
      if(bszrr .ge. bszrro) then
         cumpa = 0.0
         bszrro = bszrr
      else
         cumpa = arr * (-log(bszrr / bszrro)) ** (1.0 / crr)
      end if

!     update random roughness (eq. S-14)
! *** debugging fix

      if ((cumpa + (1.0 - csvroc) * cf2cov * dcump)/arr                 &
     &  .lt. 0.) then
         bszrr = bszrro
!         write(*,*) 'soil: debugging fix executed 1'
!         write(*,*) '  cumpa, dcump, cf2cov, arr, csvroc ',
!     *            cumpa, dcump, cf2cov, arr, csvroc
      else
! *** end of debugging fix
! ***      write(*,*) ' crr ', crr
         bszrr = bszrro * exp(-((cumpa + (1.0 - csvroc)*                &
     &     cf2cov*dcump) /arr)**crr)
      endif
      if ( bszrr .lt. 2.0) bszrr = 2.0
    end subroutine ranrou

    subroutine rid(cf2cov, bbfscv, bbffcv, bszrgh,                    &
     &  bsxrgs, bszrho, cumpa, dcump, bsvroc)

!     + + + ARGUMENT DECLARATIONS + + +
      real cf2cov, bbfscv, bbffcv, bszrgh, bsxrgs, bszrho
      real cumpa, dcump, bsvroc(*)

!     + + + LOCAL VARIABLES + + +
      real cf1rg

!     + + + LOCAL DEFINITIONS + + +
!   cf1rg     - correction factor for ridge scale

!  RIDGE SECTION:
!     calculate biomass cover sheltering factor (eq. S-9 & S-10 combined)
      cf2cov = 1.0 - 0.6 * (bbfscv + (1.0 - bbfscv)*bbffcv)

!     if ridge height is zero, skip ridge update
      if (bszrgh .ne. 0.0) then
         ! calc. ridge scale factor (eq. S-8)
         cf1rg = (348.0 / bsxrgs)**0.3
         ! calculate apparent cum. precip. (eq. S-5)
         cumpa = ((1. - bszrgh/bszrho)/(0.034*cf1rg))**2.
         ! update ridge height (eq. S-6)
         bszrgh = bszrho * (1.0 - 0.034 * sqrt(cumpa + dcump * cf2cov   &
     &          * (1.0 - bsvroc(1)))*cf1rg)

         ! check to see that minimum bszrgh/bszrho > 0.05 if not then set
         ! the ratio to 0.05.
         if ((bszrgh/bszrho) .lt. 0.05) bszrgh = 0.05 * bszrho
      endif
    end subroutine rid

end module soil_processes_mod

