!$Author$
!$Date$
!$Revision$
!$HeadURL$

module soil_processes_mod

  contains

    subroutine updlay( daysim, szlyd, &
                       bhrwc0, bhrwc, bhrwcdmx, &
                       bseagmx, bseagmn, bseags, &
                       bhrwcw, bhrwcs, &
                       bhtsmn, bhtmx0, bhtsmx, &
                       bslmin, bslmax, &
                       bslagm, &
                       bs0ags, bslagn, bslagx, bsdblk, &
                       bszlyt, bsdagd, bslay, &
                       bsdsblk, bsvroc, &
                       bhzinf, bhzwid, trigger, sr)

      use file_io_mod, only: luoasd               ! Only for printing out ASD results
      use manage_data_struct_defs, only: manFile  ! Only for printing out ASD results
      use datetime_mod, only: get_psimdate

!     + + + ARGUMENT DECLARATIONS + + +
      integer, intent(in) :: daysim
      real, intent(inout) :: szlyd(*)
      real, intent(in) :: bhrwc0(*)
      real, intent(in) :: bhrwc(*)
      real, intent(inout) :: bhrwcdmx(*)
      real, intent(inout) :: bseagmx(*)
      real, intent(inout) :: bseagmn(*)
      real, intent(inout) :: bseags(*)
      real, intent(inout) :: bhrwcw(*)
      real, intent(inout) :: bhrwcs(*)
      real, intent(in) :: bhtsmn(*)
      real, intent(in) :: bhtmx0(*)
      real, intent(in) :: bhtsmx(*)
      real, intent(inout) :: bslmin(*)
      real, intent(inout) :: bslmax(*)
      real, intent(inout) :: bslagm(*)
      real, intent(inout) :: bs0ags(*)
      real, intent(inout) :: bslagn(*)
      real, intent(inout) :: bslagx(*)
      real, intent(inout) :: bsdblk(*)
      real, intent(in) :: bhzinf
      real, intent(inout) :: bszlyt(*)
      real, intent(inout) :: bsdagd(*)
      real, intent(inout) :: bsdsblk(*)
      real, intent(inout) :: bsvroc(*)
      real, intent(inout) :: bhzwid
      integer, intent(in) :: bslay
      integer, intent(inout) :: trigger(bslay)
      integer, intent(in) :: sr

      ! + + + LOCAL VARIABLES + + + 
      real :: se0   ! relative aggregate stability prior to SOIL update
      real :: se1   ! relative aggregate stability after SOIL update
      integer :: ldx ! index for layers
      integer :: cd  ! day of month
      integer :: cm  ! month of year
      integer :: cy  ! year

      real :: k4td  ! after thaw drying coef. a fn of depth in soil
      real :: k4d   ! drying coef. a fn of depth in soil

      real, parameter :: k4f = 1.4   ! water migration & freezing expansion coef. for agg. stab.
      real, parameter :: k4fs = 4.25 ! freezing solidification coef. for agg. stability
      real, parameter :: k4fd = 5.08 ! drying while frozen coef. for agg. stability
      real, parameter :: k4w = 1.0   ! wetting process coef. modified by current dry stability

      ! + + +  UPDATE LAYERS: + + +

      call get_psimdate(sr, cd, cm, cy)
      
      do ldx = 1, bslay
         ! check for dry soil - then no changes
         if(     (bhrwc(ldx) .lt. bhrwcw(ldx)) &
           .and. (bhrwcdmx(ldx) .lt. bhrwcw(ldx)) &
           .and. (bhrwc0(ldx) .lt. bhrwcw(ldx)) ) go to 90

         ! estimate parameters as fn of depth
         k4td = 0.4*(1+0.00333*szlyd(ldx))
         k4td = min(k4td,0.667)
         k4d  = 0.6*(1+0.00333*szlyd(ldx))
         k4d  = min(k4d, 1.0)

         call aggsta( daysim, bseags(ldx), bseagmn(ldx), bseagmx(ldx), &
                      bhrwc0(ldx), bhrwc(ldx), bhrwcdmx(ldx), &
                      bhrwcw(ldx), bhrwcs(ldx), &
                      bhtmx0(ldx), bhtsmn(ldx), bhtsmx(ldx), &
                      se0,se1,  trigger(ldx),  &
                      k4f, k4fs, k4fd, k4td, k4w, k4d)

         if (BTEST(manFile(sr)%am0tfl,0) .and. (manFile(sr)%asdhflag .eq. 1) .and. (ldx .eq. 1)) then
           write(luoasd(sr),"(3(i5))",ADVANCE='NO') cd, cm, cy
           write (UNIT=luoasd(sr),FMT="(i10,5(f10.4),A)",ADVANCE="YES")   &
             1, bszlyt(1),                                               &
             bslagm(1), bs0ags(1), bslagn(1), bslagx(1),  &
             ' Before soil values'
         end if

         call asd( bslagm(ldx), bslmin(ldx), &
                   bslmax(ldx), bhtsmx(ldx), bhtmx0(ldx), bs0ags(ldx), &
                   bslagx(ldx), se0, se1)

         if (BTEST(manFile(sr)%am0tfl,0) .and. (manFile(sr)%asdhflag .eq. 1) .and. (ldx .eq. 1)) then
           write(luoasd(sr),"(3(i5))",ADVANCE='NO') cd, cm, cy
           write (UNIT=luoasd(sr),FMT="(i10,5(f10.4),A)",ADVANCE="YES")   &
             1, bszlyt(1),                                               &
             bslagm(1), bs0ags(1), bslagn(1), bslagx(1),  &
             ' After soil values'
         end if

         call den( bsdblk(ldx), bsdsblk(ldx), &
                   bszlyt(ldx), bsdagd(ldx), bsvroc(ldx), &
                   bhzinf, bhzwid, trigger(ldx) )
   90    continue

      end do

    end subroutine updlay

    pure subroutine aggsta( daysim, &
                       cseags, cseagmn, cseagmx, &
                       cbhrwc0, cbhrwc, cbhrwcdmx, &
                       chrwcw, chrwcs, &
                       chtmx0, chtsmn, chtsmx, &
                       se0, se1, trigger, &
                       k4f, k4fs, k4fd, k4td, k4w, k4d)

!     + + + ARGUMENT DECLARATIONS + + +
      integer, intent(in) :: daysim
      real, intent(inout) :: cseags
      real, intent(in) :: cseagmn
      real, intent(in) :: cseagmx
      real, intent(in) :: cbhrwc0
      real, intent(in) :: cbhrwc
      real, intent(in) :: cbhrwcdmx
      real, intent(in) :: chrwcw
      real, intent(in) :: chrwcs
      real, intent(in) :: chtmx0
      real, intent(in) :: chtsmn
      real, intent(in) :: chtsmx
      real, intent(inout) :: se0
      real, intent(inout) :: se1
      integer, intent(inout) :: trigger
      real, intent(in) :: k4f
      real, intent(in) :: k4fs
      real, intent(in) :: k4fd
      real, intent(in) :: k4td
      real, intent(in) :: k4w
      real, intent(in) :: k4d

!     + + + LOCAL VARIABLES + + +
      real :: se      ! relative aggregate stability with partial update
      real :: hrwc0   ! relative water content on prior day of each layer
      real :: hrwc1   ! relative water content on current day of each layer
      real :: hrwcdmx ! maximum relative water content on current day of each layer
      real :: se00    ! 

      real, parameter :: minse = 0.01  ! minimum value allowed for se
      real, parameter :: maxse = 1.0   ! maximum value allowed for se

      ! relative agg stability for prior day
      se0 = (cseags - cseagmn)/(cseagmx - cseagmn)
      se00 = se0   !preserve relative agg stability from prior day

      ! relative water content for prior day
      hrwc0 = (cbhrwc0 - chrwcw)/(chrwcs-chrwcw)
      if (hrwc0.lt.0.0) hrwc0 = 0.0
      if (hrwc0.gt.1.0) hrwc0 = 1.0
      ! relative water content for current day
      hrwc1 = (cbhrwc - chrwcw) / (chrwcs-chrwcw)
      if (hrwc1 .lt. 0.0) hrwc1 = 0.0
      if (hrwc1 .gt. 1.0) hrwc1 = 1.0
      ! daily maximum relative water content for current day
      hrwcdmx = (cbhrwcdmx - chrwcw) / (chrwcs - chrwcw)
      if (hrwcdmx .lt. 0.0) hrwcdmx = 0.0

      ! check for two days unfrozen
      if( (chtmx0.gt.0.0).and.(chtsmn.gt.0.0) ) then
         go to 70
      else
         ! check for two dayscontinuous frozen         
         if( (chtmx0.lt.0.0) .and. (chtsmx .lt.0.0) ) then
           ! Trap for wrong initial unfrozen stability when frozen
           if (daysim .eq. 2) then
           if ( se0 .lt. (k4fd*k4f*hrwc0+0.5)) then   !freeze
            ! Freeze process with prior day water content
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

           go to 80
         endif

       ! check for freeze/thaw
       if( (chtmx0 .gt. 0.0).and.(chtsmn.lt.0.0) .and. (chtsmx.gt.0.0) ) then 
          trigger = ibset(trigger, 1)  !freeze_thaw
          ! Freeze process with prior day water content
          se = se0*(1.0001-k4w*k4f*hrwc0)/(1.0001-k4w*hrwc0)
          se = max(0.0,se)      !set lower limit
          se0 = se + k4fs*k4f*hrwc0 + 0.5
      endif 

       ! Check for thaw process
       if(   ((chtsmn .lt. 0.0) .and. (chtsmx .gt. 0.0)) &
        .or. ((chtmx0 .lt. 0.0) .and. (chtsmx .gt. 0.0 )) ) then
           trigger = ibset (trigger, 2)     !thaw
           ! thaw process with prior day water content
           if (hrwc0*k4f .gt. 1.0) then       !soil puddling 
             se0 = max(minse,0.999 - k4td*hrwc0)
           else
             se = se0 - k4fs*k4f*hrwc0 - 0.5  !thaw
             se = max(se, 0.0)
             se0 = se + k4td*hrwc0*(k4f-1)    !shrink
           endif
        endif
       endif
     
       ! check for unfrozen drying or wetting 
   70  If (hrwc1 .lt. hrwc0) then
        trigger = ibset(trigger, 5)  !drying
        ! drying process
        se1 = se0 + k4d*(hrwc0-hrwc1)
      else
         trigger = ibset(trigger, 4) !wetting
         ! wetting process
         se1 = se0*(1.0001 - k4w*hrwc1)/(1.0001-k4w*hrwc0)
      endif
     

       ! check for freeze process after wet/dry
       if( chtmx0 .gt. 0.0 .and. chtsmx .lt. 0) then
         trigger = ibset(trigger, 0)   !freeze
         ! freeze process today
         se = se1*(1.0001-k4w*k4f*hrwc1)/(1.0001-k4w*hrwc1)
         se = max(0.0,se)        !set lower limit
         se1 = se + k4fs*k4f*hrwc1 + 0.5
       endif

                                   
   80 if (se1.lt.minse) then
         se1 = minse
      endif
      
      ! size limits based on frozen status
      if( chtsmx .gt. 0.0 ) then
         ! if not frozen, don't allow over max
         se1 = min( se1, maxse )
      elseif (chtsmx .le. 0.0) then
         ! if frozen, allow greater stability but limit to prevent
         ! out of range asd calculation 
         se1 = min(se1, 10.0)
      endif

      ! calc. new agg. stability
      ! set resulting aggregate stability based on range limited se1
      cseags = se1*(cseagmx - cseagmn) + cseagmn

      ! (may want to use today values for asd ex.after freeze)
      ! set se0 and se1 at wilting point for pass to asd subroutine
      se0 = se00 !use relative agg stability for prior day
      if (chtmx0 .gt. 0.0) then !not frozen soil prior day
        se0 = se0 + k4d*hrwc0  !dried
        se0 = min(maxse, se0)  !set upper limit
      endif

      if (chtsmx .gt. 0.0 ) then !not frozen today
        se1 = se1 + k4d*hrwc1  !dried
        se1 = min(maxse,se1)   !set upper limit
      endif

      ! if (chtsmx .gt. 0.0) then !not frozen soil
      !   if (ck4d .lt. k4w) then
      !     slpd = (ck4d - 0.4*k4w)/0.6
      !     intd = ck4d - slpd
      !     se1 = (se1 + intd*hrwc1)/(1 - slpd*hrwc1) !dried
      !   else
      !     se1 = se1 + k4d*hrwc1 !dried
      !   endif
      !   se1 = min(maxse, se1) !set upper limit
      ! endif

      ! Can't have a negative value - yet in some cases we get them
      ! So, we've put the following checks in here to trap them
      ! The question is when the invalid (negative) values occur,
      ! should they be set to the minimum or the maximum boundary
      ! condition?  For now they are set to the minimum value.

      se0 = max(minse, se0) !set lower limit
      se1 = max(minse, se1) !set lower limit
      
      return      
    end subroutine aggsta

    pure subroutine asd( cslagm, cslmin, cslmax, chtsmx, chtmx0, cs0ags, &
                    cslagx, se0, se1)

      ! asd = aggregate size distribution
      ! this subroutine calculates:
      ! aggregate geometric mean diameter (cslagm)
      ! aggregate geometric standard deviation (cs0ags)
      ! max. aggregate diameter (cslagx)

      ! + + + ARGUMENT DECLARATIONS + + +
      real, intent(inout) :: cslagm  ! aggregate geometric mean diameter
      real, intent(in) :: cslmin  ! min value of aggregate gmd
      real, intent(in) :: cslmax  ! max value of aggregate gmd
      real, intent(in) :: chtsmx  ! max temperature (C) of layer for the day
      real, intent(in) :: chtmx0  ! max temperature (C) of layer for the previous day
      real, intent(out) :: cs0ags  ! aggregate geometric standard deviation
      real, intent(out) :: cslagx  ! max value of aggregate size (mm)
      real, intent(in) :: se0     ! relative agg stability at WP prior to SOIL update
      real, intent(in) :: se1     ! relative agg stability at WP after SOIL update

      ! + + + LOCAL VARIABLES + + +
      real :: c4p   ! scale coefficent in weibull gmd distribution
      real :: c4f   ! intercept coeffient in weibull gmd distribution
      real :: gmd1  ! dimensionless geometric mean agg. diameter, today
      real :: gmd0  ! dimensionless geometric mean agg. diameter,prior day
      real :: gmd_avg0  ! dimesionless average gmd at se0 on prior day
      real :: gmd_avg1  ! dimensionless avrage gmd at se1 today
      real :: slp0      ! 
      real :: slp_avg   ! 
      real :: slp       ! 

      ! + + + END SPECIFICATIONS + + +

      ! determine gmd_avg increase by root fibers 
      ! by changing cofficients c4p, and c4f
      ! fr_tot = fiber_roots + fiber_roots_dead ??????
      ! if( fr_tot .lt. mass1/z1 ) then
      ! elseif ( 
      ! elseif (
      ! else
      ! end

      if (se0 .gt. 1.0 .and. se1 .gt. 1.0 ) then         
         ! gmd1 = gmd0         !no change or  all frozen
         return
      endif

      ! temp coef. values
      c4p = 0.6
      c4f = 0.0

      ! calculate geometric mean diameter using prior geometric mean diameter
      if (chtmx0 .gt. 0.0) then
         cslagm = max (cslmin, cslagm)   !error trap
      endif
      gmd0 = (cslagm - cslmin)/(cslmax - cslmin)    !dimensionless

      if ((se0 .lt. 1.0) .and. (se1 .gt. 1.0)) then   !freeze
         gmd1 = gmd0 + se1
      elseif ((se0 .gt. 1.0) .and. (se1 .le. 1.0)) then   !thaw
         ! se1 may be puddled, all freeze dried or between these states
         gmd1 = 1 - exp(-(se1/c4p)**2)

      elseif ((se0 .eq. 1.0).and.(se1 .eq. 1.0) ) then 
         ! no freeze; calculate gmd1
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
 
      ! restrict upper size if not frozen
      if ((chtsmx .ge. 0.0) .and. (cslagm .gt. cslmax)) then
          cslagm = cslmax
      endif

      ! restrict lower size unconditionally
      cslagm = max(cslmin, cslagm)

      ! calculate geometric standard deviation (eq. S-??)
      ! this equation is asmytotic to zero at zero and +infinity
      ! Based on the definition of Geometric Standard Deviation this
      ! should be asmototic to 1
      ! cs0ags = 1.0 / (0.0203 + 0.00193  *cslagm + 0.074 / sqrt(cslagm))
      ! this replacement equation is asmytotic to 1 and is very close
      ! to the original where the gsd was greater than 1
      cs0ags = 1.0 + 1.0 / (0.012448 + 0.002463*cslagm + 0.093467/sqrt(cslagm))

      ! calculate max. aggregate diameter (cslagx)
      c4p = 1.52 * cslagm**(-0.449)
      cslagx = (cs0ags**c4p) * cslagm

    end subroutine asd

    pure subroutine den( csdblk, csdsblk, cszlyt, csdagd, vfrock, bhzinf, chzwid, trigger )

      use soilden_mod, only: setLayThick

      real, intent(inout) :: csdblk  ! present soil bulk density (Mg/m^3)
      real, intent(in) :: csdsblk ! Settled soil bulk density (Mg/m^3)
      real, intent(inout) :: cszlyt  ! Soil layer thickness (mm)
      real, intent(out) :: csdagd  ! Soil aggregate density (Mg/m^3)
      real, intent(in) :: bhzinf  ! daily water infiltration depth (mm)
      real, intent(inout) :: chzwid  ! water infiltration depth (mm)
      real, intent(inout) :: vfrock  ! 
      integer, intent(inout) :: trigger  ! 

      ! + + + LOCAL VARIABLES + + + 
      real :: bsdbk0  ! bulk density prior to update by SOIL, Mg/m^3
      integer :: j    ! 
      integer :: nj   ! 
      real :: wsdblk  ! bulk density of wet soil
      real :: dsdblk  ! bulk density of dry soil
      real :: wszlyt  ! depth of wetness in this layer

      ! store initial value of layer density
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

        ! update for infiltrated water additions in 5 mm increments
        nj = nint( bhzinf/5.0 )
        wsdblk = csdblk
        do j = 1,nj
          if (wsdblk .lt. 0.97 * csdsblk) then
            wsdblk = min( csdsblk, wsdblk+0.75 * (1-(wsdblk/(0.97 * csdsblk)))**1.5 )
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
   
      ! update layer thickness
      call setLayThick( cszlyt, vfrock, bsdbk0, csdblk)
      
      ! update aggregate density
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

    end subroutine den

    pure subroutine cru( bszcr, cumpa, csfcla, dcump, bsfcr, bhzsmt, &
                    bsmlos, csfom, csfcce, csfsan, bsmls0, bszrgh, bszrr, bsflos)

      ! calculates 4 crust variables:
      ! crust thickness, mm
      ! fraction of soil crust cover, m2/m2
      ! mass of loose erodible material on crust, kg/m2
      ! fraction cover of loose erodible material, m2/m2 

      ! + + + ARGUMENT DECLARATIONS + + +
      real, intent(inout) :: bszcr
      real, intent(out) :: cumpa
      real, intent(in) :: csfcla
      real, intent(in) :: dcump
      real, intent(inout) :: bsfcr
      real, intent(in) :: bhzsmt
      real, intent(inout) :: bsmlos
      real, intent(in) :: csfom
      real, intent(in) :: csfcce
      real, intent(in) :: csfsan
      real, intent(out) :: bsmls0
      real, intent(in) :: bszrgh
      real, intent(in) :: bszrr
      real, intent(out) :: bsflos 

!     + + + LOCAL VARIABLES + + +
      real :: sz    ! maximum of ridge height and 4 times random roughness
      real :: cflos ! correction factor for decease of fraction loose cover
                    ! area on crust caused by roughness
      real :: temp  ! 

      ! calc. apparent precip. (eq. S-14)
      if( bszcr .ge. 7.6 ) bszcr = 7.599
      cumpa = -(alog(1.0-bszcr/7.6)) / (0.0705-0.0687*csfcla**0.146)

      ! check for threshold precip.
      ! ie. check to see if a H2O addition exceeding 10mm has been made
      ! *** threshold is not noted for S-15, this test should go later
      ! write(*,*) '*******cumpa + dcump<10.? ',cumpa,dcump
      if((cumpa + dcump) .lt. 10. ) then
         return
      end if

      ! calc. crust thickness (eq. S-16, *** sb S-15)
      temp = (0.0705 - 0.0687*csfcla**0.146)*(cumpa + dcump)
      if( temp .gt. 20.0 ) then                !check to avoid underflow
          bszcr = 7.6
      else
          bszcr = 7.6*(1.0 - exp(-temp))
      endif

      ! calc. apparent precip (eq. S-17 *** sb S-16)
      if( bsfcr .lt. 1.0 ) then
          cumpa = -(alog(1.0 - bsfcr))/0.045
          ! calc. crust cover fraction (eq. S-18, *** sb S-17)
          bsfcr = 1.0 - exp(- 0.045*(cumpa + dcump))
      end if

      ! loose erodible material on crust
      ! set max loose mass (eq S-20, *** sb S-19)
      if( bhzsmt .eq. 0.0 ) then
         ! no snow melt
         if( csfcla .eq. 0.0 ) then
            bsmlos = 0.1*exp(-0.57 + 0.22 * 999. + 7.0 * csfcce - csfom)
         else
            bsmlos = 0.1*exp(-0.57 + 0.22 * csfsan / csfcla + 7.0 * csfcce - csfom)
         end if
         ! set upper limit on loose mass (eq. S-21, *** sb S-20)
         if( bsmlos .gt. 3.0 ) then
            bsmlos = 3.0
         end if
      else
         ! check if water is from snowmelt (eq. S-22, *** sb S-21,22)
         if( bhzsmt .gt. 0.0 ) then
             ! snow melt
             bsmls0 = bsmlos
             bsmlos = bsmlos * (1.0 - 0.1 * bhzsmt)
             if( bsmlos .lt. bsmls0*0.1 ) then
                bsmlos = bsmls0 * 0.1
             end if
         else
             ! bsmlos = bsmlos * (1.0 - 0.0053 * dcump)
             bsmlos = bsmlos
         endif
      endif

      ! fraction cover of loose erodible material (eq. S-24, S-25, sb S-23,24)
      sz = amax1(4.0*bszrr, bszrgh)
      ! *** cflos = sqrt(bsmlos)/(0.24*sz)
      ! *** debugging fix
      cflos = exp(-0.08*sz**0.5)
      ! *** eodf
      if (cflos .gt. 1.0) then
         cflos = 1.0
      end if
      bsflos = (1.0 - exp(-3.5*bsmlos**1.5))*cflos

    end subroutine cru

    pure subroutine ranrou( csfsil, csfsan, bszrr, bszrro, cumpa, dcump, cf2cov, csvroc)

!     + + + ARGUMENT DECLARATIONS + + +
      real, intent(in) :: csfsil ! top layer fraction of silt.
      real, intent(in) :: csfsan ! top layer fraction of sand.
      real, intent(inout) :: bszrr  ! 
      real, intent(inout) :: bszrro ! 
      real, intent(out) :: cumpa  ! 
      real, intent(in) :: dcump  ! 
      real, intent(in) :: cf2cov ! 
      real, intent(in) :: csvroc ! soil volume fraction of rock in top layer

      ! + + + LOCAL VARIABLES + + +
      real :: arr  ! regression coef. to calc. random roughness
      real :: crr  ! regression coefficient for random roughness decrease

      ! calc. reg. coefficients (eq. S-12, S-13)
      arr = 91.08 + 765.8 * csfsil
      crr = 0.53 + 4.66 * csfsan - 3.8 * csfsan**1.5-1.22*(csfsan)**0.5

      ! calc. apparent precip. (eq. S-11 is S-14 solved for a bare surface)
      ! changed * to ** to conform to equ S-10
      ! erosion could make bszrr > bszrro so insert fix - LH
      if(bszrr .ge. bszrro) then
         cumpa = 0.0
         bszrro = bszrr
      else
         cumpa = arr * (-log(bszrr / bszrro)) ** (1.0 / crr)
      end if

      ! update random roughness (eq. S-14)
      ! *** debugging fix

      if ((cumpa + (1.0 - csvroc) * cf2cov * dcump)/arr .lt. 0.) then
         bszrr = bszrro
         ! write(*,*) 'soil: debugging fix executed 1'
         ! write(*,*) '  cumpa, dcump, cf2cov, arr, csvroc ', cumpa, dcump, cf2cov, arr, csvroc
      else
      ! *** end of debugging fix
      ! ***      write(*,*) ' crr ', crr
         bszrr = bszrro * exp(-((cumpa + (1.0 - csvroc) * cf2cov*dcump) /arr)**crr)
      endif
      if ( bszrr .lt. 2.0) then
         bszrr = 2.0
      end if
      
    end subroutine ranrou

    pure subroutine rid( cf2cov, bbfscv, bbffcv, bszrgh, bsxrgs, bszrho, cumpa, dcump, bsvroc )

      real, intent(out) :: cf2cov
      real, intent(in) :: bbfscv
      real, intent(in) :: bbffcv
      real, intent(inout) :: bszrgh
      real, intent(in) :: bsxrgs
      real, intent(in) :: bszrho
      real, intent(out) :: cumpa
      real, intent(in) :: dcump
      real, intent(in) :: bsvroc(*)

      ! + + + LOCAL VARIABLES + + +
      real :: cf1rg  ! correction factor for ridge scale

      ! calculate biomass cover sheltering factor (eq. S-9 & S-10 combined)
      cf2cov = 1.0 - 0.6 * (bbfscv + (1.0 - bbfscv)*bbffcv)

      ! if ridge height is zero, skip ridge update
      if (bszrgh .ne. 0.0) then
         ! calc. ridge scale factor (eq. S-8)
         cf1rg = (348.0 / bsxrgs)**0.3
         ! calculate apparent cum. precip. (eq. S-5)
         cumpa = ((1. - bszrgh/bszrho)/(0.034*cf1rg))**2.
         ! update ridge height (eq. S-6)
         bszrgh = bszrho * (1.0 - 0.034 * sqrt(cumpa + dcump * cf2cov * (1.0 - bsvroc(1)))*cf1rg)

         ! check to see that minimum bszrgh/bszrho > 0.05 if not then set
         ! the ratio to 0.05.
         if ((bszrgh/bszrho) .lt. 0.05) then
            bszrgh = 0.05 * bszrho
         end if
      endif
      
    end subroutine rid

end module soil_processes_mod

