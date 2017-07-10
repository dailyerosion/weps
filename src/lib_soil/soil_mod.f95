!$Author$
!$Date$
!$Revision$
!$HeadURL$

module soil_mod

  contains

    subroutine callsoil(daysim, isr, soil, croptot, biotot, h1et)
      ! Wrapper to call soil

      use biomaterial, only: biototal
      use timer_mod, only: timer, TIMSOIL, TIMSTART, TIMSTOP
      use soil_data_struct_defs, only: am0sdb, soil_def
      use hydro_data_struct_defs, only: hydro_derived_et

      ! Arguments
      integer daysim
      integer isr                   
      type(soil_def), intent(inout) :: soil  ! soil for this subregion
      type(biototal), intent(in) :: croptot, biotot
      type(hydro_derived_et), intent(inout) :: h1et

      ! Includes
      include 'p1werm.inc'
      include 'h1hydro.inc'
      include 'h1temp.inc'
      include 'h1db1.inc'

      call timer(TIMSOIL,TIMSTART)      

      if (am0sdb(isr) .eq. 1) then
         call sdbug(isr, soil, croptot, biotot, h1et)
      end if

      call soilproc(isr,daysim,ahlocirr(isr),h1et%zirr, ahzsmt(isr),  &
     &              ahtsmx(1,isr), ahtsmn(1,isr), &
     &                 soil%nslay, &
     &                 biotot%ffcvtot, biotot%fscvtot, &
     &                 ahzinf(isr), ahzwid(isr), soil)

      if (am0sdb(isr) .eq. 1) then
         call sdbug(isr, soil, croptot, biotot, h1et)
      end if

      ! recalculate  depth to bottom of soil layer
      call depthini( soil%nslay, soil%aszlyt, soil%aszlyd )

      call timer(TIMSOIL,TIMSTOP)      

    end subroutine callsoil

    subroutine soilproc (isr, daysim, bhlocirr, bhzirr, bhzsmt,           &
     &                 bhtsmx, bhtsmn,                                  &
     &                 bslay, &
     &                 bbffcv, bbfscv,                                  &
     &                 bhzinf, bhzwid, soil)


!     + + + PURPOSE + + +
! SOIL submodel for the Wind Erosion Prediction System model.
! update the SOIL (SURFACE: roughness, ridges, crust, and erodible material,
! and the LAYERS: aggregate size distribution, agg stability, and density).
! for more details on equations and processes, see SOIL SUBMODEL TECHNICAL
! DESCRIPTION.

!     + + + CONTRIBUTORS to CODE + + +
!     Imam Elminyawi,  Erik Monson, L. Hagen, Andy Hawkins, T. Zobeck

!     + + + KEY WORDS + + +
!     wind erosion, soil processes, surface process, layer process

!     + + + GLOBAL COMMON BLOCKS + + +

      use datetime_mod, only: get_simdate_doy, get_simdate_year
      use file_io_mod, only: luosoilsurf, luosoillay
      use soil_data_struct_defs, only: am0sfl
      use soil_processes_mod, only: updlay, cru, ranrou, rid
      use climate_input_mod, only: cli_today
      use process_mod, only: coef_abrasion
      use soil_data_struct_defs, only: soil_def

      include 'p1werm.inc'

!     + + + GLOBAL COMMON BLOCKS + + +
      include 'soil/cumulat.inc'

!     + + + ARGUMENT DECLARATIONS + + +
      integer, intent(in) :: isr   ! subregion number
      integer daysim
      real bhlocirr, bhzirr, bhzsmt
      real bhtsmx(*), bhtsmn(*)
      integer bslay
      real bbffcv, bbfscv
      real bhzinf, bhzwid
      type(soil_def), intent(inout) :: soil  ! soil for this subregion

!     + + + ARGUMENT DEFINITIONS + + +
!   daysim    - an index for the day of simulation.
!   bhlocirr  - location of irrigation application
!               + means above the soil surface
!               - means below the soil surface
!               soil surface reference is the bottom of the furrow 
!   bhzirr    - irrigation water applied, mm/day.
!   bhzsmt    - snowmelt, mm/day.
!   bhtsmx    - layer maximum temperature of today in C.
!   bhtsmn    - layer minimum temperature of today in C.
!   bslay     - number of soil layers
!   bbffcv    - biomass fraction flat cover
!   bbfscv    - biomass fraction standing cover
!   bhzinf    - daily water infiltration depth (mm of water)
!   bhzwid    - water infiltration depth (mm of soil)

!     + + + LOCAL VARIABLES + + +
! Retain the values of these variables for the next day
      include 'soil/prevday.inc'
! the 0 at the end of bhtmx0, bhrwc0, bszrr0, bszrh0 refer to
! prior day values of:
! max temperature, soil water content, random roughnes & ridge height

      real rain, snow, sprink
      real cumpa
      real cf2cov
      real szlyd(mnsz), laycenter(mnsz)
      real bsmls0
      real dcump
      integer yr, idoy
      integer ldx, trigger(bslay)

!     + + + LOCAL DEFINITIONS + + +
!   rain      - water added to soil as rain.
!   snow      - water equivalent added to soil surface as snow, mm.
!   sprink    - water added to soil as sprinkler irrigation, mm.
!   cumpa     - apparent (rain + sprinkler + snow-metl) to current
!               day from time of last tillage
!   cf2cov    - a plant cover correction factor for ridge height
!               and random roughness decrease as a result of rain.
!   szlyd     - depth to bottom of each soil layer, mm
!   laycenter - depth to middle of each soil layer, mm
!   bsmls0    - prior value of bsmlos before update by SOIL, kg/m^2
!   dcump     - total rain + sprinkler + snow-melt for current day.
!   yr        - current year of simulation for output.
!   idoy      - day of year for output
!   ldx       - index for layers
!   trigger   - bitmapped integer showing the state of soil property change
!               condition triggers for output into the layer detail file
!               This is the same as the value of the integer being set in 
!               powers of two
!               BIT - representative condition
!               0   - freeze
!               1   - freeze_thaw
!               2   - thaw
!               3   - frozen
!               4   - wetting
!               5   - drying
!               6   - warm_puddling
!               7   - wet_bulk_den

!     + + + END SPECIFICATIONS + + +

!     + + + INITIALIZATION  SECTION + + +

! call daily initialization
      call sinit (daysim, &
                  bhtsmx, soil%ahrwc, soil%asfom, soil%aszlyt, &
                  bslay, soil%asfsan, soil%asfsil, soil%asfcla, &
                  soil%aszrgh, soil%aslrr, soil%asfcce, soil%asfcec, &
                  cump(isr), dcump, &
                  bhtmx0(1,isr), bhrwc0(1,isr), szlyd, &
                  bszrr0(isr), bszrh0(isr), &
                  soil%aseagm, soil%aseagmn, soil%aseagmx, &
                  soil%aslmin, soil%aslmax, &
                  rain, snow, sprink, &
                  bhzirr, soil%aszrho, &
                  bhlocirr, bhzsmt, soil%aslrro, &
                  soil%asdsblk, cli_today%zdpt, cli_today%tdav, trigger)

!  UPDATE SURFACE
!     do surface processes if (rain+sprinkler+snowmelt>0)

      if (dcump .gt. 0.0) then

!  RIDGE SECTION:
        call rid(cf2cov, bbfscv, bbffcv, soil%aszrgh, &
          soil%asxrgs, soil%aszrho, cumpa, dcump, soil%asvroc)

!  RANDOM ROUGHNESS SECTION:
        call ranrou(soil%asfsil(1), soil%asfsan(1), soil%aslrr, soil%aslrro, &
     &    cumpa, dcump, cf2cov, soil%asvroc(1))

!  CRUST SECTION:
        call  cru(soil%aszcr, cumpa, soil%asfcla(1), dcump, &
          soil%asfcr, bhzsmt, soil%asmlos, soil%asfom(1), soil%asfcce(1), &
          soil%asfsan(1), bsmls0, soil%aszrgh, soil%aslrr, soil%asflos)
      endif

!  skip layer update on first simulation day
      if (daysim .ge. 2) then
        call updlay( daysim, szlyd, &
     &  bhrwc0(1,isr), soil%ahrwc, soil%ahrwcdmx, &
     &  soil%aseagmx, soil%aseagmn, soil%aseags, &
     &  soil%ahrwcw, soil%ahrwcs, &
     &  bhtsmn, bhtmx0(1,isr), bhtsmx, &
     &  soil%aslmin, soil%aslmax, &
     &  soil%aslagm, &
     &  soil%as0ags, soil%aslagn, soil%aslagx, soil%asdblk, &
     &  soil%aszlyt, soil%asdagd, bslay, &
     &  soil%asdsblk, bhzinf, bhzwid, trigger, isr)

        ! update surface properties based on surface layer properties
        ! crust stability
        soil%asecr = soil%aseags(1)
        ! crust density
        soil%asdcr = 0.576 + 0.603 * soil%asdsblk(1)
      end if

      ! aggregate coefficient of abrasion
      soil%acanag = coef_abrasion(soil%aseags(1))
      ! crust coefficient of abrasion
      soil%acancr = coef_abrasion(soil%asecr)

!     Assign today's values to 'yesterday storage'
      do ldx = 1,bslay
          bhtmx0(ldx,isr) = bhtsmx(ldx)
          bhrwc0(ldx,isr) = soil%ahrwc(ldx)
      end do

      bszrr0 = soil%aslrr
      bszrh0 = soil%aszrgh

!     + + + OUTPUT FORMATS + + +
 2100 format('#daysim idoy yr cump dcump bszrgh bsxrgs bszrr bszcr bsfcr&
     & bsecr bsmlos bsflos bcanag bcancr')
 2200 format( 3(1x,i4), 10(1x,f8.4) )
 2300 format('#daysim|idoy|yr| layer|&
     &cntr_dpth|lay_depth|lay_thick| bulk_dens|&
     &    agstab|min_agstab|ave_agstab|max_agstab|&
     &       gmd|       gsd|min_agsize|max_agsize|&
     &   min_gmd|   max_gmd|   rel_gmd|&
     &   ag_dens| rel_agden|&
     &  freeze|frz_thw|   thaw| frozen|wetting| drying|puddling|wet_bulk_den')
 2400 format( i6, 1x,i3, 1x,i4, 1x,i3, 17(1x,f10.4), 8(7x,b1) )

!  + + +  OUTPUT SECTION  + + +

      if ((am0sfl(isr) .eq. 1)) then
         ! get some date, day variables
         yr = get_simdate_year()
         idoy = get_simdate_doy()

         ! write output headers
         if( daysim .eq. 1 ) then
             write(luosoilsurf(isr),2100)
             write(luosoillay(isr),2300)
         end if
         ! insert single blank line to break layer blocks for graphing
         write(luosoillay(isr),*)
         ! insert additional blank line (make double) to break years into blocks for graphing
         if( idoy .eq. 1 ) then
             write(luosoilsurf(isr),*)
             write(luosoilsurf(isr),*)
             write(luosoillay(isr),*)
         end if

         write(luosoilsurf(isr), 2200) daysim,idoy,yr, cump(isr), dcump, &
              soil%aszrgh, soil%asxrgs, soil%aslrr, soil%aszcr, &
              soil%asfcr, soil%asecr, soil%asmlos, soil%asflos, &
              soil%acanag, soil%acancr

! output new values by layer to the soil output file.
         do ldx = 1,bslay
            if( ldx .eq. 1 ) then
              laycenter(ldx) = 0.5 * szlyd(ldx)
            else
              laycenter(ldx) = 0.5 * ( szlyd(ldx-1) + szlyd(ldx) )
            end if

            write (luosoillay(isr),2400) daysim, idoy, yr, ldx,         &
     &          laycenter(ldx), soil%aszlyd(ldx), soil%aszlyt(ldx), soil%asdblk(ldx), &
     &          soil%aseags(ldx), soil%aseagmn(ldx), soil%aseagm(ldx), soil%aseagmx(ldx),   &

     &          soil%aslagm(ldx), soil%as0ags(ldx), soil%aslagn(ldx), soil%aslagx(ldx), &
     &          soil%aslmin(ldx), soil%aslmax(ldx),     &
     &          (soil%aslagm(ldx) - soil%aslmin(ldx))/(soil%aslmax(ldx) - soil%aslmin(ldx)),&
     &          soil%asdagd(ldx), (soil%aseags(ldx)-soil%aseagmn(ldx))/(soil%aseagmx(ldx)-soil%aseagmn(ldx)), &
     &          ibits(trigger(ldx),0,1), ibits(trigger(ldx),1,1),       &
     &          ibits(trigger(ldx),2,1), ibits(trigger(ldx),3,1),       &
     &          ibits(trigger(ldx),4,1), ibits(trigger(ldx),5,1),       &
     &          ibits(trigger(ldx),6,1), ibits(trigger(ldx),7,1)
         end do
      endif

      return
    end subroutine soilproc

    subroutine depthini(nlay, bszlyt, bszlyd)

      integer nlay
      real    bszlyt(*), bszlyd(*)

      integer idx

!     nlay - number of soil layers
!     bszlyt - soil layer thickness (mm)
!     bszlyd - depth to bottom of soil layer (mm)

      bszlyd(1) = bszlyt(1)
      do idx = 2, nlay
        bszlyd(idx) = bszlyt(idx) + bszlyd(idx-1)
      end do

      return
    end subroutine depthini

    subroutine sinit (daysim,                                         &
     &                 bhtsmx, bhrwc, bsfom, bszlyt,                    &
     &                 bslay, bsfsan, bsfsil, bsfcla,                   &
     &                 bszrgh, bszrr, bsfcce, bsfcec,                   &
     &                 cump, dcump,                                     &
     &                 bhtmx0, bhrwc0, szlyd,                           &
     &                 bszrr0, bszrh0,                                  &
     &                 bseagm, bseagmn, bseagmx,                        &
     &                 bslmin, bslmax,                                  &
     &                 rain, snow, sprink,                              &
     &                 bhzirr, bszrho,                                  &
     &                 bhlocirr, bhzsmt, bszrro,                        &
     &                 bsdsblk, bwzdpt, bwtdav, trigger)

!     + + + PURPOSE + + +
! SOIL submodel for the Wind Erosion Prediction System model.
! daily initialization of soil properties
! (SURFACE: roughness, ridges, crust, and erodible material,
! and the LAYERS: aggregate size distribution, agg stability, and density).
! for more details on equations and processes, see SOIL SUBMODEL TECHNICAL
! DESCRIPTION.

!     + + + KEY WORDS + + +
!     wind erosion, soil processes, surface process, layer process

!     + + + ARGUMENT DECLARATIONS + + +
      integer daysim
      real bhtsmx(*), bhrwc(*), bsfom(*), bszlyt(*)
      integer bslay
      real bsfsan(*), bsfsil(*), bsfcla(*)
      real bszrgh, bszrr, bsfcce(*), bsfcec(*)
      real cump, dcump
      real bhtmx0(*), bhrwc0(*), szlyd(*)
      real bszrr0, bszrh0
      real bseagm(*), bseagmn(*), bseagmx(*)
      real bslmin(*),bslmax(*)
      real rain, snow, sprink
      real bhzirr, bszrho
      real bhlocirr, bhzsmt, bszrro
      real bsdsblk(*), bwzdpt, bwtdav
      integer trigger(bslay)

!     + + + ARGUMENT DEFINITIONS + + +
!   daysim    - an index for the day of simulation.
!   bhtsmx    - layer maximum temperature of today in C.
!   bhrwc     - soil water content for today, kg/kg.
!   bsfom     - layer fraction of organic matter.
!   bszlyt    - layer thickness, mm.
!   bslay     - number of soil layers
!   bsfsan    - layer fraction of sand.
!   bsfsil    - layer fraction of silt.
!   bsfcla    - layer fraction of clay.
!   bszrgh    - ridge height, mm.
!   bszrr     - random roughness height, mm
!   bsfcce    - soil fraction calcium carbonate equivalent
!   bsfcec    - soil cation exchange capacity (cmol/kg)
!   cump      - cumulative (rain + sprinkler + snow-melt) to current
!               day from day 1 or time of last tillage
!   dcump     - total rain + sprinkler + snow-melt for current day.
!   bhtmx0    - layer maximum temperature of yesterday. in C
!   bhrwc0    - soil water content for yesterday. mass basis kg/kg.
!   szlyd     - depth to bottom of each soil layer, mm
!   bszrr0    - prior day random roughness, mm
!   bszrh0    - prior day ridge height, mm
!   bseagm    - mean agg stability, ln(J/kg)
!   bseagmn   - minimum agg stability, ln(J/kg)
!   bseagmx   - maximum agg stability, ln(J/kg)
!   bslmin    - min value of aggregate gmd
!   bslmax    - max value of aggregate gmd
!   rain      - water added to soil as rain.
!   snow      - water equivalent added to soil surface as snow, mm.
!   sprink    - water added to soil as sprinkler irrigation, mm.
!   bhzirr    - irrigation water applied, mm/day.
!   bszrho    - ridge height right after tillage, mm.
!   bhlocirr  - location of irrigation application, mm.
!               + means above the soil surface
!               - means below the soil surface
!               soil surface reference is the bottom of the furrow
!   bhzsmt    - snowmelt, mm/day.
!   bszrro    - random roughness height right after tillage, mm.
!   bsdsblk    - consolidated soil bulk density by layer, Mg/m^3
!   bwzdpt    - rainfall depth (mm)
!   bwtdav    - Average daily air temperature (deg C)
!   trigger   - bitmapped integer showing the state of soil property change
!               condition triggers for output into the layer detail file

! the 0 at the end of bhtmx0, bhrwc0, bszrr0, bszrh0 refer to
! prior day values of:
! max temperature, soil water content, random roughnes & ridge height
! bszrro , bszrho are right-after-tillage

!     + + + LOCAL VARIABLES + + +
      real sf84m(bslay), sf84sd(bslay), scecr
      real tsfom, tsfcce, tsfsacl
      integer ldx

!     + + + LOCAL DEFINITIONS + + +
!   sf84m     - mean of fraction agg. < 0.84 mm
!   sf84sd    - standard deviation of fraction agg. < 0.84 mm
!   scecr     - ratio of clay fraction cation exchange capacity
!               to percent clay
!   tsfom     - temporary layer fraction of organic matter.
!   tsfcce    - temporary soil fraction calcium carbonate equivalent
!   tsfsacl   - temporary layer fraction of clay.
!   ldx       - layer index

!     + + + FUNCTIONS CALLED + + +

!     + + + SUBROUTINES CALLED + + +

!     + + + END SPECIFICATIONS + + +

      ! check for first day
      if (daysim .eq. 1) then
         ! set up tillage check
         bszrr0 = - 1.0
         bszrh0 = - 1.0
         ! initialize previous day temperature and water content
         do ldx = 1, bslay
             bhtmx0(ldx) = bhtsmx(ldx)
             bhrwc0(ldx) = bhrwc(ldx)
         end do
      endif

      szlyd(1) = bszlyt(1)
      trigger(1) = 0
      do ldx = 2, bslay
          ! calc. depth to bottom of each layer
          szlyd(ldx) = szlyd(ldx-1) + bszlyt(ldx)
          ! zero out trigger condition array
          trigger(ldx) = 0
      end do

      ! if tillage (or anything else outside of soil submodel)
      ! changed roughness or ridge height then update
      if ((bszrr0.ne.bszrr).or.(bszrh0.ne.bszrgh)) then

!        store initial or after tillage surface roughness
         bszrro = bszrr
         bszrho = bszrgh

!        set cumulative precip to zero
         cump = 0.0

!        store/calculate initial layer values
         do 10 ldx = 1, bslay
!            store initial water content & yesterday's temperature
             bhtmx0(ldx) = bhtsmx(ldx)
             bhrwc0(ldx) = bhrwc(ldx)
!           calc. mean, min, and max agg. stability
!           (eq. S-26, S-27, S-28, *** sb S-25,26,27)
            if (bsfcla(ldx) .gt. 0.6) then
               bseagm(ldx) = 3.484
            else
              bseagm(ldx) = -16.73 - 46.629*bsfcla(ldx)**2              &
     &                    + 23.514*bsfcla(ldx)**3                       &
     &                    + 17.519*exp(bsfcla(ldx))
            endif
            bseagmn(ldx) = bseagm(ldx) - 2*(0.16)*bseagm(ldx)
            bseagmx(ldx) = bseagm(ldx) + 2*(0.16)*bseagm(ldx)
!           calc. mean and standard deviation of fraction agg. < 0.84 mm
!           (eq. S-42, S-43, *** sb S-37, S-38)

! ***            sf84m(ldx)  = 0.2902 + 0.31 * bsfsan(ldx) + 0.17 * bsfsil(ldx) +
! ***     &       0.0033*bsfsan(ldx)/bsfcla(ldx) - 4.66*bsfom(ldx) - 0.95*bsfcce(ldx)
! *** debugging fix, 1st try
! ***            sf84m(ldx)  = 0.2902 + 0.31 * bsfsan(ldx) + 0.17 * bsfsil(ldx) +
! ***     &       0.0033*bsfsan(ldx)/bsfcla(ldx) - 4.66*bsfom(ldx)
! eodf
! *** debugging fix, 2nd try
! clamping upper limits on variables to keep them from forcing sf84m negative
! note that this needs correcting by a more robust regression equation
            tsfom = bsfom(ldx)
            if (tsfom.gt.0.03) tsfom = 0.03
            tsfcce = bsfcce(ldx)
            if (tsfcce.gt.0.2) tsfcce = 0.2
            if (bsfcla(ldx).eq.0) tsfsacl = 40.
            if (bsfcla(ldx).ne.0) tsfsacl = bsfsan(ldx) / bsfcla(ldx)
            if (tsfsacl.gt.40) tsfsacl = 40.
! *** convert organic carbon to organic matter by dividing by 1.724
            if ((bsfsan(ldx) .ge. .15).and.(bsfcla(ldx) .le. 0.25)) then
              sf84m(ldx)  = 0.2909 + 0.31*bsfsan(ldx) + 0.17*bsfsil(ldx) &
     &                + 0.01*tsfsacl - 4.66*tsfom/1.724 - 0.95*tsfcce
            else
              sf84m(ldx)  = 0.2909 + 0.31*bsfsan(ldx) + 0.17*bsfsil(ldx) &
     &                + 0.0033*tsfsacl - 4.66*tsfom/1.724 - 0.95*tsfcce
            end if
! *** eodf
            sf84sd(ldx) = (0.41 - 0.22*bsfsan(ldx))*sf84m(ldx)
! ***       write(*,*) ' sf84m(ldx), sf84sd(ldx) ', ldx, sf84m(ldx), sf84sd(ldx)

!           calc. min and max values of geom. mean agg. diameter (eq. S-45, S-46)
            bslmin(ldx) = exp(3.44 - 7.21*(sf84m(ldx)+ 2.0*sf84sd(ldx)))
            if (bslmin(ldx) .lt. 0.025) bslmin(ldx) = 0.025
            bslmax(ldx) = exp(3.44 - 7.21*(sf84m(ldx)- 2.0*sf84sd(ldx)))
            if (bslmax(ldx) .gt. 31.0) bslmax(ldx) = 31.0
            if(bslmin(ldx).ge.bslmax(ldx)) write(*,*) 'sinit:min.gt.max'

! ***       write(*,*) 'bslmin(ldx),bslmax(ldx)',ldx,bslmin(ldx),bslmax(ldx)
!           calc. ratio of clay cation exchange capacity to percent clay (eq. S-53)
            scecr = (bsfcec(ldx) - bsfom(ldx) * (142. + 0.17 *          &
     &        szlyd(ldx)))/ (bsfcla(ldx) * 100.0 + 0.0001)
            if (scecr .lt. 0.15) scecr = 0.15
            if (scecr .gt. 0.65) scecr = 0.65

! *** remove calculation of cbd; replace with original cbd calc from inpsub
! ***            sdbko(ldx) = bsdsblk(ldx)
! ***c           calc. consolidated bulk density (eq. S-52)
! ***            sdbko(ldx) = 1.514 + 0.25*bsfsan(ldx) -
! ***     *        13.*bsfsan(ldx)*bsfom(ldx) -6.0*bsfcla(ldx)*
! ***     *        bsfom(ldx) - 0.48*bsfcla(ldx)*scecr

! ***            if (sdbko(ldx) .gt. 2.2) sdbko(ldx) = 2.2
! ***            if (sdbko(ldx) .lt. 0.5) sdbko(ldx) = 0.5
! ***c           calc. increase in consolidated bulk density with depth
! ***c           (note the depths change slightly with time, but current
! ***c           update only occurs with tillage.)
! ***c           (eq. S-54)
! *** debugging fix
! ***            sdbko(ldx) = sdbko(ldx)*(0.975+ 1.931/
! ***     *        (1+exp(-(szlyd(ldx)-506.8)/118.5)))
! *** eodf

! *** removed since value is set in updlay for every layer calculation
! *** This equation is not the same and does not match the most current documentation
! ***          set stability drying process coefficient:
! ***            if( ldx .eq. 1) then
! ***              bsk4d(ldx) = 0.46 - 0.23 * exp(-(szlyd(ldx)/2.0)/88.57)
! ***            else
! ***              bsk4d(ldx) = 0.46 - 0.23 * exp(-(szlyd(ldx-1) +           &
! ***     &               (szlyd(ldx) - szlyd(ldx-1))/2.0)/88.57)
! ***            end if

 10      continue

      endif
!23456789*23456789*23456789*23456789*23456789*23456789*23456789*2345
!
!     set stability freezing and wetting process coefficients:
!
!     initialize rain and snow variables
      rain = 0.0
      snow = 0.0
!     Determine if precip
      if (bwzdpt .gt. 0.0) then
!     Determine if precip. is rain or snow
!     (note HYDROLOGY may do this in future using ELS results)
         if (bwtdav .ge. 0.0) rain = bwzdpt
         if (bwtdav .lt. 0.0) snow = bwzdpt
      endif

      ! add irrigation to cumulative precipitation based on application
      ! height with respect to ridge height
      if (bhlocirr .ge. bszrgh ) then
         ! irrigation is being applied from above ridge height
         ! add full amount to degrade ridge height and random roughness
         sprink = bhzirr
      else if (bhlocirr .ge. 0.0 ) then
         ! irrigation application is below ridge height
         ! partially include reducing degradation (like furrow irrigation)
         sprink = bhzirr * bhlocirr / bszrgh
      else
         ! irrigation application underground
         ! no degradation of ridge height or randowm roughness
         sprink = 0.0
      endif
!     Calc. daily and cumulative (rain + sprinkler irrigation + snowmelt)
      dcump = rain + sprink + bhzsmt
!     (note: cump not used in calc., but useful as ouptput for validation)
      cump = cump + dcump
      end subroutine sinit

    subroutine soilinit(soil)
! ***************************************************************** wjr
! Contains init code from main
!
!       Edit History
!       04-Mar-99       wjr     created
      use soil_data_struct_defs, only: soil_def

      type(soil_def), intent(inout) :: soil  ! soil for this subregion

      ! recalculate  depth to bottom of soil layer
      call depthini( soil%nslay, soil%aszlyt, soil%aszlyd )

    end subroutine soilinit

    subroutine  sdbug(isr, soil, croptot, biotot, h1et)

!     + + + PURPOSE + + +
!    This program prints out many of the global variables before
!    and after the call to SOIL provide a comparison of values
!    which may be changed by SOIL

!    author: John Tatarko
!    version: 08/30/92

!     + + + KEY WORDS + + +
!     wind, erosion, hydrology, tillage, soil, crop, decomposition

      use weps_main_mod, only: daysim, am0ifl
      use datetime_mod, only: get_simdate
      use file_io_mod, only: luosdb
      use soil_data_struct_defs, only: soil_def
      use biomaterial, only: biototal
      use erosion_data_struct_defs, only: awadir, awhrmx, awudmx, awudmn
      use climate_input_mod, only: cli_today, amzele
      use hydro_data_struct_defs, only: hydro_derived_et

!     + + + GLOBAL COMMON BLOCKS + + +
      include 'p1werm.inc'
      include 'h1hydro.inc'
      include 'h1db1.inc'
      include 'h1temp.inc'

!     + + + LOCAL COMMON BLOCKS + + +
      include 'soil/tsdbug.inc'

!     + + + ARGUMENT DECLARATIONS + + +
      integer isr
      type(soil_def), intent(inout) :: soil  ! soil for this subregion
      type(biototal), intent(in) :: croptot, biotot
      type(hydro_derived_et), intent(in) :: h1et

!     + + + LOCAL VARIABLES + + +
      integer cd, cm, cy, l

!     + + + LOCAL DEFINITIONS + + +

!   cd        - The current day of simulation month.
!   cm        - The current month of simulation year.
!   cy        - The current year of simulation run.
!   daysim    - The surrent day of the simulation run.
!   isr       - This variable holds the subregion index.
!   l         - This variable is an index on soil layers.

!     + + + SUBROUTINES CALLED + + +

!     + + + FUNCTIONS CALLED + + +

!     + + + UNIT NUMBERS FOR INPUT/OUTPUT DEVICES + + +
!     * = screen and keyboard
!    26 = debug SOIL

!     + + + DATA INITIALIZATIONS + + +

      if (am0ifl  .eqv. .true.) then
          tday = -1
          tmo = -1
          tyr = -1
          tisr = -1
      end if
      call get_simdate (cd, cm, cy)

!     + + + INPUT FORMATS + + +

!     + + + OUTPUT FORMATS + + +
 2030 format ('**',1x,2(i2,'/'),i4,' daysim=',i4,'   After  call to SOIL&
     &    Subregion No. ',i3)
 2031 format ('**',1x,2(i2,'/'),i4,' daysim=',i4,'   Before call to SOIL&
     &    Subregion No. ',i3)
 2032 format (' cli_today%zdpt  cli_today%tdmx  cli_today%tdmn  cli_today%eirr  awudmx  awudmn ', &
     &        ' cli_today%tdpt  awadir  awhrmx  amzele ')
 2038 format (f7.2,9f8.2)
 2050 format ('amrslp(',i2,') croptot%ftcvtot(',i2,') croptot%rlaitot(',&
     &  i2,')',                                                         &
     &      ' croptot%zrtd(',i2,') biotot%mftot(',i2,') ahfwsf(',i2,')',&
     &      ' ahzper(',i2,')')
 2051 format (2f10.2,2f10.5,2x,f10.2,f10.2,f12.2)
 2052 format ('ahzrun(',i2,') h1et%zirr(',i2,') ahzsno(',i2,')',           &
     &        ' ahzsmt(',i2,') asxrgs(',i2,') aszrgh(',i2,')',          &
     &        ' aslrr(',i2,')')
 2053 format (5f10.2,2f12.2)
 2054 format (' asfcr(',i2,')  asecr(',i2,') asmlos(',i2,')',           &
     &        ' asflos(',i2,')  aszcr(',i2,')')
 2055 format (2f10.2,2f10.3,i10,2f12.2)
 2056 format('layer aszlyt  ahrsk ahrwc ahrwcs ahrwca',                 &
     &       ' ahrwcf ahrwcw ah0cb aheaep ahtsmx ahtsmn')
 2060 format (i4,1x,f6.1,1x,e7.1,f6.2,4f7.2,f6.2,3f7.2)
 2065 format(' layer asfsan asfcla asfom asdsblk asdblk aslagm as0ags', &
     &       ' aslagn  aslagx  aseags')
 2070 format (i4,2x,2f7.2,f7.3,3f7.2,f8.2,f7.3,2f8.2)

!     + + + END SPECIFICATIONS + + +

!          write weather cligen and windgen variables
      if ((cd .eq. tday) .and. (cm .eq. tmo) .and. (cy .eq. tyr) .and.  &
     &   (isr .eq. tisr)) then
         write(luosdb(isr),2030) cd,cm,cy,daysim,isr
      else
         write(luosdb(isr),2031) cd,cm,cy,daysim,isr
      end if
      write(luosdb(isr),2032)
      write(luosdb(isr),2038) cli_today%zdpt,cli_today%tdmx,cli_today%tdmn,cli_today%eirr,awudmx,awudmn,&
     &               cli_today%tdpt,awadir,awhrmx,amzele

      write(luosdb(isr),2050) isr,isr,isr,isr,isr,isr,isr

      write(luosdb(isr),2051) soil%amrslp, croptot%ftcvtot,             &
     &              croptot%rlaitot,                                    &
     &              croptot%zrtd, biotot%mftot, ahfwsf(isr), h1et%zper
      write(luosdb(isr),2052) isr,isr,isr,isr,isr,isr,isr
      write(luosdb(isr),2053) h1et%zrun,h1et%zirr,ahzsno(isr), &
                     ahzsmt(isr), soil%asxrgs,soil%aszrgh,soil%aslrr
      write(luosdb(isr),2054) isr,isr,isr,isr,isr,isr,isr
      write(luosdb(isr),2055) soil%asfcr, soil%asecr, soil%asmlos, &
     &               soil%asflos, soil%aszcr
      write(luosdb(isr),2056)

      do 200 l = 1,soil%nslay
         write(luosdb(isr),2060) l, soil%aszlyt(l), soil%ahrsk(l), soil%ahrwc(l), &
     &                  soil%ahrwcs(l), soil%ahrwca(l), soil%ahrwcf(l), &
     &                  soil%ahrwcw(l), soil%ah0cb(l), soil%aheaep(l), &
     &                  ahtsmx(l,isr), ahtsmn(l,isr)
  200 continue
      write(luosdb(isr),2065)

      do 300 l=1,soil%nslay
         write(luosdb(isr),2070) l, soil%asfsan(l), soil%asfcla(l), &
     &                  soil%asfom(l), soil%asdsblk(l), soil%asdblk(l), &
     &                  soil%aslagm(l), soil%as0ags(l), soil%aslagn(l), &
     &                  soil%aslagx(l), soil%aseags(l)
  300 continue

      tisr = isr
      tday = cd
      tmo = cm
      tyr = cy

      return
    end subroutine sdbug

end module soil_mod

