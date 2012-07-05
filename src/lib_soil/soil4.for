!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine soil (daysim, bhlocirr, bhzirr, bhzsmt,                &
     &                 bhtsmx, bhtsmn,                                  &
     &                 bhrwc, bhrwcdmx, bhrwca,                         &
     &                 bhrwcw, bhrwcs, bszlyt, bslay,                   &
     &                 bsfsan, bsfsil, bsfcla, bsfom, bsvroc,           &
     &                 bsxrgs, bszrgh, bszrho,                          &
     &                 bszrr, bszrro,                                   &
     &                 bszcr, bsfcr, bsecr, bsdcr,                      &
     &                 bsmlos, bsflos,                                  &
     &                 bsdsblk, bsdwblk,                                &
     &                 bsdblk, bsdagd,                                  &
     &                 bslagm, bslagn,                                  &
     &                 bs0ags, bslagx, bseags,                          &
     &                 bseagm, bseagmn, bseagmx,                        &
     &                 bsk4d, bslmin, bslmax,                           &
     &                 bbffcv, bbfscv,                                  &
     &                 bsfcce, bsfcec, bhzinf, bhzwid, bwzdpt, bwtdav)


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
      include 'p1werm.inc'
      include 'wpath.inc'
      include 'm1subr.inc'
      include 'm1flag.inc'
      include 'file.inc'

!     + + + GLOBAL COMMON BLOCKS + + +
      include 'soil/cumulat.inc'

!     + + + ARGUMENT DECLARATIONS + + +
      integer daysim
      real bhlocirr, bhzirr, bhzsmt
      real bhtsmx(mnsz), bhtsmn(mnsz)
      real bhrwc(mnsz), bhrwcdmx(mnsz), bhrwca(mnsz)
      real bhrwcw(mnsz), bhrwcs(mnsz), bszlyt(mnsz)
      integer bslay
      real bsfsan(1:mnsz), bsfsil(1:mnsz), bsfcla(1:mnsz)
      real bsfom(1:mnsz), bsvroc(1:mnsz)
      real bsxrgs, bszrgh, bszrho
      real bszrr, bszrro
      real bszcr, bsfcr, bsecr, bsdcr
      real bsmlos, bsflos
      real bsdsblk(mnsz), bsdwblk(mnsz)
      real bsdblk(0:mnsz), bsdagd(0:mnsz)
      real bslagm(0:mnsz), bslagn(0:mnsz)
      real bs0ags(0:mnsz), bslagx(0:mnsz), bseags(0:mnsz)
      real bseagm(mnsz), bseagmn(mnsz), bseagmx(mnsz)
      real bsk4d(mnsz), bslmin(mnsz), bslmax(mnsz)
      real bbffcv, bbfscv
      real bsfcce(1:mnsz), bsfcec(1:mnsz)
      real bhzinf, bhzwid, bwzdpt, bwtdav

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
!   bhrwc     - soil water content for today, kg/kg.
!   bhrwcdmx  - daily maximum soil water content for today, kg/kg.
!   bhrwca    - soil avaiable water content on mass basis kg water/kg soil.
!   bhrwcw    - wilting point = 15 bar-grav. soil water content, kg/kg
!   bszlyt    - layer thickness, mm.
!   bslay     - number of soil layers
!   bsfsan    - layer fraction of sand.
!   bsfsil    - layer fraction of silt.
!   bsfcla    - layer fraction of clay.
!   bsfom     - layer fraction of organic matter.
!   bsvroc    - soil volume fraction of rock in each layer
!   bsxrgs    - ridge spacing. we have a relation between this and bszrgh.
!   bszrgh    - ridge height, mm.
!   bszrho    - ridge height right after tillage, mm.
!   bszrr     - random roughness height, mm.
!   bszrro    - random roughness height right after tillage, mm.
!   bszcr     - crust thickness.
!   bsfcr     - fraction of soil crust cover. m^2/m^2.
!   bsecr     - dry crust stability, ln(J/kg).
!   bsdcr     - crust density. Mg/m^2
!   bsmlos    - amount of loose material on crusted area, kg/m^2.
!   bsflos    - surface cover fraction of loose material on crust area, m^2/m^2.
!   bsdsblk    - consolidated soil bulk density by layer, Mg/m^3
!   bsdwblk    - Bulk Density of soil measured at 1/3 bar, Mg/m^3
!   bsdblk    - current layer density may be different from bsdsblk.
!   bsdagd    - aggregate density.
!   bslagm    - aggregate geometric mean diameter, mm.
!   bslagn    - minimum geometric diameter for aggregates in each
!               layer, mm.
!   bs0ags    - aggregate geometric standard deviation.
!   bslagx    - maximum value of aggregate size (mm)
!               (that aggregate may reach)
!   bseags    - agg stability, ln(J/kg).
!   bseagm    - mean agg stability, ln(J/kg)
!   bseagmn   - minimum agg stability, ln(J/kg)
!   bseagmx   - maximum agg stability, ln(J/kg)
!   bsk4d     - drying process coef. to calc. aggregate stability
!   bslmin    - min value of aggregate gmd
!   bslmax    - max value of aggregate gmd
!   bbffcv    - biomass fraction flat cover
!   bbfscv    - biomass fraction standing cover
!   bsfcce    - soil fraction calcium carbonate equivalent
!   bsfcec    - soil cation exchange capacity (cmol/kg)
!   bhzinf    - daily water infiltration depth (mm of water)
!   bhzwid    - water infiltration depth (mm of soil)
!   bwzdpt    - rainfall depth (mm)
!   bwtdav    - Average daily air temperature (deg C)

!     + + + LOCAL VARIABLES + + +
! Retain the values of these variables for the next day
      include 'soil/prevday.inc'
! the 0 at the end of bhtmx0, bhrwc0, bszrr0, bszrh0 refer to
! prior day values of:
! max temperature, soil water content, random roughnes & ridge height

      real rain, snow, sprink
      real cumpa
      real cf2cov
      real szlyd(0:mnsz), laycenter(mnsz)
      real bsmls0
      real dcump
      integer day, mo, yr, idoy
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
!   day       - current day of simulation for output.
!   mo        - current month of simulation for output.
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

!     + + + FUNCTIONS CALLED + + +
      integer dayear

!     + + + SUBROUTINES CALLED + + +
!   caldat    - input: julian day, output: day, mo, yr

!     + + + END SPECIFICATIONS + + +

!     + + + INITIALIZATION  SECTION + + +


! call daily initialization
      call sinit (daysim,                                               &
     &                 bhtsmx, bhrwc, bsfom, bszlyt,                    &
     &                 bslay, bsfsan, bsfsil, bsfcla,                   &
     &                 bszrgh, bszrr, bsfcce, bsfcec,                   &
     &                 cump, dcump, bsk4d,                              &
     &                 bhtmx0, bhrwc0, szlyd(0),                        &
     &                 bszrr0, bszrh0,                                  &
     &                 bseagm, bseagmn, bseagmx,                        &
     &                 bslmin, bslmax,                                  &
     &                 rain, snow, sprink,                              &
     &                 bhzirr, bszrho,                                  &
     &                 bhlocirr, bhzsmt, bszrro,                        &
     &                 bsdsblk, bwzdpt, bwtdav, trigger)
!
!  UPDATE SURFACE
!     do surface processes if (rain+sprinkler+snowmelt>0)

      if (dcump .gt. 0.0) then

!  RIDGE SECTION:
        call rid(cf2cov, bbfscv, bbffcv, bszrgh,                        &
     &    bsxrgs, bszrho, cumpa, dcump, bsvroc)

!
!  RANDOM ROUGHNESS SECTION:
        call ranrou(bsfsil(1), bsfsan(1), bszrr, bszrro,                &
     &    cumpa, dcump, cf2cov, bsvroc(1))

!
!  CRUST SECTION:
        call  cru(bszcr, cumpa, bsfcla(1), dcump,                       &
     &    bsfcr, bhzsmt, bsmlos, bsfom(1), bsfcce(1),                   &
     &    bsfsan(1), bsmls0, bszrgh, bszrr, bsflos)
      endif

!  skip layer update on first simulation day
      if (daysim .ge. 2)                                                &
     &  call updlay( daysim, szlyd,                                     &
     &  bhrwc0, bhrwc, bhrwcdmx,                                        &
     &  bseagmx, bseagmn, bseags,                                       &
     &  bhrwca, bhrwcw, bhrwcs,                                         &
     &  bhtsmn, bhtmx0, bhtsmx,                                         &
     &  bsecr,                                                          &
     &  bsk4d, bslmin, bslmax,                                          &
     &  bslagm,                                                         &
     &  bs0ags, bslagx, bsdblk,                                         &
     &  bszlyt, bsdagd, bslay, bsdcr,                                   &
     &  bsdsblk, bsdwblk, bhzinf, bhzwid, trigger)

!     Assign today's values to 'yesterday storage'
      do ldx = 1,bslay
          bhtmx0(ldx) = bhtsmx(ldx)
          bhrwc0(ldx) = bhrwc(ldx)
      end do

      bszrr0 = bszrr
      bszrh0 = bszrgh

!     + + + OUTPUT FORMATS + + +
 2100 format('#daysim idoy yr cump dcump bszrgh bsxrgs bszrr bszcr bsfcr&
     & bsecr bsmlos bsflos')
 2200 format( 3(1x,i4), 10(1x,f8.4) )
 2300 format('#daysim idoy yr layer depth bszlyt bsdblk bseags bseagmn b&
     &seagm bseagmx bslagn bslmin bslagm bslmax bslagx bs0ags bsdagd rel&
     &_ag_stab rel_geo_mean freeze freeze_thaw thaw frozen wetting dryin&
     &g warm_puddling wet_bulk_den ')
 2400 format( i6, 1x,i3, 1x,i4, 1x,i3, 16(1x,f10.4), 8(1x,b1) )

!  + + +  OUTPUT SECTION  + + +

      if ((am0sfl .eq. 1)) then
         ! get some date, day variables
         call caldatw(day,mo,yr)
         idoy = dayear(day, mo, yr)

         ! write output headers
         if( daysim .eq. 1 ) then
             write(luosoilsurf,2100)
             write(luosoillay,2300)
         end if
         ! insert single blank line to break layer blocks for graphing
         write(luosoillay,*)
         ! insert additional blank line (make double) to break years into blocks for graphing
         if( idoy .eq. 1 ) then
             write(luosoilsurf,*)
             write(luosoilsurf,*)
             write(luosoillay,*)
         end if

         write(luosoilsurf, 2200) daysim, idoy, yr, cump, dcump, bszrgh,&
     &               bsxrgs, bszrr, bszcr, bsfcr, bsecr, bsmlos, bsflos

! output new values by layer to the soil output file.
         do ldx = 1,bslay
            laycenter(ldx) = 0.5 * ( szlyd(ldx-1) + szlyd(ldx) )
            write (luosoillay,2400) daysim, idoy, yr, ldx,              &
     &          laycenter(ldx), bszlyt(ldx), bsdblk(ldx),               &
     &          bseags(ldx), bseagmn(ldx), bseagm(ldx), bseagmx(ldx),   &
     &          bslagn(ldx), bslmin(ldx), bslagm(ldx), bslmax(ldx),     &
     &          bslagx(ldx), bs0ags(ldx), bsdagd(ldx),                  &
     &          (bseags(ldx)-bseagmn(ldx))/(bseagmx(ldx)-bseagmn(ldx)), &
     &          (bslagm(ldx) - bslmin(ldx))/(bslmax(ldx) - bslmin(ldx)),&
     &          ibits(trigger(ldx),0,1), ibits(trigger(ldx),1,1),       &
     &          ibits(trigger(ldx),2,1), ibits(trigger(ldx),3,1),       &
     &          ibits(trigger(ldx),4,1), ibits(trigger(ldx),5,1),       &
     &          ibits(trigger(ldx),6,1), ibits(trigger(ldx),7,1)
         end do
      endif

      return
      end
