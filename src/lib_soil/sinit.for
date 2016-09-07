!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine sinit (daysim,                                         &
     &                 bhtsmx, bhrwc, bsfom, bszlyt,                    &
     &                 bslay, bsfsan, bsfsil, bsfcla,                   &
     &                 bszrgh, bszrr, bsfcce, bsfcec,                   &
     &                 cump, dcump, bsk4d,                              &
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

!     + + + GLOBAL COMMON BLOCKS + + +

      include 'p1werm.inc'
      include 'wpath.inc'
      include 'm1subr.inc'
      include 'm1sim.inc'
      include 'm1flag.inc'

!     + + + ARGUMENT DECLARATIONS + + +
      integer daysim
      real bhtsmx(mnsz), bhrwc(mnsz), bsfom(1:mnsz), bszlyt(mnsz)
      integer bslay
      real bsfsan(1:mnsz), bsfsil(1:mnsz), bsfcla(1:mnsz)
      real bszrgh, bszrr, bsfcce(1:mnsz), bsfcec(1:mnsz)
      real cump, dcump, bsk4d(mnsz)
      real bhtmx0(mnsz), bhrwc0(mnsz), szlyd(mnsz)
      real bszrr0, bszrh0
      real bseagm(mnsz), bseagmn(mnsz), bseagmx(mnsz)
      real bslmin(mnsz),bslmax(mnsz)
      real rain, snow, sprink
      real bhzirr, bszrho
      real bhlocirr, bhzsmt, bszrro
      real bsdsblk(mnsz), bwzdpt, bwtdav
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
!   bsk4d     - drying process coef. to calc. aggregate stability
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
      real sf84m(mnsz), sf84sd(mnsz), scecr
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

!           set stability drying process coefficient:
            if( ldx .eq. 1) then
              bsk4d(ldx) = 0.46 - 0.23 * exp(-(szlyd(ldx)/2.0)/88.57)
            else
              bsk4d(ldx) = 0.46 - 0.23 * exp(-(szlyd(ldx-1) +           &
     &               (szlyd(ldx) - szlyd(ldx-1))/2.0)/88.57)
            end if

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
      end
