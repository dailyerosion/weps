!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine hydro ( isr, layrsn, bmrslp, bbzht,                    &
     &                   bcrlai, bcrsai, bczht, bcdayap,                &
     &                   bcxrow, bc0rg, bbfcancov, bcfliveleaf,         &
     &                   bdmres, bbevapredu, bczrtd, bhfwsf,            &
     &                   bszlyd, bsdblk, bsdblk0, bsdpart, bsdwblk,     &
     &                   bhrwc, bhrwcdmx, bhrwcs, bhrwcf,               &
     &                   bhrwcw, bhrwcr, bhrwca,                        &
     &                   bh0cb, bheaep, bhfredsat,                      &
     &                   bsfsan, bsfsil, bsfcla,                        &
     &                   bsvroc, bsfom, bsfcec,                         &
     &                   bhtsav, bbdstm, bbffcv,                        &
     &                   bsxrgs, bszrgh, bsfcr,                         &
     &                   bslrro, bslrr, bmzele,                         &
     &                   bhzdmaxirr, bhratirr, bhdurirr,                &
     &                   bhlocirr, bhminirr, bm0monirr,                 &
     &                   bhmadirr, bhndayirr, bhmintirr,                &
     &                   bhzoutflow, bhzinf,                    &
     &                   bhzsno, bhtsno, bhfsnfrz,                      &
     &                   bhzsmt, bhfice, bhrsk,                         &
     &                   bhtsmx, bhtsmn, bhrwc0,                        &
     &                   daysim, bsfald, bsfalw, bszlyt,                &
     &                   bwudav, bhzwid, &
     &                   bhzeasurf,                                     &
     &                   cumprecip, cumirrig, &
     &                   cumrunoff, cumevap, &
     &                   cumtrans, cumdrain,                            &
     &                   presswc, pressnow, presday,                    &
     &                   bhztranspdepth, restot, h1et, wp)

!     + + + PURPOSE + + +
!     This subroutine is the main (supervisory) program for the
!     HYDROLOGY submodel.  The subroutine controls the calling of the
!     major subprograms of the HYDROLOGY submodel.

!     + + + KEY WORDS + + +
!     hydrology

      use weps_interface_defs, only: hinit, heat, store, et
      use weps_interface_defs, only: darcy, transp
      use weps_interface_defs, only: dawn, daylen, radnet, availwc
      use weps_interface_defs, only: plant_wat_t, movewind, volwatadsorb
      use datetime_mod, only: get_simdate, get_simdate_doy
      use file_io_mod, only: luohydro, luohlayers, luosurfwat, luoweather
      use biomaterial, only: biototal
      use p1unconv_mod, only: mtomm
      use timer_mod, only: timer, TIMHYDR, TIMDARC, TIMSTART, TIMSTOP
      use erosion_data_struct_defs, only: anemht, awzzo, awzdisp, wzoflg
      use grid_mod, only: amxsim
      use Points_Mod, only: slen
      use wind_mod, only: sbzdisp, sbzo, biodrag
      use hydro_data_struct_defs, only: am0hfl, hydro_derived_et
      use wepp_param_mod, only: wepp_param
      use climate_input_mod, only: cli_tyav, cli_next, cli_today, cli_prev

!     + + + ARGUMENT DECLARATIONS + + +
      integer, intent(in) :: isr   ! subregion number
      integer layrsn
      real bmrslp
      real bbzht
      real bcrlai, bcrsai, bczht
      integer bcdayap
      real bcxrow
      integer bc0rg
      real bbfcancov, bcfliveleaf
      real bdmres, bbevapredu, bczrtd, bhfwsf
      real bszlyd(*), bsdblk(*), bsdblk0(*), bsdpart(*), bsdwblk(*)
      real bhrwc(*), bhrwcdmx(*), bhrwcs(*), bhrwcf(*)
      real bhrwcw(*), bhrwcr(*), bhrwca(*)
      real bh0cb(*), bheaep(*), bhfredsat(*)
      real bsfsan(*), bsfsil(*), bsfcla(*)
      real bsvroc(*), bsfom(*), bsfcec(*)
      real bhtsav(*), bbdstm, bbffcv
      real bsxrgs, bszrgh, bsfcr
      real bslrro, bslrr, bmzele
      real bhzdmaxirr, bhratirr, bhdurirr
      real bhlocirr, bhminirr
      integer bm0monirr
      real bhmadirr
      integer bhndayirr, bhmintirr
      real bhzoutflow, bhzinf
      real bhzsno, bhtsno, bhfsnfrz
      real bhzsmt, bhfice(*), bhrsk(*)
      real bhtsmx(*), bhtsmn(*), bhrwc0(*)
      integer daysim
      real bsfald, bsfalw, bszlyt(*)
      real bwudav, bhzwid
      real bhzeasurf
      real cumprecip, cumirrig
      real cumrunoff, cumevap
      real cumtrans, cumdrain
      real presswc, pressnow, presday
      real bhztranspdepth
      type(biototal), intent(in) :: restot
      type(hydro_derived_et), intent(inout) :: h1et
      type(wepp_param), intent(inout) :: wp

!     + + +  ARGUMENT DEFINITIONS + + +
!     layrsn   - Number of soil layers used in simulation
!     bmrslp   - Average slope of subregion (mm/mm)
!     bbzht  - composite average residue height (m)
!     bcrlai - crop leaf area index (m2/m2)
!     bcrsai - crop stem area index (m2/m2)
!     bczht  - crop height (m)
!     bcdayap - number of days of growth completed since crop planted
!     bcxrow - crop row spacing (m)
!     bc0rg  - flag=0 - crop planted in furrow bottom
!              flag=1 - crop planted on ridge top
!     bbfcancov - total biomass canopy cover (decimal)
!     bcfliveleaf - fraction of standing plant leaf which is living (transpiring)
!     bdmres   - Plant residues on the soil surface (kg/m^2)
!     bbevapredu - reduction in surface evaporation from flat residue (Eactual/Epotential ratio)
!     bczrtd   - Plant root depth (m)
!     bhfwsf   - Plant growth water stress factor (unitless)
!     bszlyd   - Depth to bottom of soil layers (mm)
!     bsdblk   - Soil bulk density (Mg/m^3)
!     bsdpart  - Soil mean particle density (Mg/m^3)
!     bsdwblk  - Soil wet bulk density (Mg/m^3)
!     bhrwc    - Soil water content (mg/mg)
!     bhrwcdmx - daily maximum soil water content (Kg/Kg)
!     bhrwcs   - Soil water content at saturation (mg/mg)
!     bhrwcf   - Soil water content at fc and wp (mg/mg)
!     bhrwcw   - Soil water content at wp (mg/mg)
!     bhrwcr   - Residual Soil water content (mg/mg)
!     bh0cb    - Power of campbell's water release curve model (?)
!     bheaep   - Soil air entry potential (j/kg)
!     bhfredsat - fraction of soil porosity that will be filled with water
!                 while wetting under normal field conditions due to entrapped air
!     bsfsan   - Fraction of soil mineral which is sand
!     bsfsil   - Fraction of soil mineral which is silt
!     bsfcla   - Fraction of soil mineral which is clay
!     bsvroc   - Soil layer coarse fragments, rock (m^3/m^3)
!     bsfom    - Fraction of total soil mass which is organic
!     bsfcec   - Soil layer cation exchange capacity (cmol/kg) (meq/100g)
!     bhtsav   - daily average soil temperature for each layer (C)
!     bbdstm   - total number of stems (#/m^2)
!     bbffcv   - 
!     bsxrgs  - ridge spacing (mm)
!     bszrgh  - ridge height (mm)
!     bsfcr   - fraction of the surface that is crusted
!     bslrro   - original random roughness height, after tillage, mm
!     bslrr    - Allmaras random roughness parameter (mm)
!     bmzele   - Average site elevation (m)
!     bhzdmaxirr - characteristic maximum irrigation system application depth (mm)
!     bhratirr - characteristic irrigation system application rate (mm/hour)
!     bhdurirr - duration of irrigation water application (hours) 
!                corresponding to the characteristic maximum irrigation
!                system application depth. This is used to set the rate (depth / duration)
!     bhlocirr - emitter location point (mm)
!                positive is above the soil surface
!                negative is below the soil surface
!     bhminirr - minimum irrigation application amount (mm)
!     bm0monirr - flag setting monitoring for irrigation need
!                 0 - do not monitor irrigation need
!                 1 - monitor irrigation need
!     bhmadirr - management allowed depletion used in monitoring irrigation
!                0.0 sets up replacing yesterdays water loss today
!                1.0 schedules next application at wilting point
!     bhndayirr - the next simulation day on which an irrigation can occur (day)
!     bhmintirr - minimum interval for irrigation application (days)
!     bhzoutflow - height of runoff outlet above field surface (m)
!     bhzsno   - depth of water in snow layer (mm)
!     bhtsno   - temperature of snow layer (C)
!     bhfsnfrz   - fraction of snow layer water content which is frozen
!     bhzsnd   - depth of snow (mm)
!     bhzsmt   - Snow melt (mm)
!     bhfice   - fraction of soil water in layer which is frozen
!     bhrsk    - Saturated soil hydraulic conductivity (m/s)
!     bhtsmx   - Maximum soil temperature for each layer
!     bhtsmn   - Minimum soil temperature for each layer
!     bhrwc0   - Hourly water content of the soil surface - darcy supplies
!     daysim   - day of the simulation
!     bsfald   - dry soil albedo
!     bsfalw   - wet soil albedo
!     bszlyt   - Soil layer thickness (mm)
!     bwudav   - Daily average wind speed at 10 meters (m/s)
!     bhzwid   - Water infiltration depth (mm)
!     bhzeasurf - accumulated surface evaporation since last complete rewetting (mm)
!     cumprecip - accumulation of rainfall (mm)
!     cumirrig  - accumulation of irrigation (mm)
!     cumrunoff - accumulation of runoff (mm) (mm)
!     cumevap   - accumulation of evaporation (mm)
!     cumtrans  - accumulation of transpiration (mm)
!     cumdrain  - accumulation of drainage (mm)
!     presswc   - present soil water content (mm)
!     pressnow  - present snow water content (mm)
!     presday   - present day
!     bhztranspdepth - depth in soil from which transpiration is extracted (m)
!                     when crop is furrow planted, this is deeper than root depth
!                     and is used in place of it when calling transp subroutine
!     restot - structure containing summary residue pool amounts

!     + + + COMMON BLOCKS + + +
      include 'p1werm.inc'
      include 'p1solar.inc'
      include 'm1sim.inc'
      include 'm1flag.inc'
      include 'h1db1.inc'
      include 'command.inc'

      include 'wepp_erosion.inc'
!     + + + LOCAL COMMON BLOCKS + + +
      include 'hydro/htheta.inc'
      include 'hydro/vapprop.inc'
      include 'hydro/clayomprop.inc'

!     + + + LOCAL VARIABLES + + +
      integer day, mo, yr
      integer l, idx
      integer idoy
!      integer theta_check
      real rise, daylength
      real rn, g_soil
      integer numeq
      real lswc, lsno
      real dprecip, dirrig
      real durprecip, tptprecip
      real epart, vlh, vaptrans, evaplimit
      real rad_surf
      real eff_lai
      real loc_zorr, loc_zordg, loc_zo, loc_zov, loc_zd, brcd, fld_wind
      real rootz_p_con, rootz_p_cap, paw
      real :: len_slope
!      real old_wind, old_etp

      real d1, d2
      parameter   (d1 = 2.5002773719, d2 = 0.0023644939)

      real met_height, loc_met_height
      parameter   (met_height = 2.0)

      real standevapredu, totalevapredu, accheck
      real temp, airentry(mnsz), lambda(mnsz), theta80rh(mnsz)
      real cropdp

!     + + + LOCAL DEFINITIONS + + +
!     idoy      - Day of year
!     l         - Loop index of soil layers
!     idx       - loop index
!     rise      - time of sunrise adjusted to local time (hours)
!     daylength - length of day (hours)
!     rn        - net radiation from function radnet (Mj/m^2/day)
!     g_soil    - soil heat flux (Mj/m^2/day)
!     numeq     - number of equations to be solved by darcy routines
!     lswc      - last days soil profile water content (mm)
!     lsno      - last days snow water content (mm)
!     dprecip   - cumulative daily amount of water from precipitation that
!                 makes it through the snow filter
!     dirrig    - cumulative daily amount of water from surface irrigation
!                 that makes it through the snow filter
!     durprecip - duration of precipitation adjusted for snowmelt
!     tptprecip - time to peak intensity of precipitation adjusted for snowmelt
!     epart     - evaporation partitioning to plant (transpiration)
!     vlh       - Latent heat of vaporization (mj/kg)
!     vaptrans  - vapor transmissivity (mm/d^.5)
!     evaplimit - accumulated surface evaporation since last complete rewetting
!                 defining limit of stage 1 (energy limited) and start of 
!                 stage 2 (soil vapor transmissivity limited) evaporation (mm)
!     rad_surf  - radiation partitioned between surface and canopy
!     eff_lai   - transpiring fraction of leaf area index
!     loc_zorr  - aerodynamic roughness of random roughness (mm)
!     loc_zordg - aerodynamic roughness of ridges (mm)
!     loc_zo    - aerodynamic roughness of surface below canopy (mm)
!     loc_zov   - aerodynamic roughness length of canopy (mm)
!     loc_zd    - zero plane displacement (mm)
!     brcd      - biomass drag coefficient
!     fld_wind  - wind velocity at field location
!     d1,d2     - constants used to compute latent heat of vaporization
!     met_height - height of measurement of meteorological sensors (m)
!     loc_met_height - met_height adjusted to be representative over crop/residue
!     standevapredu  -  evaporation reduction factor attributed to standing mass
!     totalevapredu  -  combined (standing and flat) evaporation reduction factor
!     paw       - plant available water (fraction field cap - wilting point)
!     len_slope - slope length(m)
!     cropdp - depth into soil layering that a plant extracts water
!                  This is either bczrtd or bhztranspdepth depending on
!                  the command line flag transpiration_depth

!     + + + SUBROUTINES CALLED + + +
!     hinit
!     heat
!     snomlt
!     store
!     et
!     darcy
!     transp
!     caldat

!     + + + FUNCTION DECLARATIONS + + +
!      real dawn
!      real daylen
!      real radnet
!      real availwc
!      real plant_wat_t
!      real movewind
!      real volwatadsorb

!     + + + OUTPUT FORMATS + + +
! 2020 format ('#',79('-')/'#soil  depth',t16,'initial',t25,'saturated', &
!     &  t36,'field',t45,'wilting',t54,'bh0cb',t60,'air',t69,'sat.',t76, &
!     &  'bulk'/'#layer',t17,'water',t27,'water',t35,                    &
!     &  'capacity',t46,'point  term  entry',t70,'k   density'/'# no',   &
!     &  t16,'content',t26,'content',t57,'potential'/'#',t9,             &
!     &  '(mm)',3x,15('-'),'m^3/m^3',14('-'),t57,'joules/kg',            &
!     &  t69,'m/s',3x,'mg/m^3',/,'#',79('-'))
! 2030 format ('#',i3,f8.3,f9.3,f10.3,2f9.3,2f7.2,e10.3,f6.2)
! 2040 format ('#',79('-'))
! 2050 format('#','initial soil wetness at the soil-air interface =',    &
!     &         f6.3,' m^3/m^3')
! 2060 format('#','initial total amount of soil water in the soil'       &
!     & ,' profile =',f7.2,' mm'/,'#')
! 2070 format (/'#',19x,5('*'),' daily soil water balance data ',        &
!     &       5('*'))
! *** 2080 format (1x,82('-')/1x,'sr    date',t17,'etp',t23,'ep',t29,'tp',t36
! ***     &      ,'ea',t43,'ta',t47,'h1et%zper',t54,'bhzirr',t61,'dprec',t67,
! ***     *      'h1et%zrun'
! ***     &      ,t74,'swc',t79,'bhfwsf',' check '/,t16,31('-'),'mm',30('-')/1x,
! ***     *      82('-'))
! 2080 format (1x,82('-')/
!     *  1x,'Sub-  Date',t17,'----',t24,'Pot',t31,'----',t38,
!     &      '--- Act ---',t52,'Deep',
!     *      t80,'Water',t87,'Water',t94,'Check',t101,'Root',t108,'Root',
!     *      t115,'Snow'/,
!     *  1x,'Region',t17,'Total', t24,'Evap', t31, 'Trans',t38, 'Evap',
!     *  t45,'Trans', t52, 'Perc', t59, 'Irrig', t66, 'Precip', t73,
!     *  'Runoff',t80, 'Soil',t87,'Stress',t101,'Depth',t108,'Water'
!     *  ,t115,'Water'/,t17,'*hzetp',t24,'*hzep',t31,'*hzptp',t38
!     &      ,'*hzea',t45,'*hzpta',t52,'*hzper',t59,'*hzirr',t66,'dprec',
!     *      t73,'*hzrun'
!     &      ,t80,'swc',t87,'*hfwsf',t101,'*czrtd',t115,'*snwc'
!     &      ,t122,'theta'/,
!     *  /, t16,34('-'),'mm',33('-')/1x, 100('-'))
 2080 format('# daysim doy yr  h1et%zetp  h1et%zep h1et%zptp  h1et%zea h1et%zpta h1et%zper&
     & h1et%zirr cli_today%zdpt  dprec h1et%zrun bhzinf   lswc   swc  h1et%zsnd bhzsno  c&
     &heck cropdp rootwc rootwcap bhfwsf surfdry tdav vaptrans evaplim&
     &it st_evapr fl_evapr to_evapr')
 2090 format(1x,i5,1x,i3,1x,i4,11(1x,f6.2),2(1x,f8.2),2(1x,f6.2),       &
     &       1x,f7.3,11(1x,f6.2))
 3000 format('# daysim doy yr layer depth theta thetas thetaf thetaw the&
     &tar availwat satrat bhtsav unsatcond matricpot relhum bulkden aire&
     &ntry expon_b k_sat numlay = ',i3)

 3010 format('# hr daysim idoy yr bhrwc0 bhrwc0/bhrwcw(1)')

 3020 format('# daysim idoy yr rn cli_today%tdmx cli_today%tdmn cli_today%tdpt fld_wind rise')

!     + + + DATA INITIALIZATIONS + + +
!     Calculate hour of sunrise
      call get_simdate(day, mo, yr)
      idoy = get_simdate_doy()
      rise = dawn(amalat, amalon, idoy, beamrise)
      daylength = daylen(amalat, idoy, beamrise)

      call hinit (layrsn, bsdblk, bsdblk0, bsdpart, bsdwblk,            &
     &            bhrwc, bhrwcs, bhrwcf, bhrwcw, bhrwcr,                &
     &            bhrwca, bh0cb, bheaep, bhrsk, bhfredsat,              &
     &            bsfsan, bsfsil, bsfcla, bsfom, bsfcec,                &
     &            bszlyd, bszlyt, vaptrans, evaplimit)

!     set accounting variables for water balance changes in this cycle
      swc = dot_product(theta(1:layrsn),bszlyt(1:layrsn))
      lswc = swc
      lsno = bhzsno

!     + + + END SPECIFICATIONS + + +
!     write headers and inital values to hydro.out
      if(  (am0ifl .eqv. .true.)                                        &
     &   .and.((am0hfl(isr) .eq. 1) .or. (am0hfl(isr) .eq. 3)           &
     &   .or.  (am0hfl(isr) .eq. 5) .or. (am0hfl(isr) .eq. 7))) then

!     Echo print of input soil data

!         write(luohydro(isr),2020)
!         do 130 l=1,layrsn
!            write(luohydro(isr),2030) l,bszlyd(l),theta(l),thetas(l),thetaf(l),
!     &                   thetaw(l),bh0cb(l),bheaep(l),bhrsk(l),bsdblk(l)
!  130    continue
!         write(luohydro(isr),2040)
!         write(luohydro(isr),2050) theta(0)
!         write(luohydro(isr),2060) swci
!         write(luohydro(isr),2070)
         write(luohydro(isr),2080)

         ! header for file for layer information
         write(luohlayers(isr), 3000) layrsn

         ! print out hydro values by layer (profile view)
         ! day zero values
!         call printlayval( isr, 0, layrsn,                              &
!     &        bszlyt, bszlyd, bsdblk,                                   &
!     &        theta, thetas, thetaf, thetaw, thetar,                    &
!     &        bhrsk, bheaep, bh0cb, bsfcla, bsfom, bhtsav )
      end if

      if( (am0ifl .eqv. .true.) .and.((am0hfl(isr) .eq. 2).or.(am0hfl(isr) .eq. 6) &
     &   .or. (am0hfl(isr) .eq. 3) .or. (am0hfl(isr) .eq. 7)) ) then

         write(luosurfwat(isr),3010)

         ! print out daily weather as used in hydro
         write(luoweather(isr),3020)
      end if

!          write(*,*) 'hydro:total 500mm',
!     &    plant_wat_t( 0.0, 500.0, thetaf, thetaw, bszlyd, layrsn ),
!     &    plant_wat_t( 500.0, 1000.0, thetaf, thetaw, bszlyd, layrsn ),
!     &    plant_wat_t( 1000.0, 1500.0, thetaf, thetaw, bszlyd, layrsn )

      ! check command line transpiration depth flag, set plant depth accordingly
      if( transpiration_depth .eq. 0 ) then
          cropdp = bczrtd
      else
          cropdp = bhztranspdepth
      end if

      ! check irrigation flag for irrigation monitoring option
      if( (bm0monirr .gt. 0) .and. (cropdp .gt. 0.0) ) then
          ! find root zone water content above wilting point
          rootz_p_con = plant_wat_t(0.0, cropdp*mtomm, theta(1), thetaw,&
     &                              bszlyd, layrsn)
          ! find root zone water capacity between field capacity and wilting point
          rootz_p_cap = plant_wat_t(0.0, cropdp*mtomm, thetaf, thetaw,  &
     &                              bszlyd, layrsn)
          ! find paw (plant available water) ratio in rootzone
          paw = rootz_p_con / rootz_p_cap

          ! find plant water stress level
          ! this is needed to overcome soils that generate maximum
          ! stress without reaching PAW limits
          ! this call finds stress value without extracting the water
          do l=1,layrsn
              airentry(l) = bheaep(l) / gravconst
              lambda(l) = 1.0 / bh0cb(l)
              temp = bsdblk(l)*1000.0  !convert Mg/m^3 to kg/m^3
              theta80rh(l) = volwatadsorb( temp,                        &
     &                        bsfcla(l), bsfom(l),                      &
     &                        claygrav80rh, orggrav80rh )
          end do
          ! note: as written, the potential Transpiration is from previous day
          call transp (layrsn, 0, bszlyd, bszlyt, cropdp*mtomm,         &
     &                 theta, thetas, thetaf, thetaw,                   &
     &                 theta80rh, thetar, airentry, lambda,             &
     &                 bhrsk, bhtsav, h1et%zptp, h1et%zpta, bhfwsf)

          ! check for irrigation depletion or stress trigger
          ! present, both trigger at the same level
          if( ((paw.lt.(1.0-bhmadirr)) .or. (bhfwsf.lt.(1.0-bhmadirr))) &
     &        .and. (daysim .ge. bhndayirr) ) then
              ! add irrigation
              ! maximum of single day value and root zone deficit
              h1et%zirr = max( h1et%zirr, rootz_p_cap - rootz_p_con )
              ! limit value based on system characterisitic maximum and minimum depths
              h1et%zirr = min( bhzdmaxirr, max( h1et%zirr, bhminirr ) )
              ! set duration based on depth and rate
              call ratedura(h1et%zirr, bhratirr, bhdurirr)

          end if
      end if

      if( h1et%zirr .gt. 0.0 ) then
          ! irrigation applied, set next irrigation day
          bhndayirr = daysim + bhmintirr
      end if

      ! run daily precipitation and irrigation through the snow filter
      ! returned value reflects how much was left behind
      call addsnow(dprecip, dirrig, cli_today%zdpt, h1et%zirr, bhlocirr,           &
     &             cli_today%tdmn, cli_today%tdmx, cli_today%tdpt, bmzele,                      &
     &             bhzsno, bhtsno, bhfsnfrz, h1et%zsnd)

      ! Convert global to net radiation
      rn = radnet(bcrlai,cli_today%eirr, bhzsno, h1et%zsnd, cli_today%tdmx, cli_today%tdmn, amalat,&
     &                 bsfalw, bsfald, idoy, cli_today%tdpt )

      ! partition radiation between canopy and surface
      ! added exponential to keep above zero for very low lai
      epart = 1.0 - exp(-0.398*bcrlai)
      epart = max( epart, min( 1.0, (-0.21 + 0.7 * (bcrlai**0.5)) ) )
      ! check for snow
      if( bhzsno .gt. 0.0 ) then
          ! snow present, all net rad goes on the surface
          rad_surf = rn
      else
          ! no snow, so partition between canopy and soil surface
          rad_surf = rn * (1.0 - epart)
      end if

      ! recalculate partitioning to acount for non transpiring leaf surface
      eff_lai = bcfliveleaf * bcrlai
      epart = 1.0 - exp(-0.398*eff_lai)
      epart = max( epart, min( 1.0, (-0.21 + 0.7 * (eff_lai**0.5)) ) )

      ! Do energy balance for soil and cover temperatures 
      ! and determine snow melt (if any) or soil heat flux
      call heat( isr, layrsn, bszlyd, bszlyt, theta, thetas,            &
     &           bsfsan, bsfsil, bsfcla, bsfom, bsdblk,                 &
     &           cli_today%tdmn, cli_today%tdmx, cli_tyav, rad_surf, bdmres,              &
     &           bhtsmn, bhtsmx, bhtsav, bhfice,                        &
     &           bhzsno, bhtsno, bhfsnfrz, h1et%zsnd,                      &
     &           bhzsmt, g_soil )

      ! add snowmelt to precipitation water for infiltration
      if (bhzsmt .gt. 0.0) then
          dprecip = dprecip + bhzsmt
          durprecip = max(cli_today%durpt,6.0)!we have no entry for snowmelt duration yet
          tptprecip = 0.5
      else
          durprecip = cli_today%durpt
          tptprecip = cli_today%peaktpt
      endif

      ! replenish accumulated surface evaporation reservoir with applied suface water
      bhzeasurf = max(0.0, bhzeasurf - dprecip - dirrig)

!     check that no soil water contents are greater than saturation
!     this is included until the adding of water and bulk density
!     reduction can be done simultaneously
! *** temporarily disabled
! ***      theta_check = 0
! ***      do l=1,layrsn
! ***         if( theta(l).gt.thetas(l) ) theta_check = 1
! ***      end do

!     Distribute the total amount of water available for
!     infiltration throughout the simulation layers of the soil
!     profile using subroutine store.
! ***      bhzwid = 0.0
! ***      if( theta_check.eq.1 ) then
!         if( theta_check.eq.1 ) write(*,*) 'theta_check'
! ***         call store(layrsn, bheaep, bh0cb, bszlyt, bhzwid)
! ***      endif

      ! calculate dryness ratio
      if (cli_today%zdpt .eq. 0.0) then
         h1et%drat = 10.0
       else
         vlh = d1 - (d2*cli_today%tdav)                               !h-24
         h1et%drat = rn / (vlh * cli_today%zdpt)
      end if

      ! biodrag used for roughness length and below for surface evaporation reduction
      brcd = biodrag( restot%rlaitot, restot%rsaitot, bcrlai, bcrsai,   &
     &                bc0rg, bcxrow, bczht, bszrgh )

      ! find roughness length of the surface for et wind speed adjustment to 2m
      call sbzo (bsxrgs, bszrgh, bslrr, bbzht, brcd, wzoflg,            &
     &           loc_zordg, loc_zorr, loc_zo, loc_zov, awzzo)

      ! find zero plane displacement of location
      call sbzdisp (wzoflg, brcd, bbzht, bczht, awzdisp, loc_zd)

      ! set location adjusted meteorological height (mm)
      ! see RZWQM manual pages 69-70
      loc_met_height = (met_height*mtomm) + loc_zd - awzdisp

      ! adjust wind velocity to adjusted agrometeorology height
      fld_wind = movewind(bwudav, anemht*mtomm, awzzo, awzdisp,         &
     &                    loc_met_height, loc_zov, loc_zd)

!     Calculate potential evapotranspiration using subroutine et
      g_soil = 0.0 ! test
      call et(rn, g_soil, fld_wind, bmzele, cli_today%tdmx, cli_today%tdmn, cli_today%tdav,     &
     &        cli_today%tdpt, h1et%zetp, loc_met_height, loc_zov, loc_zd)

      if (  h1et%zetp  .le.  0.0  ) then
         h1et%zep = 0.0
         h1et%zptp = 0.0
         h1et%zetp = 0.0
         h1et%zea = 0.0
         standevapredu = 1.0
         totalevapredu = 1.0
      else
         ! partition ET between potential plant transpiration and potential surface
         ! evaporation based on canopy partioning above as a function of leaf area
         ! index.
        
         h1et%zptp = h1et%zetp * epart
         h1et%zep = h1et%zetp - h1et%zptp

         ! If snow is present, it completely supresses any moisture loss (energy
         ! balance on snow determined snowmelt rates, no sublimation or evap assumed)
         if ( bhzsno .gt. 0.0 )  then
             h1et%zep = 0.0
         end if
         ! this is zeroed here as a starting point for darcy which uses potential
         ! and finds actual. Previously set to snow evap amount and added to soil
         ! surface evaporation in darcy
         h1et%zea = 0.0

!        If plant residue is present, then reduce the potential soil
!        evaporation on the basis of the amount of plant residues on
!        the soil surface.
         ! standing residue
         ! estimated from McMaster et al. 2000. Optimizing wheat harvest cutting
         ! height for harvest efficiency and soil and water conservation. Agronomy J. 92:1104-1108
         standevapredu = exp(-1.7*brcd**0.4)
         ! flat residue
         ! taken from Steiner, 1989. Tillage and Surface Residue Effects on Evaporation from soils.
         ! Soil Sci Soc Am J. 53:911-916. Data was refit to an exponential 
         ! power relationship in subroutine resevapredu.
         totalevapredu = standevapredu * bbevapredu
         h1et%zep = h1et%zep * totalevapredu

      endif

!     Calculate soil water redistribution using subroutine darcy

      call timer(TIMHYDR,TIMSTOP)
      call timer(TIMDARC,TIMSTART)

      swc = dot_product(theta(1:layrsn),bszlyt(1:layrsn))

      ! select hydrology model for infiltration (insertion), evaporation and redistribution
      if( wepp_hydro .eq. 0 ) then
         ! use darcy method
         numeq = layrsn + 6

         call darcy( isr, daysim, numeq, bszlyt,bszlyd,bsdblk,          &
     &       theta, thetadmx, thetas, thetaf, thetaw, thetar,           &
     &       bhrsk, bheaep, bh0cb, bsfcla, bsfom, bhtsav,               &
     &       cli_prev%tdmx, cli_today%tdmn, cli_today%tdmx, cli_next%tdmn, cli_today%tdpt, &
     &       rise, daylength, h1et%zep, dprecip, durprecip, tptprecip,       &
     &       dirrig, bhdurirr, bhlocirr, bhzoutflow,                    &
     &       bbdstm, bbffcv, bslrro, bslrr, bmzele, bhrwc0,             &
     &       h1et%zea, h1et%zper, h1et%zrun, bhzinf, bhzwid,                     &
     &       bhzeasurf, evaplimit, vaptrans, bmrslp )

      else
         ! use WEPP infiltration, evaporation, redistribution
         ! passing in reduced saturation instead of full saturation

         ! use a representative slope length as half of the simregion diagonal distance
         len_slope = slen(amxsim(1), amxsim(2)) / 2.0

!      if( isr .eq. 1 .and. daysim .eq. 7979 ) then
!        write(*,*) isr, daysim
!        write(*,*) dprecip, durprecip, tptprecip, cli_today%peakipt,        &
!     &                   dirrig, bhdurirr, bhlocirr, bhzoutflow,        &
!     &                   bhzsno, bslrr, bmrslp, bsfcr
!        write(*,*) 'bsfsan', (bsfsan(l), l=1,layrsn)
!        write(*,*) 'bsfcla', (bsfcla(l), l=1,layrsn)
!        write(*,*) 'bsvroc', (bsvroc(l), l=1,layrsn)
!        write(*,*) 'bsdblk', (bsdblk(l), l=1,layrsn)
!        write(*,*) 'bsfcec', (bsfcec(l), l=1,layrsn)
!        write(*,*) bbffcv, bbfcancov, bbzht, bcdayap, h1et%zep
!        write(*,*) 'theta', (theta(l), l=1,layrsn)
!        write(*,*) 'thetadmx', (thetadmx(l), l=1,layrsn)
!        write(*,*) 'bhrwc0', (bhrwc0(l), l=1,layrsn)
!        write(*,*) h1et%zea, h1et%zper, h1et%zrun, bhzinf, bhzwid,         &
!     &                   len_slope, wepp_hydro, init_loop, calib_loop
!        write(*,*) 'bhfice', (bhfice(l), l=1,layrsn)
!        write(*,*) 'wp', wp%totalRunoff, wp%totalPrecip, wp%totalSnowrunoff, &
!                   wp%runoffEvents, wp%precipEvents, wp%snowmeltEvents, &
!                   wp%rkecum, wp%prev_crust_frac
!      end if

         call waterbal(layrsn, thetas, thetes, thetaf, thetaw,          &
     &                   bszlyt, bszlyd, bhrsk,                         &
     &                   dprecip, durprecip, tptprecip, cli_today%peakipt,        &
     &                   dirrig, bhdurirr, bhlocirr, bhzoutflow,        &
     &                   bhzsno, bslrr, bmrslp, bsfsan, bsfcla,         &
     &                   bsfcr, bsvroc, bsdblk, bsfcec,                 &
     &                   bbffcv, bbfcancov, bbzht, bcdayap,             &
     &                   h1et%zep, theta, thetadmx, bhrwc0,                &
     &                   h1et%zea, h1et%zper, h1et%zrun, bhzinf, bhzwid,         &
     &                   len_slope, day, mo, yr, isr,                   &
     &                   wepp_hydro, init_loop, calib_loop, bhfice, wp)

!      if( isr .eq. 1 .and. daysim .eq. 7979 ) then
!        write(*,*) isr, daysim
!        write(*,*) dprecip, durprecip, tptprecip, cli_today%peakipt,        &
!     &                   dirrig, bhdurirr, bhlocirr, bhzoutflow,        &
!     &                   bhzsno, bslrr, bmrslp, bsfcr
!        write(*,*) 'bsfsan', (bsfsan(l), l=1,layrsn)
!        write(*,*) 'bsfcla', (bsfcla(l), l=1,layrsn)
!        write(*,*) 'bsvroc', (bsvroc(l), l=1,layrsn)
!        write(*,*) 'bsdblk', (bsdblk(l), l=1,layrsn)
!        write(*,*) 'bsfcec', (bsfcec(l), l=1,layrsn)
!        write(*,*) bbffcv, bbfcancov, bbzht, bcdayap, h1et%zep
!        write(*,*) 'theta', (theta(l), l=1,layrsn)
!        write(*,*) 'thetadmx', (thetadmx(l), l=1,layrsn)
!        write(*,*) 'bhrwc0', (bhrwc0(l), l=1,layrsn)
!        write(*,*) h1et%zea, h1et%zper, h1et%zrun, bhzinf, bhzwid,         &
!     &                   len_slope, wepp_hydro, init_loop, calib_loop
!        write(*,*) 'bhfice', (bhfice(l), l=1,layrsn)
!        write(*,*) 'wp', wp%totalRunoff, wp%totalPrecip, wp%totalSnowrunoff, &
!                   wp%runoffEvents, wp%precipEvents, wp%snowmeltEvents, &
!                   wp%rkecum, wp%prev_crust_frac
!        stop
!      end if

      end if

      swc = dot_product(theta(1:layrsn),bszlyt(1:layrsn))

      call timer(TIMDARC,TIMSTOP)
      call timer(TIMHYDR,TIMSTART)

!     following darcy, check total et against reduced soil surface ET
!      h1et%zptp = min(h1et%zptp, h1et%zetp - h1et%zea)

!     Calculate actual plant transpiration using subroutine transp
!     and determine the water stress factor
      if ( h1et%zptp .gt. 0.0 )  then
          do l=1,layrsn
              airentry(l) = bheaep(l) / gravconst
              lambda(l) = 1.0 / bh0cb(l)
              temp = bsdblk(l)*1000.0  !convert Mg/m^3 to kg/m^3
              theta80rh(l) = volwatadsorb( temp,                        &
     &                        bsfcla(l), bsfom(l),                      &
     &                        claygrav80rh, orggrav80rh )
          end do

          call transp (layrsn, 1, bszlyd, bszlyt, cropdp*mtomm,         &
     &                   theta, thetas, thetaf, thetaw,                 &
     &                   theta80rh, thetar, airentry, lambda,           &
     &                   bhrsk, bhtsav, h1et%zptp, h1et%zpta, bhfwsf)

      else
          h1et%zpta = 0.0
          bhfwsf = 1.0
      end if
!     Calculate actual evapotranspiration
      h1et%zeta = h1et%zea + h1et%zpta

!     Convert water from volume back to mass basis
      do l=1,layrsn
         bhrwc(l) = theta(l) / bsdblk(l)
         bhrwcdmx(l) = thetadmx(l) / bsdblk(l)
      end do

      ! Adjust energy balance for changes in water content
!      call heat( layrsn, bszlyd, theta, bsfcla, bsdblk,                 &
!     &           cli_today%tdmn, cli_today%tdmx, bhtsmx, bhtsmn, bszlyt)

      ! update accumulated surface evaporation variable
      bhzeasurf = bhzeasurf + h1et%zea

!     update cumulative variables
      swc = dot_product(theta(1:layrsn),bszlyt(1:layrsn))
      cumprecip = cumprecip + cli_today%zdpt
      cumirrig = cumirrig + h1et%zirr
      cumrunoff = cumrunoff + h1et%zrun
      cumevap = cumevap + h1et%zea
      cumtrans = cumtrans + h1et%zpta
      cumdrain = cumdrain + h1et%zper
      presswc = swc
      pressnow = bhzsno
      presday = daysim
      
!     Added for WEPP bookeeping      
      wp%totalPrecip = wp%totalPrecip + cli_today%zdpt
      wp%totalRunoff = wp%totalRunoff + h1et%zrun
      
      if (cli_today%zdpt.gt.0) then
         wp%precipEvents = wp%precipEvents + 1
      endif
      
      if (h1et%zrun.gt.0) then
         wp%runoffEvents = wp%runoffEvents + 1
         if (bhzsmt .gt. 0.0) then    ! due to snowmelt
            wp%snowmeltEvents = wp%snowmeltEvents + 1
            wp%totalSnowrunoff = wp%totalSnowrunoff + h1et%zrun
         endif
      endif
!     End WEPP addition      

!     Print the daily soil water balance results to hydro.out
      if(    (am0hfl(isr) .eq. 1) .or. (am0hfl(isr) .eq. 3)             &
     &   .or.(am0hfl(isr) .eq. 5) .or. (am0hfl(isr) .eq. 7)) then
         ! insert double blank line to break years into blocks for graphing
         if( idoy .eq. 1 ) then
             write(luohydro(isr),*)
             write(luohydro(isr),*)
         end if
         accheck = lswc - swc + lsno - bhzsno + h1et%zirr + cli_today%zdpt         &
     &           - h1et%zea - h1et%zpta - h1et%zper - h1et%zrun
! 2090 format(1x,i5,1x,i3,1x,i4,11(1x,f6.2),2(1x,f8.2),2(1x,f6.2),1x,f7.3,11(1x,f6.2))
         write(luohydro(isr),2090) daysim,idoy,yr,h1et%zetp, h1et%zep, h1et%zptp,&
     &       h1et%zea, h1et%zpta, h1et%zper, h1et%zirr, cli_today%zdpt, dprecip, h1et%zrun,    &
     &       bhzinf, lswc, swc, h1et%zsnd, bhzsno, accheck, cropdp,        &
     &      plant_wat_t(0.0,cropdp*mtomm,theta(1),thetaw,bszlyd,layrsn),&
     &       plant_wat_t(0.0,cropdp*mtomm,thetaf,thetaw,bszlyd,layrsn), &
     &       bhfwsf, bhrwc0(12)/bhrwcw(1), cli_today%tdav, vaptrans, evaplimit, &
     &       standevapredu, bbevapredu, totalevapredu

         ! print out hydro values by layer (profile view)
         call printlayval( isr, daysim, layrsn,                         &
     &        bszlyt, bszlyd, bsdblk,                                   &
     &        theta, thetas, thetaf, thetaw, thetar,                    &
     &        bhrsk, bheaep, bh0cb, bsfcla, bsfom, bhtsav )
      end if

      if ((am0hfl(isr) .eq. 2).or.(am0hfl(isr) .eq. 6) &
     &   .or. (am0hfl(isr) .eq. 3) .or. (am0hfl(isr) .eq. 7)) then
         ! print out hourly surface water content values
         do idx = 1, 24
            write(luosurfwat(isr),*) idx, daysim, idoy, yr, bhrwc0(idx),     &
     &                          bhrwc0(idx)/bhrwcw(1)
         end do

         ! print out daily weather as used in hydro
         write(luoweather(isr),*) daysim, idoy, yr, rn, cli_today%tdmx, cli_today%tdmn,      &
     &                       cli_today%tdpt, fld_wind, rise
      end if

      return
      end
