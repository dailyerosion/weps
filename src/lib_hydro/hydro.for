!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine hydro ( layrsn, bmrslp, bbzht,                         &
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
     &                   bhzper,                                        &
     &                   bhzirr, bhzdmaxirr, bhratirr, bhdurirr,        &
     &                   bhlocirr, bhminirr, bm0monirr,                 &
     &                   bhmadirr, bhndayirr, bhmintirr,                &
     &                   bhzoutflow, bhzrun, bhzinf,                    &
     &                   bhzsno, bhtsno, bhfsnfrz, bhzsnd,              &
     &                   bhzsmt, bhfice, bhrsk,                         &
     &                   bhtsmx, bhtsmn, bhrwc0,                        &
     &                   daysim, bsfald, bsfalw, bszlyt,                &
     &                   bwzdpt, bwdurpt, bwpeaktpt, bwpeakipt,         &
     &                   bwtdmxprev, bwtdmn, bwtdmx, bwtdmnnext,        &
     &                   bwtdav, bwtyav, bwrrh,                         &
     &                   bwtdpt, bweirr, bwudav, bhzwid,                &
     &                   bhzeasurf,                                     &
     &                   cumprecip, cumrunoff, cumevap,                 &
     &                   cumtrans, cumdrain,                            &
     &                   presswc, pressnow, presday,                    &
     &                   bhztranspdepth, restot )

!     + + + PURPOSE + + +
!     This subroutine is the main (supervisory) program for the
!     HYDROLOGY submodel.  The subroutine controls the calling of the
!     major subprograms of the HYDROLOGY submodel.

!     + + + KEY WORDS + + +
!     hydrology

      use weps_interface_defs
      use file_io_mod, only: luohydro, luohlayers, luowepphdrive
      use biomaterial, only: biototal
      use p1unconv_mod, only: mtomm

!     + + + ARGUMENT DECLARATIONS + + +
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
      real bhzper
      real bhzirr, bhzdmaxirr, bhratirr, bhdurirr
      real bhlocirr, bhminirr
      integer bm0monirr
      real bhmadirr
      integer bhndayirr, bhmintirr
      real bhzoutflow, bhzrun, bhzinf
      real bhzsno, bhtsno, bhfsnfrz, bhzsnd
      real bhzsmt, bhfice(*), bhrsk(*)
      real bhtsmx(*), bhtsmn(*), bhrwc0(*)
      integer daysim
      real bsfald, bsfalw, bszlyt(*)
      real bwzdpt, bwdurpt, bwpeaktpt, bwpeakipt
      real bwtdmxprev, bwtdmx, bwtdmn, bwtdmnnext
      real bwtdav, bwtyav, bwrrh
      real bwtdpt, bweirr, bwudav, bhzwid
      real bhzeasurf
      real cumprecip, cumrunoff, cumevap
      real cumtrans, cumdrain
      real presswc, pressnow, presday
      real bhztranspdepth
      type(biototal), intent(in) :: restot

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
!     bhzper   - Daily deep percolation (mm/day)
!     bhzirr   - Daily irrigation (mm)
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
!     bhzrun   - Daily surface runoff (mm/day)
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
!     bwzdpt   - Daily precipitation (mm)
!     bwdurpt  - Duration of Daily precipitation (hours)
!     bwpeaktpt - Normalized time to peak of Daily precipitation (time to peak/duration)
!     bwpeakipt - Normalized intensity of peak Daily precipitation (peak intensity/average intensity)
!     bwtdmxprev - previous day maximum daily air temperature (deg C)
!     bwtdmn   - Minimum daily air temperature (deg C)
!     bwtdmx   - Maximum daily air temperature (deg C)
!     bwtdmnnext - Following day minimum daily air temperature (deg C)
!     bwtdav   - Average daily air temperature (deg C)
!     bwtyav   - Average yearly air temperature (deg C)
!     bwrrh    - Average daily relative humidity ratio
!     bwtdpt   - Daily dewpoint temperature (C)
!     bweirr   - Daily global total solar radiation (MJ/m^2)
!     bwudav   - Daily average wind speed at 10 meters (m/s)
!     bhzwid   - Water infiltration depth (mm)
!     bhzeasurf - accumulated surface evaporation since last complete rewetting (mm)
!     cumprecip - accumulation of rainfall (mm)
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
      include 'p1const.inc'
      include 'p1werm.inc'
      include 'p1solar.inc'
      include 'm1subr.inc'
      include 'm1sim.inc'
      include 'm1flag.inc'
      include 'h1et.inc'
      include 'h1db1.inc'
      include 'timer.inc'
      include 'command.inc'

      include 'm1geo.inc'
      include 'wepp_erosion.inc'
!     + + + LOCAL COMMON BLOCKS + + +
      include 'hydro/htheta.inc'
      include 'hydro/vapprop.inc'
      include 'hydro/clayomprop.inc'

!     + + + LOCAL VARIABLES + + +
      integer day, mo, yr
      integer l
      integer idoy
!      integer theta_check
      real rise, daylength
      real rn, g_soil
      integer numeq
      real lswc, lsno
      real dprecip, dirrig
      real epart, vlh, vaptrans, evaplimit
      real rad_surf
      real eff_lai
      real loc_zorr, loc_zordg, loc_zo, loc_zov, loc_zd, brcd, fld_wind
      real rootz_p_con, rootz_p_cap, paw, slen
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
!     slen      - slope length(m)
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
!      integer dayear
!      real dawn
!      real daylen
!      real radnet
!      real availwc
!      real plant_wat_t
!      real movewind
!      real volwatadsorb

!     + + + OUTPUT FORMATS + + +
 2000 format('#    ')
! 2009 format('# Daily HYDROLOGY output ')
! 2010 format (/,'#',18x,5('*'),'   soil   data - Subregion #',i4,3x,    &
!     &        5('*'),16x)
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
! ***     &      ,'ea',t43,'ta',t47,'bhzper',t54,'bhzirr',t61,'dprec',t67,
! ***     *      'bhzrun'
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
 2080 format('# daysim doy yr  ahzetp  ahzep ahzptp  ahzea ahzpta bhzper&
     & bhzirr bwzdpt  dprec bhzrun bhzinf   lswc   swc  bhzsnd bhzsno  c&
     &heck cropdp rootwc rootwcap bhfwsf surfdry bwtdav vaptrans evaplim&
     &it st_evapr fl_evapr to_evapr')
 2090 format(1x,i5,1x,i3,1x,i4,11(1x,f6.2),2(1x,f8.2),2(1x,f6.2),       &
     &       1x,f7.3,11(1x,f6.2))
 3000 format('# daysim doy yr layer depth theta thetas thetaf thetaw the&
     &tar availwat satrat bhtsav unsatcond matricpot relhum bulkden aire&
     &ntry expon_b k_sat numlay = ',i3)

!     + + + DATA INITIALIZATIONS + + +
!     Calculate hour of sunrise
      call caldatw(day, mo, yr)
      idoy = dayear(day, mo, yr)
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
      if ((am0ifl .eqv. .true.) .and.((am0hfl .eq. 1).or.(am0hfl .eq. 3)&
     &   .or. (am0hfl .eq. 5) .or. (am0hfl .eq. 7))) then

!     Echo print of input soil data

!         write(luohydro,2009)
!         write(luohydro,2010) am0csr
!         write(luohydro,2020)
!         do 130 l=1,layrsn
!            write(luohydro,2030) l,bszlyd(l),theta(l),thetas(l),thetaf(l),
!     &                   thetaw(l),bh0cb(l),bheaep(l),bhrsk(l),bsdblk(l)
!  130    continue
!         write(luohydro,2040)
!         write(luohydro,2050) theta(0)
!         write(luohydro,2060) swci
!         write(luohydro,2070)
         write(luohydro,2080)

         ! header for file for layer information
         write(luohlayers, 3000) layrsn

         ! print out hydro values by layer (profile view)
         ! day zero values
!         call printlayval( 0, layrsn,                                   &
!     &        bszlyt, bszlyd, bsdblk,                                   &
!     &        theta, thetas, thetaf, thetaw, thetar,                    &
!     &        bhrsk, bheaep, bh0cb, bsfcla, bsfom, bhtsav )

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
          call transp (layrsn, 0, bszlyd, bszlyt, cropdp*mtomm,         &
     &                 theta, thetas, thetaf, thetaw,                   &
     &                 theta80rh, thetar, airentry, lambda,             &
     &                 bhrsk, bhtsav, ahzptp, ahzpta, bhfwsf)

          ! check for irrigation depletion or stress trigger
          ! present, both trigger at the same level
          if( ((paw.lt.(1.0-bhmadirr)) .or. (bhfwsf.lt.(1.0-bhmadirr))) &
     &        .and. (daysim .ge. bhndayirr) ) then
              ! add irrigation
              ! maximum of single day value and root zone deficit
              bhzirr = max( bhzirr, rootz_p_cap - rootz_p_con )
              ! limit value based on system characterisitic maximum and minimum depths
              bhzirr = min( bhzdmaxirr, max( bhzirr, bhminirr ) )
              ! set duration based on depth and rate
              call ratedura(bhzirr, bhratirr, bhdurirr)

          end if
      end if

      if( bhzirr .gt. 0.0 ) then
          ! irrigation applied, set next irrigation day
          bhndayirr = daysim + bhmintirr
      end if

      ! run daily precipitation and irrigation through the snow filter
      ! returned value reflects how much was left behind
      call addsnow(dprecip, dirrig, bwzdpt, bhzirr, bhlocirr,           &
     &             bwtdmn, bwtdmx, bwtdpt, bmzele,                      &
     &             bhzsno, bhtsno, bhfsnfrz, bhzsnd)

      ! Convert global to net radiation
      rn = radnet(bcrlai,bweirr, bhzsno, bhzsnd, bwtdmx, bwtdmn, amalat,&
     &                 bsfalw, bsfald, idoy, bwtdpt, bwzdpt )

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
      call heat( layrsn, bszlyd, bszlyt, theta, thetas,                 &
     &           bsfsan, bsfsil, bsfcla, bsfom, bsdblk,                 &
     &           bwtdmn, bwtdmx, bwtyav, rad_surf, bdmres,              &
     &           bhtsmn, bhtsmx, bhtsav, bhfice,                        &
     &           bhzsno, bhtsno, bhfsnfrz, bhzsnd,                      &
     &           bhzsmt, g_soil )

      ! add snowmelt to precipitation water for infiltration
      if (bhzsmt .gt. 0.0) then
          dprecip = dprecip + bhzsmt
          bwdurpt = max(bwdurpt,6.0)!we have no entry for snowmelt duration yet
          bwpeaktpt = 0.5
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
      if (bwzdpt .eq. 0.0) then
         ah0drat = 10.0
       else
         vlh = d1 - (d2*bwtdav)                               !h-24
         ah0drat = rn / (vlh * bwzdpt)
      end if

      ! find roughness length of the surface for et wind speed adjustment to 2m
      ! biodrag returned and used below for surface evaporation reduction
      call sbzo (bsxrgs, bszrgh, bslrr, wzoflg,                         &
     &           restot%rlaitot, restot%rsaitot, bbzht,                 &
     &           bcrlai, bcrsai, bczht,                                 &
     &           bcxrow, bc0rg, loc_zorr, loc_zordg,                    &
     &           loc_zo, loc_zov, awzzo, brcd)

      ! find zero plane displacement of location
      call sbzdisp (bszrgh, bcxrow, bc0rg, wzoflg,                      &
     &      restot%rlaitot, restot%rsaitot, bbzht,                      &
     &      bcrlai, bcrsai, bczht, awzdisp, loc_zd)

      ! set location adjusted meteorological height (mm)
      ! see RZWQM manual pages 69-70
      loc_met_height = (met_height*mtomm) + loc_zd - awzdisp

      ! adjust wind velocity to adjusted agrometeorology height
      fld_wind = movewind(bwudav, anemht*mtomm, awzzo, awzdisp,         &
     &                    loc_met_height, loc_zov, loc_zd)

!     Calculate potential evapotranspiration using subroutine et
      g_soil = 0.0 ! test
      call et(rn, g_soil, fld_wind, bmzele, bwtdmx, bwtdmn, bwtdav,     &
     &        bwtdpt, bwrrh, ahzetp, loc_met_height, loc_zov, loc_zd)

      if (  ahzetp  .le.  0.0  ) then
         ahzep = 0.0
         ahzptp = 0.0
         ahzetp = 0.0
         ahzea = 0.0
         standevapredu = 1.0
         totalevapredu = 1.0
      else
         ! partition ET between potential plant transpiration and potential surface
         ! evaporation based on canopy partioning above as a function of leaf area
         ! index.
        
         ahzptp = ahzetp * epart
         ahzep = ahzetp - ahzptp

         ! If snow is present, it completely supresses any moisture loss (energy
         ! balance on snow determined snowmelt rates, no sublimation or evap assumed)
         if ( bhzsno .gt. 0.0 )  then
             ahzep = 0.0
         end if
         ! this is zeroed here as a starting point for darcy which uses potential
         ! and finds actual. Previously set to snow evap amount and added to soil
         ! surface evaporation in darcy
         ahzea = 0.0

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
         ahzep = ahzep * totalevapredu

      endif

!     Calculate soil water redistribution using subroutine darcy

      call timer(TIMHYDR,TIMSTOP)
      call timer(TIMDARC,TIMSTART)

      swc = dot_product(theta(1:layrsn),bszlyt(1:layrsn))

      ! select hydrology model for infiltration (insertion), evaporation and redistribution
      if( wepp_hydro .eq. 0 ) then
         ! use darcy method
         numeq = layrsn + 6

         call darcy( daysim, numeq, bszlyt, bszlyd, bsdblk,             &
     &       theta, thetadmx, thetas, thetaf, thetaw, thetar,           &
     &       bhrsk, bheaep, bh0cb, bsfcla, bsfom, bhtsav,               &
     &       bwtdmxprev, bwtdmn, bwtdmx, bwtdmnnext, bwtdpt,            &
     &       rise, daylength, ahzep, dprecip, bwdurpt, bwpeaktpt,       &
     &       dirrig, bhdurirr, bhlocirr, bhzoutflow,                    &
     &       bbdstm, bbffcv, bslrro, bslrr, bmzele, bhrwc0,             &
     &       ahzea, bhzper, bhzrun, bhzinf, bhzwid,                     &
     &       bhzeasurf, evaplimit, vaptrans, bmrslp )

      else
         ! use WEPP infiltration, evaporation, redistribution
         ! passing in reduced saturation instead of full saturation

         slen = amxsim(2,2)-amxsim(2,1)
         !write(*,*) 'daysim:', daysim

         call waterbal(layrsn, thetas, thetes, thetaf, thetaw,          &
     &                   bszlyt, bszlyd, bhrsk,                         &
     &                   dprecip, bwdurpt, bwpeaktpt, bwpeakipt,        &
     &                   dirrig, bhdurirr, bhlocirr, bhzoutflow,        &
     &                   bhzsno, bslrr, bmrslp, bsfsan, bsfcla,         &
     &                   bsfcr, bsvroc, bsdblk, bsfcec,                 &
     &                   bbffcv, bbfcancov, bbzht, bcdayap,             &
     &                   ahzep, theta, thetadmx, bhrwc0,                &
     &                   ahzea, bhzper, bhzrun, bhzinf, bhzwid,         &
     &                   slen, day, mo, yr, luowepphdrive,              &
     &                   wepp_hydro, init_loop, calib_loop, bhfice)

      end if

      swc = dot_product(theta(1:layrsn),bszlyt(1:layrsn))

      call timer(TIMDARC,TIMSTOP)
      call timer(TIMHYDR,TIMSTART)

!     following darcy, check total et against reduced soil surface ET
!      ahzptp = min(ahzptp, ahzetp - ahzea)

!     Calculate actual plant transpiration using subroutine transp
!     and determine the water stress factor
      if ( ahzptp .gt. 0.0 )  then
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
     &                   bhrsk, bhtsav, ahzptp, ahzpta, bhfwsf)

      else
          ahzpta = 0.0
          bhfwsf = 1.0
      end if
!     Calculate actual evapotranspiration
      ahzeta = ahzea + ahzpta

!     Convert water from volume back to mass basis
      do l=1,layrsn
         bhrwc(l) = theta(l) / bsdblk(l)
         bhrwcdmx(l) = thetadmx(l) / bsdblk(l)
      end do

      ! Adjust energy balance for changes in water content
!      call heat( layrsn, bszlyd, theta, bsfcla, bsdblk,                 &
!     &           bwtdmn, bwtdmx, bhtsmx, bhtsmn, bszlyt)

      ! update accumulated surface evaporation variable
      bhzeasurf = bhzeasurf + ahzea

!     update cumulative variables
      swc = dot_product(theta(1:layrsn),bszlyt(1:layrsn))
      cumprecip = cumprecip + bhzirr + bwzdpt
      cumrunoff = cumrunoff + bhzrun
      cumevap = cumevap + ahzea
      cumtrans = cumtrans + ahzpta
      cumdrain = cumdrain + bhzper
      presswc = swc
      pressnow = bhzsno
      presday = daysim
      
!     Added for WEPP bookeeping      
      wp_totalPrecip = wp_totalPrecip + bwzdpt
      wp_totalRunoff = wp_totalRunoff + bhzrun
      
      if (bwzdpt.gt.0) then
         wp_precipEvents = wp_precipEvents + 1
      endif
      
      if (bhzrun.gt.0) then
         wp_runoffEvents = wp_runoffEvents + 1
         if (bhzsmt .gt. 0.0) then    ! due to snowmelt
            wp_snowmeltEvents = wp_snowmeltEvents + 1
            wp_totalSnowrunoff = wp_totalSnowrunoff + bhzrun
         endif
      endif
!     End WEPP addition      

!     Print the daily soil water balance results to hydro.out
      if ((am0hfl .eq. 1).or.(am0hfl .eq. 3).or.(am0hfl .eq. 5).or.     &
     &   (am0hfl .eq.7)) then
         call caldatw(day,mo,yr)
         if ((am0csr .eq. 1) .and. (nsubr .gt. 1)) write(luohydro,2000)
         ! insert double blank line to break years into blocks for graphing
         if( idoy .eq. 1 ) then
             write(luohydro,*)
             write(luohydro,*)
         end if
         accheck = lswc - swc + lsno - bhzsno + bhzirr + bwzdpt         &
     &           - ahzea - ahzpta - bhzper - bhzrun
! 2090 format(1x,i5,1x,i3,1x,i4,11(1x,f6.2),2(1x,f8.2),2(1x,f6.2),1x,f7.3,11(1x,f6.2))
         write(luohydro,2090) daysim, idoy, yr, ahzetp, ahzep, ahzptp,  &
     &       ahzea, ahzpta, bhzper, bhzirr, bwzdpt, dprecip, bhzrun,    &
     &       bhzinf, lswc, swc, bhzsnd, bhzsno, accheck, cropdp,        &
     &      plant_wat_t(0.0,cropdp*mtomm,theta(1),thetaw,bszlyd,layrsn),&
     &       plant_wat_t(0.0,cropdp*mtomm,thetaf,thetaw,bszlyd,layrsn), &
     &       bhfwsf, bhrwc0(12)/bhrwcw(1), bwtdav, vaptrans, evaplimit, &
     &       standevapredu, bbevapredu, totalevapredu

      ! print out hydro values by layer (profile view)
         call printlayval( daysim, layrsn,                              &
     &        bszlyt, bszlyd, bsdblk,                                   &
     &        theta, thetas, thetaf, thetaw, thetar,                    &
     &        bhrsk, bheaep, bh0cb, bsfcla, bsfom, bhtsav )

      end if

      return
      end
