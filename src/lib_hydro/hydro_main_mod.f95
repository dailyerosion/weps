!$Author$
!$Date$
!$Revision$
!$HeadURL$

module hydro_main_mod

  contains

    subroutine hydro ( isr, brcd, bbzht, &
                         bbrlai, bczht, bcdayap, &
                         bbfcancov, bbrlailive, &
                         bdmres, bbevapredu, &
                         bbdstm, bbffcv, &
                         bmzele, &
                         bhzdmaxirr, bhratirr, bhdurirr, &
                         bhlocirr, bhminirr, bm0monirr, &
                         bhmadirr, bhndayirr, bhmintirr, &
                         bhzoutflow, bhzinf, &
                         bhzsno, bhtsno, bhfsnfrz, &
                         bhzsmt, &
                         bhrwc0, daysim, &
                         bhzwid, &
                         bhzeasurf, &
                         soil, plant, h1et, h1bal, wp)

      ! This subroutine is the main (supervisory) program for the
      ! HYDROLOGY submodel.  The subroutine controls the calling of the
      ! major subprograms of the HYDROLOGY submodel.

      use weps_cmdline_parms, only: transpiration_depth, wepp_hydro
      use weps_main_mod, only: init_loop, calib_loop, am0ifl, ijday, ljday
      use datetime_mod, only: get_psim_day, get_psim_mon, get_psim_year, get_psim_doy, get_psim_juld
      use file_io_mod, only: luohydro, luohlayers, luosurfwat, luoweather
      use soil_data_struct_defs, only: soil_def
      use biomaterial, only: plant_pointer, biototal
      use p1unconv_mod, only: mtomm, mmtom
      use erosion_data_struct_defs, only: anemht, awzzo, awzdisp, wzoflg
      use grid_mod, only: amxsim
      use Points_Mod, only: slen
      use wind_mod, only: sbzdisp, sbzo, biodrag
      use hydro_data_struct_defs, only: am0hfl, hydro_derived_et, claygrav80rh, orggrav80rh, gravconst
      use report_hydrobal_mod, only: hydro_balance
      use hydro_darcy_mod, only: darcy
      use hydro_wepp_mod, only: waterbal
      use hydro_heat_mod, only: heat, addsnow
      use hydro_util_mod, only: radnet, transp, volwatadsorb
      use wepp_param_mod, only: wepp_param
      use climate_input_mod, only: cli_tyav, cli_day, wind_day
      use air_water_mod, only: et, rel_humid
      use solar_mod, only: dawn, daylen, beamrise, amalat, amalon

      ! + + + ARGUMENT DECLARATIONS + + +
      integer, intent(in) :: isr   ! subregion number
      real, intent(in) :: brcd  ! biomass drag coefficient
      real bbzht
      real bbrlai, bczht
      integer bcdayap
      real bbfcancov, bbrlailive
      real bdmres, bbevapredu
      real bbdstm, bbffcv
      real bmzele
      real bhzdmaxirr, bhratirr, bhdurirr
      real bhlocirr, bhminirr
      integer bm0monirr
      real bhmadirr
      integer bhndayirr, bhmintirr
      real bhzoutflow, bhzinf
      real bhzsno, bhtsno, bhfsnfrz
      real bhzsmt
      real bhrwc0(*)
      integer daysim
      real bhzwid
      real bhzeasurf
      type(soil_def), intent(inout) :: soil  ! soil for this subregion
      type(plant_pointer), pointer :: plant  ! pointer to youngest plant data, which chains to older plant data
      type(hydro_derived_et), intent(inout) :: h1et
      type(hydro_balance), intent(inout) :: h1bal
      type(wepp_param), intent(inout) :: wp

      ! + + +  ARGUMENT DEFINITIONS + + +
      ! bbzht  - composite average residue height (m)
      ! bczht  - crop height (m)
      ! bcdayap - number of days of growth completed since crop planted
      ! bbfcancov - total biomass canopy cover (decimal)
      ! bbrlailive - leaf area index of transpiring leaves
      ! bdmres   - Plant residues on the soil surface (kg/m^2)
      ! bbevapredu - reduction in surface evaporation from flat residue (Eactual/Epotential ratio)
      ! bbdstm   - total number of stems (#/m^2)
      ! bbffcv   - 
      ! bmzele   - Average site elevation (m)
      ! bhzdmaxirr - characteristic maximum irrigation system application depth (mm)
      ! bhratirr - characteristic irrigation system application rate (mm/hour)
      ! bhdurirr - duration of irrigation water application (hours) 
      !            corresponding to the characteristic maximum irrigation
      !            system application depth. This is used to set the rate (depth / duration)
      ! bhlocirr - emitter location point (mm)
      !            positive is above the soil surface
      !            negative is below the soil surface
      ! bhminirr - minimum irrigation application amount (mm)
      ! bm0monirr - flag setting monitoring for irrigation need
      !             0 - do not monitor irrigation need
      !             1 - monitor irrigation need
      ! bhmadirr - management allowed depletion used in monitoring irrigation
      !            0.0 sets up replacing yesterdays water loss today
      !            1.0 schedules next application at wilting point
      ! bhndayirr - the next simulation day on which an irrigation can occur (day)
      ! bhmintirr - minimum interval for irrigation application (days)
      ! bhzoutflow - height of runoff outlet above field surface (m)
      ! bhzsno   - depth of water in snow layer (mm)
      ! bhtsno   - temperature of snow layer (C)
      ! bhfsnfrz   - fraction of snow layer water content which is frozen
      ! bhzsnd   - depth of snow (mm)
      ! bhzsmt   - Snow melt (mm)
      ! bhrwc0   - Hourly water content of the soil surface - darcy supplies
      ! daysim   - day of the simulation
      ! bhzwid   - Water infiltration depth (mm)
      ! bhzeasurf - accumulated surface evaporation since last complete rewetting (mm)
      ! cumprecip - accumulation of rainfall (mm)
      ! cumirrig  - accumulation of irrigation (mm)
      ! cumrunoff - accumulation of runoff (mm) (mm)
      ! cumevap   - accumulation of evaporation (mm)
      ! cumtrans  - accumulation of transpiration (mm)
      ! cumdrain  - accumulation of drainage (mm)
      ! presswc   - present soil water content (mm)
      ! pressnow  - present snow water content (mm)
      ! presday   - present day

      ! + + + LOCAL VARIABLES + + +
      integer :: pjuld  ! present julian day for subregion
      integer :: prevjuld  ! previous julian day for subregion
      integer :: nextjuld  ! next julian day for subregion
      integer l, idx
      integer idoy
      !  integer theta_check
      real rise, daylength
      real rn, g_soil
      integer numeq
      real lswc, lsno
      real dprecip, dirrig
      real durprecip, tptprecip
      real epart, vlh, vaptrans, evaplimit
      real rad_surf
      real loc_zorr, loc_zordg, loc_zo, loc_zov, loc_zd, fld_wind
      real rootz_p_con, rootz_p_cap, paw
      real :: len_slope
      !  real old_wind, old_etp

      real d1, d2
      parameter   (d1 = 2.5002773719, d2 = 0.0023644939)

      real met_height, loc_met_height
      parameter   (met_height = 2.0)

      real standevapredu, totalevapredu, accheck
      real temp, airentry(soil%nslay), lambda(soil%nslay), theta80rh(soil%nslay)
      real :: cropdp   ! depth into soil (mm) layering that a plant extracts water
                       ! This is either zrtd or ztranspdepth depending on
                       ! the command line flag transpiration_depth
      real :: cropdp_max  ! maximum crop depth over all growing crops
      real :: fwsf_wavg   ! depth weighted avarage water stress factor over all growing crops
      real :: fwsf_weight ! depth based weight
      real :: fwsf_sumw   ! sum of depth based weights
      integer :: nplant   ! counter for number of plants used in average
      type(plant_pointer), pointer :: thisPlant       ! pointer used to interate plant pointer chain

      ! + + + LOCAL DEFINITIONS + + +
      ! idoy      - Day of year
      ! l         - Loop index of soil layers
      ! idx       - loop index
      ! rise      - time of sunrise adjusted to local time (hours)
      ! daylength - length of day (hours)
      ! rn        - net radiation from function radnet (Mj/m^2/day)
      ! g_soil    - soil heat flux (Mj/m^2/day)
      ! numeq     - number of equations to be solved by darcy routines
      ! lswc      - last days soil profile water content (mm)
      ! lsno      - last days snow water content (mm)
      ! dprecip   - cumulative daily amount of water from precipitation that
      !             makes it through the snow filter
      ! dirrig    - cumulative daily amount of water from surface irrigation
      !             that makes it through the snow filter
      ! durprecip - duration of precipitation adjusted for snowmelt
      ! tptprecip - time to peak intensity of precipitation adjusted for snowmelt
      ! epart     - evaporation partitioning to plant (transpiration)
      ! vlh       - Latent heat of vaporization (mj/kg)
      ! vaptrans  - vapor transmissivity (mm/d^.5)
      ! evaplimit - accumulated surface evaporation since last complete rewetting
      !             defining limit of stage 1 (energy limited) and start of 
      !             stage 2 (soil vapor transmissivity limited) evaporation (mm)
      ! rad_surf  - radiation partitioned between surface and canopy
      ! loc_zorr  - aerodynamic roughness of random roughness (mm)
      ! loc_zordg - aerodynamic roughness of ridges (mm)
      ! loc_zo    - aerodynamic roughness of surface below canopy (mm)
      ! loc_zov   - aerodynamic roughness length of canopy (mm)
      ! loc_zd    - zero plane displacement (mm)
      ! fld_wind  - wind velocity at field location
      ! d1,d2     - constants used to compute latent heat of vaporization
      ! met_height - height of measurement of meteorological sensors (m)
      ! loc_met_height - met_height adjusted to be representative over crop/residue
      ! standevapredu  -  evaporation reduction factor attributed to standing mass
      ! totalevapredu  -  combined (standing and flat) evaporation reduction factor
      ! paw       - plant available water (fraction field cap - wilting point)
      ! len_slope - slope length(m)

      ! + + + SUBROUTINES CALLED + + +
      ! hinit
      ! heat
      ! snomlt
      ! et
      ! darcy
      ! transp
      ! caldat

      ! + + + FUNCTION DECLARATIONS + + +
      !  real dawn
      !  real daylen
      !  real radnet
      !  real availwc
      !  real plant_wat_t
      !  real movewind
      !  real volwatadsorb

      ! + + + DATA INITIALIZATIONS + + +
      ! set julian day (index to climate values)
      pjuld = get_psim_juld(isr)

      ! Calculate hour of sunrise
      idoy = get_psim_doy(isr)
      rise = dawn(amalat, amalon, idoy, beamrise)
      daylength = daylen(amalat, idoy, beamrise)

      call hinit (soil%nslay, soil%asdblk, soil%asdblk0, soil%asdpart, &
                  soil%ahrwc, soil%ahrwcs, soil%ahrwcf, soil%ahrwcw, soil%ahrwcr, &
                  soil%ahrwca, soil%ah0cb, soil%aheaep, soil%ahrsk, soil%ahfredsat, &
                  soil%asfsan, soil%asfsil, soil%asfcla, soil%asfom, soil%asfcec, &
                  soil%aszlyd, soil%aszlyt, vaptrans, evaplimit, soil)

      ! set accounting variables for water balance changes in this cycle
      soil%swc = dot_product(soil%theta(1:soil%nslay),soil%aszlyt(1:soil%nslay))
      lswc = soil%swc
      lsno = bhzsno

      ! + + + END SPECIFICATIONS + + +
      ! write headers and inital values to hydro.out
      if(    (am0ifl(isr) .eqv. .true.) &
        .and.((am0hfl(isr) .eq. 1) .or. (am0hfl(isr) .eq. 3) &
        .or.  (am0hfl(isr) .eq. 5) .or. (am0hfl(isr) .eq. 7)) ) then

         ! Echo print of input soil data

         !     write(luohydro(isr),2020)
         !     do l=1,soil%nslay
         !        write(luohydro(isr),2030) l,soil%aszlyd(l),soil%theta(l),soil%thetas(l),soil%thetaf(l), &
         !                    soil%thetaw(l),soil%ah0cb(l),soil%aheaep(l),soil%ahrsk(l),soil%asdblk(l)
         !     end do
         !     write(luohydro(isr),2040)
         !     write(luohydro(isr),2050) soil%theta(0)
         !     write(luohydro(isr),2060) soil%swci
         !     write(luohydro(isr),2070)
         write(luohydro(isr), "(3a)") &
           '# daysim doy yr  ahzetp  ahzep ahzptp  ahzea ahzpta bhzper ', &
           'bhzirr bwzdpt  dprec bhzrun bhzinf   lswc   swc  bhzsnd bhzsno  check surfdry bwtdav ', &
           'vaptrans evaplimit st_evapr fl_evapr to_evapr cropdp rootwc rootwcap bhfwsf'

         ! header for file for layer information
         write(luohlayers(isr), "(3a,i3)") &
           '# daysim doy yr layer depth theta thetas thetaf thetaw thetar ', &
           'availwat satrat bhtsav unsatcond matricpot relhum bulkden ', &
           'airentry expon_b k_sat numlay = ', soil%nslay

         ! print out hydro values by layer (profile view)
         ! day zero values
         ! call printlayval( isr, 0, soil%nslay, &
         !         soil%aszlyt, soil%aszlyd, soil%asdblk, &
         !         soil%theta, soil%thetas, soil%thetaf, soil%thetaw, soil%thetar, &
         !         soil%ahrsk, soil%aheaep, soil%ah0cb, soil%asfcla, soil%asfom, soil%tsav )
      end if

      if( (am0ifl(isr) .eqv. .true.) .and.((am0hfl(isr) .eq. 2).or.(am0hfl(isr) .eq. 6) &
         .or. (am0hfl(isr) .eq. 3) .or. (am0hfl(isr) .eq. 7)) ) then

         write(luosurfwat(isr), "(a)") '# hr daysim idoy yr bhrwc0 bhrwc0/ahrwcw(1)'

         ! print out daily weather as used in hydro
         write(luoweather(isr), "(a)") '# daysim idoy yr rn cli_day%tdmx cli_day%tdmn cli_day%tdpt fld_wind rise rel_humid'
      end if

      !      write(*,*) 'hydro:total 500mm',
      !      plant_wat_t( 0.0, 500.0, soil%thetaf, soil%thetaw, soil%aszlyd, soil%nslay ),
      !      plant_wat_t( 500.0, 1000.0, soil%thetaf, soil%thetaw, soil%aszlyd, soil%nslay ),
      !      plant_wat_t( 1000.0, 1500.0, soil%thetaf, soil%thetaw, soil%aszlyd, soil%nslay )

      ! calculate transpiration related soil values
      do l=1,soil%nslay
          airentry(l) = soil%aheaep(l) / gravconst
          lambda(l) = 1.0 / soil%ah0cb(l)
          temp = soil%asdblk(l)*1000.0  !convert Mg/m^3 to kg/m^3
          theta80rh(l) = volwatadsorb( temp, soil%asfcla(l), soil%asfom(l), claygrav80rh, orggrav80rh )
      end do

      ! point to youngest plant
      thisPlant => plant

      ! zero actual for summing over plants
      h1et%zpta = 0.0

      ! interate over all plants
      ! applies water amount require to meet depletion of most stressed plant
      do while ( associated(thisPlant) )

        ! check command line transpiration depth flag, set plant depth accordingly
        if( transpiration_depth .eq. 0 ) then
          cropdp = thisPlant%geometry%zrtd * mtomm
        else
          cropdp = thisPlant%deriv%ztranspdepth * mtomm
        end if

        ! check irrigation flag for irrigation monitoring option
        if(      (bm0monirr .gt. 0) .and. (cropdp .gt. 0.0) &
           .and. (thisPlant%mass%standleaflive .gt. 0.0) .and. thisPlant%growth%living ) then

          ! find root zone water content above wilting point
          rootz_p_con = plant_wat_t(0.0, cropdp, soil%theta(1), soil%thetaw, soil%aszlyd, soil%nslay)
          ! find root zone water capacity between field capacity and wilting point
          rootz_p_cap = plant_wat_t(0.0, cropdp, soil%thetaf, soil%thetaw, soil%aszlyd, soil%nslay)
          ! find paw (plant available water) ratio in rootzone
          paw = rootz_p_con / rootz_p_cap

          ! find plant water stress level
          ! this is needed to overcome soils that generate maximum stress without reaching PAW limits
          ! this call finds stress value without extracting the water
          ! note: as written, the potential Transpiration is from previous day
          ! partition potential transpiration based on proportion of living leaf area in canopy leaf area
          if( bbrlailive .gt. 0.0 ) then
              thisPlant%growth%ptp = h1et%zptp * ((thisPlant%deriv%fliveleaf * thisPlant%deriv%rlai) / bbrlailive)
          else
              thisPlant%growth%ptp = 0.0
          end if

          call transp (soil%nslay, 0, soil%aszlyd, soil%aszlyt, cropdp, &
                       soil%theta, soil%thetas, soil%thetaf, soil%thetaw, &
                       theta80rh, soil%thetar, airentry, lambda, &
                       soil%ahrsk, soil%tsav, thisPlant%growth%ptp, thisPlant%growth%pta, thisPlant%growth%fwsf)
          ! sum actual transipration
          h1et%zpta = h1et%zpta + thisPlant%growth%pta

          ! check for irrigation depletion or stress trigger
          ! present, both trigger at the same level
          if( ((paw.lt.(1.0-bhmadirr)) .or. (thisPlant%growth%fwsf.lt.(1.0-bhmadirr))) &
              .and. (daysim .ge. bhndayirr) ) then
              ! add irrigation
              ! maximum of single day value and root zone deficit
              h1et%zirr = max( h1et%zirr, rootz_p_cap - rootz_p_con )
              ! limit value based on system characteristic maximum and minimum depths
              h1et%zirr = min( bhzdmaxirr, max( h1et%zirr, bhminirr ) )
              ! set duration based on depth and rate
              call ratedura(h1et%zirr, bhratirr, bhdurirr)

          end if
        end if

        ! point to next older plant
        thisPlant => thisPlant%olderPlant

      end do

      if( h1et%zirr .gt. 0.0 ) then
          ! irrigation applied, set next irrigation day
          bhndayirr = daysim + bhmintirr
      end if

      ! run daily precipitation and irrigation through the snow filter
      ! returned value reflects how much was left behind
      call addsnow(dprecip, dirrig, cli_day(pjuld)%zdpt, h1et%zirr, bhlocirr, &
                   cli_day(pjuld)%tdmn, cli_day(pjuld)%tdmx, cli_day(pjuld)%tdpt, bmzele, &
                   bhzsno, bhtsno, bhfsnfrz, h1et%zsnd)

      ! Convert global to net radiation
      ! this includes residue leaf area
      rn = radnet(bbrlai,cli_day(pjuld)%eirr, bhzsno, h1et%zsnd, cli_day(pjuld)%tdmx, cli_day(pjuld)%tdmn, amalat,&
                  idoy, cli_day(pjuld)%tdpt, soil )

      ! partition radiation between canopy and surface
      ! added exponential to keep above zero for very low lai
      epart = 1.0 - exp(-0.398*bbrlai)
      epart = max( epart, min( 1.0, (-0.21 + 0.7 * (bbrlai**0.5)) ) )
      ! check for snow
      if( bhzsno .gt. 0.0 ) then
          ! snow present, all net rad goes on the surface
          rad_surf = rn
      else
          ! no snow, so partition between canopy and soil surface
          rad_surf = rn * (1.0 - epart)
      end if

      ! recalculate partitioning to acount for non transpiring leaf surface
      epart = 1.0 - exp(-0.398*bbrlailive)
      epart = max( epart, min( 1.0, (-0.21 + 0.7 * (bbrlailive**0.5)) ) )

      ! Do energy balance for soil and cover temperatures 
      ! and determine snow melt (if any) or soil heat flux
      call heat( isr, soil%nslay, soil%aszlyd, soil%aszlyt, soil%theta, soil%thetas, &
                soil%asfsan, soil%asfsil, soil%asfcla, soil%asfom, soil%asdblk, &
                cli_day(pjuld)%tdmn, cli_day(pjuld)%tdmx, cli_tyav, rad_surf, bdmres, &
                soil%tsmn, soil%tsmx, soil%tsav, soil%fice, &
                bhzsno, bhtsno, bhfsnfrz, h1et%zsnd, &
                bhzsmt, g_soil )

      ! add snowmelt to precipitation water for infiltration
      if (bhzsmt .gt. 0.0) then
          dprecip = dprecip + bhzsmt
          durprecip = max(cli_day(pjuld)%durpt,6.0)!we have no entry for snowmelt duration yet
          tptprecip = 0.5
      else
          durprecip = cli_day(pjuld)%durpt
          tptprecip = cli_day(pjuld)%peaktpt
      endif

      ! replenish accumulated surface evaporation reservoir with applied suface water
      bhzeasurf = max(0.0, bhzeasurf - dprecip - dirrig)

      ! calculate dryness ratio
      if (cli_day(pjuld)%zdpt .eq. 0.0) then
         h1et%drat = 10.0
       else
         vlh = d1 - (d2*cli_day(pjuld)%tdav)                               !h-24
         h1et%drat = rn / (vlh * cli_day(pjuld)%zdpt)
      end if

      ! find roughness length of the surface for et wind speed adjustment to 2m
      call sbzo (soil%asxrgs, soil%aszrgh, soil%aslrr, bbzht, brcd, wzoflg, &
                 loc_zordg, loc_zorr, loc_zo, loc_zov, awzzo)

      ! find zero plane displacement of location
      call sbzdisp (wzoflg, brcd, bbzht, bczht, awzdisp, loc_zd)

      ! set location adjusted meteorological height (mm)
      ! see RZWQM manual pages 69-70
      loc_met_height = (met_height*mtomm) + loc_zd - awzdisp

      ! adjust wind velocity to adjusted agrometeorology height
      fld_wind = movewind(wind_day(pjuld)%wawudav, anemht*mtomm, awzzo, awzdisp, &
                          loc_met_height, loc_zov, loc_zd)

      ! Calculate potential evapotranspiration using function et
      g_soil = 0.0 ! test
      h1et%zetp = et(rn, g_soil, fld_wind, bmzele, cli_day(pjuld)%tdmx, cli_day(pjuld)%tdmn, &
                     cli_day(pjuld)%tdav, cli_day(pjuld)%tdpt, loc_met_height, loc_zov, loc_zd)

      if (  h1et%zetp  .le.  0.0  ) then
         h1et%zep = 0.0
         h1et%zptp = 0.0
         h1et%zetp = 0.0
         h1et%zea = 0.0
         standevapredu = 1.0
         totalevapredu = 1.0
      else
         ! partition ET between potential plant transpiration and potential surface
         ! evaporation based on canopy partioning above as a function of live leaf area
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

         ! If plant residue is present, then reduce the potential soil
         ! evaporation on the basis of the amount of plant residues on
         ! the soil surface.
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

      ! Calculate soil water redistribution using subroutine darcy

      soil%swc = dot_product(soil%theta(1:soil%nslay),soil%aszlyt(1:soil%nslay))

      ! select hydrology model for infiltration (insertion), evaporation and redistribution
      if( wepp_hydro .eq. 0 ) then
         ! use darcy method
         numeq = soil%nslay + 6

         ! set previous and next julian days (index into climate array)
         if( pjuld .eq. ijday ) then
             prevjuld = ljday
         else
             prevjuld = pjuld - 1
         end if
         if( pjuld .eq. ljday ) then
             nextjuld = ijday
         else
             nextjuld = pjuld + 1
         end if

         call darcy( isr, daysim, numeq, soil%aszlyt,soil%aszlyd,soil%asdblk, &
            soil%theta, soil%thetadmx, soil%thetas, soil%thetaf, soil%thetaw, soil%thetar, &
            soil%ahrsk, soil%aheaep, soil%ah0cb, soil%asfcla, soil%asfom, soil%tsav, &
            cli_day(prevjuld)%tdmx, cli_day(pjuld)%tdmn, cli_day(pjuld)%tdmx, cli_day(nextjuld)%tdmn, cli_day(pjuld)%tdpt, &
            rise, daylength, h1et%zep, dprecip, durprecip, tptprecip, &
            dirrig, bhdurirr, bhlocirr, bhzoutflow, &
            bbdstm, bbffcv, soil%aslrro, soil%aslrr, bmzele, bhrwc0, &
            h1et%zea, h1et%zper, h1et%zrun, bhzinf, bhzwid, &
            bhzeasurf, evaplimit, vaptrans, soil%amrslp )

      else
         ! use WEPP infiltration, evaporation, redistribution
         ! passing in reduced saturation instead of full saturation

         ! use a representative slope length as half of the simregion diagonal distance
         len_slope = slen(amxsim(1), amxsim(2)) / 2.0

         call waterbal(soil%nslay, soil%thetas, soil%thetes, soil%thetaf, soil%thetaw, &
                        soil%aszlyt, soil%aszlyd, soil%ahrsk, &
                        dprecip, durprecip, tptprecip, cli_day(pjuld)%peakipt, &
                        dirrig, bhdurirr, bhlocirr, bhzoutflow, &
                        bhzsno, soil%aslrr, soil%amrslp, soil%asfsan, soil%asfcla, &
                        soil%asfcr, soil%asvroc, soil%asdblk, soil%asfcec, &
                        bbffcv, bbfcancov, bbzht, bcdayap, &
                        h1et%zep, soil%theta, soil%thetadmx, bhrwc0, &
                        h1et%zea, h1et%zper, h1et%zrun, bhzinf, bhzwid, &
                        len_slope, get_psim_day(isr), get_psim_mon(isr), get_psim_year(isr), isr, &
                        wepp_hydro, init_loop(isr), calib_loop(isr), soil%fice, wp)

      end if

      soil%swc = dot_product(soil%theta(1:soil%nslay),soil%aszlyt(1:soil%nslay))

      ! following darcy, check total et against reduced soil surface ET
      !  h1et%zptp = min(h1et%zptp, h1et%zetp - h1et%zea)

      ! find maximum depth for transpiring plants
      ! point to youngest plant
      thisPlant => plant
      ! zero actual for finding max over plants
      cropdp_max = 0.0
      ! interate over all transpiring plants
      do while ( associated(thisPlant) )
        if( (thisPlant%mass%standleaflive .gt. 0.0) .and. thisPlant%growth%living) then
          ! check command line transpiration depth flag, set plant depth accordingly
          if( transpiration_depth .eq. 0 ) then
            cropdp = thisPlant%geometry%zrtd * mtomm
          else
            cropdp = thisPlant%deriv%ztranspdepth * mtomm
          end if
          cropdp_max = max(cropdp_max, cropdp)
        end if
        ! point to next older plant
        thisPlant => thisPlant%olderPlant
      end do

      ! Calculate actual plant transpiration using subroutine transp,
      ! remove water from the soil and determine the water stress factor.
      ! NOTE: this gives priority to the youngest plant for soil water.
      ! NOTE: with smaller rootzone of younger plant, should be small effect
      if ( h1et%zptp .gt. 0.0 ) then
        ! transpiration occurs

        ! recalculate transpiration related soil values
        do l=1,soil%nslay
            airentry(l) = soil%aheaep(l) / gravconst
            lambda(l) = 1.0 / soil%ah0cb(l)
            temp = soil%asdblk(l)*1000.0  !convert Mg/m^3 to kg/m^3
            theta80rh(l) = volwatadsorb( temp, soil%asfcla(l), soil%asfom(l), claygrav80rh, orggrav80rh )
        end do

        ! point to youngest plant
        thisPlant => plant
        ! zero actual for summing over plants
        h1et%zpta = 0.0
        ! interate over all transpiring plants
        do while ( associated(thisPlant) )
          if( (thisPlant%mass%standleaflive .gt. 0.0) .and. thisPlant%growth%living) then
            ! check command line transpiration depth flag, set plant depth accordingly
            if( transpiration_depth .eq. 0 ) then
              cropdp = thisPlant%geometry%zrtd * mtomm
            else
              cropdp = thisPlant%deriv%ztranspdepth * mtomm
            end if
            ! partition potential transpiration based on proportion of living leaf area in canopy leaf area
            thisPlant%growth%ptp = h1et%zptp * ((thisPlant%deriv%fliveleaf * thisPlant%deriv%rlai) / bbrlailive)
            call transp (soil%nslay, 1, soil%aszlyd, soil%aszlyt, cropdp, &
                       soil%theta, soil%thetas, soil%thetaf, soil%thetaw, &
                       theta80rh, soil%thetar, airentry, lambda, &
                       soil%ahrsk, soil%tsav, thisPlant%growth%ptp, thisPlant%growth%pta, thisPlant%growth%fwsf)
            ! sum actual transpiration
            h1et%zpta = h1et%zpta + thisPlant%growth%pta
          end if
          ! point to next older plant
          thisPlant => thisPlant%olderPlant
        end do

      else
        ! zero potential transpiration
        h1et%zpta = 0.0

        ! interate over all transpiring plants
        ! point to youngest plant
        thisPlant => plant
        do while ( associated(thisPlant) )
          if( thisPlant%mass%standleaflive .gt. 0.0 ) then
            ! has transpiring leaf area
            thisPlant%growth%fwsf = 1.0
          end if
          ! point to next older plant
          thisPlant => thisPlant%olderPlant
        end do
      end if
      ! Calculate actual evapotranspiration
      h1et%zeta = h1et%zea + h1et%zpta

      ! Convert water from volume back to mass basis
      do l=1,soil%nslay
         soil%ahrwc(l) = soil%theta(l) / soil%asdblk(l)
         soil%ahrwcdmx(l) = soil%thetadmx(l) / soil%asdblk(l)
      end do

      ! Adjust energy balance for changes in water content
      !  call heat( soil%nslay, soil%aszlyd, soil%theta, soil%asfcla, soil%asdblk, &
      !             cli_day(pjuld)%tdmn, cli_day(pjuld)%tdmx, soil%tsmx, soil%tsmn, soil%aszlyt)

      ! update accumulated surface evaporation variable
      bhzeasurf = bhzeasurf + h1et%zea

      ! update cumulative variables
      soil%swc = dot_product(soil%theta(1:soil%nslay),soil%aszlyt(1:soil%nslay))
      h1bal%cumprecip = h1bal%cumprecip + cli_day(pjuld)%zdpt
      h1bal%cumirrig = h1bal%cumirrig + h1et%zirr
      h1bal%cumrunoff = h1bal%cumrunoff + h1et%zrun
      h1bal%cumevap = h1bal%cumevap + h1et%zea
      h1bal%cumtrans = h1bal%cumtrans + h1et%zpta
      h1bal%cumdrain = h1bal%cumdrain + h1et%zper
      h1bal%presswc = soil%swc
      h1bal%pressnow = bhzsno
      h1bal%presday = daysim
      
      ! Added for WEPP bookeeping      
      wp%totalPrecip = wp%totalPrecip + cli_day(pjuld)%zdpt
      wp%totalRunoff = wp%totalRunoff + h1et%zrun
      
      if (cli_day(pjuld)%zdpt.gt.0) then
         wp%precipEvents = wp%precipEvents + 1
      endif
      
      if (h1et%zrun.gt.0) then
         wp%runoffEvents = wp%runoffEvents + 1
         if (bhzsmt .gt. 0.0) then    ! due to snowmelt
            wp%snowmeltEvents = wp%snowmeltEvents + 1
            wp%totalSnowrunoff = wp%totalSnowrunoff + h1et%zrun
         endif
      endif
      ! End WEPP addition      

      ! Print the daily soil water balance results to hydro.out
      if(    (am0hfl(isr) .eq. 1) .or. (am0hfl(isr) .eq. 3) &
        .or.(am0hfl(isr) .eq. 5) .or. (am0hfl(isr) .eq. 7)) then
        ! insert double blank line to break years into blocks for graphing
        if( idoy .eq. 1 ) then
            write(luohydro(isr),*)
            write(luohydro(isr),*)
        end if
        accheck = lswc - soil%swc + lsno - bhzsno + h1et%zirr + cli_day(pjuld)%zdpt &
                - h1et%zea - h1et%zpta - h1et%zper - h1et%zrun

        write(luohydro(isr),"(1x,i6,1x,i3,1x,i4,11(1x,f6.2),2(1x,f8.2),2(1x,f6.2),1x,f7.3,10(1x,f6.2))",ADVANCE="NO") &
            daysim, idoy, get_psim_year(isr), h1et%zetp, h1et%zep, h1et%zptp,&
            h1et%zea, h1et%zpta, h1et%zper, h1et%zirr, cli_day(pjuld)%zdpt, dprecip, h1et%zrun, &
            bhzinf, lswc, soil%swc, h1et%zsnd, bhzsno, accheck, &
            bhrwc0(12)/soil%ahrwcw(1), cli_day(pjuld)%tdav, vaptrans, evaplimit, &
            standevapredu, bbevapredu, totalevapredu, cropdp_max*mmtom, &
              plant_wat_t(0.0,cropdp_max,soil%theta(1),soil%thetaw,soil%aszlyd,soil%nslay),&
              plant_wat_t(0.0,cropdp_max,soil%thetaf,soil%thetaw,soil%aszlyd,soil%nslay)

        ! find weighted average of plant water stress factor
        ! zero out weighted average and counter
        fwsf_wavg = 0.0
        nplant = 0
        fwsf_sumw = 0.0
        ! point to youngest plant
        thisPlant => plant
        ! interate over all transpiring plants
        do while ( associated(thisPlant) )

          if( (thisPlant%mass%standleaflive .gt. 0.0) .and. thisPlant%growth%living) then
            ! increment counter
            nplant = nplant + 1 

            ! check command line transpiration depth flag, set plant depth accordingly
            if( transpiration_depth .eq. 0 ) then
              cropdp = thisPlant%geometry%zrtd * mtomm
            else
              cropdp = thisPlant%deriv%ztranspdepth * mtomm
            end if

            ! weight for this plant
            fwsf_weight = cropdp/cropdp_max
            ! accumulate weighted sums
            fwsf_wavg = fwsf_wavg + fwsf_weight * thisPlant%growth%fwsf
            ! accumulate sum of weights
            fwsf_sumw = fwsf_sumw + fwsf_weight
          end if
          ! point to next older plant
          thisPlant => thisPlant%olderPlant

        end do
        if( fwsf_sumw .gt. 0.0 ) then
          ! final weighted average
          fwsf_wavg = fwsf_wavg / fwsf_sumw
        end if
        write(luohydro(isr),"(1x,f6.2)",ADVANCE="YES") fwsf_wavg

        ! print out hydro values by layer (profile view)
        call printlayval( isr, daysim, soil%nslay, &
             soil%aszlyt, soil%aszlyd, soil%asdblk, &
             soil%theta, soil%thetas, soil%thetaf, soil%thetaw, soil%thetar, &
             soil%ahrsk, soil%aheaep, soil%ah0cb, soil%asfcla, soil%asfom, soil%tsav )
      end if

      if ((am0hfl(isr) .eq. 2).or.(am0hfl(isr) .eq. 6) &
         .or. (am0hfl(isr) .eq. 3) .or. (am0hfl(isr) .eq. 7)) then
         ! print out hourly surface water content values
         do idx = 1, 24
            write(luosurfwat(isr),*) idx, daysim, idoy, get_psim_year(isr), bhrwc0(idx), bhrwc0(idx)/soil%ahrwcw(1)
         end do

         ! print out daily weather as used in hydro
         write(luoweather(isr),*) daysim, idoy, get_psim_year(isr), rn, cli_day(pjuld)%tdmx, cli_day(pjuld)%tdmn, &
               cli_day(pjuld)%tdpt, fld_wind, rise, rel_humid(cli_day(pjuld)%tdmx, cli_day(pjuld)%tdmn, cli_day(pjuld)%tdpt)
      end if

      return
    end subroutine hydro

    subroutine hinit(layrsn, bsdblk, bsdblk0, bsdpart, &
                     bhrwc, bhrwcs, bhrwcf, bhrwcw, bhrwcr, &
                     bhrwca, bh0cb, bheaep, bhrsk, bhfredsat, &
                     bsfsan, bsfsil, bsfcla, bsfom, bsfcec, &
                     bszlyd, bszlyt, vaptrans, evaplimit, soil)

      ! + + + PURPOSE + + +
      ! This subroutine controls the initialization of the HYDROLOGY
      ! sumbodel of WEPS.  The program initializes the depth variables
      ! of the soil simulation layers and converts the soil water
      ! content variables from mass basis to volume basis.
      ! DATE:  09/16/93
      ! MODIFIED:  12/13/93
      ! MODIFIED:  07/28/95
      ! MODIFIED:  07/29/95
      ! This change was done to determine the average of soil
      ! properties from the first simulation layer (10 mm) and the second
      ! uppermost simulation layer (40 mm).  The average for the new
      ! uppermost simulation layer for the HYDROLOGY submodel (50 mm thick)
      ! Using 50 mm as the thickness of the uppermost simulation layer for the
      ! HYDROLOGY submodel will increase the speed of simulation and reduce the
      ! the potential for errors.

      ! + + + KEYWORDS + + +
      ! initialization, hydrology

      use weps_cmdline_parms, only: wc_type
      use hydro_data_struct_defs, only: claygrav80rh, orggrav80rh
      use hydro_util_mod, only: param_blkden_adj, param_prop_bc, volwatadsorb
      use soil_data_struct_defs, only: soil_def

      ! + + + ARGUMENT DECLARATIONS + + +
      integer layrsn
      real bsdblk(*), bsdblk0(*), bsdpart(*)
      real bhrwc(*), bhrwcs(*), bhrwcf(*), bhrwcw(*), bhrwcr(*)
      real bhrwca(*), bh0cb(*), bheaep(*), bhrsk(*), bhfredsat(*)
      real bsfsan(*), bsfsil(*), bsfcla(*), bsfom(*), bsfcec(*)
      real bszlyd(*), bszlyt(*), vaptrans, evaplimit
      type(soil_def), intent(inout) :: soil  ! soil for this subregion

      ! + + + ARGUMENT DEFINITIONS + + +
      ! layrsn - Number of soil layers used in simulation
      ! bsdblk  - Soil bulk density (Mg/m^3)
      ! bsdblk0 - Previous day soil bulk density (Mg/m^3)
      ! bsdpart - Soil particle density (Mg/m^3)
      ! bhrwc   - Soil water content (mg/mg)
      ! bhrwcs  - Soil water content at saturation (mg/mg)
      ! bhrwcf  - Soil water content at field capacity (mg/mg)
      ! bhrwcw  - Soil water content at wilting point (mg/mg)
      ! bhrwcr  - Residual Soil water content (mg/mg)
      ! bh0cb   - Power of campbell's water release curve model (unitless)
      ! bheaep  - Soil air entry potential (j/kg)
      ! bhrsk   - Saturated hydraulic conductivity (m/s)
      ! bhfredsat - fraction of soil porosity that will be filled with water
      !             while wetting under normal field conditions due to entrapped air
      ! bsfsan  - Sand fractions
      ! bsfsil  - Silt fractions
      ! bsfcla  - Clay fractions
      ! bsfom   - fraction of total soil mass which is organic matter
      ! bsfcec  - Soil layer cation exchange capacity (cmol/kg) (meq/100g)
      ! bszlyd  - depth to the bottom of soil layer (mm)
      ! bszlyt  - soil layer thickness (mm)
      ! vaptrans - vapor transmissivity (mm/d^.5)
      ! evaplimit - accumulated surface evaporation since last complete rewetting
      !             defining limit of stage 1 (energy limited) and start of 
      !             stage 2 (soil vapor transmissivity limited) evaporation (mm)

      ! + + + LOCAL VARIABLES + + +
      integer k
      real :: potes ! locally used air entry potential
      real :: gmd   ! locally used particle geometric mean diameter
      real :: gsd   ! locally used particle geometric standard deviation
      real :: temp  ! temporary variable
      ! real temp1, temp2, temp3

      ! + + + LOCAL DEFINITION + + +
      ! potes  - Air entry potential at a std. bsdblk of 1.3 Mg/m^3

      ! + + + END SPECIFICATIONS + + +
      ! initialize various soil layer references

      if( wc_type.eq.4 ) then
          ! use texture based calculations from Rawls to set all soil
          ! water properties.
          call param_prop_bc( &
              layrsn, bszlyd, bsdblk, bsdpart, &
              bsfcla, bsfsan, bsfom, bsfcec, &
              bhrwcs, bhrwcf, bhrwcw, bhrwcr, &
              bhrwca, bh0cb, bheaep, bhrsk, &
              bhfredsat )
      else
          ! adjust hydro parameters for a change in bulk density
          call param_blkden_adj( layrsn, bsdblk, bsdblk0, &
              bsdpart, bhrwcf, bhrwcw, bhrwca, &
              bsfcla, bsfom, &
              bh0cb, bheaep, bhrsk )
      end if

      ! convert soil water contents from mass basis to volume basis
      do k=1,layrsn

          soil%theta(k) = bhrwc(k) * bsdblk(k)
          soil%thetadmx(k) = soil%theta(k)
          if( soil%theta(k) .lt. 0.0 ) then
              write (*,*) 'hinit: theta(',k,') .lt. 0'
              write (*,*) 'hinit: bhrwc =',bhrwc(k),'bsdblk =',bsdblk(k)
          end if
          if( wc_type.ne.4 ) then
              bhfredsat(k) = 0.883
          end if
          soil%thetas(k) = 1 - bsdblk(k) / bsdpart(k)   ! saturation
          soil%thetes(k) = soil%thetas(k) * bhfredsat(k)     ! reduced saturation content accounted for by entrapped air
          soil%thetaf(k) = bhrwcf(k) * bsdblk(k)        ! field capacity
          soil%thetaw(k) = bhrwcw(k) * bsdblk(k)        ! wilting point

        if( wc_type.eq.4 ) then
          soil%thetar(k) = bhrwcr(k) * bsdblk(k)        ! residual water content
        else
      !    use theta corresponding to 80% relhum in soil for soil%thetar
          temp = bsdblk(k)*1000.0  !convert Mg/m^3 to kg/m^3
          soil%thetar(k) = volwatadsorb( temp, bsfcla(k), bsfom(k), &
                                    claygrav80rh, orggrav80rh )
        end if

      !    call propsaxt(bsfsan(k), bsfcla(k), &
      !                  temp, temp1, temp2 )   !soil%thetas, soil%thetaf, soil%thetaw

      !    temp3 = (1.0-temp) * bsdpart(k) !bulk density
      !    write(*,1000) k,bszlyt(k), &
      !          bsfsan(k),bsfcla(k),bsfom(k), &
      !          bsdblk(k),soil%thetas(k), &
      !          soil%thetaf(k),soil%thetaw(k), &
      !          temp3, temp,     !bulkden, sat vol &
      !          temp1,           !field vol &
      !          temp2            !wilt vol

      !  used with output for soil file screening
      ! 1000     format(i3,f7.0,20f7.4)

      !      write(*,*) 'hinit:',k,bh0cb(k),bheaep(k),bhrsk(k),soil%thetas(k),
      !                 soil%thetaf(k),soil%thetaw(k),soil%thetar(k)

      !     Campbell functions
      !      call psd( bsfsan(k), bsfsil(k), bsfcla(k), gmd, gsd )
      !      potes(k) = -0.2 * gmd**(-0.5)                      !H-77
      !      bh0cb(k) = -2. * potes(k) + 0.2 * gsd              !H-78
      !      bheaep(k) = potes(k)*(bsdblk(k)/1.3)**(0.67*bh0cb(k)) !H-79

      !     reverse calculation of field capacity and wilting point
      !      soil%thetar(k) = 0.0
      !      temp = -33.33
      !      temp1 = 1.0 / bh0cb(k)
      !      soil%thetaf(k) = volwat_matpot_bc(temp, soil%thetar(k), soil%thetas(k), &
      !                                   bheaep(k), temp1)
      !      temp = -1500.0
      !      soil%thetaw(k) = volwat_matpot_bc(temp, soil%thetar(k), soil%thetas(k), &
      !                                   bheaep(k), temp1)

      !      write(*,*) 'hinit:',k,bsfsan(k),bsfsil(k),bsfcla(k),bsfom(k), &
      !                 bh0cb(k),bheaep(k),soil%thetas(k), &
      !                 soil%thetaf(k),soil%thetaw(k),soil%thetar(k),bhrsk(k)

      !     this is used with campbell functions as well
      !      bhrsk(k) = waterk(bsdblk(k), bh0cb(k), bsfcla(k), bsfsil(k))

      !      write(*,*) 'hinit:',k,bsfsan(k),bsfsil(k),bsfcla(k),bsfom(k), &
      !                 bh0cb(k),bheaep(k),soil%thetas(k), &
      !                 soil%thetaf(k),soil%thetaw(k),soil%thetar(k),bhrsk(k)

      end do

      !  soil%swci = sum(wc(1:layrsn))
      soil%swci = dot_product(soil%theta(1:layrsn),bszlyt(1:layrsn))

      !  soil%theta(0) = calctht0(bszlyd, soil%thetes, soil%thetar, soil%theta,
      ! *  soil%thetaw, soil%thetaf(1) - soil%thetaw(1), 0.0_8, 0.0_8)                        !H-64,65,66
      soil%theta(0) = soil%theta(1)

      ! calculate the vapor transmissivity (mm/d^.5) using the surface layer
      ! taken from WEPP documentation eq 5.2.11 with conversion to use soil 
      ! minerals in fractions not percent
      vaptrans = 4.165 + 2.456 * bsfsan(1) - 1.703 * bsfcla(1) &
               - 4.0 * bsfsan(1) * bsfsan(1)

      ! calculate the cumulative evaporation limit between stage 1 and stage 2 evap
      ! taken from WEPP documentation eq 5.2.10 with conversion to mm
      if( vaptrans .le. 3.0 ) then
          evaplimit = 0.0
      else
          evaplimit = 9.0 * (vaptrans - 3.0) ** 0.42
      end if

      ! call subroutine psd and calculate soil hydraulic parameters
      do k=1,layrsn
          if ( bh0cb(k) .eq. -99.9 ) then
              call psd( bsfsan(k), bsfsil(k), bsfcla(k), gmd, gsd)
              potes = -0.2 * gmd**(-0.5)         !H-77
              bh0cb(k) = -2. * potes + 0.2 * gsd  !H-78
              if ( bheaep(k) .eq. -99.90 ) then
                 bheaep(k) = potes*(bsdblk(k)/1.3)**(0.67*bh0cb(k))    !H-79
              end if
          end if
          if ( bhrsk(k) .eq. -99.90 ) then
             bhrsk(k) = waterk(bsdblk(k), bh0cb(k), bsfcla(k),bsfsil(k))
          end if
      end do

      return
    end subroutine hinit

    subroutine printlayval( isr, daysim, layrsn, &
             bszlyt, bszlyd, bulkden, &
             theta, thetas, thetaf, thetaw, thetar, &
             bhrsk, bheaep, bh0cb, bsfcla, bsfom, bhtsav )

!     + + + PURPOSE + + +
!     This subroutine print out soil hydro properties by layer

!     + + + KEYWORDS + + +
!     output hydro

      use file_io_mod, only: luohlayers
      use datetime_mod, only: get_psim_doy, get_psim_year
      use hydro_data_struct_defs, only: claygrav80rh, orggrav80rh, gravconst
      use hydro_util_mod, only: matricpot_bc, volwatadsorb, availwc, unsatcond_bc

!     + + + ARGUMENT DECLARATIONS + + +
      integer, intent(in) :: isr   ! subregion number
      integer daysim, layrsn
      real bszlyt(*), bszlyd(*), bulkden(*)
      real theta(0:*), thetas(*), thetaf(*), thetar(*), thetaw(*)
      real bhrsk(*), bheaep(*), bh0cb(*), bsfcla(*), bsfom(*), bhtsav(*)

!     + + + ARGUMENT DEFINITIONS + + +
!     daysim     - day of the simulation (very useful for debugging, not necessary otherwise)
!     bszlyt(*)  - thickness of the soil layer (mm)
!     bszlyd(*)  - depth to bottom of the soil layer (mm)
!     bulkden(*) - soil bulk density Mg/m^3)
!     theta(*)   - volumetric water content (m^3/m^3)
!     thetas(*)  - saturated volumetric water content (m^3/m^3)
!     thetaf(*)  - field capacity volumetric water content (m^3/m^3)
!     thetar(*)  - residual (conductivity) volumetric water content (m^3/m^3)
!     thetaw(*)  - wilting point volumetric water content (m^3/m^3)
!     bhrsk(*)   - saturated hydraulic conductivity (m/s)
!     bheaep(*)  - air entry potential (J/kg)
!     bh0cb(*)   - exponent of Campbell soil water release curve (unitless)
!     bsfcla(*)  - fraction of soil mineral content which is clay (unitless)
!     bsfom(*)   - fraction of total soil which is organic (unitless)
!     bhtsav(*)  - daily average soil temperature (C)

!     + + + LOCAL VARIABLES + + +
      integer    idx
      integer    idoy
      integer    year
      real       availwat, temp
      real       unsatcond, matricpot, soilrh
      real       laycenter, sat_rat
      real       airentry, lambda, theta80rh

!     + + + LOCAL DEFINITIONS + + +
!     idx   - array index for loops
!     availwat  - soil plant availale water content (for output)
!     unsatcond - unsaturated hydraulic conductivity (m/s) (for output)
!     matricpot - soil matric potential (m) (for output)
!     layercenter - depth to the center of a soil layer (mm) (for output)
!     sat_rat - saturation ratio (for output)

!     + + + END SPECIFICATIONS + + +

      idoy = get_psim_doy(isr)
      year = get_psim_year(isr)
      if( idoy .eq. 1 ) then
         ! insert double blank line to break years into blocks for graphing
         write(luohlayers(isr),*)
         write(luohlayers(isr),*)
      else
         ! print a single blank line to separate layer blocks
         write(luohlayers(isr),*)
      end if
      do idx=1,layrsn
         lambda = 1.0 / bh0cb(idx)
         availwat = availwc( theta(idx), thetaw(idx), thetaf(idx) )
         unsatcond = unsatcond_bc( theta(idx), thetar(idx), &
                     thetas(idx), bhrsk(idx), lambda )
         airentry = bheaep(idx) / gravconst
         temp = bulkden(idx)*1000.0  !convert Mg/m^3 to kg/m^3
         theta80rh = volwatadsorb( temp, bsfcla(idx), bsfom(idx), &
                     claygrav80rh, orggrav80rh )
         call matricpot_bc( theta(idx), thetar(idx), thetas(idx), &
              airentry, lambda, thetaw(idx), theta80rh, bhtsav(idx), &
              matricpot, soilrh )
         laycenter = bszlyd(idx) - 0.5*bszlyt(idx)
         sat_rat = (theta(idx)-thetar(idx)) / (thetas(idx)-thetar(idx))
 2190    format(1x,i5,1x,i3,1x,i4,1x,i3,1x,16g11.3)
         write(luohlayers(isr),2190) daysim, idoy, year, idx, laycenter, &
               theta(idx), thetas(idx), thetaf(idx), thetaw(idx), &
               thetar(idx), availwat, sat_rat, bhtsav(idx), &
               unsatcond, -matricpot, soilrh, bulkden(idx), &
               -airentry, bh0cb(idx), bhrsk(idx)
      end do

      return
    end subroutine printlayval

    real function plant_wat_t( begind, endd, thetaf, thetaw,          &
     &                           bszlyd, nlay )

!     + + + PURPOSE + + +
!     Determines the amount of water in any soil interval between any
!     two water contents.

      use soillay_mod, only: intersect

!     + + + ARGUMENT DECLARATIONS + + +
      real begind, endd
      integer nlay
      real thetaf(nlay), thetaw(nlay), bszlyd(nlay)

!     + + + ARGUMENT DEFINITIONS + + +
!     begind - uppper depth of soil interval
!     endd   - lower depth of soil interval
!     nlay   - number of layers in soil input array
!     thetaf - wetter soil water content value by layer (mm/mm)
!     thetaw - dryer soil water content value by layer (mm/mm)
!     bszlyd - depth to bottom of soil layers (mm)

!     + + + LOCAL VARIABLES + + +
      integer lay
      real sumwat, depth, thick

!     + + + LOCAL DEFINITIONS + + +
!     lay    - layer index
!     sumwat - running sum of water as added from each layer
!     depth  - cumulative depth in soil
!     prevdepth - previous cumulative depth in soil
!     thick  - thickness of soil slice whose water content is being
!              added to sum

!     + + + END SPECIFICATIONS + + +

      sumwat = 0.0
      depth = 0.0
      do lay = 1,nlay
          ! find thickness of intersection between soil layer and 
          ! desired interval
          thick = intersect( depth, bszlyd(lay), begind, endd )
          if( thick .gt. 0.0 ) then
              sumwat = sumwat + (thetaf(lay) - thetaw(lay)) * thick
          end if
          depth = bszlyd(lay)
      end do
      plant_wat_t = sumwat

      return
    end function plant_wat_t

    real function movewind( meas_wind, meas_za, meas_zo, meas_zd,     &
     &                          loc_za, loc_zo, loc_zd)

!     + + + PURPOSE + + +
      ! returns wind velocity in the same units as the measured wind 
      ! adjusted from measured height, roughness, zero plane displacement
      ! to the location height, roughness and zero plane displacement.
      ! Reference: Jensen, Burman, Allen. 1989. ASCE 70, Evapotranspiration
      ! and irrigation water requirements. Adjustment for differential roughness
      ! included as a power function (Hagen reference: Panofsky and Dutton, 1984)

!     + + + KEY WORDS + + +
!     log law wind velocity adjustment

!     + + + COMMON BLOCKS + + +

!     + + + LOCAL COMMON BLOCKS + + +

!     + + + ARGUMENT DECLARATIONS + + +
      real meas_wind, meas_za, meas_zo, meas_zd
      real loc_za, loc_zo, loc_zd

!     + + + ARGUMENT DEFINITIONS + + +
!     meas_wind - measured wind velocity (units same as output units)
      ! these parameters should all have the same units
!     meas_za - measured wind anemometer height
!     meas_zo - measured wind aerodynamic roughness
!     meas_zd - measured wind zero plane displacement
!     loc_za - location wind velocity height
!     loc_zo - location wind aerodynamic roughness
!     loc_zd - location wind zero plane displacement

!     + + + PARAMETERS + + +

!     + + + LOCAL VARIABLES + + +

!     + + + LOCAL DEFINITIONS + + +

!     + + +   FUNCTION CALLS +++

!     + + + END SPECIFICATIONS + + +

      movewind = meas_wind * ( log( (loc_za - loc_zd) / loc_zo )        &
     &         / log( (meas_za - meas_zd) / meas_zo ) )                 &
     &         * ( (loc_zo / meas_zo)**0.067 )

      return
    end function movewind

    subroutine psd (sandm, siltm, claym, pgmd, pgsd)

!     + + + PURPOSE + + +
!     This subroutine calculates the soil geometric mean diameter and
!     geometric standard deviation from percent sand, silt, and clay
!     using geometric mean diameters within each of the three soil
!     particle size fractions.
!     From: Shirazi, M.A., Boersma, L. 1984. A unifying Quantitative
!     Analysis of Soil Texture. SOil Sci. Soc. Am. J. 48:142-147
!     DATE:  09/29/93

!     + + + KEY WORDS + + +
!     Geometric mean diameter (GMD), Geometric standard deviation (GSD)

!     + + + ARGUMENT DECLARATIONS + + +
      real claym
      real pgmd
      real pgsd
      real sandm
      real siltm

!     + + + ARGUMENT DEFINITIONS + + +
!     sandm  - Mass fraction of sand
!     siltm   - Mass fraction of silt
!     claym  - Mass fraction of clay
!     pgmd   - Geometric mean diameter of psd (mm)
!     pgsd   - Geometric std deviation of psd (mm)

!     + + + PARAMETERS + + +

      real   sandg, siltg, clayg

      parameter   (sandg = 1.025, siltg = 0.026, clayg = 0.001)

!     sandg - percent sand
!     siltg - percent silt
!     clayg - percent clay

!     + + + LOCAL VARIABLES + + +
      real a, b

!     + + + LOCAL DEFINITIONS + + +
!     a, b   - Temporary variables

!     + + + END SPECIFICATIONS + + +

!     calculate geometric mean diameter
          a = sandm*log(sandg)+siltm*log(siltg)+claym*log(clayg)
          pgmd = exp(a)

!     calculate geometric standard deviation
          b = (sandm*log(sandg)**2 + siltm*log(siltg)**2 +               &
     &         claym*log(clayg)**2)
          pgsd = exp(sqrt(b-a**2))

      return
    end subroutine psd

    subroutine ratedura(bhzirr, bhratirr, bhdurirr)

!     + + + PURPOSE + + +
!     makes sure that irrigation depth, application rate and duration
!     are consistent. This routine always requires an irrigation depth
!     to be set on entry, although it can be safely be zero.

!     + + + ARGUMENT DECLARATIONS + + +

      real bhzirr, bhratirr, bhdurirr

!     + + + ARGUMENT DEFINITIONS + + +

!     bhzirr    - daily irrigation application depth (mm)
!     bhratirr  - characteristic system irrigation rate (mm/hour)
!     bhdurirr  - irrigation application duration (hours)

!     + + + END SPECIFICATIONS + + +

      if( bhratirr .gt. 0.0 ) then
          ! irrigation characteristic rate known, so find duration
          ! to apply the given depth
          bhdurirr = min( 24.0, bhzirr / bhratirr )
      else if( bhdurirr .gt. 0.0 ) then
          ! irrigation duration known, rate not known, so find the characteristic rate
          bhratirr = bhzirr / bhdurirr
      else
          ! neither rate nor duration known, so apply over 24 hours
          bhdurirr = 24.0
          bhratirr = bhzirr / bhdurirr
      end if

      return
    end subroutine ratedura

    real function waterk (bd, cb, clay, silt)
!
!     + + + purpose + + +
!     this function estimates soil saturated hydraulic conductivity
!     if it is not readily available.  the function predicts saturated
!     hydraulic conductivity as a function of soil particle size dis-
!     tribution and bulk density (eq. 6.12a, p. 54)
!     reference:  campbell, g.s. 1985. soil physics with basic: trans-
!                 port models for soil-plant systems.  elsevier science
!                 publishers b.v.  amsterdam, the netherlands.
!
!     + + + argument declaration + + +
      real bd
      real cb
      real clay
      real silt
!
!     + + + argument definitions + + +
!     bd     - soil bulk density (Mg/m^3)
!     cb     - soil pore size scaling exponent
!     clay   - clay fraction
!     silt   - silt fraction
!     waterk - saturated hydraulic conductivity (m/s)

!     + + + end specifications + + +
!
      waterk = 3.92e-5*((1.3/bd)**(1.3*cb))*exp((-6.9*clay)-(3.7*silt))
!
      return
    end function waterk

end module hydro_main_mod

