!$Author$
!$Date$
!$Revision$
!$HeadURL$
      subroutine heat(isr, layrsn, bszlyd, bszlyt, theta, thetas,       &
     &                bsfsan, bsfsil, bsfcla, bsfom, bsdblk,            &
     &                bwtdmn, bwtdmx, bwtyav, rad_net, bdmres,          &
     &                bhtsmn, bhtsmx, bhtsav, bhfice,                   &
     &                bhzsno, bhtsno, bhfsnfrz, bhzsnd,                 &
     &                bhzsmt, soil_heat_flux )

!     + + + PURPOSE + + +
!     This program simulates daily soil temperature based on a daily heat
!     balance. 
!     soil temperature based on the algorithm
!     described by campbell g. s. (1985) chapter 4: soil temperature
!     and heat flux. p. 26-39. in soil physics with basic.
!     The program estimates daily minimum, maximum, and average soil
!     temperature at the center of each simulation layer.
!     The inputs needed to run the program are maximum daily air
!     temperature, and minimum daily air temperature.  Furthermore,
!     soil bulk density, volumetric water content, and clay
!     fraction are used to calculate soil thermal properties.
!     DATE:  10/05/93

!     + + + KEY WORDS + + +
!     soil temperature

!     + + + COMMON BLOCKS + + +

      use weps_main_mod, only: am0ifl
      use weps_interface_defs, only: heatcond, heatcap, snowcond
      use weps_interface_defs, only: drainsnow
      use file_io_mod, only: luotempsoil
      use datetime_mod, only: get_simdate
      use p1unconv_mod, only: mmtom, pi, SEC_PER_DAY
      use hydro_data_struct_defs, only: am0hfl

!     + + + LOCAL COMMON BLOCKS + + +
      include 'hydro/snowprop.inc'

!     + + + ARGUMENT DECLARATIONS + + +
      integer, intent(in) :: isr   ! subregion number
      integer layrsn
      real bszlyd(*), bszlyt(*), theta(0:*), thetas(*)
      real bsfsan(*), bsfsil(*), bsfcla(*), bsfom(*), bsdblk(*)
      real bwtdmn, bwtdmx, bwtyav, rad_net, bdmres
      real bhtsmn(*), bhtsmx(*), bhtsav(*), bhfice(*)
      real bhzsno, bhtsno, bhfsnfrz, bhzsnd
      real bhzsmt, soil_heat_flux

!     + + + ARGUMENT DEFINITIONS + + +
!     layrsn  - Number of soil layers used in the simulation
!     bszlyd  - distance from surface to bottom of layer (mm)
!     bszlyt  - Layer thickness (mm)
!     theta   - volumetric soil water content
!     thetas  - volumetric saturated soil water content
!     bsfsan  - sand fraction of mineral soil content by layer (Mg sand/Mg soil mineral)
!     bsfsil  - silt fraction of mineral soil content by layer (Mg silt/Mg soil mineral)
!     bsfcla  - clay fraction of mineral soil content by layer (Mg clay/Mg soil mineral)
!     bsfom   - organic matter fraction of soil content by layer (Mg organic/Mg soil solids)
!     bsdblk  - Soil bulk density by layer (Mg/m^3)
!     bwtdmn  - Daily minimum air temperature (C)
!     bwtdmx  - Daily maximum air temperature (C)
!     bwtyav  - Average yearly air temperature (deg C)
!     rad_net - net radiation onto surface (Mj/m^2/day)
!     bdmres  - plant residue on the soil surface (kg/m^2)
!     bhtsmn  - Daily minimum soil temperature by layer (C)
!     bhtsmx  - Daily maximum soil temperature by layer (C)
!     bhfice  - fraction of water in soil layer which is frozen
!     bhzsno  - depth of water contained in snow layer (mm)
!     bhtsno  - temperature of snow layer (C)
!     bhfsnfrz  - fraction of snow layer water content which is frozen
!     bhzsnd  - actual thickness of snow layer (mm)
!     bhzsmt  - depth of water melted and discharged from snow layer (mm)
!     soil_heat_flux - soil heat flux estimated from top layer temperature
!                      used in pot et calc (Mj/m^2/day)

!     + + + PARAMETERS + + +
      real con_temp_depth
      parameter(con_temp_depth = 5.0)
!     con_temp_depth - depth in the soil at which soil temperature is assumed
!                      to equal average annual temperature

!     + + + LOCAL VARIABLES + + +
      integer lay, day, mo, yr, bly, loopcnt
      real vsheat, thermk, zdamp, freq
!      real time
      real t_air, t_surf_end, delta_t, delta_f
      real tamp, dmlayr(layrsn)
      real thermt_up, thermt_dn
      real thermk_up, thermk_dn
      real res_depth
      real nt_sno, nf_sno, nt_lay(layrsn), nf_ice(layrsn)
      real heat_cap_thaw, heat_cap_froz
      real rad_val, soil_val

!     + + + LOCAL DEFINITIONS + + +
!     lay    - soil layer index
!     bly    - soil layer index + 1
!     loopcnt - relaxation loop counter
!     vsheat - average total volumetric heat capacity
!     thermk - average total thermal conductivity (J/(s m C))
!     zdamp  - Diurnal damping depth, m
!     freq   - daily oscillation frequency
!     time   - time of day (seconds)
!     t_air  - daily average air temperature (C)
!     t_surf_end - ending temperature of the soil surface (C)
!     delta_t - temperature change observed as energy balance solution is relaxed (C)
!     delta_f - frozen fraction change observed as energy balance solution is relaxed (C)
!     thermt_up - heat transfer coefficient at upper boundary of layer (J/(s m^2 C))
!     thermt_dn - heat transfer coefficient at lower boundary of layer (J/(s m^2 C))
!     thermk_up - thermal conductivity of layer above boundary (J/(s m C))
!     thermk_dn - thermal conductivity of layer below boundary (J/(s m C))
!     res_depth - residue depth found from residue mass (mm)
!     nt_sno - new average temperature of the snow layer (C)
!     nf_sno - new fraction of snow water content which is frozen
!     nt_lay - new average temperature of the soil layers (C)
!     nf_lay - new fraction of soil water content which is frozen
!     heat_cap_thaw - layer heat capacity in a thawed condition (J/(m^3 C))
!     heat_cap_froz - layer heat capacity in a thawed condition (J/(m^3 C))
!     rad_val - radiation value actually passed to energy balance routine
!     soil_val - soil heat flux value returned from energy balance routine

!     + + + FUNCTIONS CALLED + + +
!      real heatcond
!      real heatcap
!      real snowcond

!     + + + SUBROUTINES CALLED + + +
!     stat
!     drainsnow

!     + + + DATA INITIALIZATIONS + + +
!      phi = -(7. * pi) / 12.

!     initialize various soil layer references

      dmlayr(1) = 0.5 * bszlyd(1) * mmtom
      do lay=2, layrsn
          dmlayr(lay) = 0.5 * mmtom * (bszlyd(lay-1) + bszlyd(lay))
      end do

!     + + + OUTPUT FORMATS + + +
2009  format ('# Soil temperature (celsius), number of layers = ', i4)
2010  format ('# dy mo year minL1 maxL1 minL2 maxL2  ......')
2040  format (1x,2(i2,1x),i4,200(1x,f5.1))

!     + + + END SPECIFICATIONS + + +

      if( (am0ifl .eqv. .true.) .and. ((am0hfl(isr) .eq. 4)             &
     &  .or. (am0hfl(isr) .eq. 5) .or. (am0hfl(isr) .eq. 6)             &
     &  .or. (am0hfl(isr) .eq. 7)) ) then
         write(luotempsoil(isr),2009) layrsn
         write(luotempsoil(isr),2010)
      end if

      ! calculate simple (explicit) soil heat balance on a daily basis
      ! to find daily average soil temperature for layers

      ! initialize new heat state values to old values
      nt_sno = bhtsno
      nf_sno = bhfsnfrz
      do lay = 1,layrsn
          nt_lay(lay) = bhtsav(lay)
          nf_ice(lay) = bhfice(lay)
      end do

      t_air = 0.5 * (bwtdmn + bwtdmx)

      ! set up loop for relaxation to answer
      delta_t = 100.0
      loopcnt = 1
      do while( (delta_t .gt. 0.01) .and. (loopcnt .lt. 50) )

      ! energy balance change tracking, initial value
      delta_t = nt_lay(1)
      delta_f = nf_ice(1)

      ! thermal conductivity of top soil layer
      bly = 1
      thermk_dn = heatcond(bsdblk(bly), theta(bly), thetas(bly),        &
     &            bhtsav(bly), bhfice(bly), bsfsan(bly), bsfsil(bly),   &
     &            bsfcla(bly), bsfom(bly))

      ! set net radiation on surface (drops through to soil surface if no snow)
      rad_val = rad_net
      ! start with surface layer of snow (or lack thereof)
      if( bhzsno .gt. 0.0 ) then
          ! snow is present - no residue effect
          ! snow density, passed to snowcond is calculated
          ! using units mass = mm * (1m/1000mm) * 1000 kg/m^3 = kg/m^2
          ! density kg/m^2 / (mm /1000mm/m) = kg/m^3
          ! flux units [J/(s m C)] * C / (mm * mmtom) = J/(s m^2)
          thermk_up = snowcond(1000.0*bhzsno/bhzsnd)
          thermt_up = thermk_up / (mmtom*bhzsnd/2.0)
          thermt_dn = 2.0 * thermk_up * thermk_dn                       &
     &              / (mmtom*(bhzsnd*thermk_dn + bszlyt(bly)*thermk_up))
          heat_cap_thaw = heatcap(1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0 )
          heat_cap_froz = heatcap(1.0, 1.0, 1.0, 0.0, 0.0, 0.0, 0.0 )
          call energy_bal(bhtsno, nt_sno, bhfsnfrz, nf_sno,             &
     &         t_air, nt_lay(1), thermt_up, thermt_dn,                  &
     &         heat_cap_thaw, heat_cap_froz, 1.0, bhzsno,               &
     &         SEC_PER_DAY, rad_val, soil_heat_flux )
          t_surf_end = nt_sno

          ! clear surface radiation since it is absorbed by snow already
          rad_val = 0.0
      else
          ! no snow is present, do surface layer calculations based on dry
          ! residue and assume no heat capacity for residue layer
          ! thermal conductivity of residue
          ! Using total residue not including living crop for first approximation
          call res_cond( bdmres, thermk, res_depth )

          ! take geometric average of layer and residue thermal conductivity
          thermk_dn = 1.0                                               &
     &              / ( (0.5*bszlyt(bly)/thermk_dn + res_depth/thermk)  &
     &              / (0.5*bszlyt(bly) + res_depth) )

          thermt_dn = thermk_dn / (mmtom*(0.5*bszlyt(bly) + res_depth))
          t_surf_end = t_air
      end if

      ! continue with soil layers
      do lay = 1,layrsn-1
          ! values previously calculated for upper interface as we move down
          thermk_up = thermk_dn
          thermt_up = thermt_dn

          ! using layer below, set property values for lower interface
          bly = lay + 1
          thermk_dn = heatcond(bsdblk(bly), theta(bly), thetas(bly),    &
     &             bhtsav(bly), bhfice(bly), bsfsan(bly), bsfsil(bly),  &
     &             bsfcla(bly), bsfom(bly))
          thermt_dn = 2.0 * thermk_up * thermk_dn                       &
     &             / (mmtom*(bszlyt(lay)*thermk_dn                      &
     &             + bszlyt(bly)*thermk_up))
          heat_cap_thaw = heatcap(bsdblk(lay), theta(lay), 0.0,         &
     &             bsfsan(lay), bsfsil(lay), bsfcla(lay), bsfom(lay))
          heat_cap_froz = heatcap(bsdblk(lay), theta(lay), 1.0,         &
     &             bsfsan(lay), bsfsil(lay), bsfcla(lay), bsfom(lay))
          call energy_bal(bhtsav(lay), nt_lay(lay), bhfice(lay),        &
     &         nf_ice(lay),                                             &
     &         t_surf_end, nt_lay(bly), thermt_up, thermt_dn,           &
     &         heat_cap_thaw, heat_cap_froz, theta(lay), bszlyt(lay),   &
     &         SEC_PER_DAY, rad_val, soil_val )

          if( lay .eq. 1 ) then
              ! for surface layer
              ! clear net radiation, absorbed already by surface energy balance
              rad_val = 0.0
              ! recover soil surface heat flux
              if( bhzsno .le. 0.0 ) then
                  ! recovered only if no snow, otherwise set in snow calc
                  soil_heat_flux = soil_val
              end if
          end if          

          ! set layer above temperatures for next layer calculation
          t_surf_end = nt_lay(lay)
      end do

      ! bottom layer use annual average soil temperature at large distance
      ! use thermal conductivity of bottom layer
      lay = layrsn
      thermt_up = thermt_dn
      thermt_dn = 2.0 * thermk_dn / ( mmtom*0.5*bszlyt(lay)             &
     &         + max(0.0, con_temp_depth - mmtom*bszlyd(lay)) )
      heat_cap_thaw = heatcap(bsdblk(lay), theta(lay), 0.0,             &
     &                bsfsan(lay), bsfsil(lay), bsfcla(lay), bsfom(lay))
      heat_cap_froz = heatcap(bsdblk(lay), theta(lay), 1.0,             &
     &                bsfsan(lay), bsfsil(lay), bsfcla(lay), bsfom(lay))
      call energy_bal(bhtsav(lay),nt_lay(lay), bhfice(lay), nf_ice(lay),&
     &         t_surf_end, bwtyav, thermt_up, thermt_dn,                &
     &         heat_cap_thaw, heat_cap_froz, theta(lay), bszlyt(lay),   &
     &         SEC_PER_DAY, rad_val, soil_val )

      delta_t = max(abs(delta_t-nt_lay(1)), 10.0*abs(delta_f-nf_ice(1)))
      loopcnt = loopcnt + 1

      end do
      ! end of loop for relaxation to answer

      ! adjust snow depth
      if( bhzsno .gt. 0.0 ) then
          call setlsnow(bhzsno, bhfsnfrz, nf_sno, bhzsnd, nt_sno,bwtdmx)
      end if

          ! adjust snow depth based on change in frozen water content fraction
          ! never increase depth here. If maximum air temperature is above
          ! freezing density will increase beyond what may occur using daily
          ! average temperature. Decrease 1% for every degree
!          bhzsnd = bhzsnd*min(1.0,min(1.0-bwtdmx/100, nf_sno/bhfsnfrz))
          ! add an increase due to compaction

      ! check for density and drain water if above the drainage density
      call drainsnow( bhzsmt, bhzsno, nf_sno, bhzsnd )

      ! completed daily average temperature solution, assign
      bhtsno = min( 0.0, nt_sno )
      bhfsnfrz = nf_sno

      do lay=1,layrsn
          bhtsav(lay) = nt_lay(lay)
          bhfice(lay) = nf_ice(lay)
      end do

      ! calculate weighted soil heat capacity and thermal conductivity
      vsheat = 0.0
      thermk = 0.0

      do lay=1,layrsn
          vsheat = vsheat + heatcap(bsdblk(lay),theta(lay), bhfice(lay),&
     &             bsfsan(lay), bsfsil(lay), bsfcla(lay), bsfom(lay))   &
     &           * bszlyt(lay)
          thermk = thermk                                               &
     &           + heatcond(bsdblk(lay), theta(lay), thetas(lay),       &
     &             bhtsav(lay), bhfice(lay), bsfsan(lay), bsfsil(lay),  &
     &             bsfcla(lay), bsfom(lay)) * bszlyt(lay)
      end do

      vsheat = vsheat / bszlyd(layrsn)
      thermk = thermk / bszlyd(layrsn)

      ! calculate angular frequency of the temperature oscillation
      freq = (2*pi)/SEC_PER_DAY

      ! calculate diurnal damping depth
      zdamp= sqrt((2*(thermk/vsheat))/freq)

!     begin simulating heat flow for each day of the simulation period
!     calculate mean temperature and amplitude temperature at the
!     soil-atmosphere interface  ( upper boundary condition )
!     set lower boundary condition to an average air temperature

      tamp = bwtdmx - t_air

      do lay=1,layrsn
!          do  j = 1,24
!            time = j * 3600
!            tsoil(lay,j)= bhtsav(lay)+tamp*exp(-dmlayr(lay)/zdamp)*     &
!     &                  sin((freq*time)-(dmlayr(lay)/zdamp)+phi)
!           end do

!!        use subroutine stat to calculate the daily maximum,
!         and minimum temperatures at the center of each simulation
!         layer
!          call statt(tsoil,bhtsmx(k),bhtsmn(k),k, layrsn, hrday)

          bhtsmx(lay) = bhtsav(lay)                                     &
     &                + tamp * max(0.0, (1.0 - dmlayr(lay)/zdamp))
          bhtsmn(lay) = bhtsav(lay)                                     &
     &                - tamp * max(0.0, (1.0 - dmlayr(lay)/zdamp))

      end do

      if( (am0hfl(isr) .eq. 4) .or. (am0hfl(isr) .eq. 5)                &
     &   .or. (am0hfl(isr) .eq. 6) .or. (am0hfl(isr) .eq. 7)) then
         call get_simdate (day,mo,yr)
         write(luotempsoil(isr),2040) day, mo, yr,                      &
     &        (bhtsmn(lay), bhtsmx(lay), lay=1,layrsn)
      end if

      return
      end

      subroutine   statt (tsoil, bhtsmx, bhtsmn, k, nlay, hrday)

!     This subroutine uses the simulated hourly soil temperature
!     values to calculate the daily maximum, and minimum soil
!     temperatures at the center of each simulation layer

!     + + + COMMON BLOCKS + + +

!     + + + ARGUMENT DECLARATIONS + + +
      integer k, j, nlay, hrday
      real tsoil(nlay, hrday)
      real bhtsmn
      real bhtsmx

!     + + + ARGUMENT DEFINITIONS + + +
!     k      - Simulation layer counter
!     tsoil  - Hourly soil temperature, c
!     bhtsmn   - Daily minimum soil temperature, c
!     bhtsmx   - Daily maximum soil temperature, c

!     + + + END SPECIFICATIONS + + +

      bhtsmx= tsoil(k,1)
      bhtsmn= tsoil(k,1)
      do  100 j=1,24
        if (tsoil(k,j) .gt. bhtsmx)  bhtsmx= tsoil(k,j)
        if (tsoil(k,j) .lt. bhtsmn)  bhtsmn= tsoil(k,j)
 100  continue

      return
      end

      real function snowcond( snow_den )

!     returns the average thermal conductivity of the snow layer (J/(s m C))

!     + + + ARGUMENT DECLARATIONS + + +
      real snow_den

!     + + + ARGUMENT DEFINITIONS + + +
!     snow_den - snow density (kg/m^3)

!     + + + END SPECIFICATIONS + + +

      ! density calculation taken from: Alaska Lake Ice and Snow Observatory Network
      ! Martin Jeffries, Geophysical Institute, University of Alaska Fairbanks 
      ! 903 Koyukuk Dr., P.O. Box 757320 
      ! Fairbanks, AK 99775-7320 
      ! e-mail: martin.jeffries@gi.alaska.edu, tel: 907.474.5257, fax: 907.474.7290
      ! http://www.gi.alaska.edu/alison/measurements.html

      ! if( snow_den < 0.156 g cm-3, then keff = 0.023 + 0.234 * snow_den 
      ! If 0.156 = snow_den = 0.6 g cm-3 , then keff = 0.138 - 1.01*snow_den + 3.233 * snow_den **2 
      ! keff is expressed as W m-1 K-1(W/m/K, Watts per metre per Kelvin) 

      ! conversion of snow_den to kg/m^3 results in:
      ! note this works since maximum density is held to 522.0
      ! curve was noted to cross twice, so break point was moved to second crossing.
      ! thermal conductivity approaches that of liquid water, not ice in upper limit

      if( snow_den .lt. 230.3858 ) then
          snowcond = 0.023 + 0.234e-3 * snow_den
      else
          snowcond = 0.138 - 1.01e-3*snow_den + 3.233e-6*snow_den**2
      end if

      ! try SHAW conductivity taken from Anderson, 1976
      snowcond = 0.021 + 2.51 * (snow_den/1000.0)**2.0

      return
      end

      subroutine res_cond( flat_mass, thermk, thickness )

!     returns the average thermal conductivity of the residue layer (J/(s m C))
!     and the thickness of the layer (mm)

!     + + + ARGUMENT DECLARATIONS + + +
      real flat_mass, thermk, thickness

!     + + + ARGUMENT DEFINITIONS + + +
!     flat_mass - flat residue mass (kg/m^2)
!     thermk - apparent thermal conductivity of the residue layer (J/(s m C))
!     thickness - soil layer thickness (mm)

!     + + + PARAMETERS + + +
      real res_bd
      real res_t_cond

      parameter( res_bd = 13 )
      parameter( res_t_cond = 0.22 )

!     res_bd - residue bulk density (kg/m^3)
!     res_t_cond - base thermal conductivity of residue material (J/(s m C))

!     + + + END SPECIFICATIONS + + +

      ! based on : S.J. van Donk, and E.W. Tollner. 2000. Apparent thermal conductivity
      ! of mulch materials exposed to forced convection. Transactions of the ASAE
      ! Vol. 43(5):1117-1127

      ! initially used single residue bulk density and no wind velocity function

      ! units: mtomm * kg/m^2 / kg/m^3 = mm
      thickness = flat_mass / res_bd

      ! units
      thermk = res_t_cond

      return
      end

      subroutine energy_bal(tlay_beg, tlay_end, froz_beg, froz_end,     &
     &           tup_end, tdn_end, thermt_up, thermt_dn,                &
     &           heat_cap_thaw, heat_cap_froz, vol_wat, lay_thick,      &
     &           time_step, rad_net, soil_heat_flux )

!     returns the energy balance of a single layer computing the new layer
!     temperature (C) and water frozen fraction. Net radiation is set by the
!     calling routine to simulate a surface layer exposed to the sun. Similarly
!     the soil heat flux is returned in case it is need to describe a surface
!     effect (enters into the ET calculation)

      use p1unconv_mod, only: SEC_PER_DAY, mmtom
      use precision_mod, only: max_real

!     + + + LOCAL COMMON BLOCKS + + +
      include 'hydro/snowprop.inc'

!     + + + ARGUMENT DECLARATIONS + + +
      real tlay_beg, tlay_end, froz_beg, froz_end
      real tup_end, tdn_end, thermt_up, thermt_dn
      real heat_cap_thaw, heat_cap_froz, vol_wat, lay_thick
      real time_step, rad_net, soil_heat_flux

!     + + + ARGUMENT DEFINITIONS + + +
!     tlay_beg - layer temperature at beginning of time step (C)
!     tlay_end - layer temperature at end of time step (C)
!     froz_beg - fraction of layer water content which is frozen at beginning of time step
!     froz_end - fraction of layer water content which is frozen at end of time step
!     tup_end - layer above temperature at end of time step (C)
!     tdn_end - layer below temperature at end of time step (C)
!     thermt_up - thermal transfer coeff from layer above (J/(m^2 s C))
!     thermt_dn - thermal transfer coeff from layer below (J/(m^2 s C))
!     heat_cap_thaw - layer heat capacity in a thawed condition (J/(m^3 C))
!     heat_cap_froz - layer heat capacity in a thawed condition (J/(m^3 C))
!     vol_wat - layer volumetric water content (m^3 wat/m^3 lay)
!     lay_thick - layer thickness (mm)
!     time_step - time duration that energy balance will be applied (seconds)
!     rad_net - daily soil (snow) surface net radiation (Mj/m^2/day)
!     soil_heat_flux - daily ground heat flux (soil surface) (Mj/m^2/day)

!     + + + FUNCTIONS CALLED + + +
      real diff_heat

!     + + + LOCAL VARIABLES + + +
      real time_brk_1, time_brk_2, diff_heat_0, heat_cap_0, heat_vol_0
      real rate_rad_net, rate_soil_heat

!     + + + LOCAL DEFINITIONS + + +
!     time_brk_1 - time to reach first change of state boundary (seconds)
!                  temperature change to begin state change
!     time_brk_2 - time to reach second change of state boundary (seconds)
!                  complete state change and restart temperature change
!     diff_heat_0 - heat transfer rate given zero degree temperatures (J/(m^2 s))
!     heat_cap_0  - heat capacity of the soil layer based on temperature (J/(m^3 C))
!     heat_vol_0  - heat volume available for phase change of water in layer (J/kg/mm)
!     rate_rad_net - net radiation exchange rate (J/(m^2 s))
!     rate_soil_heat - soil surface heat exchange rate (J/(m^2 s))


!     + + + END SPECIFICATIONS + + +

      ! convert daily net radiation to rate
      ! Mj/m^2/day * 1000 j/Mj / 86400 sec/day
      rate_rad_net = rad_net * 1000.0 / SEC_PER_DAY
      
      ! set layer heat capacity
      if( froz_beg .lt. 0.5 ) then
          ! liquid water heat capacity
          heat_cap_0 = heat_cap_thaw
      else
          ! frozen water heat capacity
          heat_cap_0 = heat_cap_froz
      end if

      ! check for layer status
      if( (froz_beg .gt. 0.0) .and. (froz_beg .lt. 1.0) ) then
          ! layer water content partially frozen
          ! set beginning value for time to the first break
          time_brk_1 = 0.0
      else
          ! layer water either fully liquid or fully frozen
          ! units denom: s * J/(m^2 s C) / (J/(m^3 C) * mm * m/1000mm)
          diff_heat_0 = rate_rad_net + diff_heat(0.0, tup_end, tdn_end, &
     &                            thermt_up, thermt_dn)
          tlay_end = ( tlay_beg + time_step * diff_heat_0               &
     &             / (heat_cap_0 * lay_thick * mmtom) )                 &
     &             / ( 1.0 + time_step * (thermt_up + thermt_dn)        &
     &             / (heat_cap_0 * lay_thick * mmtom) )

          if( (tlay_beg * tlay_end .lt. 0.0) ) then
              ! crossing T=0, find time to reach T=0
              time_brk_1 = - tlay_beg * heat_cap_0 * lay_thick * mmtom  &
     &                   / diff_heat_0
          else if( ((froz_beg .eq. 1.0) .and. (tlay_end .gt. 0.0)) .or. &
     &        ((froz_beg .eq. 0.0) .and. (tlay_end .lt. 0.0)) ) then
              ! layer is fully frozen or thawed, at zero degrees and
              ! will be changing phase, not temperature
              time_brk_1 = 0.0
          else
              ! temperature change complete
              time_brk_1 = time_step
          end if
      end if

      if( time_brk_1 .lt. time_step ) then
          ! layer being driven across the freezing or thawing process

          ! calculate time to fully frozen or fully thawed condition
          ! units: (m^3 wat/m^3 lay) * (J/kg wat) * mm *(1000 kg wat/m^3 wat)
          ! * (1m/1000mm) / (J/m^2 s) = seconds (1000 factors cancel)
          diff_heat_0 = rate_rad_net + diff_heat(0.0, tup_end, tdn_end, &
     &                            thermt_up, thermt_dn)
          
          ! broke out the subcalculation to facilitate testing for numerical instability
          ! = (mm/mm)*(J/kg)(mm)
          heat_vol_0 = vol_wat * heat_fusion * lay_thick
          if( diff_heat_0 .gt. froz_beg * heat_vol_0 / max_real) then
              ! thawing cycle
              time_brk_2 = time_brk_1 + froz_beg                        &
     &                   * heat_vol_0 / diff_heat_0
              froz_end = 0.0
          else if(diff_heat_0.lt.(froz_beg-1.0)*heat_vol_0/max_real)then
              ! freezing cycle
              time_brk_2 = time_brk_1 + (froz_beg - 1.0)                &
     &                   * heat_vol_0 / diff_heat_0
              froz_end = 1.0
          else
              ! no change
              time_brk_2 = time_step + 1.0
          end if
    
          if( time_brk_2 .ge. time_step ) then
              ! incomplete freezing or thawing, find new water frozen fraction
              tlay_end = 0.0
              froz_end = froz_beg - diff_heat_0*(time_step - time_brk_1)&
     &                  / (vol_wat * heat_fusion * lay_thick)
          end if
      else
          ! temperature change complete
          time_brk_2 = time_step
      end if

      if( time_brk_2 .lt. time_step ) then
          ! state change complete, continue with layer temperature change

          ! set layer heat capacity
          if( froz_end .lt. 0.5 ) then
              ! liquid water heat capacity
              heat_cap_0 = heat_cap_thaw
          else
              ! frozen water heat capacity
              heat_cap_0 = heat_cap_froz
          end if

          ! layer water either fully liquid or fully frozen
          diff_heat_0 = rate_rad_net + diff_heat(0.0, tup_end, tdn_end, &
     &                            thermt_up, thermt_dn)
          tlay_end = ( (time_step - time_brk_2) * diff_heat_0           &
     &             / (heat_cap_0 * lay_thick * mmtom) )                 &
     &             / ( 1.0 + (time_step - time_brk_2)                   &
     &             * (thermt_up + thermt_dn)                            &
     &             / (heat_cap_0 * lay_thick * mmtom) )

      end if

      ! find soil heat flux (heat transfer on upper side of soil layer)
      ! provided as information if needed as in soil surface heat flux
      rate_soil_heat = thermt_up * (tup_end - tlay_end)
      ! units (J/m^2/s * SEC_PER_DAY / 1000 j/Mj)
      soil_heat_flux = rate_soil_heat * SEC_PER_DAY / 1000.0 

      return
      end

      real function diff_heat(tlay_end, tup_end, tdn_end,               &
     &                        thermt_up, thermt_dn )

!     returns differential heat transfer rate (J/(m^2 s))

!     + + + ARGUMENT DECLARATIONS + + +
      real tlay_end, tup_end, tdn_end
      real thermt_up, thermt_dn

!     + + + ARGUMENT DEFINITIONS + + +
!     tlay_end - layer temperature at end of time step (C)
!     tup_end - layer above temperature at end of time step (C)
!     tdn_end - layer below temperature at end of time step (C)
!     thermt_up - thermal transfer coeff from layer above (J/(s C))
!     thermt_dn - thermal transfer coeff from layer below (J/(s C))

!     + + + END SPECIFICATIONS + + +

      diff_heat = thermt_up * (tup_end - tlay_end)                      &
     &          - thermt_dn * (tlay_end - tdn_end)

      return
      end
