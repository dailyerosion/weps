!$Author$
!$Date$
!$Revision$
!$HeadURL$
module hydro_heat_mod

    real, parameter :: min_snow_den = 50.0    ! minimum density of new snow (kg/m^3)
    real, parameter :: melt_snow_den = 350.0  ! snow density above which melt water is released (kg/m^3)
    real, parameter :: max_snow_den = 522.0   ! maximum possible snow density (kg/m^3)
    real, parameter :: heat_fusion = 337000.0 ! latent heat of fusion for water (J/kg)

    ! values from Kluitenberg
    real, parameter :: sandheatcap = 730.0   ! heat capacity of sand (J/kg C)
    real, parameter :: siltheatcap = 730.0   ! heat capacity of silt (J/kg C)
    real, parameter :: clayheatcap = 730.0   ! heat capacity of clay (J/kg C)
    real, parameter :: waterheatcap = 4180.0 ! heat capacity of water (J/kg C)
    real, parameter :: organheatcap = 1900.0 ! heat capacity of organic matter (J/kg C)

    ! value from deVries and from Incropera and DeWit
    real, parameter :: iceheatcap = 2040.0   ! heat capacity of ice (J/kg C)

    ! values from Bristow in Dane and Topp (2002)
    real, parameter :: quartzheatcond = 8.8   ! thermal conductivity of quartz (J/s m C) or (W/m C)
    real, parameter :: mineralheatcond = 2.9  ! thermal conductivity of soil minerals (J/s m C) or (W/m C)
    real, parameter :: iceheatcond = 2.18     ! thermal conductivity of ice (J/s m C) or (W/m C)
    real, parameter :: organicheatcond = 0.25 ! thermal conductivity of organic matter (J/s m C) or (W/m C)

  contains

    subroutine heat(isr, layrsn, bszlyd, bszlyt, theta, thetas, &
                      bsfsan, bsfsil, bsfcla, bsfom, bsdblk, &
                      bwtdmn, bwtdmx, bwtyav, rad_net, bdmres, &
                      bhtsmn, bhtsmx, bhtsav, bhfice, &
                      bhzsno, bhtsno, bhfsnfrz, bhzsnd, &
                      bhzsmt, soil_heat_flux )

      ! + + + PURPOSE + + +
      ! This program simulates daily soil temperature based on a daily heat
      ! balance. 
      ! soil temperature based on the algorithm
      ! described by campbell g. s. (1985) chapter 4: soil temperature
      ! and heat flux. p. 26-39. in soil physics with basic.
      ! The program estimates daily minimum, maximum, and average soil
      ! temperature at the center of each simulation layer.
      ! The inputs needed to run the program are maximum daily air
      ! temperature, and minimum daily air temperature.  Furthermore,
      ! soil bulk density, volumetric water content, and clay
      ! fraction are used to calculate soil thermal properties.
      ! DATE:  10/05/93

      ! + + + KEY WORDS + + +
      ! soil temperature

      ! + + + COMMON BLOCKS + + +

      use weps_main_mod, only: am0ifl
      use file_io_mod, only: luotempsoil
      use datetime_mod, only: get_simdate
      use p1unconv_mod, only: mmtom, pi, SEC_PER_DAY
      use hydro_data_struct_defs, only: am0hfl

      ! + + + ARGUMENT DECLARATIONS + + +
      integer, intent(in) :: isr   ! subregion number
      integer layrsn
      real bszlyd(*), bszlyt(*), theta(0:*), thetas(*)
      real bsfsan(*), bsfsil(*), bsfcla(*), bsfom(*), bsdblk(*)
      real bwtdmn, bwtdmx, bwtyav, rad_net, bdmres
      real bhtsmn(*), bhtsmx(*), bhtsav(*), bhfice(*)
      real bhzsno, bhtsno, bhfsnfrz, bhzsnd
      real bhzsmt, soil_heat_flux

      ! + + + ARGUMENT DEFINITIONS + + +
      ! layrsn  - Number of soil layers used in the simulation
      ! bszlyd  - distance from surface to bottom of layer (mm)
      ! bszlyt  - Layer thickness (mm)
      ! theta   - volumetric soil water content
      ! thetas  - volumetric saturated soil water content
      ! bsfsan  - sand fraction of mineral soil content by layer (Mg sand/Mg soil mineral)
      ! bsfsil  - silt fraction of mineral soil content by layer (Mg silt/Mg soil mineral)
      ! bsfcla  - clay fraction of mineral soil content by layer (Mg clay/Mg soil mineral)
      ! bsfom   - organic matter fraction of soil content by layer (Mg organic/Mg soil solids)
      ! bsdblk  - Soil bulk density by layer (Mg/m^3)
      ! bwtdmn  - Daily minimum air temperature (C)
      ! bwtdmx  - Daily maximum air temperature (C)
      ! bwtyav  - Average yearly air temperature (deg C)
      ! rad_net - net radiation onto surface (Mj/m^2/day)
      ! bdmres  - plant residue on the soil surface (kg/m^2)
      ! bhtsmn  - Daily minimum soil temperature by layer (C)
      ! bhtsmx  - Daily maximum soil temperature by layer (C)
      ! bhfice  - fraction of water in soil layer which is frozen
      ! bhzsno  - depth of water contained in snow layer (mm)
      ! bhtsno  - temperature of snow layer (C)
      ! bhfsnfrz  - fraction of snow layer water content which is frozen
      ! bhzsnd  - actual thickness of snow layer (mm)
      ! bhzsmt  - depth of water melted and discharged from snow layer (mm)
      ! soil_heat_flux - soil heat flux estimated from top layer temperature
      !                  used in pot et calc (Mj/m^2/day)

      ! + + + PARAMETERS + + +
      real con_temp_depth
      parameter(con_temp_depth = 5.0)
      ! con_temp_depth - depth in the soil at which soil temperature is assumed
      !                  to equal average annual temperature

      ! + + + LOCAL VARIABLES + + +
      integer lay, day, mo, yr, bly, loopcnt
      real vsheat, thermk, zdamp, freq
      !  real time
      real t_air, t_surf_end, delta_t, delta_f
      real tamp, dmlayr(layrsn)
      real thermt_up, thermt_dn
      real thermk_up, thermk_dn
      real res_depth
      real nt_sno, nf_sno, nt_lay(layrsn), nf_ice(layrsn)
      real heat_cap_thaw, heat_cap_froz
      real rad_val, soil_val

      ! + + + LOCAL DEFINITIONS + + +
      ! lay    - soil layer index
      ! bly    - soil layer index + 1
      ! loopcnt - relaxation loop counter
      ! vsheat - average total volumetric heat capacity
      ! thermk - average total thermal conductivity (J/(s m C))
      ! zdamp  - Diurnal damping depth, m
      ! freq   - daily oscillation frequency
      ! time   - time of day (seconds)
      ! t_air  - daily average air temperature (C)
      ! t_surf_end - ending temperature of the soil surface (C)
      ! delta_t - temperature change observed as energy balance solution is relaxed (C)
      ! delta_f - frozen fraction change observed as energy balance solution is relaxed (C)
      ! thermt_up - heat transfer coefficient at upper boundary of layer (J/(s m^2 C))
      ! thermt_dn - heat transfer coefficient at lower boundary of layer (J/(s m^2 C))
      ! thermk_up - thermal conductivity of layer above boundary (J/(s m C))
      ! thermk_dn - thermal conductivity of layer below boundary (J/(s m C))
      ! res_depth - residue depth found from residue mass (mm)
      ! nt_sno - new average temperature of the snow layer (C)
      ! nf_sno - new fraction of snow water content which is frozen
      ! nt_lay - new average temperature of the soil layers (C)
      ! nf_lay - new fraction of soil water content which is frozen
      ! heat_cap_thaw - layer heat capacity in a thawed condition (J/(m^3 C))
      ! heat_cap_froz - layer heat capacity in a thawed condition (J/(m^3 C))
      ! rad_val - radiation value actually passed to energy balance routine
      ! soil_val - soil heat flux value returned from energy balance routine

      ! + + + FUNCTIONS CALLED + + +
      !  real heatcond
      !  real heatcap
      !  real snowcond

      ! + + + SUBROUTINES CALLED + + +
      ! stat
      ! drainsnow

      ! + + + DATA INITIALIZATIONS + + +
      !  phi = -(7. * pi) / 12.

      ! initialize various soil layer references

      dmlayr(1) = 0.5 * bszlyd(1) * mmtom
      do lay=2, layrsn
          dmlayr(lay) = 0.5 * mmtom * (bszlyd(lay-1) + bszlyd(lay))
      end do

      ! + + + OUTPUT FORMATS + + +
2009  format ('# Soil temperature (celsius), number of layers = ', i4)
2010  format ('# dy mo year minL1 maxL1 minL2 maxL2  ......')
2040  format (1x,2(i2,1x),i4,200(1x,f5.1))

      ! + + + END SPECIFICATIONS + + +

      if( (am0ifl .eqv. .true.) .and. ((am0hfl(isr) .eq. 4) &
        .or. (am0hfl(isr) .eq. 5) .or. (am0hfl(isr) .eq. 6) &
        .or. (am0hfl(isr) .eq. 7)) ) then
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
      thermk_dn = heatcond(bsdblk(bly), theta(bly), thetas(bly), &
                  bhtsav(bly), bhfice(bly), bsfsan(bly), bsfsil(bly), &
                  bsfcla(bly), bsfom(bly))

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
          thermt_dn = 2.0 * thermk_up * thermk_dn &
                    / (mmtom*(bhzsnd*thermk_dn + bszlyt(bly)*thermk_up))
          heat_cap_thaw = heatcap(1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0 )
          heat_cap_froz = heatcap(1.0, 1.0, 1.0, 0.0, 0.0, 0.0, 0.0 )
          call energy_bal(bhtsno, nt_sno, bhfsnfrz, nf_sno, &
               t_air, nt_lay(1), thermt_up, thermt_dn, &
               heat_cap_thaw, heat_cap_froz, 1.0, bhzsno, &
               SEC_PER_DAY, rad_val, soil_heat_flux )
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
          thermk_dn = 1.0 &
                    / ( (0.5*bszlyt(bly)/thermk_dn + res_depth/thermk) &
                    / (0.5*bszlyt(bly) + res_depth) )

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
          thermk_dn = heatcond(bsdblk(bly), theta(bly), thetas(bly), &
                   bhtsav(bly), bhfice(bly), bsfsan(bly), bsfsil(bly), &
                   bsfcla(bly), bsfom(bly))
          thermt_dn = 2.0 * thermk_up * thermk_dn &
                   / (mmtom*(bszlyt(lay)*thermk_dn &
                   + bszlyt(bly)*thermk_up))
          heat_cap_thaw = heatcap(bsdblk(lay), theta(lay), 0.0, &
                   bsfsan(lay), bsfsil(lay), bsfcla(lay), bsfom(lay))
          heat_cap_froz = heatcap(bsdblk(lay), theta(lay), 1.0, &
                   bsfsan(lay), bsfsil(lay), bsfcla(lay), bsfom(lay))
          call energy_bal(bhtsav(lay), nt_lay(lay), bhfice(lay), &
               nf_ice(lay), &
               t_surf_end, nt_lay(bly), thermt_up, thermt_dn, &
               heat_cap_thaw, heat_cap_froz, theta(lay), bszlyt(lay), &
               SEC_PER_DAY, rad_val, soil_val )

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
      thermt_dn = 2.0 * thermk_dn / ( mmtom*0.5*bszlyt(lay) &
               + max(0.0, con_temp_depth - mmtom*bszlyd(lay)) )
      heat_cap_thaw = heatcap(bsdblk(lay), theta(lay), 0.0, &
                      bsfsan(lay), bsfsil(lay), bsfcla(lay), bsfom(lay))
      heat_cap_froz = heatcap(bsdblk(lay), theta(lay), 1.0, &
                      bsfsan(lay), bsfsil(lay), bsfcla(lay), bsfom(lay))
      call energy_bal(bhtsav(lay),nt_lay(lay), bhfice(lay), nf_ice(lay),&
               t_surf_end, bwtyav, thermt_up, thermt_dn, &
               heat_cap_thaw, heat_cap_froz, theta(lay), bszlyt(lay), &
               SEC_PER_DAY, rad_val, soil_val )

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
      !      bhzsnd = bhzsnd*min(1.0,min(1.0-bwtdmx/100, nf_sno/bhfsnfrz))
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
                   bsfsan(lay), bsfsil(lay), bsfcla(lay), bsfom(lay)) &
                 * bszlyt(lay)
          thermk = thermk &
                 + heatcond(bsdblk(lay), theta(lay), thetas(lay), &
                   bhtsav(lay), bhfice(lay), bsfsan(lay), bsfsil(lay), &
                   bsfcla(lay), bsfom(lay)) * bszlyt(lay)
      end do

      vsheat = vsheat / bszlyd(layrsn)
      thermk = thermk / bszlyd(layrsn)

      ! calculate angular frequency of the temperature oscillation
      freq = (2*pi)/SEC_PER_DAY

      ! calculate diurnal damping depth
      zdamp= sqrt((2*(thermk/vsheat))/freq)

      ! begin simulating heat flow for each day of the simulation period
      ! calculate mean temperature and amplitude temperature at the
      ! soil-atmosphere interface  ( upper boundary condition )
      ! set lower boundary condition to an average air temperature

      tamp = bwtdmx - t_air

      do lay=1,layrsn
      !      do  j = 1,24
      !        time = j * 3600
      !        tsoil(lay,j)= bhtsav(lay)+tamp*exp(-dmlayr(lay)/zdamp)* &
      ! &                  sin((freq*time)-(dmlayr(lay)/zdamp)+phi)
      !       end do

      !  !    use subroutine stat to calculate the daily maximum,
      !     and minimum temperatures at the center of each simulation
      !     layer
      !      call statt(tsoil,bhtsmx(k),bhtsmn(k),k, layrsn, hrday)

          bhtsmx(lay) = bhtsav(lay) &
                      + tamp * max(0.0, (1.0 - dmlayr(lay)/zdamp))
          bhtsmn(lay) = bhtsav(lay) &
                      - tamp * max(0.0, (1.0 - dmlayr(lay)/zdamp))

      end do

      if( (am0hfl(isr) .eq. 4) .or. (am0hfl(isr) .eq. 5) &
         .or. (am0hfl(isr) .eq. 6) .or. (am0hfl(isr) .eq. 7)) then
         call get_simdate (day,mo,yr)
         write(luotempsoil(isr),2040) day, mo, yr, &
              (bhtsmn(lay), bhtsmx(lay), lay=1,layrsn)
      end if

      return
    end subroutine heat

    subroutine statt(tsoil, bhtsmx, bhtsmn, k, nlay, hrday)

      ! This subroutine uses the simulated hourly soil temperature
      ! values to calculate the daily maximum, and minimum soil
      ! temperatures at the center of each simulation layer

      ! + + + COMMON BLOCKS + + +

      ! + + + ARGUMENT DECLARATIONS + + +
      integer k, j, nlay, hrday
      real tsoil(nlay, hrday)
      real bhtsmn
      real bhtsmx

      ! + + + ARGUMENT DEFINITIONS + + +
      ! k      - Simulation layer counter
      ! tsoil  - Hourly soil temperature, c
      ! bhtsmn   - Daily minimum soil temperature, c
      ! bhtsmx   - Daily maximum soil temperature, c

      ! + + + END SPECIFICATIONS + + +

      bhtsmx= tsoil(k,1)
      bhtsmn= tsoil(k,1)
      do  100 j=1,24
        if (tsoil(k,j) .gt. bhtsmx)  bhtsmx= tsoil(k,j)
        if (tsoil(k,j) .lt. bhtsmn)  bhtsmn= tsoil(k,j)
 100  continue

      return
    end subroutine statt

    real function heatcap(bsdblk, theta, bhfice, &
                            bsfsan, bsfsil, bsfcla, bsfom)

      ! + + + PURPOSE + + +
      ! This function returns the volumetric heat capacity of the soil
      ! given mass fractions of the soil constituents. (J/m^3 C)

      ! + + + KEYWORDS + + +
      ! soil heat capacity

      ! + + + ARGUMENT DECLARATIONS + + +
      real bsdblk
      real theta
      real bhfice
      real bsfsan
      real bsfsil
      real bsfcla
      real bsfom

      ! + + + ARGUMENT DEFINITIONS + + +
      ! bsdblk  - Soil bulk density (Mg/m^3)
      ! theta   - volumetric Soil water content (m water/m soil)
      ! bhfice  - mass fraction of soil water which is ice (kg ice/kg water)
      ! bsfsan  - Sand mass fractions (kg clay/kg soil mineral)
      ! bsfsil  - Silt mass fractions (kg clay/kg soil mineral)
      ! bsfcla  - Clay mass fractions (kg clay/kg soil mineral)
      ! bsfom   - Organic matter fraction (kg organic matter/kg soil)

      ! + + + LOCAL VARIABLES + + +
      real grav_wat

      ! + + + LOCAL VARIABLE DEFINITION + + +
      ! grav_wat - gravimetric Soil water content (Mg water/Mg soil)

      ! + + + END SPECIFICATIONS + + +

      ! mass fraction weighted volumetric heat capacity based on the 
      ! method by De Vries as defined in:
      ! Kluitenberg, G.J. 2002. Heat Capacity and Specific Heat. in Dane, J.H. and
      ! Topp, G.C. eds. Methods of Soil Analysis, Part 4, Physical Methods. 
      ! Soil Science Society of America, Inc. Madison, Wisconsin, USA

      ! NOTE: (1-bsfom) gives (kg mineral soil/kg soil)
      ! air is not included

      ! convert volumetric to gravimetric
      grav_wat = theta / bsdblk

      ! units: Mg/m^3 * 1000kg/Mg * (J/(kg C)) = J/(m^3 C))
      heatcap = bsdblk * 1000.0 * ( bsfsan * (1.0-bsfom) * sandheatcap &
              + bsfsil * (1.0-bsfom) * siltheatcap &
              + bsfcla * (1.0-bsfom) * clayheatcap &
              + bsfom * organheatcap &
              + grav_wat * (1.0 - bhfice) * waterheatcap &
              + grav_wat * bhfice * iceheatcap )

      return
    end function heatcap

    real function heatcond(bsdblk, theta, thetas, bhtsav, bhfice, &
                             bsfsan, bsfsil, bsfcla, bsfom)

      ! + + + PURPOSE + + +
      ! This function returns the volumetrically based thermal conductivity of the soil
      ! given mass fractions of the soil constituents. (J/s m C) or (W/m C)

      ! + + + KEYWORDS + + +
      ! soil heat capacity

      use soilden_mod, only: den_ice, den_quartz, den_organic

      ! + + + ARGUMENT DECLARATIONS + + +
      real bsdblk, theta, thetas, bhtsav, bhfice
      real bsfsan, bsfsil, bsfcla, bsfom

      ! + + + ARGUMENT DEFINITIONS + + +
      ! bsdblk  - Soil bulk density (Mg/m^3)
      ! theta   - soil layer water content (m^3/m^3 bulk soil)
      ! thetas  - soil water content at saturation (m^3/m^3 bulk soil)
      ! bhtsav  - soil layer average daily temperature (C)
      ! bhfice  - mass fraction of soil water which is ice (kg ice/kg water)
      ! bsfsan  - Sand mass fractions (kg clay/kg soil mineral)
      ! bsfsil  - Silt mass fractions (kg clay/kg soil mineral)
      ! bsfcla  - Clay mass fractions (kg clay/kg soil mineral)
      ! bsfom   - Organic matter fraction (kg organic matter/kg soil)

      ! + + + LOCAL VARIABLES + + +
      real fac_a, fac_b, fac_c, fac_d, fac_e
      real kersten, deg_sat, cond_dry, cond_sat
      real volf_quartz, volf_mineral, volf_organic, volf_solid
      real cond_soil, cond_water, volf_liq_water

      ! + + + LOCAL VARIABLE DEFINITION + + +
      ! fac_a - sub calculation
      ! fac_b - sub calculation
      ! fac_c - sub calculation
      ! fac_d - sub calculation
      ! fac_e - sub calculation
      ! kersten - kersten number to proportion thermal conductivity between dry and saturated values
      ! deg_sat - degree of soil saturation with water
      ! cond_dry - dry soil thermal conductivity (J/s m C) or (W/m C)
      ! cond_sat - saturated soil thermal conductivity (J/s m C) or (W/m C)
      ! volf_quartz - volumetric fraction of soil which is quartz (m^3/m^3 bulk soil)
      ! volf_mineral - volumetric fraction of soil which is remaining mineral (m^3/m^3 bulk soil)
      ! volf_organic - volumetric fraction of soil which is organic (m^3/m^3 bulk soil)
      ! volf_solid - volumetric fraction of soil which is solid (excluding organic) (m^3/m^3 bulk soil)
      ! cond_soil - soil solids thermal conductivity (J/s m C) or (W/m C)
      ! cond_water - liquid water thermal conductivity (J/s m C) or (W/m C)
      ! volf_liq_water - volumetric fraction of liquid water (m^3/m^3 bulk soil)

      ! + + + END SPECIFICATIONS + + +

      ! Thermal Conductivity volumetrically weighted based on
      ! method by Campbell (1985) as defined in:
      ! Bristow, K.I. 2002. Thermal Conductivity. in Dane, J.H. and
      ! Topp, G.C. eds. Methods of Soil Analysis, Part 4, Physical Methods. 
      ! Soil Science Society of America, Inc. Madison, Wisconsin, USA

      ! BIG NOTE: this approximation does not account for organic matter content or ice.
      ! A small attempt was made to slant this method by only using the volume
      ! fractions of mineral elements, implying that organic matter conducts like air.
      ! The full treatment of temperature, organic matter and ice effects needs to use
      ! the other method in the same reference drawn from DeVries (1963)

      ! NOTE: (1-bsfom) gives (kg mineral soil/kg soil)
      ! air is not included

      !  volf_quartz = bsfsan * (1.0-bsfom) * bsdblk / den_quartz
      !  volf_mineral = (bsfsil+bsfcla) * (1.0-bsfom) * bsdblk/den_quartz
      !  volf_solid = volf_quartz + volf_mineral
      !  volf_organic = bsfom * bsdblk / den_organic

      !  fac_a = (0.57 + 1.73*volf_quartz + 0.93*volf_mineral) &
      ! &      / (1.0 - 0.47*volf_quartz - 0.49*volf_mineral) &
      ! &      - 2.8*volf_solid*(1.0-volf_solid)

      !  fac_b = 2.8 * volf_solid
      !  fac_c = 1.0 + ( 2.6 / bscla**0.5 )
      !  fac_d = 0.03 + 0.7 * volf_solid * volf_solid
      !  fac_e = 4.0

      !  heatcond = fac_a + fac_b*theta &
      ! &         - (fac_a - fac_d) * exp(-(fac_c*theta)**fac_e)

!========================================
! alternate method accounting for soil freezing

      ! volf_quartz - volumetric fraction of soil solids which is quartz (m^3/m^3 soil solids)
      ! volf_mineral - volumetric fraction of soil which is remaining mineral (m^3/m^3 soil solids)
      ! volf_organic - volumetric fraction of soil which is organic (m^3/m^3 soil solids)
      ! volf_solid - volumetric fraction of soil which is solid (excluding organic) (m^3/m^3 bulk soil)
      ! volf_water - volumetric fraction of saturated soil which is unfrozen water(m^3/m^3 bulk soil)

      ! Thermal Conductivity volumetrically weighted based on
      ! method by Johansen (1975) in:
      ! Peters-Lidard, C.D., E. Blackburn, X. Liang, and E.F. Wood. 1998.
      ! The effect of soil thermal conductivity parameterization on surface
      ! energy fluxes and temperatures. Journal of the Atmospheric Sciences
      ! vol. 55 pgs. 1209-1224

      ! dry soil thermal conductivity
      cond_dry = (135.0*bsdblk + 64.7) / (2700.0 - 947.0*bsdblk)

      ! liquid water thermal conductivity from Bristow in Dane and Topp (2002)
      if( bhtsav .gt. 0.0 ) then
          cond_water = 0.552 + 0.00234*bhtsav - 1.1e-5*bhtsav*bhtsav
      else
          cond_water = 0.552
      end if

      ! total soil volume fraction of unfrozen water
      volf_liq_water = thetas * den_ice * (1.0 - bhfice) &
                     / (den_ice * (1.0 - bhfice) + bhfice)

      ! volume fraction of quartz
      volf_quartz = bsfsan * (1.0-bsfom) &
                  / ( 1.0+ bsfom*(den_quartz/den_organic - 1.0) )

      ! volume fraction of organic matter
      volf_organic = bsfom * (1.0-bsfsan) &
                   / ( (1.0-bsfom)*den_organic/den_quartz + bsfom )

      ! soil solid portion thermal conductivity
      cond_soil = quartzheatcond**volf_quartz &
                * mineralheatcond**(1.0-volf_quartz-volf_organic) &
                * organicheatcond**volf_organic

      ! saturated soil thermal conductivity
      cond_sat = cond_soil**(1-thetas) &
               * iceheatcond**(thetas-volf_liq_water) &
               * cond_water**volf_liq_water

      ! degree of soil saturation
      deg_sat = max( 0.0, min(1.0, theta / thetas) )

      ! kersten number for unfrozen soil
      if( deg_sat .gt. 10**(-1.0/(1.0 - 0.3*bsfsan)) ) then
          kersten = (1.0 - 0.3*bsfsan) * log10(deg_sat) + 1.0
      else
          kersten = 0.0
      end if
      ! modify based on degree of soil layer that is frozen
      kersten = kersten * (1.0-bhfice) + deg_sat*bhfice

      ! thermal conductivity is between dry and saturated
      heatcond = kersten * (cond_sat - cond_dry) + cond_dry

      return
    end function heatcond

    real function snowcond( snow_den )

      ! returns the average thermal conductivity of the snow layer (J/(s m C))

      ! + + + ARGUMENT DECLARATIONS + + +
      real snow_den

      ! + + + ARGUMENT DEFINITIONS + + +
      ! snow_den - snow density (kg/m^3)

      ! + + + END SPECIFICATIONS + + +

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
    end function snowcond

    subroutine res_cond( flat_mass, thermk, thickness )

      ! returns the average thermal conductivity of the residue layer (J/(s m C))
      ! and the thickness of the layer (mm)

      ! + + + ARGUMENT DECLARATIONS + + +
      real flat_mass, thermk, thickness

      ! + + + ARGUMENT DEFINITIONS + + +
      ! flat_mass - flat residue mass (kg/m^2)
      ! thermk - apparent thermal conductivity of the residue layer (J/(s m C))
      ! thickness - soil layer thickness (mm)

      ! + + + PARAMETERS + + +
      real res_bd
      real res_t_cond

      parameter( res_bd = 13 )
      parameter( res_t_cond = 0.22 )

      ! res_bd - residue bulk density (kg/m^3)
      ! res_t_cond - base thermal conductivity of residue material (J/(s m C))

      ! + + + END SPECIFICATIONS + + +

      ! based on : S.J. van Donk, and E.W. Tollner. 2000. Apparent thermal conductivity
      ! of mulch materials exposed to forced convection. Transactions of the ASAE
      ! Vol. 43(5):1117-1127

      ! initially used single residue bulk density and no wind velocity function

      ! units: mtomm * kg/m^2 / kg/m^3 = mm
      thickness = flat_mass / res_bd

      ! units
      thermk = res_t_cond

      return
    end subroutine res_cond

    subroutine energy_bal(tlay_beg, tlay_end, froz_beg, froz_end, &
                 tup_end, tdn_end, thermt_up, thermt_dn, &
                 heat_cap_thaw, heat_cap_froz, vol_wat, lay_thick, &
                 time_step, rad_net, soil_heat_flux )

      ! returns the energy balance of a single layer computing the new layer
      ! temperature (C) and water frozen fraction. Net radiation is set by the
      ! calling routine to simulate a surface layer exposed to the sun. Similarly
      ! the soil heat flux is returned in case it is need to describe a surface
      ! effect (enters into the ET calculation)

      use p1unconv_mod, only: SEC_PER_DAY, mmtom
      use precision_mod, only: max_real

      ! + + + ARGUMENT DECLARATIONS + + +
      real tlay_beg, tlay_end, froz_beg, froz_end
      real tup_end, tdn_end, thermt_up, thermt_dn
      real heat_cap_thaw, heat_cap_froz, vol_wat, lay_thick
      real time_step, rad_net, soil_heat_flux

      ! + + + ARGUMENT DEFINITIONS + + +
      ! tlay_beg - layer temperature at beginning of time step (C)
      ! tlay_end - layer temperature at end of time step (C)
      ! froz_beg - fraction of layer water content which is frozen at beginning of time step
      ! froz_end - fraction of layer water content which is frozen at end of time step
      ! tup_end - layer above temperature at end of time step (C)
      ! tdn_end - layer below temperature at end of time step (C)
      ! thermt_up - thermal transfer coeff from layer above (J/(m^2 s C))
      ! thermt_dn - thermal transfer coeff from layer below (J/(m^2 s C))
      ! heat_cap_thaw - layer heat capacity in a thawed condition (J/(m^3 C))
      ! heat_cap_froz - layer heat capacity in a thawed condition (J/(m^3 C))
      ! vol_wat - layer volumetric water content (m^3 wat/m^3 lay)
      ! lay_thick - layer thickness (mm)
      ! time_step - time duration that energy balance will be applied (seconds)
      ! rad_net - daily soil (snow) surface net radiation (Mj/m^2/day)
      ! soil_heat_flux - daily ground heat flux (soil surface) (Mj/m^2/day)

      ! + + + FUNCTIONS CALLED + + +
      ! real diff_heat

      ! + + + LOCAL VARIABLES + + +
      real time_brk_1, time_brk_2, diff_heat_0, heat_cap_0, heat_vol_0
      real rate_rad_net, rate_soil_heat

      ! + + + LOCAL DEFINITIONS + + +
      ! time_brk_1 - time to reach first change of state boundary (seconds)
      !              temperature change to begin state change
      ! time_brk_2 - time to reach second change of state boundary (seconds)
      !              complete state change and restart temperature change
      ! diff_heat_0 - heat transfer rate given zero degree temperatures (J/(m^2 s))
      ! heat_cap_0  - heat capacity of the soil layer based on temperature (J/(m^3 C))
      ! heat_vol_0  - heat volume available for phase change of water in layer (J/kg/mm)
      ! rate_rad_net - net radiation exchange rate (J/(m^2 s))
      ! rate_soil_heat - soil surface heat exchange rate (J/(m^2 s))


      ! + + + END SPECIFICATIONS + + +

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
                                  thermt_up, thermt_dn)
          tlay_end = ( tlay_beg + time_step * diff_heat_0 &
                   / (heat_cap_0 * lay_thick * mmtom) ) &
                   / ( 1.0 + time_step * (thermt_up + thermt_dn) &
                   / (heat_cap_0 * lay_thick * mmtom) )

          if( (tlay_beg * tlay_end .lt. 0.0) ) then
              ! crossing T=0, find time to reach T=0
              time_brk_1 = - tlay_beg * heat_cap_0 * lay_thick * mmtom &
                         / diff_heat_0
          else if( ((froz_beg .eq. 1.0) .and. (tlay_end .gt. 0.0)) .or. &
              ((froz_beg .eq. 0.0) .and. (tlay_end .lt. 0.0)) ) then
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
                                  thermt_up, thermt_dn)
          
          ! broke out the subcalculation to facilitate testing for numerical instability
          ! = (mm/mm)*(J/kg)(mm)
          heat_vol_0 = vol_wat * heat_fusion * lay_thick
          if( diff_heat_0 .gt. froz_beg * heat_vol_0 / max_real) then
              ! thawing cycle
              time_brk_2 = time_brk_1 + froz_beg &
                         * heat_vol_0 / diff_heat_0
              froz_end = 0.0
          else if(diff_heat_0.lt.(froz_beg-1.0)*heat_vol_0/max_real)then
              ! freezing cycle
              time_brk_2 = time_brk_1 + (froz_beg - 1.0) &
                         * heat_vol_0 / diff_heat_0
              froz_end = 1.0
          else
              ! no change
              time_brk_2 = time_step + 1.0
          end if
    
          if( time_brk_2 .ge. time_step ) then
              ! incomplete freezing or thawing, find new water frozen fraction
              tlay_end = 0.0
              froz_end = froz_beg - diff_heat_0*(time_step - time_brk_1)&
                        / (vol_wat * heat_fusion * lay_thick)
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
                                  thermt_up, thermt_dn)
          tlay_end = ( (time_step - time_brk_2) * diff_heat_0 &
                   / (heat_cap_0 * lay_thick * mmtom) ) &
                   / ( 1.0 + (time_step - time_brk_2) &
                   * (thermt_up + thermt_dn) &
                   / (heat_cap_0 * lay_thick * mmtom) )

      end if

      ! find soil heat flux (heat transfer on upper side of soil layer)
      ! provided as information if needed as in soil surface heat flux
      rate_soil_heat = thermt_up * (tup_end - tlay_end)
      ! units (J/m^2/s * SEC_PER_DAY / 1000 j/Mj)
      soil_heat_flux = rate_soil_heat * SEC_PER_DAY / 1000.0 

      return
    end subroutine energy_bal

    real function diff_heat(tlay_end, tup_end, tdn_end, &
                              thermt_up, thermt_dn )

      ! returns differential heat transfer rate (J/(m^2 s))

      ! + + + ARGUMENT DECLARATIONS + + +
      real tlay_end, tup_end, tdn_end
      real thermt_up, thermt_dn

      ! + + + ARGUMENT DEFINITIONS + + +
      ! tlay_end - layer temperature at end of time step (C)
      ! tup_end - layer above temperature at end of time step (C)
      ! tdn_end - layer below temperature at end of time step (C)
      ! thermt_up - thermal transfer coeff from layer above (J/(s C))
      ! thermt_dn - thermal transfer coeff from layer below (J/(s C))

      ! + + + END SPECIFICATIONS + + +

      diff_heat = thermt_up * (tup_end - tlay_end) &
                - thermt_dn * (tlay_end - tdn_end)

      return
    end function diff_heat

    subroutine addsnow(dprecip, dirrig, bwzdpt, bhzirr, bhlocirr, &
                         bwtdmn, bwtdmx, bwtdpt, bmzele, &
                         bhzsno, bhtsno, bhfsnfrz, bhzsnd )

      ! + + + PURPOSE + + +
      ! This subroutine checks added water to see if it is snow and then
      ! properly adjusts the snow water content, depth, frozen to liquid
      ! ratio and temperature

      use p1unconv_mod, only: mtomm
      use air_water_mod, only: wetbulb

      ! + + + ARGUMENT DECLARATIONS + + +
      real dprecip, dirrig, bwzdpt, bhzirr, bhlocirr
      real bwtdmn, bwtdmx, bwtdpt, bmzele
      real bhzsno, bhtsno, bhfsnfrz, bhzsnd

      ! + + + ARGUMENT DEFINITIONS + + +
      ! dprecip - depth of precipitation reaching soil surface through snow (mm)
      ! dirrig  - depth of irrigation reaching soil surface through snow (mm)
      ! bwzdpt  - depth of precipitation added (mm)
      ! bhzirr  - depth of irrigation added (mm)
      ! bhlocirr - location of irrigation emitter (mm)
      ! bwtdmn  - Daily minimum air temperature (C)
      ! bwtdmx  - Daily maximum air temperature (C)
      ! bwtdpt  - dew point temperature (C)
      ! bmzele  - elevation (m)
      ! bhzsno  - depth of water contained in snow layer (mm)
      ! bhtsno  - temperature of snow layer (C)
      ! bhfsnfrz  - fraction of snow layer water content which is frozen
      ! bhzsnd  - actual thickness of snow layer (mm)

      ! + + + PARAMETERS + + +

      ! + + + LOCAL VARIABLES + + +
      real t_air, t_wb
      real new_energy, new_mass, new_depth
      real snow_den

      ! + + + LOCAL DEFINITIONS + + +
      ! t_air  - daily average air temperature (C)
      ! t_wb   - wet bulb temperature (C)
      ! new_energy - energy content of new snow (J/m^2)
      ! new_mass - mass of new snow (kg/m^2)
      ! new_depth - depth associated with new snow (mm) (indirect density)
      ! snow_den - calculated value of snow density (kg/m^3)

      ! + + + FUNCTIONS CALLED + + +
      !  real wetbulb

      ! + + + SUBROUTINES CALLED + + +
      ! statesnow

      ! + + + DATA INITIALIZATIONS + + +

      ! + + + END SPECIFICATIONS + + +

      ! find daily average air temperature
      t_air = 0.5 * (bwtdmn + bwtdmx)

      if( (bhzirr .gt. 0.0) .and. (bhlocirr .gt. 0.0) ) then
          ! irrigation water applied above or within snow layer
          ! add as liquid water at air temperature (0 and above)
          ! set mass of added liquid. units mm*(1m/1000mm)*1000kg/m^3 = kg/m^2
          new_mass = bhzirr

          ! calculate energy content of new water. units as above
          new_energy = new_mass * max(0.0, t_air) * waterheatcap
          new_depth = 0.0

          ! update state of snow cover and return liquid output
          call statesnow( dirrig, new_mass, new_energy, new_depth, &
                          bhzsno, bhtsno, bhfsnfrz, bhzsnd )
      else
          ! irrigation water applied below snow layer
          ! return directly as water applied to soil
          ! no change in snow state
          dirrig = bhzirr
      end if

      if( bwzdpt .gt. 0.0 ) then
          ! temperature check (Note: Anderson (1976) uses T_wb less than 1 degree C as the snow point)
          if( t_air .le. 0.0 ) then
              ! added water is snow, adjust snow total water content,
              ! average temperature, fraction liquid ratio, and total depth

              ! find wet bulb temperature from daily average air temperature
              t_wb = wetbulb( t_air, bwtdpt, bmzele )

              ! set mass of new snow added. units as above
              new_mass = bwzdpt

              ! calculate energy content of new snow. units as above
              new_energy = new_mass * (t_air * iceheatcap - heat_fusion)

              ! set physical depth of new snow (use new snow density)
              ! units: kg/m^2 / kg/m^3 = m * mtomm = mm
              if( t_wb .gt. -14.99 ) then
                  if( t_wb .lt. 0.0 ) then
                      snow_den = min_snow_den + 1.723*(t_wb+14.99)**1.5
                  else
                      snow_den = 150.0
                  end if
              else
                  snow_den = min_snow_den
              end if
              new_depth = mtomm * new_mass / snow_den
          else
              ! added water is liquid
              ! set mass of added liquid. units as above
              new_mass = bwzdpt

              ! calculate energy content of new water. units as above
              new_energy = new_mass * t_air * waterheatcap
              new_depth = 0.0
          end if

          ! update state of snow cover and return liquid output
          call statesnow( dprecip, new_mass, new_energy, new_depth, &
                          bhzsno, bhtsno, bhfsnfrz, bhzsnd )
      else
          ! if no precipitation then set return value (so it is set :-)
          dprecip = bwzdpt
      end if

      return
    end subroutine addsnow

    subroutine drainsnow(dh2o, bhzsno, bhfsnfrz, bhzsnd )

      ! + + + PURPOSE + + +
      ! This subroutine checks for drainage snow density and releases the 
      ! excess water to bring the snow density down to the drainage density
      ! and adjusts the snow water content and frozen to liquid ratio

      ! + + + KEY WORDS + + +
      ! drain snow

      use p1unconv_mod, only: mtomm, mmtom

      ! + + + ARGUMENT DECLARATIONS + + +
      real dh2o, bhzsno, bhfsnfrz, bhzsnd

      ! + + + ARGUMENT DEFINITIONS + + +
      ! dh2o    - depth of water drained from snow (mm)
      ! bhzsno  - depth of water contained in snow layer (mm)
      ! bhfsnfrz  - fraction of snow layer water content which is frozen
      ! bhzsnd  - actual thickness of snow layer (mm)

      ! + + + PARAMETERS + + +

      ! + + + LOCAL VARIABLES + + +
      real snow_den, new_fsnfrz

      ! + + + LOCAL DEFINITIONS + + +
      ! snow_den - calculated value of snow density (kg/m^3)
      ! new_fsnfrz - new fraction of snow layer water content which is frozen

      ! + + + END SPECIFICATIONS + + +

      ! check for density and drain water if above the drainage density
      ! units: mm * (1m/1000mm) * 1000 kg/m^3 = kg/m^2
      ! units: kg/m^2 / (mm * mmtom) = kg/m^3
      if( bhzsnd .gt. 0.0 ) then
          snow_den = bhzsno / (bhzsnd * mmtom)

          ! check against maximum densities
          if( snow_den .gt. melt_snow_den ) then
              ! melt water will be discharged
              ! find frozen water content fraction corresponding to melt snow density
              ! cannot remove more than available liquid
              new_fsnfrz = min(1.0, bhfsnfrz * snow_den / melt_snow_den)
              ! water released from snow layer
              ! remember kg/m^2 = mm of water using standard density
              dh2o = bhzsno * ( (1.0-bhfsnfrz) - (bhfsnfrz/new_fsnfrz) &
                   *(1.0-new_fsnfrz) )
              bhzsno = bhzsno - dh2o
              bhfsnfrz = new_fsnfrz
          else
              dh2o = 0.0
          end if

          ! adjust depth to not exceed maximum snow density
          bhzsnd = max(bhzsnd, mtomm * bhzsno / max_snow_den )
      else
          ! snow depth has collapsed (by melting)
          dh2o = bhzsno
          bhzsno = 0.0
          bhfsnfrz = 0.0
      end if

      return
    end subroutine drainsnow

    subroutine setlsnow(snow_wat, snow_froz_old, snow_froz_new, &
                          snow_depth, snow_temp, bwtdmx )

      ! + + + PURPOSE + + +
      ! This subroutine increases the snow density based on the temperature,
      ! snow depth, and liquid water content

      ! + + + KEY WORDS + + +
      ! settling snow

      use p1unconv_mod, only: mmtom

      ! + + + ARGUMENT DECLARATIONS + + +
      real snow_wat, snow_froz_old, snow_froz_new
      real snow_depth, snow_temp, bwtdmx

      ! + + + ARGUMENT DEFINITIONS + + +
      ! snow_wat - depth of water contained in snow layer (mm)
      ! snow_froz_old - old fraction of snow layer water content which is frozen
      ! snow_froz_new - new fraction of snow layer water content which is frozen
      ! snow_depth  - actual thickness of snow layer (mm)
      ! snow_temp - temperature of snow layer (C)
      ! bwtdmx - maximum daily air temperature (C)

      ! + + + PARAMETERS + + +

      ! + + + LOCAL VARIABLES + + +
      real snow_den, term2

      ! + + + LOCAL DEFINITIONS + + +
      ! snow_den - calculated value of snow density (kg/m^3)
      ! term2 - intermediate term value

      ! + + + END SPECIFICATIONS + + +

      ! reduce depth based on change in frozen fraction
      ! assuming that frozen portion remains constant density
      ! and melted portion fills voids, never increase depth
      snow_depth = snow_depth * min(1.0, snow_froz_new / snow_froz_old)

      if( snow_depth .gt. 0.0 ) then
          ! snow has depth, find density
          ! units: mm * (1m/1000mm) * 1000 kg/m^3 = kg/m^2
          ! units: kg/m^2 / (mm * mmtom) = kg/m^3
          snow_den = snow_wat / (snow_depth * mmtom)

          ! add an increase due to compaction. With single layer
          ! estimate use part of snow water content as overburden
          snow_den = snow_den * (1.0 + 0.08 * snow_wat &
                     * exp(0.08 * snow_temp - 21.0 * snow_den/1000.0))

          ! add an increase due to settling
          ! snow density factor
          if( snow_den .gt. 150.0 ) then
              term2 = exp(-46.0*(snow_den - 150.0))
          else
              term2 = 1.0
          end if
          snow_den = snow_den * (1.0 + 0.24 * exp(0.04*snow_temp)*term2)

          ! increase density to compensate for using average temperature
          snow_den = snow_den * ( 1.0 + max( 0.0, bwtdmx/25.0 ) )

          ! compute new depth based on density
          snow_depth = snow_wat / (snow_den * mmtom)
      end if

      return
    end subroutine setlsnow

    subroutine statesnow( dh2o, new_mass, new_energy, new_depth, &
                            bhzsno, bhtsno, bhfsnfrz, bhzsnd )

      ! + + + PURPOSE + + +
      ! Using inputs of present snow state, new added mass, energy, and
      ! snow depth, determines the new snow state and any water drainage.

      ! + + + ARGUMENT DECLARATIONS + + +
      real dh2o, new_mass, new_energy, new_depth
      real bhzsno, bhtsno, bhfsnfrz, bhzsnd

      ! + + + ARGUMENT DEFINITIONS + + +
      ! dh2o - depth of water reaching soil surface through snow (mm)
      ! new_energy - energy content of new snow (J/m^2)
      ! new_mass - mass of new snow (kg/m^2)
      ! new_depth - depth associated with new snow (mm) (indirect density)
      ! bhzsno  - depth of water contained in snow layer (mm)
      ! bhtsno  - temperature of snow layer (C)
      ! bhfsnfrz  - fraction of snow layer water content which is frozen
      ! bhzsnd  - actual thickness of snow layer (mm)

      ! + + + LOCAL VARIABLES + + +
      real old_energy, old_mass, tot_energy, tot_mass
      real fz_energy, new_fsnfrz

      ! + + + LOCAL DEFINITIONS + + +
      ! old_energy - energy content of existing snow layer (J/m^2)
      ! old_mass - mass of existing snow layer (kg/m^2)
      ! tot_energy - sum of old and new energy
      ! tot_mass - sum of old and new mass
      ! fz_energy - energy of snow at zero degrees all frozen (J/m^2)
      ! new_fsnfrz - new fraction of snow layer water content which is frozen

      ! + + + SUBROUTINES CALLED + + +
      ! drainsnow

      ! + + + DATA INITIALIZATIONS + + +

      ! + + + END SPECIFICATIONS + + +

      ! set mass of existing snow
      ! units: mm * (1m/1000mm) * 1000 kg/m^3 = kg/m^2
      old_mass = bhzsno

      ! calculate energy content of old snow
      ! units: kg/m^2 * ( C * J/(kg C) - J/kg ) = J/m^2
      old_energy = old_mass * bhfsnfrz &
                 * (bhtsno * iceheatcap - heat_fusion) &
                 + old_mass * (1.0 - bhfsnfrz) * bhtsno * waterheatcap

      ! sum mass and energy
      tot_mass = old_mass + new_mass
      tot_energy = old_energy + new_energy

      ! find energy of full frozen zero degree snow
      fz_energy = (- heat_fusion * tot_mass)

      ! select based on break point energy
      if( tot_energy .le. fz_energy ) then
          ! all snow is frozen, find temperature
          bhfsnfrz = 1.0
          bhtsno = (tot_energy/tot_mass + heat_fusion) / iceheatcap

          ! set snow depth
          bhzsnd = bhzsnd + new_depth
          bhzsno = tot_mass

          ! zero out liquid water content
          dh2o = 0.0
      else if( tot_energy .lt. 0.0 ) then
          ! mixture of snow and water
          ! temperature at freezing
          bhtsno = 0.0

          ! find new frozen water content fraction
          new_fsnfrz = - tot_energy/(tot_mass*heat_fusion)

          ! adjust snow depth based on change in frozen water content fraction
          bhzsnd = bhzsnd * new_fsnfrz * tot_mass / (bhfsnfrz*old_mass)
          bhfsnfrz = new_fsnfrz

          ! check for density and drain water if above the drainage density
          call drainsnow( dh2o, tot_mass, bhfsnfrz, bhzsnd )
          bhzsno = tot_mass
      else
          ! all liquid water
          ! add all snow water content to liquid water
          ! remember kg/m^2 = mm of water using standard density
          dh2o = tot_mass

          ! zero out snow
          bhtsno = 0.0
          bhfsnfrz = 0.0
          bhzsnd = 0.0
          bhzsno = 0.0
      end if

      return
    end subroutine statesnow

end module hydro_heat_mod

