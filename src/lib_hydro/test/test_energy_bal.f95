!$Author:$
!$Date:$
!$Revision:$
!$HeadURL:$

program test_energy_bal

  ! test of the energy_bal routine

  include 'p1unconv.inc'
  include 'precision.inc'

  ! variable definitions
  real :: tlay_beg       ! layer temperature at beginning of time step (C)
  real :: tlay_end       ! layer temperature at end of time step (C)
  real :: froz_beg       ! fraction of layer water content which is frozen at beginning of time step
  real :: froz_end       ! fraction of layer water content which is frozen at end of time step
  real :: tup_end        ! layer above temperature at end of time step (C)
  real :: tdn_end        ! layer below temperature at end of time step (C)
  real :: thermt_up      ! thermal transfer coeff from layer above (J/(m^2 s C))
  real :: thermt_dn      ! thermal transfer coeff from layer below (J/(m^2 s C))
  real :: heat_cap_thaw  ! layer heat capacity in a thawed condition (J/(m^3 C))
  real :: heat_cap_froz  ! layer heat capacity in a thawed condition (J/(m^3 C))
  real :: vol_wat        ! layer volumetric water content (m^3 wat/m^3 lay)
  real :: lay_thick      ! layer thickness (mm)
  real :: time_step      ! time duration that energy balance will be applied (seconds)
  real :: rad_net        ! daily soil (snow) surface net radiation (Mj/m^2/day)
  real :: soil_heat_flux ! daily ground heat flux (soil surface) (Mj/m^2/day)

  real :: snowdepth      ! depth of snow (mm)
  real :: snowwater      ! snow total water content (mm)

  ! functions
  real :: snowcond
  real :: heatcap

  max_real = huge(1.0) * 0.999150

  ! some initial values for a water only layer
  snowdepth = 10.0
  snowwater = 5.0

  thermt_up = snowcond(1000.0*snowwater/snowdepth) / (mmtom*snowdepth/2.0)
  thermt_dn = thermt_up
  heat_cap_thaw = heatcap(1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0 )
  heat_cap_froz = heatcap(1.0, 1.0, 1.0, 0.0, 0.0, 0.0, 0.0 )
  vol_wat = 1.0
  lay_thick = snowwater
  time_step = 3600
  rad_net = 0.0

  ! this set was used to demonstrate an in the calculation of frozen fraction
  ! where the time_brk_1 was not subtracted from the frozen fraction calculation
  ! when temperature moved toward zero but did not fully cross.

  write(*,*) 'warm then thaw'
  tlay_beg = -10.0     ! from all frozen
  tlay_end = tlay_beg
  froz_beg = 1.0
  froz_end = froz_beg
  tup_end = 3.825
  tdn_end = 0.0

  ! test of slow thawing. with innitial temperature change
  do while( froz_end .gt. 0.0 )

    ! do the energy balance
    call energy_bal(tlay_beg, tlay_end, froz_beg, froz_end, tup_end, tdn_end, &
         thermt_up, thermt_dn, heat_cap_thaw, heat_cap_froz, vol_wat, lay_thick, &
         time_step, rad_net, soil_heat_flux )

    write(*,*) 'tlay_end, froz_end', tlay_end, froz_end

    tlay_beg = tlay_end
    froz_beg = froz_end

  end do  

  write(*,*) 'cool then freeze'
  tlay_beg = 10.0     ! from all thawed
  tlay_end = tlay_beg
  froz_beg = 0.0
  froz_end = froz_beg
  tup_end = -4.05
  tdn_end = 0.0

  ! test of slow freezing. with innitial temperature change
  do while( froz_end .lt. 1.0 )

    ! do the energy balance
    call energy_bal(tlay_beg, tlay_end, froz_beg, froz_end, tup_end, tdn_end, &
         thermt_up, thermt_dn, heat_cap_thaw, heat_cap_froz, vol_wat, lay_thick, &
         time_step, rad_net, soil_heat_flux )

    write(*,*) 'tlay_end, froz_end', tlay_end, froz_end

    tlay_beg = tlay_end
    froz_beg = froz_end

  end do  

  write(*,*) 'only thaw'
  tlay_beg = 0.0     ! from all frozen
  tlay_end = tlay_beg
  froz_beg = 1.0
  froz_end = froz_beg
  tup_end = 3.825
  tdn_end = 0.0

  ! test of slow thawing. with innitial temperature change
  do while( froz_end .gt. 0.0 )

    ! do the energy balance
    call energy_bal(tlay_beg, tlay_end, froz_beg, froz_end, tup_end, tdn_end, &
         thermt_up, thermt_dn, heat_cap_thaw, heat_cap_froz, vol_wat, lay_thick, &
         time_step, rad_net, soil_heat_flux )

    write(*,*) 'tlay_end, froz_end', tlay_end, froz_end

    tlay_beg = tlay_end
    froz_beg = froz_end

  end do  

  write(*,*) 'only freeze'
  tlay_beg = 0.0     ! from all thawed
  tlay_end = tlay_beg
  froz_beg = 0.0
  froz_end = froz_beg
  tup_end = -4.05
  tdn_end = 0.0

  ! test of slow freezing. with innitial temperature change
  do while( froz_end .lt. 1.0 )

    ! do the energy balance
    call energy_bal(tlay_beg, tlay_end, froz_beg, froz_end, tup_end, tdn_end, &
         thermt_up, thermt_dn, heat_cap_thaw, heat_cap_froz, vol_wat, lay_thick, &
         time_step, rad_net, soil_heat_flux )

    write(*,*) 'tlay_end, froz_end', tlay_end, froz_end

    tlay_beg = tlay_end
    froz_beg = froz_end

  end do  


end program test_energy_bal