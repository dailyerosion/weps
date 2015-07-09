!$Author$
!$Date$
!$Revision$
!$HeadURL$

subroutine compact( u, load, tillf, tlay, nlay, density, settled_bd, proc_bd_wc, proc_bd, laythk )

  ! + + + PURPOSE + + +
     
  ! This subroutine compacts soil layers. 

  ! + + + KEYWORDS + + +
  ! compaction 

  include 'p1werm.inc'
  include 'p1unconv.inc'  ! definition of pi

  ! + + + ARGUMENT DECLARATIONS + + +
  integer :: tlay       ! starting soil layer for compaction
  integer :: nlay       ! total number of soil layers in horizon
  real :: u             ! Compaction coefficient
  real :: load          ! Compaction load (Mg, Megagrams) (also known as metric ton)
  real :: tillf         ! fraction of soil area tilled by the machine
  real :: density(mnsz) ! present soil bulk density (Mg/m^3)
  real :: settled_bd(mnsz) ! proctor soil bulk density adjusted for water content (Mg/m^3)
  real :: proc_bd_wc(mnsz) ! proctor soil bulk density adjusted for water content (Mg/m^3)
  real :: proc_bd(mnsz) ! proctor soil bulk density (maximum dry density) (Mg/m^3)
  real :: laythk(mnsz)  ! layer thickness (mm)

  ! + + + ACCESSED COMMON BLOCK VARIABLE DEFINITIONS + + +
  ! mnsz        - max number of soil layers

  ! + + + LOCAL VARIABLES + + +
  integer :: blay       ! bottom soil layer to be compacted
  integer :: i          ! loop variable on layers 
  integer :: concfactor_harder  ! concentration factor for the harder soil condition
  integer :: concfactor_softer  ! concentration factor for the softer soil condition
  real :: concfactor_interp     ! interpolated concentration factor
  real :: eff_depth(mnsz)! effective depth to bottom of soil layer accounting for stress propagation in upper layers (m)
  real :: force         ! load converted to force (N, newtons)
  real :: dum(mnsz)     ! dummy variable used in calculating the adjusted density
  real :: interpfactor  ! interpolation factor in proportion to present bulk density
  real :: lay_stress_top  ! stress at top of soil layer (Pa, pascals)
  real :: lay_stress_bot  ! stress at bottom of soil layer (Pa, pascals)
  real :: bear_interp   ! interpolated soil load bearing capacity for layer (Pa, pascals)
  real :: eff_depth_avg ! average of effective depth at top and bottom of layer (m)
  real :: u_adj         ! compaction coefficient adjusted for depth in soil compation zone

  real, parameter :: bear_max = 95760.52 ! very hard soil load bearing (Pa, pascals)
  real, parameter :: bear_min = 47880.26 ! soft soil load bearing (Pa, pascals)

  ! + + + END SPECIFICATIONS + + + 

  ! beginning stress at the top of soil layer (point load assumption) is infinite.
  lay_stress_top = huge(lay_stress_top)
  ! convert load to force
  force = load * MgtoN

  ! find stress for each layer
  do i = tlay, nlay
    ! interpolate concentration factor between soft, firm, hard, and very hard conditions which affect
    ! the depth of the stress distribution. soft = 6, firm = 5, hard = 4, very hard = 3
    ! Smith, D.L.0. 1985. Compaction by wheels: a numerical model for agricultural soils. Journal of Soil Science, vol. 36:,621-632
    if( density(i) .le. settled_bd(i) ) then
      concfactor_harder = 5
      concfactor_softer = 6
      interpfactor = ( ((2.0/3.0)*settled_bd(i) - density(i)) / ((2.0/3.0)*settled_bd(i) - settled_bd(i)) )
    elseif( (density(i) .gt. settled_bd(i)) &
      .and. (density(i) .le. proc_bd_wc(i)) ) then
      concfactor_harder = 4
      concfactor_softer = 5
      interpfactor = ( (settled_bd(i) - density(i)) / (settled_bd(i) - proc_bd_wc(i)) )
    elseif( density(i) .lt. proc_bd_wc(i) ) then
      concfactor_harder = 3
      concfactor_softer = 4
      interpfactor = ( (proc_bd_wc(i) - density(i)) / (proc_bd_wc(i) - proc_bd(i)) )
    end if
    concfactor_interp = concfactor_softer - (concfactor_softer-concfactor_harder) * interpfactor
    bear_interp = bear_min + (bear_max - bear_min) * (6.0-concfactor_interp)/3.0
 
    ! propagate the load into the soil layer by layer using the stress penetration of Soehne (1958)
    ! Soehne, W., 1958. Fundamentals of pressure distribution and soil compaction under tractor tyres. Agric. Eng. 39, 276-281.
    ! stop propagating compaction at layer where compaction pressure is less than the load bearing pressure.

    ! to accomodate varying soil concentration factors, effective depth must be back calculated to reflect stress in layers above
    eff_depth(i) = laythk(i)*mmtom + (force * concfactor_interp / (2.0*pi*lay_stress_top))**0.5
    
    ! find the stress at the bottom of layer
    lay_stress_bot = force * concfactor_interp / (2.0*pi*eff_depth(i)*eff_depth(i))

    ! find thickness of compaction layer as point where stress is less than bearing capacity
    if( lay_stress_bot .le. bear_interp ) then
      blay = i
      exit
    end if

    ! set values for next layer
    lay_stress_top = lay_stress_bot
  end do

  do i = tlay, blay
    ! effective depth at middle of layer
    if( i .eq. 1 ) then
      eff_depth_avg = eff_depth(i) / 2.0
    else
      eff_depth_avg = (eff_depth(i) + eff_depth(i-1)) / 2.0
    end if

    ! find ajusted compaction coefficient
    u_adj = u * (1.0 - eff_depth_avg/eff_depth(blay))

    ! compaction - linear decrease to depth of influence
    dum(i) = density(i) + ( (proc_bd_wc(i) - density(i)) * u_adj * tillf )
    laythk(i) = laythk(i) * ( density(i) / dum(i) )
    density(i) = dum(i)
  end do

  ! perform compaction process

end subroutine compact
