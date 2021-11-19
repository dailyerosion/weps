!$Author$
!$Date$
!$Revision$
!$HeadURL$

module soilden_mod

  ! den_organic - density of soil organic particles (Mg/m^3)
  !   Particle density of organic matter
  !   1.37 in Baver, Soil Physics, 1972 p.44 (humus)
  !   1.1  in Marshall and Holmes, Soil Physics, 1979, p.277 (soil organic matter)
  ! den_quartz - density of soil quartz articles (Mg/m^3)

  real, parameter :: den_organic = 1.23
  real, parameter :: den_quartz = 2.65
  real, parameter :: den_ice = .92
  real, parameter :: den_rock = 2.55  ! until the specific type of rock is determined from the soil survey, use this.

  interface getLayMass
    module procedure get_layer_mass_ssc
    module procedure get_layer_mass_om
    module procedure get_layer_mass_roc
  end interface getLayMass

  interface setMassFrac
    module procedure set_mass_fraction_ssc
    module procedure set_mass_fraction_om
  end interface setMassFrac

  interface setVolFrac
    module procedure set_volume_fraction_roc
  end interface setVolFrac

  contains

    pure function setpartden ( organf ) result( partden )

      ! + + + PURPOSE + + +
      ! Estimates an average particle density assuming the mineral portion
      ! of the soil is derived from quartz materials

      real, intent(in) :: organf     ! fraction of soil organic matter
      real :: partden                ! particle density

      partden = 1.0 / (organf/den_organic + (1.0-organf)/den_quartz)

    end function setpartden

    function setbdref ( clay, sand, om ) result( bdref )

      ! + + + PURPOSE + + +
      ! The following function estimates a reference soil bulk density from
      ! intrinsic properties. see Thomas Keller, Inge Håkansson. 2010. Estimation
      ! of reference bulk density from soil particle size distribution and soil
      ! organic matter content. Geoderma 154:398–406.

      ! The multilinear fit reported in the paper

      ! The article references: Heinonen, R., 1960. Das Volumengewicht als Kennzeichen
      ! der “normalen” Bodenstruktur. Zeitschrift der Landwirtschafts-wissenschaftlichen
      ! Gesellschaft in Finnland, vol. 32, pp. 81–87. A regression equation for "natural"
      ! or settled bulk density is provided. When tested, it matches RAWLS settled bulk
      ! density closely for OM less than 0.1
      ! bd_normal = 1.40 - 7.2*om - 0.13*clay + 0.14*sand

      ! + + + ARGUMENTS + + +
      real, intent(in) :: clay       ! fraction of soil clay content (mineral fraction)
      real, intent(in) :: sand       ! fraction of soil sand content (mineral fraction)
      real, intent(in) :: om         ! fraction of soil organic matter
      real :: bdref     ! reference bulk density (compressed to 200Kpa for 1 week)

      ! + + + PARAMETERS + + +
      real, parameter :: a = -1.89943
      real, parameter :: b = 2.92681
      real, parameter :: c = 2.01621
      real, parameter :: d = -0.568659

      ! + + + LOCAL VARIABLES + + +
      real :: bd_adjustment              ! difference between settled and reference bulk density

      ! + + + END SPECIFICATIONS + + +

      bd_adjustment = a*cos(sand) + b*sin(sand) + c*cos(2*sand) + d*sin(2*sand)

      bdref = setbds(clay, sand, om) + 1.0/((om/0.1)+(1.0-om)/bd_adjustment)

      return

    end function setbdref

    pure function optimalwat( clay, sand, om ) result( owc )

      ! + + + PURPOSE + + +
      ! Calculation to test proctor density calculation

      real, intent(in) :: clay       ! fraction of soil clay content (mineral fraction)
      real, intent(in) :: sand       ! fraction of soil sand content (mineral fraction)
      real, intent(in) :: om         ! fraction of soil organic matter
      real :: owc          ! optimal water content for compaction (%)

      ! + + + LOCAL VARIABLES + + +
      real, parameter :: owc_a = 16.0932
      real, parameter :: owc_b = 0.129032*100
      real, parameter :: owc_c = -0.0883454*100
      real, parameter :: owc_d = 1.06305*100

      ! find optimal water content
      owc = owc_a + owc_b*clay + owc_c*sand + owc_d*om

    end function optimalwat

    pure function setbdproc ( clay, sand, om, pden ) result( bdproc )

      ! + + + PURPOSE + + +
      ! The following function estimates the proctor soil bulk density from
      ! intrinsic properties. see Wagner, L.E., Ambe, N.M., Ding, D. Estimating
      ! a Proctor Density Curve from Intrinsic Soil Properties. Trans of the ASAE;
      ! 1994; 37((4)): 1121-1125. Additional data from the SERDP project is used to
      ! update the OWC function to a different form

      ! + + + ARGUMENTS + + +
      real, intent(in) :: clay       ! fraction of soil clay content (mineral fraction)
      real, intent(in) :: sand       ! fraction of soil sand content (mineral fraction)
      real, intent(in) :: om         ! fraction of soil organic matter
      real, intent(in) :: pden       ! average particle density
      real :: bdproc     ! proctor bulk density (standard method)

      ! + + + LOCAL VARIABLES + + +
      real :: owc          ! optimal water content for compaction

      ! find optimal water content
      owc = optimalwat( clay, sand, om )

      bdproc = pden / (1.0 + pden*owc/80.0)

      return

    end function setbdproc

    function setbdproc_wc ( clay, sand, om, pden, swc ) result( bd_wc )

      ! + + + PURPOSE + + +
      ! The following function estimates the proctor soil bulk density from
      ! intrinsic properties. see Wagner, L.E., Ambe, N.M., Ding, D. Estimating
      ! a Proctor Density Curve from Intrinsic Soil Properties. Trans of the ASAE;
      ! 1994; 37((4)): 1121-1125. Additional data from the SERDP project is used to
      ! update the OWC function to a different form

      ! + + + ARGUMENTS + + +
      real, intent(in) :: clay       ! fraction of soil clay content (mineral fraction)
      real, intent(in) :: sand       ! fraction of soil sand content (mineral fraction)
      real, intent(in) :: om         ! fraction of soil organic matter
      real, intent(in) :: pden       ! average particle density
      real, intent(in) :: swc        ! soil water content (g/g)
      real :: bd_wc     ! proctor bulk density adjusted for water content

      ! + + + LOCAL VARIABLES + + +
      real :: owc          ! optimal soil water content for compaction %(g/g)
      real :: bdproc       ! proctor bulk density (standard method)
      real :: pswc         ! percent soil water content %(g/g)
      real, parameter :: k_low = 0.0154    ! slope of density reduction when swc less than owc
      real, parameter :: k_high = -0.0241  ! slope of density reduction when swc greater than owc

      ! find optimal water content
      owc = optimalwat( clay, sand, om )

      bdproc = pden / (1.0 + pden*owc/80.0)

      pswc = 100.0 * swc
      if( pswc .lt. owc ) then
        bd_wc = k_low * (pswc - owc) + bdproc
      else
        bd_wc = k_high * (pswc - owc) + bdproc
      end if

      bd_wc = max( bd_wc, setbds(clay, sand, om) )

      return

    end function setbdproc_wc

    function setbds ( clay, sand, om ) result ( bds )

      ! + + + PURPOSE + + +
      ! The following function estimates settled soil bulk density from
      ! intrinsic properties. see Rawls (1983) Soil Science 135, 123-125.

      ! + + + KEYWORDS + + +
      ! bulk density, initialization

      ! + + + ARGUMENT DECLARATIONS + + +
      real, intent(in) :: clay       ! fraction of soil clay content (mineral fraction)
      real, intent(in) :: sand       ! fraction of soil sand content (mineral fraction)
      real, intent(in) :: om         ! fraction of soil organic matter
      real :: bds       ! settled bulk density

      ! + + + LOCAL VARIABLES + + +
      real :: sand_adj ! sand content adjusted so sand and clay sum to less than 1
      real :: clay_adj ! clay content adjusted so sand and clay sum to less than 1
      real :: tempsum  ! temporary sum of sand and clay fractions
      integer :: li  ! index into table less than sand content
      integer :: lj  ! index into table less than clay content
      integer :: hi  ! index into table higher than sand content
      integer :: hj  ! index into table higher than clay content
      real :: mi  ! value between indexes for interpolation for sand
      real :: mj  ! value between indexes for interpolation for clay
      real :: fi  ! fraction of distance between grid cells for sand
      real :: fj  ! fraction of distance between grid cells for clay
      real :: mbdtv (0:10,0:10) ! data table of settled bulk density
                                ! as a function of sand (across the top)
                                ! and clay (down the side)
      real :: mbd   ! mineral bulk density without organic matter
      real :: mbd_hi_hj ! value for mbdtv(hi,hj), if outside triangular
                        ! part of table it is reflected from mbdtv(li,lj)
                        ! otherwise it is just the real point

      ! + + + DATA INITIALIZATIONS + + +
      ! first index in this direction ->
      ! second index || goes down 
      !              \/
      data mbdtv /1.48,1.25,1.00,1.06,1.16,1.22,1.30,1.39,1.45,1.51,1.52, &
                  1.52,1.40,1.19,1.25,1.32,1.40,1.52,1.58,1.63,1.65,0.00, &
                  1.52,1.40,1.25,1.35,1.45,1.53,1.60,1.64,1.72,0.00,0.00, &
                  1.52,1.40,1.29,1.41,1.50,1.57,1.63,1.68,0.00,0.00,0.00, &
                  1.50,1.40,1.35,1.43,1.53,1.61,1.64,0.00,0.00,0.00,0.00, &
                  1.46,1.40,1.40,1.43,1.53,1.62,0.00,0.00,0.00,0.00,0.00, &
                  1.45,1.40,1.38,1.42,1.50,0.00,0.00,0.00,0.00,0.00,0.00, &
                  1.42,1.37,1.33,1.33,0.00,0.00,0.00,0.00,0.00,0.00,0.00, &
                  1.33,1.32,1.20,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00, &
                  1.23,1.18,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00, &
                  1.15,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00/

      ! + + + END SPECIFICATIONS + + +

      tempsum = sand + clay
      if( tempsum.gt.1.0 ) then
          sand_adj = sand / tempsum
          clay_adj = clay / tempsum
          write(*,*) "setbds: sand plus clay fractions greater than 1.0"
          write(*,*) "values adjusted by averaging the difference"
      else
          sand_adj = sand
          clay_adj = clay
      endif

      ! i = nint(sand_adj*100.0/10.0)
      ! j = nint(clay_adj*100.0/10.0)

      ! mbd = mbdtv(i,j)

      mi = sand_adj*10.0
      li = int(mi)
      fi = mi - li
      hi = min( li+1, 10 )
       
      mj = clay_adj*10.0
      lj = int(mj)
      fj = mj - lj
      hj = min( lj+1, 10 )

      ! check for table edge
      if( li + lj .eq. 10 ) then
          ! on table edge, no interpolation necessary
          mbd = mbdtv(li,lj)
      else
          if( hi + hj .gt. 10 ) then
              ! interpolation on the triangular edge of the table
              ! mirror li,lj value to make using grid interpolation possible
              mbd_hi_hj =  mbdtv(li,hj) + mbdtv(hi,lj) - mbdtv(li,lj)
          else
              ! interpolation within the table, use grid point
              mbd_hi_hj = mbdtv(hi,hj)
          end if
          mbd = (1-fi) * (1-fj) * mbdtv(li,lj) &
              + (1-fi) * fj * mbdtv(li,hj) &
              + fi * (1-fj) * mbdtv(hi,lj) &
              + fi * fj * mbd_hi_hj
      end if

      ! using geometric mean, adjust for OM, converging to 0.224 for 100% OM
      bds = 1.0 / ((om / 0.224)  + (1.0 - om)/ mbd)

      return
    end function setbds

    pure function get_layer_mass_ssc ( fssc, laythk, vfrock, bulkden, fom ) result (mass_ssc)
      ! Returns the mass of sand, silt or clay in a soil layer
      ! Units: kg/m^2 = (mm/m^2)(m/1000mm)(Mg/m^3)(1000kg/Mg)(m^3/m^3)(Mg/Mg)
      real, intent(in) :: fssc       ! mass fraction of fine soil sand, silt or clay (Mg/Mg) (mineral fraction)
      real, intent(in) :: laythk     ! thickness of soil layer (mm)
      real, intent(in) :: vfrock     ! volume fraction of soil rock fragments (m^3/m^3)
      real, intent(in) :: bulkden    ! bulk density of fine soil fraction (Mg/m^3)
      real, intent(in) :: fom        ! mass fraction of fine soil organic matter (Mg/Mg)
      real :: mass_ssc               ! mass of fine soil sand, silt or clay (kg/m^2)

      mass_ssc = fssc * laythk * bulkden * (1.0 - vfrock) * (1.0 - fom)

      return
    end function get_layer_mass_ssc 

    pure function set_mass_fraction_ssc ( mass_ssc, laythk, vfrock, bulkden, fom ) result (fssc)
      ! Returns the mass fraction of sand, silt or clay in fine soil of a layer
      real, intent(in) :: mass_ssc   ! mass of fine soil sand, silt or clay (kg/m^2)
      real, intent(in) :: laythk     ! thickness of soil layer (mm)
      real, intent(in) :: vfrock     ! volume fraction of soil rock fragments (m^3/m^3)
      real, intent(in) :: bulkden    ! bulk density of fine soil fraction (Mg/m^3)
      real, intent(in) :: fom        ! mass fraction of fine soil organic matter (Mg/Mg)
      real :: fssc                   ! mass fraction of fine soil sand, silt or clay (Mg/Mg) (mineral fraction)

      fssc = mass_ssc / (laythk * bulkden * (1.0 - vfrock) * (1.0 - fom))

      return
    end function set_mass_fraction_ssc 

    pure function get_layer_mass_om ( fom, laythk, vfrock, bulkden ) result (mass_om)
      ! Returns the mass of organic matter in fine soil of a layer
      real, intent(in) :: fom        ! mass fraction of fine soil organic matter (Mg/Mg)
      real, intent(in) :: laythk     ! thickness of soil layer (mm)
      real, intent(in) :: vfrock     ! volume fraction of soil rock fragments (m^3/m^3)
      real, intent(in) :: bulkden    ! bulk density of fine soil fraction (Mg/m^3)
      real :: mass_om                ! mass of fine soil organic matter (kg/m^2)

      mass_om = fom * laythk * bulkden * (1.0 - vfrock)

      return
    end function get_layer_mass_om 

    pure function set_mass_fraction_om ( mass_om, laythk, vfrock, bulkden) result (fom)
      ! Returns the mass fraction of organic matter in fine soil of a layer
      real, intent(in) :: mass_om    ! mass of fine soil organic matter (kg/m^2)
      real, intent(in) :: laythk     ! thickness of soil layer (mm)
      real, intent(in) :: vfrock     ! volume fraction of soil rock fragments (m^3/m^3)
      real, intent(in) :: bulkden    ! bulk density of fine soil fraction (Mg/m^3)
      real :: fom                    ! mass fraction of soil organic matter (Mg/Mg)

      fom = mass_om / (laythk * bulkden * (1.0 - vfrock))

      return
    end function set_mass_fraction_om

    pure function get_layer_mass_roc ( vfrock, laythk, bulkden ) result (mass_roc)
      ! Returns the mass of rock fragments in a soil layer
      real, intent(in) :: vfrock     ! volume fraction of soil rock fragments (m^3/m^3)
      real, intent(in) :: laythk     ! thickness of soil layer (mm)
      real, intent(in) :: bulkden    ! bulk density of rock fragments (Mg/m^3)
      real :: mass_roc               ! mass of rock fragments (kg/m^2)

      mass_roc = vfrock * laythk * bulkden

      return
    end function get_layer_mass_roc 

    pure function set_volume_fraction_roc ( mass_roc, laythk, bulkden) result (vfrock)
      ! Returns the volume fraction of rock fragments in a soil layer
      real, intent(in) :: mass_roc   ! mass of rock fragments (kg/m^2)
      real, intent(in) :: laythk     ! thickness of soil layer (mm)
      real, intent(in) :: bulkden    ! bulk density of rock fragments (Mg/m^3)
      real :: vfrock                 ! volume fraction of soil rock fragments (m^3/m^3)

      vfrock = mass_roc / (laythk * bulkden)

      return
    end function set_volume_fraction_roc

    pure subroutine setLayThick( laythk, vfrock, oldbulkden, newbulkden)
      ! based on change in fine soil bulk density, changes layer thickness rock volume fraction
      real, intent(inout) :: laythk  ! thickness of soil layer (mm)
      real, intent(inout) :: vfrock  ! volume fraction of soil rock fragments (m^3/m^3)
      real, intent(in) :: oldbulkden    ! bulk density of fine soil (Mg/m^3)
      real, intent(in) :: newbulkden    ! bulk density of fine soil (Mg/m^3)

      real :: newlaythk

      newlaythk = laythk * (1.0 + (1.0-vfrock)*(oldbulkden/newbulkden - 1.0))
      ! rock volume does not change, so volume fraction must change
      vfrock = vfrock * (laythk/newlaythk)
      ! return new layer thickness
      laythk = newlaythk

      return
    end subroutine setLayThick

end module soilden_mod

