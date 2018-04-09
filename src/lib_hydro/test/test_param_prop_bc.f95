!$Author:$
!$Date:$
!$Revision:$
!$HeadURL:$

program test_param_prop_bc

  ! test of the param_prop_bc routine

  include 'p1unconv.inc'
  include 'precision.inc'
  include 'hydro/vapprop.inc'

  ! variable definitions
  integer :: idx                              ! generic index
  integer :: nsoil                            ! number of soils
  character(LEN=4), dimension(:), allocatable :: soilname  ! soil texture class names
  real, dimension(:), allocatable :: bszlyd   ! Depth to bottom of each soil layer for each subregion (mm)
  real, dimension(:), allocatable :: bsdblk   ! bulk density (Mg/m^3) = (g/cm^3)
  real, dimension(:), allocatable :: bsdsblk  ! settled bulk density (Mg/m^3) = (g/cm^3)
  real, dimension(:), allocatable :: bsdpblk  ! proctor bulk density (Mg/m^3) = (g/cm^3)
  real, dimension(:), allocatable :: bsdwblk  ! wet bulk density (Mg/m^3) = (g/cm^3)
  real, dimension(:), allocatable :: wet_set_rat  ! wet to settled bulk density ratio
  real, dimension(:), allocatable :: bsdpart  ! particle density (Mg/m^3)
  real, dimension(:), allocatable :: bsfcla   ! fraction of soil mineral portion which is clay
  real, dimension(:), allocatable :: bsfsan   ! fraction of soil mineral portion which is sand
  real, dimension(:), allocatable :: bsfom    ! fraction of total soil mass which is organic matter
  real, dimension(:), allocatable :: bsfcec   ! Soil layer cation exchange capacity (cmol/kg) (meq/100g)
  real, dimension(:), allocatable :: bhrwcs   ! gravimetric saturated water
  real, dimension(:), allocatable :: bhrwcf   ! gravimetric 1/3 bar water
  real, dimension(:), allocatable :: bhrwcw   ! gravimetric 15 bar water
  real, dimension(:), allocatable :: bhrwcr   ! gravimetric residual water
  real, dimension(:), allocatable :: bhrwca   ! gravimetric plant available water
  real, dimension(:), allocatable :: bh0cb    ! Brooks and Corey pore size interation exponent b
  real, dimension(:), allocatable :: bheaep   ! Brooks and Corey air entry potential (J/kg)
  real, dimension(:), allocatable :: bhrsk    ! saturated hydraulic conductivity (m/s)
  real, dimension(:), allocatable :: bhfredsat! fraction of soil porosity that will be filled with water
                                              ! while wetting under normal field conditions due to entrapped air

  ! the soils are the standard soils defined by texture by NRCS
  nsoil = 12

  ! allocate arrays
  allocate( soilname(nsoil) )
  allocate( bszlyd(nsoil) )
  allocate( bsdblk(nsoil) )
  allocate( bsdsblk(nsoil) )
  allocate( bsdpblk(nsoil) )
  allocate( bsdwblk(nsoil) )
  allocate( wet_set_rat(nsoil) )
  allocate( bsdpart(nsoil) )
  allocate( bsfcla(nsoil) )
  allocate( bsfsan(nsoil) )
  allocate( bsfom(nsoil) )
  allocate( bsfcec(nsoil) )
  allocate( bhrwcs(nsoil) )
  allocate( bhrwcf(nsoil) )
  allocate( bhrwcw(nsoil) )
  allocate( bhrwcr(nsoil) )
  allocate( bhrwca(nsoil) )
  allocate( bh0cb(nsoil) )
  allocate( bheaep(nsoil) )
  allocate( bhrsk(nsoil) )
  allocate( bhfredsat(nsoil) )

  ! populate input values
  idx = 1  ! sand
  soilname(idx) = 'S'
  bszlyd(idx) = 100
  bsdblk(idx) = 1.64
  bsfcla(idx) = 0.03
  bsfsan(idx) = 0.93
  bsfom(idx) = 0.015
  bsfcec(idx) = 4.5

  idx = 2  ! loamy sand
  soilname(idx) = 'LS'
  bszlyd(idx) = 100
  bsdblk(idx) = 1.64
  bsfcla(idx) = 0.05
  bsfsan(idx) = 0.83
  bsfom(idx) = 0.015
  bsfcec(idx) = 5.5

  idx = 3  ! sandy loam
  soilname(idx) = 'SL'
  bszlyd(idx) = 100
  bsdblk(idx) = 1.55
  bsfcla(idx) = 0.11
  bsfsan(idx) = 0.65
  bsfom(idx) = 0.015
  bsfcec(idx) = 8.5

  idx = 4  ! loam
  soilname(idx) = 'L'
  bszlyd(idx) = 100
  bsdblk(idx) = 1.43
  bsfcla(idx) = 0.18
  bsfsan(idx) = 0.41
  bsfom(idx) = 0.015
  bsfcec(idx) = 12.0

  idx = 5  ! silt loam
  soilname(idx) = 'SiL'
  bszlyd(idx) = 100
  bsdblk(idx) = 1.44
  bsfcla(idx) = 0.13
  bsfsan(idx) = 0.21
  bsfom(idx) = 0.015
  bsfcec(idx) = 9.5

  idx = 6  ! sandy clay loam
  soilname(idx) = 'SCL'
  bszlyd(idx) = 100
  bsdblk(idx) = 1.41
  bsfcla(idx) = 0.27
  bsfsan(idx) = 0.61
  bsfom(idx) = 0.015
  bsfcec(idx) = 8.0

  idx = 7  ! clay loam
  soilname(idx) = 'CL'
  bszlyd(idx) = 100
  bsdblk(idx) = 1.32
  bsfcla(idx) = 0.33
  bsfsan(idx) = 0.33
  bsfom(idx) = 0.015
  bsfcec(idx) = 19.5

  idx = 8  ! silty clay loam
  soilname(idx) = 'SiCL'
  bszlyd(idx) = 100
  bsdblk(idx) = 1.28
  bsfcla(idx) = 0.33
  bsfsan(idx) = 0.11
  bsfom(idx) = 0.015
  bsfcec(idx) = 19.5

  idx = 9  ! sandy clay
  soilname(idx) = 'SC'
  bszlyd(idx) = 100
  bsdblk(idx) = 1.33
  bsfcla(idx) = 0.40
  bsfsan(idx) = 0.53
  bsfom(idx) = 0.015
  bsfcec(idx) = 23.0

  idx = 10  ! silty clay
  soilname(idx) = 'SiC'
  bszlyd(idx) = 100
  bsdblk(idx) = 1.22
  bsfcla(idx) = 0.45
  bsfsan(idx) = 0.07
  bsfom(idx) = 0.015
  bsfcec(idx) = 25.5

  idx = 11  ! clay
  soilname(idx) = 'C'
  bszlyd(idx) = 100
  bsdblk(idx) = 1.21
  bsfcla(idx) = 0.65
  bsfsan(idx) = 0.18
  bsfom(idx) = 0.015
  bsfcec(idx) = 35.5

  idx = 12  ! pure sand
  soilname(idx) = 'PS'
  bszlyd(idx) = 100
  bsdblk(idx) = 1.64
  bsfcla(idx) = 0.0000001
  bsfsan(idx) = 1.0
  bsfom(idx) = 0.0
  bsfcec(idx) = 0.0000001

  do idx = 1, nsoil
    wet_set_rat(idx) = -1.0
  end do

  ! find the particle density adjusted for organic matter
  call proptext( nsoil, bsfcla, bsfsan, bsfom, bsdblk, bsdsblk, bsdpblk, bsdwblk, wet_set_rat, bsdpart )

  call param_prop_bc( nsoil, bszlyd, bsdblk, bsdpart, bsfcla, bsfsan, bsfom, bsfcec, &
                      bhrwcs, bhrwcf, bhrwcw, bhrwcr, bhrwca, bh0cb, bheaep, bhrsk, bhfredsat )

  ! print out results
  ! to match Rawls table, potential converted from m to cm and conductivity converted from m/s to cm/hr
  write(*,*) 'Texture tot_porosity residual eff_porosity bub_press pore_size_dist -0.33bar -15bar k_sat'
  do idx = 1, nsoil
    write(*,*) soilname(idx), bhrwcs(idx)*bsdblk(idx), bhrwcr(idx)*bsdblk(idx), bhrwcs(idx)*bsdblk(idx)-bhrwcr(idx)*bsdblk(idx), &
               100*bheaep(idx)/gravconst, 1.0/bh0cb(idx), bhrwcf(idx)*bsdblk(idx), bhrwcw(idx)*bsdblk(idx), 100*3600*bhrsk(idx)
  end do


end program test_param_prop_bc
