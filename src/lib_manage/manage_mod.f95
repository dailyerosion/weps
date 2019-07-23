!$Author$
!$Date$
!$Revision$
!$HeadURL$

module manage_mod

    use crop_data_struct_defs, only: crop_residue, create_crop_residue, destroy_crop_residue

    type(crop_residue) :: cropres  ! structure for temporary crop

    character(len=80), private :: cropname
    character(len=80), private :: amdname
    real, private :: fracarea  ! fraction of the surface affected by the process
    real, private :: imprs     ! implement ridge spacing (can be used to set row spacing)
    real, private :: ospeed    ! operation speed (m/s)
    real, private :: odir      ! operation direction (degrees from NORTH)
    real, private :: ostdspeed
    real, private :: ominspeed
    real, private :: omaxspeed
    real, private :: tdepth
    real, private :: ti
    real, private :: tstddepth
    real, private :: tmindepth
    real, private :: tmaxdepth
    integer, private :: tlayer
    integer, private :: rdgflag

    logical :: am0til  ! flag to determine if surfce has been updated by management
                       ! .true. - tillage has occurred
                       ! .false. - not

  contains

    subroutine mfinit (manFile)
!
!     + + + PURPOSE + + +
!     Mfinit should be called during the initialization stage of the the
!     main weps program. Mfinit searches the management data file; marking
!     the start sections of each subregion, while storing the number of
!     years in each subregion's management cycle.
!
!
!       Edit History
!       19-Feb-99       wjr     rewrote
!
!     + + + KEYWORDS + + +
!     tillage, management file, initialization
!
!     + + + PARAMETERS AND COMMON BLOCKS + + +

      use file_io_mod, only: fopenk
      use manage_data_struct_defs, only: man_file_struct, operation_date
      use flib_sax
      use manage_xml_mod, only: init_man_xml, read_old_manfile
      use manage_xml_mod, only: manfile_complete
      use manage_xml_mod, only: begin_man_element_handler, end_man_element_handler, pcdata_man_chunk_handler
      use update_mod, only: am0cropupfl

!     + + + ARGUMENT DECLARATIONS + + +
      type(man_file_struct), intent(inout) :: manFile  ! management file data structure

!     + + + LOCAL VARIABLES + + +
      integer :: luimandate   ! unit number for reading in management file
      character*256 :: line

      type(xml_t) :: fxml   ! xml file handle structure
      integer :: read_stat  ! reading file status

!     + + + DATA INITIALIZATIONS + + +

      ! initialize value for crop effect flags
      am0cropupfl = .false.

      manFile%rpt_season_flg = .true.

!     + + + END SPECIFICATIONS + + +

!     read in management file

      call fopenk(luimandate, trim(manFile%tinfil), 'old')
      read(luimandate, '(a)', iostat=read_stat) line
      if (read_stat /= 0) then
        stop "Cannot read input file"
      end if

      call init_man_xml( manFile%isub )
      if ( (line (1:8).ne.'Version: ') .and. (index(line, 'xml') .gt. 0) ) then
        close(luimandate)
        ! open input file
        call open_xmlfile(trim(manFile%tinfil),fxml,read_stat)
        if (read_stat /= 0) stop "Cannot open xml input file"
        ! read in xml based input file
        call xml_parse(fxml, &
           begin_element_handler = begin_man_element_handler, &
           end_element_handler = end_man_element_handler, &
           pcdata_chunk_handler = pcdata_man_chunk_handler, &
           verbose = .false.)
        if (.not. manfile_complete) then
          write(*,*) 'Management file incomplete: ', trim(manFile%tinfil)
          call exit(1)
        end if
      else
        call read_old_manfile ( manFile%isub, luimandate )
      end if

      ! init flag calibration of crops with multiple harvests.
      manFile%harv_calib_not_selected = .true.
      ! init rotation counter
      manFile%mcount = 0
      ! init rotation year counter
      manfile%mnryr = 1

      return

    end subroutine mfinit

    subroutine tdbug(sr, output, soil, plant)

!     + + + PURPOSE + + +
!    This program prints out many of the global variables before
!    and after the call to various MANAGEMENT practices

!     + + + KEY WORDS + + +
!     wind, erosion, tillage, soil, plant, decomposition
!     management

      use file_io_mod, only: luotdb
      use soil_data_struct_defs, only: soil_def
      use biomaterial, only: plant_pointer, residue_pointer

!     + + + ARGUMENT DECLARATIONS + + +
      integer sr, output
      type(soil_def), intent(in) :: soil  ! soil for this subregion
      type(plant_pointer), pointer :: plant ! pointer to youngest plant data, which chains to older plant data

!     + + + ARGUMENT DEFINITIONS + + +
!     sr      - subregion number
!     output  - process number for debugging output

!     + + + LOCAL VARIABLES + + +
      integer :: idx  ! loop counter for printing output
      integer :: jdx  ! loop counter for printing output
      real :: total   ! summation variable
      type(plant_pointer), pointer :: thisPlant
      type(residue_pointer), pointer :: thisResidue

!     + + + END SPECIFICATIONS + + +

      select case (output)
      case (1) ! crust breakdown process (process code 01)

      case (2) ! random roughness process (process code 02)
 2067     format('aslrr') 
 2062     format (f7.2)
          write(luotdb(sr),2067)
          write(luotdb(sr),2062) soil%aslrr

      case (3) ! oriented roughness ridge only process (process code 03)
 2070     format(3x,'aszrgh asxrgw asxrgs asargo asxdks asxdkh')
 2071     format (4x,6(2x,f7.3))
          write(luotdb(sr),2070)
          write(luotdb(sr),2071) soil%aszrgh, soil%asxrgw, soil%asxrgs, &
            soil%asargo, soil%asxdks, soil%asxdkh

      case (4) ! oriented roughness process dike only (process code 04)
          write(luotdb(sr),2070)
          write(luotdb(sr),2071) soil%aszrgh, soil%asxrgw, soil%asxrgs, &
            soil%asargo, soil%asxdks, soil%asxdkh

      case (5) ! oriented roughness process (process code 05)
 2072     format(3x,'asfcr  asflos')
 2073     format (1x,2f7.3)
          write(luotdb(sr),2072)
          write(luotdb(sr),2073) soil%asfcr, soil%asflos
          write(luotdb(sr),2070)
          write(luotdb(sr),2071) soil%aszrgh, soil%asxrgw, soil%asxrgs, &
            soil%asargo, soil%asxdks, soil%asxdkh

      case (11) ! crushing process (process code 11)
 2040     format(3x,'aslagn aslagx aslagm as0ags') 
 2050     format (1x,4f7.2)
          write(luotdb(sr),2040) 
          do idx = 1,soil%nslay
            write(luotdb(sr),2050) soil%aslagn(idx),  soil%aslagx(idx), &
               soil%aslagm(idx),  soil%as0ags(idx) 
          end do

      case (12) ! loosening process (process code 12)
 2041     format(3x,'asdblk  asdsblk   aszlyt') 
 2051     format (1x,f7.2,2x,f7.2,2x,f7.2)
          write(luotdb(sr),2041) 
          do idx = 1,soil%nslay
            write(luotdb(sr),2051) &
            soil%asdblk(idx), soil%asdsblk(idx), soil%aszlyt(idx)
          end do 

      case (13) ! mixing process (process code 13)
          write(luotdb(sr),*) '   layer asdblk aszlyt sfsan asfsil asfcla as0ph  ascmg ascna asfcce asfcec asfesp'
          do idx = 1,soil%nslay
            write(luotdb(sr),'(1x,i4,1x,f7.2,1x,f7.2,f6.2,5f7.2)') idx, soil%asdblk(idx), soil%aszlyt(idx), &
              soil%asfsan(idx), soil%asfsil(idx), soil%asfcla(idx), &
              soil%as0ph(idx), soil%asfcce(idx), soil%asfcec(idx)
          end do 
          write(luotdb(sr),*) '   asfom asfnoh asfpoh asfpsp asfsmb asdagd aseags ahrwc aheaep ahrwcw ahrwcf ahrwca ahrwcs'
          do idx = 1,soil%nslay
            write(luotdb(sr),'(4x,i1,6(1x,f8.4))') soil%asfom(idx), &
              soil%asdagd(idx), soil%aseags(idx), soil%ahrwc(idx), &
              soil%aheaep(idx), soil%ahrwcw(idx), soil%ahrwcf(idx), &
              soil%ahrwca(idx), soil%ahrwcs(idx)
          end do 

          if( associated(plant%residue) ) then
            write(luotdb(sr),*) '   layer plant%residue%deriv%mrtz(s)  plant%residue%deriv%mbgz(s)'
            do idx = 1,soil%nslay
              write(luotdb(sr),'(4x,i1,6(1x,f8.4))') idx, plant%residue%deriv%mrtz(idx), plant%residue%deriv%mbgz(idx)
            end do 
          else
            write(luotdb(sr),*) 'No residue'
          end if

      case (14) ! inversion process (process code 14)
          do idx = 1,soil%nslay
            write(luotdb(sr),'(1x,i4,1x,f7.2,1x,f7.2,f6.2,5f7.2)') idx, soil%asdblk(idx), soil%aszlyt(idx), &
              soil%asfsan(idx), soil%asfsil(idx), soil%asfcla(idx), &
              soil%as0ph(idx), soil%asfcce(idx), soil%asfcec(idx)
          end do 
          write(luotdb(sr),*) '   asfom asfnoh asfpoh asfpsp asfsmb asdagd aseags ahrwc aheaep ahrwcw ahrwcf ahrwca ahrwcs'
          do idx = 1,soil%nslay
            write(luotdb(sr),'(4x,i1,6(1x,f8.4))') soil%asfom(idx), &
              soil%asdagd(idx), soil%aseags(idx), soil%ahrwc(idx), &
              soil%aheaep(idx), soil%ahrwcw(idx), soil%ahrwcf(idx), &
              soil%ahrwca(idx), soil%ahrwcs(idx)
          end do 

      case (21) ! below layer compaction (process code 21)

      case (24) ! flatten process variable toughness (process code 24)

      case (25) ! mass bury process variable toughness (process code 25)
 2500     format ('pool stem leaf store rootstore rootfiber (all flat)')
 2501     format ( i2, 5(1x, f7.4) )
 2502     format ( 2(1x,i2), 5(1x, f7.4) )
          ! sum pools to get total flat mass
          total = cropres%flatstem + cropres%flatleaf + cropres%flatstore &
                + cropres%flatrootstore + cropres%flatrootfiber
          thisPlant => plant
          do while( associated(thisPlant) )
            thisResidue => thisPlant%residue
            do while( associated(thisResidue) )
              total = total + thisResidue%flatstem + thisResidue%flatleaf &
                    + thisResidue%flatstore + thisResidue%flatrootstore &
                    + thisResidue%flatrootfiber
              thisResidue => thisResidue%olderResidue
            end do
            thisPlant => thisPlant%olderPlant
          end do 
          write(luotdb(sr),*) total, ' total flat mass'
          write(luotdb(sr),2500)
          write(luotdb(sr),2501) 0, cropres%flatstem, cropres%flatleaf, &
            cropres%flatstore, cropres%flatrootstore, cropres%flatrootfiber
          idx = 0
          thisPlant => plant
          do while( associated(thisPlant) )
            idx = idx + 1
            jdx = 0
            thisResidue => thisPlant%residue
            do while( associated(thisResidue) )
              jdx = jdx + 1
              write(luotdb(sr),2502) idx, jdx, thisResidue%flatstem, &
              thisResidue%flatleaf, thisResidue%flatstore, &
              thisResidue%flatrootstore, thisResidue%flatrootfiber
              thisResidue => thisResidue%olderResidue
            end do
            thisPlant => thisPlant%olderPlant
          end do 

      case (26) ! re-surface process variable toughness (process code 26)
          ! sum pools to get total flat mass
          total = cropres%flatstem + cropres%flatleaf + cropres%flatstore &
                + cropres%flatrootstore + cropres%flatrootfiber
          thisPlant => plant
          do while( associated(thisPlant) )
            thisResidue => thisPlant%residue
            do while( associated(thisResidue) )
              total = total + thisResidue%flatstem + thisResidue%flatleaf &
                    + thisResidue%flatstore + thisResidue%flatrootstore &
                    + thisResidue%flatrootfiber
              thisResidue => thisResidue%olderResidue
            end do
            thisPlant => thisPlant%olderPlant
          end do 
          write(luotdb(sr),*) total, ' total flat mass'
          write(luotdb(sr),2500)
          write(luotdb(sr),2501) 0, cropres%flatstem, cropres%flatleaf, &
            cropres%flatstore, cropres%flatrootstore, cropres%flatrootfiber
          idx = 0
          thisPlant => plant
          do while( associated(thisPlant) )
            idx = idx + 1
            jdx = 0
            thisResidue => thisPlant%residue
            do while( associated(thisResidue) )
              jdx = jdx + 1
              write(luotdb(sr),2502) idx, jdx, thisResidue%flatstem, &
              thisResidue%flatleaf, thisResidue%flatstore, &
              thisResidue%flatrootstore, thisResidue%flatrootfiber
              thisResidue => thisResidue%olderResidue
            end do
            thisPlant => thisPlant%olderPlant
          end do 

      case (31) ! killing process (process code 31)

      case (32) ! cutting to height process (process code 32)

      case (33) ! cutting by fraction process (process code 33)

      case (34) ! modify standing fall rate process variable toughness (process code 34)

          if( associated(plant%residue) ) then
            write(luotdb(sr),*) '   layer plant%residue%deriv%mrtz(s)  plant%residue%deriv%mbgz(s)'
            do idx = 1,soil%nslay
              write(luotdb(sr),'(4x,i1,2(1x,f8.4))') idx, plant%residue%deriv%mrtz(idx), plant%residue%deriv%mbgz(idx)
            end do 
            write(luotdb(sr),*) '   plant%residue%deriv%mf plant%residue%deriv%mst'
            write(luotdb(sr),'(2(2x,f7.3))') plant%residue%deriv%mf, plant%residue%deriv%mst
          else
            write(luotdb(sr),*) 'No residue'
          end if

      case (37) ! thinning to population process (process code 37)

      case (38) ! thinning by fraction process (process code 38)

      case (40) ! crop to biomass transfer process (process code 40)

      case (50) ! residue initialization process (process code 50)

      case (51) ! planting process (process code 51)

      case (61) ! biomass remove process (process code 61)
 2164     format (3x,3f7.3)
 2169     format(4x,'acmyld  aczht  aczrtd')
 2269     format(4x,'residue()%deriv%fscv  residue()%deriv%ffcv ')
          write(luotdb(sr),2169)
          write(luotdb(sr),2164) plant%mass%standstore, plant%geometry%zht, plant%geometry%zrtd
          write(luotdb(sr),2269)

          if( associated(plant%residue) ) then
            write(luotdb(sr),2073) plant%residue%deriv%fscv, plant%residue%deriv%ffcv
          else
            write(luotdb(sr),*) 'No residue'
          end if 

      case (62) ! biomass remove pool process (process code 62)
 6200     format ( a2, 9(1x, f7.4) )
 6201     format ( i2, 9(1x, f7.4) )
          write(luotdb(sr),*) 'pool stand(height stem leaf store)', &
                      'flat(stem leaf store rootstore rootfiber)' 
          write(luotdb(sr),6200) 'T', cropres%zht, cropres%standstem, &
              cropres%standleaf, cropres%standstore, &
              cropres%flatstem, cropres%flatleaf, &
            cropres%flatstore, cropres%flatrootstore, cropres%flatrootfiber

          if( associated(plant%residue) ) then
            write(luotdb(sr),6201) idx, plant%residue%zht, plant%residue%standstem,&
              plant%residue%standleaf, plant%residue%standstore, &
              plant%residue%flatstem, plant%residue%flatleaf, &
              plant%residue%flatstore, plant%residue%flatrootstore, &
              plant%residue%flatrootfiber
          else
            write(luotdb(sr),*) 'No residue'
          end if 

      case (65) ! add residue process (process code 65)

      case (71) ! irrigate process (process code 71) (OBSOLETE)

      case (72) ! irrigation monitoring process (process code 72)

      case (73) ! single event irrigation process (process code 73)

      case default
      end select

      return

    end subroutine tdbug

    subroutine dooper (manFile)

!     + + + PURPOSE + + +
!     Dooper reads in any coefficients associated with the
!     operation.

!     + + + KEYWORDS + + +
!     tillage, operation, management

      use weps_cmdline_parms, only: report_info
      use manage_data_struct_defs, only: lastoper, man_file_struct 
      use manage_data_struct_mod, only: getManVal

!     + + + ARGUMENT DECLARATIONS + + +
      type(man_file_struct), intent(in) :: manFile

!     + + + LOCAL VARIABLES + + +
      integer :: sr  ! the subregion being processed

!     + + + DATA INITIALIZATIONS + + +
      sr = manFile%isub

!     + + + END SPECIFICATIONS + + +

      lastoper(sr)%code = manFile%oper%operType
      lastoper(sr)%name = manFile%oper%operName
      if( (lastoper(sr)%code.eq.0).and.(manFile%mcount.gt.0) ) then
          lastoper(sr)%skip = 1
          print*, 'SR',sr,' Skip operation', lastoper(sr)%code,' ', trim(lastoper(sr)%name)
      else
          if (report_info >= 1) then
            print*, 'SR',sr,' Do operation', lastoper(sr)%code,' ', trim(lastoper(sr)%name)
          end if
      end if

      ! set value of tlayer to zero before operation begins. Compaction occurs from tlayer
      ! downward, so operations without tillage need this set to zero to model surface compaction.
      tlayer = 0

      ! assign default fuel as blank.  Treated as default in reports
      lastoper(sr)%fuel = ''

      select case (lastoper(sr)%code)

      case (1)  ! original ground engaging operation
          ! set energy and stir values to default
          lastoper(sr)%energyarea = -1
          lastoper(sr)%stir = -1
          ! read tillage speed and direction
          call getManVal(manFile%oper, 'ospeed', ospeed)
          call getManVal(manFile%oper, 'odirect', odir)
          call getManVal(manFile%oper, 'ostdspeed', ostdspeed)
          call getManVal(manFile%oper, 'ominspeed', ominspeed)
          call getManVal(manFile%oper, 'omaxspeed', omaxspeed)

      case (3) ! added energy and stir to O1
          ! read tillage speed and direction
          call getManVal(manFile%oper, 'oenergyarea', lastoper(sr)%energyarea)
          call getManVal(manFile%oper, 'ostir', lastoper(sr)%stir)
          call getManVal(manFile%oper, 'ospeed', ospeed)
          call getManVal(manFile%oper, 'odirect', odir)
          call getManVal(manFile%oper, 'ostdspeed', ostdspeed)
          call getManVal(manFile%oper, 'ominspeed', ominspeed)
          call getManVal(manFile%oper, 'omaxspeed', omaxspeed)
          ! Version 1.5 added ofuel
          if (manFile%mversion .ge. 1.50) then
            ! get fuel line
            call getManVal(manFile%oper, 'ofuel', lastoper(sr)%fuel)
          end if

      case (4) ! added energy and stir to O2
          ! read tillage speed and direction
          call getManVal(manFile%oper, 'oenergyarea', lastoper(sr)%energyarea)
          call getManVal(manFile%oper, 'ostir', lastoper(sr)%stir)
          ! Version 1.5 added ofuel
          if (manFile%mversion .ge. 1.50) then
            ! get fuel line
            call getManVal(manFile%oper, 'ofuel', lastoper(sr)%fuel)
          end if

      case default
          ! set energy and stir values to default
          lastoper(sr)%energyarea = -1
          lastoper(sr)%stir = -1
          ! set fuel to blank (default)
          lastoper(sr)%fuel = ''
      end select

      ! initialize row spacing and ridge flag to zero. They are needed
      ! by P51, (set in P3 or P5) but may be set and not cleared by a previous operation.
      imprs = 0.0
      rdgflag = 0

      return

    end subroutine dooper

    subroutine dogroup (soil, plant, plantIndex, manFile)

!     + + + PURPOSE + + +
!     Dogroup reads in any coefficients associated with the group of
!     processes. 

!     + + + KEYWORDS + + +
!     tillage, operation, management

      use manage_data_struct_defs, only: lastoper, man_file_struct
      use soil_data_struct_defs, only: soil_def
      use manage_data_struct_mod, only: getManVal
      use biomaterial, only: plant_pointer, plantAdd
      use datetime_mod, only: get_simdate

!     + + + ARGUMENT DECLARATIONS + + +
      type(soil_def), intent(in) :: soil  ! soil for this subregion
      type(plant_pointer), pointer :: plant     ! pointer to youngest plant data, which chains to older plant data
      integer, intent(inout) :: plantIndex      ! index used for detailed plant/residue output
      type(man_file_struct), intent(in) :: manFile

!     + + + LOCAL VARIABLES + + +
      integer :: sr  ! the subregion being processed

!     + + + DATA INITIALIZATIONS + + +
      sr = manFile%isub

!     + + + END SPECIFICATIONS + + +

      lastoper(sr)%grcode = manFile%grp%grpType
      lastoper(sr)%grname = manFile%grp%grpName

      select case (lastoper(sr)%grcode)

      case (0)  ! null group does nothing

      case (1)  ! tillage group
        ! read tillage depth, intensity and area
        call getManVal(manFile%grp, 'gtdepth', tdepth)
        call getManVal(manFile%grp, 'gtilint', ti)
        call getManVal(manFile%grp, 'gtilArea', fracarea)
        call getManVal(manFile%grp, 'gtstddepth', tstddepth)
        call getManVal(manFile%grp, 'gtmindepth', tmindepth)
        call getManVal(manFile%grp, 'gtmaxdepth', tmaxdepth)

        tlayer = tillay(tdepth, soil%aszlyt, soil%nslay)

      case (2)  ! biomass manipulation group
        ! read biomass area affected
        call getManVal(manFile%grp, 'gbioarea', fracarea)

      case (3) ! grow group
        ! create plant
        plant => plantAdd(plant, plantIndex, soil%nslay)

        ! read crop name
        call getManVal(manFile%grp, 'gcropname', cropname)

        plant%bname = cropname
        call get_simdate( plant%pday, plant%pmon, plant%psimyr )

      case (4) ! ammend group
        ! read amendment name
        call getManVal(manFile%grp, 'gamdname', amdname)

      case default
        write(0, *) 'Invalid Group: ', lastoper(sr)%grcode, &
                                       manFile%grp%grpName
        call exit (1)

      end select

      return

    end subroutine dogroup

    subroutine doproc (soil, plant, biotot, mandate, hstate, h1et, manFile)

!     + + + PURPOSE + + +
!     Doproc is called when a processline is found in the management file
!     Doproc reads in any coefficients associated with the
!     process. Doproc then makes a call to a subroutine which, in turn,
!     modifies the state variables to mimic the processes of doing the
!     process.

!     + + + KEYWORDS + + +
!     tillage, process, management

      use weps_cmdline_parms, only: cook_yield, resurf_roots, upgm_growth, wc_type
      use file_io_mod, only: luomanage, luotdb, luoasd, luowc
      use soil_data_struct_defs, only: soil_def
      use input_soil_mod, only: proptext
      use biomaterial, only: plant_pointer, biototal
      use biomaterial, only: plantDestroy, residueAdd, residueDestroyAll
      use mandate_mod, only: opercrop_date
      use p1unconv_mod, only: mmtom
      use manage_data_struct_defs, only: lastoper, man_file_struct
      use crop_data_struct_defs, only: am0cfl
      use soilden_mod, only: setbdproc_wc
      use hydro_data_struct_defs, only: hydro_derived_et, hydro_state
      use hydro_util_mod, only: param_blkden_adj, param_pot_bc, param_prop_bc
      use hydro_main_mod, only: ratedura
      use soil_mod, only: depthini
      use crop_mod, only: plant_endseason
      use report_harvest_mod, only: report_harvest, report_calib_harvest
      use report_hydrobal_mod, only: report_hydrobal
      use datetime_mod, only: get_simdate, get_simdate_jday, get_simdate_doy, dayear
      use manage_data_struct_mod, only: getManVal
      use asd_mod, only: msieve, nsieve, sdia, mdia, asd2m, m2asd
      use mproc_bio_mod, only: mnrbc, flatvt, fall_mod_vt, liftvt, mburyvt, kill_plant, defoliate, buryadj, resinit
      use mproc_cut_mod, only: cut
      use mproc_thin_mod, only: thin
      use mproc_remove_mod, only: remove
      use mproc_soil_mod, only: mix, invert, loosn, compact, crush, set_asd, set_wc
      use mproc_surface_mod, only: crust, rough, orient
      use calib_plant_mod, only: get_calib_crops, get_calib_yield, set_calib
      use update_mod, only: am0cropupfl
      use WEPS_UPGM_mod, only: init_WEPS_UPGM
      use upgm_mod
      use constants, only : dp, int32

!     + + + ARGUMENT DECLARATIONS + + +
      type(soil_def), intent(inout) :: soil  ! soil for this subregion
      type(plant_pointer), pointer :: plant     ! pointer to youngest plant data, which chains to older plant data
      type(biototal), intent(in) :: biotot
      type(opercrop_date), dimension(:), intent(inout) :: mandate
      type(hydro_state), intent(inout) :: hstate
      type(hydro_derived_et), intent(inout) :: h1et
      type(man_file_struct), intent(inout) :: manFile

!     + + + ACCESSED COMMON BLOCK VARIABLE DEFINITIONS + + +

!     acdpop    - crop seeding density
!     acrlai    - crop leaf area index
!     aheaep    - soil air entery potential
!     ahrwc     - soil water content (mass bases)
!     ahrwca    - available soil water content
!     ahrwcf    - 1/3 bar soil water content
!     ahrwcs    - saturation soil water content
!     ahrwcw    - 15 bar soil water content
!     as0ags    - aggr. size geom. mean std. dev.
!     as0ph     - soil Ph
!     asargo    - ridge orientation (clockwise from true North) (degrees)
!     ascmg     - magnesium ion concentration
!     ascna     - sodium ion concentration
!     asdadg    - aggregrate density
!     asdblk    - soil layer bulk density
!     aseags    - dry aggregrate stability
!     asfcce    - fraction of calcium carbonate
!     asfcec    - cation exchange capcity
!     asfcla    - fraction of clay
!     asfesp    - exchangable sodium percentage
!     asfnoh    - organic N concentration of humus
!     asfom     - fraction of organic matter
!     asfpoh    - organic P concentration of humus
!     asfpsp    - fraction of fertilizer P that is labile
!     asfsan    - fraction of sand
!     asfsil    - fraction of silt
!     asfsmb    - sum of bases
!     aslagm    - aggr. size geom. mean diameter (mm)
!     aslagn    - min. aggr. size of each layer (mm)
!     aslagx    - max aggr. size of each layer (mm)
!     aslrr     - Allmaras random roughness parameter (mm)
!     asxrgs    - ridge spacing (mm)
!     asxrgw    - ridge width (mm)
!     aszlyt    - soil layer thickness (mm)
!     aszrgh    - ridge height (mm)

!     + + + LOCAL VARIABLES + + +
      integer :: sr  ! the subregion being processed
      integer cutflg
      real    alpha, beta, mu, rho
      integer roughflg
      real    rrimpl
      real    kappa
      real    thinval
      real    pyieldf, pstalkf, rstandf
      integer harv_report_flg, harv_calib_flg, harv_unit_flg
      integer mature_warn_flg
      integer sel_position, sel_pool
      real    stemf, leaff, storef, rootstoref, rootfiberf
      real    rdght,rdgwt,dikeht,dikespac
      real    afvt(mnrbc), mfvt(mnrbc)
      integer burydistflg
      real    irrig
      integer  idx, thinflg
      real    dmassres, zmassres, dmassrot, zmassrot
      real    mass_rem, mass_left
      integer crop_present
      real    rate_mult_vt(mnrbc), thresh_mult_vt(mnrbc)
      ! temporary crop parameter values for process 66 only
      real    manure_buried_fraction, manure_total_mass
      real :: compact_load  ! 
      real, dimension(:), allocatable :: procbdadj

      integer :: cd, cm, cy
      integer :: i,j

      real :: gmdx, gsdx  ! transformed geometric mean dia. (mm) and geometric std. deviation (mm/mm)
      real :: mnot, minf  ! max and min aggregate size values of aggregate size distribution (mm)
      real :: asddepth    ! Depth (mm) of soil to apply "set_asd" parameters
      integer :: asdlayer ! Number of soil layers to apply "set_asd" parameters
      real :: asd_tdepth  ! Computed depth (mm) to bottom of all soil layers affected by set_asd()

      real :: wc          ! water content value (Mg/Mg)
      real :: wcdepth     ! Depth (mm) of soil to apply "set_wc" parameters
      integer :: wclayer  ! Number of soil layers to apply "set_wc" parameters
      real :: wc_tdepth   ! Computed depth (mm) to bottom of all soil layers affected by set_wc()

      integer :: prcode   ! process id number
      character*30 :: prname ! process name
      integer :: bioflg   ! bioflg - flag indicating what to manipulate
                          ! 0 - All standing material is manipulated (both crop and residue)
                          ! 1 - Crop
                          ! 2 - 1'st residue pool
                          ! 4 - 2'nd residue pool
                          ! ....
                          ! 2**n - nth residue pool
      real, dimension (:,:), allocatable :: massf ! (msieve+1,soil%nslay)
      integer :: alloc_stat  ! return status of memory allocation, deallocation
      integer :: am0kilfl  ! flag to determine if an operation is killing a perennial
                           ! or annual crop. Also used to indicate leaf removal (defoliation) as of 8/23/00.
                           ! 0 - does not kill anything
                           ! 1 - kills annual crop, but not perennial
                           ! 2 - kills annual and perennial crop
                           ! 3 - leaves killed and dropped to ground (defoliation)
      type(plant_pointer), pointer :: thisPlant  ! pointer for interating through plant list
      type(plant_pointer), pointer :: harvPlant  ! pointer to the most recent harvestable plant

      logical :: succ      ! return value for JSON name assignment
      real(dp) :: r_setter
      real(dp), dimension(:), allocatable :: ra_setter
      integer(int32) :: i_setter
      logical :: l_setter

!     + + + LOCAL VARIABLE DEFINITIONS + + +

!     alpha    - parameter reflecting the breakage of all soil
!                aggregrates regardless of size
!     beta     - parameter reflecting the uneveness of breakage among
!                aggregrates in different size classes
!     buryf    - fraction of mass to be buried
!     kappa    - fraction of the crust destroyed during a tillage operation
!     dikeht   - dike height (mm)
!     dikespac - dike spacing (mm)
!     fltcoef  - flattening coefficient of an implement
!     pyieldf  - fraction of crop and residue above ground plant reproductive mass removed
!     pstalkf  - fraction of crop stems, leaves and remaining reproductive mass removed
!     rstandf  - fraction of residue stems, leaves and remaining reproductive mass removed
!     harv_report_flg - place in harvest report flag
!                0 - do not place in harvest report
!                1 - place in harvest report
!     harv_calib_flg - Use harvested biomass in calibration flag
!                0 - do not use harvest in calibration
!                1 - use harvest amount in calibration
!     harv_unit_flg - overide units given in crop record
!                0  - use units given in crop record
!                1  - use lb/ac or kg/m^2
!     mature_warn_flg - flag to indicate use of crop maturity warning
!                0  - no crop maturity warning given for any crop
!                1  - Warnings generated for any crop unless supressed by crop type
!     sel_position - position to which percentages will be applied
!                0 - don't apply to anything
!                1 - apply to standing (and attached roots)
!                2 - apply to flat
!                3 - apply to standing (and attached roots) and flat
!                4 - apply to buried
!                5 - apply to standing (and attached roots) and buried
!                6 - apply to flat and buried
!                7 - apply to standing (and attached roots), flat and buried
!                this corresponds to the bit pattern:
!                msb(buried, flat, standing)lsb
                
!     sel_pool - pool to which percentages will be applied
!            0 - don't apply to anything
!            1 - apply to crop pool
!            2 - apply to temporary pool
!            3 - apply to crop and temporary pools
!            4 - apply to residue
!            5 - apply to crop and residue pools
!            6 - apply to temporary and residue pools
!            7 - apply to crop, temporary and residue pools
!                this corresponds to the bit pattern:
!                msb(residue, temporary, crop)lsb

!     storef   - fraction of storage (reproductive components) removed (kg/kg)
!     leaff    - fraction of plant leaves removed (kg/kg)
!     stemf    - fraction of plant stems removed (kg/kg)
!     rootstoref - fraction of plant storage root removed (kg/kg)
!     rootfiberf - fraction of plant fibrous root removed (kg/kg)
!     harvflag - flag indicating a harvest
!     intens   - tillage intensity factor
!     liftf    - fraction of mass to be lifted
!     massf    - mass fractions of aggregrates within sieve cuts
!                 (sum of all the mass fractions are expected to be 1.0)
!     rdght    - ridge height (mm)
!     rdgwt    - ridge top width (mm)
!     rrimpl   - assigned nominal RR value for the tillage operation (mm)
!     mu       - loosening coefficient (0 <= mu <= 1)
!     rho      - mixing coefficient (0 <= rho <= 1)
!     irrig    - irrigation quantity for a day (mm)
!     dmassres - Buried crop residue mass(kg/m^2)
!     zmassres - depth in soil of Buried crop residue mass (mm)
!     dmassrot - Buried root residue mass(kg/m^2)
!     zmassrot - depth in soil of Buried root residue mass (mm)
!     mass_rem - mass removed by harvest process (cut,remove)
!     mass_left - mass left behind in pool which mass was removed from by harvest process (cut,remove)
!     crop_present - flag to show crop biomass pool status
!                0 - no crop biomass present
!                1 - crop biomass present
!     rate_mult_vt - array of multipliers for modifying standing stem fall rate
!     thresh_mult_vt - array of multipliers for modifying standing stem fall threshold

!     manure_total_mass - total mass of manure added to field (dry weight)
!     manure_buried_fraction - fraction of total manure applied that is buried

!     gmd        - geometric mean dia. (mm)
!     gsd        - geometric std. deviation (mm/mm)

!     + + + SUBROUTINES CALLED + + +
!
!     asd2m     - aggregate size distribution to mass fraction converter
!     crush     - the crushing process
!     crust     - destroys a crusted surface depending on the operation that
!                 is performed
!     m2asd     - mass fraction to aggregate size distribution converter
!     orient    - calculates the oriented roughness
!     remove    - performs the biomass removal during a harvest, burn, etc.
!                 and updates the decomposition pools accordingly.
!     rough     - calculated the post tillage random roughness
!     set_asd   - set the asd (gmd,gsd) parameter values
!     tdbug     - subroutine which writes out variables for debugging purposes

!     + + + DATA INITIALIZATIONS + + +
      sr = manFile%isub

!     + + + OUTPUT FORMATS + + +
2015  format (' Process code ',i2,1x,'Process ',1x,a20 )

!     + + + END SPECIFICATIONS + + +

      ! allocate massf array
      allocate( massf(msieve+1, soil%nslay), stat=alloc_stat)
      if ( alloc_stat .gt. 0 ) then
        write(*,*) 'Unable to allocate memory for P 11'
        call exit(1)
      end if

      ! set local flag to indicate whether a crop is growing or not
      ! this is used to eliminate spurious harvest reports from residue removal
      ! returns mass if there is a living crop or residue pool which was just killed today
      ! this logic needs reworked when growing multiple crops, or to allow reporting harvest
      ! done multiple days after kill.
      crop_present = 0
      thisPlant => plant
      do while( associated(thisPlant) )
        if( (thisPlant%database%idc .gt. 0) .and. (poolmass( soil%nslay, thisPlant ) .gt. 0.0) ) then
          ! this is a plant, not some added residue
          crop_present = 1
          ! most recent plant, one which is to be harvested
          harvPlant => thisPlant
          exit
        else
          thisPlant => thisPlant%olderPlant
        end if
      end do

      prcode = manFile%proc%procType
      prname = trim(manFile%proc%procName)

      if (BTEST(manFile%am0tfl,0)) write (luomanage(sr),2015) prcode,prname

        ! process calls follow
      select case (prcode)

      case (1)  ! crust breakdown process
        ! pre-process stuff
        kappa = 1.0 ! *** NOTE that kappa is NOT being read from file

        if (manFile%am0tdb .eq. 1) then
          write (luotdb(sr),*)
          write (luotdb(sr),*) '//Before crust breakdown process//'
          call tdbug(sr, prcode, soil, plant)
        end if

        am0til = .true.  !set flag for surface modification

        ! do process
        call crust(kappa,fracarea,soil%asfcr,soil%asflos,soil%asmlos)

        ! post-process stuff
        if (manFile%am0tdb .eq. 1) then
          write (luotdb(sr),*) '//After crust breakdown process//'
          call tdbug(sr, prcode, soil, plant)
        end if

      case (2)  ! random roughness process
        ! pre-process stuff
        if (manFile%am0tdb .eq. 1) then
          write (luotdb(sr),*)
          write (luotdb(sr),*) '//Before random roughness process//'
          call tdbug(sr, prcode, soil, plant)
        end if

        ! read the random roughness for the implement. tillage intensity
        ! factor, and the fraction of the surface tilled come in as group parameter
        call getManVal(manFile%proc, 'rroughflag', roughflg)
        call getManVal(manFile%proc, 'rrough', rrimpl)

        am0til = .true.  !set flag for surface modification

        ! do process
        ! the biomass in the soil affects this calculation. Since it is 
        ! the integrated soil biomass, not fresh biomass that causes this,
        ! the best estimate is the number from sumbio from the previous day.
        call rough(roughflg,rrimpl,ti,fracarea,soil%aslrr, &
                   tlayer, soil%asfcla, soil%asfsil, &
                   biotot%mrtz, biotot%mbgz, &
                   soil%aszlyd)

        ! post-process stuff
        if (manFile%am0tdb .eq. 1) then
          write (luotdb(sr),*) '//After random roughness process//'
          call tdbug(sr, prcode, soil, plant)
        end if

      case (5)  ! oriented roughness process
        ! pre-process stuff
        if (manFile%am0tdb .eq. 1) then
          write (luotdb(sr),*)
          write (luotdb(sr),*) '//Before oriented roughness process//'
          call tdbug(sr, prcode, soil, plant)
        end if

        ! read the oriented roughness parameters for the implement
        call getManVal(manFile%proc, 'rdgflag', rdgflag)
        call getManVal(manFile%proc, 'rdghit', rdght)
        call getManVal(manFile%proc, 'rdgspac', imprs)
        call getManVal(manFile%proc, 'rdgwidth', rdgwt)
        call getManVal(manFile%proc, 'dkhit', dikeht)
        call getManVal(manFile%proc, 'dkspac', dikespac)

        am0til = .true.  !set flag for surface modification

        ! do process
        call orient(soil%aszrgh,soil%asxrgw,soil%asxrgs,soil%asargo, &
                    soil%asxdkh,soil%asxdks, &
                    rdght,rdgwt,imprs,odir,dikeht,dikespac, &
                    tdepth,rdgflag)

        ! post-process stuff
        if (manFile%am0tdb .eq. 1) then
          write (luotdb(sr),*) '//After oriented roughness process//'
          call tdbug(sr, prcode, soil, plant)
        end if

      case (11)  ! crushing process
        ! pre-process stuff
        if (manFile%am0tdb .eq. 1) then
          write (luotdb(sr),*)
          write (luotdb(sr),*) '//Before crushing process//'
          call tdbug(sr, prcode, soil, plant)
        end if

        if( soil%aslagm(5).gt.soil%aslagx(5) ) then
            write (*,*) 'before crush:',soil%aslagm(5),soil%aslagx(5)
        end if

        ! Convert ASD from modified log-normal to sieve classes
        call asd2m(soil%aslagn, soil%aslagx, soil%aslagm, &
                 soil%as0ags, soil%nslay, massf)

        ! read the crushing parameters for the implement
        call getManVal(manFile%proc, 'asdf', alpha)
        call getManVal(manFile%proc, 'crif', beta)

        ! check for valid crushing parameters
        if( alpha.lt.beta) then
           write(0,*) 'Process 11:Crushing:Alpha=',alpha, &
                      'must be greater than Beta=',beta
           call exit (-1)
        endif

        ! adjust parameters based on soil aggregate stability?

        ! do process
        call crush(alpha, beta, tlayer, massf)

        ! post-process stuff
        ! Convert ASD back from sieve classes to modified log-normal
        call m2asd(massf, soil%nslay, &
          soil%aslagn, soil%aslagx, soil%aslagm, soil%as0ags)

        if( soil%aslagm(5).gt.soil%aslagx(5) ) then
            write (*,*) 'after crush:',soil%aslagm(5),soil%aslagx(5)
        end if

        if (manFile%am0tdb .eq. 1) then
          write (luotdb(sr),*) '//After crushing process//'
          call tdbug(sr, prcode, soil, plant)
        end if

      case (12)  ! loosening process
        ! pre-process stuff
        if (manFile%am0tdb .eq. 1) then
          write (luotdb(sr),*)
          write (luotdb(sr),*) '//Before loosening process//'
          call tdbug(sr, prcode, soil, plant)
        end if
        if( soil%aslagm(5).gt.soil%aslagx(5) ) then
            write (*,*) 'before loose:',soil%aslagm(5),soil%aslagx(5)
        end if

        ! read the loosening parameter for the implement
        call getManVal(manFile%proc, 'soilos', mu)

        ! do process
        call loosn(mu,fracarea,tlayer, &
          soil%asdblk,soil%asdsblk,soil%aszlyt, soil%asvroc)

        ! post-process stuff
        ! recalculate  depth to bottom of soil layer
        call depthini( soil%nslay, soil%aszlyt, soil%aszlyd )

        if( wc_type.eq.4 ) then
          ! use texture based calculations from Rawls to set all soil
          ! water properties.
          call param_prop_bc( &
              soil%nslay, soil%aszlyd, soil%asdblk, soil%asdpart, &
              soil%asfcla, soil%asfsan, soil%asfom, soil%asfcec, &
              soil%ahrwcs, soil%ahrwcf, soil%ahrwcw,soil%ahrwcr, &
              soil%ahrwca, soil%ah0cb, soil%aheaep, soil%ahrsk, &
              soil%ahfredsat )
        else
          ! adjust soil hydraulic properties for change in density
          call param_blkden_adj( tlayer, soil%asdblk, soil%asdblk0, &
             soil%asdpart, soil%ahrwcf, soil%ahrwcw, soil%ahrwca, &
             soil%asfcla, soil%asfom, &
             soil%ah0cb, soil%aheaep, soil%ahrsk )
        end if

        if (manFile%am0tdb .eq. 1) then
          write (luotdb(sr),*) '//After loosening process//'
          call tdbug(sr, prcode, soil, plant)
        end if

      case (13)  ! mixing process
        ! pre-process stuff
        if (manFile%am0tdb .eq. 1) then
          write (luotdb(sr),*)
          write (luotdb(sr),*) '//Before mixing process//'
          write (luotdb(sr),*) 'Tillage layer depth is', tlayer
          call tdbug(sr, prcode, soil, plant)
        end if

        if( soil%aslagm(5).gt.soil%aslagx(5) ) then
            write (*,*) 'before mix:',soil%aslagm(5),soil%aslagx(5)
        end if

        ! read the mixing coefficient from the data file
        call getManVal(manFile%proc, 'laymix', rho)

        ! Convert ASD from modified log-normal to sieve classes
        call asd2m(soil%aslagn, soil%aslagx, soil%aslagm, soil%as0ags, soil%nslay, massf)

        ! do process
        call mix(rho,fracarea,tlayer,soil%asdblk,soil%aszlyt, &
          soil%asfsan, soil%asfsil,soil%asfcla, soil%asvroc, &
          soil%asfvcs, soil%asfcs, soil%asfms, soil%asffs, soil%asfvfs, &
          soil%asdwblk, &
          soil%asfom, soil%as0ph, soil%asfcce, soil%asfcec, &
          soil%asfcle, &
          soil%asdagd,soil%aseags, &
          soil%ahrwc, &
          soil%ahrwcs,soil%ahrwcf, soil%ahrwcw, &
          soil%ahrwca, &
          soil%ah0cb, soil%aheaep, soil%ahrsk, &
          plant, &
          massf)

        ! post-process stuff
        ! With the change in composition of the layers, it is necessary
        ! to update soil properties that are a function of texture
        call proptext( tlayer, soil%asfcla, soil%asfsan, soil%asfom, &
                       soil%asdblk, soil%asdsblk, soil%asdprocblk, &
                       soil%asdwblk, soil%asdwsrat, soil%asdpart )

        if( wc_type.eq.4 ) then
          ! use texture based calculations from Rawls to set all soil
          ! water properties.
          call param_prop_bc( &
              tlayer, soil%aszlyd, soil%asdblk, soil%asdpart, &
              soil%asfcla, soil%asfsan, soil%asfom, soil%asfcec, &
              soil%ahrwcs, soil%ahrwcf, soil%ahrwcw,soil%ahrwcr, &
              soil%ahrwca, soil%ah0cb, soil%aheaep, soil%ahrsk, &
              soil%ahfredsat )
        else
          ! set matrix potential parameters to match 1/3 bar and 15 bar water contents
          call param_pot_bc( tlayer, soil%asdblk, soil%asdpart, &
                           soil%ahrwcf, soil%ahrwcw, &
                           soil%asfcla, soil%asfom, &
                           soil%ah0cb, soil%aheaep )
        end if

        ! set previous day bulk density for the changed layers since
        ! this is a change in composition not in bulk density per se
        call set_prevday_blk( tlayer, soil%asdblk, soil%asdblk0 )

        ! Convert ASD back from sieve classes to modified log-normal
        call m2asd(massf, soil%nslay, soil%aslagn, soil%aslagx, soil%aslagm, soil%as0ags)

        if (manFile%am0tdb .eq. 1) then
          write (luotdb(sr),*) '//After mixing process//'
          call tdbug(sr, prcode, soil, plant)
        end if

      case (14)  ! inversion process
        ! pre-process stuff
        if (manFile%am0tdb .eq. 1) then
          write (luotdb(sr),*)
          write (luotdb(sr),*) '//Before inversion process//'
          call tdbug(sr, prcode, soil, plant)
        end if

        ! Convert ASD from modified log-normal to sieve classes
        call asd2m(soil%aslagn, soil%aslagx, soil%aslagm, soil%as0ags, soil%nslay, massf)

        ! do process
        call invert(tlayer,soil%asdblk,soil%aszlyt, &
          soil%asfsan, soil%asfsil,soil%asfcla, soil%asvroc, &
          soil%asfvcs, soil%asfcs, soil%asfms, soil%asffs, soil%asfvfs, &
          soil%asdwblk, &
          soil%asfom, soil%as0ph, soil%asfcce, soil%asfcec, &
          soil%asfcle, &
          soil%asdagd, soil%aseags, &
          soil%ahrwc, &
          soil%ahrwcs,soil%ahrwcf, soil%ahrwcw, &
          soil%ahrwca, &
          soil%ah0cb, soil%aheaep, soil%ahrsk, &
          plant, &
          massf)

        ! post-process stuff

        ! Convert ASD back from sieve classes to modified log-normal
        call m2asd(massf, soil%nslay, soil%aslagn, soil%aslagx, soil%aslagm, soil%as0ags)

        if (manFile%am0tdb .eq. 1) then
          write (luotdb(sr),*) '//After inversion process//'
          call tdbug(sr, prcode, soil, plant)
        end if

      case (21)  ! Compaction
        ! pre-process stuff
        if (manFile%am0tdb .eq. 1) then
          write (luotdb(sr),*)
          write (luotdb(sr),*) '//Before compaction process//'
          call tdbug(sr, prcode, soil, plant)
        end if
        if( soil%aslagm(5).gt.soil%aslagx(5) ) then
            write (*,*) 'before compaction:',soil%aslagm(5),soil%aslagx(5)
        end if

        ! read the compaction parameter for the implement
        call getManVal(manFile%proc, 'mu', mu)
        call getManVal(manFile%proc, 'compact_load', compact_load)

        ! do process
        ! compaction occurs below the tlayer depth
        ! find maximum bulk density (soil water content)
        ! find depth of compaction using water content adjusted proctor density
        allocate( procbdadj(soil%nslay) )
        do idx=1,soil%nslay
          procbdadj(idx) = setbdproc_wc( soil%asfcla(idx), soil%asfsan(idx), soil%asfom(idx), soil%asdpart(idx), soil%ahrwc(idx) )
        end do
        call compact( mu, compact_load, fracarea, tlayer+1, soil%nslay, soil%asdblk, soil%asdsblk, &
                      procbdadj, soil%asdprocblk, soil%aszlyt, soil%asvroc )
        deallocate( procbdadj )
        ! post-process stuff
        ! recalculate  depth to bottom of soil layer
        call depthini( soil%nslay, soil%aszlyt, soil%aszlyd )

        if( wc_type.eq.4 ) then
          ! use texture based calculations from Rawls to set all soil
          ! water properties.
          call param_prop_bc( &
              soil%nslay, soil%aszlyd, soil%asdblk, soil%asdpart, &
              soil%asfcla, soil%asfsan, soil%asfom, soil%asfcec, &
              soil%ahrwcs, soil%ahrwcf, soil%ahrwcw,soil%ahrwcr, &
              soil%ahrwca, soil%ah0cb, soil%aheaep, soil%ahrsk, &
              soil%ahfredsat )

        else
          ! adjust soil hydraulic properties for change in density
          call param_blkden_adj( soil%nslay, soil%asdblk, soil%asdblk0, &
             soil%asdpart, soil%ahrwcf, soil%ahrwcw, soil%ahrwca, &
             soil%asfcla, soil%asfom, &
             soil%ah0cb, soil%aheaep, soil%ahrsk )
        end if

        if (manFile%am0tdb .eq. 1) then
          write (luotdb(sr),*) '//After compaction process//'
          call tdbug(sr, prcode, soil, plant)
        end if

      case (24)  ! flatten process variable toughness
        ! pre-process stuff
        if (manFile%am0tdb .eq. 1) then
          write (luotdb(sr),*)
          write (luotdb(sr),*) '//Before flatten variable toughness proc.//'
          call tdbug(sr, prcode, soil, plant)
        end if

        call getManVal(manFile%proc, 'fbioflagvt', bioflg)
        call getManVal(manFile%proc, 'massflatvt1', afvt(1))
        call getManVal(manFile%proc, 'massflatvt2', afvt(2))
        call getManVal(manFile%proc, 'massflatvt3', afvt(3))
        call getManVal(manFile%proc, 'massflatvt4', afvt(4))
        call getManVal(manFile%proc, 'massflatvt5', afvt(5))

        ! do process
        call flatvt(afvt, fracarea, plant, bioflg)

        ! post-process stuff
        ! crop pool state has been changed, force dependent variable update  
        am0cropupfl = .true.

        if (manFile%am0tdb .eq. 1) then
          write (luotdb(sr),*) '//After flatten variable toughness proc.//'
          call tdbug(sr, prcode, soil, plant)
        end if

      case (25)  ! mass bury process variable toughness
        ! pre-process stuff
        if (manFile%am0tdb .eq. 1) then
          write (luotdb(sr),*)
          write (luotdb(sr),*) '//Before mass bury variable toughness pr.//'
          call tdbug(sr, prcode, soil, plant)
        end if

        call getManVal(manFile%proc, 'burydist', burydistflg)
        call getManVal(manFile%proc, 'massburyvt1', mfvt(1))
        call getManVal(manFile%proc, 'massburyvt2', mfvt(2))
        call getManVal(manFile%proc, 'massburyvt3', mfvt(3))
        call getManVal(manFile%proc, 'massburyvt4', mfvt(4))
        call getManVal(manFile%proc, 'massburyvt5', mfvt(5))

        ! Default all bury processes to "all" biomass for now.
        bioflg = 0

        ! adjust all burial coefficients for speed and depth
        call buryadj(mfvt,mnrbc, &
                   ospeed,ostdspeed,ominspeed,omaxspeed, &
                   tdepth,tstddepth,tmindepth,tmaxdepth)

        ! do process
        if( tlayer .gt. 0 ) then
          call mburyvt(mfvt,fracarea, burydistflg, tlayer, soil, plant, bioflg)
        end if 

        ! post-process stuff
        if (manFile%am0tdb .eq. 1) then
          write (luotdb(sr),*) '//After mass bury variable toughness pr.//'
          call tdbug(sr, prcode, soil, plant)
        end if

      case (26)  ! re-surface process variable toughness
        ! pre-process stuff
        if (manFile%am0tdb .eq. 1) then
          write (luotdb(sr),*)
          write (luotdb(sr),*) '//Before re-surface vari. toughness proc.//'
          call tdbug(sr, prcode, soil, plant)
        end if

        call getManVal(manFile%proc, 'massresurvt1', mfvt(1))
        call getManVal(manFile%proc, 'massresurvt2', mfvt(2))
        call getManVal(manFile%proc, 'massresurvt3', mfvt(3))
        call getManVal(manFile%proc, 'massresurvt4', mfvt(4))
        call getManVal(manFile%proc, 'massresurvt5', mfvt(5))

        ! Lift processes only sees the decomp biomass pools. This default gets them all.
        bioflg = 0

        ! do process
        if( tlayer .gt. 0 ) then
          call liftvt(mfvt, fracarea, tlayer, soil%nslay, plant, resurf_roots, bioflg)
        end if

        ! post-process stuff
        if (manFile%am0tdb .eq. 1) then
          write (luotdb(sr),*) '//After re-surface vari. toughness proc.//'
          call tdbug(sr, prcode, soil, plant)
        end if

      case (31)  ! killing process

        ! Note that the "kill" process only stops the crop growth
        ! submodel and moves the "crop" parameters to the "temporary"
        ! crop pool.  The "transfer" process does the final transfer
        ! of the "temporary" crop pool values over to the "decomp"
        ! pools where they can now begin to decay.

        ! pre-process stuff
        if (manFile%am0tdb .eq. 1) then
          write (luotdb(sr),*)
          write (luotdb(sr),*) '//Before kill process//'
          call tdbug(sr, prcode, soil, plant)
        end if

        ! Some operations will not kill certain types of crops,
        ! ie., a mowing operation usually will not kill a perennial
        ! crop like alfalfa but would kill many annual crops.

        ! this flag remains set until a biomass transfer process (40)
        ! occurs so any side effects can be triggered

        ! This flag may get expanded in the future as new situations
        ! arise.

        ! set am0kilfl
          ! 0 - no kill being done
          ! 1 - annual killed,perennial crop NOT killed
          ! 2 - annual or perennial crop is killed
          ! 3 - defoliation triggered

        call getManVal(manFile%proc, 'kilflag', am0kilfl)

        ! Checks all plants. If living then applies kill flag as appropriate.
        if( kill_plant( am0kilfl, soil%nslay, plant ) ) then
          if( manFile%rpt_season_flg ) then
            call report_hydrobal( sr, manFile%mcount, manFile%mperod )
            ! This may be harvest or non-harvest termination, allow early harvest warnings
            mature_warn_flg = 1
            call plant_endseason( sr, manFile%mcount, manFile%mperod, am0cfl(sr), &
                                 soil%nslay, mature_warn_flg, plant )
            ! set to stop additional report in this operation
            manFile%rpt_season_flg = .false.
          end if
          ! crop pool state has been changed, force dependent variable update  
          am0cropupfl = .true.
        end if
        ! defoliate by moving all living leaf mass to flat residue
        if( defoliate( am0kilfl, soil%nslay, plant ) ) then
          ! crop pool state has been changed, force dependent variable update  
          am0cropupfl = .true.
        end if

        ! post-process stuff
        if (manFile%am0tdb .eq. 1) then
          write (luotdb(sr),*) '//After kill process//'
          call tdbug(sr, prcode, soil, plant)
        end if

      case (32)  ! cutting to height process
        ! pre-process stuff
        if (manFile%am0tdb .eq. 1) then
          write (luotdb(sr),*)
          write (luotdb(sr),*) '//Before cutting to height process//'
          call tdbug(sr, prcode, soil, plant)
        end if

        ! set process parameters
        call getManVal(manFile%proc, 'cutflag', cutflg)
        call getManVal(manFile%proc, 'cutvalh', lastoper(sr)%cutht)
        call getManVal(manFile%proc, 'cyldrmh', pyieldf)
        call getManVal(manFile%proc, 'cplrmh', pstalkf)
        call getManVal(manFile%proc, 'cstrmh', rstandf)

        ! do process
        call cut(cutflg, lastoper(sr)%cutht, pyieldf, pstalkf, rstandf, &
                 soil%nslay, plant, mass_rem, mass_left)

        ! post-process stuff
        if (manFile%am0tdb .eq. 1) then
          write (luotdb(sr),*) '//After cutting to height process//'
          call tdbug(sr, prcode, soil, plant)
        end if
        ! crop pool state has been changed, force dependent variable update  
        am0cropupfl = .true.
        mature_warn_flg = 1
        ! no harvest report if nothing removed or no crop present
        if( (pyieldf+pstalkf+rstandf.gt.0.0) .and. (crop_present.gt.0) ) then
          if( manFile%harv_calib_not_selected ) then
            call get_calib_crops(sr, harvPlant)
            call get_calib_yield(sr, manFile%mcount, mass_rem, mass_left, harvPlant)
            call report_calib_harvest( sr, manFile%mcount, mass_rem, mass_left, harvPlant )
            manFile%harv_calib_not_selected = .false.
          end if
          call report_harvest( sr, manFile%mcount, mass_rem, mass_left, 0, 1, harvPlant)
          if( manFile%rpt_season_flg ) then
              ! not reported by the kill process in this
              call report_hydrobal( sr, manFile%mcount, manFile%mperod )
              call plant_endseason( sr, manFile%mcount, manFile%mperod, am0cfl(sr), &
                                   soil%nslay, mature_warn_flg, harvPlant )
              ! set to stop additional report in this operation
              manFile%rpt_season_flg = .false.
          end if
        endif

      case (33)  ! cutting by fraction process
        ! pre-process stuff
        if (manFile%am0tdb .eq. 1) then
          write (luotdb(sr),*)
          write (luotdb(sr),*) '//Before cutting by fraction process//'
          call tdbug(sr, prcode, soil, plant)
        end if

        call getManVal(manFile%proc, 'cutvalf', lastoper(sr)%cutht)
        call getManVal(manFile%proc, 'cyldrmf', pyieldf)
        call getManVal(manFile%proc, 'cplrmf', pstalkf)
        call getManVal(manFile%proc, 'cstrmf', rstandf)

        ! do process
        cutflg = 2
        call cut(cutflg, lastoper(sr)%cutht, pyieldf, pstalkf, rstandf, &
                 soil%nslay, plant, mass_rem, mass_left)

        ! post-process stuff
        if (manFile%am0tdb .eq. 1) then
          write (luotdb(sr),*) '//After cutting by fraction process//'
          call tdbug(sr, prcode, soil, plant)
        end if
        ! crop pool state has been changed, force dependent variable update  
        am0cropupfl = .true.
        mature_warn_flg = 1
        ! no harvest report if nothing removed or no crop present
        if( (pyieldf+pstalkf+rstandf.gt.0.0) .and. (crop_present.gt.0) ) then
          if( manFile%harv_calib_not_selected ) then
            call get_calib_crops(sr, harvPlant)
            call get_calib_yield(sr, manFile%mcount, mass_rem, mass_left, harvPlant)
            call report_calib_harvest( sr, manFile%mcount, mass_rem, mass_left, harvPlant )
            manFile%harv_calib_not_selected = .false.
          end if
          call report_harvest( sr, manFile%mcount, mass_rem, mass_left, 0, 1, harvPlant)
          if( manFile%rpt_season_flg ) then
              ! not reported by the kill process in this
              call report_hydrobal( sr, manFile%mcount, manFile%mperod )
              call plant_endseason( sr, manFile%mcount, manFile%mperod, am0cfl(sr), &
                                   soil%nslay, mature_warn_flg, harvPlant )
              ! set to stop additional report in this operation
              manFile%rpt_season_flg = .false.
          end if
        end if

      case (34)  ! modify standing fall rate process variable toughness
        ! pre-process stuff
        if (manFile%am0tdb .eq. 1) then
          write (luotdb(sr),*)
          write (luotdb(sr),*) '//Before modify standing fall rate proc.//'
          call tdbug(sr, prcode, soil, plant)
        end if

        call getManVal(manFile%proc, 'frselpool', sel_pool)
        call getManVal(manFile%proc, 'ratemultvt1', rate_mult_vt(1))
        call getManVal(manFile%proc, 'ratemultvt2', rate_mult_vt(2))
        call getManVal(manFile%proc, 'ratemultvt3', rate_mult_vt(3))
        call getManVal(manFile%proc, 'ratemultvt4', rate_mult_vt(4))
        call getManVal(manFile%proc, 'ratemultvt5', rate_mult_vt(5))
        call getManVal(manFile%proc, 'threshmultvt1', thresh_mult_vt(1))
        call getManVal(manFile%proc, 'threshmultvt2', thresh_mult_vt(2))
        call getManVal(manFile%proc, 'threshmultvt3', thresh_mult_vt(3))
        call getManVal(manFile%proc, 'threshmultvt4', thresh_mult_vt(4))
        call getManVal(manFile%proc, 'threshmultvt5', thresh_mult_vt(5))

        ! do process
        call fall_mod_vt( rate_mult_vt, thresh_mult_vt, sel_pool, fracarea, plant )

        ! post-process stuff
        if (manFile%am0tdb .eq. 1) then
          write (luotdb(sr),*) '//After modify standing fall rate proc.//'
          call tdbug(sr, prcode, soil, plant)
        end if

      case (37)  ! thinning to population process
        ! pre-process stuff
        if (manFile%am0tdb .eq. 1) then
          write (luotdb(sr),*)
          write (luotdb(sr),*) '//Before thinning to population process//'
          call tdbug(sr, prcode, soil, plant)
        end if

        call getManVal(manFile%proc, 'thinvalp', thinval)
        call getManVal(manFile%proc, 'tyldrmp', pyieldf)
        call getManVal(manFile%proc, 'tplrmp', pstalkf)
        call getManVal(manFile%proc, 'tstrmp', rstandf)

        ! do process
        thinflg = 1
        call thin(thinflg, thinval, pyieldf, pstalkf, rstandf, soil%nslay, harvPlant, &
             mass_rem, mass_left)

        ! post-process stuff
        if (manFile%am0tdb .eq. 1) then
          write (luotdb(sr),*) '//After thinning to population process//'
          call tdbug(sr, prcode, soil, plant)
        end if
        ! crop pool state has been changed, force dependent variable update  
        am0cropupfl = .true.
        mature_warn_flg = 1
        ! no harvest report if nothing removed or no crop present
        if( (pyieldf+pstalkf+rstandf.gt.0.0) .and. (crop_present.gt.0) ) then
          if( manFile%harv_calib_not_selected ) then
            call get_calib_crops(sr, harvPlant)
            call get_calib_yield(sr, manFile%mcount, mass_rem, mass_left, harvPlant)
            call report_calib_harvest( sr, manFile%mcount, mass_rem, mass_left, harvPlant )
            manFile%harv_calib_not_selected = .false.
          end if
          call report_harvest( sr, manFile%mcount, mass_rem, mass_left, 0,1, harvPlant)
          if( manFile%rpt_season_flg ) then
              ! not reported by the kill process in this
              call report_hydrobal( sr, manFile%mcount, manFile%mperod )
              call plant_endseason( sr, manFile%mcount, manFile%mperod, am0cfl(sr), &
                                   soil%nslay, mature_warn_flg, harvPlant )
              ! set to stop additional report in this operation
              manFile%rpt_season_flg = .false.
          end if
        end if

      case (38)  ! thinning by fraction process
        ! pre-process stuff
        if (manFile%am0tdb .eq. 1) then
          write (luotdb(sr),*)
          write (luotdb(sr),*) '//Before thinning by fraction process//'
          call tdbug(sr, prcode, soil, plant)
        end if

        call getManVal(manFile%proc, 'thinvalf', thinval)
        call getManVal(manFile%proc, 'tyldrmf', pyieldf)
        call getManVal(manFile%proc, 'tplrmf', pstalkf)
        call getManVal(manFile%proc, 'tstrmf', rstandf)

        ! do process
        thinflg = 0
        call thin(thinflg, thinval, pyieldf, pstalkf, rstandf, soil%nslay, harvPlant, &
             mass_rem, mass_left)

        ! post-process stuff
        if (manFile%am0tdb .eq. 1) then
          write (luotdb(sr),*) '//After thinning by fraction process//'
          call tdbug(sr, prcode, soil, plant)
        end if
        ! crop pool state has been changed, force dependent variable update  
        am0cropupfl = .true.
        mature_warn_flg = 1
        ! no harvest report if nothing removed or no crop present
        if( (pyieldf+pstalkf+rstandf.gt.0.0) .and. (crop_present.gt.0) ) then
            call get_calib_crops(sr, harvPlant)
            call get_calib_yield(sr, manFile%mcount, mass_rem, mass_left, harvPlant)
            call report_harvest( sr, manFile%mcount, mass_rem, mass_left, 0, 1, harvPlant)
            call report_calib_harvest( sr, manFile%mcount, mass_rem, mass_left, harvPlant )
            if( manFile%rpt_season_flg ) then
              ! not reported by the kill process in this
              call report_hydrobal( sr, manFile%mcount, manFile%mperod )
              call plant_endseason( sr, manFile%mcount, manFile%mperod, am0cfl(sr), &
                                   soil%nslay, mature_warn_flg, harvPlant )
              ! set to stop additional report in this operation
              manFile%rpt_season_flg = .false.
            end if
        end if

      case (40)  ! crop to biomass transfer process
        ! pre-process stuff
        if (manFile%am0tdb .eq. 1) then
          write (luotdb(sr),*)
          write (luotdb(sr),*) '//Before biomass transfer process//'
          call tdbug(sr, prcode, soil, plant)
        end if

        ! this process in now a no op.

        ! post-process stuff
        if (manFile%am0tdb .eq. 1) then
          write (luotdb(sr),*) '//After biomass transfer process//'
          call tdbug(sr, prcode, soil, plant)
        end if

      case (42)  ! flagged cutting to height process
        ! pre-process stuff
        if (manFile%am0tdb .eq. 1) then
          write (luotdb(sr),*)
          write (luotdb(sr),*) '//Before flagged cutting to height proc.//'
          call tdbug(sr, prcode, soil, plant)
        end if

        ! set process parameters
        call getManVal(manFile%proc, 'harv_report_flg', harv_report_flg)
        call getManVal(manFile%proc, 'harv_calib_flg', harv_calib_flg)
        call getManVal(manFile%proc, 'harv_unit_flg', harv_unit_flg)
        call getManVal(manFile%proc, 'mature_warn_flg', mature_warn_flg)
        call getManVal(manFile%proc, 'cutflag', cutflg)
        call getManVal(manFile%proc, 'cutvalh', lastoper(sr)%cutht)
        call getManVal(manFile%proc, 'cyldrmh', pyieldf)
        call getManVal(manFile%proc, 'cplrmh', pstalkf)
        call getManVal(manFile%proc, 'cstrmh', rstandf)

        ! do process
        call cut(cutflg, lastoper(sr)%cutht, pyieldf, pstalkf, rstandf, &
                 soil%nslay, plant, mass_rem, mass_left)

        ! post-process stuff
        if (manFile%am0tdb .eq. 1) then
          write (luotdb(sr),*) '//After flagged cutting to height proc.//'
          call tdbug(sr, prcode, soil, plant)
        end if
        ! crop pool state has been changed, force dependent variable update  
        am0cropupfl = .true.
        ! no harvest report if nothing removed or no crop present
        if( (pyieldf+pstalkf+rstandf.gt.0.0) .and. (crop_present.gt.0) ) then
          if(      (harv_calib_flg .gt. 0) &
             .and. (manFile%harv_calib_not_selected) ) then
            call get_calib_crops(sr, harvPlant)
            call get_calib_yield(sr, manFile%mcount, mass_rem, mass_left, harvPlant)
            call report_calib_harvest( sr, manFile%mcount, mass_rem, mass_left, harvPlant )
            manFile%harv_calib_not_selected = .false.
          end if
          call report_harvest( sr, manFile%mcount, mass_rem, mass_left, &
                               harv_unit_flg, harv_report_flg, harvPlant )
          if( manFile%rpt_season_flg ) then
              ! not reported by the kill process in this
              call report_hydrobal( sr, manFile%mcount, manFile%mperod )
              call plant_endseason( sr, manFile%mcount, manFile%mperod, am0cfl(sr), &
                                   soil%nslay, mature_warn_flg, harvPlant )
              ! set to stop additional report in this operation
              manFile%rpt_season_flg = .false.
          end if
        endif

      case (43)  ! flagged cutting by fraction process
        ! pre-process stuff
        if (manFile%am0tdb .eq. 1) then
          write (luotdb(sr),*)
          write (luotdb(sr),*) '//Before flagged cutting by fraction pr.//'
          call tdbug(sr, prcode, soil, plant)
        end if

        call getManVal(manFile%proc, 'harv_report_flg', harv_report_flg)
        call getManVal(manFile%proc, 'harv_calib_flg', harv_calib_flg)
        call getManVal(manFile%proc, 'harv_unit_flg', harv_unit_flg)
        call getManVal(manFile%proc, 'mature_warn_flg', mature_warn_flg)
        call getManVal(manFile%proc, 'cutvalf', lastoper(sr)%cutht)
        call getManVal(manFile%proc, 'cyldrmf', pyieldf)
        call getManVal(manFile%proc, 'cplrmf', pstalkf)
        call getManVal(manFile%proc, 'cstrmf', rstandf)

        ! do process
        cutflg = 2
        call cut(cutflg, lastoper(sr)%cutht, pyieldf, pstalkf, rstandf, &
                 soil%nslay, plant, mass_rem, mass_left)

        ! post-process stuff
        if (manFile%am0tdb .eq. 1) then
          write (luotdb(sr),*) '//After flagged cutting by fraction pr.//'
          call tdbug(sr, prcode, soil, plant)
        end if
        ! crop pool state has been changed, force dependent variable update  
        am0cropupfl = .true.
        ! no harvest report if nothing removed or no crop present
        if( (pyieldf+pstalkf+rstandf.gt.0.0) .and. (crop_present.gt.0) ) then
          if(      (harv_calib_flg .gt. 0) &
             .and. (manFile%harv_calib_not_selected) ) then
            call get_calib_crops(sr, harvPlant)
            call get_calib_yield(sr, manFile%mcount, mass_rem, mass_left, harvPlant)
            call report_calib_harvest( sr, manFile%mcount, mass_rem, mass_left, harvPlant )
            manFile%harv_calib_not_selected = .false.
          end if
          call report_harvest( sr, manFile%mcount, mass_rem, mass_left, &
                               harv_unit_flg, harv_report_flg, harvPlant )
          if( manFile%rpt_season_flg ) then
              ! not reported by the kill process in this
              call report_hydrobal( sr, manFile%mcount, manFile%mperod )
              call plant_endseason( sr, manFile%mcount, manFile%mperod, am0cfl(sr), &
                                   soil%nslay, mature_warn_flg, harvPlant )
              ! set to stop additional report in this operation
              manFile%rpt_season_flg = .false.
          end if
        end if

      case (47)  ! flagged thinning to population process
        ! pre-process stuff
        if (manFile%am0tdb .eq. 1) then
          write (luotdb(sr),*)
          write(luotdb(sr),*)'//Before flagged thinning to population pr.//'
          call tdbug(sr, prcode, soil, plant)
        end if

        call getManVal(manFile%proc, 'harv_report_flg', harv_report_flg)
        call getManVal(manFile%proc, 'harv_calib_flg', harv_calib_flg)
        call getManVal(manFile%proc, 'harv_unit_flg', harv_unit_flg)
        call getManVal(manFile%proc, 'mature_warn_flg', mature_warn_flg)
        call getManVal(manFile%proc, 'thinvalp', thinval)
        call getManVal(manFile%proc, 'tyldrmp', pyieldf)
        call getManVal(manFile%proc, 'tplrmp', pstalkf)
        call getManVal(manFile%proc, 'tstrmp', rstandf)

        ! do process
        thinflg = 1
        call thin(thinflg, thinval, pyieldf, pstalkf, rstandf, soil%nslay, harvPlant, &
             mass_rem, mass_left)

        ! post-process stuff
        if (manFile%am0tdb .eq. 1) then
          write(luotdb(sr),*) '//After flagged thinning to population pr.//'
          call tdbug(sr, prcode, soil, plant)
        end if
        ! crop pool state has been changed, force dependent variable update  
        am0cropupfl = .true.
        ! no harvest report if nothing removed or no crop present
        if( (pyieldf+pstalkf+rstandf.gt.0.0) .and. (crop_present.gt.0) ) then
          if(      (harv_calib_flg .gt. 0) &
             .and. (manFile%harv_calib_not_selected) ) then
            call get_calib_crops(sr, harvPlant)
            call get_calib_yield(sr, manFile%mcount, mass_rem, mass_left, harvPlant)
            call report_calib_harvest( sr, manFile%mcount, mass_rem, mass_left, harvPlant )
            manFile%harv_calib_not_selected = .false.
          end if
          call report_harvest( sr, manFile%mcount, mass_rem, mass_left, &
                               harv_unit_flg, harv_report_flg, harvPlant )
          if( manFile%rpt_season_flg ) then
              ! not reported by the kill process in this
              call report_hydrobal( sr, manFile%mcount, manFile%mperod )
              call plant_endseason( sr, manFile%mcount, manFile%mperod, am0cfl(sr), &
                                   soil%nslay, mature_warn_flg, harvPlant )
              ! set to stop additional report in this operation
              manFile%rpt_season_flg = .false.
          end if
        end if

      case (48)  ! flagged thinning by fraction process
        ! pre-process stuff
        if (manFile%am0tdb .eq. 1) then
          write (luotdb(sr),*)
          write (luotdb(sr),*) '//Before flagged thinning by fraction pr.//'
          call tdbug(sr, prcode, soil, plant)
        end if

        call getManVal(manFile%proc, 'harv_report_flg', harv_report_flg)
        call getManVal(manFile%proc, 'harv_calib_flg', harv_calib_flg)
        call getManVal(manFile%proc, 'harv_unit_flg', harv_unit_flg)
        call getManVal(manFile%proc, 'mature_warn_flg', mature_warn_flg)
        call getManVal(manFile%proc, 'thinvalf', thinval)
        call getManVal(manFile%proc, 'tyldrmf', pyieldf)
        call getManVal(manFile%proc, 'tplrmf', pstalkf)
        call getManVal(manFile%proc, 'tstrmf', rstandf)

        ! do process
        thinflg = 0
        call thin(thinflg, thinval, pyieldf, pstalkf, rstandf, soil%nslay, harvPlant, &
             mass_rem, mass_left)

        ! post-process stuff
        if (manFile%am0tdb .eq. 1) then
          write (luotdb(sr),*) '//After flagged thinning by fraction pr.//'
          call tdbug(sr, prcode, soil, plant)
        end if
        ! crop pool state has been changed, force dependent variable update  
        am0cropupfl = .true.
        ! no harvest report if nothing removed or no crop present
        if( (pyieldf+pstalkf+rstandf.gt.0.0) .and. (crop_present.gt.0) ) then
          if(      (harv_calib_flg .gt. 0) &
             .and. (manFile%harv_calib_not_selected) ) then
            call get_calib_crops(sr, harvPlant)
            call get_calib_yield(sr, manFile%mcount, mass_rem, mass_left, harvPlant)
            call report_calib_harvest( sr, manFile%mcount, mass_rem, mass_left, harvPlant )
            manFile%harv_calib_not_selected = .false.
          end if
          call report_harvest( sr, manFile%mcount, mass_rem, mass_left, &
                               harv_unit_flg, harv_report_flg, harvPlant )
          if( manFile%rpt_season_flg ) then
              ! not reported by the kill process in this
              call report_hydrobal( sr, manFile%mcount, manFile%mperod )
              call plant_endseason( sr, manFile%mcount, manFile%mperod, am0cfl(sr), &
                                   soil%nslay, mature_warn_flg, harvPlant )
              ! set to stop additional report in this operation
              manFile%rpt_season_flg = .false.
          end if
        end if

      case (50)  ! residue initialization process
        ! pre-process stuff
        if (manFile%am0tdb .eq. 1) then
          write (luotdb(sr),*)
          write (luotdb(sr),*) '//Before residue initialization process//'
          call tdbug(sr, prcode, soil, plant)
        end if

        ! new plant created by biomass group (G 03)
        ! Delete residue from All older residue pools
        ! Delete all non growing plants
        ! Delete residue from all growing plants
        thisPlant => plant%olderPlant
        do while( associated(thisPlant) )
          if( thisPlant%growth%am0cgf ) then
            ! growing plant, delete only plant residue pools
            call residueDestroyAll( thisPlant%residue )
            ! move to next plant
            thisPlant => thisPlant%olderPlant
          else
            ! not growing delete and move older plants up
            call plantDestroy( thisPlant )
            if( .not. associated( thisPlant ) ) then
              ! no more plants, stop looping
              exit
            end if
          end if
        end do

        ! create residue pool in new plant (initializes all values)
        plant%residue => residueAdd(plant%residue, plant%residueIndex, soil%nslay)

        ! New residue is assigned to this residue pool.

        ! do process
        ! Read surface residue counts and amount
        call getManVal(manFile%proc, 'numst', plant%residue%dstm)
        call getManVal(manFile%proc, 'rstandht', plant%residue%zht)
        call getManVal(manFile%proc, 'rstandmass', plant%residue%standstem)
        call getManVal(manFile%proc, 'rflatmass', plant%residue%flatstem)
        call getManVal(manFile%proc, 'rbc', plant%database%rbc)
        call getManVal(manFile%proc, 'rburiedmass', dmassres)
        call getManVal(manFile%proc, 'rburieddepth', zmassres)
        call getManVal(manFile%proc, 'rrootmass', dmassrot)
        call getManVal(manFile%proc, 'rrootdepth', zmassrot)
        ! place buried residue in pools by layer
        call resinit(dmassrot, zmassrot, soil%nslay, plant%residue%rootfiberz, soil%aszlyt)
        call resinit(dmassres,zmassres,soil%nslay, plant%residue%stemz, soil%aszlyt)
        ! read decomposition parameters for type of residue buried
        call getManVal(manFile%proc, 'standdk', plant%database%dkrate(1))
        call getManVal(manFile%proc, 'surfdk', plant%database%dkrate(2))
        call getManVal(manFile%proc, 'burieddk', plant%database%dkrate(3))
        call getManVal(manFile%proc, 'rootdk', plant%database%dkrate(4))
        call getManVal(manFile%proc, 'stemnodk', plant%database%dkrate(5))
        call getManVal(manFile%proc, 'stemdia', plant%database%xstm)
        call getManVal(manFile%proc, 'thrddys', plant%database%ddsthrsh)
        call getManVal(manFile%proc, 'covfact', plant%database%covfact)
        ! read decomposition parameters for type of residue buried
        call getManVal(manFile%proc, 'resevapa', plant%database%resevapa)
        call getManVal(manFile%proc, 'resevapb', plant%database%resevapa)

        ! use xstm value for xstmrep
        plant%residue%xstmrep = plant%database%xstm
        ! use zmassrot value for zrtd
        plant%residue%zrtd = zmassrot

        ! grainf is not set in this process. Default value is used.

        ! post-process stuff
        if (manFile%am0tdb .eq. 1) then
          write (luotdb(sr),*) '//After residue initialization process//'
          call tdbug(sr, prcode, soil, plant)
        end if

      case (51)  ! planting process
        ! pre-process stuff
        if (manFile%am0tdb .eq. 1) then
          write (luotdb(sr),*)
          write (luotdb(sr),*) '//Before planting process//'
          call tdbug(sr, prcode, soil, plant)
        end if

        ! new plant created by biomass group (G 03)

        ! for now do not allow more than one growing planting at a time
        ! set kill flag to kill anything living
        am0kilfl = 2
        if( kill_plant( am0kilfl, soil%nslay, plant%olderPlant ) ) then
          ! Old planting still growing
          ! non-harvest termination, suppress early harvest warnings
          mature_warn_flg = 0
          call plant_endseason( sr, manFile%mcount, manFile%mperod, am0cfl(sr), &
                               soil%nslay, mature_warn_flg, plant%olderPlant )
          ! set to guarantee corresponding report hydrolbal at end of planting
          manFile%rpt_season_flg = .true.
        endif

        ! crop pool state has been changed, force dependent variable update  
        am0cropupfl = .true.

        ! read population, spacing and yield flags
        call getManVal(manFile%proc, 'rowflag', plant%geometry%rsfg)
        call getManVal(manFile%proc, 'rowspac', plant%geometry%xrow)
        call getManVal(manFile%proc, 'rowridge', plant%geometry%rg)
        call getManVal(manFile%proc, 'plantpop', plant%geometry%dpop)
        call getManVal(manFile%proc, 'dmaxshoot', plant%database%dmaxshoot)
        call getManVal(manFile%proc, 'cbaflag', plant%database%baflg)
        call getManVal(manFile%proc, 'tgtyield', plant%database%ytgt)
        call getManVal(manFile%proc, 'cbafact', plant%database%baf)
        call getManVal(manFile%proc, 'cyrafact', plant%database%yraf)
        call getManVal(manFile%proc, 'hyldflag', plant%geometry%hyfg)
        ! read yield reporting name
        call getManVal(manFile%proc, 'hyldunits', plant%database%ynmu)
        ! read yield reporting values and growth characteristics
        call getManVal(manFile%proc, 'hyldwater', plant%database%ywct)
        call getManVal(manFile%proc, 'hyconfact', plant%database%ycon)
        call getManVal(manFile%proc, 'idc', plant%database%idc)
        call getManVal(manFile%proc, 'grf', plant%database%grf)
        call getManVal(manFile%proc, 'ck', plant%database%ck)
        call getManVal(manFile%proc, 'hui0', plant%database%ehu0)
        ! read crop growth parameters
        call getManVal(manFile%proc, 'hmx', plant%database%zmxc)
        call getManVal(manFile%proc, 'growdepth', plant%database%growdepth)
        call getManVal(manFile%proc, 'rdmx', plant%database%zmrt)
        call getManVal(manFile%proc, 'tbas', plant%database%tmin)
        call getManVal(manFile%proc, 'topt', plant%database%topt)
        call getManVal(manFile%proc, 'thudf', plant%database%thudf)
        call getManVal(manFile%proc, 'dtm', plant%database%tdtm)
        call getManVal(manFile%proc, 'thum', plant%database%thum)
        call getManVal(manFile%proc, 'frsx1', plant%database%fd1(1))
        call getManVal(manFile%proc, 'frsx2', plant%database%fd2(1))
        call getManVal(manFile%proc, 'frsy1', plant%database%fd1(2))
        call getManVal(manFile%proc, 'frsy2', plant%database%fd2(2))
        call getManVal(manFile%proc, 'verndel', plant%database%tverndel)
        call getManVal(manFile%proc, 'bceff', plant%database%bceff)
        call getManVal(manFile%proc, 'a_lf', plant%database%alf)
        call getManVal(manFile%proc, 'b_lf', plant%database%blf)
        call getManVal(manFile%proc, 'c_lf', plant%database%clf)
        call getManVal(manFile%proc, 'd_lf', plant%database%dlf)
        call getManVal(manFile%proc, 'a_rp', plant%database%arp)
        call getManVal(manFile%proc, 'b_rp', plant%database%brp)
        call getManVal(manFile%proc, 'c_rp', plant%database%crp)
        call getManVal(manFile%proc, 'd_rp', plant%database%drp)
        call getManVal(manFile%proc, 'a_ht', plant%database%aht)
        call getManVal(manFile%proc, 'b_ht', plant%database%bht)
        call getManVal(manFile%proc, 'ssaa', plant%database%ssa)
        call getManVal(manFile%proc, 'ssab', plant%database%ssb)
        call getManVal(manFile%proc, 'sla', plant%database%sla)
        call getManVal(manFile%proc, 'huie', plant%database%hue)
        call getManVal(manFile%proc, 'transf', plant%database%transf)
        call getManVal(manFile%proc, 'diammax', plant%database%diammax)
        call getManVal(manFile%proc, 'storeinit', plant%database%storeinit)
        call getManVal(manFile%proc, 'mshoot', plant%database%shoot)
        call getManVal(manFile%proc, 'leafstem', plant%database%fleafstem)
        call getManVal(manFile%proc, 'fshoot', plant%database%fshoot)
        call getManVal(manFile%proc, 'leaf2stor', plant%database%fleaf2stor)
        call getManVal(manFile%proc, 'stem2stor', plant%database%fstem2stor)
        call getManVal(manFile%proc, 'stor2stor', plant%database%fstor2stor)
        call getManVal(manFile%proc, 'rbc',plant%database%rbc)
        call getManVal(manFile%proc, 'standdk', plant%database%dkrate(1))
        call getManVal(manFile%proc, 'surfdk', plant%database%dkrate(2))
        call getManVal(manFile%proc, 'burieddk', plant%database%dkrate(3))
        call getManVal(manFile%proc, 'rootdk', plant%database%dkrate(4))
        call getManVal(manFile%proc, 'stemnodk', plant%database%dkrate(5))
        call getManVal(manFile%proc, 'stemdia', plant%database%xstm)
        call getManVal(manFile%proc, 'thrddys', plant%database%ddsthrsh)
        call getManVal(manFile%proc, 'covfact', plant%database%covfact)
        call getManVal(manFile%proc, 'resevapa', plant%database%resevapa)
        call getManVal(manFile%proc, 'resevapb', plant%database%resevapb)
        call getManVal(manFile%proc, 'yield_coefficient', plant%database%yld_coef)
        call getManVal(manFile%proc, 'residue_intercept', plant%database%resid_int)
        call getManVal(manFile%proc, 'regrow_location', plant%database%zloc_regrow)

        ! reading of process parameters complete

        call plant_setup( sr, plant, soil, lastoper(sr), imprs, rdgflag )

        ! initialize flag to prevent multiple calibration harvests for single crop
        manFile%harv_calib_not_selected = .true.

        ! do process
        ! do not initialize crop if no crop is present
        if( (plant%geometry%dpop .gt. 0.0) .and. (plant%database%idc .gt. 0) ) then
          ! set flag for crop initialization - jt
          plant%growth%am0cif = .true.
          ! set crop growth flag on - jt
          plant%growth%am0cgf = .true.

          if( upgm_growth .eq. 1 ) then
            ! grow WEPS crop using upgm
            call init_WEPS_UPGM( soil, plant )
          end if
        endif
        ! post-process stuff
        if (manFile%am0tdb .eq. 1) then
          write (luotdb(sr),*) '//After planting process//'
          call tdbug(sr, prcode, soil, plant)
        end if
        call set_calib(sr, plant)
        if( manFile%rpt_season_flg ) then
          ! not reported by the kill process in this
          call report_hydrobal( sr, manFile%mcount, manFile%mperod )
        end if

      case (61)  ! biomass remove process
        ! pre-process stuff
        if (manFile%am0tdb .eq. 1) then
          write (luotdb(sr),*)
          write (luotdb(sr),*) '//Before biomass remove process//'
          call tdbug(sr, prcode, soil, plant)
        end if

        call getManVal(manFile%proc, 'selpos', sel_position)
        call getManVal(manFile%proc, 'selpool', sel_pool)
        call getManVal(manFile%proc, 'rstore', storef)
        call getManVal(manFile%proc, 'rleaf', leaff)
        call getManVal(manFile%proc, 'rstem', stemf)
        call getManVal(manFile%proc, 'rrootstore', rootstoref)
        call getManVal(manFile%proc, 'rrootfiber', rootfiberf)

        ! Set bioflg to look at all pools
        bioflg = 0

        ! do process
        call remove( sel_position, sel_pool, bioflg, &
          stemf, leaff, storef, rootstoref, rootfiberf, &
          soil%nslay, plant, mass_rem, mass_left)

        ! post-process stuff
        if (manFile%am0tdb .eq. 1) then
          write (luotdb(sr),*) '//After biomass remove process//'
          call tdbug(sr, prcode, soil, plant)
        end if
        ! crop pool state has been changed, force dependent variable update  
        am0cropupfl = .true.
        mature_warn_flg = 1
        ! no harvest report if nothing removed or no crop present
        if( (storef + leaff + stemf + rootstoref + rootfiberf .gt. 0.0) &
            .and. (crop_present.gt.0) ) then
          if( manFile%harv_calib_not_selected ) then
            call get_calib_crops(sr, harvPlant)
            call get_calib_yield(sr, manFile%mcount, mass_rem, mass_left, harvPlant)
            call report_calib_harvest( sr, manFile%mcount, mass_rem, mass_left, harvPlant )
            manFile%harv_calib_not_selected = .false.
          end if
            call report_harvest( sr, manFile%mcount, mass_rem, mass_left, 0, 1, harvPlant)
          if( manFile%rpt_season_flg ) then
              ! not reported by the kill process in this
              call report_hydrobal( sr, manFile%mcount, manFile%mperod )
              call plant_endseason( sr, manFile%mcount, manFile%mperod, am0cfl(sr), &
                                   soil%nslay, mature_warn_flg, harvPlant )
              ! set to stop additional report in this operation
              manFile%rpt_season_flg = .false.
          end if
        end if

      case (62)  ! biomass remove pool process
        ! pre-process stuff
        if (manFile%am0tdb .eq. 1) then
          write (luotdb(sr),*)
          write (luotdb(sr),*) '//Before biomass remove pool process//'
          call tdbug(sr, prcode, soil, plant)
        end if

        call getManVal(manFile%proc, 'harv_report_flg', harv_report_flg)
        call getManVal(manFile%proc, 'harv_calib_flg', harv_calib_flg)
        call getManVal(manFile%proc, 'harv_unit_flg', harv_unit_flg)
        call getManVal(manFile%proc, 'mature_warn_flg', mature_warn_flg)
        call getManVal(manFile%proc, 'selpos', sel_position)
        call getManVal(manFile%proc, 'selpool', sel_pool)
        call getManVal(manFile%proc, 'selagepool', bioflg)
        call getManVal(manFile%proc, 'rstore', storef)
        call getManVal(manFile%proc, 'rleaf', leaff)
        call getManVal(manFile%proc, 'rstem', stemf)
        call getManVal(manFile%proc, 'rrootstore', rootstoref)
        call getManVal(manFile%proc, 'rrootfiber', rootfiberf)

        ! do process
        call remove( sel_position, sel_pool, bioflg, &
          stemf, leaff, storef, rootstoref, rootfiberf, &
          soil%nslay, plant, mass_rem, mass_left)

        ! post-process stuff
        if (manFile%am0tdb .eq. 1) then
          write (luotdb(sr),*) '//After biomass remove pool process//'
          call tdbug(sr, prcode, soil, plant)
        end if
        ! crop pool state has been changed, force dependent variable update  
        am0cropupfl = .true.
        ! no harvest report if nothing removed
        if( (storef + leaff + stemf + rootstoref + rootfiberf .gt. 0.0) &
            .and. (crop_present.gt.0) ) then
          ! removed mass is used in calibration
          if(      (harv_calib_flg .gt. 0) &
             .and. (manFile%harv_calib_not_selected) ) then
            call get_calib_crops(sr, harvPlant)
            call get_calib_yield(sr, manFile%mcount, mass_rem, mass_left, harvPlant)
            call report_calib_harvest( sr, manFile%mcount, mass_rem, mass_left, harvPlant )
            manFile%harv_calib_not_selected = .false.
          end if
          ! removed mass appears in crop report
          call report_harvest( sr, manFile%mcount, mass_rem, mass_left, &
                               harv_unit_flg, harv_report_flg, harvPlant )
          if( manFile%rpt_season_flg ) then
            ! not reported by the kill process in this
            call report_hydrobal( sr, manFile%mcount, manFile%mperod )
            call plant_endseason( sr, manFile%mcount, manFile%mperod, am0cfl(sr), &
                                 soil%nslay, mature_warn_flg, harvPlant )
              ! set to stop additional report in this operation
            manFile%rpt_season_flg = .false.
          end if
        end if

      case (65)  ! add residue process
        ! New residue is place in new plant created by G03

        ! create residue pool in new plant (inintializes all values)
        plant%residue => residueAdd(plant%residue, plant%residueIndex, soil%nslay)

        ! New residue is assigned to this residue pool.

        ! pre-process stuff
        if (manFile%am0tdb .eq. 1) then
          write (luotdb(sr),*)
          write (luotdb(sr),*) '//Before add residue process//'
          call tdbug(sr, prcode, soil, plant)
        end if

        call getManVal(manFile%proc, 'numst', plant%residue%dstm)
        call getManVal(manFile%proc, 'rstandht', plant%residue%zht)
        call getManVal(manFile%proc, 'rstandmass', plant%residue%standstem)
        call getManVal(manFile%proc, 'rflatmass', plant%residue%flatstem)
        call getManVal(manFile%proc, 'rbc', plant%database%rbc)
        ! read buried residue amounts
        call getManVal(manFile%proc, 'rburiedmass', dmassres)
        call getManVal(manFile%proc, 'rburieddepth', zmassres)
        call getManVal(manFile%proc, 'rrootmass', dmassrot)
        call getManVal(manFile%proc, 'rrootdepth', zmassrot)

        ! place buried residue in pools by layer
        call resinit(dmassrot, zmassrot, soil%nslay, plant%residue%rootfiberz, soil%aszlyt)
        call resinit(dmassres,zmassres,soil%nslay, plant%residue%stemz, soil%aszlyt)
        ! read decomposition parameters
        call getManVal(manFile%proc, 'standdk', plant%database%dkrate(1))
        call getManVal(manFile%proc, 'surfdk', plant%database%dkrate(2))
        call getManVal(manFile%proc, 'burieddk', plant%database%dkrate(3))
        call getManVal(manFile%proc, 'rootdk', plant%database%dkrate(4))
        call getManVal(manFile%proc, 'stemnodk', plant%database%dkrate(5))
        call getManVal(manFile%proc, 'stemdia', plant%database%xstm)
        call getManVal(manFile%proc, 'thrddys', plant%database%ddsthrsh)
        call getManVal(manFile%proc, 'covfact', plant%database%covfact)
        ! read parameters for residue suppression of evaporation
        call getManVal(manFile%proc, 'resevapa', plant%database%resevapa)
        call getManVal(manFile%proc, 'resevapb', plant%database%resevapb)

        ! use xstm value for xstmrep
        plant%residue%xstmrep = plant%database%xstm
        ! use zmassrot value for zrtd
        plant%residue%zrtd = zmassrot

        ! post-process stuff
        if (manFile%am0tdb .eq. 1) then
          write (luotdb(sr),*) '//After add residue process//'
          call tdbug(sr, prcode, soil, plant)
        end if

      case (66)  ! add manure process
        ! ADD MANURE was modeled after ADD RESIDUE (process 65)
        ! The only difference between process ADD MANURE and
        ! ADD RESIDUE is that NRCS wanted to be able to specify
        ! the "total" mass of manure applied and the fraction
        ! that is buried of that total.  So, ADD MANURE is a
        ! special case of ADD RESIDUE (just uses two additional
        ! input parameters)

        ! New residue is place in new plant created by G03

        ! create residue pool in new plant
        plant%residue => residueAdd(plant%residue, plant%residueIndex, soil%nslay)

        ! New residue is assigned to this residue pool.

        ! pre-process stuff
        if (manFile%am0tdb .eq. 1) then
          write (luotdb(sr),*)
          write (luotdb(sr),*) '//Before add manure process//'
          call tdbug(sr, prcode, soil, plant)
        end if

        call getManVal(manFile%proc, 'M_numst', plant%residue%dstm)
        call getManVal(manFile%proc, 'M_rstandht', plant%residue%zht)
        call getManVal(manFile%proc, 'M_rstandmass', plant%residue%standstem)
        call getManVal(manFile%proc, 'M_rflatmass', plant%residue%flatstem)
        call getManVal(manFile%proc, 'rbc', plant%database%rbc)
        ! read buried residue amounts
        call getManVal(manFile%proc, 'M_rburiedmass', dmassres)
        call getManVal(manFile%proc, 'M_rburieddepth', zmassres)
        call getManVal(manFile%proc, 'M_rrootmass', dmassrot)
        call getManVal(manFile%proc, 'M_rrootdepth', zmassrot)

        ! read total manure mass amount and buried fraction
        call getManVal(manFile%proc, 'manure_total_mass', manure_total_mass)
        call getManVal(manFile%proc, 'manure_buried_ratio', manure_buried_fraction)

       ! Now we add the "flat and buried" manure to the generic residue
       ! flat and buried quantities
        plant%residue%flatstem = plant%residue%flatstem + (1.0 - manure_buried_fraction) * manure_total_mass
        dmassres = dmassres + (manure_buried_fraction) * manure_total_mass

        ! place buried residue in pools by layer
        call resinit(dmassrot, zmassrot, soil%nslay, plant%residue%rootfiberz, soil%aszlyt)
        call resinit(dmassres,zmassres,soil%nslay, plant%residue%stemz, soil%aszlyt)

        ! read decomposition parameters
        call getManVal(manFile%proc, 'standdk', plant%database%dkrate(1))
        call getManVal(manFile%proc, 'surfdk', plant%database%dkrate(2))
        call getManVal(manFile%proc, 'burieddk', plant%database%dkrate(3))
        call getManVal(manFile%proc, 'rootdk', plant%database%dkrate(4))
        call getManVal(manFile%proc, 'stemnodk', plant%database%dkrate(5))
        call getManVal(manFile%proc, 'stemdia', plant%database%xstm)
        call getManVal(manFile%proc, 'thrddys', plant%database%ddsthrsh)
        call getManVal(manFile%proc, 'covfact', plant%database%covfact)
        ! read parameters for residue suppression of evaporation
        call getManVal(manFile%proc, 'resevapa', plant%database%resevapa)
        call getManVal(manFile%proc, 'resevapb', plant%database%resevapb)
 
        ! use zmassrot value for zrtd
        plant%residue%zrtd = zmassrot

        ! post-process stuff
        if (manFile%am0tdb .eq. 1) then
          write (luotdb(sr),*) '//After add manure process//'
          call tdbug(sr, prcode, soil, plant)
        end if

      case (71) ! irrigate process (OBSOLETE)
        ! pre-process stuff
        if (manFile%am0tdb .eq. 1) then
          write (luotdb(sr),*)
          write (luotdb(sr),*) '//Before irrigation process//'
          call tdbug(sr, prcode, soil, plant)
        end if

        call getManVal(manFile%proc, 'irrtype', roughflg)
        call getManVal(manFile%proc, 'irrdepth', irrig)

        ! do process
        ! replaced am0irr (1 - sprinkler, 2 furrow) with locirr
        ! using roughflg to read in old value and set some default values
        if( roughflg .eq. 1 ) then
            hstate%locirr = 2000.0
        else
            hstate%locirr = 0.0
        end if
        h1et%zirr = h1et%zirr + irrig
        ! make sure rate and duration are consistent
        ! these values are not set in this process but may have been set
        ! in process 72, if this is used in conjunction with it
        call ratedura(h1et%zirr, hstate%ratirr, hstate%durirr)
        ! post-process stuff
        if (manFile%am0tdb .eq. 1) then
          write (luotdb(sr),*) '//After irrigate process//'
          call tdbug(sr, prcode, soil, plant)
        end if

      case (72)  ! irrigation monitoring process
        ! pre-process stuff
        if (manFile%am0tdb .eq. 1) then
          write (luotdb(sr),*)
          write (luotdb(sr),*) '//Before irrigation monitoring process//'
          call tdbug(sr, prcode, soil, plant)
        end if

        call getManVal(manFile%proc, 'irrmonflag', hstate%monirr)
        call getManVal(manFile%proc, 'irrmaxapp', hstate%zdmaxirr)
        call getManVal(manFile%proc, 'irrrate', hstate%ratirr)
        call getManVal(manFile%proc, 'irrduration', hstate%durirr)
        call getManVal(manFile%proc, 'irrapploc', hstate%locirr)
        call getManVal(manFile%proc, 'irrminapp', hstate%minirr)
        call getManVal(manFile%proc, 'irrmad', hstate%madirr)
        call getManVal(manFile%proc, 'irrminint', hstate%mintirr)

        ! do process
        ! set next irrigation day to zero so irrigations will trigger
        hstate%ndayirr = 0
        ! use inputs to set the irrigation rate, if 
        call ratedura(hstate%zdmaxirr, hstate%ratirr, hstate%durirr)
        ! post-process stuff
        if (manFile%am0tdb .eq. 1) then
          write (luotdb(sr),*) '//After irrigation monitoring process//'
          call tdbug(sr, prcode, soil, plant)
        end if

      case (73)  ! single event irrigation process
        ! pre-process stuff
        if (manFile%am0tdb .eq. 1) then
          write (luotdb(sr),*)
          write (luotdb(sr),*) '//Before single event irrigation process//'
          call tdbug(sr, prcode, soil, plant)
        end if

        call getManVal(manFile%proc, 'irrdepth', irrig)
        call getManVal(manFile%proc, 'irrrate', hstate%ratirr)
        call getManVal(manFile%proc, 'irrduration', hstate%durirr)
        call getManVal(manFile%proc, 'irrapploc', hstate%locirr)

        ! do process
        ! add this irrigation event to any previous event on this same day
        h1et%zirr = h1et%zirr + irrig
        ! use inputs to set the irrigation rate, if 
        call ratedura(h1et%zirr, hstate%ratirr, hstate%durirr)
        if (manFile%am0tdb .eq. 1) then
          write (luotdb(sr),*) '//After single event irrigation process//'
          !call tdbug(sr, prcode, soil, plant)
        end if
        ! post-process stuff
        if (manFile%am0tdb .eq. 1) then
          write (luotdb(sr),*) '//After single event irrigation process//'
          call tdbug(sr, prcode, soil, plant)
        end if

      case (74)  ! terminate irrigation monitoring terminate process
        ! pre-process stuff
        if (manFile%am0tdb .eq. 1) then
          write (luotdb(sr),*)
          write (luotdb(sr),*) '//Before terminate irrigation monitoring//'
          call tdbug(sr, prcode, soil, plant)
        end if

        ! do process
        hstate%monirr = 0
        ! post-process stuff
        if (manFile%am0tdb .eq. 1) then
          write (luotdb(sr),*) '//After terminate irrigation monitoring//'
          call tdbug(sr, prcode, soil, plant)
        end if

      case (91)  ! initialize (set) soil layer asd
        ! pre-process stuff
        if (manFile%am0tdb .eq. 1) then
          write (luotdb(sr),*)
          write (luotdb(sr),*) '//Before initialize soil layer asd conditions//'
          call tdbug(sr, prcode, soil, plant)
        end if

        !write(0,*) 'prior to set_asd() call: ', 'msieve: ', msieve, 'nsieve: ', nsieve

        if (manFile%am0tdb .eq. 2) then  !Debug info printouts
          ! print out the sdia() array
          write(UNIT=0, FMT="('           ')",ADVANCE="NO")
          do j=1, nsieve
            write(UNIT=0, FMT="(27(i9))",ADVANCE="NO") j
            if (j == nsieve) write(0,*) ''
          end do
          write(UNIT=0, FMT="('sdia(',i2,':',i2,')')",ADVANCE="NO") 1,nsieve
          do j=1, nsieve
            write(UNIT=0, FMT="(27(f9.4))",ADVANCE="NO") sdia(j)
            if (j == nsieve) write(0,*) ""
          end do
          write(0,*) ""
          ! print out the mdia() array
          write(UNIT=0, FMT="('           ')",ADVANCE="NO")
          do j=1, msieve
            write(UNIT=0, FMT="(27(i9))",ADVANCE="NO") j
            if (j == msieve) write(0,*) ''
          end do
          write(UNIT=0, FMT="('mdia(',i2,':',i2,')')",ADVANCE="NO") 1,msieve
          do j=1, msieve
            write(UNIT=0, FMT="(27(f9.4))",ADVANCE="NO") mdia(j)
            if (j == msieve) write(0,*) ""
          end do
          write(0,*) ""
        end if

        call getManVal(manFile%proc, 'asddepth', asddepth)
        call getManVal(manFile%proc, 'gmdx', gmdx)
        call getManVal(manFile%proc, 'gsdx', gsdx)
        call getManVal(manFile%proc, 'mnot', mnot)
        call getManVal(manFile%proc, 'minf', minf)

        ! New parameters for set_asd initialization process
        write(UNIT=0,FMT="(5(f10.4))") asddepth, gmdx, gsdx, mnot, minf
        write(0,*)

        ! Obtain the number of layers the ASD values will be set to,
        ! based upon the specified depth and the individual layer thicknesses
        asdlayer = tillay(asddepth, soil%aszlyt, soil%nslay)

        if (BTEST(manFile%am0tfl,0) .and. manFile%asdhflag .eq. 0) then
          write(luoasd(sr),"(3(A5))",ADVANCE="NO") '# day', 'mon', 'year'
          write(luoasd(sr),"(6(A10))", ADVANCE="YES") 'layer(s)', 'depth(mm)', 'GMDx', 'GSDx', 'm_not', 'm_inf'
          manFile%asdhflag = 1
        end if
        if (BTEST(manFile%am0tfl,0)) then
          call get_simdate(cd, cm, cy)
          ! write(luoasd(sr),"(3(i5))",ADVANCE='NO') lastoper(sr)%day, lastoper(sr)%mon, lastoper(sr)%yr
          write(luoasd(sr),"(3(i5))",ADVANCE='NO') cd, cm, cy
          write(luoasd(sr),"(i10,5(f10.4),A)",ADVANCE='YES') asdlayer, asddepth, &
            gmdx, gsdx, mnot, minf,' New initialization values'

          asd_tdepth = 0.0
          do i=1, asdlayer
            asd_tdepth = asd_tdepth + soil%aszlyt(i)
          end do
          write(luoasd(sr),"(3(i5))",ADVANCE='NO') cd, cm, cy
          write(luoasd(sr),"(i10,5(f10.4),A)",ADVANCE="YES") asdlayer, asd_tdepth, &
            soil%aslagm(1), soil%as0ags(1), soil%aslagn(1), soil%aslagx(1), ' Original values'
        end if

        if (manFile%am0tdb .eq. 2) then  !Debug info printouts
          write (UNIT=0,FMT="(A)",ADVANCE="NO") '//Before set_asd process// '
          write(0,*) 'No. of soil layers to modify/total and depth are: ', asdlayer, soil%nslay, asddepth
          write(UNIT=0,FMT="(A3,5(A10))") 'lay', 'depth', 'GMDx', 'GSDx', 'm_not', 'm_inf'
          do i=1, asdlayer
            write (UNIT=0,FMT="(i3,5(f10.4))",ADVANCE="YES") &
               i, soil%aszlyt(i), soil%aslagm(i), soil%as0ags(i), soil%aslagn(i), soil%aslagx(i)
          end do
          write(0,*) "layers below asdlayer"
          do i=asdlayer+1, soil%nslay
            write (UNIT=0,FMT="(i3,5(f10.4))",ADVANCE="YES") &
               i, soil%aszlyt(i), soil%aslagm(i), soil%as0ags(i), soil%aslagn(i), soil%aslagx(i)
          end do

          ! Convert ASD from modified log-normal to sieve classes
          call asd2m(soil%aslagn, soil%aslagx, soil%aslagm, soil%as0ags, soil%nslay, massf)

          write(0,*) 'after asd2m() call: ', 'msieve: ', msieve, 'nsieve: ', nsieve

          do i=1, asdlayer
            write(UNIT=0, FMT="('massf(',A,',',i2,')')",ADVANCE="NO") 'x', i
            do j=1, msieve+1
              if (j < msieve+1) then
                write(UNIT=0,FMT="(f9.4)",ADVANCE="NO") massf(j,i)
              else
                write(UNIT=0,FMT="(A)",ADVANCE="YES") ""
              endif
            end do
          end do
          write(0,*) "layers below asdlayer"
          do i=asdlayer+1, soil%nslay
            write(UNIT=0, FMT="('massf(',A,',',i2,')')",ADVANCE="NO") 'x', i
            do j=1, msieve+1
              if (j < msieve+1) then
                write(UNIT=0,FMT="(f9.4)",ADVANCE="NO") massf(j,i)
              else
                write(UNIT=0,FMT="(A)",ADVANCE="YES") ""
              endif
            end do
          end do
        end if

        ! do process
        call set_asd(gmdx, gsdx, mnot, minf, asdlayer, soil)

        !write (UNIT=0,FMT="(A)",ADVANCE="NO") '//After set_asd process// '
        !write(0,*) 'no. of soil layers to modify/total and depth are: ', asdlayer, soil%nslay, asddepth
        !write(UNIT=0,FMT="(A3,5(A10))") 'lay', 'depth', 'GMDx', 'GSDx', 'm_not', 'm_inf'
        !do i=1, asdlayer
        !  write (UNIT=0,FMT="(i3,5(f10.4))",ADVANCE="YES") &
        !      i, soil%aszlyt(i), soil%aslagm(i), soil%as0ags(i), soil%aslagn(i), soil%aslagx(i)
        !end do
        !write(0,*) "layers below asdlayer"
        !do i=asdlayer+1, soil%nslay
        !  write (UNIT=0,FMT="(i3,5(f10.4))",ADVANCE="YES") &
        !      i, soil%aszlyt(i), soil%aslagm(i), soil%as0ags(i), soil%aslagn(i), soil%aslagx(i)
        !end do

        if (manFile%am0tdb .eq. 2) then  !Debug info printouts
          ! Convert ASD from modified log-normal to sieve classes
          call asd2m(soil%aslagn, soil%aslagx, soil%aslagm, soil%as0ags, soil%nslay, massf)

          do i=1, asdlayer
            write(UNIT=0, FMT="('massf(',A,',',i2,')')",ADVANCE="NO") 'x', i
            do j=1, msieve+1
              if (j < msieve+1) then
                write(UNIT=0,FMT="(f9.4)",ADVANCE="NO") massf(j,i)
              else
                write(UNIT=0,FMT="(A)",ADVANCE="YES") ""
              endif
            end do
          end do

          ! Convert ASD from modified log-normal to sieve classes
          call asd2m(soil%aslagn, soil%aslagx, soil%aslagm, soil%as0ags, soil%nslay, massf)

          write(0,*) 'after 2nd asd2m() call: ', 'msieve: ', msieve, 'nsieve: ', nsieve

          do i=1, asdlayer
            write(UNIT=0, FMT="('massf(',A,',',i2,')')",ADVANCE="NO") 'x', i
            do j=1, msieve+1
              if (j < msieve+1) then
                write(UNIT=0,FMT="(f9.4)",ADVANCE="NO") massf(j,i)
              else
                write(UNIT=0,FMT="(A)",ADVANCE="YES") ""
              endif
            end do
          end do
          write(0,*) "layers below asdlayer"
          do i=asdlayer+1, soil%nslay
            write(UNIT=0, FMT="('massf(',A,',',i2,')')",ADVANCE="NO") 'x', i
            do j=1, msieve+1
              if (j < msieve+1) then
                write(UNIT=0,FMT="(f9.4)",ADVANCE="NO") massf(j,i)
              else
                write(UNIT=0,FMT="(A)",ADVANCE="YES") ""
              endif
            end do
          end do

          ! Convert ASD back from sieve classes to modified log-normal
          call m2asd(massf, soil%nslay, soil%aslagn, soil%aslagx, soil%aslagm, soil%as0ags)

          write(0,*) 'after m2asd: ','no. of modified soil layers/total are: ', asdlayer, soil%nslay
          do i=1, asdlayer
            write (UNIT=0,FMT="(i10,5(f10.4))",ADVANCE="YES") &
              i, soil%aszlyt(i), soil%aslagm(i), soil%as0ags(i), soil%aslagn(i), soil%aslagx(i)
          end do
          write(0,*) "layers below asdlayer"
          do i=asdlayer+1, soil%nslay
            write (UNIT=0,FMT="(i3,5(f10.4))",ADVANCE="YES") &
              i, soil%aszlyt(i), soil%aslagm(i), soil%as0ags(i), soil%aslagn(i), soil%aslagx(i)
          end do
        end if

        ! post-process stuff
        if (manFile%am0tdb .eq. 1) then
          write (luotdb(sr),*) '//After initialize soil layer asd conditions//'
          call tdbug(sr, prcode, soil, plant)
        end if

        if (BTEST(manFile%am0tfl,0)) then
          write(luoasd(sr),"(3(i5))",ADVANCE='NO') cd, cm, cy
          write(luoasd(sr),"(i10,5(f10.4),A)",ADVANCE="YES") 1, soil%aszlyt(1), &
             soil%aslagm(1), soil%as0ags(1), soil%aslagn(1), soil%aslagx(1), &
             ' New values - After initialized soil layer asd conditions'
          ! write(luoasd(sr),"(i10,4(i5))",ADVANCE="YES") get_simdate_jday(), cd, cm, cy, get_simdate_doy()
        end if

      case (92)  ! initialize (set) soil layer water content value
        ! pre-process stuff
        if (manFile%am0tdb .eq. 1) then
          write (luotdb(sr),*)
          write (luotdb(sr),*) '//Before initialize soil layer water content conditions//'
          call tdbug(sr, prcode, soil, plant)
        end if

        !write(0,*) 'prior to set_wc() call: ', ''
        !write(0,*) ""

        call getManVal(manFile%proc, 'wcdepth', wcdepth)
        call getManVal(manFile%proc, 'wc', wc)

        ! New parameters for set_water content initialization process
        !write(UNIT=0,FMT="(5(f10.4))") wcdepth, wc
        !write(0,*)

        ! Obtain the number of layers the water content values will be set to,
        ! based upon the specified depth and the individual layer thicknesses
        wclayer = tillay(wcdepth, soil%aszlyt, soil%nslay)

        if (BTEST(manFile%am0tfl,1) .and. manFile%wchflag .eq. 0) then
          write(luowc(sr),"(3(A5))",ADVANCE="NO") '# day', 'mon', 'year'
          write(luowc(sr),"(3(A10))", ADVANCE="YES") 'layer(s)', 'depth(mm)', 'wc (Mg/Mg)'
          manFile%wchflag = 1
        end if
        if (BTEST(manFile%am0tfl,1)) then
          call get_simdate(cd, cm, cy)
          ! write(luowc(sr),"(3(i5))",ADVANCE='NO') lastoper(sr)%day, lastoper(sr)%mon, lastoper(sr)%yr
          write(luowc(sr),"(3(i5))",ADVANCE='NO') cd, cm, cy
          write(luowc(sr),"(i10,2(f10.4),A)",ADVANCE='YES') wclayer, wcdepth, wc, &
            ' New initialization values'

          wc_tdepth = 0.0
          do i=1, wclayer
            wc_tdepth = wc_tdepth + soil%aszlyt(i)
          end do
          write(luowc(sr),"(3(i5))",ADVANCE='NO') cd, cm, cy
          write(luowc(sr),"(i10,1(f10.4),A)",ADVANCE="YES") wclayer, wc_tdepth, wc, &
             ' Original values'
        end if

        if (manFile%am0tdb .eq. 2) then  !Debug info printouts
          write (UNIT=0,FMT="(A)",ADVANCE="NO") '//Before set_asd process// '
          write(0,*) 'No. of soil layers to modify/total and depth are: ', wclayer, soil%nslay, wcdepth
          write(UNIT=0,FMT="(A3,1(A10))") 'lay', 'depth'
          do i=1, asdlayer
            write (UNIT=0,FMT="(i3,2(f10.4))",ADVANCE="YES") &
                i, soil%aszlyt(i), soil%ahrwc(i)
          end do
          write(0,*) "layers below wclayer"
          do i=wclayer+1, soil%nslay
            write (UNIT=0,FMT="(i3,2(f10.4))",ADVANCE="YES") &
                i, soil%aszlyt(i), soil%ahrwc(i)
          end do
        end if

        ! do process
        call set_wc(wc, wclayer, soil)

        !write (UNIT=0,FMT="(A)",ADVANCE="NO") '//After set_wc process// '
        !write(0,*) 'no. of soil layers to modify/total and depth are: ', asdlayer, soil%nslay, wcdepth
        !write(UNIT=0,FMT="(A3,2(A10))") 'lay', 'depth', 'wc'
        !do i=1, wclayer
        !  write (UNIT=0,FMT="(i3,2(f10.4))",ADVANCE="YES") &
        !      i, soil%aszlyt(i), soil%ahrwc(i)
        !end do
        !write(0,*) "layers below asdlayer"
        !do i=wclayer+1, soil%nslay
        !  write (UNIT=0,FMT="(i3,2(f10.4))",ADVANCE="YES") &
        !      i, soil%aszlyt(i), soil%ahrwc(i)
        !end do

        ! post-process stuff
        if (manFile%am0tdb .eq. 1) then
          write (luotdb(sr),*) '//After initialize soil layer wc conditions//'
          call tdbug(sr, prcode, soil, plant)
        end if

        if (BTEST(manFile%am0tfl,1)) then
          write(luowc(sr),"(3(i5))",ADVANCE='NO') cd, cm, cy
          write(luowc(sr),"(i10,2(f10.4),A)",ADVANCE="YES") 1, soil%aszlyt(1), &
             soil%ahrwc(1), &
            ' New values - After initialized soil layer water content conditions'
          ! write(luowc(sr),"(i10,4(i5))",ADVANCE="YES") get_simdate_jday(), cd, cm, cy, get_simdate_doy()
        end if

      case(100)  !  UPGMinWEPS_init

        ! new plant created by biomass group (G 03)

        ! crop pool state has been changed, force dependent variable update  
        am0cropupfl = .true.

        ! read population, spacing and yield flags
        call getManVal(manFile%proc, 'rowflag', plant%geometry%rsfg)        ! setup
        call getManVal(manFile%proc, 'rowspac', plant%geometry%xrow)        ! setup
        call getManVal(manFile%proc, 'rowridge', plant%geometry%rg)         ! setup
        call getManVal(manFile%proc, 'plantpop', plant%geometry%dpop)       ! setup
        call getManVal(manFile%proc, 'dmaxshoot', plant%database%dmaxshoot) ! setup
        call getManVal(manFile%proc, 'cbaflag', plant%database%baflg)       ! calibration
        call getManVal(manFile%proc, 'tgtyield', plant%database%ytgt)       ! calibration
        call getManVal(manFile%proc, 'cbafact', plant%database%baf)         ! calibration
        call getManVal(manFile%proc, 'cyrafact', plant%database%yraf)       ! not used
        call getManVal(manFile%proc, 'hyldflag', plant%geometry%hyfg)       ! setup
        ! read yield reporting values and growth characteristics
        call getManVal(manFile%proc, 'hyldunits', plant%database%ynmu)      ! report
        call getManVal(manFile%proc, 'hyldwater', plant%database%ywct)      ! setup, calibration, report
        call getManVal(manFile%proc, 'hyconfact', plant%database%ycon)      ! calibration, report
        call getManVal(manFile%proc, 'idc', plant%database%idc)             ! setup, growth
        call getManVal(manFile%proc, 'grf', plant%database%grf)             ! setup
        call getManVal(manFile%proc, 'ck', plant%database%ck)               ! growth
        call getManVal(manFile%proc, 'hui0', plant%database%ehu0)           ! senescence
        ! read crop growth parameters
        call getManVal(manFile%proc, 'hmx', plant%database%zmxc)            ! growth
        call getManVal(manFile%proc, 'growdepth', plant%database%growdepth) ! setup
        call getManVal(manFile%proc, 'rdmx', plant%database%zmrt)           ! setup
        call getManVal(manFile%proc, 'tbas', plant%database%tmin)           ! setup (days to maturity)
        call getManVal(manFile%proc, 'topt', plant%database%topt)           ! setup (days to maturity)
        call getManVal(manFile%proc, 'thudf', plant%database%thudf)         ! setup (days to maturity)
        call getManVal(manFile%proc, 'dtm', plant%database%tdtm)            ! setup (days to maturity)
        call getManVal(manFile%proc, 'thum', plant%database%thum)           ! setup (days to maturity)
        call getManVal(manFile%proc, 'frsx1', plant%database%fd1(1))        ! not used
        call getManVal(manFile%proc, 'frsx2', plant%database%fd2(1))        ! not used
        call getManVal(manFile%proc, 'frsy1', plant%database%fd1(2))        ! not used
        call getManVal(manFile%proc, 'frsy2', plant%database%fd2(2))        ! not used
        call getManVal(manFile%proc, 'verndel', plant%database%tverndel)    ! not used
        call getManVal(manFile%proc, 'bceff', plant%database%bceff)         ! growth
        call getManVal(manFile%proc, 'a_lf', plant%database%alf)            ! not used
        call getManVal(manFile%proc, 'b_lf', plant%database%blf)            ! not used
        call getManVal(manFile%proc, 'c_lf', plant%database%clf)            ! not used
        call getManVal(manFile%proc, 'd_lf', plant%database%dlf)            ! not used
        call getManVal(manFile%proc, 'a_rp', plant%database%arp)            ! not used
        call getManVal(manFile%proc, 'b_rp', plant%database%brp)            ! not used
        call getManVal(manFile%proc, 'c_rp', plant%database%crp)            ! not used
        call getManVal(manFile%proc, 'd_rp', plant%database%drp)            ! not used
        call getManVal(manFile%proc, 'a_ht', plant%database%aht)            ! not used
        call getManVal(manFile%proc, 'b_ht', plant%database%bht)            ! not used
        call getManVal(manFile%proc, 'ssaa', plant%database%ssa)            ! growth, report
        call getManVal(manFile%proc, 'ssab', plant%database%ssb)            ! growth, report
        call getManVal(manFile%proc, 'sla', plant%database%sla)             ! growth
        call getManVal(manFile%proc, 'huie', plant%database%hue)            ! setup
        call getManVal(manFile%proc, 'transf', plant%database%transf)       ! not used
        call getManVal(manFile%proc, 'diammax', plant%database%diammax)     ! growth
        call getManVal(manFile%proc, 'storeinit', plant%database%storeinit) ! setup
        call getManVal(manFile%proc, 'mshoot', plant%database%shoot)        ! setup
        call getManVal(manFile%proc, 'leafstem', plant%database%fleafstem)  ! growth
        call getManVal(manFile%proc, 'fshoot', plant%database%fshoot)       ! growth
        call getManVal(manFile%proc, 'leaf2stor', plant%database%fleaf2stor) ! growth
        call getManVal(manFile%proc, 'stem2stor', plant%database%fstem2stor) ! growth
        call getManVal(manFile%proc, 'stor2stor', plant%database%fstor2stor) ! growth
        call getManVal(manFile%proc, 'rbc',plant%database%rbc)               ! decomp, mproc
        call getManVal(manFile%proc, 'standdk', plant%database%dkrate(1))   ! decomp
        call getManVal(manFile%proc, 'surfdk', plant%database%dkrate(2))    ! decomp
        call getManVal(manFile%proc, 'burieddk', plant%database%dkrate(3))  ! decomp
        call getManVal(manFile%proc, 'rootdk', plant%database%dkrate(4))    ! decomp
        call getManVal(manFile%proc, 'stemnodk', plant%database%dkrate(5))  ! decomp
        call getManVal(manFile%proc, 'stemdia', plant%database%xstm)        ! decomp
        call getManVal(manFile%proc, 'thrddys', plant%database%ddsthrsh)    ! decomp
        call getManVal(manFile%proc, 'covfact', plant%database%covfact)     ! decomp
        call getManVal(manFile%proc, 'resevapa', plant%database%resevapa)   ! hydro
        call getManVal(manFile%proc, 'resevapb', plant%database%resevapb)   ! hydro
        call getManVal(manFile%proc, 'yield_coefficient', plant%database%yld_coef)  ! growth
        call getManVal(manFile%proc, 'residue_intercept', plant%database%resid_int) ! growth
        call getManVal(manFile%proc, 'regrow_location', plant%database%zloc_regrow) ! not used

        ! reading of process parameters complete

        call plant_setup( sr, plant, soil, lastoper(sr), imprs, rdgflag )

        ! initialize flag to prevent multiple calibration harvests for single crop
        manFile%harv_calib_not_selected = .true.

        ! do process
        ! do not initialize crop if no crop is present
        if( (plant%geometry%dpop .gt. 0.0) .and. (plant%database%idc .gt. 0) ) then
          ! set flag for crop initialization - jt
          plant%growth%am0cif = .true.
          ! set crop growth flag on - jt
          plant%growth%am0cgf = .true.

          ! initialize upgm_grow model
          plant%upgm_grow = UPGM()
          call plant%upgm_grow%plant%plantstate%init()

          ! create all input names
          r_setter = plant%geometry%dpop
          call plant%upgm_grow%plant%plantstate%pars%put("plantpop", r_setter, succ)        

        endif

        ! post-process stuff
        if (manFile%am0tdb .eq. 1) then
          write (luotdb(sr),*) '//After planting process//'
          call tdbug(sr, prcode, soil, plant)
        end if
        call set_calib(sr, plant)
        if( manFile%rpt_season_flg ) then
          ! not reported by the kill process in this
          call report_hydrobal( sr, manFile%mcount, manFile%mperod )
        end if

      case default
        write(0,*) 'Invalid process: ', prname, ' ', prcode
        call exit (1)

      end select

      ! deallocate massf array
      deallocate( massf, stat=alloc_stat)
      if ( alloc_stat .gt. 0 ) then
        write(*,*) 'Unable to deallocate memory for P 11'
        call exit(1)
      end if

      return

    end subroutine doproc

    subroutine mgdreset (bhzirr)

!     + + + PURPOSE + + +
!     mgdreset is called before any management operations for the day are 
!     executed. It resets global variables that are set in management
!     that should only apply for a single day. Resetting them here makes
!     sure that any submodel that needs to use them will have access to
!     them for exactly one day.

!     + + + ARGUMENT DECLARATIONS + + +
      real :: bhzirr   ! daily irrigation amount

!     + + + END SPECIFICATIONS + + +

      am0til = .false.
      bhzirr = 0.0   ! zero out irrig amount from previous day

      return
    end subroutine mgdreset

    subroutine manage( sr, startyr, soil, plant, plantIndex, biotot, mandate, hstate, h1et, manFile)

!     + + + PURPOSE + + +
!     This is the main routine of the MANAGEMENT submodel. The date passed
!     to this routine is checked with the next operation date in the
!     management file. If the dates match, then an operation is to be
!     performed today on the given subregion.
!     The date of last operation (op*) is also passed for output purposes.jt

!     Edit History
!     19-Feb-99   wjr   rewrote
!     20-Feb-99   wjr   made date return

!     + + + KEYWORDS + + +
!     tillage, management

      use datetime_mod, only: difdat, get_simdate
      use file_io_mod, only: luomanage
      use soil_data_struct_defs, only: soil_def
      use biomaterial, only: plant_pointer, biototal
      use mandate_mod, only: opercrop_date
      use stir_report_mod, only: stir_report
      use hydro_data_struct_defs, only: hydro_derived_et, hydro_state
      use manage_data_struct_defs, only: man_file_struct, lastoper

!     + + + ARGUMENT DECLARATIONS + + +
      integer :: sr       ! the subregion number
      integer :: startyr  ! starting year of the simulation run
      type(soil_def), intent(inout) :: soil  ! soil for this subregion
      type(plant_pointer), pointer :: plant     ! pointer to youngest plant data, which chains to older plant data
      integer, intent(inout) :: plantIndex      ! index used for detailed plant/residue output
      type(biototal), intent(in) :: biotot
      type(opercrop_date), dimension(:), intent(inout) :: mandate
      type(hydro_state), intent(inout) :: hstate
      type(hydro_derived_et), intent(inout) :: h1et
      type(man_file_struct), intent(inout) :: manFile

!     + + + LOCAL VARIABLES + + +
      integer :: simdd    ! current simulation day
      integer :: simmm    ! current simulation month
      integer :: simyr    ! current simulation year
      integer :: mansimyr ! the simulation year which corresponds to the year from the management file

!     + + + SUBROUTINES CALLED + + +
!     dooper - DO OPERation is called when dates match
!     dogroup - DO GROUP is called when G code encountered
!     doproc - DO PROCess is called when P code encountered

!     + + + OUTPUT FORMATS + + +
2015     format ('Op Date ', i2,1x,i2,1x,i4,' Rot yr ',i2,' sr #',i2)
!2015     format ('Operation Date ',i2,1x,i2,1x,i4,', subregion #',i2)

!     + + + END SPECIFICATIONS + + +

      ! get current simulation day, month, year
      call get_simdate( simdd, simmm, simyr )

      ! reset any global variables whose setting should only be valid
      ! for one day
      call mgdreset(h1et%zirr)

      ! find simulation year to which management year corresponds
      mansimyr = simyr - mod (simyr-startyr, manFile%mperod) + manFile%oper%operDate%year - 1
      if (difdat (simdd,simmm,simyr,manFile%oper%operDate%day,manFile%oper%operDate%month,mansimyr).ne.0) then
        ! simulation date precedes management date 
        return
      end if

      if (manFile%am0tfl .eq. 1) then
        write (luomanage(sr),*)
        write (luomanage(sr),2015) simdd,simmm,simyr,manFile%oper%operDate%year,sr
      endif

      ! pass date of operation to MAIN for output purposes, used by STIR also
      lastoper(0)%day = manFile%oper%operDate%day
      lastoper(0)%mon = manFile%oper%operDate%month
      lastoper(0)%yr = manFile%oper%operDate%year
      lastoper(sr)%day = manFile%oper%operDate%day
      lastoper(sr)%mon = manFile%oper%operDate%month
      lastoper(sr)%yr = manFile%oper%operDate%year

      ! perform all operations that occur on this date
      do while ( associated(manFile%oper) )
        lastoper(sr)%skip = 0
        cropres = create_crop_residue(soil%nslay)
        call dooper(manFile)
        ! do groups
        manFile%grp => manFile%oper%grpFirst
        do while ( associated(manFile%grp) )
          if(lastoper(sr)%skip.eq.0) then
            call dogroup(soil, plant, plantIndex, manFile)
            ! do processes
            manFile%proc => manFile%grp%procFirst
            do while ( associated(manFile%proc) )
              call doproc(soil, plant, biotot, mandate, hstate, h1et, manFile)
              ! next process
              manFile%proc => manFile%proc%procNext
            end do
            ! next group
            manFile%grp => manFile%grp%grpNext
          end if
        end do
        ! operation complete
        ! deallocate temporary crop residue structure
        call destroy_crop_residue(cropres)
        ! next operation
        manFile%oper => manFile%oper%operNext
        if( associated(manFile%oper) ) then
          ! find simulation year to which management year corresponds
          mansimyr = simyr - mod (simyr-startyr, manFile%mperod) + manFile%oper%operDate%year - 1
          if( difdat (simdd,simmm,simyr,manFile%oper%operDate%day,manFile%oper%operDate%month,mansimyr) .ne. 0) then
            ! this is a future operation
            ! initialize end of season / hydrobal reporting flag to true to generate a report
            manFile%rpt_season_flg = .true.
            exit
          end if
        else  ! not associated
          ! end of rotation
          manFile%mcount = manFile%mcount + 1
          manFile%oper => manFile%operFirst
          ! this is a future operation
          ! initialize end of season / hydrobal reporting flag to true to generate a report
          manFile%rpt_season_flg = .true.
          exit
        end if
      end do

      return

    end subroutine manage

    real function poolmass( nslay, plant )

      ! returns the sum of all biomass (living and fresh residue) in a single plant

      use biomaterial, only: plant_pointer

      ! + + + VARIABLE DECLARATIONS + + +
      integer ::  nslay          ! number of soil layers
      type(plant_pointer), pointer :: plant ! pointer to youngest plant data, which chains to older plant data

      ! + + + LOCAL VARIABLES + + +
      integer :: idx  ! layer counter
      real :: mass    ! summation variable for poolmass

!     + + + LOCAL VARIABLE DEFINITIONS + + +
!     idx     - layer counter
!     mass    - summation variable for poolmass

      ! zero accumulator
      mass = 0.0

      if( associated(plant) ) then
        ! sum all above ground biomass pools
        mass = plant%mass%standstem + plant%mass%standleaf + plant%mass%standstore &
             + plant%mass%flatstem + plant%mass%flatleaf + plant%mass%flatstore
        ! add in below ground biomass pools
        do idx = 1, nslay
          mass = mass + plant%mass%stemz(idx) &
               + plant%mass%rootstorez(idx) + plant%mass%rootfiberz(idx)
        end do

        if( associated(plant%residue) ) then
          ! residue exists
          if( plant%residue%resday .eq. 0 ) then
            ! this is residue killed today, so count it
            mass = mass + plant%residue%standstem + plant%residue%standleaf + plant%residue%standstore &
                 + plant%residue%flatstem + plant%residue%flatleaf + plant%residue%flatstore
            ! add in below ground biomass pools
            do idx = 1, nslay
              mass = mass + plant%residue%stemz(idx) &
                   + plant%residue%rootstorez(idx) + plant%residue%rootfiberz(idx)
            end do
          end if
        end if
      end if

      poolmass = mass

      return
    end function poolmass

    integer function tillay (tdepth, lthick, nlay)
      ! This routine accepts the tillage depth, soil layer thicknesses,
      ! and the number of soil layers.  It returns the number of layers
      ! that will be considered to be within the tillage zone for this
      ! operation.

      real    tdepth
      integer nlay
      real    lthick(nlay)

      integer i
      real    d

      if (tdepth .eq. 0.0) then
        tillay = 0
        return
      else if (tdepth .le. lthick(1)) then
        tillay = 1
        return
      endif
      d = lthick(1)
      do i=2, nlay
        d = d + lthick(i)
        if (tdepth .lt. d) then
          if ( (d - tdepth) .lt. (tdepth - (d-lthick(i))) ) then
            tillay = i
          else
            tillay = i-1
          endif
          ! found depth, result set, return
          return
        endif
      end do
      tillay = nlay
      return
    end function tillay

    real function furrowcut ( bszrgh, bsxrgw, bsxrgs )
!     + + + PURPOSE + + +
!     This function estimates the depth of soil cut from a flat surface
!     to form a ridge and furrow. It is used to find a transpiration depth
!     where a newly planted seed is placed in a deeper, wetter soil layer.

!     + + + KEYWORDS + + +
!     ridges, furrow, seeding, transpiration

!     + + + ARGUMENT DECLARATIONS + + +
      real bszrgh, bsxrgw, bsxrgs

!     + + + ARGUMENT DEFINITIONS + + +
!     bszrgh - Ridge height (mm)
!     bsxrgw - Ridge width (mm)
!     bsxrgs - Ridge spacing (mm)

!     + + + LOCAL VARIABLES + + +
      real furrowdepth

!     + + + LOCAL DEFINITIONS + + +
!     furrowdepth - the furrow depth that the combination of spacing and
!     top width will give if the furrow side slope is limited to 1:1

!     + + + END SPECIFICATIONS + + +

      if ( bszrgh .ge. (bsxrgs - bsxrgw) ) then
          ! ridge height is greater than furrow width
          ! ie. side slope is steeper than 1:1 then limit to 1:1
         furrowdepth = bsxrgs - bsxrgw
      else
         furrowdepth = bszrgh
      endif

      furrowcut = 0.5 * furrowdepth * (1.0 + bsxrgw/bsxrgs)

      return
    end function furrowcut

    subroutine set_prevday_blk( nlay, bsdblk, bsdblk0 )

!     + + + PURPOSE + + +
!     This subroutine sets the previous day bulk density to the present
!     day bulk density

!     + + + KEYWORDS + + +
!     bulk density

!     + + + ARGUMENT DECLARATIONS + + +
      integer nlay
      real bsdblk(*), bsdblk0(*)

!     + + + ARGUMENT DEFINITIONS + + +
!     nlay     - number of soil layers to be updated
!     bsdblk   - bulk density (Mg/m^3)

!     + + + LOCAL VARIABLES + + +
      integer lay

!     + + + END SPECIFICATIONS + + + 

      do lay = 1,nlay
          bsdblk0(lay) = bsdblk(lay)
      end do

    end subroutine set_prevday_blk

    subroutine plant_setup( isr, plant, soil, lastoper, imprs, rdgflag )

      use biomaterial, only: plant_pointer
      use manage_data_struct_defs, only: last_operation
      use weps_cmdline_parms, only: cook_yield
      use p1unconv_mod, only: mgtokg, mmtom
      use datetime_mod, only: dayear
      use soil_data_struct_defs, only: soil_def
      use climate_input_mod, only: cli_mav
      use crop_climate_mod, only: huc1
      use cubic_spline_mod
      use file_io_mod, only: luoinpt
      use crop_data_struct_defs, only: am0cfl

      ! + + +   ARGUMENT DECLARATIONS + + +
      integer, intent(in) :: isr
      type(plant_pointer), pointer :: plant     ! pointer to youngest plant data, which chains to older plant data
      type(soil_def), intent(in) :: soil  ! soil for this subregion
      type(last_operation), intent(in) :: lastoper
      real, intent(in) :: imprs     ! implement ridge spacing (can be used to set row spacing)
      integer, intent(in) :: rdgflag

!     + + + LOCAL VARIABLES + +
      integer idx, mdx, hdate, ydx, lay, bnslay
      integer dtm
      real phu
      real jreal
      real dy_mon(14),mx_air_temp(14),mn_air_temp(14)
      real mx_air_temp2(14), mn_air_temp2(14)
      real sphu, yp1, ypn, bphu, ephu
      real max_air, min_air, heat_unit
      type day_heatunits
          integer day
          real heatunits
          real cumheatunits
      end type day_heatunits
      type(day_heatunits) d1(365), d2(730)

!     Initialize

      bnslay = size(soil%aszlyd)

!     initialize variables needed for season heat unit computation: added on
!     3/16/1998 (A. Retta)
      data dy_mon /-15,15,45,74,105,135,166,196,227,258,288,319,349,380/
!     transfer average monthly temperatures from the global array to a local.
!     For the southern hemisphere, monthly average temperatures should start
!     in July.1?
      do idx=1,12
          mx_air_temp(idx+1) = cli_mav%tmx(idx)
          mn_air_temp(idx+1) = cli_mav%tmn(idx)
      end do
      mx_air_temp(1) = mx_air_temp(13)
      mx_air_temp(14) = mx_air_temp(2)
      mn_air_temp(1) = mn_air_temp(13)
      mn_air_temp(14) = mn_air_temp(2)


        ! input is residue yield ratio. internal use is total biomass yield ratio
        ! all input values are on a dry weight basis.
        ! plant%database%yld_coef = plant%database%yld_coef + 1.0

        ! adjust yield coefficient to generate values on dry weight basis
        ! from total above ground biomass increments
        plant%database%yld_coef = (plant%database%yld_coef + 1.0 - plant%database%ywct/100.0) / (1.0-plant%database%ywct/100.0)

        ! check crop type to see if yield coefficient and grain fraction are used
        if( cook_yield .eq. 1 ) then
            if(     (plant%geometry%hyfg .eq. 0) &
               .or. (plant%geometry%hyfg .eq. 1) &
               .or. (plant%geometry%hyfg .eq. 5) ) then
            ! grain fraction is used
                if(       (plant%database%yld_coef .gt. 1.0 ) &
                    .and. (plant%database%yld_coef * plant%database%grf .lt. 1.0) ) then
                    ! these values will physically require the transfer of
                    ! biomass from stem or leaf pools to meet the incremental
                    ! need for reproductive mass to meet the residue yield ratio.
                    ! If acresid_int is not greateer than zero, this will
                    ! not be possible
                    write(*,*) 'Error: crop named (', trim(plant%bname), &
               ') has bad grain fraction and residue yield ratio values'
                    write(*,*) 'Error: grf*(ryrat+1-mc)/(1-mc) must be > 1',&
                               ', Value is: ',plant%database%yld_coef*plant%database%grf
                    stop
                end if
            end if
        end if

        ! set planting date vars (day, month, rotation year)
        plant%database%plant_doy = dayear(lastoper%day, lastoper%mon, lastoper%yr)
        plant%database%plant_day = lastoper%day
        plant%database%plant_month = lastoper%mon
        plant%database%plant_rotyr = lastoper%yr

        ! initialize transpiration depth parameters
        plant%geometry%zfurcut = 0.0
        plant%geometry%ztransprtmin = 0.0
        plant%geometry%ztransprtmax = 0.0
        ! set row spacing based on flag
        select case( plant%geometry%rsfg )
        case(0) ! Broadcast Planting
            plant%geometry%xrow = 0.0
        case(1) ! Use Implement Ridge Spacing
           if(imprs.gt.0.001) then
             plant%geometry%xrow = imprs * mmtom
             ! check for implement seed placement and ridging
             if( (plant%geometry%rg .eq. 0) .and. (rdgflag .eq. 1) ) then
               ! seed placed in furrow bottom and ridge made unconditionally
               ! set transpiration depth parameters (meters)
               plant%geometry%zfurcut = mmtom * furrowcut(soil%aszrgh,soil%asxrgw,soil%asxrgs)
               plant%geometry%ztransprtmin = plant%geometry%zfurcut + plant%database%growdepth
               plant%geometry%ztransprtmax = plant%database%zmrt
             end if
           else  ! no ridges, so this is a broadcast crop
              plant%geometry%xrow = 0.0
           endif
        case(2) ! Use Specified Row Spacing
           ! convert incoming mm to meters used in acxrow
           plant%geometry%xrow = plant%geometry%xrow*mmtom
        case default
           write(*,*) 'Invalid row spacing flag value'
        end select

!     start calculation of seasonal heat unit requirement
      sphu = 0.
      ephu = 0.
      bphu = 0.
      mdx = 14
      yp1 = 1.0e31    ! signals spline to use natural bound (2nd deriv = 0)
      ypn = 1.0e31    ! signals spline to use natural bound (2nd deriv = 0)

      ! call cubic spline interpolation routines for air temperature
      call spline (dy_mon, mx_air_temp, mdx, yp1, ypn, mx_air_temp2)
      call spline (dy_mon, mn_air_temp, mdx, yp1, ypn, mn_air_temp2)
      do idx = 1, 365
          jreal = idx
          ! calculate daily temps. and heat units
          call splint(dy_mon,mx_air_temp,mx_air_temp2,mdx,jreal,max_air)
          call splint(dy_mon,mn_air_temp,mn_air_temp2,mdx,jreal,min_air)
          heat_unit = huc1(max_air, min_air, plant%database%topt, plant%database%tmin)
          d1(idx)%day=idx
          d1(idx)%heatunits=heat_unit
          d2(idx)%day=idx
          d2(idx)%heatunits=heat_unit
      end do
!     duplicate the first year into the second year
      do idx=1,365
          ydx=idx+365
          d2(ydx)%day=ydx
          d2(ydx)%heatunits=d1(idx)%heatunits
      end do
!     running sum of heat units
      do idx=1,730
          sphu=sphu+d2(idx)%heatunits
          d2(idx)%cumheatunits=sphu
!          if (am0cfl(isr) .gt. 0) then
!              print for debugging
!              write(luoinpt(isr),*) d2(idx)%day,d2(idx)%heatunits,d2(idx)%cumheatunits
!          end if
      end do
      sphu=0.

!     find dtm or phu depending on heat unit flag=1
      do idx=1,730
            if (d2(idx)%day.eq.plant%database%plant_doy) bphu = d2(idx)%cumheatunits
      end do
      if( plant%database%thudf .eq. 1 ) then
         ! use heat unit calculations to find dtm 
         phu = plant%database%thum
         do idx=1,730
            if (d2(idx)%cumheatunits.le.bphu+phu) dtm = d2(idx)%day - plant%database%plant_doy
         end do
         hdate = plant%database%plant_doy + dtm
      else
         ! calculate average seasonal heat units
         dtm=plant%database%tdtm
         hdate = plant%database%plant_doy + dtm
         if( hdate.gt.d2(730)%day) then
            ! this crop grows longer than one year
            ephu = d2(730)%cumheatunits
            phu = ephu - bphu
            ! cap this at two years
            ydx = min(730,hdate - int(d2(730)%day))
            phu = phu + d2(ydx)%cumheatunits
         else
            do idx=1,730
               if (d2(idx)%day.eq.hdate) ephu = d2(idx)%cumheatunits
            end do
            phu = ephu - bphu
         end if
         if (phu .le. 10*(plant%database%topt - plant%database%tmin)) then
            write(*,"(a,i3,a)") 'Warning: Crop will not grow in the',   &
                                 dtm, &
                                 ' days specified. Insufficient heat units accumulate. Check planting date.'
         end if
      end if

      ! print out heat average heat unit and days to maturity
      if (am0cfl(isr) .gt. 0) then
         write(luoinpt(isr), "(i5, i7, i9, i11, i10, 2x, 2f10.1)" ) &
               plant%database%plant_doy, hdate, plant%database%thudf, dtm, plant%database%tdtm, phu, plant%database%thum
      end if

      ! Set the global parameter for maximum heat units to the new calculated value
      ! (this database value is read from management file every time crop is planted,
      ! so changing it here does not corrupt it)
      plant%database%thum = phu

      ! brought in from cinit

      ! determine number of shoots (for seeds, plant%database%shoot should be much
      ! greater than plant%database%storeinit resulting in one shoot with a mass
      ! reduced below plant%database%shoot
      ! units are mg/plant * plant/m^2 / mg/shoot = shoots/m^2
      plant%geometry%dstm = plant%database%storeinit * plant%geometry%dpop / plant%database%shoot
      if( plant%geometry%dstm .lt. plant%geometry%dpop ) then
          ! adjust count to reflect limit
          plant%geometry%dstm = plant%geometry%dpop
          ! not enough mass to make a full shoot
          ! adjust shoot mass to reflect storage mass for one shoot per plant
          ! units are mg/plant * kg/mg * plant/m^2 = kg/m^2
          plant%growth%mtotshoot = plant%database%storeinit * mgtokg * plant%geometry%dpop
      else if( plant%geometry%dstm .gt. plant%database%dmaxshoot*plant%geometry%dpop ) then
          ! adjust count to reflect limit
          plant%geometry%dstm = plant%database%dmaxshoot * plant%geometry%dpop
          ! more mass than maximum number of shoots
          ! adjust total shoot mass to reflect maximum number of shoots
          ! units are shoots/m^2 * mg/shoot * kg/mg = kg/m^2
          plant%growth%mtotshoot = plant%geometry%dstm * plant%database%shoot * mgtokg
      else
          ! mass and shoot number correspond
          ! units are mg/plant * kg/mg * plant/m^2 = kg/m^2
          plant%growth%mtotshoot = plant%database%storeinit * mgtokg * plant%geometry%dpop
      end if

      plant%growth%zgrowpt = plant%database%growdepth

      ! root depth
      plant%geometry%zrtd = plant%database%growdepth

      ! initialize the root storage mass into a single layer
      if( (soil%aszlyd(1)*mmtom .gt. plant%geometry%zrtd) ) then
          ! mg/plant * #/m^2 * 1kg/1.0e6mg = kg/m^2
          plant%mass%rootstorez(1) = plant%database%storeinit * plant%geometry%dpop * mgtokg
      else
          plant%mass%rootstorez(1) = 0.0
      end if
      do lay = 2, bnslay
          if( ( (soil%aszlyd(lay-1)*mmtom .le. plant%geometry%zrtd) &
              .and. (soil%aszlyd(lay)*mmtom .gt. plant%geometry%zrtd) ) ) then
              ! mg/plant * #/m^2 * 1kg/1.0e6mg = kg/m^2
              plant%mass%rootstorez(lay) = plant%database%storeinit * plant%geometry%dpop * mgtokg
          else
              plant%mass%rootstorez(lay) = 0.0
          end if
      end do

      ! set initial emergence (shoot growth) values
      plant%growth%thu_shoot_end = plant%database%hue

      ! set previous values to initial values
      plant%prev%standstem = plant%mass%standstem
      plant%prev%standleaf = plant%mass%standleaf
      plant%prev%standstore = plant%mass%standstore
      plant%prev%flatstem = plant%mass%flatstem
      plant%prev%flatleaf = plant%mass%flatleaf
      plant%prev%flatstore = plant%mass%flatstore
      plant%prev%mshoot = plant%growth%mshoot
      do lay = 1, bnslay
         plant%prev%stemz(lay) = plant%mass%stemz(lay)
         plant%prev%rootstorez(lay) = plant%mass%rootstorez(lay)
         plant%prev%rootfiberz(lay) = plant%mass%rootfiberz(lay)
      end do
      plant%prev%ht = plant%geometry%zht
      plant%prev%zshoot = plant%geometry%zshoot
      plant%prev%stm = plant%geometry%dstm
      plant%prev%rtd = plant%geometry%zrtd
      plant%prev%dayap = plant%growth%dayap
      plant%prev%hucum = plant%growth%thucum
      plant%prev%rthucum = plant%growth%trthucum
      plant%prev%grainf = plant%geometry%grainf
      plant%prev%chillucum = plant%growth%tchillucum
      plant%prev%liveleaf = plant%growth%fliveleaf
      plant%prev%dayspring = plant%growth%dayspring


    end subroutine plant_setup

end module manage_mod

