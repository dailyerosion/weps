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

    integer :: am0cropupfl  ! flag to determine that the crop state has been changed
                                     ! external to crop and that the crop update process must
                                     ! run to synchronize dependent variable values with state values
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

      use weps_interface_defs
      use file_io_mod, only: fopenk
      use manage_data_struct_defs, only: man_file_struct, operation_date
      use flib_sax
      use manage_xml_mod, only: init_man_xml, read_old_manfile
      use manage_xml_mod, only: manfile_complete
      use manage_xml_mod, only: begin_man_element_handler, end_man_element_handler, pcdata_man_chunk_handler

!     + + + ARGUMENT DECLARATIONS + + +
      type(man_file_struct), intent(inout) :: manFile  ! management file data structure

!     + + + LOCAL VARIABLES + + +
      integer :: luimandate   ! unit number for reading in management file
      character*256 :: line

      type(xml_t) :: fxml   ! xml file handle structure
      integer :: read_stat  ! reading file status

!     + + + DATA INITIALIZATIONS + + +

      ! initialize value for crop effect flags
      am0cropupfl = 0

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

    subroutine tdbug(sr, output, soil, crop, residue)

!     + + + PURPOSE + + +
!    This program prints out many of the global variables before
!    and after the call to various MANAGEMENT practices

!     + + + KEY WORDS + + +
!     wind, erosion, tillage, soil, crop, decomposition
!     management

      use weps_interface_defs
      use file_io_mod, only: luotdb
      use soil_data_struct_defs, only: soil_def
      use biomaterial, only: biomatter

!     + + + ARGUMENT DECLARATIONS + + +
      integer sr, output
      type(soil_def), intent(in) :: soil  ! soil for this subregion
      type(biomatter), intent(in) :: crop
      type(biomatter), dimension(:), intent(in) :: residue

!     + + + ARGUMENT DEFINITIONS + + +
!     sr      - subregion number
!     output  - process number for debugging output

!     + + + GLOBAL COMMON BLOCKS + + +
      include 'p1werm.inc'
      include 'h1hydro.inc'
      include 'h1db1.inc'

!     + + + LOCAL VARIABLES + + +
      integer idx
      real total

!     + + + LOCAL DEFINITIONS + + +

!     idx     - loop indexing variable
!     total   - summation variable

!     + + + SUBROUTINES CALLED + + +

!     + + + FUNCTIONS CALLED + + +

!     + + + UNIT NUMBERS FOR INPUT/OUTPUT DEVICES + + +
!     * = screen and keyboard
!    29 = debug MANAGement

!     + + + DATA INITIALIZATIONS + + +

!     + + + INPUT FORMATS + + +

!     + + + OUTPUT FORMATS + + +

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
     &         soil%aslagm(idx),  soil%as0ags(idx) 
          end do

      case (12) ! loosening process (process code 12)
 2041     format(3x,'asdblk  asdsblk   aszlyt') 
 2051     format (1x,f7.2,2x,f7.2,2x,f7.2)
          write(luotdb(sr),2041) 
          do idx = 1,soil%nslay
            write(luotdb(sr),2051)                                          &
     &      soil%asdblk(idx), soil%asdsblk(idx), soil%aszlyt(idx)
          end do 

      case (13) ! mixing process (process code 13)
 2060     format (1x,i4,1x,f7.2,1x,f7.2,f6.2,5f7.2)
 2061     format (f7.3,4f7.2,f6.2,3f7.2)
 2063     format (4x,i1,6(1x,f8.4))
 2065     format (3x,'layer asdblk aszlyt sfsan asfsil asfcla ',        &
     &       'as0ph  ascmg ascna asfcce asfcec asfesp')
 2066     format(3x,'asfom asfnoh asfpoh asfpsp asfsmb asdagd aseags ', &
     &       'ahrwc aheaep ahrwcw ahrwcf ahrwca ahrwcs')
 2068     format(3x,'layer residue(1)%deriv%mrtz(s)  residue(2)%deriv%mrtz(s)  residue(3)%deriv%mrtz(s) ',           &
     &               ' residue(1)%deriv%mbgz(s)  residue(2)%deriv%mbgz(s)  residue(3)%deriv%mbgz(s)') 
          write(luotdb(sr),2065)
          do idx = 1,soil%nslay
            write(luotdb(sr),2060) idx, soil%asdblk(idx), soil%aszlyt(idx), &
     &        soil%asfsan(idx), soil%asfsil(idx), soil%asfcla(idx), &
     &        soil%as0ph(idx), soil%asfcce(idx), soil%asfcec(idx)
          end do 
          write(luotdb(sr),2066)
          do idx = 1,soil%nslay
            write(luotdb(sr),2061) soil%asfom(idx), &
     &        soil%asdagd(idx), soil%aseags(idx), soil%ahrwc(idx), &
     &        soil%aheaep(idx), soil%ahrwcw(idx), soil%ahrwcf(idx), &
     &        soil%ahrwca(idx), soil%ahrwcs(idx)
          end do 
          write(luotdb(sr),2068)
          do idx = 1,soil%nslay
            write(luotdb(sr),2063)                                          &
     &        idx, residue(1)%deriv%mrtz(idx), residue(2)%deriv%mrtz(idx), residue(3)%deriv%mrtz(idx),&
     &        residue(1)%deriv%mbgz(idx), residue(2)%deriv%mbgz(idx), residue(3)%deriv%mbgz(idx)
          end do 

      case (14) ! inversion process (process code 14)
          do idx = 1,soil%nslay
            write(luotdb(sr),2060) idx, soil%asdblk(idx), soil%aszlyt(idx), &
     &        soil%asfsan(idx), soil%asfsil(idx), soil%asfcla(idx), &
     &        soil%as0ph(idx), soil%asfcce(idx), soil%asfcec(idx)
          end do 
          write(luotdb(sr),2066)
          do idx = 1,soil%nslay
            write(luotdb(sr),2061) soil%asfom(idx), &
     &        soil%asdagd(idx), soil%aseags(idx), soil%ahrwc(idx), &
     &        soil%aheaep(idx), soil%ahrwcw(idx), soil%ahrwcf(idx), &
     &        soil%ahrwca(idx), soil%ahrwcs(idx)
          end do 

      case (21) ! below layer compaction (process code 21)

      case (24) ! flatten process variable toughness (process code 24)

      case (25) ! mass bury process variable toughness (process code 25)
 2500     format ('pool stem leaf store rootstore rootfiber (all flat)')
 2501     format ( i2, 5(1x, f7.4) )
          ! sum pools to get total flat mass
          total = cropres%flatstem + cropres%flatleaf + cropres%flatstore  &
     &          + cropres%flatrootstore + cropres%flatrootfiber
          do idx = 1, mnbpls
            total = total + residue(idx)%mass%flatstem + residue(idx)%mass%flatleaf   &
     &            + residue(idx)%mass%flatstore + residue(idx)%mass%flatrootstore     &
     &            + residue(idx)%mass%flatrootfiber
          end do 
          write(luotdb(sr),*) total, ' total flat mass'
          write(luotdb(sr),2500)
          write(luotdb(sr),2501) 0, cropres%flatstem, cropres%flatleaf,       &
     &      cropres%flatstore, cropres%flatrootstore, cropres%flatrootfiber
          do idx = 1, mnbpls
            write(luotdb(sr),2501) idx, residue(idx)%mass%flatstem,                &
     &        residue(idx)%mass%flatleaf, residue(idx)%mass%flatstore,                &
     &        residue(idx)%mass%flatrootstore, residue(idx)%mass%flatrootfiber
          end do 

      case (26) ! re-surface process variable toughness (process code 26)
          ! sum pools to get total flat mass
          total = cropres%flatstem + cropres%flatleaf + cropres%flatstore  &
     &          + cropres%flatrootstore + cropres%flatrootfiber
          do idx = 1, mnbpls
            total = total + residue(idx)%mass%flatstem + residue(idx)%mass%flatleaf   &
     &            + residue(idx)%mass%flatstore + residue(idx)%mass%flatrootstore     &
     &            + residue(idx)%mass%flatrootfiber
          end do 
          write(luotdb(sr),*) total, ' total flat mass'
          write(luotdb(sr),2500)
          write(luotdb(sr),2501) 0, cropres%flatstem, cropres%flatleaf,       &
     &      cropres%flatstore, cropres%flatrootstore, cropres%flatrootfiber
          do idx = 1, mnbpls
            write(luotdb(sr),2501) idx, residue(idx)%mass%flatstem,                &
     &        residue(idx)%mass%flatleaf, residue(idx)%mass%flatstore,                &
     &        residue(idx)%mass%flatrootstore, residue(idx)%mass%flatrootfiber
          end do 

      case (31) ! killing process (process code 31)

      case (32) ! cutting to height process (process code 32)

      case (33) ! cutting by fraction process (process code 33)

      case (34) ! modify standing fall rate process variable toughness (process code 34)
 2074     format(3x,'residue(1)%deriv%mf residue(2)%deriv%mf residue(3)%deriv%mf residue(1)%deriv%mst',                 &
     &      ' residue(2)%deriv%mst residue(3)%deriv%mst')
 2075     format (6(2x,f7.3))
          write(luotdb(sr),2068)
          do idx = 1,soil%nslay
            write(luotdb(sr),2063) idx, residue(1)%deriv%mrtz(idx), residue(2)%deriv%mrtz(idx), &
     &        residue(3)%deriv%mrtz(idx), residue(1)%deriv%mbgz(idx), residue(2)%deriv%mbgz(idx),     &
     &        residue(3)%deriv%mbgz(idx)
          end do 
          write(luotdb(sr),2074)
          write(luotdb(sr),2075) residue(1)%deriv%mf, residue(2)%deriv%mf, residue(3)%deriv%mf,        &
     &      residue(1)%deriv%mst, residue(2)%deriv%mst, residue(3)%deriv%mst

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
          write(luotdb(sr),2164) crop%mass%standstore, crop%geometry%zht, crop%geometry%zrtd
          write(luotdb(sr),2269)
          do idx = 1, mnbpls
            write(luotdb(sr),2073) residue(idx)%deriv%fscv, residue(idx)%deriv%ffcv
          end do 

      case (62) ! biomass remove pool process (process code 62)
 6200     format ( a2, 9(1x, f7.4) )
 6201     format ( i2, 9(1x, f7.4) )
          write(luotdb(sr),*) 'pool stand(height stem leaf store)',         &
     &                'flat(stem leaf store rootstore rootfiber)' 
          write(luotdb(sr),6200) 'T', cropres%zht, cropres%standstem, &
     &        cropres%standleaf, cropres%standstore, &
     &        cropres%flatstem, cropres%flatleaf, &
     &      cropres%flatstore, cropres%flatrootstore, cropres%flatrootfiber
          do idx = 1, mnbpls
            write(luotdb(sr),6201) idx, residue(idx)%geometry%zht, residue(idx)%mass%standstem,&
     &        residue(idx)%mass%standleaf, residue(idx)%mass%standstore,              &
     &        residue(idx)%mass%flatstem, residue(idx)%mass%flatleaf,                 &
     &        residue(idx)%mass%flatstore, residue(idx)%mass%flatrootstore,           &
     &        residue(idx)%mass%flatrootfiber
          end do 

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

      use weps_interface_defs
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
          print*, 'SR',sr,' Do operation', lastoper(sr)%code,' ', trim(lastoper(sr)%name)
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

    subroutine dogroup (soil, manFile)

!     + + + PURPOSE + + +
!     Dogroup reads in any coefficients associated with the group of
!     processes. 

!     + + + KEYWORDS + + +
!     tillage, operation, management

      use weps_interface_defs
      use manage_data_struct_defs, only: lastoper, man_file_struct
      use soil_data_struct_defs, only: soil_def
      use manage_data_struct_mod, only: getManVal

!     + + + ARGUMENT DECLARATIONS + + +
      type(soil_def), intent(in) :: soil  ! soil for this subregion
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
        ! read crop name
        call getManVal(manFile%grp, 'gcropname', cropname)

      case (4) ! ammend group
        ! read amendment name
        call getManVal(manFile%grp, 'gamdname', amdname)

      case default
        write(0, *) 'Invalid Group: ', lastoper(sr)%grcode,             &
     &                                 manFile%grp%grpName
        call exit (1)

      end select

      return

    end subroutine dogroup

    subroutine doproc (soil, crop, cropprev, residue, biotot, mandate, h1et, manFile)

!     + + + PURPOSE + + +
!     Doproc is called when a processline is found in the management file
!     Doproc reads in any coefficients associated with the
!     process. Doproc then makes a call to a subroutine which, in turn,
!     modifies the state variables to mimic the processes of doing the
!     process.

!     + + + KEYWORDS + + +
!     tillage, process, management

      use weps_interface_defs
      use weps_main_mod, only: cook_yield, resurf_roots, wc_type
      use file_io_mod, only: luomanage, luotdb, luoasd, luowc
      use soil_data_struct_defs, only: soil_def
      use biomaterial, only: biomatter, biototal, bio_prevday
      use mandate_mod, only: opercrop_date
      use p1unconv_mod, only: mmtom
      use manage_data_struct_defs, only: lastoper, man_file_struct
      use crop_data_struct_defs, only: am0cfl
      use soilden_mod, only: setbdproc_wc
      use hydro_data_struct_defs, only: hydro_derived_et
      use soil_mod, only: depthini
      use crop_mod, only: crop_endseason
      use report_harvest_mod, only: report_harvest, report_calib_harvest
      use report_hydrobal_mod, only: report_hydrobal
      use datetime_mod, only: get_simdate, get_simdate_jday, get_simdate_doy
      use manage_data_struct_mod, only: getManVal
      use asd_mod, only: msieve, nsieve, sdia, mdia, asd2m, m2asd

!     + + + PARAMETERS AND COMMON BLOCKS + + +
      include 'p1werm.inc'
      include 'h1hydro.inc'
      include 'h1db1.inc'

!     + + + ARGUMENT DECLARATIONS + + +
      type(soil_def), intent(inout) :: soil  ! soil for this subregion
      type(biomatter), intent(inout) :: crop    ! structure containing full crop description
      type(bio_prevday), intent(inout) :: cropprev    ! structure containing crop previous day values
      type(biomatter), dimension(:), intent(inout) :: residue
      type(biototal), intent(in) :: biotot
      type(opercrop_date), dimension(:), intent(inout) :: mandate
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
      integer crop_present, temp_present
      real    noparam1, noparam2, noparam3
      real    rate_mult_vt(mnrbc), thresh_mult_vt(mnrbc)
      real    dummy1(soil%nslay), dummy2(soil%nslay)
      ! temporary crop parameter values for process 65 and 66
      integer trbc, thyfg
      real    tdkrate(5), txstm, tddsthrsh, tcovfact
      real    tresevapa, tresevapb
      real    t0sla, t0ck
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
!     temp_present - flag to show temporary crop biomass pool status
!                0 - no temporary crop biomass present
!                1 - temporary crop biomass present
!     noparam1-6   - variaable to allow reading in six non-used crop parameters in single read statement
!     rate_mult_vt - array of multipliers for modifying standing stem fall rate
!     thresh_mult_vt - array of multipliers for modifying standing stem fall threshold
!     dummy1(soil%nslay), dummy2(soil%nslay) - place holder variables (set to zero)
!                                  for call to poolmass

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
!     invert    - performs an inversion of the vertical soil layers
!     loosn     - performs the loosen/compact process
!     m2asd     - mass fraction to aggregate size distribution converter
!     mix       - mixes components in specified layers
!     orient    - calculates the oriented roughness
!     remove    - performs the biomass removal during a harvest, burn, etc.
!                 and updates the decomposition pools accordingly.
!     rough     - calculated the post tillage random roughness
!     set_asd   - set the asd (gmd,gsd) parameter values
!     tdbug     - subroutine which writes out variables for debugging purposes

!     + + + DATA INITIALIZATIONS + + +
      noparam1 = 0.0
      noparam2 = 0.0
      dummy1 = 0.0  ! array, assigns all values
      dummy2 = 0.0  ! array, assigns all values
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
      if( poolmass( soil%nslay, &
                 crop%mass%standstem, crop%mass%standleaf, crop%mass%standstore, &
                 crop%mass%flatstem, crop%mass%flatleaf, crop%mass%flatstore, &
                 noparam1, noparam2, &
                 crop%mass%stemz, dummy1, dummy2, &
                 crop%mass%rootstorez, crop%mass%rootfiberz ) &
          .gt. 0.0) then
          crop_present = 1
      else
          crop_present = 0
      end if

      if( poolmass( soil%nslay, &
                 cropres%standstem, cropres%standleaf, cropres%standstore, &
                 cropres%flatstem, cropres%flatleaf, cropres%flatstore, &
                 cropres%flatrootstore, cropres%flatrootfiber, &
                 cropres%bgstemz, cropres%bgleafz, cropres%bgstorez, &
                 cropres%bgrootstorez, cropres%bgrootfiberz ) &
          .gt. 0.0 ) then
          temp_present = 1
      else
          temp_present = 0
      end if

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
          call tdbug(sr, prcode, soil, crop, residue)
        end if

        am0til = .true.  !set flag for surface modification

        ! do process
        call crust(kappa,fracarea,soil%asfcr,soil%asflos,soil%asmlos)

        ! post-process stuff
        if (manFile%am0tdb .eq. 1) then
          write (luotdb(sr),*) '//After crust breakdown process//'
          call tdbug(sr, prcode, soil, crop, residue)
        end if

      case (2)  ! random roughness process
        ! pre-process stuff
        if (manFile%am0tdb .eq. 1) then
          write (luotdb(sr),*)
          write (luotdb(sr),*) '//Before random roughness process//'
          call tdbug(sr, prcode, soil, crop, residue)
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
                   biotot%mbgz, biotot%mrtz, &
                   soil%aszlyd)

        ! post-process stuff
        if (manFile%am0tdb .eq. 1) then
          write (luotdb(sr),*) '//After random roughness process//'
          call tdbug(sr, prcode, soil, crop, residue)
        end if

      case (5)  ! oriented roughness process
        ! pre-process stuff
        if (manFile%am0tdb .eq. 1) then
          write (luotdb(sr),*)
          write (luotdb(sr),*) '//Before oriented roughness process//'
          call tdbug(sr, prcode, soil, crop, residue)
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
          call tdbug(sr, prcode, soil, crop, residue)
        end if

      case (11)  ! crushing process
        ! pre-process stuff
        if (manFile%am0tdb .eq. 1) then
          write (luotdb(sr),*)
          write (luotdb(sr),*) '//Before crushing process//'
          call tdbug(sr, prcode, soil, crop, residue)
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
          call tdbug(sr, prcode, soil, crop, residue)
        end if

      case (12)  ! loosening process
        ! pre-process stuff
        if (manFile%am0tdb .eq. 1) then
          write (luotdb(sr),*)
          write (luotdb(sr),*) '//Before loosening process//'
          call tdbug(sr, prcode, soil, crop, residue)
        end if
        if( soil%aslagm(5).gt.soil%aslagx(5) ) then
            write (*,*) 'before loose:',soil%aslagm(5),soil%aslagx(5)
        end if

        ! read the loosening parameter for the implement
        call getManVal(manFile%proc, 'soilos', mu)

        ! do process
        call loosn(mu,fracarea,tlayer, &
          soil%asdblk,soil%asdsblk,soil%aszlyt)

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
          call tdbug(sr, prcode, soil, crop, residue)
        end if

      case (13)  ! mixing process
        ! pre-process stuff
        if (manFile%am0tdb .eq. 1) then
          write (luotdb(sr),*)
          write (luotdb(sr),*) '//Before mixing process//'
          write (luotdb(sr),*) 'Tillage layer depth is', tlayer
          call tdbug(sr, prcode, soil, crop, residue)
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
          soil%asfcs, soil%asfms, soil%asffs, soil%asfvfs, &
          soil%asdwblk, &
          soil%asfom, soil%as0ph, soil%asfcce, soil%asfcec, &
          soil%asfcle, &
          soil%asdagd,soil%aseags, &
          soil%ahrwc, &
          soil%ahrwcs,soil%ahrwcf, soil%ahrwcw, &
          soil%ahrwca, &
          soil%ah0cb, soil%aheaep, soil%ahrsk, &
          residue, &
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
          call tdbug(sr, prcode, soil, crop, residue)
        end if

      case (14)  ! inversion process
        ! pre-process stuff
        if (manFile%am0tdb .eq. 1) then
          write (luotdb(sr),*)
          write (luotdb(sr),*) '//Before inversion process//'
          call tdbug(sr, prcode, soil, crop, residue)
        end if

        ! Convert ASD from modified log-normal to sieve classes
        call asd2m(soil%aslagn, soil%aslagx, soil%aslagm, soil%as0ags, soil%nslay, massf)

        ! do process
        call invert(tlayer,soil%asdblk,soil%aszlyt, &
          soil%asfsan, soil%asfsil,soil%asfcla, soil%asvroc, &
          soil%asfcs, soil%asfms, soil%asffs, soil%asfvfs, &
          soil%asdwblk, &
          soil%asfom, soil%as0ph, soil%asfcce, soil%asfcec, &
          soil%asfcle, &
          soil%asdagd, soil%aseags, &
          soil%ahrwc, &
          soil%ahrwcs,soil%ahrwcf, soil%ahrwcw, &
          soil%ahrwca, &
          soil%ah0cb, soil%aheaep, soil%ahrsk, &
          residue, &
          massf)

        ! post-process stuff

        ! Convert ASD back from sieve classes to modified log-normal
        call m2asd(massf, soil%nslay, soil%aslagn, soil%aslagx, soil%aslagm, soil%as0ags)

        if (manFile%am0tdb .eq. 1) then
          write (luotdb(sr),*) '//After inversion process//'
          call tdbug(sr, prcode, soil, crop, residue)
        end if

      case (21)  ! Compaction
        ! pre-process stuff
        if (manFile%am0tdb .eq. 1) then
          write (luotdb(sr),*)
          write (luotdb(sr),*) '//Before compaction process//'
          call tdbug(sr, prcode, soil, crop, residue)
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
                      procbdadj, soil%asdprocblk, soil%aszlyt )
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
          call tdbug(sr, prcode, soil, crop, residue)
        end if

      case (24)  ! flatten process variable toughness
        ! pre-process stuff
        if (manFile%am0tdb .eq. 1) then
          write (luotdb(sr),*)
          write (luotdb(sr),*) '//Before flatten variable toughness proc.//'
          call tdbug(sr, prcode, soil, crop, residue)
        end if

        call getManVal(manFile%proc, 'fbioflagvt', bioflg)
        call getManVal(manFile%proc, 'massflatvt1', afvt(1))
        call getManVal(manFile%proc, 'massflatvt2', afvt(2))
        call getManVal(manFile%proc, 'massflatvt3', afvt(3))
        call getManVal(manFile%proc, 'massflatvt4', afvt(4))
        call getManVal(manFile%proc, 'massflatvt5', afvt(5))

        ! do process
        call flatvt(afvt, fracarea, crop%database%rbc, &
             crop%mass%standstem, crop%mass%standleaf, crop%mass%standstore, &
             cropres%flatstem, cropres%flatleaf, cropres%flatstore, &
             crop%geometry%dstm, residue, bioflg)

        ! post-process stuff
        ! crop pool state has been changed, force dependent variable update  
        am0cropupfl = 1

        if (manFile%am0tdb .eq. 1) then
          write (luotdb(sr),*) '//After flatten variable toughness proc.//'
          call tdbug(sr, prcode, soil, crop, residue)
        end if

      case (25)  ! mass bury process variable toughness
        ! pre-process stuff
        if (manFile%am0tdb .eq. 1) then
          write (luotdb(sr),*)
          write (luotdb(sr),*) '//Before mass bury variable toughness pr.//'
          call tdbug(sr, prcode, soil, crop, residue)
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
          call mburyvt(mfvt,fracarea,crop%database%rbc, burydistflg, &
                   tlayer,soil%aszlyt,soil%aszlyd, &
             cropres%flatstem, cropres%flatleaf, cropres%flatstore, &
             cropres%flatrootstore, cropres%flatrootfiber, &
             cropres%bgstemz, cropres%bgleafz, cropres%bgstorez, &
             cropres%bgrootstorez, cropres%bgrootfiberz, &
             residue, bioflg)
        end if 

        ! post-process stuff
        if (manFile%am0tdb .eq. 1) then
          write (luotdb(sr),*) '//After mass bury variable toughness pr.//'
          call tdbug(sr, prcode, soil, crop, residue)
        end if

      case (26)  ! re-surface process variable toughness
        ! pre-process stuff
        if (manFile%am0tdb .eq. 1) then
          write (luotdb(sr),*)
          write (luotdb(sr),*) '//Before re-surface vari. toughness proc.//'
          call tdbug(sr, prcode, soil, crop, residue)
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
          call liftvt(mfvt, fracarea, tlayer, residue, resurf_roots, bioflg)
        end if

        ! post-process stuff
        if (manFile%am0tdb .eq. 1) then
          write (luotdb(sr),*) '//After re-surface vari. toughness proc.//'
          call tdbug(sr, prcode, soil, crop, residue)
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
          call tdbug(sr, prcode, soil, crop, residue)
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

        if( crop%growth%am0cgf .and. .not. crop%growth%am0cif ) then
          ! crop growth flag on and not on initialization cycle
          if ((am0kilfl.eq.2).or.((am0kilfl.eq.1).and.((crop%database%idc.eq.1)&
             .or.(crop%database%idc.eq.2).or.(crop%database%idc.eq.4) &
             .or.(crop%database%idc.eq.5)))) then
             ! Stop the crop growth (ie. stop calling crop submodel) and
             ! transfer crop state to temporary crop pool
             call kill_crop( crop%growth%am0cgf, soil%nslay, &
                 crop%mass%standstem, crop%mass%standleaf, crop%mass%standstore, &
                 crop%mass%flatstem, crop%mass%flatleaf, crop%mass%flatstore, &
                 crop%mass%rootstorez, crop%mass%rootfiberz, &
                 crop%mass%stemz, &
                 crop%geometry%zht, crop%geometry%dstm, crop%geometry%xstmrep, crop%geometry%zrtd, &
                 crop%geometry%grainf, &
                 cropres%standstem, cropres%standleaf, cropres%standstore, &
                 cropres%flatstem, cropres%flatleaf, cropres%flatstore, &
                 cropres%bgrootstorez, cropres%bgrootfiberz, &
                 cropres%bgstemz, &
                 cropres%zht, cropres%dstm, cropres%xstmrep, cropres%zrtd, &
                 cropres%grainf )
             if( manFile%rpt_season_flg ) then
               call report_hydrobal( sr, manFile%mcount, manFile%mperod )
               ! This may be harvest or non-harvest termination, allow early harvest warnings
               mature_warn_flg = 1
               call crop_endseason( sr, manFile%mcount, manFile%mperod, am0cfl(sr), &
                                    soil%nslay, mature_warn_flg, crop, cropprev )
               ! set to stop additional report in this operation
               manFile%rpt_season_flg = .false.
             end if
          else if( am0kilfl .eq. 3 ) then
             ! defoliate by dropping all crop leaf mass into crop flat pool
             crop%mass%flatleaf = crop%mass%flatleaf + crop%mass%standleaf
             crop%mass%standleaf = 0.0
          end if
          ! crop pool state has been changed, force dependent variable update  
          am0cropupfl = 1
        end if

        ! post-process stuff
        if (manFile%am0tdb .eq. 1) then
          write (luotdb(sr),*) '//After kill process//'
          call tdbug(sr, prcode, soil, crop, residue)
        end if

      case (32)  ! cutting to height process
        ! pre-process stuff
        if (manFile%am0tdb .eq. 1) then
          write (luotdb(sr),*)
          write (luotdb(sr),*) '//Before cutting to height process//'
          call tdbug(sr, prcode, soil, crop, residue)
        end if

        ! set process parameters
        call getManVal(manFile%proc, 'cutflag', cutflg)
        call getManVal(manFile%proc, 'cutvalh', lastoper(sr)%cutht)
        call getManVal(manFile%proc, 'cyldrmh', pyieldf)
        call getManVal(manFile%proc, 'cplrmh', pstalkf)
        call getManVal(manFile%proc, 'cstrmh', rstandf)

        ! do process
        call cut(cutflg, lastoper(sr)%cutht, pyieldf, pstalkf, rstandf, &
             crop%mass%standstem, crop%mass%standleaf, crop%mass%standstore, &
             crop%mass%flatstem, crop%mass%flatleaf, crop%mass%flatstore, &
             crop%geometry%zht, crop%geometry%grainf, crop%geometry%hyfg, &
             cropres%standstem, cropres%standleaf, cropres%standstore, &
             cropres%flatstem, cropres%flatleaf, cropres%flatstore, &
             cropres%zht, cropres%grainf, residue, &
             mass_rem, mass_left)

        ! post-process stuff
        if (manFile%am0tdb .eq. 1) then
          write (luotdb(sr),*) '//After cutting to height process//'
          call tdbug(sr, prcode, soil, crop, residue)
        end if
        ! crop pool state has been changed, force dependent variable update  
        am0cropupfl = 1
        mature_warn_flg = 1
        ! no harvest report if nothing removed or no crop present
        if( (pyieldf+pstalkf+rstandf.gt.0.0) &
            .and. ((crop_present.gt.0) .or. (temp_present.gt.0)) ) then
          if( manFile%harv_calib_not_selected ) then
            call get_calib_crops(sr, crop)
            call get_calib_yield(sr, manFile%mcount, mass_rem, mass_left, crop)
            call report_calib_harvest( sr, manFile%mcount, mass_rem, mass_left, crop )
            manFile%harv_calib_not_selected = .false.
          end if
          call report_harvest( sr, manFile%mcount, mass_rem, mass_left, 0,1,&
                 mandate, crop)
          if( manFile%rpt_season_flg ) then
              ! not reported by the kill process in this
              call report_hydrobal( sr, manFile%mcount, manFile%mperod )
              call crop_endseason( sr, manFile%mcount, manFile%mperod, am0cfl(sr), &
                                   soil%nslay, mature_warn_flg, crop, cropprev )
              ! set to stop additional report in this operation
              manFile%rpt_season_flg = .false.
          end if
        endif

      case (33)  ! cutting by fraction process
        ! pre-process stuff
        if (manFile%am0tdb .eq. 1) then
          write (luotdb(sr),*)
          write (luotdb(sr),*) '//Before cutting by fraction process//'
          call tdbug(sr, prcode, soil, crop, residue)
        end if

        call getManVal(manFile%proc, 'cutvalf', lastoper(sr)%cutht)
        call getManVal(manFile%proc, 'cyldrmf', pyieldf)
        call getManVal(manFile%proc, 'cplrmf', pstalkf)
        call getManVal(manFile%proc, 'cstrmf', rstandf)

        ! do process
        cutflg = 2
        call cut(cutflg, lastoper(sr)%cutht, pyieldf, pstalkf, rstandf, &
             crop%mass%standstem, crop%mass%standleaf, crop%mass%standstore, &
             crop%mass%flatstem, crop%mass%flatleaf, crop%mass%flatstore, &
             crop%geometry%zht, crop%geometry%grainf, crop%geometry%hyfg, &
             cropres%standstem, cropres%standleaf, cropres%standstore, &
             cropres%flatstem, cropres%flatleaf, cropres%flatstore, &
             cropres%zht, cropres%grainf, residue, &
             mass_rem, mass_left)
        ! post-process stuff
        if (manFile%am0tdb .eq. 1) then
          write (luotdb(sr),*) '//After cutting by fraction process//'
          call tdbug(sr, prcode, soil, crop, residue)
        end if
        ! crop pool state has been changed, force dependent variable update  
        am0cropupfl = 1
        mature_warn_flg = 1
        ! no harvest report if nothing removed or no crop present
        if( (pyieldf+pstalkf+rstandf.gt.0.0) &
            .and. ((crop_present.gt.0) .or. (temp_present.gt.0)) ) then
          if( manFile%harv_calib_not_selected ) then
            call get_calib_crops(sr, crop)
            call get_calib_yield(sr, manFile%mcount, mass_rem, mass_left, crop)
            call report_calib_harvest( sr, manFile%mcount, mass_rem, mass_left, crop )
            manFile%harv_calib_not_selected = .false.
          end if
          call report_harvest( sr, manFile%mcount, mass_rem, mass_left, 0,1,&
                 mandate, crop)
          if( manFile%rpt_season_flg ) then
              ! not reported by the kill process in this
              call report_hydrobal( sr, manFile%mcount, manFile%mperod )
              call crop_endseason( sr, manFile%mcount, manFile%mperod, am0cfl(sr), &
                                   soil%nslay, mature_warn_flg, crop, cropprev )
              ! set to stop additional report in this operation
              manFile%rpt_season_flg = .false.
          end if
        end if

      case (34)  ! modify standing fall rate process variable toughness
        ! pre-process stuff
        if (manFile%am0tdb .eq. 1) then
          write (luotdb(sr),*)
          write (luotdb(sr),*) '//Before modify standing fall rate proc.//'
          call tdbug(sr, prcode, soil, crop, residue)
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
        call fall_mod_vt( rate_mult_vt, thresh_mult_vt, &
                          sel_pool, fracarea, &
                          crop%database%rbc, crop%database%dkrate, crop%database%ddsthrsh, &
                          residue )

        ! post-process stuff
        if (manFile%am0tdb .eq. 1) then
          write (luotdb(sr),*) '//After modify standing fall rate proc.//'
          call tdbug(sr, prcode, soil, crop, residue)
        end if

      case (37)  ! thinning to population process
        ! pre-process stuff
        if (manFile%am0tdb .eq. 1) then
          write (luotdb(sr),*)
          write (luotdb(sr),*) '//Before thinning to population process//'
          call tdbug(sr, prcode, soil, crop, residue)
        end if

        call getManVal(manFile%proc, 'thinvalp', thinval)
        call getManVal(manFile%proc, 'tyldrmp', pyieldf)
        call getManVal(manFile%proc, 'tplrmp', pstalkf)
        call getManVal(manFile%proc, 'tstrmp', rstandf)

        ! do process
        thinflg = 1
        call thin(thinflg, thinval, pyieldf, pstalkf, rstandf, &
             crop%mass%standstem, crop%mass%standleaf, crop%mass%standstore, &
             crop%mass%flatstem, crop%mass%flatleaf, crop%mass%flatstore, &
             crop%geometry%dstm, crop%geometry%grainf, crop%geometry%hyfg, &
             cropres%standstem, cropres%standleaf, cropres%standstore, &
             cropres%flatstem, cropres%flatleaf, cropres%flatstore, &
             cropres%dstm, cropres%grainf, residue, &
             mass_rem, mass_left)

        ! post-process stuff
        if (manFile%am0tdb .eq. 1) then
          write (luotdb(sr),*) '//After thinning to population process//'
          call tdbug(sr, prcode, soil, crop, residue)
        end if
        ! crop pool state has been changed, force dependent variable update  
        am0cropupfl = 1
        mature_warn_flg = 1
        ! no harvest report if nothing removed or no crop present
        if( (pyieldf+pstalkf+rstandf.gt.0.0) &
            .and. ((crop_present.gt.0) .or. (temp_present.gt.0)) ) then
          if( manFile%harv_calib_not_selected ) then
            call get_calib_crops(sr, crop)
            call get_calib_yield(sr, manFile%mcount, mass_rem, mass_left, crop)
            call report_calib_harvest( sr, manFile%mcount, mass_rem, mass_left, crop )
            manFile%harv_calib_not_selected = .false.
          end if
          call report_harvest( sr, manFile%mcount, mass_rem, mass_left, 0,1,&
      &          mandate, crop)
          if( manFile%rpt_season_flg ) then
              ! not reported by the kill process in this
              call report_hydrobal( sr, manFile%mcount, manFile%mperod )
              call crop_endseason( sr, manFile%mcount, manFile%mperod, am0cfl(sr), &
                                   soil%nslay, mature_warn_flg, crop, cropprev )
              ! set to stop additional report in this operation
              manFile%rpt_season_flg = .false.
          end if
        end if

      case (38)  ! thinning by fraction process
        ! pre-process stuff
        if (manFile%am0tdb .eq. 1) then
          write (luotdb(sr),*)
          write (luotdb(sr),*) '//Before thinning by fraction process//'
          call tdbug(sr, prcode, soil, crop, residue)
        end if

        call getManVal(manFile%proc, 'thinvalf', thinval)
        call getManVal(manFile%proc, 'tyldrmf', pyieldf)
        call getManVal(manFile%proc, 'tplrmf', pstalkf)
        call getManVal(manFile%proc, 'tstrmf', rstandf)

        ! do process
        thinflg = 0
        call thin(thinflg, thinval, pyieldf, pstalkf, rstandf, &
             crop%mass%standstem, crop%mass%standleaf, crop%mass%standstore, &
             crop%mass%flatstem, crop%mass%flatleaf, crop%mass%flatstore, &
             crop%geometry%dstm, crop%geometry%grainf, crop%geometry%hyfg, &
             cropres%standstem, cropres%standleaf, cropres%standstore, &
             cropres%flatstem, cropres%flatleaf, cropres%flatstore, &
             cropres%dstm, cropres%grainf, residue, &
             mass_rem, mass_left)
        ! post-process stuff
        if (manFile%am0tdb .eq. 1) then
          write (luotdb(sr),*) '//After thinning by fraction process//'
          call tdbug(sr, prcode, soil, crop, residue)
        end if
        ! crop pool state has been changed, force dependent variable update  
        am0cropupfl = 1
        mature_warn_flg = 1
        ! no harvest report if nothing removed or no crop present
        if( (pyieldf+pstalkf+rstandf.gt.0.0) &
            .and. ((crop_present.gt.0) .or. (temp_present.gt.0)) ) then
            call get_calib_crops(sr, crop)
            call get_calib_yield(sr, manFile%mcount, mass_rem, mass_left, crop)
            call report_harvest( sr, manFile%mcount, mass_rem, mass_left, 0,&
                 1, mandate, crop)
            call report_calib_harvest( sr, manFile%mcount, mass_rem, mass_left, crop )
            if( manFile%rpt_season_flg ) then
              ! not reported by the kill process in this
              call report_hydrobal( sr, manFile%mcount, manFile%mperod )
              call crop_endseason( sr, manFile%mcount, manFile%mperod, am0cfl(sr), &
                                   soil%nslay, mature_warn_flg, crop, cropprev )
              ! set to stop additional report in this operation
              manFile%rpt_season_flg = .false.
            end if
        end if

      case (40)  ! crop to biomass transfer process
        ! pre-process stuff
        if (manFile%am0tdb .eq. 1) then
          write (luotdb(sr),*)
          write (luotdb(sr),*) '//Before biomass transfer process//'
          call tdbug(sr, prcode, soil, crop, residue)
        end if

        ! do process
        ! This checks if there is biomass in the temporary pool to be
        ! transferred into the residue pool. This check is here so that
        ! repeated calls to trans do not put all biomass in the 
        ! "slow decay" pool.

        if ( temp_present .gt. 0.0 ) then
          call trans( &
            cropres%standstem, cropres%standleaf, cropres%standstore, &
            cropres%flatstem, cropres%flatleaf, cropres%flatstore, &
            cropres%flatrootstore, cropres%flatrootfiber, &
            cropres%bgstemz, cropres%bgleafz, cropres%bgstorez, &
            cropres%bgrootstorez, cropres%bgrootfiberz, &
            cropres%zht, cropres%dstm,cropres%xstmrep,cropres%grainf, &
            crop%bname, crop%database%xstm, crop%database%rbc, crop%database%sla, crop%database%ck, &
            crop%database%dkrate, crop%database%covfact, crop%database%ddsthrsh, crop%geometry%hyfg, &
            crop%database%resevapa, crop%database%resevapb, &
            soil%nslay, residue )
        end if

        ! post-process stuff
        if (manFile%am0tdb .eq. 1) then
          write (luotdb(sr),*) '//After biomass transfer process//'
          call tdbug(sr, prcode, soil, crop, residue)
        end if

      case (42)  ! flagged cutting to height process
        ! pre-process stuff
        if (manFile%am0tdb .eq. 1) then
          write (luotdb(sr),*)
          write (luotdb(sr),*) '//Before flagged cutting to height proc.//'
          call tdbug(sr, prcode, soil, crop, residue)
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
             crop%mass%standstem, crop%mass%standleaf, crop%mass%standstore, &
             crop%mass%flatstem, crop%mass%flatleaf, crop%mass%flatstore, &
             crop%geometry%zht, crop%geometry%grainf, crop%geometry%hyfg, &
             cropres%standstem, cropres%standleaf, cropres%standstore, &
             cropres%flatstem, cropres%flatleaf, cropres%flatstore, &
             cropres%zht, cropres%grainf, residue, &
             mass_rem, mass_left)

        ! post-process stuff
        if (manFile%am0tdb .eq. 1) then
          write (luotdb(sr),*) '//After flagged cutting to height proc.//'
          call tdbug(sr, prcode, soil, crop, residue)
        end if
        ! crop pool state has been changed, force dependent variable update  
        am0cropupfl = 1
        ! no harvest report if nothing removed or no crop present
        if( (pyieldf+pstalkf+rstandf.gt.0.0) &
            .and. ((crop_present.gt.0) .or. (temp_present.gt.0)) ) then
          if(      (harv_calib_flg .gt. 0) &
             .and. (manFile%harv_calib_not_selected) ) then
            call get_calib_crops(sr, crop)
            call get_calib_yield(sr, manFile%mcount, mass_rem, mass_left, crop)
            call report_calib_harvest( sr, manFile%mcount, mass_rem, mass_left, crop )
            manFile%harv_calib_not_selected = .false.
          end if
          call report_harvest( sr, manFile%mcount, mass_rem, mass_left, &
                               harv_unit_flg, harv_report_flg, &
                               mandate, crop )
          if( manFile%rpt_season_flg ) then
              ! not reported by the kill process in this
              call report_hydrobal( sr, manFile%mcount, manFile%mperod )
              call crop_endseason( sr, manFile%mcount, manFile%mperod, am0cfl(sr), &
                                   soil%nslay, mature_warn_flg, crop, cropprev )
              ! set to stop additional report in this operation
              manFile%rpt_season_flg = .false.
          end if
        endif

      case (43)  ! flagged cutting by fraction process
        ! pre-process stuff
        if (manFile%am0tdb .eq. 1) then
          write (luotdb(sr),*)
          write (luotdb(sr),*) '//Before flagged cutting by fraction pr.//'
          call tdbug(sr, prcode, soil, crop, residue)
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
             crop%mass%standstem, crop%mass%standleaf, crop%mass%standstore, &
             crop%mass%flatstem, crop%mass%flatleaf, crop%mass%flatstore, &
             crop%geometry%zht, crop%geometry%grainf, crop%geometry%hyfg, &
             cropres%standstem, cropres%standleaf, cropres%standstore, &
             cropres%flatstem, cropres%flatleaf, cropres%flatstore, &
             cropres%zht, cropres%grainf, residue, &
             mass_rem, mass_left)
        ! post-process stuff
        if (manFile%am0tdb .eq. 1) then
          write (luotdb(sr),*) '//After flagged cutting by fraction pr.//'
          call tdbug(sr, prcode, soil, crop, residue)
        end if
        ! crop pool state has been changed, force dependent variable update  
        am0cropupfl = 1
        ! no harvest report if nothing removed or no crop present
        if( (pyieldf+pstalkf+rstandf.gt.0.0) &
            .and. ((crop_present.gt.0) .or. (temp_present.gt.0)) ) then
          if(      (harv_calib_flg .gt. 0) &
             .and. (manFile%harv_calib_not_selected) ) then
            call get_calib_crops(sr, crop)
            call get_calib_yield(sr, manFile%mcount, mass_rem, mass_left, crop)
            call report_calib_harvest( sr, manFile%mcount, mass_rem, mass_left, crop )
            manFile%harv_calib_not_selected = .false.
          end if
          call report_harvest( sr, manFile%mcount, mass_rem, mass_left, &
                               harv_unit_flg, harv_report_flg, &
                               mandate, crop )
          if( manFile%rpt_season_flg ) then
              ! not reported by the kill process in this
              call report_hydrobal( sr, manFile%mcount, manFile%mperod )
              call crop_endseason( sr, manFile%mcount, manFile%mperod, am0cfl(sr), &
                                   soil%nslay, mature_warn_flg, crop, cropprev )
              ! set to stop additional report in this operation
              manFile%rpt_season_flg = .false.
          end if
        end if

      case (47)  ! flagged thinning to population process
        ! pre-process stuff
        if (manFile%am0tdb .eq. 1) then
          write (luotdb(sr),*)
          write(luotdb(sr),*)'//Before flagged thinning to population pr.//'
          call tdbug(sr, prcode, soil, crop, residue)
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
        call thin(thinflg, thinval, pyieldf, pstalkf, rstandf, &
             crop%mass%standstem, crop%mass%standleaf, crop%mass%standstore, &
             crop%mass%flatstem, crop%mass%flatleaf, crop%mass%flatstore, &
             crop%geometry%dstm, crop%geometry%grainf, crop%geometry%hyfg, &
             cropres%standstem, cropres%standleaf, cropres%standstore, &
             cropres%flatstem, cropres%flatleaf, cropres%flatstore, &
             cropres%dstm, cropres%grainf, residue, &
             mass_rem, mass_left)

        ! post-process stuff
        if (manFile%am0tdb .eq. 1) then
          write(luotdb(sr),*) '//After flagged thinning to population pr.//'
          call tdbug(sr, prcode, soil, crop, residue)
        end if
        ! crop pool state has been changed, force dependent variable update  
        am0cropupfl = 1
        ! no harvest report if nothing removed or no crop present
        if( (pyieldf+pstalkf+rstandf.gt.0.0) &
            .and. ((crop_present.gt.0) .or. (temp_present.gt.0)) ) then
          if(      (harv_calib_flg .gt. 0) &
             .and. (manFile%harv_calib_not_selected) ) then
            call get_calib_crops(sr, crop)
            call get_calib_yield(sr, manFile%mcount, mass_rem, mass_left, crop)
            call report_calib_harvest( sr, manFile%mcount, mass_rem, mass_left, crop )
            manFile%harv_calib_not_selected = .false.
          end if
          call report_harvest( sr, manFile%mcount, mass_rem, mass_left, &
                               harv_unit_flg, harv_report_flg, &
                               mandate, crop )
          if( manFile%rpt_season_flg ) then
              ! not reported by the kill process in this
              call report_hydrobal( sr, manFile%mcount, manFile%mperod )
              call crop_endseason( sr, manFile%mcount, manFile%mperod, am0cfl(sr), &
                                   soil%nslay, mature_warn_flg, crop, cropprev )
              ! set to stop additional report in this operation
              manFile%rpt_season_flg = .false.
          end if
        end if

      case (48)  ! flagged thinning by fraction process
        ! pre-process stuff
        if (manFile%am0tdb .eq. 1) then
          write (luotdb(sr),*)
          write (luotdb(sr),*) '//Before flagged thinning by fraction pr.//'
          call tdbug(sr, prcode, soil, crop, residue)
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
        call thin(thinflg, thinval, pyieldf, pstalkf, rstandf, &
             crop%mass%standstem, crop%mass%standleaf, crop%mass%standstore, &
             crop%mass%flatstem, crop%mass%flatleaf, crop%mass%flatstore, &
             crop%geometry%dstm, crop%geometry%grainf, crop%geometry%hyfg, &
             cropres%standstem, cropres%standleaf, cropres%standstore, &
             cropres%flatstem, cropres%flatleaf, cropres%flatstore, &
             cropres%dstm, cropres%grainf, residue, &
             mass_rem, mass_left)
        ! post-process stuff
        if (manFile%am0tdb .eq. 1) then
          write (luotdb(sr),*) '//After flagged thinning by fraction pr.//'
          call tdbug(sr, prcode, soil, crop, residue)
        end if
        ! crop pool state has been changed, force dependent variable update  
        am0cropupfl = 1
        ! no harvest report if nothing removed or no crop present
        if( (pyieldf+pstalkf+rstandf.gt.0.0) &
            .and. ((crop_present.gt.0) .or. (temp_present.gt.0)) ) then
          if(      (harv_calib_flg .gt. 0) &
             .and. (manFile%harv_calib_not_selected) ) then
            call get_calib_crops(sr, crop)
            call get_calib_yield(sr, manFile%mcount, mass_rem, mass_left, crop)
            call report_calib_harvest( sr, manFile%mcount, mass_rem, mass_left, crop )
            manFile%harv_calib_not_selected = .false.
          end if
          call report_harvest( sr, manFile%mcount, mass_rem, mass_left, &
                               harv_unit_flg, harv_report_flg, &
                               mandate, crop )
          if( manFile%rpt_season_flg ) then
              ! not reported by the kill process in this
              call report_hydrobal( sr, manFile%mcount, manFile%mperod )
              call crop_endseason( sr, manFile%mcount, manFile%mperod, am0cfl(sr), &
                                   soil%nslay, mature_warn_flg, crop, cropprev )
              ! set to stop additional report in this operation
              manFile%rpt_season_flg = .false.
          end if
        end if

      case (50)  ! residue initialization process
        ! New residue is assigned to residue pool 1.
        ! Existing residue is set to 0.
        ! pre-process stuff
        if (manFile%am0tdb .eq. 1) then
          write (luotdb(sr),*)
          write (luotdb(sr),*) '//Before residue initialization process//'
          call tdbug(sr, prcode, soil, crop, residue)
        end if

        ! do process
        ! Read surface residue counts and amount
        call getManVal(manFile%proc, 'numst', residue(1)%geometry%dstm)
        call getManVal(manFile%proc, 'rstandht', residue(1)%geometry%zht)
        call getManVal(manFile%proc, 'rstandmass', residue(1)%mass%standstem)
        call getManVal(manFile%proc, 'rflatmass', residue(1)%mass%flatstem)
        call getManVal(manFile%proc, 'rbc', residue(1)%database%rbc)
        call getManVal(manFile%proc, 'rburiedmass', dmassres)
        call getManVal(manFile%proc, 'rburieddepth', zmassres)
        call getManVal(manFile%proc, 'rrootmass', dmassrot)
        call getManVal(manFile%proc, 'rrootdepth', zmassrot)
        ! place buried residue in pools by layer
        call resinit(dmassrot, zmassrot, soil%nslay, residue(1)%mass%rootfiberz, soil%aszlyt)
        call resinit(dmassres,zmassres,soil%nslay, residue(1)%mass%stemz, soil%aszlyt)
        ! read decomposition parameters for type of residue buried
        call getManVal(manFile%proc, 'standdk', residue(1)%database%dkrate(1))
        call getManVal(manFile%proc, 'surfdk', residue(1)%database%dkrate(2))
        call getManVal(manFile%proc, 'burieddk', residue(1)%database%dkrate(3))
        call getManVal(manFile%proc, 'rootdk', residue(1)%database%dkrate(4))
        call getManVal(manFile%proc, 'stemnodk', residue(1)%database%dkrate(5))
        call getManVal(manFile%proc, 'stemdia', residue(1)%database%xstm)
        call getManVal(manFile%proc, 'thrddys', residue(1)%database%ddsthrsh)
        call getManVal(manFile%proc, 'covfact', residue(1)%database%covfact)
        ! read decomposition parameters for type of residue buried
        call getManVal(manFile%proc, 'resevapa', residue(1)%database%resevapa)
        call getManVal(manFile%proc, 'resevapb', residue(1)%database%resevapa)

        ! give residue the proper name
        residue(1)%bname = cropname
        ! post-process stuff
        ! set calendar days for residue to zero
        residue(1)%decomp%resday = 0
        residue(1)%decomp%resyear = residue(1)%decomp%resyear + 1
        ! set cumulative decomposition days for residue to zero
        residue(1)%decomp%cumdds = 0.0
        residue(1)%decomp%cumddf = 0.0
        do idx=1,soil%nslay
          residue(1)%decomp%cumddg(idx) = 0.0
        end do

        ! zero out uninitialized mass pools
        dmassres = 0.0
        zmassres = 0.0
        dmassrot = 0.0
        zmassrot = 0.0
        do idx = 2, mnbpls
            residue(idx)%mass%standstem = 0.0
            residue(idx)%mass%flatstem = 0.0
            call resinit(dmassrot, zmassrot, soil%nslay, residue(idx)%mass%rootfiberz, soil%aszlyt)
            call resinit(dmassres,zmassres,soil%nslay, residue(idx)%mass%stemz, soil%aszlyt)
        end do

        do idx = 1, mnbpls
            residue(idx)%mass%standleaf = 0.0
            residue(idx)%mass%standstore = 0.0
            residue(idx)%mass%flatleaf = 0.0
            residue(idx)%mass%flatstore = 0.0
            residue(idx)%mass%flatrootstore = 0.0
            residue(idx)%mass%flatrootfiber = 0.0
            call resinit(dmassres, zmassres, soil%nslay, residue(idx)%mass%leafz, soil%aszlyt)
            call resinit(dmassres, zmassres, soil%nslay, residue(idx)%mass%storez, soil%aszlyt)
            call resinit(dmassrot, zmassrot, soil%nslay, residue(idx)%mass%rootstorez, soil%aszlyt)
            ! set other state variables
            residue(idx)%geometry%xstmrep = residue(idx)%database%xstm
            residue(idx)%geometry%grainf = 1.0
            residue(idx)%geometry%hyfg = 0
        end do
        ! post-process stuff
        if (manFile%am0tdb .eq. 1) then
          write (luotdb(sr),*) '//After residue initialization process//'
          call tdbug(sr, prcode, soil, crop, residue)
        end if

      case (51)  ! planting process
        ! pre-process stuff
        if (manFile%am0tdb .eq. 1) then
          write (luotdb(sr),*)
          write (luotdb(sr),*) '//Before planting process//'
          call tdbug(sr, prcode, soil, crop, residue)
        end if
        ! kill and transfer only if existing crop and new crop
        if( crop%growth%am0cgf.and.(crop%geometry%dstm.gt.0.0) ) then
          ! In a growth model growing only a single crop, any existing crop must
          ! be killed and transferred to residue or all the residue will be lost
          ! when the new crop is initialized
          ! (remove when multiple species capable)
          call kill_crop( crop%growth%am0cgf, soil%nslay, &
                 crop%mass%standstem, crop%mass%standleaf, crop%mass%standstore, &
                 crop%mass%flatstem, crop%mass%flatleaf, crop%mass%flatstore, &
                 crop%mass%rootstorez, crop%mass%rootfiberz, &
                 crop%mass%stemz, &
                 crop%geometry%zht, crop%geometry%dstm, crop%geometry%xstmrep, crop%geometry%zrtd, &
                 crop%geometry%grainf, &
                 cropres%standstem, cropres%standleaf, cropres%standstore, &
                 cropres%flatstem, cropres%flatleaf, cropres%flatstore, &
                 cropres%bgrootstorez, cropres%bgrootfiberz, &
                 cropres%bgstemz, &
                 cropres%zht, cropres%dstm, cropres%xstmrep, cropres%zrtd, &
                 cropres%grainf )
          call trans( &
            cropres%standstem, cropres%standleaf, cropres%standstore, &
            cropres%flatstem, cropres%flatleaf, cropres%flatstore, &
            cropres%flatrootstore, cropres%flatrootfiber, &
            cropres%bgstemz, cropres%bgleafz, cropres%bgstorez, &
            cropres%bgrootstorez, cropres%bgrootfiberz, &
            cropres%zht, cropres%dstm,cropres%xstmrep,cropres%grainf, &
            crop%bname, crop%database%xstm, crop%database%rbc, crop%database%sla, crop%database%ck, &
            crop%database%dkrate, crop%database%covfact, crop%database%ddsthrsh, crop%geometry%hyfg, &
            crop%database%resevapa, crop%database%resevapb, &
            soil%nslay, residue )
          ! non-harvest termination, suppress early harvest warnings
          mature_warn_flg = 0
          call crop_endseason( sr, manFile%mcount, manFile%mperod, am0cfl(sr), &
                               soil%nslay, mature_warn_flg, crop, cropprev )
          ! set to guarantee corresponding report hydrolbal at end of planting
          manFile%rpt_season_flg = .true.
        endif
        ! crop pool state has been changed, force dependent variable update  
        am0cropupfl = 1

        ! read population, spacing and yield flags
        call getManVal(manFile%proc, 'rowflag', crop%geometry%rsfg)
        call getManVal(manFile%proc, 'rowspac', crop%geometry%xrow)
        call getManVal(manFile%proc, 'rowridge', crop%geometry%rg)
        call getManVal(manFile%proc, 'plantpop', crop%geometry%dpop)
        call getManVal(manFile%proc, 'dmaxshoot', crop%database%dmaxshoot)
        call getManVal(manFile%proc, 'cbaflag', crop%database%baflg)
        call getManVal(manFile%proc, 'tgtyield', crop%database%ytgt)
        call getManVal(manFile%proc, 'cbafact', crop%database%baf)
        call getManVal(manFile%proc, 'cyrafact', crop%database%yraf)
        call getManVal(manFile%proc, 'hyldflag', crop%geometry%hyfg)
        ! read yield reporting name
        call getManVal(manFile%proc, 'hyldunits', crop%database%ynmu)
        ! read yield reporting values and growth characteristics
        call getManVal(manFile%proc, 'hyldwater', crop%database%ywct)
        call getManVal(manFile%proc, 'hyconfact', crop%database%ycon)
        call getManVal(manFile%proc, 'idc', crop%database%idc)
        call getManVal(manFile%proc, 'grf', crop%database%grf)
        call getManVal(manFile%proc, 'ck', crop%database%ck)
        call getManVal(manFile%proc, 'hui0', crop%database%ehu0)
        ! read crop growth parameters
        call getManVal(manFile%proc, 'hmx', crop%database%zmxc)
        call getManVal(manFile%proc, 'growdepth', crop%database%growdepth)
        call getManVal(manFile%proc, 'rdmx', crop%database%zmrt)
        call getManVal(manFile%proc, 'tbas', crop%database%tmin)
        call getManVal(manFile%proc, 'topt', crop%database%topt)
        call getManVal(manFile%proc, 'thudf', crop%database%thudf)
        call getManVal(manFile%proc, 'dtm', crop%database%tdtm)
        call getManVal(manFile%proc, 'thum', crop%database%thum)
        call getManVal(manFile%proc, 'frsx1', crop%database%fd1(1))
        call getManVal(manFile%proc, 'frsx2', crop%database%fd2(1))
        call getManVal(manFile%proc, 'frsy1', crop%database%fd1(2))
        call getManVal(manFile%proc, 'frsy2', crop%database%fd2(2))
        call getManVal(manFile%proc, 'verndel', crop%database%tverndel)
        call getManVal(manFile%proc, 'bceff', crop%database%bceff)
        call getManVal(manFile%proc, 'a_lf', crop%database%alf)
        call getManVal(manFile%proc, 'b_lf', crop%database%blf)
        call getManVal(manFile%proc, 'c_lf', crop%database%clf)
        call getManVal(manFile%proc, 'd_lf', crop%database%dlf)
        call getManVal(manFile%proc, 'a_rp', crop%database%arp)
        call getManVal(manFile%proc, 'b_rp', crop%database%brp)
        call getManVal(manFile%proc, 'c_rp', crop%database%crp)
        call getManVal(manFile%proc, 'd_rp', crop%database%drp)
        call getManVal(manFile%proc, 'a_ht', crop%database%aht)
        call getManVal(manFile%proc, 'b_ht', crop%database%bht)
        call getManVal(manFile%proc, 'ssaa', crop%database%ssa)
        call getManVal(manFile%proc, 'ssab', crop%database%ssb)
        call getManVal(manFile%proc, 'sla', crop%database%sla)
        call getManVal(manFile%proc, 'huie', crop%database%hue)
        call getManVal(manFile%proc, 'transf', crop%database%transf)
        call getManVal(manFile%proc, 'diammax', crop%database%diammax)
        call getManVal(manFile%proc, 'storeinit', crop%database%storeinit)
        call getManVal(manFile%proc, 'mshoot', crop%database%shoot)
        call getManVal(manFile%proc, 'leafstem', crop%database%fleafstem)
        call getManVal(manFile%proc, 'fshoot', crop%database%fshoot)
        call getManVal(manFile%proc, 'leaf2stor', crop%database%fleaf2stor)
        call getManVal(manFile%proc, 'stem2stor', crop%database%fstem2stor)
        call getManVal(manFile%proc, 'stor2stor', crop%database%fstor2stor)
        call getManVal(manFile%proc, 'rbc',crop%database%rbc)
        call getManVal(manFile%proc, 'standdk', crop%database%dkrate(1))
        call getManVal(manFile%proc, 'surfdk', crop%database%dkrate(2))
        call getManVal(manFile%proc, 'burieddk', crop%database%dkrate(3))
        call getManVal(manFile%proc, 'rootdk', crop%database%dkrate(4))
        call getManVal(manFile%proc, 'stemnodk', crop%database%dkrate(5))
        call getManVal(manFile%proc, 'stemdia', crop%database%xstm)
        call getManVal(manFile%proc, 'thrddys', crop%database%ddsthrsh)
        call getManVal(manFile%proc, 'covfact', crop%database%covfact)
        call getManVal(manFile%proc, 'resevapa', crop%database%resevapa)
        call getManVal(manFile%proc, 'resevapb', crop%database%resevapb)
        call getManVal(manFile%proc, 'yield_coefficient', crop%database%yld_coef)
        call getManVal(manFile%proc, 'residue_intercept', crop%database%resid_int)
        call getManVal(manFile%proc, 'regrow_location', crop%database%zloc_regrow)
        call getManVal(manFile%proc, 'noparam3', noparam3)
        call getManVal(manFile%proc, 'noparam2', noparam2)
        call getManVal(manFile%proc, 'noparam1', noparam1)

        ! reading of process parameters complete

        ! input is residue yield ratio. internal use is total biomass yield ratio
        ! all input values are on a dry weight basis.
        ! crop%database%yld_coef = crop%database%yld_coef + 1.0

        ! adjust yield coefficient to generate values on dry weight basis
        ! from total above ground biomass increments
        crop%database%yld_coef = (crop%database%yld_coef + 1.0 - crop%database%ywct/100.0) / (1.0-crop%database%ywct/100.0)

        ! check crop type to see if yield coefficient and grain fraction are used
        if( cook_yield .eq. 1 ) then
            if(     (crop%geometry%hyfg .eq. 0) &
               .or. (crop%geometry%hyfg .eq. 1) &
               .or. (crop%geometry%hyfg .eq. 5) ) then
            ! grain fraction is used
                if(       (crop%database%yld_coef .gt. 1.0 ) &
                    .and. (crop%database%yld_coef * crop%database%grf .lt. 1.0) ) then
                    ! these values will physically require the transfer of
                    ! biomass from stem or leaf pools to meet the incremental
                    ! need for reproductive mass to meet the residue yield ratio.
                    ! If acresid_int is not greateer than zero, this will
                    ! not be possible
                    write(*,*) 'Error: crop named (', trim(cropname), &
               ') has bad grain fraction and residue yield ratio values'
                    write(*,*) 'Error: grf*(ryrat+1-mc)/(1-mc) must be > 1',&
                               ', Value is: ',crop%database%yld_coef*crop%database%grf
                    stop
                end if
            end if
        end if

        ! set planting date vars (day, month, rotation year)
        crop%database%plant_day = lastoper(sr)%day
        crop%database%plant_month = lastoper(sr)%mon
        crop%database%plant_rotyr = lastoper(sr)%yr

        ! initialize flag to prevent multiple calibration harvests for single crop
        manFile%harv_calib_not_selected = .true.

        ! initialize transpiration depth parameters
        ahzfurcut(sr) = 0.0
        ahztransprtmin(sr) = 0.0
        ahztransprtmax(sr) = 0.0
        ! set row spacing based on flag
        select case( crop%geometry%rsfg )
        case(0) ! Broadcast Planting
            crop%geometry%xrow = 0.0
        case(1) ! Use Implement Ridge Spacing
           if(imprs.gt.0.001) then
             crop%geometry%xrow = imprs * mmtom
             ! check for implement seed placement and ridging
             if( (crop%geometry%rg .eq. 0) .and. (rdgflag .eq. 1) ) then
               ! seed placed in furrow bottom and ridge made unconditionally
               ! set transpiration depth parameters (meters)
               ahzfurcut(sr) = mmtom * furrowcut(soil%aszrgh,soil%asxrgw,soil%asxrgs)
               ahztransprtmin(sr) = ahzfurcut(sr) + crop%database%growdepth
               ahztransprtmax(sr) = crop%database%zmrt
             end if
           else  ! no ridges, so this is a broadcast crop
              crop%geometry%xrow = 0.0
           endif
        case(2) ! Use Specified Row Spacing
           ! convert incoming mm to meters used in acxrow
           crop%geometry%xrow = crop%geometry%xrow*mmtom
        case default
           write(*,*) 'Invalid row spacing flag value'
        end select

        ! do process
        ! do not initialize crop if no crop is present
        if( (crop%geometry%dpop .gt. 0.0) .and. (crop%database%idc .gt. 0) ) then
          ! set flag for crop initialization - jt
          crop%growth%am0cif = .true.
          ! set crop growth flag on - jt
          crop%growth%am0cgf = .true.
          ! give crop the proper name
          crop%bname = cropname
        endif
        ! post-process stuff
        if (manFile%am0tdb .eq. 1) then
          write (luotdb(sr),*) '//After planting process//'
          call tdbug(sr, prcode, soil, crop, residue)
        end if
        call set_calib(sr, crop)
        if( manFile%rpt_season_flg ) then
          ! not reported by the kill process in this
          call report_hydrobal( sr, manFile%mcount, manFile%mperod )
        end if

      case (61)  ! biomass remove process
        ! pre-process stuff
        if (manFile%am0tdb .eq. 1) then
          write (luotdb(sr),*)
          write (luotdb(sr),*) '//Before biomass remove process//'
          call tdbug(sr, prcode, soil, crop, residue)
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
          crop%mass%standstem, crop%mass%standleaf, crop%mass%standstore, &
          crop%mass%flatstem, crop%mass%flatleaf, crop%mass%flatstore, &
          crop%mass%rootstorez, crop%mass%rootfiberz, &
          crop%mass%stemz, &
          crop%geometry%zht, crop%geometry%dstm, crop%geometry%grainf, crop%geometry%hyfg, &
          cropres%standstem, cropres%standleaf, cropres%standstore, &
          cropres%flatstem, cropres%flatleaf, cropres%flatstore, &
          cropres%flatrootstore, cropres%flatrootfiber, &
          cropres%bgstemz, cropres%bgleafz, cropres%bgstorez, &
          cropres%bgrootstorez, cropres%bgrootfiberz, &
          cropres%zht, cropres%dstm, cropres%grainf, residue, &
          soil%nslay, mass_rem, mass_left)

        ! post-process stuff
        if (manFile%am0tdb .eq. 1) then
          write (luotdb(sr),*) '//After biomass remove process//'
          call tdbug(sr, prcode, soil, crop, residue)
        end if
        ! crop pool state has been changed, force dependent variable update  
        am0cropupfl = 1
        mature_warn_flg = 1
        ! no harvest report if nothing removed or no crop present
        if( (storef + leaff + stemf + rootstoref + rootfiberf .gt. 0.0) &
            .and. ((crop_present.gt.0) .or. (temp_present.gt.0)) ) then
          if( manFile%harv_calib_not_selected ) then
            call get_calib_crops(sr, crop)
            call get_calib_yield(sr, manFile%mcount, mass_rem, mass_left, crop)
            call report_calib_harvest( sr, manFile%mcount, mass_rem, mass_left, crop )
            manFile%harv_calib_not_selected = .false.
          end if
            call report_harvest( sr, manFile%mcount, mass_rem, mass_left, 0,&
                 1, mandate, crop)
          if( manFile%rpt_season_flg ) then
              ! not reported by the kill process in this
              call report_hydrobal( sr, manFile%mcount, manFile%mperod )
              call crop_endseason( sr, manFile%mcount, manFile%mperod, am0cfl(sr), &
                                   soil%nslay, mature_warn_flg, crop, cropprev )
              ! set to stop additional report in this operation
              manFile%rpt_season_flg = .false.
          end if
        end if

      case (62)  ! biomass remove pool process
        ! pre-process stuff
        if (manFile%am0tdb .eq. 1) then
          write (luotdb(sr),*)
          write (luotdb(sr),*) '//Before biomass remove pool process//'
          call tdbug(sr, prcode, soil, crop, residue)
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
          crop%mass%standstem, crop%mass%standleaf, crop%mass%standstore, &
          crop%mass%flatstem, crop%mass%flatleaf, crop%mass%flatstore, &
          crop%mass%rootstorez, crop%mass%rootfiberz, &
          crop%mass%stemz, &
          crop%geometry%zht, crop%geometry%dstm, crop%geometry%grainf, crop%geometry%hyfg, &
          cropres%standstem, cropres%standleaf, cropres%standstore, &
          cropres%flatstem, cropres%flatleaf, cropres%flatstore, &
          cropres%flatrootstore, cropres%flatrootfiber, &
          cropres%bgstemz, cropres%bgleafz, cropres%bgstorez, &
          cropres%bgrootstorez, cropres%bgrootfiberz, &
          cropres%zht, cropres%dstm, cropres%grainf, residue, &
          soil%nslay, mass_rem, mass_left)

        ! post-process stuff
        if (manFile%am0tdb .eq. 1) then
          write (luotdb(sr),*) '//After biomass remove pool process//'
          call tdbug(sr, prcode, soil, crop, residue)
        end if
        ! crop pool state has been changed, force dependent variable update  
        am0cropupfl = 1
        ! no harvest report if nothing removed
        if( (storef + leaff + stemf + rootstoref + rootfiberf .gt. 0.0) &
            .and. ((crop_present.gt.0) .or. (temp_present.gt.0)) ) then
          ! removed mass is used in calibration
          if(      (harv_calib_flg .gt. 0) &
             .and. (manFile%harv_calib_not_selected) ) then
            call get_calib_crops(sr, crop)
            call get_calib_yield(sr, manFile%mcount, mass_rem, mass_left, crop)
            call report_calib_harvest( sr, manFile%mcount, mass_rem, mass_left, crop )
            manFile%harv_calib_not_selected = .false.
          end if
          ! removed mass appears in crop report
          call report_harvest( sr, manFile%mcount, mass_rem, mass_left, &
                               harv_unit_flg, harv_report_flg, &
                               mandate, crop )
          if( manFile%rpt_season_flg ) then
            ! not reported by the kill process in this
            call report_hydrobal( sr, manFile%mcount, manFile%mperod )
            call crop_endseason( sr, manFile%mcount, manFile%mperod, am0cfl(sr), &
                                 soil%nslay, mature_warn_flg, crop, cropprev )
              ! set to stop additional report in this operation
            manFile%rpt_season_flg = .false.
          end if
        end if

      case (65)  ! add residue process
        ! New residue is assigned to residue pool 1.
        ! Existing residue is transfered to other pools.
        ! ADD RESIDUE was modeled after residue initialization (process 50)

        ! this is modified to avoid polluting the parameters of an
        ! existing crop, which could happen if residue is added while a
        ! crop is growing.

        ! pre-process stuff
        if (manFile%am0tdb .eq. 1) then
          write (luotdb(sr),*)
          write (luotdb(sr),*) '//Before add residue process//'
          call tdbug(sr, prcode, soil, crop, residue)
        end if

        call getManVal(manFile%proc, 'numst', cropres%dstm)
        call getManVal(manFile%proc, 'rstandht', cropres%zht)
        call getManVal(manFile%proc, 'rstandmass', cropres%standstem)
        call getManVal(manFile%proc, 'rflatmass', cropres%flatstem)
        call getManVal(manFile%proc, 'rbc', trbc)
        ! read buried residue amounts
        call getManVal(manFile%proc, 'rburiedmass', dmassres)
        call getManVal(manFile%proc, 'rburieddepth', zmassres)
        call getManVal(manFile%proc, 'rrootmass', dmassrot)
        call getManVal(manFile%proc, 'rrootdepth', zmassrot)

        ! place buried residue in pools by layer
        call resinit(dmassrot, zmassrot, soil%nslay, &
                     cropres%bgrootfiberz, soil%aszlyt)
        call resinit(dmassres,zmassres,soil%nslay, &
                     cropres%bgstemz, soil%aszlyt)
        ! read decomposition parameters
        call getManVal(manFile%proc, 'standdk', tdkrate(1))
        call getManVal(manFile%proc, 'surfdk', tdkrate(2))
        call getManVal(manFile%proc, 'burieddk', tdkrate(3))
        call getManVal(manFile%proc, 'rootdk', tdkrate(4))
        call getManVal(manFile%proc, 'stemnodk', tdkrate(5))
        call getManVal(manFile%proc, 'stemdia', txstm)
        call getManVal(manFile%proc, 'thrddys', tddsthrsh)
        call getManVal(manFile%proc, 'covfact', tcovfact)
        ! read parameters for residue suppression of evaporation
        call getManVal(manFile%proc, 'resevapa', tresevapa)
        call getManVal(manFile%proc, 'resevapb', tresevapb)

        !Set to 0
        !above ground
        cropres%standleaf = 0.0
        cropres%standstore = 0.0
        cropres%flatleaf = 0.0
        cropres%flatstore = 0.0
        cropres%flatrootstore = 0.0
        cropres%flatrootfiber = 0.0
        !below ground by layer
        dmassres = 0.0
        zmassres = 0.0
        dmassrot = 0.0
        zmassrot = 0.0
        call resinit(dmassres, zmassres, soil%nslay, &
                     cropres%bgleafz, soil%aszlyt)
        call resinit(dmassres, zmassres, soil%nslay, &
                     cropres%bgstorez, soil%aszlyt)
        call resinit(dmassrot, zmassrot, soil%nslay, &
                     cropres%bgrootstorez, soil%aszlyt)

        cropres%grainf = 1.0
        cropres%xstmrep = txstm
        thyfg = 0

        !I don't think it matters what values we put here.
        !We set leaf mass to 0 anyway.
        t0sla = 0.0
        t0ck = 0.0

        ! check for amount of added biomass
        if( poolmass( soil%nslay, &
                 cropres%standstem, cropres%standleaf, cropres%standstore, &
                 cropres%flatstem, cropres%flatleaf, cropres%flatstore, &
                 cropres%flatrootstore, cropres%flatrootfiber, &
                 cropres%bgstemz, cropres%bgleafz, cropres%bgstorez, &
                 cropres%bgrootstorez, cropres%bgrootfiberz ) &
          .gt. 0.0 ) then
          ! biomass was added, so do transfer
          call trans( &
            cropres%standstem, cropres%standleaf, cropres%standstore, &
            cropres%flatstem, cropres%flatleaf, cropres%flatstore, &
            cropres%flatrootstore, cropres%flatrootfiber, &
            cropres%bgstemz, cropres%bgleafz, cropres%bgstorez, &
            cropres%bgrootstorez, cropres%bgrootfiberz, &
            cropres%zht, cropres%dstm,cropres%xstmrep,cropres%grainf, &
            cropname, txstm, trbc, t0sla, t0ck, &
            tdkrate(1), tcovfact, tddsthrsh, thyfg, &
            tresevapa, tresevapb, &
            soil%nslay, residue )
        end if

        ! post-process stuff
        if (manFile%am0tdb .eq. 1) then
          write (luotdb(sr),*) '//After add residue process//'
          call tdbug(sr, prcode, soil, crop, residue)
        end if

      case (66)  ! add manure process
        ! New residue (manure) is assigned to residue pool 1.
        ! Existing residue is transfered to other pools.
        ! ADD MANURE was modeled after ADD RESIDUE (process 65)
        ! The only difference between process ADD MANURE and
        ! ADD RESIDUE is that NRCS wanted to be able to specify
        ! the "total" mass of manure applied and the fraction
        ! that is buried of that total.  So, ADD MANURE is a
        ! special case of ADD RESIDUE (just uses two additional
        ! input parameters)

        ! this is modified to avoid polluting the parameters of an
        ! existing crop, which could happen if residue is added while a
        ! crop is growing.

        ! pre-process stuff
        if (manFile%am0tdb .eq. 1) then
          write (luotdb(sr),*)
          write (luotdb(sr),*) '//Before add manure process//'
          call tdbug(sr, prcode, soil, crop, residue)
        end if

        call getManVal(manFile%proc, 'M_numst', cropres%dstm)
        call getManVal(manFile%proc, 'M_rstandht', cropres%zht)
        call getManVal(manFile%proc, 'M_rstandmass', cropres%standstem)
        call getManVal(manFile%proc, 'M_rflatmass', cropres%flatstem)
        call getManVal(manFile%proc, 'rbc', trbc)
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
        cropres%flatstem = cropres%flatstem +                             &      
                (1.0 - manure_buried_fraction) * manure_total_mass
        dmassres = dmassres + &
                (manure_buried_fraction) * manure_total_mass

        ! place buried residue in pools by layer
        call resinit(dmassrot, zmassrot, soil%nslay, &
                     cropres%bgrootfiberz, soil%aszlyt)
        call resinit(dmassres,zmassres,soil%nslay, &
                     cropres%bgstemz, soil%aszlyt)

        ! read decomposition parameters
        call getManVal(manFile%proc, 'standdk', tdkrate(1))
        call getManVal(manFile%proc, 'surfdk', tdkrate(2))
        call getManVal(manFile%proc, 'burieddk', tdkrate(3))
        call getManVal(manFile%proc, 'rootdk', tdkrate(4))
        call getManVal(manFile%proc, 'stemnodk', tdkrate(5))
        call getManVal(manFile%proc, 'stemdia', txstm)
        call getManVal(manFile%proc, 'thrddys', tddsthrsh)
        call getManVal(manFile%proc, 'covfact', tcovfact)
        ! read parameters for residue suppression of evaporation
        call getManVal(manFile%proc, 'resevapa', tresevapa)
        call getManVal(manFile%proc, 'resevapb', tresevapb)

        !Set to 0
        !above ground
        cropres%standleaf = 0.0
        cropres%standstore = 0.0
        cropres%flatleaf = 0.0
        cropres%flatstore = 0.0
        cropres%flatrootstore = 0.0
        cropres%flatrootfiber = 0.0
        !below ground by layer
        dmassres = 0.0
        zmassres = 0.0
        dmassrot = 0.0
        zmassrot = 0.0
        call resinit(dmassres, zmassres, soil%nslay, &
                     cropres%bgleafz, soil%aszlyt)
        call resinit(dmassres, zmassres, soil%nslay, &
                     cropres%bgstorez, soil%aszlyt)
        call resinit(dmassrot, zmassrot, soil%nslay, &
                     cropres%bgrootstorez, soil%aszlyt)

        cropres%grainf = 1.0
        cropres%xstmrep = txstm
        thyfg = 0

        !I don't think it matters what values we put here.
        !We set leaf mass to 0 anyway.
        t0sla = 0.0
        t0ck = 0.0

        if( poolmass( soil%nslay, &
                 cropres%standstem, cropres%standleaf, cropres%standstore, &
                 cropres%flatstem, cropres%flatleaf, cropres%flatstore, &
                 cropres%flatrootstore, cropres%flatrootfiber, &
                 cropres%bgstemz, cropres%bgleafz, cropres%bgstorez, &
                 cropres%bgrootstorez, cropres%bgrootfiberz ) &
          .gt. 0.0 ) then
          ! biomass was added, so do transfer
          call trans( &
            cropres%standstem, cropres%standleaf, cropres%standstore, &
            cropres%flatstem, cropres%flatleaf, cropres%flatstore, &
            cropres%flatrootstore, cropres%flatrootfiber, &
            cropres%bgstemz, cropres%bgleafz, cropres%bgstorez, &
            cropres%bgrootstorez, cropres%bgrootfiberz, &
            cropres%zht, cropres%dstm,cropres%xstmrep,cropres%grainf, &
            cropname, txstm, trbc, t0sla, t0ck, &
            tdkrate(1), tcovfact, tddsthrsh, thyfg, &
            tresevapa, tresevapb, &
            soil%nslay, residue )
        end if
 
        ! post-process stuff
        if (manFile%am0tdb .eq. 1) then
          write (luotdb(sr),*) '//After add manure process//'
          call tdbug(sr, prcode, soil, crop, residue)
        end if

      case (71) ! irrigate process (OBSOLETE)
        ! pre-process stuff
        if (manFile%am0tdb .eq. 1) then
          write (luotdb(sr),*)
          write (luotdb(sr),*) '//Before irrigation process//'
          call tdbug(sr, prcode, soil, crop, residue)
        end if

        call getManVal(manFile%proc, 'irrtype', roughflg)
        call getManVal(manFile%proc, 'irrdepth', irrig)

        ! do process
        ! replaced am0irr (1 - sprinkler, 2 furrow) with ahlocirr
        ! using roughflg to read in old value and set some default values
        if( roughflg .eq. 1 ) then
            ahlocirr(sr) = 2000.0
        else
            ahlocirr(sr) = 0.0
        end if
        h1et%zirr = h1et%zirr + irrig
        ! make sure rate and duration are consistent
        ! these values are not set in this process but may have been set
        ! in process 72, if this is used in conjunction with it
        call ratedura(h1et%zirr, ahratirr(sr), ahdurirr(sr))
        ! post-process stuff
        if (manFile%am0tdb .eq. 1) then
          write (luotdb(sr),*) '//After irrigate process//'
          call tdbug(sr, prcode, soil, crop, residue)
        end if

      case (72)  ! irrigation monitoring process
        ! pre-process stuff
        if (manFile%am0tdb .eq. 1) then
          write (luotdb(sr),*)
          write (luotdb(sr),*) '//Before irrigation monitoring process//'
          call tdbug(sr, prcode, soil, crop, residue)
        end if

        call getManVal(manFile%proc, 'irrmonflag', am0monirr(sr))
        call getManVal(manFile%proc, 'irrmaxapp', ahzdmaxirr(sr))
        call getManVal(manFile%proc, 'irrrate', ahratirr(sr))
        call getManVal(manFile%proc, 'irrduration', ahdurirr(sr))
        call getManVal(manFile%proc, 'irrapploc', ahlocirr(sr))
        call getManVal(manFile%proc, 'irrminapp', ahminirr(sr))
        call getManVal(manFile%proc, 'irrmad', ahmadirr(sr))
        call getManVal(manFile%proc, 'irrminint', ahmintirr(sr))

        ! do process
        ! set next irrigation day to zero so irrigations will trigger
        ahndayirr(sr) = 0
        ! use inputs to set the irrigation rate, if 
        call ratedura(ahzdmaxirr(sr), ahratirr(sr), ahdurirr(sr))
        ! post-process stuff
        if (manFile%am0tdb .eq. 1) then
          write (luotdb(sr),*) '//After irrigation monitoring process//'
          call tdbug(sr, prcode, soil, crop, residue)
        end if

      case (73)  ! single event irrigation process
        ! pre-process stuff
        if (manFile%am0tdb .eq. 1) then
          write (luotdb(sr),*)
          write (luotdb(sr),*) '//Before single event irrigation process//'
          call tdbug(sr, prcode, soil, crop, residue)
        end if

        call getManVal(manFile%proc, 'irrdepth', irrig)
        call getManVal(manFile%proc, 'irrrate', ahratirr(sr))
        call getManVal(manFile%proc, 'irrduration', ahdurirr(sr))
        call getManVal(manFile%proc, 'irrapploc', ahlocirr(sr))

        ! do process
        ! add this irrigation event to any previous event on this same day
        h1et%zirr = h1et%zirr + irrig
        ! use inputs to set the irrigation rate, if 
        call ratedura(h1et%zirr, ahratirr(sr), ahdurirr(sr))
        if (manFile%am0tdb .eq. 1) then
          write (luotdb(sr),*) '//After single event irrigation process//'
          !call tdbug(sr, prcode, soil, crop, residue)
        end if
        ! post-process stuff
        if (manFile%am0tdb .eq. 1) then
          write (luotdb(sr),*) '//After single event irrigation process//'
          call tdbug(sr, prcode, soil, crop, residue)
        end if

      case (74)  ! terminate irrigation monitoring terminate process
        ! pre-process stuff
        if (manFile%am0tdb .eq. 1) then
          write (luotdb(sr),*)
          write (luotdb(sr),*) '//Before terminate irrigation monitoring//'
          call tdbug(sr, prcode, soil, crop, residue)
        end if

        ! do process
        am0monirr(sr) = 0
        ! post-process stuff
        if (manFile%am0tdb .eq. 1) then
          write (luotdb(sr),*) '//After terminate irrigation monitoring//'
          call tdbug(sr, prcode, soil, crop, residue)
        end if

      case (91)  ! initialize (set) soil layer asd
        ! pre-process stuff
        if (manFile%am0tdb .eq. 1) then
          write (luotdb(sr),*)
          write (luotdb(sr),*) '//Before initialize soil layer asd conditions//'
          call tdbug(sr, prcode, soil, crop, residue)
        end if

        write(0,*) 'prior to set_asd() call: ', 'msieve: ', msieve, 'nsieve: ', nsieve

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

        write (UNIT=0,FMT="(A)",ADVANCE="NO") '//After set_asd process// '
        write(0,*) 'no. of soil layers to modify/total and depth are: ', asdlayer, soil%nslay, asddepth
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
          call tdbug(sr, prcode, soil, crop, residue)
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
          call tdbug(sr, prcode, soil, crop, residue)
        end if

        write(0,*) 'prior to set_wc() call: ', ''
        write(0,*) ""

        call getManVal(manFile%proc, 'wcdepth', wcdepth)
        call getManVal(manFile%proc, 'wc', wc)

        ! New parameters for set_water content initialization process
        write(UNIT=0,FMT="(5(f10.4))") wcdepth, wc
        write(0,*)

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

        write (UNIT=0,FMT="(A)",ADVANCE="NO") '//After set_wc process// '
        write(0,*) 'no. of soil layers to modify/total and depth are: ', asdlayer, soil%nslay, wcdepth
        write(UNIT=0,FMT="(A3,2(A10))") 'lay', 'depth', 'wc'
        do i=1, wclayer
          write (UNIT=0,FMT="(i3,2(f10.4))",ADVANCE="YES") &
              i, soil%aszlyt(i), soil%ahrwc(i)
        end do
        write(0,*) "layers below asdlayer"
        do i=wclayer+1, soil%nslay
          write (UNIT=0,FMT="(i3,2(f10.4))",ADVANCE="YES") &
              i, soil%aszlyt(i), soil%ahrwc(i)
        end do

        ! post-process stuff
        if (manFile%am0tdb .eq. 1) then
          write (luotdb(sr),*) '//After initialize soil layer wc conditions//'
          call tdbug(sr, prcode, soil, crop, residue)
        end if

        if (BTEST(manFile%am0tfl,1)) then
          write(luowc(sr),"(3(i5))",ADVANCE='NO') cd, cm, cy
          write(luowc(sr),"(i10,2(f10.4),A)",ADVANCE="YES") 1, soil%aszlyt(1), &
             soil%ahrwc(1), &
            ' New values - After initialized soil layer water content conditions'
          ! write(luowc(sr),"(i10,4(i5))",ADVANCE="YES") get_simdate_jday(), cd, cm, cy, get_simdate_doy()
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

    subroutine manage( sr, startyr, soil, crop, cropprev, residue, biotot, mandate, h1et, manFile)

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

      use weps_interface_defs
      use datetime_mod, only: difdat, get_simdate
      use file_io_mod, only: luomanage
      use soil_data_struct_defs, only: soil_def
      use biomaterial, only: biomatter, biototal, bio_prevday
      use mandate_mod, only: opercrop_date
      use stir_report_mod, only: stir_report
      use hydro_data_struct_defs, only: hydro_derived_et
      use manage_data_struct_defs, only: man_file_struct, lastoper

!     + + + ARGUMENT DECLARATIONS + + +
      integer :: sr       ! the subregion number
      integer :: startyr  ! starting year of the simulation run
      type(soil_def), intent(inout) :: soil  ! soil for this subregion
      type(biomatter), intent(inout) :: crop    ! structure containing full crop description
      type(bio_prevday), intent(inout) :: cropprev    ! structure containing crop previous day values
      type(biomatter), dimension(:), intent(inout) :: residue
      type(biototal), intent(in) :: biotot
      type(opercrop_date), dimension(:), intent(inout) :: mandate
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
            call dogroup(soil, manFile)
            ! do processes
            manFile%proc => manFile%grp%procFirst
            do while ( associated(manFile%proc) )
              call doproc(soil, crop, cropprev, residue, biotot, mandate, h1et, manFile)
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

end module manage_mod

