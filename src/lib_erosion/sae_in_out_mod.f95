!$Author$
!$Date$
!$Revision$
!$HeadURL$

module sae_in_out_mod

  implicit none
!     This module is for the creation of single or multiple stand alone erosion input files,
!     depending on the command line switches given

  type make_sae_in_out
     integer :: jday      ! the present day in julian days for output
     integer :: simday    ! the present day in simulation days for creation of file
     integer :: maxday    ! maximum simday number
     character*256 :: fullpath  ! the root path plus subdirectory if indicated by multiple files
  end type make_sae_in_out

  type(make_sae_in_out) :: mksaeinp
  type(make_sae_in_out) :: mksaeout

  type subregion_files
    integer :: isub      ! subregion index
    character(len=512) :: treatfil  ! treatment file name
    character(len=512) :: slstfil  ! soil surface state file name
  end type subregion_files

  character(len=512) :: infilebase  ! base name (without .in extension) of old format input file
  character(len=512) :: sweepfile  ! name of the sweep input file
  logical :: saeinp_forceday = .false. ! true when current day was requested by -O/-o
  type(subregion_files), dimension(:), allocatable :: subrfiles

  ! placed here for sharing back with hagen_plot_flag by daily_erodout
  real :: aegt, aegtcs, aegtss, aegt10, aegt2_5

  contains

      subroutine saeinp( julday, nsubr )

!     +++ PURPOSE +++
!     print out input file for stand alone erosion

!     + + + Modules Used + + +
      use datetime_mod, only: caldat
      use file_io_mod, only: fopenk, makenamnum, makedir
      use grid_mod, only: amxsim, amasim, gridfile, write_grid
      use subregions_mod
      use barriers_mod, only: barrier
      use erosion_data_struct_defs, only: awzypt, awdair, anemht, awzzo, wzoflg, &
                                          awadir, subday, ntstep, cellsurfacestate, subrsurf
      use sweep_io_xml_defs
      use read_write_xml_mod, only: w_begin_tag, w_end_tag, w_whole_tag

!     +++ ARGUMENT DECLARATIONS +++
      integer, intent(in) :: julday ! current julian day (index into subrsurf array)
      integer, intent(in) :: nsubr   ! number of subregions
      
!     +++ LOCAL VARIABLES +++
      integer k,l, sr, ip
      integer b, nbr
      integer day, mon, yr
      integer :: alloc_stat, dealloc_stat
      integer :: ipool   ! index of biomass pool tag being written
      character*512 :: daypath  ! the root path plus subdirectory plus specific day directory for sweep input files
      character*512 :: filename ! filename with any preceding path removed
      integer :: luo_saeinp     ! output unit number
      character*30, dimension(:), allocatable:: subr_text ! subregion output directory text string

!     + + + LOCAL VARIABLE DEFINITIONS + + +
!     sr - index used in subregion loop
!     ip - index to polygon coordinates

!     +++ END SPECIFICATIONS +++

      ! path to sweep input files
      if( mksaeinp%simday .gt. 0 ) then
        ! running in WEPS
        ! directory for files for this day
        daypath = trim(mksaeinp%fullpath) // makenamnum('saeros', mksaeinp%simday, mksaeinp%maxday,'/')
      else
        ! running in SWEEP
        ! path for files based on old format sweep input file
        daypath = trim(mksaeinp%fullpath) // trim(infilebase) // '/'
      end if
      call makedir(daypath)

      allocate( subr_text(nsubr) )

      ! write main sweep file
      call fopenk (luo_saeinp, (trim(daypath) // trim(sweepfile)), 'unknown')

      allocate(subrfiles(nsubr), stat=alloc_stat)
      if( alloc_stat .gt. 0 ) then
        ! allocation failed
        write(*,*) "ERROR: unable to allocate memory for subrfiles array"
      end if

      do sr = 1, nsubr
         ! create numbered subregion name
         subr_text(sr) = makenamnum( 'subregion', sr, nsubr, '/' )
         ! create directory with that name
         call makedir( trim(daypath)//trim(subr_text(sr)) )
         ! must trim tinfil to just the file name, not path prefix, when not running in run directory
         filename = trim(subrsurf(julday,sr)%tinfil)
         if( index(filename,'\') .gt. 0 ) then
            filename = trim(filename((index(filename,'\',back=.true.)+1):))
         else if( index(filename,'/') .gt. 0 ) then
            filename = trim(filename((index(filename,"/",back=.true.)+1):))
         end if
         subrfiles(sr)%treatfil = trim(filename) // '.treat'
         filename = trim(subrsurf(julday,sr)%sinfil)
         if( index(filename,'\') .gt. 0 ) then
            filename = trim(filename((index(filename,'\',back=.true.)+1):))
         else if( index(filename,'/') .gt. 0 ) then
            filename = trim(filename((index(filename,"/",back=.true.)+1):))
         end if
         subrfiles(sr)%slstfil = trim(filename) // '.slst'
      end do

      call caldat (mksaeinp%jday,day,mon,yr)
      write(*,'(4(a,i0))') 'Made SWEEP input files D/M/Y: ', day,'/', mon,'/', yr,' simulation day: ', mksaeinp%simday

      ! write XML header
      write(luo_saeinp,"(a)") '<?xml version="1.0" encoding="ISO-8859-1"?>'
      write(luo_saeinp,"(a)") '<!DOCTYPE sweepData SYSTEM "sweep.dtd">'

      call init_input_xml()  ! creates input_tag array

      call w_begin_tag( luo_saeinp, input_tag(sweepData)%name )
      call w_begin_tag( luo_saeinp, 'WEPS_date' )
      call w_whole_tag( luo_saeinp, 'WEPS_day', day )
      call w_whole_tag( luo_saeinp, 'WEPS_month', mon )
      call w_whole_tag( luo_saeinp, 'WEPS_year', yr )
      call w_whole_tag( luo_saeinp, 'WEPS_SimulationDay', mksaeinp%simday )
      call w_end_tag( luo_saeinp, 'WEPS_date' )
      call w_whole_tag( luo_saeinp, input_tag(SCI_XOrigin)%name, amxsim(1)%x )
      call w_whole_tag( luo_saeinp, input_tag(SCI_YOrigin)%name, amxsim(1)%y )
      call w_whole_tag( luo_saeinp, input_tag(SCI_XLength)%name, amxsim(2)%x )
      call w_whole_tag( luo_saeinp, input_tag(SCI_YLength)%name, amxsim(2)%y )
      call w_whole_tag( luo_saeinp, input_tag(SCI_RegionAngle)%name, amasim )
      call w_whole_tag( luo_saeinp, input_tag(SCI_GridFile)%name, trim(gridfile) )

      ! subregions
      call w_begin_tag( luo_saeinp, input_tag(SCI_Subregions)%name, &
                                      input_tag(SCI_number)%name, nsubr)
      ! loop through all subregions
      do sr = 1, nsubr                ! NOTE: index adjusted to zero based
         call w_begin_tag( luo_saeinp, input_tag(SCI_Subregion)%name, &
                                       input_tag(SCI_index)%name, sr-1)
         call w_whole_tag( luo_saeinp, input_tag(SCI_treat)%name, trim(subr_text(sr))//subrfiles(sr)%treatfil )
         call w_whole_tag( luo_saeinp, input_tag(SCI_soilsurf)%name, trim(subr_text(sr))//subrfiles(sr)%slstfil )
         call w_whole_tag( luo_saeinp, input_tag(GUI_soilifc)%name, '../../' // subrsurf(julday,sr)%sinfil )
         call w_end_tag( luo_saeinp, input_tag(SCI_Subregion)%name )
      end do
      call w_end_tag( luo_saeinp, input_tag(SCI_Subregions)%name )

      ! barriers
      nbr = size(barrier)
      if ( nbr .gt. 0 ) then
         call w_begin_tag( luo_saeinp, input_tag(SCI_Barriers)%name, input_tag(SCI_number)%name, nbr)
         ! loop through individual barriers
         do b = 1, nbr                   ! NOTE: index adjusted to zero based
            call w_begin_tag( luo_saeinp, input_tag(SCI_Barrier)%name, &
                                          input_tag(SCI_index)%name, b-1, &
                                          input_tag(SCI_number)%name, barrier(b)%np )
            do ip = 1, barrier(b)%np        ! NOTE: index adjusted to zero based
               call w_begin_tag( luo_saeinp, input_tag(SCI_BarPoint)%name, &
                                             input_tag(SCI_index)%name, ip-1 )
                call w_whole_tag( luo_saeinp, input_tag(SCI_x)%name, barrier(b)%points(ip)%x )
                call w_whole_tag( luo_saeinp, input_tag(SCI_y)%name, barrier(b)%points(ip)%y )
                call w_whole_tag( luo_saeinp, input_tag(SCI_height)%name, barrier(b)%param(ip)%amzbr )
                call w_whole_tag( luo_saeinp, input_tag(SCI_width)%name, barrier(b)%param(ip)%amxbrw )
                call w_whole_tag( luo_saeinp, input_tag(SCI_porosity)%name, barrier(b)%param(ip)%ampbr )
                call w_end_tag( luo_saeinp, input_tag(SCI_BarPoint)%name )
            end do
            call w_end_tag( luo_saeinp, input_tag(SCI_Barrier)%name )
         end do
         call w_end_tag( luo_saeinp, input_tag(SCI_Barriers)%name )
      end if

      ! weather
      call w_whole_tag( luo_saeinp, input_tag(SCI_AirDensity)%name, awdair )
      call w_whole_tag( luo_saeinp, input_tag(SCI_WindDirection)%name, awadir )
      call w_whole_tag( luo_saeinp, input_tag(SCI_AnemometerHeight)%name, anemht )
      call w_whole_tag( luo_saeinp, input_tag(SCI_AerodynamicRoughness)%name, awzzo )
      call w_whole_tag( luo_saeinp, input_tag(SCI_AnemometerFlag)%name, wzoflg )
      call w_whole_tag( luo_saeinp, input_tag(SCI_AverageAnnualPrecipitation)%name, awzypt )

      ! wind
      call w_begin_tag( luo_saeinp, input_tag(SCI_WindSpeeds)%name, &
                                    input_tag(SCI_number)%name, ntstep)
      do k = 1, ntstep                ! NOTE: index adjusted to zero based
         call w_whole_tag( luo_saeinp, input_tag(SCI_WindSpeed)%name, &
                                       input_tag(SCI_index)%name, k-1, subday(k)%awu )
      end do
      call w_end_tag( luo_saeinp, input_tag(SCI_WindSpeeds)%name )
      call w_end_tag( luo_saeinp, input_tag(sweepData)%name )

      close(luo_saeinp)

      if( mksaeinp%simday .eq. 0 ) then
         ! running in SWEEP
         ! write single subregion grid array file
         call write_grid( trim(daypath) )
      end if

      ! write treatment and soil state files for subregions
      do sr = 1, nsubr                ! NOTE: index adjusted to zero based

         ! write treatment file
         call fopenk (luo_saeinp, (trim(daypath) // trim(subr_text(sr)) // subrfiles(sr)%treatfil), 'unknown')

         ! write XML header
         write(luo_saeinp,"(a)") '<?xml version="1.0" encoding="ISO-8859-1"?>'
         write(luo_saeinp,"(a)") '<!DOCTYPE TreatmentData SYSTEM "treatment.dtd">'

         call w_begin_tag( luo_saeinp, input_tag(TreatmentData)%name )
         ! brcd Inputs (biomass by pool)
         call w_begin_tag( luo_saeinp, input_tag(SCI_BrcdInputs)%name, &
                                        input_tag(SCI_number)%name, subrsurf(julday,sr)%npools)
         do ipool = 1, subrsurf(julday,sr)%npools
            call w_begin_tag( luo_saeinp, input_tag(SCI_brcdInput)%name, &
                                          input_tag(SCI_index)%name, ipool-1)
            call w_whole_tag( luo_saeinp, input_tag(SCI_brcdBname)%name, subrsurf(julday,sr)%brcdInput(ipool)%bname )
            call w_whole_tag( luo_saeinp, input_tag(SCI_brcdRlai)%name, subrsurf(julday,sr)%brcdInput(ipool)%rlai )
            call w_whole_tag( luo_saeinp, input_tag(SCI_brcdRsai)%name, subrsurf(julday,sr)%brcdInput(ipool)%rsai )
            call w_whole_tag( luo_saeinp, input_tag(SCI_brcdRg)%name, subrsurf(julday,sr)%brcdInput(ipool)%rg )
            call w_whole_tag( luo_saeinp, input_tag(SCI_brcdXrow)%name, subrsurf(julday,sr)%brcdInput(ipool)%xrow )
            call w_whole_tag( luo_saeinp, input_tag(SCI_brcdZht)%name, subrsurf(julday,sr)%brcdInput(ipool)%zht )
            call w_end_tag( luo_saeinp, input_tag(SCI_brcdInput)%name )
         end do
         call w_end_tag( luo_saeinp, input_tag(SCI_BrcdInputs)%name )
         call w_whole_tag( luo_saeinp, input_tag(SCI_BiomassFlatCover)%name, subrsurf(julday,sr)%abffcv )
         call w_whole_tag( luo_saeinp, input_tag(SCI_CrustThick)%name, subrsurf(julday,sr)%aszcr )
         call w_whole_tag( luo_saeinp, input_tag(SCI_CrustDensity)%name, subrsurf(julday,sr)%asdcr )
         call w_whole_tag( luo_saeinp, input_tag(SCI_CrustStability)%name, subrsurf(julday,sr)%asecr )
         call w_whole_tag( luo_saeinp, input_tag(SCI_CrustCover)%name, subrsurf(julday,sr)%asfcr )
         call w_whole_tag( luo_saeinp, input_tag(SCI_CrustMassCoverLoose)%name, subrsurf(julday,sr)%asmlos )
         call w_whole_tag( luo_saeinp, input_tag(SCI_CrustFracCoverLoose)%name, subrsurf(julday,sr)%asflos )
         call w_whole_tag( luo_saeinp, input_tag(SCI_RandomRoughness)%name, subrsurf(julday,sr)%aslrr )
         call w_whole_tag( luo_saeinp, input_tag(SCI_RidgeOrientation)%name, subrsurf(julday,sr)%asargo )
         call w_whole_tag( luo_saeinp, input_tag(SCI_RidgeHeight)%name, subrsurf(julday,sr)%aszrgh )
         call w_whole_tag( luo_saeinp, input_tag(SCI_RidgeSpacing)%name, subrsurf(julday,sr)%asxrgs )
         call w_whole_tag( luo_saeinp, input_tag(SCI_RidgeWidth)%name, subrsurf(julday,sr)%asxrgw )
         call w_whole_tag( luo_saeinp, input_tag(SCI_DikeSpacing)%name, subrsurf(julday,sr)%asxdks )
         call w_whole_tag( luo_saeinp, input_tag(SCI_SnowDepth)%name, subrsurf(julday,sr)%ahzsnd )
         call w_begin_tag( luo_saeinp, input_tag(SCI_SurfaceSubDayWaters)%name, &
                                        input_tag(SCI_number)%name, subrsurf(julday,sr)%nswet)
         do l = 1, subrsurf(julday,sr)%nswet    ! NOTE: index adjusted to zero based
            call w_whole_tag( luo_saeinp, input_tag(SCI_SurfaceSubDayWater)%name, &
                                          input_tag(SCI_index)%name, l-1, &
                                          subrsurf(julday,sr)%ahrwc0(l) )
         end do
         call w_end_tag( luo_saeinp, input_tag(SCI_SurfaceSubDayWaters)%name )
         call w_end_tag( luo_saeinp, input_tag(TreatmentData)%name )

         close(luo_saeinp)

         ! write soil state file
         call fopenk (luo_saeinp, (trim(daypath) // trim(subr_text(sr)) // subrfiles(sr)%slstfil), 'unknown')

         ! write XML header
         write(luo_saeinp,"(a)") '<?xml version="1.0" encoding="ISO-8859-1"?>'
         write(luo_saeinp,"(a)") '<!DOCTYPE SoilState SYSTEM "soilState.dtd">'

         call w_begin_tag( luo_saeinp, input_tag(SoilState)%name )
         ! soil
         call w_begin_tag( luo_saeinp, input_tag(SCI_SoilLays)%name, &
                                        input_tag(SCI_number)%name, subrsurf(julday,sr)%nslay)
         ! the coordinate pairs
         do l = 1, subrsurf(julday,sr)%nslay    ! NOTE: index adjusted to zero based
            call w_begin_tag( luo_saeinp, input_tag(SCI_SoilLay)%name, &
                                          input_tag(SCI_index)%name, l-1)
            call w_whole_tag( luo_saeinp, input_tag(SCI_LayerThickness)%name, subrsurf(julday,sr)%bsl(l)%aszlyt )
            call w_whole_tag( luo_saeinp, input_tag(SCI_BulkDensity)%name, subrsurf(julday,sr)%bsl(l)%asdblk )
            call w_whole_tag( luo_saeinp, input_tag(SCI_Sand)%name, subrsurf(julday,sr)%bsl(l)%asfsan )
            call w_whole_tag( luo_saeinp, input_tag(SCI_VeryFineSand)%name, subrsurf(julday,sr)%bsl(l)%asfvfs )
            call w_whole_tag( luo_saeinp, input_tag(SCI_Silt)%name, subrsurf(julday,sr)%bsl(l)%asfsil )
            call w_whole_tag( luo_saeinp, input_tag(SCI_Clay)%name, subrsurf(julday,sr)%bsl(l)%asfcla )
            call w_whole_tag( luo_saeinp, input_tag(SCI_RockVolume)%name, subrsurf(julday,sr)%bsl(l)%asvroc )
            call w_whole_tag( luo_saeinp, input_tag(SCI_AggregateDensity)%name, subrsurf(julday,sr)%bsl(l)%asdagd )
            call w_whole_tag( luo_saeinp, input_tag(SCI_AggregateStability)%name, subrsurf(julday,sr)%bsl(l)%aseags )
            call w_whole_tag( luo_saeinp, input_tag(SCI_AggregateGMD)%name, subrsurf(julday,sr)%bsl(l)%aslagm )
            call w_whole_tag( luo_saeinp, input_tag(SCI_AggregateGSD)%name, subrsurf(julday,sr)%bsl(l)%as0ags )
            call w_whole_tag( luo_saeinp, input_tag(SCI_AggregateMIN)%name, subrsurf(julday,sr)%bsl(l)%aslagn )
            call w_whole_tag( luo_saeinp, input_tag(SCI_AggregateMAX)%name, subrsurf(julday,sr)%bsl(l)%aslagx )
            call w_whole_tag( luo_saeinp, input_tag(SCI_WiltingPoint)%name, subrsurf(julday,sr)%bsl(l)%ahrwcw )
            call w_whole_tag( luo_saeinp, input_tag(SCI_WaterContent)%name, subrsurf(julday,sr)%bsl(l)%ahrwca )
            call w_end_tag( luo_saeinp, input_tag(SCI_SoilLay)%name )
         end do
         call w_end_tag( luo_saeinp, input_tag(SCI_SoilLays)%name )
         call w_end_tag( luo_saeinp, input_tag(SoilState)%name )

         close(luo_saeinp)

      end do

      ! deallocate Tag array
      deallocate( input_tag, stat=dealloc_stat)
      if( dealloc_stat .gt. 0 ) then
         ! deallocation failed
         write(*,*) "ERROR: unable to deallocate memory for Tag array"
      end if

      ! deallocate subrfiles array
      deallocate(subrfiles, stat=dealloc_stat)
      if( dealloc_stat .gt. 0 ) then
         ! deallocation failed
         write(*,*) "ERROR: unable to deallocate memory for subrfiles array"
      end if

      deallocate( subr_text )

   end subroutine saeinp

   subroutine daily_erodout( o_unit, o_E_unit, sgrd_u, input_filename, cellstate )

!     +++  PURPOSE +++
!     To print output desired from standalone EROSION submodel

      use file_io_mod, only: fopenk, makenamnum, makedir, in_weps
      use datetime_mod, only: get_systime_string, caldat
      use erosion_data_struct_defs, only: cellsurfacestate, am0efl
      use grid_mod, only: imax, jmax, amasim, amxsim

!     +++ ARGUMENT DECLARATIONS +++
      integer, intent(inout) :: o_unit, o_E_unit, sgrd_u
      character(len=*), intent(in) :: input_filename
      type(cellsurfacestate), dimension(0:,0:), intent(in) :: cellstate     ! initialized grid cell state values

!     ++++ LOCAL VARIABLES +++
      character(len=21) :: rundatetime
      integer i, j
      real tt, lx, ly
      real topt, topcs, topss, top10, top2_5, bott, botcs, botss, bot10, bot2_5
      real ritt, ritcs, ritss, rit10, rit2_5, lftt, lftcs, lftss, lft10, lft2_5
      real tot, totbnd
      character*512 :: daypath  ! the root path plus subdirectory plus specific day directory for sweep output files

      integer yr, mon, day

!     +++ END SPECIFICATIONS +++

      ! Calculate Averages Crossing Borders
      ! top border
      aegt   = 0.0
      aegtcs = 0.0
      aegtss = 0.0
      aegt10 = 0.0
      aegt2_5 = 0.0
      j = jmax
      do i = 1, imax-1
         aegt    = aegt   + cellstate(i,j)%egt
         aegtcs  = aegtcs + cellstate(i,j)%egtcs
         aegtss  = aegtss + cellstate(i,j)%egtss
         aegt10  = aegt10 + cellstate(i,j)%egt10
         aegt2_5 = aegt2_5 + cellstate(i,j)%egt2_5
      end do
      ! calc. average at top border
      topt  = aegt/(imax-1)
      topcs = aegtcs/(imax-1)
      topss = aegtss/(imax-1)
      top10 = aegt10/(imax-1)
      top2_5 = aegt2_5/(imax-1)

      ! bottom border
      aegt   = 0.0
      aegtcs = 0.0
      aegtss = 0.0
      aegt10 = 0.0
      aegt2_5 = 0.0
      j = 0
      do i = 1, imax-1
         aegt    = aegt   + cellstate(i,j)%egt
         aegtcs  = aegtcs + cellstate(i,j)%egtcs
         aegtss  = aegtss + cellstate(i,j)%egtss
         aegt10  = aegt10 + cellstate(i,j)%egt10
         aegt2_5 = aegt2_5 + cellstate(i,j)%egt2_5
      end do
      ! calc. average at bottom border
      bott  = aegt/(imax-1)
      botcs = aegtcs/(imax-1)
      botss = aegtss/(imax-1)
      bot10 = aegt10/(imax-1)
      bot2_5 = aegt2_5/(imax-1)

      ! right border
      aegt   = 0.0
      aegtcs = 0.0
      aegtss = 0.0
      aegt10 = 0.0
      aegt2_5 = 0.0
      i = imax
      do j = 1, jmax-1
         aegt    = aegt   + cellstate(i,j)%egt
         aegtcs  = aegtcs + cellstate(i,j)%egtcs
         aegtss  = aegtss + cellstate(i,j)%egtss
         aegt10  = aegt10 + cellstate(i,j)%egt10
         aegt2_5 = aegt2_5 + cellstate(i,j)%egt2_5
      end do
      ! calc. average at right border
      ritt  = aegt/(jmax-1)
      ritcs = aegtcs/(jmax-1)
      ritss = aegtss/(jmax-1)
      rit10 = aegt10/(jmax-1)
      rit2_5 = aegt2_5/(jmax-1)

      ! left border
      aegt   = 0.0
      aegtcs = 0.0
      aegtss = 0.0
      aegt10 = 0.0
      aegt2_5 = 0.0
      i = 0
      do j = 1, jmax-1
         aegt    = aegt   + cellstate(i,j)%egt
         aegtcs  = aegtcs + cellstate(i,j)%egtcs
         aegtss  = aegtss + cellstate(i,j)%egtss
         aegt10  = aegt10 + cellstate(i,j)%egt10
         aegt2_5 = aegt2_5 + cellstate(i,j)%egt2_5
      end do
      ! calc. average at left border
      lftt   = aegt/(jmax-1)
      lftcs  = aegtcs/(jmax-1)
      lftss  = aegtss/(jmax-1)
      lft10  = aegt10/(jmax-1)
      lft2_5  = aegt2_5/(jmax-1)

      ! calculate averages of inner grid points
      aegt   = 0.0
      aegtcs = 0.0
      aegtss = 0.0
      aegt10 = 0.0
      do j=1,jmax-1
         do i= 1, imax-1
            aegt   = aegt   + cellstate(i,j)%egt
            aegtcs = aegtcs + cellstate(i,j)%egtcs
            aegtss = aegtss + cellstate(i,j)%egtss
            aegt10 = aegt10 + cellstate(i,j)%egt10
            aegt2_5 = aegt2_5 + cellstate(i,j)%egt2_5
         end do
      end do
      tt     = (imax-1)*(jmax-1)
      aegt   = aegt/tt
      aegtcs = aegtcs/tt
      aegtss = aegtss/tt
      aegt10 = aegt10/tt
      aegt2_5 = aegt2_5/tt

      ! calculate comparison of boundary and interior losses
      lx = amxsim(2)%x - amxsim(1)%x
      ly = amxsim(2)%y - amxsim(1)%y
      tot = aegt*lx*ly
      totbnd = (topt + bott)*lx + (ritt + lftt)*ly


      if (btest(am0efl,1)) then

      if( o_unit .eq. 0 ) then
         daypath = trim(mksaeout%fullpath) // makenamnum('saeros', mksaeout%simday, mksaeout%maxday,'/')
         call makedir(daypath)
         call fopenk (o_unit, trim(daypath) // 'erod.egrd','unknown')
         call caldat (mksaeout%jday,day,mon,yr)
         write(*,'(4(a,i0))') 'Made Daily Erosion grid file for: ', day,'/', mon,'/', yr,' simulation day: ', mksaeout%simday
         write(o_unit, "('# WEPS erosion day mon yr daysim',4(1x,i0))") day, mon, yr, mksaeout%simday
         write (o_unit,'(a)')
         write (o_unit,*) 'Grid cell output from WEPS run'
         write (o_unit,'(a)')
      else
         ! write header to files
         write (o_unit,'(a)')
         write (o_unit,'(a)')
         write (o_unit,*) 'Grid cell output from SWEEP run'
         write (o_unit,'(a)')
      end if

      ! Print date of Run
      rundatetime = get_systime_string() ! with Lahey f95, had to assign to variable first
      write(o_unit,"(1x,'Date of run: ',a21)") rundatetime
      write(o_unit,'(a)')

      write(o_unit,fmt="(1x,a)") "<field dimensions>"
      write(o_unit,fmt="(1x,5f10.2)") amasim, amxsim(1)%x, amxsim(1)%y, amxsim(2)%x, amxsim(2)%y
      write(o_unit,fmt="(1x,a)") "</field dimensions>"
      write(o_unit,'(a)')
      write (o_unit,'(a,i0,a,i0,2a,i0,a,i0,a)') 'Total grid size: (', imax+1,',', jmax+1, ')   ', &
                                                'Inner grid size: (', imax-1,',', jmax-1, ')'

      write (o_unit,'(a)')
      write (o_unit,fmt="(1x,'  Passing Border Grid Cells - Total  egt(kg/m)')")
      write (o_unit,'(4a)') '  top(i=1,imax-1,j=jmax) ', &
                       'bottom(i=1,imax-1,j=0) ', &
                       'right(i=imax,j=1,jmax-1) ', &
                       'left(i=0, j=1,jmax-1) '
      write (o_unit,10)  (cellstate(i,jmax)%egt, i = 1, imax-1)
      write (o_unit,10)  (cellstate(i,0)%egt, i = 1, imax-1)
      write (o_unit,10)  (cellstate(imax,j)%egt, j = 1, jmax-1)
      write (o_unit,10)  (cellstate(0,j)%egt, j = 1, jmax-1)

      write (o_unit,'(a)')
      write (o_unit,fmt="(1x,'  Passing Border Grid Cells - Salt/Creep egtcs(kg/m)')")
      write (o_unit,'(4a)') '  top(i=1,imax-1,j=jmax) ', &
                       'bottom(i=1,imax-1,j=0) ', &
                       'right(i=imax,j=1,jmax-1) ', &
                       'left(i=0, j=1,jmax-1) '
      write (o_unit,10)  (cellstate(i,jmax)%egtcs, i = 1, imax-1)
      write (o_unit,10)  (cellstate(i,0)%egtcs, i = 1, imax-1)
      write (o_unit,10)  (cellstate(imax,j)%egtcs, j = 1, jmax-1)
      write (o_unit,10)  (cellstate(0,j)%egtcs, j = 1, jmax-1)

      write (o_unit,'(a)')
      write (o_unit,fmt="(1x,'  Passing Border Grid Cells - Suspension egtss(kg/m)')")
      write (o_unit,'(4a)') '  top(i=1,imax-1,j=jmax) ', &
                       'bottom(i=1,imax-1,j=0) ', &
                       'right(i=imax,j=1,jmax-1) ', &
                       'left(i=0, j=1,jmax-1) '
      write (o_unit,10)  (cellstate(i,jmax)%egtss, i = 1, imax-1)
      write (o_unit,10)  (cellstate(i,0)%egtss, i = 1, imax-1)
      write (o_unit,10)  (cellstate(imax,j)%egtss, j = 1, jmax-1)
      write (o_unit,10)  (cellstate(0,j)%egtss, j = 1, jmax-1)

      write (o_unit,'(a)')
      write (o_unit,fmt="(1x,'  Passing Border Grid Cells - PM10       egt10(kg/m)')")
      write (o_unit,'(4a)') '  top(i=1,imax-1,j=jmax) ', &
                       'bottom(i=1,imax-1,j=0) ', &
                       'right(i=imax,j=1,jmax-1) ', &
                       'left(i=0,j=1,jmax-1) '
      write (o_unit,11)  (cellstate(i,jmax)%egt10, i = 1, imax-1)
      write (o_unit,11)  (cellstate(i,0)%egt10, i = 1, imax-1)
      write (o_unit,11)  (cellstate(imax,j)%egt10, j = 1, jmax-1)
      write (o_unit,11)  (cellstate(0,j)%egt10, j = 1, jmax-1)

      write (o_unit,'(a)')
      write (o_unit,fmt="(1x,'  Passing Border Grid Cells - PM2_5      egt2_5(kg/m)')")
      write (o_unit,'(4a)') '  top(i=1,imax-1,j=jmax) ', &
                       'bottom(i=1,imax-1,j=0) ', &
                       'right(i=imax,j=1,jmax-1) ', &
                       'left(i=0,j=1,jmax-1) '
      write (o_unit,11)  (cellstate(i,jmax)%egt2_5, i = 1, imax-1)
      write (o_unit,11)  (cellstate(i,0)%egt2_5, i = 1, imax-1)
      write (o_unit,11)  (cellstate(imax,j)%egt2_5, j = 1, jmax-1)
      write (o_unit,11)  (cellstate(0,j)%egt2_5, j = 1, jmax-1)

      write (o_unit,'(a)')
      write (o_unit,fmt="(' <grid data> | ',3('|',a))") 'Total Soil Loss', 'soil loss', '(kg/m^2)'
      do j = jmax-1, 1, -1
         write (o_unit,10)  (cellstate(i,j)%egt, i = 1, imax-1)
      end do
      write (o_unit,fmt="(' </grid data>')")

      write (o_unit,'(a)')
      write (o_unit,fmt="(' <grid data> | ',3('|',a))") 'Saltation/Creep Soil Loss', 'salt/creep soil loss', '(kg/m^2)'
      do j = jmax-1, 1, -1
         write (o_unit,10)  (cellstate(i,j)%egtcs, i = 1, imax-1)
      end do
      write (o_unit,fmt="(' </grid data>')")

      write (o_unit,'(a)')
      write (o_unit,fmt="(' <grid data> | ',3('|',a))") 'Suspension Soil Loss', 'suspension soil loss', '(kg/m^2)'
      do j = jmax-1, 1, -1
         write (o_unit,10)  (cellstate(i,j)%egtss, i = 1, imax-1)
      end do
      write (o_unit,fmt="(' </grid data>')")

      write (o_unit,'(a)')
      write (o_unit,fmt="(' <grid data> | ',3('|',a))") 'PM10 Soil Loss', 'PM10 soil loss', '(kg/m^2)'
      do j = jmax-1, 1, -1
         write (o_unit,11)  (cellstate(i,j)%egt10, i = 1, imax-1)
      end do
      write (o_unit,fmt="(' </grid data>')")

      write (o_unit,'(a)')
      write (o_unit,fmt="(' <grid data> | ',3('|',a))") 'PM2_5 Soil Loss', 'PM2_5 soil loss', '(kg/m^2)'
      do j = jmax-1, 1, -1
         write (o_unit,11)  (cellstate(i,j)%egt2_5, i = 1, imax-1)
      end do
      write (o_unit,fmt="(' </grid data>')")

      write (o_unit,'(a)')
      write (o_unit,*) '**Averages - Field'
      write (o_unit,*) '     Total    salt/creep      susp       PM10        PM2.5'
      write (o_unit,*) '     egt                      egtss      egt10       egt2_5'
      write (o_unit,*) '   -----------------------kg/m^2---------------------------'
      write (o_unit,fmt="(1x, 3(f12.4,2x), 2(f12.6,2x))")    aegt, aegtcs, aegtss, aegt10, aegt2_5
      write (o_unit,'(a)')
      write (o_unit,*) '**Averages - Crossing Boundaries '
      write (o_unit,*) 'Location      Total  Salt/Creep   Susp    PM10     PM2_5'
      write (o_unit,*) '-------------------------kg/m---------------------------'
      write (o_unit,fmt="(1x, 'top   ', 1x, 5(f9.2,1x))") topt, topcs, topss, top10, top2_5
      write (o_unit,fmt="(1x, 'bottom', 1x, 5(f9.2,1x))") bott, botcs, botss, bot10, bot2_5
      write (o_unit,fmt="(1x, 'right ', 1x, 5(f9.2,1x))") ritt, ritcs, ritss, rit10, rit2_5
      write (o_unit,fmt="(1x, 'left  ', 1x, 5(f9.2,1x))") lftt, lftcs, lftss, lft10, lft2_5
      write (o_unit,'(a)')
      write (o_unit,*) '   Comparison of interior & boundary loss'
      write (o_unit,*) '      interior       boundary    int/bnd ratio'
      if( totbnd.gt.1.0e-9 ) then
         write (o_unit,16) tot, totbnd, tot/totbnd
      else
         !Boundary loss near or equal to zero
         write (o_unit,16) tot, totbnd, 1.0e-9
      end if

!     additional output statements for easy shell script parsing
      write (o_unit,'(a)')
!     write losses as positive numbers
      write (o_unit,fmt="(' repeat of total, salt/creep, susp, PM10, PM2.5:', 3f12.4,3f12.6)") &
                                    -aegt, -aegtcs, -aegtss, -aegt10, -aegt2_5
      close(o_unit)

!     output formats
   10 format (1x, 500f12.4)
   11 format (1x, 500f12.6)
   16 format (1x, 2(f15.4,2x),2x, f13.4)

      end if !if (btest(am0efl,1)) then

      !Erosion summary - total, salt/creep, susp, pm10
      !(loss values are positive - deposition values are negative)
      if (btest(am0efl,0)) then
         if( in_weps ) then
            call caldat (mksaeout%jday,day,mon,yr)
            write(*,'(4(a,i0))') 'Wrote to Daily Erosion summary file: ', day,'/', mon,'/', yr,' simulation day: ', mksaeout%simday
            write (UNIT=o_E_unit,FMT="(5(f12.6),' ')",ADVANCE="NO") -aegt, -aegtcs, -aegtss, -aegt10, -aegt2_5
            write (UNIT=o_E_unit,FMT="('# WEPS erosion day mon yr',4(1x,i0))",ADVANCE="NO") day, mon, yr, mksaeout%simday
            write (UNIT=o_E_unit,FMT="(A)",ADVANCE="YES") ' (loss values are positive - deposition values are negative)'
         else
            write (UNIT=o_E_unit,FMT="(5(f12.6),' ')",ADVANCE="NO") -aegt, -aegtcs, -aegtss, -aegt10, -aegt2_5
            write (UNIT=o_E_unit,FMT="(A)",ADVANCE="NO") trim(input_filename)
            write (UNIT=o_E_unit,FMT="(A)",ADVANCE="YES") ' (loss values are positive - deposition values are negative)'
         end if
      end if

      !Duplicate Erosion summary info for the *.sgrd file so "sweep" interface
      ! can display this info on graphical report window
      if (btest(am0efl,3) .and. (sgrd_u .ge. 0) ) then
         write (sgrd_u,'(a)')
         write (sgrd_u,*) '**Averages - Field'
         write (sgrd_u,*) '     Total    salt/creep      susp       PM10        PM2.5'
         write (sgrd_u,*) '     egt                      egtss      egt10       egt2_5'
         write (sgrd_u,*) '   -----------------------kg/m^2---------------------------'
         write (sgrd_u,fmt="(1x, 3(f12.4,2x), 2(f12.6,2x))")    aegt, aegtcs, aegtss, aegt10, aegt2_5
         write (sgrd_u,'(a)')
         write (sgrd_u,*) '**Averages - Crossing Boundaries '
         write (sgrd_u,*) 'Location      Total  Salt/Creep   Susp    PM10     PM2_5'
         write (sgrd_u,*) '--------------------------kg/m--------------------------'
         write (sgrd_u,fmt="(1x, 'top   ', 1x, 5(f9.2,1x))") topt, topcs, topss, top10, top2_5
         write (sgrd_u,fmt="(1x, 'bottom', 1x, 5(f9.2,1x))") bott, botcs, botss, bot10, bot2_5
         write (sgrd_u,fmt="(1x, 'right ', 1x, 5(f9.2,1x))") ritt, ritcs, ritss, rit10, rit2_5
         write (sgrd_u,fmt="(1x, 'left  ', 1x, 5(f9.2,1x))") lftt, lftcs, lftss, lft10, lft2_5
         write (sgrd_u,'(a)')
         write (sgrd_u,*) '   Comparison of interior & boundary loss'
         write (sgrd_u,*) '      interior       boundary    int/bnd ratio'
         if( totbnd.gt.1.0e-9 ) then
            write (sgrd_u,16) tot, totbnd, tot/totbnd
         else
            !Boundary loss near or equal to zero
            write (sgrd_u,16) tot, totbnd, 1.0e-9
         end if
         if( sgrd_u .ge. 0 ) then
            close(sgrd_u)
         end if
      end if

   end subroutine daily_erodout

   subroutine sb1out( jj, nn, hr, ws, wdir, o_unit, subrsurf, cellstate )

!     + + + PURPOSE + + +
!     To print to file tst.out some key variables used in erosion
!     use wind dir of 270 for most to see output along wind direction

      use datetime_mod, only: get_systime_string, caldat
      use erosion_data_struct_defs, only: subregionsurfacestate, cellsurfacestate, awzypt, anemht, wzoflg, ntstep
      use erosion_data_struct_defs, only: initflag, ipd, npd
      use grid_mod, only: awa, imax, jmax, amasim, amxsim

!     + + + ARGUEMENT DECLARATIONS + + +
      real ws, wdir, hr
      integer  jj, nn, o_unit
      type(subregionsurfacestate), intent(in) :: subrsurf  ! subregion surface conditions (erosion specific set)
      type(cellsurfacestate), dimension(0:,0:), intent(inout) :: cellstate     ! initialized grid cell state values

!     + + + ARGUMENT DEFINITIONS + + +
!     o_unit= Unit number for output file

!     + + + LOCAL VARIABLES + + +
      character(len=21) :: rundatetime
      integer :: yr, mo, da
      integer :: i, j, kbr

!     + + + END SPECIFICATIONS + + +

!     output headings?
      if (initflag .eq. 0) then

        ipd = 0
        npd = nn * ntstep

        call caldat( mksaeout%jday, da, mo, yr) ! Set day, month and year

        write (o_unit,'(a)')
        write (o_unit,*) 'OUT PUT from sb1out'
        write (o_unit,'(a)')

        ! Print date of Run
        rundatetime = get_systime_string() ! with Lahey f95, had to assign to variable first
        write(o_unit,"(1x,'Date of run: ',a21)") rundatetime
        write(o_unit,'(a)')

        write (unit=o_unit,fmt="(a,f5.2,a2,a,i1)") ' anemht = ', anemht, 'm', '    wzoflg = ', wzoflg
        write (unit=o_unit,fmt="(a,f6.2,a4)") ' wind direction = ', wdir, 'deg'
        write (unit=o_unit,fmt="(a,f6.2,a4)") ' wind direction relative to field orientation = ', awa, 'deg'
        write (o_unit,'(a)')

        ! this section inserted for compatibility with previous output version
        if (wdir .ge. 337.5 .or. wdir .lt. 22.5) then
          kbr = 1
        elseif (wdir .lt. 67.5) then
          kbr = 2
        elseif (wdir.lt. 112.5) then
          kbr = 3
        elseif (wdir .lt. 157.5) then
          kbr = 4
        elseif (wdir .lt. 202.5) then
          kbr = 5
        elseif (wdir.lt. 247.5) then
          kbr = 6
        elseif (wdir .lt. 292.5) then
          kbr = 7
        else
          kbr = 8
        endif
        write (unit=o_unit,fmt="(a,i1)") ' wind quadrant = ', kbr
        write (o_unit,'(a)')
        ! end inserted section

        write (o_unit,*) 'orientation and dimensions of sim region'
        write (o_unit,*) 'amasim(deg)  amxsim - (x1,y1) (x2,y2)'
        write(o_unit,fmt="(1x,5f8.2)") amasim, amxsim(1)%x, amxsim(1)%y, amxsim(2)%x, amxsim(2)%y
        write (o_unit,'(a)')

        write (o_unit,*) "Surface properties"
        write (o_unit,fmt="(a,f8.2,a)") "Ridge spacing parallel to wind direction", subrsurf%sxprg, " (mm)"
        write (o_unit,fmt="(a,f5.2,a)") "Composite weighted average biomass height", subrsurf%abzht, " (m)"
        write (o_unit,fmt="(a,f5.2,a)") "Biomass leaf area index", subrsurf%abrlai, " (m^2/m^2)"
        write (o_unit,fmt="(a,f5.2,a)") "Biomass stem area index", subrsurf%abrsai, " (m^2/m^2)"
        write (o_unit,fmt="(a,f5.2,a)") "Biomass flat cover", subrsurf%abffcv, " (m^2/m^2)"

        write (o_unit,fmt="(a,f8.2,a)") "Average yearly total precipitation ", awzypt, " (mm)"
        write (o_unit,'(a)')

        write(o_unit,fmt="(1x,a)") "<field dimensions>"
        write(o_unit,fmt="(1x,5f10.2)")amasim, amxsim(1)%x, amxsim(1)%y, amxsim(2)%x, amxsim(2)%y
        write(o_unit,fmt="(1x,a)") "</field dimensions>"

        write (o_unit,'(a)')
      endif

      call caldat( mksaeout%jday, da, mo, yr) ! Set day, month and year

      write (o_unit, fmt="(a, i5, 2(i3), f7.3, 4(i4))") ' yr mon day hr upd_pd jj nn(subpd) npd (sbqout 1)', &
                                                          yr,mo, da, hr, ipd,  jj, nn,      npd
      write (o_unit,'(a)')
      write (o_unit, fmt="(a, f5.2, 2(f7.2))") ' pd wind speed, dir and dir rel to field ', ws, wdir, awa
      write (o_unit,'(a)')

      write (o_unit,*) "Surface layer properties"
      write (o_unit,fmt="(a,f5.2,a)") "Surface course fragments", subrsurf%bsl(1)%asvroc, " (m^3/m^3)"
      write (o_unit,fmt="(a,a,f5.2,a)") "Initial soil ", "mass fraction in surface layer < 0.10 mm ", subrsurf%sf10ic, " (kg/kg)"
      write (o_unit,fmt="(a,a,f5.2,a)") "Initial soil ", "mass fraction in surface layer < 0.84 mm ", subrsurf%sf84ic, " (kg/kg)"

      write (o_unit,*) "PM10 emission properties"
      write (o_unit,fmt="(a,f5.2,a)") "Soil fraction PM10 in abraded suspension ", subrsurf%asf10an
      write (o_unit,fmt="(a,f5.2,a)") "Soil fraction PM10 in emitted suspension ", subrsurf%asf10en
      write (o_unit,fmt="(a,f5.2,a)") "Soil fraction PM10 in saltation breakage suspension ", subrsurf%asf10bk
      write (o_unit,fmt="(a,f5.2,a)") "Coefficient of abrasion of aggregates ", subrsurf%acanag
      write (o_unit,fmt="(a,f5.2,a)") "Coefficient of abrasion of crust ", subrsurf%acancr

      if (initflag .eq. 0) then

        ! Grid cell data
        ! Friction Velocity
        write(o_unit,fmt="(' <grid data> |',i5,2(i3),f7.3,3('|',a))") yr, mo, da, hr, &
            'Surface Friction Velocity', 'friction velocity', '(m/s)'
        do j = jmax-1, 1, -1
          write (o_unit, fmt="(500f12.4)") (cellstate(i,j)%wus, i = 1, imax-1)
        end do
        write(o_unit,fmt="(' </grid data>')")
        write(o_unit,fmt="(' <grid data> |',i5,2(i3),f7.3,3('|',a))") yr, mo, da, hr, &
            'Threshold Surface Friction Velocity', 'threshold friction velocity', '(m/s)'
        do j = jmax-1, 1, -1
          write (o_unit, fmt="(500f12.4)") (cellstate(i,j)%wust, i = 1, imax-1)
        end do
        write(o_unit,fmt="(' </grid data>')")
        write(o_unit,fmt="(' <grid data> |',i5,2(i3),f7.3,3('|',a))") yr, mo, da, hr, &
            'Transport Threshold Surface Friction Velocity', 'transport threshold friction velocity', '(m/s)'
        do j = jmax-1, 1, -1
          write (o_unit, fmt="(500f12.4)") (cellstate(i,j)%wusp, i = 1, imax-1)
        end do
        write(o_unit,fmt="(' </grid data>')")
        write (o_unit,'(a)')

        ! Emissions
        write(o_unit,fmt="(' <grid data> |',i5,2(i3),f7.3,3('|',a))") yr, mo, da, hr, &
             'Cumulative Total Soil Loss', 'soil loss', '(kg/m^2)'
        do j = jmax-1, 1, -1
          write (o_unit, fmt="(500f12.4)") (cellstate(i,j)%egt, i = 1, imax-1)
        end do
        write(o_unit,fmt="(' </grid data>')")

        write(o_unit,fmt="(' <grid data> |',i5,2(i3),f7.3,3('|',a))") yr, mo, da, hr, &
            'Cumulative Saltation/Creep Soil Loss', 'salt/creep soil loss', '(kg/m^2)'
        do j = jmax-1, 1, -1
          write (o_unit, fmt="(500f12.4)")(cellstate(i,j)%egtcs,i=1,imax-1)
        end do
        write(o_unit,fmt="(' </grid data>')")

        write(o_unit,fmt="(' <grid data> |',i5,2(i3),f7.3,3('|',a))") yr, mo, da, hr, &
            'Cumulative Suspension Soil Loss', 'suspension soil loss', '(kg/m^2)'
        do j = jmax-1, 1, -1
          write (o_unit, fmt="(500f12.4)") (cellstate(i,j)%egtss, i = 1, imax-1)
        end do
        write(o_unit,fmt="(' </grid data>')")

        write(o_unit,fmt="(' <grid data> |',i5,2(i3),f7.3,3('|',a))") yr, mo, da, hr, &
            'Cumulative PM10 Soil Loss', 'PM10 soil loss', '(kg/m^2)'
        do j = jmax-1, 1, -1
          write (o_unit, fmt="(500f12.6)") (cellstate(i,j)%egt10, i = 1, imax-1)
        end do
        write(o_unit,fmt="(' </grid data>')")

        write(o_unit,fmt="(' <grid data> |',i5,2(i3),f7.3,3('|',a))") yr, mo, da, hr, &
            'Cumulative PM2_5 Soil Loss', 'PM2_5 soil loss', '(kg/m^2)'
        do j = jmax-1, 1, -1
          write (o_unit, fmt="(500f12.6)") (cellstate(i,j)%egt2_5, i = 1, imax-1)
        end do
        write(o_unit,fmt="(' </grid data>')")

        ! Grid Cell Surface properties
        write(o_unit,fmt="(' <grid data> |',i5,2(i3),f7.3,3('|',a))") yr, mo, da, hr, &
            'Surface Random Roughness', 'random roughness', '(mm)'
        do j = jmax-1, 1, -1
          write (o_unit, fmt="(500f12.4)") (cellstate(i,j)%slrr, i = 1, imax-1)
        end do
        write(o_unit,fmt="(' </grid data>')")
        write(o_unit,fmt="(' <grid data> |',i5,2(i3),f7.3,3('|',a))") yr, mo, da, hr, &
            'Surface Oriented Roughness', 'ridge height', '(mm)'
        do j = jmax-1, 1, -1
          write (o_unit, fmt="(500f12.4)") (cellstate(i,j)%szrgh, i = 1, imax-1)
        end do
        write(o_unit,fmt="(' </grid data>')")
        write(o_unit,fmt="(' <grid data> |',i5,2(i3),f7.3,3('|',a))") yr, mo, da, hr, &
            'Surface Rock', 'surface volume rock fraction', '(m^3/m^3)'
        do j = jmax-1, 1, -1
          write (o_unit, fmt="(500f12.4)") (cellstate(i,j)%svroc, i = 1, imax-1)
        end do
        write(o_unit,fmt="(' </grid data>')")
        write (o_unit,'(a)')

        write(o_unit,fmt="(' <grid data> |',i5,2(i3),f7.3,3('|',a))") yr, mo, da, hr, &
            'Soil Agg. Size < 0.01','mass fraction < 0.01 mm size','(fract.)'
        do j = jmax-1, 1, -1
          write (o_unit, fmt="(500f12.4)") (cellstate(i,j)%sf1, i = 1, imax-1)
        end do
        write(o_unit,fmt="(' </grid data>')")
        write(o_unit,fmt="(' <grid data> |',i5,2(i3),f7.3,3('|',a))") yr, mo, da, hr, &
            'Soil Agg. Size < 0.1', 'mass fraction < 0.1 mm size', '(fract.)'
        do j = jmax-1, 1, -1
          write (o_unit, fmt="(500f12.4)") (cellstate(i,j)%sf10, i = 1, imax-1)
        end do
        write(o_unit,fmt="(' </grid data>')")
        write(o_unit,fmt="(' <grid data> |',i5,2(i3),f7.3,3('|',a))") yr, mo, da, hr, &
            'Soil Agg. Size < 0.84','mass fraction < 0.84 mm size','(fract.)'
        do j = jmax-1, 1, -1
          write (o_unit, fmt="(500f12.4)") (cellstate(i,j)%sf84, i = 1, imax-1)
        end do
        write(o_unit,fmt="(' </grid data>')")
        write(o_unit,fmt="(' <grid data> |',i5,2(i3),f7.3,3('|',a))") yr, mo, da, hr, &
            'Soil Agg. Size < 2.0', 'mass fraction < 2.0 mm size', '(fract.)'
        do j = jmax-1, 1, -1
          write (o_unit, fmt="(500f12.4)") (cellstate(i,j)%sf200, i = 1, imax-1)
        end do
        write(o_unit,fmt="(' </grid data>')")
        write(o_unit,fmt="(' <grid data> |',i5,2(i3),f7.3,3('|',a))") yr, mo, da, hr, &
            'Soil Agg. Size for u* to be the thresh. friction velocity', '"effective" mass fraction < 0.84 mm size', '(fract.)'
        do j = jmax-1, 1, -1
          write (o_unit, fmt="(500f12.4)") (cellstate(i,j)%sf84mn, i = 1, imax-1)
        end do
        write(o_unit,fmt="(' </grid data>')")
        write(o_unit,fmt="(' <grid data> |',i5,2(i3),f7.3,3('|',a))") yr, mo, da, hr, &
            'Mobile soil removable from aggregated surface', 'mass removable', '(kg/m^2)'
        do j = jmax-1, 1, -1
          write (o_unit, fmt="(500f12.4)") (cellstate(i,j)%smaglos, i = 1, imax-1)
        end do
        write(o_unit,fmt="(' </grid data>')")
        write(o_unit,fmt="(' <grid data> |',i5,2(i3),f7.3,3('|',a))") yr, mo, da, hr, &
            'Change in mobile soil on aggregated surface', 'net mass change', '(kg/m^2)'
        do j = jmax-1, 1, -1
          write (o_unit, fmt="(500f12.4)") (cellstate(i,j)%dmlos, i = 1, imax-1)
        end do
        write(o_unit,fmt="(' </grid data>')")

        ! Crust properties
        write(o_unit,fmt="(' <grid data> |',i5,2(i3),f7.3,3('|',a))") yr, mo, da, hr, &
            'Consolidated crust thickness', 'crust thickness', '(mm)'
        do j = jmax-1, 1, -1
          write (o_unit, fmt="(500f12.4)") (cellstate(i,j)%szcr, i = 1, imax-1)
        end do
        write(o_unit,fmt="(' </grid data>')")
        write(o_unit,fmt="(' <grid data> |',i5,2(i3),f7.3,3('|',a))") yr, mo, da, hr, &
            'Fraction of Surface covered with Crust','crust cover','(fract.)'
        do j = jmax-1, 1, -1
          write (o_unit, fmt="(500f12.4)") (cellstate(i,j)%sfcr, i = 1, imax-1)
        end do
        write(o_unit,fmt="(' </grid data>')")
        write(o_unit,fmt="(' <grid data> |',i5,2(i3),f7.3,3('|',a))") yr, mo, da, hr, &
            'Fraction of Crusted Surface covered with Loose Erodible Soil', 'loose erodible material', '(fract.)'
        do j = jmax-1, 1, -1
          write (o_unit, fmt="(500f12.4)") (cellstate(i,j)%sflos, i = 1, imax-1)
        end do
        write(o_unit,fmt="(' </grid data>')")
        write(o_unit,fmt="(' <grid data> |',i5,2(i3),f7.3,3('|',a))") yr, mo, da, hr, &
            'Mass of Loose Erodible Soil on Crusted Surface', 'loose erodible material', '(kg/m^2)'
        do j = jmax-1, 1, -1
          write (o_unit, fmt="(500f12.4)") (cellstate(i,j)%smlos, i = 1, imax-1)
        end do
        write(o_unit,fmt="(' </grid data>')")
        write (o_unit,'(a)')

        ! input fluxes
        write(o_unit,fmt="(' <grid data> |',i5,2(i3),f7.3,3('|',a))") yr, mo, da, hr, &
           'Cell Total Input Flux', 'total input flux', '(kg/m/s)'
        do j = jmax-1, 1, -1
          write (o_unit, fmt="(500f12.4)") (cellstate(i,j)%qi, i = 1, imax-1)
        end do
        write(o_unit,fmt="(' </grid data>')")

        write(o_unit,fmt="(' <grid data> |',i5,2(i3),f7.3,3('|',a))") yr, mo, da, hr, &
           'Cell Saltation/Creep Input Flux', 'saltation/creep input flux', '(kg/m/s)'
        do j = jmax-1, 1, -1
          write (o_unit, fmt="(500f12.4)") (cellstate(i,j)%qcsi, i = 1, imax-1)
        end do
        write(o_unit,fmt="(' </grid data>')")

        write(o_unit,fmt="(' <grid data> |',i5,2(i3),f7.3,3('|',a))") yr, mo, da, hr, &
           'Cell Suspension Input Flux', 'suspension input flux', '(kg/m/s)'
        do j = jmax-1, 1, -1
          write (o_unit, fmt="(500f12.4)") (cellstate(i,j)%qssi, i = 1, imax-1)
        end do
        write(o_unit,fmt="(' </grid data>')")

        write(o_unit,fmt="(' <grid data> |',i5,2(i3),f7.3,3('|',a))") yr, mo, da, hr, &
           'Cell PM10 Input Flux', 'pm10 input flux', '(kg/m/s)'
        do j = jmax-1, 1, -1
          write (o_unit, fmt="(500f12.4)") (cellstate(i,j)%q10i, i = 1, imax-1)
        end do
       write(o_unit,fmt="(' </grid data>')")

        ! output fluxes
        write(o_unit,fmt="(' <grid data> |',i5,2(i3),f7.3,3('|',a))") yr, mo, da, hr, &
           'Cell Total Output Flux', 'total output flux', '(kg/m/s)'
        do j = jmax-1, 1, -1
          write (o_unit, fmt="(500f12.4)") (cellstate(i,j)%qo, i = 1, imax-1)
        end do
        write(o_unit,fmt="(' </grid data>')")

        write(o_unit,fmt="(' <grid data> |',i5,2(i3),f7.3,3('|',a))") yr, mo, da, hr, &
           'Cell Saltation/Creep Output Flux', 'saltation/creep output flux', '(kg/m/s)'
        do j = jmax-1, 1, -1
          write (o_unit, fmt="(500f12.4)") (cellstate(i,j)%qcso, i = 1, imax-1)
        end do
        write(o_unit,fmt="(' </grid data>')")

        write(o_unit,fmt="(' <grid data> |',i5,2(i3),f7.3,3('|',a))") yr, mo, da, hr, &
           'Cell Suspension Output Flux', 'suspension output flux', '(kg/m/s)'
        do j = jmax-1, 1, -1
          write (o_unit, fmt="(500f12.4)") (cellstate(i,j)%qsso, i = 1, imax-1)
        end do
        write(o_unit,fmt="(' </grid data>')")

        write(o_unit,fmt="(' <grid data> |',i5,2(i3),f7.3,3('|',a))") yr, mo, da, hr, &
           'Cell PM10 Output Flux', 'pm10 output flux', '(kg/m/s)'
        do j = jmax-1, 1, -1
          write (o_unit, fmt="(500f12.4)") (cellstate(i,j)%q10o, i = 1, imax-1)
        end do
        write(o_unit,fmt="(' </grid data>')")

        ! delta fluxes
        write(o_unit,fmt="(' <grid data> |',i5,2(i3),f7.3,3('|',a))") yr, mo, da, hr, &
           'Cell Total Flux Change', 'total flux change', '(kg/m/s)'
        do j = jmax-1, 1, -1
          write (o_unit, fmt="(500f12.4)") (cellstate(i,j)%qo-cellstate(i,j)%qi, i = 1, imax-1)
        end do
        write(o_unit,fmt="(' </grid data>')")

        write(o_unit,fmt="(' <grid data> |',i5,2(i3),f7.3,3('|',a))") yr, mo, da, hr, &
           'Cell Saltation/Creep Flux Change', 'saltation/creep flux change', '(kg/m/s)'
        do j = jmax-1, 1, -1
          write (o_unit, fmt="(500f12.4)") ( cellstate(i,j)%qcso-cellstate(i,j)%qcsi, i = 1, imax-1)
        end do
        write(o_unit,fmt="(' </grid data>')")

        write(o_unit,fmt="(' <grid data> |',i5,2(i3),f7.3,3('|',a))") yr, mo, da, hr, &
           'Cell Suspension Flux Change', 'suspension flux change', '(kg/m/s)'
        do j = jmax-1, 1, -1
          write (o_unit, fmt="(500f12.4)") (cellstate(i,j)%qsso-cellstate(i,j)%qssi, i = 1, imax-1)
        end do
        write(o_unit,fmt="(' </grid data>')")

        write(o_unit,fmt="(' <grid data> |',i5,2(i3),f7.3,3('|',a))") yr, mo, da, hr, &
           'Cell PM10 Flux Change', 'pm10 flux change', '(kg/m/s)'
        do j = jmax-1, 1, -1
          write (o_unit, fmt="(500f12.4)") (cellstate(i,j)%q10o-cellstate(i,j)%q10i, i = 1, imax-1)
        end do
        write(o_unit,fmt="(' </grid data>')")

        initflag = 1    !     turn off heading output
      endif

   end subroutine sb1out

   subroutine sb2out (jj, nn, hr, o_unit, cellstate)

!     + + + PURPOSE + + +
!     To print to file some key variables used in erosion

      use datetime_mod, only: caldat
      use erosion_data_struct_defs, only: cellsurfacestate
      use erosion_data_struct_defs, only: ipd, npd
      use grid_mod, only: imax, jmax

!     + + + ARGUEMENT DECLARATIONS + + +
      real hr
      integer  jj, nn, o_unit
      type(cellsurfacestate), dimension(0:,0:), intent(inout) :: cellstate     ! initialized grid cell state values

!     + + + ARGUMENT DEFINITIONS + + +
!     o_unit= Unit number for output file

!     + + + LOCAL VARIABLES + + +
      real egavg(imax)
      integer yr, mo, da
      integer i, j, k     ! do loop indexes

!     outflag = 0 - print heading output, 1 - no more heading

!     + + + END SPECIFICATIONS + + +

      ipd = ipd + 1

      call caldat( mksaeout%jday, da, mo, yr) ! Set day, month and year

      write (o_unit, fmt="(a, i5, 2(i3), f7.3, 4(i4))") &
             ' yr mon day hr upd_pd jj nn(subpd) npd (sbqout 2)', &
               yr,mo, da, hr, ipd,  jj,nn,       npd

      ! Friction Velocity
      write(o_unit,fmt="(' <grid data> |',i5,2(i3),f7.3,3('|',a))") yr, mo, da, hr, &
            'Surface Friction Velocity', 'friction velocity', '(m/s)'
      do j = jmax-1, 1, -1
        write (o_unit, fmt="(500f12.4)") (cellstate(i,j)%wus, i = 1, imax-1)
      end do
      write(o_unit,fmt="(' </grid data>')")
      write(o_unit,fmt="(' <grid data> |',i5,2(i3),f7.3,3('|',a))") yr, mo, da, hr, &
            'Threshold Surface Friction Velocity', 'threshold friction velocity', '(m/s)'
       do j = jmax-1, 1, -1
        write (o_unit, fmt="(500f12.4)") (cellstate(i,j)%wust, i = 1, imax-1)
      end do
      write(o_unit,fmt="(' </grid data>')")
      write(o_unit,fmt="(' <grid data> |',i5,2(i3),f7.3,3('|',a))") yr, mo, da, hr, &
            'Transport Threshold Surface Friction Velocity', 'transport threshold friction velocity', '(m/s)'
      do j = jmax-1, 1, -1
        write (o_unit, fmt="(500f12.4)") (cellstate(i,j)%wusp, i = 1, imax-1)
      end do
      write(o_unit,fmt="(' </grid data>')")
      write (o_unit,'(a)')

      ! Emissions
      write(o_unit,fmt="(' <grid data> |',i5,2(i3),f7.3,3('|',a))") yr, mo, da, hr, &
             'Cumulative Total Soil Loss', 'soil loss', '(kg/m^2)'
      do j = jmax-1, 1, -1
        write (o_unit, fmt="(500f12.4)") (cellstate(i,j)%egt, i = 1, imax-1)
      end do
      write(o_unit,fmt="(' </grid data>')")

      write(o_unit,fmt="(' <grid data> |',i5,2(i3),f7.3,3('|',a))") yr, mo, da, hr, &
            'Cumulative Saltation/Creep Soil Loss', 'salt/creep soil loss', '(kg/m^2)'
      do j = jmax-1, 1, -1
        write (o_unit, fmt="(500f12.4)")(cellstate(i,j)%egtcs,i=1,imax-1)
      end do
      write(o_unit,fmt="(' </grid data>')")

      write(o_unit,fmt="(' <grid data> |',i5,2(i3),f7.3,3('|',a))") yr, mo, da, hr, &
            'Cumulative Suspension Soil Loss', 'suspension soil loss', '(kg/m^2)'
      do j = jmax-1, 1, -1
        write (o_unit, fmt="(500f12.4)") (cellstate(i,j)%egtss, i = 1, imax-1)
      end do
      write(o_unit,fmt="(' </grid data>')")

      write(o_unit,fmt="(' <grid data> |',i5,2(i3),f7.3,3('|',a))") yr, mo, da, hr, &
            'Cumulative PM10 Soil Loss', 'PM10 soil loss', '(kg/m^2)'
      do j = jmax-1, 1, -1
        write (o_unit, fmt="(500f12.6)") (cellstate(i,j)%egt10, i = 1, imax-1)
      end do
      write(o_unit,fmt="(' </grid data>')")

      write(o_unit,fmt="(' <grid data> |',i5,2(i3),f7.3,3('|',a))") yr, mo, da, hr, &
            'Cumulative PM2_5 Soil Loss', 'PM2_5 soil loss', '(kg/m^2)'
      do j = jmax-1, 1, -1
        write (o_unit, fmt="(500f12.6)") (cellstate(i,j)%egt2_5, i = 1, imax-1)
      end do
      write(o_unit,fmt="(' </grid data>')")

      ! Surface properties
      write(o_unit,fmt="(' <grid data> |',i5,2(i3),f7.3,3('|',a))") yr, mo, da, hr, &
            'Surface Random Roughness', 'random roughness', '(mm)'
      do j = jmax-1, 1, -1
        write (o_unit, fmt="(500f12.4)") (cellstate(i,j)%slrr, i = 1, imax-1)
      end do
      write(o_unit,fmt="(' </grid data>')")
      write(o_unit,fmt="(' <grid data> |',i5,2(i3),f7.3,3('|',a))") yr, mo, da, hr, &
            'Surface Oriented Roughness', 'ridge height', '(mm)'
      do j = jmax-1, 1, -1
        write (o_unit, fmt="(500f12.4)") (cellstate(i,j)%szrgh, i = 1, imax-1)
      end do
      write(o_unit,fmt="(' </grid data>')")
      write(o_unit,fmt="(' <grid data> |',i5,2(i3),f7.3,3('|',a))") yr, mo, da, hr, &
            'Surface Rock', 'surface volume rock fraction', '(m^3/m^3)'
      do j = jmax-1, 1, -1
        write (o_unit, fmt="(500f12.4)") (cellstate(i,j)%svroc, i = 1, imax-1)
      end do
      write(o_unit,fmt="(' </grid data>')")
      write (o_unit,'(a)')

      write(o_unit,fmt="(' <grid data> |',i5,2(i3),f7.3,3('|',a))") yr, mo, da, hr, &
            'Soil Agg. Size < 0.01','mass fraction < 0.01 mm size','(fract.)'
      do j = jmax-1, 1, -1
        write (o_unit, fmt="(500f12.4)") (cellstate(i,j)%sf1, i = 1, imax-1)
      end do
      write(o_unit,fmt="(' </grid data>')")
      write(o_unit,fmt="(' <grid data> |',i5,2(i3),f7.3,3('|',a))") yr, mo, da, hr, &
            'Soil Agg. Size < 0.1', 'mass fraction < 0.1 mm size', '(fract.)'
      do j = jmax-1, 1, -1
        write (o_unit, fmt="(500f12.4)") (cellstate(i,j)%sf10, i = 1, imax-1)
      end do
      write(o_unit,fmt="(' </grid data>')")
      write(o_unit,fmt="(' <grid data> |',i5,2(i3),f7.3,3('|',a))") yr, mo, da, hr, &
            'Soil Agg. Size < 0.84','mass fraction < 0.84 mm size','(fract.)'
      do j = jmax-1, 1, -1
        write (o_unit, fmt="(500f12.4)") (cellstate(i,j)%sf84, i = 1, imax-1)
      end do
      write(o_unit,fmt="(' </grid data>')")
      write(o_unit,fmt="(' <grid data> |',i5,2(i3),f7.3,3('|',a))") yr, mo, da, hr, &
            'Soil Agg. Size < 2.0', 'mass fraction < 2.0 mm size', '(fract.)'
      do j = jmax-1, 1, -1
        write (o_unit, fmt="(500f12.4)") (cellstate(i,j)%sf200, i = 1, imax-1)
      end do
      write(o_unit,fmt="(' </grid data>')")
      write(o_unit,fmt="(' <grid data> |',i5,2(i3),f7.3,3('|',a))") yr, mo, da, hr, &
            'Soil Agg. Size for u* to be the thresh. friction velocity', '"effective" mass fraction < 0.84 mm size', '(fract.)'
      do j = jmax-1, 1, -1
        write (o_unit, fmt="(500f12.4)") (cellstate(i,j)%sf84mn, i = 1, imax-1)
      end do
      write(o_unit,fmt="(' </grid data>')")
      write(o_unit,fmt="(' <grid data> |',i5,2(i3),f7.3,3('|',a))") yr, mo, da, hr, &
            'Mobile soil removable from aggregated surface', 'mass removable', '(kg/m^2)'
      do j = jmax-1, 1, -1
        write (o_unit, fmt="(500f12.4)") (cellstate(i,j)%smaglos, i = 1, imax-1)
      end do
      write(o_unit,fmt="(' </grid data>')")
      write(o_unit,fmt="(' <grid data> |',i5,2(i3),f7.3,3('|',a))") yr, mo, da, hr, &
            'Change in mobile soil on aggregated surface', 'net mass change', '(kg/m^2)'
      do j = jmax-1, 1, -1
        write (o_unit, fmt="(500f12.4)") (cellstate(i,j)%dmlos, i = 1, imax-1)
      end do
      write(o_unit,fmt="(' </grid data>')")

      ! Crust properties
      write(o_unit,fmt="(' <grid data> |',i5,2(i3),f7.3,3('|',a))") yr, mo, da, hr, &
             'Consolidated crust thickness', 'crust thickness', '(mm)'
      do j = jmax-1, 1, -1
        write (o_unit, fmt="(500f12.4)") (cellstate(i,j)%szcr, i = 1, imax-1)
      end do
      write(o_unit,fmt="(' </grid data>')")
      write(o_unit,fmt="(' <grid data> |',i5,2(i3),f7.3,3('|',a))") yr, mo, da, hr, &
            'Fraction of Surface covered with Crust', 'crust cover','(fract.)'
      do j = jmax-1, 1, -1
        write (o_unit, fmt="(500f12.4)") (cellstate(i,j)%sfcr, i = 1, imax-1)
      end do
      write(o_unit,fmt="(' </grid data>')")
      write(o_unit,fmt="(' <grid data> |',i5,2(i3),f7.3,3('|',a))") yr, mo, da, hr, &
            'Fraction of Crusted Surface covered with Loose Erodible Soil', 'loose erodible material', '(fract.)'
      do j = jmax-1, 1, -1
        write (o_unit, fmt="(500f12.4)") (cellstate(i,j)%sflos, i = 1, imax-1)
      end do
      write(o_unit,fmt="(' </grid data>')")
      write(o_unit,fmt="(' <grid data> |',i5,2(i3),f7.3,3('|',a))") yr, mo, da, hr, &
           'Mass of Loose Erodible Soil on Crusted Surface', 'loose erodible material', '(kg/m^2)'
      do j = jmax-1, 1, -1
        write (o_unit, fmt="(500f12.4)") (cellstate(i,j)%smlos, i = 1, imax-1)
      end do
      write(o_unit,fmt="(' </grid data>')")

      ! input fluxes
      write(o_unit,fmt="(' <grid data> |',i5,2(i3),f7.3,3('|',a))") yr, mo, da, hr, &
           'Cell Total Input Flux', 'total input flux', '(kg/m/s)'
      do j = jmax-1, 1, -1
        write (o_unit, fmt="(500f12.4)") (cellstate(i,j)%qi, i = 1, imax-1)
      end do
      write(o_unit,fmt="(' </grid data>')")

      write(o_unit,fmt="(' <grid data> |',i5,2(i3),f7.3,3('|',a))") yr, mo, da, hr, &
           'Cell Saltation/Creep Input Flux', 'saltation/creep input flux', '(kg/m/s)'
      do j = jmax-1, 1, -1
        write (o_unit, fmt="(500f12.4)") (cellstate(i,j)%qcsi, i = 1, imax-1)
      end do
      write(o_unit,fmt="(' </grid data>')")

      write(o_unit,fmt="(' <grid data> |',i5,2(i3),f7.3,3('|',a))") yr, mo, da, hr, &
           'Cell Suspension Input Flux', 'suspension input flux', '(kg/m/s)'
      do j = jmax-1, 1, -1
        write (o_unit, fmt="(500f12.4)") (cellstate(i,j)%qssi, i = 1, imax-1)
      end do
      write(o_unit,fmt="(' </grid data>')")

      write(o_unit,fmt="(' <grid data> |',i5,2(i3),f7.3,3('|',a))") yr, mo, da, hr, &
           'Cell PM10 Input Flux', 'pm10 input flux', '(kg/m/s)'
      do j = jmax-1, 1, -1
        write (o_unit, fmt="(500f12.4)") (cellstate(i,j)%q10i, i = 1, imax-1)
      end do
      write(o_unit,fmt="(' </grid data>')")

      ! output fluxes
      write(o_unit,fmt="(' <grid data> |',i5,2(i3),f7.3,3('|',a))") yr, mo, da, hr, &
           'Cell Total Output Flux', 'total output flux', '(kg/m/s)'
      do j = jmax-1, 1, -1
        write (o_unit, fmt="(500f12.4)") (cellstate(i,j)%qo, i = 1, imax-1)
      end do
      write(o_unit,fmt="(' </grid data>')")

      write(o_unit,fmt="(' <grid data> |',i5,2(i3),f7.3,3('|',a))") yr, mo, da, hr, &
           'Cell Saltation/Creep Output Flux', 'saltation/creep output flux', '(kg/m/s)'
      do j = jmax-1, 1, -1
        write (o_unit, fmt="(500f12.4)") (cellstate(i,j)%qcso, i = 1, imax-1)
      end do
      write(o_unit,fmt="(' </grid data>')")

      write(o_unit,fmt="(' <grid data> |',i5,2(i3),f7.3,3('|',a))") yr, mo, da, hr, &
           'Cell Suspension Output Flux', 'suspension output flux', '(kg/m/s)'
      do j = jmax-1, 1, -1
        write (o_unit, fmt="(500f12.4)") (cellstate(i,j)%qsso, i = 1, imax-1)
      end do
      write(o_unit,fmt="(' </grid data>')")

      write(o_unit,fmt="(' <grid data> |',i5,2(i3),f7.3,3('|',a))") yr, mo, da, hr, &
           'Cell PM10 Output Flux', 'pm10 output flux', '(kg/m/s)'
      do j = jmax-1, 1, -1
        write (o_unit, fmt="(500f12.4)") (cellstate(i,j)%q10o, i = 1, imax-1)
      end do
      write(o_unit,fmt="(' </grid data>')")

      ! delta fluxes
      write(o_unit,fmt="(' <grid data> |',i5,2(i3),f7.3,3('|',a))") yr, mo, da, hr, &
           'Cell Total Flux Change', 'total flux change', '(kg/m/s)'
      do j = jmax-1, 1, -1
        write (o_unit, fmt="(500f12.4)") (cellstate(i,j)%qo-cellstate(i,j)%qi, i = 1, imax-1)
      end do
      write(o_unit,fmt="(' </grid data>')")

      write(o_unit,fmt="(' <grid data> |',i5,2(i3),f7.3,3('|',a))") yr, mo, da, hr, &
           'Cell Saltation/Creep Flux Change', 'saltation/creep flux change', '(kg/m/s)'
      do j = jmax-1, 1, -1
        write (o_unit, fmt="(500f12.4)") ( cellstate(i,j)%qcso-cellstate(i,j)%qcsi, i = 1, imax-1)
      end do
      write(o_unit,fmt="(' </grid data>')")

      write(o_unit,fmt="(' <grid data> |',i5,2(i3),f7.3,3('|',a))") yr, mo, da, hr, &
           'Cell Suspension Flux Change', 'suspension flux change', '(kg/m/s)'
      do j = jmax-1, 1, -1
        write (o_unit, fmt="(500f12.4)") (cellstate(i,j)%qsso-cellstate(i,j)%qssi, i = 1, imax-1)
      end do
      write(o_unit,fmt="(' </grid data>')")

      write(o_unit,fmt="(' <grid data> |',i5,2(i3),f7.3,3('|',a))") yr, mo, da, hr, &
           'Cell PM10 Flux Change', 'pm10 flux change', '(kg/m/s)'
      do j = jmax-1, 1, -1
        write (o_unit, fmt="(500f12.4)") (cellstate(i,j)%q10o-cellstate(i,j)%q10i, i = 1, imax-1)
      end do
      write(o_unit,fmt="(' </grid data>')")

      write (o_unit,'(a)')

      ! initialize avg erosion variable
      do j = 1, imax
         egavg(j) = 0.0
      end do

      ! calc. avg erosion over a given field length
      do i = 1,(imax-1)
         !average over y-direction
         do j = 1, (jmax-1)
            egavg(i) = egavg(i) + cellstate(i,j)%egt/(jmax-1)
         end do
      end do
       !average over x-direction
      do i = 2, (imax-1)
         egavg(i) = ((i-1)*(egavg(i-1))+egavg(i))/i
      end do

      write (o_unit, fmt="(a)", advance="NO") ' egavg = '
      do k = 1, (imax-1)
         write (o_unit, fmt="(f8.2)", advance="NO") egavg(k)
      end do
      write(o_unit,'(a)')
      write (o_unit,*) '----------------------------------------------'

   end subroutine sb2out

   subroutine sbemit (ounit, ws, hhr, cellstate, first_emit)

!     To calc the emissions for each time step of the input wind speed
!     The emissions for EPA are the suspension component
!      with units kg m-2 s-1.
!
!     Instructions & logic:
!     To get ntstep period emissions output on erosion days:
!       user assigns am0efl bit 2 in WEPS configuration screen
!       to print (hourly) Weps emissions on erosion days.
!       or
!       user sets ae0efl (print flg)=4 in stand_alone input file
!           EROSION opens emit.out file, calls sbemit to write headings
!           & sets  ae0efl to 99, then calls sbemit
!            to print period emissions for an erosion day.

      use datetime_mod, only: get_systime_string, caldat
      use erosion_data_struct_defs, only: cellsurfacestate, ntstep
      use grid_mod, only: imax, jmax

!     +++ ARGUMENT DECLARATIONS +++
      integer        ounit   !Unit number for detail grid erosion
      real           ws, hhr
      type(cellsurfacestate), dimension(0:,0:), intent(inout) :: cellstate     ! initialized grid cell state values
      logical, intent(inout) :: first_emit   ! indicates entry of emit from erosion for the first time this day

!     +++ LOCAL VARIABLES +++
      character(len=21) :: rundatetime

      integer j,i
      integer yr, mo, da
      save    yr, mo, da
      real    tims, aegtp, aegtcsp, aegtssp, aegt10p, aegt2_5p
      save    tims, aegtp, aegtcsp, aegtssp, aegt10p, aegt2_5p
 !     real    hr
 !     save    hr
      real    aegt, aegtcs, aegtss, aegt10, aegt2_5 ! these have local scope
      real    emittot, emitcs, emitss, emit10, emit2_5, tt

!     +++ OUTPUT FORMATS +++

  100 format (1x,'  yr  mo  day     hr  ws  emission (kg m-2 s-1)')
  110 format (22x,'        total    salt/creep    susp      PM10       PM2_5')
  120 format (1x,3(i4),F7.3,F6.2, 5(1x,F11.8))

!     +++ END SPECIFICATIONS +++

!     set initial conditions

      if( first_emit ) then

          tims = 3600*24/ntstep !seconds in each emission period

          call caldat( mksaeout%jday, da, mo, yr) ! Set day, month and year

          write (ounit,*) 'SBEMIT output'
!          write (ounit,*) 'Suspended emissions < 0.10 mm dia.'
          write (ounit,'(a)')

          ! Print date of Run
          rundatetime = get_systime_string() ! with Lahey f95, had to assign to variable first
          write(ounit,"(1x,'Date of run: ',a21)") rundatetime
          write (ounit,'(a)')

          write (ounit,100)
          write (ounit,110) 
          write (ounit,'(a)')

          ! init prev erosion hr values to zero if this is new erosion day
          first_emit = .false.
          aegtp   = 0.0
          aegtcsp = 0.0
          aegtssp = 0.0
          aegt10p = 0.0
          aegt2_5p = 0.0
      endif   

      !write(0,*) 'Subsequent ntstep is: ', ntstep, tims, tims/3600

      call caldat( mksaeout%jday, da, mo, yr) ! Set day, month and year

      aegt   = 0.0
      aegtcs = 0.0
      aegtss = 0.0
      aegt10 = 0.0
      aegt2_5 = 0.0

      do  j=1,jmax-1
         do  i= 1, imax-1
            aegt= aegt + cellstate(i,j)%egt
            aegtcs = aegtcs + cellstate(i,j)%egtcs
            aegtss = aegtss + cellstate(i,j)%egtss
            aegt10 = aegt10 + cellstate(i,j)%egt10
            aegt2_5 = aegt2_5 + cellstate(i,j)%egt2_5
         enddo
      enddo

      tt     = (imax-1)*(jmax-1)
      aegt   = - aegt/tt     ! change signs to positive=emission
      aegtcs = - aegtcs/tt
      aegtss = - aegtss/tt
      aegt10 = - aegt10/tt
      aegt2_5 = - aegt2_5/tt

      emittot = (aegt - aegtp)/tims
      emitcs  = (aegtcs - aegtcsp)/tims
      emitss  = (aegtss - aegtssp)/tims
      emit10  = (aegt10 - aegt10p)/tims
      emit2_5  = (aegt2_5 - aegt2_5p)/tims

!     Save prior hour average emission
      aegtp   =  aegt
      aegtcsp =  aegtcs
      aegtssp =  aegtss
      aegt10p =  aegt10
      aegt2_5p =  aegt2_5

!     Write to emit.out file
      write (ounit,120) yr, mo, da, hhr, ws, emittot, emitcs, emitss, emit10, emit2_5

   end subroutine sbemit

end module sae_in_out_mod

