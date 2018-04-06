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

  ! placed here for sharing back with hagen_plot_flag by daily_erodout
  real :: aegt, aegtss, aegt10, aegt2_5
  logical :: in_weps

  contains

      subroutine saeinp( luo_saeinp, subrsurf )

!     +++ PURPOSE +++
!     print out input file for stand alone erosion

!     + + + Modules Used + + +
      use datetime_mod, only: caldat
      use file_io_mod, only: fopenk, makenamnum
      use climate_input_mod, only: amalat, amalon
      use grid_mod, only: amxsim, amasim, xgdpt, ygdpt
      use subregions_mod
      use barriers_mod, only: barrier
      use erosion_data_struct_defs, only: subregionsurfacestate, awzypt, awdair, anemht, awzzo, wzoflg, &
                                          awadir, subday, ntstep
      use sweep_io_xml_defs
      use read_write_xml_mod, only: w_begin_tag, w_end_tag, w_whole_tag

!     +++ ARGUMENT DECLARATIONS +++
      integer, intent(inout) :: luo_saeinp      ! output unit number
      type(subregionsurfacestate), dimension(0:), intent(in) :: subrsurf  ! subregion surface conditions (erosion specific set)

!     +++ LOCAL VARIABLES +++
      integer k,l, sr, ip
      integer b, nbr, nacctr, nsubr
      integer day, mon, yr
      integer :: dealloc_stat
      integer :: ipool   ! index of biomass pool tag being written

!     + + + LOCAL VARIABLE DEFINITIONS + + +
!     sr - index used in subregion loop
!     ip - index to polygon coordinates

!     +++ END SPECIFICATIONS +++

      if( luo_saeinp .lt. 0 ) then
        call fopenk (luo_saeinp, trim(mksaeinp%fullpath) // makenamnum('saeros', mksaeinp%simday, mksaeinp%maxday, '.in'),'unknown')
        call caldat (mksaeinp%jday,day,mon,yr)
        write(*,'(4(a,i0))') 'Made SWEEP input file D/M/Y: ', day,'/', mon,'/', yr,' simulation day: ', mksaeinp%simday
        ! XML header
        write(luo_saeinp,"(a)") '<?xml version="1.0" encoding="ISO-8859-1"?>'
        write(luo_saeinp,"(a)") '<!DOCTYPE sweepData SYSTEM "sweep.dtd">'
      else
        write(luo_saeinp,*) '      REPORT OF INPUTS (read by erodin.for) '
      end if

      call init_input_xml()

      call w_begin_tag( luo_saeinp, input_tag(sweepData)%name )
        call w_begin_tag( luo_saeinp, 'WEPS_date' )
          call w_whole_tag( luo_saeinp, 'WEPS_day', day )
          call w_whole_tag( luo_saeinp, 'WEPS_month', mon )
          call w_whole_tag( luo_saeinp, 'WEPS_year', yr )
          call w_whole_tag( luo_saeinp, 'WEPS_SimulationDay', mksaeinp%simday )
        call w_end_tag( luo_saeinp, 'WEPS_date' )
        call w_whole_tag( luo_saeinp, input_tag(GUI_lat)%name, amalat )
        call w_whole_tag( luo_saeinp, input_tag(GUI_lon)%name, amalon )
        call w_whole_tag( luo_saeinp, input_tag(SCI_XOrigin)%name, amxsim(1)%x )
        call w_whole_tag( luo_saeinp, input_tag(SCI_YOrigin)%name, amxsim(1)%y )
        call w_whole_tag( luo_saeinp, input_tag(SCI_XLength)%name, amxsim(2)%x )
        call w_whole_tag( luo_saeinp, input_tag(SCI_YLength)%name, amxsim(2)%y )
        call w_whole_tag( luo_saeinp, input_tag(SCI_RegionAngle)%name, amasim )
        call w_whole_tag( luo_saeinp, input_tag(SCI_XGrid)%name, xgdpt )
        call w_whole_tag( luo_saeinp, input_tag(SCI_YGrid)%name, ygdpt )

        nacctr = size(acct_poly)
        if ( nacctr .gt. 0 ) then
          call w_begin_tag( luo_saeinp, input_tag(SCI_Accounts)%name, &
                                        input_tag(SCI_number)%name, nacctr)
          ! loop through all accounting regions
          do sr = 1, nacctr                ! NOTE: index adjusted to zero based
            call w_begin_tag( luo_saeinp, input_tag(SCI_Account)%name, &
                                          input_tag(SCI_index)%name, sr-1)
              ! number of coordinate pairs in polygon
              call w_begin_tag( luo_saeinp, input_tag(SCI_coords)%name, &
                                            input_tag(SCI_number)%name, acct_poly(sr)%np)
              ! the coordinate pairs
              do ip = 1, acct_poly(sr)%np    ! NOTE: index adjusted to zero based
                call w_begin_tag( luo_saeinp, input_tag(SCI_coord)%name, &
                                              input_tag(SCI_index)%name, ip-1)
                  call w_whole_tag( luo_saeinp, input_tag(SCI_x)%name, acct_poly(sr)%points(ip)%x )
                  call w_whole_tag( luo_saeinp, input_tag(SCI_y)%name, acct_poly(sr)%points(ip)%y )
                call w_end_tag( luo_saeinp, input_tag(SCI_coord)%name )
              end do
              call w_end_tag( luo_saeinp, input_tag(SCI_coords)%name )
            call w_end_tag( luo_saeinp, input_tag(SCI_Account)%name )
          end do
          call w_end_tag( luo_saeinp, input_tag(SCI_Accounts)%name )
        end if
  
        ! subregions
        nsubr = size(subr_poly)
        call w_begin_tag( luo_saeinp, input_tag(SCI_Subregions)%name, &
                                      input_tag(SCI_number)%name, nsubr)
        ! loop through all subregions
        do sr = 1, nsubr                ! NOTE: index adjusted to zero based
          call w_begin_tag( luo_saeinp, input_tag(SCI_Subregion)%name, &
                                        input_tag(SCI_index)%name, sr-1)
            call w_begin_tag( luo_saeinp, input_tag(SCI_coords)%name, &
                                          input_tag(SCI_number)%name, subr_poly(sr)%np)
            ! the coordinate pairs
            do ip = 1, subr_poly(sr)%np    ! NOTE: index adjusted to zero based
              call w_begin_tag( luo_saeinp, input_tag(SCI_coord)%name, &
                                            input_tag(SCI_index)%name, ip-1)
                call w_whole_tag( luo_saeinp, input_tag(SCI_x)%name, subr_poly(sr)%points(ip)%x )
                call w_whole_tag( luo_saeinp, input_tag(SCI_y)%name, subr_poly(sr)%points(ip)%y )
              call w_end_tag( luo_saeinp, input_tag(SCI_coord)%name )
            end do
            call w_end_tag( luo_saeinp, input_tag(SCI_coords)%name )

            call w_whole_tag( luo_saeinp, input_tag(SCI_BiomassRsai)%name, subrsurf(sr)%abrsai )
            call w_whole_tag( luo_saeinp, input_tag(SCI_BiomassRlai)%name, subrsurf(sr)%abrlai )
            call w_whole_tag( luo_saeinp, input_tag(SCI_BiomassHeight)%name, subrsurf(sr)%abzht )
            call w_whole_tag( luo_saeinp, input_tag(SCI_BiomassFlatCover)%name, subrsurf(sr)%abffcv )
            call w_whole_tag( luo_saeinp, input_tag(SCI_CrustCover)%name, subrsurf(sr)%asfcr )
            call w_whole_tag( luo_saeinp, input_tag(SCI_CrustThick)%name, subrsurf(sr)%aszcr )
            call w_whole_tag( luo_saeinp, input_tag(SCI_CrustFracCoverLoose)%name, subrsurf(sr)%asflos )
            call w_whole_tag( luo_saeinp, input_tag(SCI_CrustMassCoverLoose)%name, subrsurf(sr)%asmlos )
            call w_whole_tag( luo_saeinp, input_tag(SCI_CrustDensity)%name, subrsurf(sr)%asdcr )
            call w_whole_tag( luo_saeinp, input_tag(SCI_CrustStability)%name, subrsurf(sr)%asecr )
            call w_whole_tag( luo_saeinp, input_tag(SCI_RandomRoughness)%name, subrsurf(sr)%aslrr )
            call w_whole_tag( luo_saeinp, input_tag(SCI_RidgeHeight)%name, subrsurf(sr)%aszrgh )
            call w_whole_tag( luo_saeinp, input_tag(SCI_RidgeSpacing)%name, subrsurf(sr)%asxrgs )
            call w_whole_tag( luo_saeinp, input_tag(SCI_RidgeWidth)%name, subrsurf(sr)%asxrgw )
            call w_whole_tag( luo_saeinp, input_tag(SCI_RidgeOrientation)%name, subrsurf(sr)%asargo )
            call w_whole_tag( luo_saeinp, input_tag(SCI_DikeSpacing)%name, subrsurf(sr)%asxdks )
            call w_whole_tag( luo_saeinp, input_tag(SCI_SnowDepth)%name, subrsurf(sr)%ahzsnd )

            ! brcd Inputs (biomass by pool)
            call w_begin_tag( luo_saeinp, input_tag(SCI_BrcdInputs)%name, &
                                          input_tag(SCI_number)%name, subrsurf(sr)%npools)
              do ipool = 1, subrsurf(sr)%npools
                call w_begin_tag( luo_saeinp, input_tag(SCI_brcdInput)%name, &
                                            input_tag(SCI_index)%name, ipool-1)
                  call w_whole_tag( luo_saeinp, input_tag(SCI_brcdRlai)%name, subrsurf(sr)%brcdInput(ipool)%rlai )
                  call w_whole_tag( luo_saeinp, input_tag(SCI_brcdRsai)%name, subrsurf(sr)%brcdInput(ipool)%rsai )
                  call w_whole_tag( luo_saeinp, input_tag(SCI_brcdRg)%name, subrsurf(sr)%brcdInput(ipool)%rg )
                  call w_whole_tag( luo_saeinp, input_tag(SCI_brcdXrow)%name, subrsurf(sr)%brcdInput(ipool)%xrow )
                  call w_whole_tag( luo_saeinp, input_tag(SCI_brcdZht)%name, subrsurf(sr)%brcdInput(ipool)%zht )
                call w_end_tag( luo_saeinp, input_tag(SCI_brcdInput)%name )
              end do
            call w_end_tag( luo_saeinp, input_tag(SCI_BrcdInputs)%name )

            ! soil
            call w_begin_tag( luo_saeinp, input_tag(SCI_SoilLays)%name, &
                                          input_tag(SCI_number)%name, subrsurf(sr)%nslay)
            ! the coordinate pairs
            do l = 1, subrsurf(sr)%nslay    ! NOTE: index adjusted to zero based
              call w_begin_tag( luo_saeinp, input_tag(SCI_SoilLay)%name, &
                                            input_tag(SCI_index)%name, l-1)
                call w_whole_tag( luo_saeinp, input_tag(SCI_LayerThickness)%name, subrsurf(sr)%bsl(l)%aszlyt )
                call w_whole_tag( luo_saeinp, input_tag(SCI_BulkDensity)%name, subrsurf(sr)%bsl(l)%asdblk )
                call w_whole_tag( luo_saeinp, input_tag(SCI_Sand)%name, subrsurf(sr)%bsl(l)%asfsan )
                call w_whole_tag( luo_saeinp, input_tag(SCI_VeryFineSand)%name, subrsurf(sr)%bsl(l)%asfvfs )
                call w_whole_tag( luo_saeinp, input_tag(SCI_Silt)%name, subrsurf(sr)%bsl(l)%asfsil )
                call w_whole_tag( luo_saeinp, input_tag(SCI_Clay)%name, subrsurf(sr)%bsl(l)%asfcla )
                call w_whole_tag( luo_saeinp, input_tag(SCI_RockVolume)%name, subrsurf(sr)%bsl(l)%asvroc )
                call w_whole_tag( luo_saeinp, input_tag(SCI_AggregateDensity)%name, subrsurf(sr)%bsl(l)%asdagd )
                call w_whole_tag( luo_saeinp, input_tag(SCI_AggregateStability)%name, subrsurf(sr)%bsl(l)%aseags )
                call w_whole_tag( luo_saeinp, input_tag(SCI_AggregateGMD)%name, subrsurf(sr)%bsl(l)%aslagm )
                call w_whole_tag( luo_saeinp, input_tag(SCI_AggregateGSD)%name, subrsurf(sr)%bsl(l)%as0ags )
                call w_whole_tag( luo_saeinp, input_tag(SCI_AggregateMIN)%name, subrsurf(sr)%bsl(l)%aslagn )
                call w_whole_tag( luo_saeinp, input_tag(SCI_AggregateMAX)%name, subrsurf(sr)%bsl(l)%aslagx )
                call w_whole_tag( luo_saeinp, input_tag(SCI_WiltingPoint)%name, subrsurf(sr)%bsl(l)%ahrwcw )
                call w_whole_tag( luo_saeinp, input_tag(SCI_WaterContent)%name, subrsurf(sr)%bsl(l)%ahrwca )
              call w_end_tag( luo_saeinp, input_tag(SCI_SoilLay)%name )
            end do
            call w_end_tag( luo_saeinp, input_tag(SCI_SoilLays)%name )
            call w_begin_tag( luo_saeinp, input_tag(SCI_SurfaceSubDayWaters)%name, &
                                          input_tag(SCI_number)%name, subrsurf(sr)%nswet)
            do l = 1, subrsurf(sr)%nswet    ! NOTE: index adjusted to zero based
              call w_whole_tag( luo_saeinp, input_tag(SCI_SurfaceSubDayWater)%name, &
                                            input_tag(SCI_index)%name, l-1, &
                                            subrsurf(sr)%ahrwc0(l) )
            end do
            call w_end_tag( luo_saeinp, input_tag(SCI_SurfaceSubDayWaters)%name )
          call w_end_tag( luo_saeinp, input_tag(SCI_Subregion)%name )
        end do
        call w_end_tag( luo_saeinp, input_tag(SCI_Subregions)%name )

        ! barriers
        nbr = size(barrier)
        if ( nbr .gt. 0 ) then
          call w_begin_tag( luo_saeinp, input_tag(SCI_Barriers)%name, &
                                        input_tag(SCI_number)%name, nbr)
          ! loop through all accounting regions
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
                                        input_tag(SCI_index)%name, k-1, &
                                        subday(k)%awu )
        end do
        call w_end_tag( luo_saeinp, input_tag(SCI_WindSpeeds)%name )
      call w_end_tag( luo_saeinp, input_tag(sweepData)%name )

      close(luo_saeinp)

      ! deallocate Tag array
      deallocate( input_tag, stat=dealloc_stat)
      if( dealloc_stat .gt. 0 ) then
        ! deallocation failed
        write(*,*) "ERROR: unable to deallocate memory for Tag array"
      end if

   end subroutine saeinp

   subroutine daily_erodout( o_unit, o_E_unit, sgrd_u, input_filename, cellstate )

!     +++  PURPOSE +++
!     To print output desired from standalone EROSION submodel

      use file_io_mod, only: fopenk, makenamnum
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
      real topt,topss, top10, top2_5, bott, botss, bot10, bot2_5
      real ritt, ritss, rit10, rit2_5, lftt, lftss, lft10, lft2_5
      real tot, totbnd

      integer yr, mon, day

!     +++ END SPECIFICATIONS +++

!     Calculate Averages Crossing Borders
!      top border
       aegt   = 0.0
       aegtss = 0.0
       aegt10 = 0.0
       aegt2_5 = 0.0
       j = jmax
       do 1 i = 1, imax-1
         aegt    = aegt   + cellstate(i,j)%egt
         aegtss  = aegtss + cellstate(i,j)%egtss
         aegt10  = aegt10 + cellstate(i,j)%egt10
         aegt2_5 = aegt2_5 + cellstate(i,j)%egt2_5
    1  continue
!      calc. average at top border
       topt  = aegt/(imax-1)
       topss = aegtss/(imax-1)
       top10 = aegt10/(imax-1)
       top2_5 = aegt2_5/(imax-1)

!      bottom border
       aegt   = 0.0
       aegtss = 0.0
       aegt10 = 0.0
       aegt2_5 = 0.0
       j = 0
       do 2 i = 1, imax-1
         aegt    = aegt   + cellstate(i,j)%egt
         aegtss  = aegtss + cellstate(i,j)%egtss
         aegt10  = aegt10 + cellstate(i,j)%egt10
         aegt2_5 = aegt2_5 + cellstate(i,j)%egt2_5
    2  continue
!      calc. average at bottom border
        bott  = aegt/(imax-1)
        botss = aegtss/(imax-1)
        bot10 = aegt10/(imax-1)
        bot2_5 = aegt2_5/(imax-1)

!     right border
       aegt   = 0.0
       aegtss = 0.0
       aegt10 = 0.0
       aegt2_5 = 0.0
       i = imax
       do 3 j = 1, jmax-1
         aegt    = aegt   + cellstate(i,j)%egt
         aegtss  = aegtss + cellstate(i,j)%egtss
         aegt10  = aegt10 + cellstate(i,j)%egt10
         aegt2_5 = aegt2_5 + cellstate(i,j)%egt2_5
    3  continue
!      calc. average at right border
        ritt  = aegt/(jmax-1)
        ritss = aegtss/(jmax-1)
        rit10 = aegt10/(jmax-1)
        rit2_5 = aegt2_5/(jmax-1)

!     left border
       aegt   = 0.0
       aegtss = 0.0
       aegt10 = 0.0
       aegt2_5 = 0.0
       i = 0
       do 4 j = 1, jmax-1
         aegt    = aegt   + cellstate(i,j)%egt
         aegtss  = aegtss + cellstate(i,j)%egtss
         aegt10  = aegt10 + cellstate(i,j)%egt10
         aegt2_5 = aegt2_5 + cellstate(i,j)%egt2_5
    4  continue
!      calc. average at left border
        lftt   = aegt/(jmax-1)
        lftss  = aegtss/(jmax-1)
        lft10  = aegt10/(jmax-1)
        lft2_5  = aegt2_5/(jmax-1)

!     calculate averages of inner grid points
      aegt   = 0.0
      aegtss = 0.0
      aegt10 = 0.0
      do 5 j=1,jmax-1
       do 5 i= 1, imax-1
        aegt= aegt + cellstate(i,j)%egt
        aegtss = aegtss + cellstate(i,j)%egtss
        aegt10 = aegt10 + cellstate(i,j)%egt10
        aegt2_5 = aegt2_5 + cellstate(i,j)%egt2_5
    5 continue
      tt     = (imax-1)*(jmax-1)
      aegt   = aegt/tt
      aegtss = aegtss/tt
      aegt10 = aegt10/tt
      aegt2_5 = aegt2_5/tt

!    calculate comparison of boundary and interior losses
      lx = amxsim(2)%x - amxsim(1)%x
      ly = amxsim(2)%y - amxsim(1)%y
      tot = aegt*lx*ly
      totbnd = (topt + bott + topss + botss)*lx + (ritt + lftt + ritss + lftss)*ly


      if (btest(am0efl,1)) then

      if( o_unit .lt. 0 ) then
        call fopenk (o_unit, trim(mksaeout%fullpath) // makenamnum('saeros', mksaeout%simday, mksaeout%maxday, '.egrd'),'unknown')
        call caldat (mksaeout%jday,day,mon,yr)
        write(*,'(4(a,i0))') 'Made Daily Erosion grid file for: ', day,'/', mon,'/', yr,' simulation day: ', mksaeout%simday
        write(o_unit, "('# WEPS erosion day mon yr',2(1x,i2),2x,i4)") day, mon, yr
        write (o_unit,*)
        write (o_unit,*) 'Grid cell output from WEPS run'
        write (o_unit,*)
      else
        ! write header to files
        write (o_unit,*)
        write (o_unit,*)
        write (o_unit,*) 'Grid cell output from SWEEP run'
        write (o_unit,*)
      end if

      ! Print date of Run
      rundatetime = get_systime_string() ! with Lahey f95, had to assign to variable first
      write(o_unit,"(1x,'Date of run: ',a21)") rundatetime
      write(o_unit,*)

      write(o_unit,fmt="(1x,a)") "<field dimensions>"
      write(o_unit,fmt="(1x,5f10.2)") amasim, amxsim(1)%x, amxsim(1)%y, amxsim(2)%x, amxsim(2)%y
      write(o_unit,fmt="(1x,a)") "</field dimensions>"
      write(o_unit,*)
      write (o_unit,*) 'Total grid size: (', imax+1,',', jmax+1, ')   ',&
                       'Inner grid size: (', imax-1,',', jmax-1, ')'

      write (o_unit,*)
      write (o_unit,6)
      write (o_unit,*) '  top(i=1,imax-1,j=jmax) ', &
                       'bottom(i=1,imax-1,j=0) ', &
                       'right(i=imax,j=1,jmax-1) ', &
                       'left(i=0, j=1,jmax-1) '
      write (o_unit,10)  (cellstate(i,jmax)%egt+cellstate(i,jmax)%egtss, i = 1, imax-1)
      write (o_unit,10)  (cellstate(i,0)%egt+cellstate(i,0)%egtss, i = 1, imax-1)
      write (o_unit,10)  (cellstate(imax,j)%egt+cellstate(imax,j)%egtss, j = 1, jmax-1)
      write (o_unit,10)  (cellstate(0,j)%egt+cellstate(0,j)%egtss, j = 1, jmax-1)

      write (o_unit,*)
      write (o_unit,7)
      write (o_unit,*) '  top(i=1,imax-1,j=jmax) ', &
                       'bottom(i=1,imax-1,j=0) ', &
                       'right(i=imax,j=1,jmax-1) ', &
                       'left(i=0, j=1,jmax-1) '
      write (o_unit,10)  (cellstate(i,jmax)%egt, i = 1, imax-1)
      write (o_unit,10)  (cellstate(i,0)%egt, i = 1, imax-1)
      write (o_unit,10)  (cellstate(imax,j)%egt, j = 1, jmax-1)
      write (o_unit,10)  (cellstate(0,j)%egt, j = 1, jmax-1)

      write (o_unit,*)
      write (o_unit,8)
      write (o_unit,*) '  top(i=1,imax-1,j=jmax) ', &
                       'bottom(i=1,imax-1,j=0) ', &
                       'right(i=imax,j=1,jmax-1) ', &
                       'left(i=0, j=1,jmax-1) '
      write (o_unit,10)  (cellstate(i,jmax)%egtss, i = 1, imax-1)
      write (o_unit,10)  (cellstate(i,0)%egtss, i = 1, imax-1)
      write (o_unit,10)  (cellstate(imax,j)%egtss, j = 1, jmax-1)
      write (o_unit,10)  (cellstate(0,j)%egtss, j = 1, jmax-1)

      write (o_unit,*)
      write (o_unit,9)
      write (o_unit,*) '  top(i=1,imax-1,j=jmax) ', &
                       'bottom(i=1,imax-1,j=0) ', &
                       'right(i=imax,j=1,jmax-1) ', &
                       'left(i=0,j=1,jmax-1) '
      write (o_unit,11)  (cellstate(i,jmax)%egt10, i = 1, imax-1)
      write (o_unit,11)  (cellstate(i,0)%egt10, i = 1, imax-1)
      write (o_unit,11)  (cellstate(imax,j)%egt10, j = 1, jmax-1)
      write (o_unit,11)  (cellstate(0,j)%egt10, j = 1, jmax-1)

      write (o_unit,*)
      write (o_unit,12)
      write (o_unit,*) '  top(i=1,imax-1,j=jmax) ', &
                       'bottom(i=1,imax-1,j=0) ', &
                       'right(i=imax,j=1,jmax-1) ', &
                       'left(i=0,j=1,jmax-1) '
      write (o_unit,11)  (cellstate(i,jmax)%egt2_5, i = 1, imax-1)
      write (o_unit,11)  (cellstate(i,0)%egt2_5, i = 1, imax-1)
      write (o_unit,11)  (cellstate(imax,j)%egt2_5, j = 1, jmax-1)
      write (o_unit,11)  (cellstate(0,j)%egt2_5, j = 1, jmax-1)

      write (o_unit,*)
      write (o_unit,fmt="(' <grid data> | ',3('|',a))") 'Total Soil Loss', 'soil loss', '(kg/m^2)'
      do 19  j = jmax-1, 1, -1
      write (o_unit,10)  (cellstate(i,j)%egt, i = 1, imax-1)
   19 continue
      write (o_unit,fmt="(' </grid data>')")

      write (o_unit,*)
      write (o_unit,fmt="(' <grid data> | ',3('|',a))") 'Saltation/Creep Soil Loss', 'salt/creep soil loss', '(kg/m^2)'
      do 29  j = jmax-1, 1, -1
      write (o_unit,10)  (cellstate(i,j)%egt-cellstate(i,j)%egtss, i = 1, imax-1)
   29 continue
      write (o_unit,fmt="(' </grid data>')")

      write (o_unit,*)
      write (o_unit,fmt="(' <grid data> | ',3('|',a))") 'Suspension Soil Loss', 'suspension soil loss', '(kg/m^2)'
      do 39  j = jmax-1, 1, -1
      write (o_unit,10)  (cellstate(i,j)%egtss, i = 1, imax-1)
   39 continue
      write (o_unit,fmt="(' </grid data>')")

      write (o_unit,*)
      write (o_unit,fmt="(' <grid data> | ',3('|',a))") 'PM10 Soil Loss', 'PM10 soil loss', '(kg/m^2)'
      do 49  j = jmax-1, 1, -1
      write (o_unit,11)  (cellstate(i,j)%egt10, i = 1, imax-1)
   49 continue
      write (o_unit,fmt="(' </grid data>')")

      write (o_unit,*)
      write (o_unit,fmt="(' <grid data> | ',3('|',a))") 'PM2_5 Soil Loss', 'PM2_5 soil loss', '(kg/m^2)'
      do 59  j = jmax-1, 1, -1
      write (o_unit,11)  (cellstate(i,j)%egt2_5, i = 1, imax-1)
   59 continue
      write (o_unit,fmt="(' </grid data>')")

      write (o_unit,*)
      write (o_unit,*) '**Averages - Field'
      write (o_unit,*) '     Total    salt/creep      susp       PM10        PM2.5'
      write (o_unit,*) '     egt                      egtss      egt10       egt2_5'
      write (o_unit,*) '   -----------------------kg/m^2---------------------------'
      write (o_unit,15)    aegt, aegt-aegtss, aegtss, aegt10, aegt2_5
      write (o_unit,*)
      write (o_unit,*) '**Averages - Crossing Boundaries '
      write (o_unit,*) 'Location      Total  Salt/Creep   Susp    PM10     PM2_5'
      write (o_unit,*) '-------------------------kg/m---------------------------'
      write (o_unit,21) topt+topss, topt, topss, top10, top2_5
      write (o_unit,22) bott+botss, bott, botss, bot10, bot2_5
      write (o_unit,23) ritt+ritss, ritt, ritss, rit10, rit2_5
      write (o_unit,24) lftt+lftss, lftt, lftss, lft10, lft2_5
      write (o_unit,*)
      write (o_unit,*) '   Comparison of interior & boundary loss'
      write (o_unit,*) '      interior       boundary    int/bnd ratio'
      if( totbnd.gt.1.0e-9 ) then
          write (o_unit,16) tot, totbnd, tot/totbnd
      else
          !Boundary loss near or equal to zero
          write (o_unit,16) tot, totbnd, 1.0e-9
      end if

!     additional output statements for easy shell script parsing
      write (o_unit,*)
!     write losses as positive numbers
      write (o_unit,17) -aegt, aegtss-aegt, -aegtss, -aegt10, -aegt2_5
   17 format (' repeat of total, salt/creep, susp, PM10, PM2.5:', 3f12.4,3f12.6)

      close(o_unit)

!     output formats
    6 format (1x,'  Passing Border Grid Cells - Total  egt+egtss(kg/m)')
    7 format (1x,'  Passing Border Grid Cells - Salt/Creep   egt(kg/m)')
    8 format (1x,'  Passing Border Grid Cells - Suspension egtss(kg/m)')
    9 format (1x,'  Passing Border Grid Cells - PM10       egt10(kg/m)')
   12 format (1x,'  Passing Border Grid Cells - PM2_5      egt2_5(kg/m)')
   10 format (1x, 500f12.4)
   11 format (1x, 500f12.6)
   15 format (1x, 3(f12.4,2x), 2(f12.6,2x))
   16 format (1x, 2(f13.4,2x),2x, f13.4)
   21 format (1x, 'top   ', 1x, 5(f9.2,1x))
   22 format (1x, 'bottom', 1x, 5(f9.2,1x))
   23 format (1x, 'right ', 1x, 5(f9.2,1x))
   24 format (1x, 'left  ', 1x, 5(f9.2,1x))

      end if !if (btest(am0efl,1)) then

      !Erosion summary - total, salt/creep, susp, pm10
      !(loss values are positive - deposition values are negative)
      if (btest(am0efl,0)) then

      if( in_weps ) then
         call caldat (mksaeout%jday,day,mon,yr)
         write(*,'(4(a,i0))') 'Wrote to Daily Erosion summary file: ', day,'/', mon,'/', yr,' simulation day: ', mksaeout%simday
         write (UNIT=o_E_unit,FMT="(5(f12.6),' ')",ADVANCE="NO") -aegt, -(aegt-aegtss), -aegtss, -aegt10, -aegt2_5
         write (UNIT=o_E_unit,FMT="('# WEPS erosion day mon yr',2(1x,i2),2x,i4)",ADVANCE="NO") day, mon, yr
         write (UNIT=o_E_unit,FMT="(A)",ADVANCE="YES") ' (loss values are positive - deposition values are negative)'
      else
         write (UNIT=o_E_unit,FMT="(5(f12.6),' ')",ADVANCE="NO") -aegt, -(aegt-aegtss), -aegtss, -aegt10, -aegt2_5
         write (UNIT=o_E_unit,FMT="(A)",ADVANCE="NO") trim(input_filename)
         write (UNIT=o_E_unit,FMT="(A)",ADVANCE="YES") ' (loss values are positive - deposition values are negative)'
      end if

      end if

      !Duplicate Erosion summary info for the *.sgrd file so "sweep" interface
      ! can display this info on graphical report window
      if (btest(am0efl,3) .and. (sgrd_u .ge. 0) ) then
       write (sgrd_u,*)
       write (sgrd_u,*) '**Averages - Field'
       write (sgrd_u,*) '     Total    salt/creep      susp       PM10        PM2.5'
       write (sgrd_u,*) '     egt                      egtss      egt10       egt2_5'
       write (sgrd_u,*) '   -----------------------kg/m^2---------------------------'
       write (sgrd_u,15)    aegt, aegt-aegtss, aegtss, aegt10, aegt2_5
       write (sgrd_u,*)
       write (sgrd_u,*) '**Averages - Crossing Boundaries '
       write (sgrd_u,*) 'Location      Total  Salt/Creep   Susp    PM10     PM2_5'
       write (sgrd_u,*) '--------------------------kg/m--------------------------'
       write (sgrd_u,21) topt+topss, topt, topss, top10, top2_5
       write (sgrd_u,22) bott+botss, bott, botss, bot10, bot2_5
       write (sgrd_u,23) ritt+ritss, ritt, ritss, rit10, rit2_5
       write (sgrd_u,24) lftt+lftss, lftt, lftss, lft10, lft2_5
       write (sgrd_u,*)
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
      !integer m, n, k
      integer initflag, ipd, npd
      save    initflag, ipd, npd
      integer yr, mo, da
      real    hhrr, tims
      save    yr, mo, da, hhrr, tims
      integer i,j, kbr

!     + + + END SPECIFICATIONS + + +

!     output headings?
      if (initflag .eq. 0) then

        ipd = 0
        npd = nn * ntstep

        tims = 3600*24/ntstep     ! seconds in each emission period
        call caldat( mksaeout%jday, da, mo, yr) ! Set day, month and year
        hhrr = 0 - tims/3600        !Pre-set hhrr so we get end of period times

        write (o_unit,*)
        write (o_unit,*) 'OUT PUT from sb1out'
        write (o_unit,*)

        ! Print date of Run
        rundatetime = get_systime_string() ! with Lahey f95, had to assign to variable first
        write(o_unit,"(1x,'Date of run: ',a21)") rundatetime
        write(o_unit,*)

        write (unit=o_unit,fmt="(a,f5.2,a2,a,i1)") ' anemht = ', anemht, 'm', '    wzoflg = ', wzoflg
        write (unit=o_unit,fmt="(a,f6.2,a4)") ' wind direction = ', wdir, 'deg'
        write (unit=o_unit,fmt="(a,f6.2,a4)") ' wind direction relative to field orientation = ', awa, 'deg'
        write (o_unit,*)

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
        write (o_unit,*)
        ! end inserted section

        write (o_unit,*) 'orientation and dimensions of sim region'
        write (o_unit,*) 'amasim(deg)  amxsim - (x1,y1) (x2,y2)'
        write(o_unit,fmt="(1x,5f8.2)") amasim, amxsim(1)%x, amxsim(1)%y, amxsim(2)%x, amxsim(2)%y
        write (o_unit,*)

       write (o_unit,*) "Surface properties"
      write (o_unit,fmt="(a,f8.2,a)") "Ridge spacing parallel to wind direction", subrsurf%sxprg, " (mm)"
      write (o_unit,fmt="(a,f5.2,a)") "Composite weighted average biomass height", subrsurf%abzht, " (m)"
      write (o_unit,fmt="(a,f5.2,a)") "Biomass leaf area index", subrsurf%abrlai, " (m^2/m^2)"
      write (o_unit,fmt="(a,f5.2,a)") "Biomass stem area index", subrsurf%abrsai, " (m^2/m^2)"
      write (o_unit,fmt="(a,f5.2,a)") "Biomass flat cover", subrsurf%abffcv, " (m^2/m^2)"

      write (o_unit,fmt="(a,f8.2,a)") "Average yearly total precipitation ", awzypt, " (mm)"
      write (o_unit,*)



        write(o_unit,fmt="(1x,a)") "<field dimensions>"
       write(o_unit,fmt="(1x,5f10.2)")amasim, amxsim(1)%x, amxsim(1)%y, amxsim(2)%x, amxsim(2)%y
        write(o_unit,fmt="(1x,a)") "</field dimensions>"

        write (o_unit,*)
        initflag = 1    !     turn off heading output
      endif

      ipd = ipd + 1
      if (hhrr .ge. 24) then
         hhrr = tims/3600
         call caldat( mksaeout%jday, da, mo, yr) ! Set day, month and year
      else
         hhrr = hhrr + tims/3600
      endif

      call caldat( mksaeout%jday, da, mo, yr) ! Set day, month and year
!      write (o_unit, fmt="(a, 3(i3), f6.2, 4(i4)f7.2)") ' day mon yr hhrr upd_pd jj nn npd ',da,mo,yr,hr,ipd,jj,nn,npd,hhrr
      write (o_unit, fmt="(a, i5, 2(i3), f7.3, 4(i4))") ' yr mon day hr upd_pd jj nn(subpd) npd (sbqout 1)', &
                                                          yr,mo, da, hr,ipd,   jj,nn,       npd
      write (o_unit,*)
      write (o_unit, fmt="(a, f5.2, 2(f7.2))") ' pd wind speed, dir and dir rel to field ', ws, wdir, awa
      write (o_unit,*)

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

!Grid cell data

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
      write (o_unit,*)

!Grid Cell Surface properties
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
      write (o_unit,*)

      write(o_unit,fmt="(' <grid data> |',i5,2(i3),f7.3,3('|',a))") yr, mo, da, hr, &
            'Soil Agg. Size<0.01', 'mass fraction < 0.01 mm size', '(fract.)'
      do j = jmax-1, 1, -1
        write (o_unit, fmt="(500f12.4)") (cellstate(i,j)%sf1, i = 1, imax-1)
      end do
      write(o_unit,fmt="(' </grid data>')")
      write(o_unit,fmt="(' <grid data> |',i5,2(i3),f7.3,3('|',a))") yr, mo, da, hr, &
            'Soil Agg. Size<0.1', 'mass fraction < 0.1 mm size', '(fract.)'
      do j = jmax-1, 1, -1
        write (o_unit, fmt="(500f12.4)") (cellstate(i,j)%sf10, i = 1, imax-1)
      end do
      write(o_unit,fmt="(' </grid data>')")
      write(o_unit,fmt="(' <grid data> |',i5,2(i3),f7.3,3('|',a))") yr, mo, da, hr, &
            'Soil Agg. Size<0.84', 'mass fraction < 0.84 mm size', '(fract.)'
      do j = jmax-1, 1, -1
        write (o_unit, fmt="(500f12.4)") (cellstate(i,j)%sf84, i = 1, imax-1)
      end do
      write(o_unit,fmt="(' </grid data>')")
      write(o_unit,fmt="(' <grid data> |',i5,2(i3),f7.3,3('|',a))") yr, mo, da, hr, &
            'Soil Agg. Size<2.0', 'mass fraction < 2.0 mm size', '(fract.)'
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
            'Fraction of Crusted Surface covered with Loose Erodible Soil ', 'loose erodible material', '(fract.)'
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
      write (o_unit,*)

   end subroutine sb1out

   subroutine sb2out (jj, nn, hr, o_unit, cellstate)

!     + + + PURPOSE + + +
!     To print to file tst.out some key variables used in erosion
!     use wind direction of 270 to see output along downwind direction

      use datetime_mod, only: caldat
      use erosion_data_struct_defs, only: cellsurfacestate, ntstep
      use grid_mod, only: imax, jmax

!     + + + ARGUEMENT DECLARATIONS + + +
      real hr
      integer  jj, nn, o_unit
      type(cellsurfacestate), dimension(0:,0:), intent(inout) :: cellstate     ! initialized grid cell state values

!     + + + ARGUMENT DEFINITIONS + + +
!     o_unit= Unit number for output file

!     + + + LOCAL VARIABLES + + +
      real egavg(imax)
      integer m, n, k, icsr
      integer initflag, ipd, npd
      save    initflag, ipd, npd

      integer yr, mo, da
      real    hhrr, tims
      save    yr, mo, da, hhrr, tims
      integer i,j

!     outflag = 0 - print heading output, 1 - no more heading

!     + + + END SPECIFICATIONS + + +

!     define index of current subregions
      icsr = 1

!     output headings?
      if (initflag .eq. 0) then

        ipd = 0
        npd = nn * ntstep

        tims = 3600*24/ntstep !seconds in each emission period
        call caldat( mksaeout%jday, da, mo, yr) ! Set day, month and year
        hhrr = 0                    !Pre-set hhrr so we get start of period times

!        write (o_unit,*)
!        write (o_unit,*) 'OUT PUT from sb2out'
!        write (o_unit,*)

        initflag = 1    !     turn off heading output
      endif
      ipd = ipd + 1
      if (hhrr .ge. 24) then
         hhrr = tims/3600
        call caldat( mksaeout%jday, da, mo, yr) ! Set day, month and year
      else
         hhrr = hhrr + tims/3600
      endif

        call caldat( mksaeout%jday, da, mo, yr) ! Set day, month and year
!      write (o_unit, fmt="(a, 3(i3), f5.2, i3)") &
!             ' day mon yr hhrr update_period ', da, mo, yr, hhrr, jj

      write (o_unit, fmt="(a, i5, 2(i3), f7.3, 4(i4))") &
             ' yr mon day hr upd_pd jj nn(subpd) npd (sbqout 2)', &
               yr,mo, da, hr,ipd,   jj,nn,       npd


      write(o_unit,fmt="(' <grid data> |',i5,2(i3),f7.3,3('|',a))") yr, mo, da, hr, &
             'Cumulative Total Soil Loss', 'soil loss', '(kg/m^2)'
      do j = jmax-1, 1, -1
        write (o_unit, fmt="(500f12.4)") (cellstate(i,j)%egt, i = 1, imax-1)
      end do
      write(o_unit,fmt="(' </grid data>')")

      write(o_unit,fmt="(' <grid data> |',i5,2(i3),f7.3,3('|',a))") yr, mo, da, hr, &
            'Cumulative Saltation/Creep Soil Loss', 'salt/creep soil loss', '(kg/m^2)'
      do j = jmax-1, 1, -1
        write (o_unit, fmt="(500f12.4)")(cellstate(i,j)%egt-cellstate(i,j)%egtss,i=1,imax-1)
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

!!      if (ipd .eq. npd) then
!Grid Cell Surface properties
      write(o_unit,fmt="(' <grid data> |',i5,2(i3),f7.3,3('|',a))") yr, mo, da, hr, &
            'Surface Random Roughness (after)', 'random roughness', '(mm)'
      do j = jmax-1, 1, -1
        write (o_unit, fmt="(500f12.4)") (cellstate(i,j)%slrr, i = 1, imax-1)
      end do
      write(o_unit,fmt="(' </grid data>')")
      write(o_unit,fmt="(' <grid data> |',i5,2(i3),f7.3,3('|',a))") yr, mo, da, hr, &
            'Surface Oriented Roughness (after)', 'ridge height', '(mm)'
      do j = jmax-1, 1, -1
        write (o_unit, fmt="(500f12.4)") (cellstate(i,j)%szrgh, i = 1, imax-1)
      end do
      write(o_unit,fmt="(' </grid data>')")
      write(o_unit,fmt="(' <grid data> |',i5,2(i3),f7.3,3('|',a))") yr, mo, da, hr, &
            'Surface Rock (after)', 'surface volume rock fraction', '(m^3/m^3)'
      do j = jmax-1, 1, -1
        write (o_unit, fmt="(500f12.4)") (cellstate(i,j)%svroc, i = 1, imax-1)
      end do
      write(o_unit,fmt="(' </grid data>')")
      write (o_unit,*)

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
            'Soil Agg. Size for u* to be the thresh. friction velocity (af)','"effective" mass fraction < 0.84 mm size', '(fract.)'
      do j = jmax-1, 1, -1
        write (o_unit, fmt="(500f12.4)") (cellstate(i,j)%sf84mn, i = 1, imax-1)
      end do
      write(o_unit,fmt="(' </grid data>')")
      write(o_unit,fmt="(' <grid data> |',i5,2(i3),f7.3,3('|',a))") yr, mo, da, hr, &
            'Mobile soil removable from aggregated surface (after)', 'mass removable', '(kg/m^2)'
      do j = jmax-1, 1, -1
        write (o_unit, fmt="(500f12.4)") (cellstate(i,j)%smaglos, i = 1, imax-1)
      end do
      write(o_unit,fmt="(' </grid data>')")
      write(o_unit,fmt="(' <grid data> |',i5,2(i3),f7.3,3('|',a))") yr, mo, da, hr, &
            'Change in mobile soil on aggregated surface (after)', 'net mass change', '(kg/m^2)'
      do j = jmax-1, 1, -1
        write (o_unit, fmt="(500f12.4)") (cellstate(i,j)%dmlos, i = 1, imax-1)
      end do
      write(o_unit,fmt="(' </grid data>')")

! Crust properties
      write(o_unit,fmt="(' <grid data> |',i5,2(i3),f7.3,3('|',a))") yr, mo, da, hr, &
             'Consolidated crust thickness (after)', 'crust thickness', '(mm)'
      do j = jmax-1, 1, -1
        write (o_unit, fmt="(500f12.4)") (cellstate(i,j)%szcr, i = 1, imax-1)
      end do
      write(o_unit,fmt="(' </grid data>')")
      write(o_unit,fmt="(' <grid data> |',i5,2(i3),f7.3,3('|',a))") yr, mo, da, hr, &
            'Fraction of Surface covered with Crust (after)', 'crust cover','(fract.)'
      do j = jmax-1, 1, -1
        write (o_unit, fmt="(500f12.4)") (cellstate(i,j)%sfcr, i = 1, imax-1)
      end do
      write(o_unit,fmt="(' </grid data>')")
      write(o_unit,fmt="(' <grid data> |',i5,2(i3),f7.3,3('|',a))") yr, mo, da, hr, &
            'Fraction of Crusted Surface covered with Loose Erodible Soil(a)', 'loose erodible material', '(fract.)'
      do j = jmax-1, 1, -1
        write (o_unit, fmt="(500f12.4)") (cellstate(i,j)%sflos, i = 1, imax-1)
      end do
      write(o_unit,fmt="(' </grid data>')")
      write(o_unit,fmt="(' <grid data> |',i5,2(i3),f7.3,3('|',a))") yr, mo, da, hr, &
           'Mass of Loose Erodible Soil on Crusted Surface (after)', 'loose erodible material', '(kg/m^2)'
      do j = jmax-1, 1, -1
        write (o_unit, fmt="(500f12.4)") (cellstate(i,j)%smlos, i = 1, imax-1)
      end do
      write(o_unit,fmt="(' </grid data>')")
      write (o_unit,*)

!!      endif

!     set output increment
      m = (imax - 1)/8
      m = max0(m,1)
      n = (jmax-1)/2
      n = max(n,1) 

!     initialize avg erosion variable
      do 3 j = 1, imax
        egavg(j) = 0.0
    3 continue

!     calc. avg erosion over a given field length
       do 5  i = 1,(imax-1)
       !average over y-direction
       do 4  j = 1, (jmax-1)
          egavg(i) = egavg(i) + cellstate(i,j)%egt/(jmax-1)
    4  continue
    5  continue
       !average over x-direction
       do 6 i = 2, (imax-1)
         egavg(i) = ((i-1)*(egavg(i-1))+egavg(i))/i
    6  continue

      write (o_unit,35) (egavg(k), k=1,(imax-1))
      write (o_unit,*) '----------------------------------------------'

!     output formats
   35 format (1x, 'egavg = ', 20f6.2)

   end subroutine sb2out

   subroutine sbemit (ounit, ws, hhr, cellstate, first_emit)

!     To calc the emissions for each time step of the input wind speed
!     The emissions for EPA are the suspension component
!      with units kg m-2 s-1.
!     To write out a file in the format:
!      12 blank col, yr, mo, day, hr, soucename, emissionrate
!
!     Instructions & logic:
!     To get ntstep period emissions output on erosion days:
!       user sets am0efl = 3 in WEPS configuration screen
!          subroutine openfils creates output file emit.out
!          EROSION calls sbemit to write heading in emit.out file,
!          & sets am0efl to 98, then calls sbemit
!          to print (hourly) Weps emissions on erosion days.
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
      integer        initflg
      save           initflg

      integer j,i
      integer yr, mo, da
      save    yr, mo, da
      real    tims, aegtp, aegtssp, aegt10p, aegt2_5p
      save    tims, aegtp, aegtssp, aegt10p, aegt2_5p
 !     real    hr
 !     save    hr
      real    aegt, aegtss, aegt10, aegt2_5 ! these have local scope
      real    emittot, emitss, emit10, emit2_5, tt

!     +++ OUTPUT FORMATS +++

  100 format (1x,'  yr  mo  day     hr  ws  emission (kg m-2 s-1)')
  110 format (22x,'        total    salt/creep    susp      PM10       PM2_5')
  120 format (1x,3(i4),F7.3,F6.2, 1x,5(F11.8))

!     +++ END SPECIFICATIONS +++

!     set initial conditions

      if (initflg .eq. 0) then
          initflg = initflg + 1

          tims = 3600*24/ntstep !seconds in each emission period

          call caldat( mksaeout%jday, da, mo, yr) ! Set day, month and year

          write(0,*) 'First ntstep is: ', ntstep, tims, tims/3600

          write (ounit,*) 'SBEMIT output'
!          write (ounit,*) 'Suspended emissions < 0.10 mm dia.'
          write (ounit,*)

          ! Print date of Run
          rundatetime = get_systime_string() ! with Lahey f95, had to assign to variable first
          write(ounit,"(1x,'Date of run: ',a21)") rundatetime
          write (ounit,*)

          write (ounit,100)
          write (ounit,110) 
          write (ounit,*)
      endif

      ! init prev erosion hr values to zero if this is new erosion day
      if( first_emit ) then
          first_emit = .false.
          aegtp   = 0.0
          aegtssp = 0.0
          aegt10p = 0.0
          aegt2_5p = 0.0
      endif   

      !write(0,*) 'Subsequent ntstep is: ', ntstep, tims, tims/3600

      call caldat( mksaeout%jday, da, mo, yr) ! Set day, month and year

      aegt   = 0.0
      aegtss = 0.0
      aegt10 = 0.0
      aegt2_5 = 0.0

      do  j=1,jmax-1
         do  i= 1, imax-1
            aegt= aegt + cellstate(i,j)%egt
            aegtss = aegtss + cellstate(i,j)%egtss
            aegt10 = aegt10 + cellstate(i,j)%egt10
            aegt2_5 = aegt2_5 + cellstate(i,j)%egt2_5
         enddo
      enddo

      tt     = (imax-1)*(jmax-1)
      aegt   = - aegt/tt     ! change signs to positive=emission
      aegtss = - aegtss/tt
      aegt10 = - aegt10/tt
      aegt2_5 = - aegt2_5/tt

      emittot = (aegt - aegtp)/tims
      emitss  = (aegtss - aegtssp)/tims
      emit10  = (aegt10 - aegt10p)/tims
      emit2_5  = (aegt2_5 - aegt2_5p)/tims

!     Save prior hour average emission
      aegtp   =  aegt
      aegtssp =  aegtss
      aegt10p =  aegt10
      aegt2_5p =  aegt2_5

!     Write to emit.out file
      write (ounit,120) yr, mo, da, hhr, ws, emittot, emittot-emitss, emitss, emit10, emit2_5

   end subroutine sbemit

end module sae_in_out_mod

