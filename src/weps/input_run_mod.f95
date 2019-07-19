!$Author$
!$Date$
!$Revision$
!$HeadURL$

module input_run_mod

  use weps_main_mod
  use weps_cmdline_parms, only: report_info

contains

    subroutine input( soil )

!     + + + PURPOSE + + +
!     This subroutine perforns some screen I/O, reads in the
!     run files and performs various error checkings.

!     + + + GLOBAL COMMON BLOCKS + + +
      use soil_data_struct_defs, only: soil_def
      use flib_sax
      use input_run_xml_mod, only: runfile_complete, init_run_xml
      use input_run_xml_mod, only: begin_element_handler, end_element_handler, pcdata_chunk_handler

!     + + + ARGUMENT DECLARATIONS + + +
      type(soil_def), dimension(:), allocatable, intent(inout) :: soil 

!     + + + LOCAL VARIABLES + + +
      logical :: fexist     ! flag used to indicate result of file existence
      type(xml_t) :: fxml   ! xml file handle structure
      integer :: iostat     ! input/output status

!     + + + END SPECIFICATIONS + + +

      ! check for xml format input run file
      runfil = rootp(1:len_trim(rootp)) // 'weps.run.xml'
      inquire(file = runfil(1:len_trim(runfil)), exist = fexist)
      if (fexist) then
        old_run_file = .false.
        ! open simulation run file
        write (*,*) 'runfil is ', '>>', runfil(1:len_trim(runfil)), '<<'
        call open_xmlfile(runfil(1:len_trim(runfil)),fxml,iostat)
        if (iostat /= 0) stop "Cannot open runfil"
        ! read in xml based run file
        call init_run_xml()
        call xml_parse(fxml, &
             begin_element_handler = begin_element_handler, &
             end_element_handler = end_element_handler, &
             pcdata_chunk_handler = pcdata_chunk_handler, &
             verbose = .false.)
        if (.not. runfile_complete) then
          write(*,*) 'Simulation run file incomplete'
          call exit(1)
        end if
      else
        ! check for old fixed format run file
        runfil = rootp(1:len_trim(rootp)) // 'weps.run'
        inquire(file = runfil(1:len_trim(runfil)), exist = fexist)
        if (fexist) then
          ! read in old fixed format run file
          call inprun(soil)
        else
          write(0,*) ' simulation run file not found '
          call exit(1)
        end if
      end if

      !call echo_inputs(soil)

!     If this is a simulation that does water erosion read any extra WEPP
!     input data.
      if (run_erosion.gt.1) call inpwepp

      return
    end subroutine input

    subroutine inprun( soil )
      ! reads weps simulation run file

      !     + + + Modules Used + + +
      use soil_data_struct_defs, only: soil_def
      use datetime_mod, only: lstday
      use Polygons_Mod, only: create_polygon, set_area_polygon
      use subregions_mod, only: acct_poly, subr_poly
      use file_io_mod, only: fopenk, luicli, luiwin, luolog
      use erosion_data_struct_defs, only: subday, ntstep, am0efl
      use barriers_mod, only: create_barrier, barrier, barseas
      use grid_mod, only: amasim, amxsim, sim_area, xgdpt, ygdpt
      use hydro_data_struct_defs, only: am0hfl, am0hdb
      use soil_data_struct_defs, only: am0sfl, am0sdb
      use manage_data_struct_defs, only: manFile, manFileAlloc
      use crop_data_struct_defs, only: am0cfl, am0cdb
      use decomp_data_struct_defs, only: am0dfl, am0ddb
      use climate_input_mod, only: cli_gen_fmt_flag, wind_gen_fmt_flag, cligen_sname
      use climate_input_mod, only: amalat, amalon, amzele

!     + + + ARGUMENT DECLARATIONS + + +
      type(soil_def), dimension(:), allocatable, intent(inout) :: soil 

!     + + + LOCAL VARIABLES + + +
      integer :: nacctr   ! Number of accounting regions
      integer :: nsubr    ! Number of subregions
      integer :: nbr      ! number of barriers
      integer :: seas_flg ! barrier season flag
      integer :: iexp     ! counter for extra parameters used with season flag .eq. 2
      integer :: ntm_seas ! number of time marks for seasonal barrier
      integer :: poly_np  ! number of points in polygon or polyline
      integer       isr, iar, ios, ibr, ipol, iseas
      character     line*256
      real          sclsim, sclbar
      real          cligen_version
      real          wepsrun_version
      integer       lui1
      integer    :: sum_stat, alloc_stat
      character*80     awwisn   ! made local since not used anywhere else
      integer, parameter :: max_simyear = 100000  ! value used to test simulation year input range

!     + + + Local Variable Definitions + + +
!     isr - subregion index counter
!     iar - accounting region index counter
!     ios - input output status flag
!     ibr - barrier index counter
!     ipol - polygon points index counter

!     line - character array to hold contents of input line

!     sclsim - scaling factor used by interface, not within WEPS
!     sclbar - scaling factor used by interface, not within WEPS

!     cligen_version - version of the specified cligen file
!     wepsrun_version - version of the weps.run file being read

!     subr_poly - polygons defining each subregion extent
!     poly_np - number of points in polygon read from file
!     lui1 - unit number for input of weps.run file

      integer linnum, typidx

      wepsrun_version = -1.0
      linnum = 1
      typidx = 0
 
!     open simulation run file
      write (*,*) 'runfil is ', '>>',                                   &
     &  runfil(1:len_trim(runfil)), '<<'
      call fopenk (lui1, runfil(1:len_trim(runfil)), 'old')

      ! check for version number at top of file
      read (lui1,'(a)',err=80) line
      if( line(2:8) .eq. 'VERSION' ) then
          read (line(10:),*,err=80) wepsrun_version
      else
      end if

      if( wepsrun_version .lt. 1.1 ) then
         old_run_file = .true.
         write(*,*) 'WEPS run file not subregion enabled, reading old single region formats'
      else
         old_run_file = .false.
      end if
       
      ! read simulation run file
  100 linnum = linnum + 1

      if( old_run_file ) then
         if( wepsrun_version .lt. 1.0 ) then
            if( typidx .eq. 39 ) go to 200
         else if( wepsrun_version .ge. 1.02 ) then
            ! new value is 4th line past end of barrier info
            if( typidx .eq. 43 ) go to 200
         else
            ! new value is 3rd line past end of barrier info
            if( typidx .eq. 42 ) go to 200
         end if

         read (lui1,'(a)',err=80) line

         ! skip comment lines
         if (line(1:1) .eq. '#') go to 100

         ! use case statement to appropriately assign values
         typidx = typidx + 1

         ! write(*,*) 'INPRUN: typidx: ', typidx, 'line: ', trim(line)

         select case (typidx)
         case (1)
            usrnam = line(1:80)

         case (2)
            farmid = line(1:80)
            read (farmid((index(farmid,"|",back=.true.)+1):),*,err=80, iostat=ios) run_rot_cycles
            if (report_info >= 1) then
              print *, 'run_rot_cycles', run_rot_cycles
            end if
         case (3)
            fieldid = line(1:80)

         case (4)
            read (line,*,err=80, iostat=ios) amalat
            if ((amalat .lt. -90.) .or. (amalat .gt. 90.)) then
               write (*,2220)
               goto 80
            end if

         case (5)
            read (line,*,err=80) amalon
            if ((amalon .lt. -180.) .or. (amalon .gt. 180.)) then
               write (*,2230)
               goto 80
            end if

         case (6)
            read (line,*,err=80) amzele

         case (7)
            read (line,*,err=80) cligen_sname

         case (8)
            read (line,*,err=80) awwisn

         case (9)
            read (line,*,err=80) id,im,iy

         case (10)
            read (line,*,err=80) ld,lm,ly
            if (((id .lt. 1) .or. (id .gt. lstday(im,iy))) .or. ((ld .lt. 1) .or. (ld .gt. lstday(lm,ly)))) then
               write (*,2240)
               goto 80
            end if
            if (((id .lt. 1) .or. (id .gt. 31)) .or. ((ld .lt. 1) .or. (ld .gt. 31))) then
               write (*,2250)
               goto 80
            end if
            if (((im .lt. 1) .or. (im .gt. 12)) .or. ((lm .lt. 1) .or. (lm .gt. 12))) then
               write (*,2250)
               goto 80
            end if
            if (((iy .lt. 0) .or. (iy .gt.max_simyear)) .or. ((ly .lt. 0) .or. (ly .gt. max_simyear))) then
               write (*,2260)
               goto 80
            end if
            if ((ly - iy) .lt. 0) then
               write (*,2265)
               goto 80
            end if

         case (11)
            read (line,*,err=80) ntstep

            ! allocate wind direction and speed array
            allocate(subday(ntstep), stat=alloc_stat)
            if( alloc_stat .gt. 0 ) then
               Write(*,*) 'ERROR: memory alloc., wind direction and speed'
            end if

         case (12)
            ! read CLIGEN file name
            clifil = rootp(1:len_trim(rootp)) // line(1:len_trim(line))
            write(luolog, *) 'clifil: ', clifil(1:len_trim(clifil))
            ! open CLIGEN run file
            call fopenk (luicli, clifil, 'old')
            write(luolog,*) 'opened cligen file to determine db format...'
            ! read 1st line of CLIGEN file

            read(luicli,fmt="(a)",err=190) line
            write(6,"(a30,a)") 'First cligen output line is: ', trim(line)

            ! I think this is pretty messy.  It was working with the Lahey compiler
            ! with a "73x,f" format but the Sun F95 compiler didn't like that, so
            ! it was changed to "73x,f6.3".  I am now assuming that the "old versions"
            ! of cligen had the version number there.  Anyway, I had to change from
            ! "f" to "f6.3" for the Sun compiler on the second read of the line string.

            ! Probably not a very robust way to do this
            read(line,fmt="(73x,f8.5)",err=190) cligen_version
            if (cligen_version <= 5.1) then   ! assume new version of cligen
               read(line,fmt="(f8.5)",err=190) cligen_version
            end if

            write(luolog,"(a17,f8.5)") 'cligen version: ', cligen_version
            write(6,"(a17,f8.5)") 'cligen version: ', cligen_version

            ! I assume this is where I read the old cligen's version info
            ! read(luicli,fmt="(73x,f)",err=190) cligen_version
            ! write(luolog,*) 'cligen version: ', cligen_version

            ! We will now check the header to determine which cligen data file
            ! format we are reading, either the old one or the new one.
            ! if (index(line,'CLIGEN VERSION 5.101') > 0 ) then

            if (cligen_version >= 5.110) then
               cli_gen_fmt_flag = 3
            else if (cligen_version >= 5.101) then
               cli_gen_fmt_flag = 2
               write(luolog,*) 'Forest Service cligen db format'
            else
               cli_gen_fmt_flag = 1
               write(luolog,*) '3.1 version cligen db format'
            endif
            rewind luicli
            goto 130

            ! check for errors opening cli_gen data file here
  190       write(*,9002) clifil, line
            goto 80
  130       continue

         case (13)
            ! read WINDGEN file name
            winfil = rootp(1:len_trim(rootp)) // line
            ! open WINDGEN file
            call fopenk (luiwin, winfil, 'old')
            ! We will now check the header to determine which wind_gen data file
            ! format we are reading, either the old one (daily max and min wind
            ! speed, etc.) or the new one (24 hourly values per day).
            ! We now have a global wind_gen format flag we will set once we know.
            read(luiwin,fmt="(a80)",err=191) line
            ! write(6,*) 'line:', line
            if (    (index(line,'WIND_GEN5') > 0) &
               .or. (index(line,'WIND_GEN4') > 0) &
               .or. (index(line,'WIND_GEN3') > 0) &
               .or. (index(line,'WIND_GEN2') > 0) ) then
               wind_gen_fmt_flag = 2
            else
               wind_gen_fmt_flag = 1
            endif
            rewind luiwin
            goto 140

            ! check for errors opening wind_gen data file here
  191       write(*,9002) winfil, line
            goto 80
  140       continue

         case (14)
            ! read subdaily wind file name
            if (line(1:4) .ne. 'none') then
               subfil = rootp(1:len_trim(rootp)) // line
               write(*,*) 'Subdaily wind file feature obsolete. File specified is: ', trim(subfil)
            end if

         case (15)
            ! old run file is for single subregion
            nsubr = 1
            isr = 1
            ! create array of subregion polygons
            allocate(subr_poly(nsubr), stat=alloc_stat)
            if( alloc_stat .gt. 0 ) then
               write(*,*) 'ERROR: memory alloc., subregion polygons'
            end if
            ! create arrays for submodel output flags
            sum_stat = 0
            allocate(am0hfl(nsubr), stat=alloc_stat)
            sum_stat = sum_stat + alloc_stat
            allocate(am0sfl(nsubr), stat=alloc_stat)
            sum_stat = sum_stat + alloc_stat
            allocate(am0cfl(nsubr), stat=alloc_stat)
            sum_stat = sum_stat + alloc_stat
            allocate(am0dfl(nsubr), stat=alloc_stat)
            sum_stat = sum_stat + alloc_stat
            if( sum_stat .gt. 0 ) then
               write(*,*) 'ERROR: memory alloc., submodel output flags'
            end if

            ! create arrays for submodel debug flags
            sum_stat = 0
            allocate(am0hdb(nsubr), stat=alloc_stat)
            sum_stat = sum_stat + alloc_stat
            allocate(am0sdb(nsubr), stat=alloc_stat)
            sum_stat = sum_stat + alloc_stat
            allocate(am0cdb(nsubr), stat=alloc_stat)
            sum_stat = sum_stat + alloc_stat
            allocate(am0ddb(nsubr), stat=alloc_stat)
            sum_stat = sum_stat + alloc_stat
            if( sum_stat .gt. 0 ) then
               write(*,*) 'ERROR: memory alloc., debug output flags'
            end if

            allocate(soil(nsubr), stat=alloc_stat)
            if( alloc_stat .gt. 0 ) then
               write(*,*) 'ERROR: memory alloc., soil structure array'
            end if

            call manFileAlloc(nsubr)

            ! read in initial field conditions file name
            soil(isr)%sinfil = rootp(1:len_trim(rootp)) // line

         case (16)
            ! read in management file name
            manFile(isr)%tinfil = rootp(1:len_trim(rootp)) // line

         case (17)
            ! deprecated line
            ! read output file name
            ! simout = rootp(1:len_trim(rootp)) // line
            ! Quit opening this file.  We haven't used it for years. - 11/17/05 - LEW
            ! open (unit = 2, file = simout)

         case (18)
            ! deprecated line
            ! read the flags to select the various general report forms
            ! read (line,*,err=80) (gnrpt(i), i=1,6)
            ! read code to select period for output
            ! yearly and simulation summaries are always given

         case (19)
            ! deprecated line
            ! read (line,*,err=80) erosrpt

         case (20)
            read (line,*,err=80) am0hfl(isr),am0sfl(isr),manFile(isr)%am0tfl, am0cfl(isr),am0dfl(isr),am0efl

         case (21)
            ! debug flag line. Add zero integer to end to make sure six values
            ! are available to read. Previously interface only set 5 flags.
            ! Now should set six.
            ! this is not needed now, am0edb is deprecated
            ! line = line(1:len_trim(line)) // ' 0'
            read (line,*,err=80) am0hdb(isr),am0sdb(isr),manFile(isr)%am0tdb, am0cdb(isr),am0ddb(isr)

         case (22)
            read (line,*,err=80) amasim

         case (23)
            read (line,*,err=80) amxsim(1)%x, amxsim(1)%y

         case (24)
            read (line,*,err=80) amxsim(2)%x, amxsim(2)%y
            ! compute the simulation area
            sim_area = (amxsim(2)%x - amxsim(1)%x) * (amxsim(2)%y - amxsim(1)%y)

         case (25)
            ! These values are scaling factors for interface, not used in WEPS
            read (line,*,err=80) sclsim, sclbar

         case (26)
            read (line,*,err=80) nacctr
            ! set up iar for reading in next lines
            iar = 1
            ! create array of accounting region polygons
            allocate(acct_poly(nacctr), stat = alloc_stat)
            if( alloc_stat .gt. 0 ) then
               Write(*,*) 'ERROR: memory alloc., accounting region polygons'
            end if

         case (27)
            ! set accounting region polygon point count
            poly_np = 4
            ! create polygon point storage
            acct_poly(iar) = create_polygon(poly_np)
            ! read first corner into first location
            ipol = 1
            read (line,*,err=80) acct_poly(iar)%points(ipol)%x, acct_poly(iar)%points(ipol)%y

         case (28)
            ! read opposite corner into third location
            ipol = 3
            read (line,*,err=80) acct_poly(iar)%points(ipol)%x, acct_poly(iar)%points(ipol)%y
            ! fill out remaining points for square accounting region
            ipol = 2
            acct_poly(iar)%points(ipol)%x = acct_poly(iar)%points(3)%x
            acct_poly(iar)%points(ipol)%y = acct_poly(iar)%points(1)%y
            ipol = 4
            acct_poly(iar)%points(ipol)%x = acct_poly(iar)%points(1)%x
            acct_poly(iar)%points(ipol)%y = acct_poly(iar)%points(3)%y
            ! Single polygon, no need to close
            ! polygon complete
            call set_area_polygon(acct_poly(isr))
            ! send us back to case (25) to read in array
            if (iar.lt.nacctr) typidx = typidx - 2
            iar = iar + 1

         case (29)
            read (line,*,err=80) isr
            ! read in sub-region data (currently only 1 allowed)
            isr = 1
            ! set subregion polygon point count
            poly_np = 4
            ! create polygon container for the single subregion
            subr_poly(isr) = create_polygon(poly_np)

         case (30)
            ! read first corner into first location
            ipol = 1
            read (line,*,err=80) subr_poly(isr)%points(ipol)%x, subr_poly(isr)%points(ipol)%y

         case (31)
            ! read opposite corner into third location
            ipol = 3
            read (line,*,err=80) subr_poly(isr)%points(ipol)%x, subr_poly(isr)%points(ipol)%y
            ! fill out remaining points for square sub-region
            ipol = 2
            subr_poly(isr)%points(ipol)%x = subr_poly(isr)%points(3)%x
            subr_poly(isr)%points(ipol)%y = subr_poly(isr)%points(1)%y
            ipol = 4
            subr_poly(isr)%points(ipol)%x = subr_poly(isr)%points(1)%x
            subr_poly(isr)%points(ipol)%y = subr_poly(isr)%points(3)%y
            ! Single polygon, no need to close
            ! polygon complete
            call set_area_polygon(subr_poly(isr))

         case (32)
            ! The new "versioned" IFC files contain a slope value
            ! which will be used if this value is set negative, 
            ! ie. not entered. It is now the only way to set a 
            ! non default slope when using the older "non-versioned"
            ! IFC files.
            read (line,*,err=80) soil(isr)%amrslp        ! weps.run file has slope gradient (m/m)
            ! disabled reading in multiple subregion points, only one was allowed in old files
            ! isr = isr + 1
            ! if (isr.le.nsubr) typidx=typidx-3

         case (33)
            ! old run files could have 0 barriers but still always read info in for 1.
            read (line,*,err=80) nbr
            ! write(6,*) ' reading barriers ', nbr
            ! allocate structure for barriers (nbr .lt. 1 gives zero size array)
            allocate(barrier(nbr), stat = alloc_stat)
            if( alloc_stat .gt. 0 ) then
               write(*,*) 'ERROR: memory alloc., barrier'
            end if
            allocate(barseas(nbr), stat = alloc_stat)
            if( alloc_stat .gt. 0 ) then
               write(*,*) 'ERROR: memory alloc., seasonal barrier'
            end if
            ! initialize counter for reading barrier parameters
            ibr = 1

         case (34)
            if( nbr .ge. 1 ) then
               ! number of points in barrier polyline
               poly_np = 2
               ! number of time marks in season
               ntm_seas = 1
               ! create storage for point and barrier data
               call create_barrier(barrier(ibr), poly_np)
               call create_barrier(barseas(ibr), poly_np,ntm_seas,0)
               ! read first point pair
               ipol = 1
               read (line,*,err=80) barseas(ibr)%points(ipol)%x, barseas(ibr)%points(ipol)%y
               !  also place in fixed barrier structure
               barrier(ibr)%points(ipol) = barseas(ibr)%points(ipol)
            end if

         case (35)
            if( nbr .ge. 1 ) then
               ! read second point pair
               ipol = 2
               read (line,*,err=80) barseas(ibr)%points(ipol)%x, barseas(ibr)%points(ipol)%y
               !  also place in fixed barrier structure
               barrier(ibr)%points(ipol) = barseas(ibr)%points(ipol)

               !  Convert (x,y) barrier rectangular corner coordinates to
               !  (x,y) midline coordinates and width as currently defined in WEPS.

               !  NOTE:  We don't convert to true midline coordinates because
               !         the erosion submodel assumes the midline is the barrier
               !         edge at this time.  Since WEPS 1.0 only handles barriers
               !         that exist on the simulation region (field) boundary, the
               !         the barrier (x,y) coordinates are set to match the simulation
               !         region boundary coordinates, not to the actual barrier
               !         midline coordinates.  LEW AUG 23, 2000  8:07 AM
               if ((amxsim(1)%x .eq. barseas(ibr)%points(1)%x) .and. &
                   (amxsim(2)%x .eq. barseas(ibr)%points(2)%x)) then ! N or S barrier
                  if (amxsim(1)%y .eq. barseas(ibr)%points(2)%y) then ! S barrier
                     barseas(ibr)%points(1)%y = amxsim(1)%y
                     !  also place in fixed barrier structure
                     barrier(ibr)%points(1) = barseas(ibr)%points(1)
                     !write(6,*) 'South barrier'
                  else if (amxsim(2)%y .eq. barseas(ibr)%points(1)%y) then ! N barrier
                     barseas(ibr)%points(2)%y = amxsim(2)%y
                     !  also place in fixed barrier structure
                     barrier(ibr)%points(2) = barseas(ibr)%points(2)
                     !write(6,*) 'North barrier'
                  endif
               else if ((amxsim(1)%y .eq. barseas(ibr)%points(1)%y) .and. &
                        (amxsim(2)%y .eq. barseas(ibr)%points(2)%y)) then ! E or W barrier
                  if (amxsim(1)%x .eq. barseas(ibr)%points(2)%x) then       ! W barrier
                     barseas(ibr)%points(1)%x = amxsim(1)%x
                     !  also place in fixed barrier structure
                     barrier(ibr)%points(1) = barseas(ibr)%points(1)
                     !write(6,*) 'West barrier'
                  else if (amxsim(2)%x .eq. barseas(ibr)%points(1)%x) then  ! E barrier
                     barseas(ibr)%points(2)%x = amxsim(2)%x
                     !  also place in fixed barrier structure
                     barrier(ibr)%points(2) = barseas(ibr)%points(2)
                     !write(6,*) 'East barrier'
                  endif
               else
                  write(6,*) 'No barrier match for barrier: ', ibr
               endif
            end if

         case (36)
            if( nbr .ge. 1 ) then
               barseas(ibr)%amzbt = line(1:80)
               !  also place in fixed barrier structure
               barrier(ibr)%amzbt = barseas(ibr)%amzbt
            end if

         case (37)
            if( nbr .ge. 1 ) then
               ! read first point value
               ipol = 1
               read (line,*,err=80) barseas(ibr)%param(ipol,1)%amzbr
               if( barseas(ibr)%param(ipol,1)%amzbr .le. 0.0 ) then
                  write(*,*) 'ERROR: Barrier height must be > 0'
                  write(*,FMT='(2(i0))') 'Barrier #: ', ibr, 'Point #: ', ipol
                  call exit(37)
               end if
               ! set second point value to same
               ipol = 2
               barseas(ibr)%param(ipol,1)%amzbr = barseas(ibr)%param(1,1)%amzbr
            end if

         case (38)
            if( nbr .ge. 1 ) then
               ! read first point value
               ipol = 1
               read (line,*,err=80) barseas(ibr)%param(ipol,1)%amxbrw
               ! set second point value to same
               ipol = 2
               barseas(ibr)%param(ipol,1)%amxbrw = barseas(ibr)%param(1,1)%amxbrw
            end if

         case (39)
            if( nbr .ge. 1 ) then
               ! read first point value
               ipol = 1
               read (line,*,err=80) barseas(ibr)%param(ipol,1)%ampbr
               ! set second point value to same
               ipol = 2
               barseas(ibr)%param(ipol,1)%ampbr = barseas(ibr)%param(1,1)%ampbr
            end if
            ibr = ibr + 1
            if (ibr.le.nbr) then
               ! read in next barrier
               typidx=typidx-6
            end if

         case (40)
           ! this does nothing but skip the line for shape name

         case (41)
            ! this does nothing but skip the line for shape radius
   
         case (42)
            read (line,*,err=80) soil(isr)%WaterErosion
            isr = isr + 1
            if (isr.le.nsubr) typidx=typidx-1
            !!!! I don't think this works as intended - LEW
            ! will only work if isr .le. 2 not .gt. 2
            ! set subregion counter for next line
            isr = 1

         case (43)
            read (line,*,err=80) soil(isr)%SoilRockFragments
            isr = isr + 1
            if (isr.le.nsubr) typidx=typidx-1
            !!!! I don't think this works as intended - LEW
            ! will only work if isr .le. 2 not .gt. 2
            ! set subregion counter for next line
            isr = 1

         end select

      else
         ! read subregion enabled simulation run file
         if( typidx .eq. 48 ) go to 200

         read (lui1,'(a)',err=80) line

         ! skip comment lines
         if (line(1:1) .eq. '#') go to 100

         ! use case statement to appropriately assign values
         typidx = typidx + 1

         ! write(*,*) 'INPRUN: typidx: ', typidx, 'line: ', trim(line)

         select case (typidx)
         case (1)
            usrnam = line(1:80)

         case (2)
            farmid = line(1:80)
            read (farmid((index(farmid,"|",back=.true.)+1):),*,err=80, iostat=ios) run_rot_cycles
            if (report_info >= 1) then
              print *, 'run_rot_cycles', run_rot_cycles
            end if

         case (3)
            fieldid = line(1:80)

         case (4)
            read (line,*,err=80, iostat=ios) amalat
            if ((amalat .lt. -90.) .or. (amalat .gt. 90.)) then
               write (*,2220)
               goto 80
            end if

         case (5)
            read (line,*,err=80) amalon
            if ((amalon .lt. -180.) .or. (amalon .gt. 180.)) then
               write (*,2230)
               goto 80
            end if

         case (6)
            read (line,*,err=80) amzele

         case (7)
            read (line,*,err=80) cligen_sname

         case (8)
            read (line,*,err=80) awwisn

         case (9)
            read (line,*,err=80) id,im,iy

         case (10)
            read (line,*,err=80) ld,lm,ly
            if (((id .lt. 1) .or. (id .gt. lstday(im,iy))) .or. ((ld .lt. 1) .or. (ld .gt. lstday(lm,ly)))) then
               write (*,2240)
               goto 80
            end if
            if (((id .lt. 1) .or. (id .gt. 31)) .or. ((ld .lt. 1) .or. (ld .gt. 31))) then
               write (*,2250)
               goto 80
            end if
            if (((im .lt. 1) .or. (im .gt. 12)) .or. ((lm .lt. 1) .or. (lm .gt. 12))) then
               write (*,2250)
               goto 80
            end if
            if (((iy .lt. 0) .or. (iy .gt.max_simyear)) .or. ((ly .lt. 0) .or. (ly .gt. max_simyear))) then
               write (*,2260)
               goto 80
            end if
            if ((ly - iy) .lt. 0) then
               write (*,2265)
               goto 80
            end if

         case (11)
            read (line,*,err=80) ntstep

            ! allocate wind direction and speed array
            allocate(subday(ntstep), stat=alloc_stat)
            if( alloc_stat .gt. 0 ) then
               Write(*,*) 'ERROR: memory alloc., wind direction and speed'
            end if

         case (12)
            ! read CLIGEN file name
            clifil = rootp(1:len_trim(rootp)) // line(1:len_trim(line))
            write(luolog, *) 'clifil: ', clifil(1:len_trim(clifil))
            ! open CLIGEN run file
            call fopenk (luicli, clifil, 'old')
            write(luolog,*) 'opened cligen file to determine db format...'
            ! read 1st line of CLIGEN file

            read(luicli,fmt="(a)",err=290) line
            if (report_info >= 1) then
              write(6,*) '1st cligen input line is: ', line
            end if

            ! I think this is pretty messy.  It was working with the Lahey compiler
            ! with a "73x,f" format but the Sun F95 compiler didn't like that, so
            ! it was changed to "73x,f6.3".  I am now assuming that the "old versions"
            ! of cligen had the version number there.  Anyway, I had to change from
            ! "f" to "f6.3" for the Sun compiler on the second read of the line string.

            ! Probably not a very robust way to do this
            read(line,fmt="(73x,f6.3)",err=290) cligen_version
            if (cligen_version <= 5.1) then   ! assume new version of cligen
               read(line,fmt="(f6.3)",err=290) cligen_version
            end if

            write(luolog,*) 'cligen version: ', cligen_version
            if (report_info >= 1) then
              write(6,*) 'cligen version: ', cligen_version
            end if
            ! I assume this is where I read the old cligen's version info
            ! read(luicli,fmt="(73x,f)",err=290) cligen_version
            ! write(luolog,*) 'cligen version: ', cligen_version

            ! We will now check the header to determine which cligen data file
            ! format we are reading, either the old one or the new one.
            ! if (index(line,'CLIGEN VERSION 5.101') > 0 ) then

            if (cligen_version >= 5.110) then
               cli_gen_fmt_flag = 3
            else if (cligen_version >= 5.101) then
               cli_gen_fmt_flag = 2
               write(luolog,*) 'Forest Service cligen db format'
            else
               cli_gen_fmt_flag = 1
               write(luolog,*) '3.1 version cligen db format'
            endif
            rewind luicli
            goto 230

            ! check for errors opening cli_gen data file here
  290       write(*,9002) clifil, line
            goto 80
  230       continue

         case (13)
            ! read WINDGEN file name
            winfil = rootp(1:len_trim(rootp)) // line
            ! open WINDGEN file
            call fopenk (luiwin, winfil, 'old')
            ! We will now check the header to determine which wind_gen data file
            ! format we are reading, either the old one (daily max and min wind
            ! speed, etc.) or the new one (24 hourly values per day).
            ! We now have a global wind_gen format flag we will set once we know.
            read(luiwin,fmt="(a80)",err=291) line
            ! write(6,*) 'line:', line
            if (    (index(line,'WIND_GEN5') > 0) &
               .or. (index(line,'WIND_GEN4') > 0) &
               .or. (index(line,'WIND_GEN3') > 0) &
               .or. (index(line,'WIND_GEN2') > 0) ) then
               wind_gen_fmt_flag = 2
            else
               wind_gen_fmt_flag = 1
            endif
            rewind luiwin
            goto 240

            ! check for errors opening wind_gen data file here
  291       write(*,9002) winfil, line
            goto 80
  240       continue

         case (14)
            ! read subdaily wind file name
            if (line(1:4) .ne. 'none') then
               subfil = rootp(1:len_trim(rootp)) // line
               write(*,*) 'Subdaily wind file feature obsolete. File specified is: ', trim(subfil)
            end if

         case (15)
            ! read erosion submodel detail flag
            read (line,*,err=80) am0efl

         case (16)
            ! simulation region angle from north (+/- 45 degrees)
            read (line,*,err=80) amasim

         case (17)
            ! simulation region diagonal coordinates (lower left)
            read (line,*,err=80) amxsim(1)%x, amxsim(1)%y

         case (18)
            ! simulation region diagonal coordinates (upper right)
            read (line,*,err=80) amxsim(2)%x, amxsim(2)%y
            ! compute the simulation area
            sim_area = (amxsim(2)%x - amxsim(1)%x) * (amxsim(2)%y - amxsim(1)%y)
            if (report_info >= 1) then
              write(6,*) "Simulation area (m^2)", sim_area
            end if
         case (19)
            ! the simulation grid resolution in x and y directions
            read (line,*,err=80) xgdpt, ygdpt

         case (20)
            ! These values are scaling factors for interface, not used in WEPS
            read (line,*,err=80) sclsim, sclbar

         case (21)
            read (line,*,err=80) nacctr  ! can be set to 0, skip to subregion definitions
            ! set counter iar for reading in next lines
            iar = 1
            ! create array of accounting region polygons
            allocate(acct_poly(nacctr), stat = alloc_stat)
            if( alloc_stat .gt. 0 ) then
               Write(*,*) 'ERROR: memory alloc., accounting region polygons'
            end if
            if( nacctr .lt. 1 ) then
               ! no accounting region polygons defined, skip to subregion section
               typidx = typidx + 2
            end if

         case (22)
            ! read accounting region polygon point count
            read (line,*,err=80) poly_np
            ! create polygon point storage
            acct_poly(iar) = create_polygon(poly_np)
            ! set counter for reading each point pair
            ipol = 1

         case (23)
            ! read point pair
            read (line,*,err=80) acct_poly(iar)%points(ipol)%x, acct_poly(iar)%points(ipol)%y
            ! read next point pair
            ipol = ipol + 1
            if( ipol .le. poly_np ) then
               ! read another point pair
               typidx = typidx - 1
            else
               ! finished with this accounting region
               call set_area_polygon( acct_poly(iar) )
               iar = iar + 1
               if( iar .le. nacctr ) then
                  ! read another accounting region
                  typidx = typidx - 2
               end if
            end if

         case (24)
            ! read Subregion count
            read (line,*,err=80) nsubr  ! must be at least 1
            ! set up isr for reading in next lines for each subregion
            isr = 1
            ! create array of subregion polygons
            allocate(subr_poly(nsubr), stat=alloc_stat)
            if( alloc_stat .gt. 0 ) then
               write(*,*) 'ERROR: memory alloc., subregion polygons'
            end if
            ! create arrays for submodel output flags
            sum_stat = 0
            allocate(am0hfl(nsubr), stat=alloc_stat)
            sum_stat = sum_stat + alloc_stat
            allocate(am0sfl(nsubr), stat=alloc_stat)
            sum_stat = sum_stat + alloc_stat
            allocate(am0cfl(nsubr), stat=alloc_stat)
            sum_stat = sum_stat + alloc_stat
            allocate(am0dfl(nsubr), stat=alloc_stat)
            sum_stat = sum_stat + alloc_stat
            if( alloc_stat .gt. 0 ) then
               write(*,*) 'ERROR: memory alloc., submodel output flags'
            end if

            ! create arrays for submodel debug flags
            sum_stat = 0
            allocate(am0hdb(nsubr), stat=alloc_stat)
            sum_stat = sum_stat + alloc_stat
            allocate(am0sdb(nsubr), stat=alloc_stat)
            sum_stat = sum_stat + alloc_stat
            allocate(am0cdb(nsubr), stat=alloc_stat)
            sum_stat = sum_stat + alloc_stat
            allocate(am0ddb(nsubr), stat=alloc_stat)
            sum_stat = sum_stat + alloc_stat
            if( alloc_stat .gt. 0 ) then
               write(*,*) 'ERROR: memory alloc., debug output flags'
            end if

            allocate(soil(nsubr), stat=alloc_stat)
            if( alloc_stat .gt. 0 ) then
               write(*,*) 'ERROR: memory alloc., soil structure array'
            end if

            call manFileAlloc(nsubr)

         case (25)
            read (line,*,err=80) am0hfl(isr),am0sfl(isr),manFile(isr)%am0tfl, am0cfl(isr),am0dfl(isr)
            ! debug flag line.

         case (26)
            read (line,*,err=80) am0hdb(isr),am0sdb(isr),manFile(isr)%am0tdb, am0cdb(isr),am0ddb(isr)

         case (27)
            ! read subregion polygon point count
            read (line,*,err=80) poly_np
            ! create polygon point storage
            subr_poly(isr) = create_polygon(poly_np)
            ! set counter for reading each point pair
            ipol = 1

         case (28)
            ! read point pair
            read (line,*,err=80) subr_poly(isr)%points(ipol)%x, subr_poly(isr)%points(ipol)%y
            ! read next point pair
            ipol = ipol + 1
            if( ipol .le. poly_np ) then
                ! read another point pair
                typidx = typidx - 1
            else
                ! polygon complete
                call set_area_polygon(subr_poly(isr))
            end if

         case (29)
            !        The new "versioned" IFC files contain a slope value
            !        which will be used if this value is set negative, 
            !        ie. not entered. It is now the only way to set a 
            !        non default slope when using the older "non-versioned"
            !        IFC files.   
            read (line,*,err=80) soil(isr)%amrslp        ! weps.run file has slope gradient (m/m)
   
         case (30)
            read (line,*,err=80) soil(isr)%SoilRockFragments

         case (31)
            ! read in initial field conditions file name
            soil(isr)%sinfil = rootp(1:len_trim(rootp)) // line

         case (32)
            ! read in management file name
            manFile(isr)%tinfil = rootp(1:len_trim(rootp)) // line

         case (33)
            read (line,*,err=80) soil(isr)%WaterErosion

            ! this is last item in subregion group
            ! index to next subregion or continue on
            isr = isr + 1
            if (isr .le. nsubr) then
               typidx = typidx - 9
            end if

         case (34)
            !  These barriers as entered are considered to be thin, having no real
            !  area effect such as erodible material source or deposition area.
            !  The polyline entered is the "effective location".

            !  Barriers wider than anything approaching the scale of a cell (1/10th
            !  a cell width)should probably be entered as subregions and the erosion
            !  submodel changed to consider their wind shadow effect on adjoining cells

            !  Note: the barrier point number must be read first and the barrier storage
            !  allocated, then the barrier level data populated. (hence the barrier type
            !  string now comes last)

            !  Note: When seas_flg = 2 is specified, it is required that two points (no
            !  more no less) in time be provided, the first date being when it can be
            !  guaranteed that leaves are at a minimum, and the data values for porosity
            !  correspond to that. The second date is when it can be quaranteed that
            !  leaves are at a maximum and the data values for porosity correspond to that.

            ! read in barrier info
            read (line,*,err=80) nbr
            ! write(6,*) ' reading barriers ', nbr
            ! allocate structure for barriers (nbr .lt. 1 gives zero size array)
            allocate(barrier(nbr), stat = alloc_stat)
            if( alloc_stat .gt. 0 ) then
               write(*,*) 'ERROR: memory alloc., barrier'
            end if
            allocate(barseas(nbr), stat = alloc_stat)
            if( alloc_stat .gt. 0 ) then
               write(*,*) 'ERROR: memory alloc., seasonal barrier'
            end if
            if( nbr .lt. 1 ) then
               ! skip reading barrier information
               typidx = typidx + 14
            else
               ! set index for first barrier
               ibr = 1
            end if

         case (35)
            ! barrier season flag
            read (line,*,err=80) seas_flg

         case (36)
            ! number of time marks in seasonal barrier
            read (line,*,err=80) ntm_seas

         case (37)
            ! number of points in barrier polyline
            read (line,*,err=80) poly_np
            ! create storage for point and barrier data
            ! this also sets values for barr%np and barr%ntm
            call create_barrier(barrier(ibr), poly_np)
            call create_barrier(barseas(ibr), poly_np,ntm_seas,seas_flg)
            ! set counter for reading each point pair
            ipol = 1
            iseas = 1

            if( (seas_flg .eq. 0) .or. (seas_flg .eq. 1) ) then
               ! no extra parameters required, skip reading of extra parameters
               typidx = typidx + 6
            else if( seas_flg .eq. 2 ) then
               if( ntm_seas .ne. 2 ) then
                 ! exactly 2 time marks required for seas_flg = 2
                 write(*,*) 'ERROR: Barrier season flag value of 2 requires exactly 2 time marks'
                 write(*,FMT='(i0)') 'Input value was: ', ntm_seas
                 call exit(36)
               end if
               ! initialize counter for extra parameters 1 = leaf on parameter set 2 = leaf off parameter set
               iexp = 1
            else
               write(*,*) 'ERROR: Barrier season flag value must be 0, 1 or 2'
               write(*,FMT='(i0)') 'Input value was: ', seas_flg
               call exit(35)
            end if

         case (38)
            ! barrier climate data type flag for beginning of leaf on/off
            read (line,*,err=80) barseas(ibr)%clim(iexp)%beg_flg

         case (39)
            ! barrier begin leaf on/off climate accumulation threshold value
            read (line,*,err=80) barseas(ibr)%clim(iexp)%beg_thresh

         case (40)
            ! barrier begin leaf on/off climate base value for accumulation
            read (line,*,err=80) barseas(ibr)%clim(iexp)%beg_base
            ! initialize accumulator
            barseas(ibr)%clim(iexp)%beg_accum = 0.0

         case (41)
            ! barrier climate data type flag for completion of leaf on/off
            read (line,*,err=80) barseas(ibr)%clim(iexp)%end_flg

         case (42)
            ! barrier completion of leaf on/off climate accumulation threshold value
            read (line,*,err=80) barseas(ibr)%clim(iexp)%end_thresh

         case (43)
            ! barrier completion of leaf on/off climate base value for accumulation
            read (line,*,err=80) barseas(ibr)%clim(iexp)%end_base
            ! initialize accumulator
            barseas(ibr)%clim(iexp)%end_accum = 0.0

            ! increment counter
            iexp = iexp + 1
            if( iexp .eq. 2 ) then
               ! return to read leaf off parameters
               typidx = typidx - 6
            end if
            
         case (44)
            ! read in day of year time mark for barrier seasons
            read (line,*,err=80) barseas(ibr)%dst(iseas)%doy
            if( iseas .gt. 1 ) then
               if( barseas(ibr)%dst(iseas)%doy .le. barseas(ibr)%dst(iseas-1)%doy ) then
                 write(*,*) 'ERROR: Barrier time marks (day of year) must always increase'
                 write(*,FMT='(i0)') 'Previous value was: ', barseas(ibr)%dst(iseas-1)%doy
                 write(*,FMT='(i0)') 'Newest value is: ', barseas(ibr)%dst(iseas)%doy
                 call exit(44)
               end if
            end if

         case (45)
            ! read in text description of time mark for barrier seasons
            read (line,*,err=80) barseas(ibr)%dst(iseas)%st_desc
            ! read next day of year time mark
            iseas = iseas + 1
            if( iseas .le. ntm_seas ) then
                ! read another day of year time mark 
                typidx = typidx - 2
            else
                ! done reading reset for seasonal data below
                iseas = 1
            end if

         case (46)
            ! read point pair
            read (line,*,err=80) barseas(ibr)%points(ipol)%x, barseas(ibr)%points(ipol)%y
            !  also place in fixed barrier structure
            barrier(ibr)%points(ipol) = barseas(ibr)%points(ipol)

         case (47)
            ! barrier height
            read (line,*,err=80) barseas(ibr)%param(ipol,iseas)%amzbr, &
                                 barseas(ibr)%param(ipol,iseas)%amxbrw, &
                                 barseas(ibr)%param(ipol,iseas)%ampbr
            if( barseas(ibr)%param(ipol,iseas)%amzbr .le. 0.0 ) then
               write(*,*) 'ERROR: Barrier height must be > 0'
               write(*,FMT='(2(i0))') 'Barrier #: ', ibr, 'Point #: ', ipol
               call exit(40)
            end if
            ! read next season of barrier data
            iseas = iseas + 1
            if( iseas .le. ntm_seas ) then
                ! read barrier data for another day of year time mark 
                typidx = typidx - 1
            else
                ! done reading reset for next point seasonal barrier data
                iseas = 1
                ! read next group of point and barrier data
                ipol = ipol + 1
                if( ipol .le. poly_np ) then
                    ! read another group of point and barrier data
                    typidx = typidx - 2
                end if
            end if

         case (48)
            ! barrier type character string
            barseas(ibr)%amzbt = line(1:80)
            !  also place in fixed barrier structure
            barrier(ibr)%amzbt = barseas(ibr)%amzbt

            ! increment for next barrier
            ibr = ibr + 1
            if (ibr.le.nbr) then
               ! read in next barrier
               typidx = typidx - 14
            end if

         end select
      end if

      goto 100

   80 write(0,9001) runfil, linnum, typidx, line
      call exit(1)
  200 close (lui1)
     

! Format statements

 2220 format (/,' error, latitude is not between -90. and 90. degrees',/&
     &,' - please check run file')
 2230 format (/,' error, longitude is not between -180. and 180. degrees&
     &',/,'  - please check run file')
 2240 format (/,' error, initial or last day of simulation is out of bou&
     &nds',/,'  - please check run file')
 2250 format (/,' error, initial or last day or month of simulation is o&
     &ut of bounds',/,'  - please check run file')
 2260 format (/,' error, initial or last year of simulation is not betwe&
     &en 0 and max_simyear',/,'  - please check run file')
 2265 format (/,' error, initial year of simulation is greater than the &
     &last year of simulation',/,'  - please check run file')

 9001  format('Error in file ',a,' on line #',i4,i3,' ',a)
 9002  format('Error in file: ',a,' reading: ',a)

   end subroutine inprun

   subroutine echo_inputs(soil)
      !     + + + Modules Used + + +
      use soil_data_struct_defs, only: soil_def
      use datetime_mod, only: lstday
      use Polygons_Mod, only: create_polygon, set_area_polygon
      use subregions_mod, only: acct_poly, subr_poly
      use file_io_mod, only: fopenk
      use erosion_data_struct_defs, only: ntstep, am0efl
      use barriers_mod, only: create_barrier, barrier, barseas
      use grid_mod, only: amasim, amxsim, sim_area
      use hydro_data_struct_defs, only: am0hfl, am0hdb
      use soil_data_struct_defs, only: am0sfl, am0sdb
      use manage_data_struct_defs, only: manFile
      use crop_data_struct_defs, only: am0cfl, am0cdb
      use decomp_data_struct_defs, only: am0dfl, am0ddb
      use climate_input_mod, only: cli_gen_fmt_flag, wind_gen_fmt_flag
      use climate_input_mod, only: amalat, amalon, amzele

!     + + + ARGUMENT DECLARATIONS + + +
      type(soil_def), dimension(:), intent(inout) :: soil 

!     + + + LOCAL VARIABLES + + +
      integer :: nacctr   ! Number of accounting regions
      integer :: nsubr    ! Number of subregions
      integer :: nbr      ! number of barriers
      integer :: poly_np  ! number of points in polygon or polyline
      integer       isr, iar, ibr, ipol
      real          cligen_version

!     + + + Local Variable Definitions + + +
!     isr - subregion index counter
!     iar - accounting region index counter
!     ios - input output status flag
!     ibr - barrier index counter
!     ipol - polygon points index counter

!     line - character array to hold contents of input line

!     sclsim - scaling factor used by interface, not within WEPS
!     sclbar - scaling factor used by interface, not within WEPS

!     cligen_version - version of the specified cligen file
!     wepsrun_version - version of the weps.run file being read

!     subr_poly - polygons defining each subregion extent
!     poly_np - number of points in polygon read from file
!     lui1 - unit number for input of weps.run file

      write(*,*) 'INVALS', amalat
      write(*,*) 'INVALS', amalon
      write(*,*) 'INVALS', amzele
      ! cligen_sname
      !write(*,*) 'INVALS', awwisn
      write(*,*) 'INVALS', id,im,iy
      write(*,*) 'INVALS', ld,lm,ly
      write(*,*) 'INVALS', ntstep
      write(*,*) 'INVALS', trim(clifil)
      write(*,*) 'INVALS', cligen_version
      write(*,*) 'INVALS', cli_gen_fmt_flag
      write(*,*) 'INVALS', trim(winfil)
      write(*,*) 'INVALS', wind_gen_fmt_flag
      write(*,*) 'INVALS', nsubr
      isr = 1
      write(*,*) 'INVALS', trim(soil(isr)%sinfil)
      write(*,*) 'INVALS', trim(manFile(isr)%tinfil)
      write(*,*) 'INVALS', am0hfl(isr)
      write(*,*) 'INVALS', am0sfl(isr)
      write(*,*) 'INVALS', manFile(isr)%am0tfl
      write(*,*) 'INVALS', am0cfl(isr)
      write(*,*) 'INVALS', am0dfl(isr)
      write(*,*) 'INVALS', am0efl
      write(*,*) 'INVALS', am0hdb(isr)
      write(*,*) 'INVALS', am0sdb(isr)
      write(*,*) 'INVALS', manFile(isr)%am0tdb
      write(*,*) 'INVALS', am0cdb(isr)
      write(*,*) 'INVALS', am0ddb(isr)
      write(*,*) 'INVALS', amasim
      write(*,*) 'INVALS', amxsim(1)%x
      write(*,*) 'INVALS', amxsim(1)%y
      write(*,*) 'INVALS', amxsim(2)%x
      write(*,*) 'INVALS', amxsim(2)%y
      write(*,*) 'INVALS', sim_area
      write(*,*) 'INVALS', nacctr
      iar = 1
      poly_np = 4
      do ipol = 1, poly_np
        write(*,*) 'INVALS', acct_poly(iar)%points(ipol)%x, acct_poly(iar)%points(ipol)%y
      end do
      write(*,*) 'INVALS', acct_poly(isr)%area
      isr = 1
      poly_np = 4
      do ipol = 1, poly_np
        write(*,*) 'INVALS', subr_poly(isr)%points(ipol)%x, subr_poly(isr)%points(ipol)%y
      end do
      write(*,*) 'INVALS', subr_poly(isr)%area
      write(*,*) 'INVALS', soil(isr)%amrslp
      write(*,*) 'INVALS', nbr
      do ibr = 1, nbr
        do ipol = 1, poly_np
          write(*,*) 'INVALS', barseas(ibr)%points(ipol)%x
          write(*,*) 'INVALS', barseas(ibr)%points(ipol)%y
          write(*,*) 'INVALS', barrier(ibr)%points(ipol)%x
          write(*,*) 'INVALS', barrier(ibr)%points(ipol)%y
          write(*,*) 'INVALS', barseas(ibr)%param(ipol,1)%amzbr
          write(*,*) 'INVALS', barseas(ibr)%param(ipol,1)%amxbrw
          write(*,*) 'INVALS', barseas(ibr)%param(ipol,1)%ampbr
        end do
        write(*,*) 'INVALS', barseas(ibr)%amzbt
        write(*,*) 'INVALS', barrier(ibr)%amzbt
      end do
      write(*,*) 'INVALS', soil(isr)%WaterErosion
      write(*,*) 'INVALS', soil(isr)%SoilRockFragments

   end subroutine echo_inputs

end module input_run_mod

