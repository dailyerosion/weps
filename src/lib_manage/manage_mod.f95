!$Author$
!$Date$
!$Revision$
!$HeadURL$

module manage_mod

  contains

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
      use manage_data_struct_defs, only: am0tfl, lastoper
      use hydro_data_struct_defs, only: hydro_derived_et
      use manage_data_struct_defs, only: man_file_struct

!     + + + PARAMETERS AND COMMON BLOCKS + + +
      include 'p1werm.inc'
      include 'manage/man.inc'
      include 'manage/asd.inc'
      include 'manage/mproc.inc'

!     + + + ARGUMENT DECLARATIONS + + +
      integer sr, startyr
      type(soil_def), intent(inout) :: soil  ! soil for this subregion
      type(biomatter), intent(inout) :: crop    ! structure containing full crop description
      type(bio_prevday), intent(inout) :: cropprev    ! structure containing crop previous day values
      type(biomatter), dimension(:), intent(inout) :: residue
      type(biototal), intent(in) :: biotot
      type(opercrop_date), dimension(:), intent(inout) :: mandate
      type(hydro_derived_et), intent(inout) :: h1et
      type(man_file_struct), intent(inout) :: manFile

!     + + + ARGUMENT DEFINITIONS + + +
!        sr - the subregion number
!     startyr - starting year of the simulation run

!     + + + LOCAL VARIABLES + + +

      integer simdd, simmm, simyr, mansimyr, manmon, manday, manyr
      character*256   line

!        simdd - current simulation day
!        simmm - current simulation month
!        simyr - current simulation year
!     mansimyr - the simulation year which corresponds to the year from the management file
!       manmon - month from the management file
!       manday - day from the management file
!        manyr - year from the management file

!     + + + SUBROUTINES CALLED + + +
!     dooper - DO OPERation is called when dates match
!     dogroup - DO GROUP is called when G code encountered
!     doproc - DO PROCess is called when P code encountered

!     + + + OUTPUT FORMATS + + +
2015     format ('Op Date ', i2,1x,i2,1x,i4,' Rot yr ',i2,' sr #',i2)
!2015     format ('Operation Date ',i2,1x,i2,1x,i4,', subregion #',i2)

!     + + + END SPECIFICATIONS + + +

!     Don't do anything if the subregion isn't in the data file.
      if (mbeg(sr).eq.mbeg(sr+1)) then
        write(*,*) 'Sub-region not in data file', sr
        return
      endif

      ! get current simulation day, month, year
      call get_simdate( simdd, simmm, simyr )

      ! reset any global variables whose setting should only be valid
      ! for one day
      call mgdreset(h1et%zirr)

      line = mtbl(mcur(sr))

!     If we aren't pointing at a date, we have a problem
      
      if (line(1:1).ne.'D') goto 901

!     Must be a space between 'D' and date in dd/mm/yyyy format
      read (line (3:12),'(i2,1x,i2,1x,i4)', err=902) manday,manmon,manyr

      ! find simulation year to which management year corresponds
      mansimyr = simyr - mod (simyr-startyr, manFile%mperod) + manyr - 1

      if (difdat (simdd,simmm,simyr,manday,manmon,mansimyr).ne.0) then
        ! management date does not match simulation date
        return
      end if

      if (am0tfl(sr) .eq. 1) then
        write (luomanage(sr),*)
        write (luomanage(sr),2015) simdd,simmm,simyr,manyr,sr
      endif

!     pass date of operation to MAIN for output purposes, used by STIR also
      lastoper(0)%day = manday
      lastoper(0)%mon = manmon
      lastoper(0)%yr = manyr
      lastoper(sr)%day = manday
      lastoper(sr)%mon = manmon
      lastoper(sr)%yr = manyr

!     Move the tbl ptr to the first operation after the date

  10  mcur(sr) = mcur(sr) + 1
      line = mtbl(mcur(sr))
      select case (line(1:1))
      case ('O')
        lastoper(sr)%skip = 0
        call dooper(sr)
      case ('G')
        if(lastoper(sr)%skip.eq.0) call dogroup(sr, soil)
      case ('P')
        if(lastoper(sr)%skip.eq.0) then

           call doproc(sr, mcount(sr), soil, crop, cropprev, residue, biotot, mandate, h1et, manFile)
        endif
      case ('D')
        call stir_report(sr, .false., lastoper(sr)%stir, lastoper(sr)%energyarea)
        read (line (3:12),'(i2,1x,i2,1x,i4)', err=902) manday,manmon,manyr
        ! find simulation year to which management year corresponds
        mansimyr = simyr - mod (simyr-startyr, manFile%mperod) + manyr - 1
        if( difdat (simdd,simmm,simyr,manday,manmon,mansimyr).ne.0) then
           ! initialize end of season / hydrobal reporting flag to true to generate a report
           rpt_season_flg(sr) = .true.
           return
        end if
      case ('*')
        call stir_report(sr, .true., lastoper(sr)%stir, lastoper(sr)%energyarea)
        mcount(sr) = mcount(sr) + 1
        mcur(sr) = mbeg(sr)
  101   mcur(sr) = mcur(sr) + 1
        line = mtbl(mcur(sr))
        if (line(1:1).ne.'D') goto 101
        return
      case ('+')
        continue
      case default
        goto 903
      end select
      goto 10

! Error stops
      
901   write(0,*) 'Enter manage not pointing at date'
      call exit (1)
902   write(0, 9902) line, sr
9902  format('Bad date format ',a,' in region ',i2)
      call exit (1)
903   write(0,*) 'Invalid management code -', line (1:1)      
      call exit (1)
    end subroutine manage

    subroutine mfinit (sr, manFile)
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
      use manage_data_struct_defs, only: lastoper, man_file_struct
      use flib_sax
      use manage_xml_mod, only: init_man_xml
      use manage_xml_mod, only: manfile_complete
      use manage_xml_mod, only: begin_element_handler, end_element_handler, pcdata_chunk_handler

      include 'p1werm.inc'
      include 'm1flag.inc'
      include 'manage/man.inc'
      include 'manage/asd.inc'
      include 'manage/tcrop.inc'
      include 'manage/mproc.inc'

!     + + + ARGUMENT DECLARATIONS + + +
      integer sr                        ! current subregion
      type(man_file_struct) :: manFile  ! management file data structure

!     + + + LOCAL VARIABLES + + +
      integer           linidx, eofidx, endidx
      character*256      line
      integer           idx
      integer           luimandate   ! unit number for reading in management file

      type(xml_t) :: fxml   ! xml file handle structure
      integer :: read_stat  ! reading file status

!     + + + SUBROUTINES CALLED + + +

!     + + + FUNCTION DECLARATONS + + +

!     + + + DATA INITIALIZATIONS + + +

      linidx=1
      mbeg(1)=1
      if (sr.ne.1) linidx = mbeg(sr)
      mcur(sr) = 0
      mcount(sr) = 0

      ! initialize values for crop effect flags
      am0kilfl = 0
      am0cropupfl = 0
      am0defoliatefl = 0

      ! initialize the manage/tcrop.inc variables

      atmstandstem(sr) = 0.0
      atmstandleaf(sr) = 0.0
      atmstandstore(sr) = 0.0

      atmflatstem(sr) = 0.0
      atmflatleaf(sr) = 0.0
      atmflatstore(sr) = 0.0

      atmflatrootstore(sr) = 0.0
      atmflatrootfiber(sr) = 0.0

      atzht(sr) = 0.0
      atdstm(sr) = 0.0
      atxstmrep(sr) = 0.0
      atzrtd(sr) = 0.0
      atgrainf(sr) = 0.0

      do idx = 1,mnsz
         atmbgstemz(idx,sr) = 0.0
         atmbgleafz(idx,sr) = 0.0
         atmbgstorez(idx,sr) = 0.0

         atmbgrootstorez(idx,sr) = 0.0
         atmbgrootfiberz(idx,sr) = 0.0
      end do
      lastoper(sr)%day = 0
      lastoper(sr)%mon = 0
      lastoper(sr)%yr = 0

      rpt_season_flg(sr) = .true.

!     + + + END SPECIFICATIONS + + +

!     read in management file

      call fopenk(luimandate, trim(manFile%tinfil), 'old')
      read(luimandate, '(a)', iostat=read_stat) line
      if (read_stat /= 0) stop "Cannot read input file"
      if ( (line (1:8).ne.'Version: ') .and. (index(line, 'xml') .gt. 0) ) then
        close(luimandate)
        ! open input file
        call open_xmlfile(trim(manFile%tinfil),fxml,read_stat)
        if (read_stat /= 0) stop "Cannot open xml input file"
        ! read in xml based input file
        call init_man_xml()
        call xml_parse(fxml, &
           begin_element_handler = begin_element_handler, &
           end_element_handler = end_element_handler, &
           pcdata_chunk_handler = pcdata_chunk_handler, &
           verbose = .false.)
        if (.not. manfile_complete) then
          write(*,*) 'Simulation run file incomplete'
          call exit(1)
        end if
        return

      else
        rewind(luimandate) 
   10   read(luimandate, '(a)', end=20) line
      select case (line(1:1))
      case ('V')  ! first line begins with word "Version: "
        goto 15
      case ('O')
        goto 15
      case ('G')
        goto 15
      case ('P')
        goto 15
      case ('D')
        goto 15
      case ('*')
        goto 15
      case ('+')
        goto 15
      case default
        goto 10
      end select
   15 mtbl(linidx) = line    !Actually add the line to the management table      
      linidx = linidx + 1
! ***      write (*,*) ' man fil: ',linidx, line
      if (linidx.le.mxtbln) goto 10
      write (0,*) 'Management table too long - ', trim(manFile%tinfil)
      call exit (1)
   20 mbeg(sr+1) = linidx
      close(luimandate)
! ***
!     debugging code to dump table
!
! ***      write(*,*) 'start dump of management file ', trim(manFile%tinfil)
! ***      do 111 linidx = mbeg(sr), mbeg(sr+1)
! ***        write(*,*) linidx, mtbl(linidx)
! ***  111 continue
! ***      write(*,*) 'end of dump'
! ***
!   
!     First need to find the version of the management file we are
!     going to read.  All files should now have a version #. ANH
      line = mtbl(mbeg(sr))

      if (line (1:8).eq.'Version: ') then
!       We have found the version # of the management file
!       Read the version into the common block variable
        read(line (10:13), *) mversion(sr)

!       Report the version to stdout
        write (6, *) 'Management file version: ', mversion(sr)

!       Test if the version is at least 1.4.  Version 1.5 adds the ability to test 
!       mversion within the operations, groups and procs so that graceful upgrades 
!       are possible.  This test version should not need to be updated as the format
!       changes.  Upgrades can be handled within the dooper, dogroup and doproc subroutines.
        if (mversion(sr) .lt. 1.40) then
!        if (line(10:13).ne.'1.40') then
           write(0,*) 'Management file version: ', mversion(sr)
           write(0,*) 'Version >= 1.40 is required for this release.'
           write(0,*) 'You need to convert ', trim(manFile%tinfil)
           write(0,*) ' to the correct format.'
           call exit (1)
        endif
      else
        write(0,*) 'Version not found in management file ', trim(manFile%tinfil)
        call exit (1)
      endif
!
      line = mtbl(mbeg(sr) + 1)
!     "*START" position found?
      if (line (1:6).eq.'*START') then

!       Obtain the number of years for the subregion's management cycle
        read (line (8:10), '(i3)', err=901) manFile%mperod

      else 
        write(0,*) '*START not second non-comment line in ', trim(manFile%tinfil)
        call exit (1)
      endif
!
! Find end and eof statements
!
      eofidx = 0
      endidx = 0
      do 30 linidx=mbeg(sr),mbeg(sr+1)-1
        line = mtbl(linidx)
        if (line (1:4).eq.'*END') then
          if (endidx.ne.0) goto 902
          endidx = linidx
        endif
        if (line (1:4).eq.'*EOF') then
          if (eofidx.ne.0) goto 903
          eofidx = linidx
        endif
   30 continue
! 
! Make sure that eof is last & end next to last
!   
      mbeg(sr+1) = eofidx+1
      line = mtbl(mbeg(sr+1) - 2)
!     "*END" position found?
      if (line (1:4).ne.'*END') goto 904

      line = mtbl(mbeg(sr+1) - 1)
!     "*EOF" position found?
      if (line (1:4).ne.'*EOF') goto 905
!
! Leave current pointer for region at first date
!
      do 40 linidx = mbeg(sr), mbeg(sr+1) - 1
        line = mtbl(linidx)
        if (line(1:1).eq.'D') goto 41
   40 continue
      goto 906
   41 mcur(sr) = linidx

! Used for debugging purposes
!       Output info about each subregion's management cycle
!        print *, 'Management filename is: ', trim(manFile%tinfil)
!        print *, 'Management cycle is ', manFile%mperod,
!     &         ' years for Subregion ', sr
!        print *, 'The *START line is: ', start(sr)
!     &         ' first operation line is: ', curnt(sr)

! *** return before dump
      return
!     debugging code to dump table
!
!      write(*,*) 'start dump of management file ', trim(manFile%tinfil)
!      do 111 linidx = mbeg(sr), mbeg(sr+1)
!        write(*,*) linidx, mtbl(linidx)
!  111 continue
!      write(*,*) 'end of dump'
!      write(*,*) 'leaving mfinit'
      return

      end if

! Error stops

  901 write(0,*) 'Error reading start param ', line(8:10)
      call exit (1)
  902 write(0,*) 'Duplicate *END statements in ', trim(manFile%tinfil)
      call exit (1)
  903 write(0,*) 'Duplicate *EOF statements in ', trim(manFile%tinfil)
      call exit (1)
  904 write(0,*) '*END not penultimate line in ', trim(manFile%tinfil)
      call exit (1)
  905 write(0,*) '*EOF not last line in ', trim(manFile%tinfil)
      call exit (1)
  906 write(0,*) 'No starting date specified in ', trim(manFile%tinfil)
      call exit (1)

    end subroutine mfinit

end module manage_mod

