!$Author$
!$Date$
!$Revision$
!$HeadURL$
      subroutine inprun( n_rot_cycles )
! ***************************************************************** wjr
! reads weps simulation run file
!
!     Edit History
!     06-Feb-99   wjr   created from existing code, select added

!     + + + Modules Used + + +
      use weps_interface_defs
      use Polygons_Mod
      use subregions_mod
      use file_io_mod, only: fopenk, luicli, luiwin, luiwsd,            &
     &                       luomanage, luolog
      use erosion_data_struct_defs, only: subday, ntstep, am0efl

!     + + + ARGUMENT DECLARATIONS + + +
      integer, intent(out) :: n_rot_cycles

      include 'p1werm.inc'
      include 'wpath.inc'
      include 'm1subr.inc'
      include 'm1sim.inc'
      include 'm1geo.inc'
      include 'm1flag.inc'
      include 'm1dbug.inc'
      include 's1layr.inc'
      include 's1surf.inc'
      include 's1phys.inc'
      include 's1agg.inc'
      include 's1dbh.inc'
      include 's1dbc.inc'
      include 's1sgeo.inc'
      include 'c1gen.inc'
      include 'h1hydro.inc'
      include 'h1scs.inc'
      include 'h1db1.inc'
      include 'w1clig.inc'

!     + + + LOCAL COMMON BLOCKS + + +
      include 'main/main.inc'

!     + + + LOCAL VARIABLES + + +
      integer    :: nsubr
      integer       i, isr, iar, ios, ibr, ipol
      character     line*256
      real          sclsim, sclbar
      real          cligen_version
      logical       fexist
      real          wepsrun_version
      integer       subr_np
      integer       lui1
      integer    :: alloc_stat
      character*80     awwisn   ! made local since not used anywhere else

!     + + + Local Variable Definitions + + +
!     i   - local index counter
!     isr - subregion index counter
!     iar - accounting region index counter
!     ios - input output status flag
!     ibr - barrier index counter
!     ipol - polygon points index counter

!     line - character array to hold contents of input line

!     sclsim - scaling factor used by interface, not within WEPS
!     sclbar - scaling factor used by interface, not within WEPS

!     cligen_version - version of the specified cligen file

!     fexist - flag indicating existence of file

!     wepsrun_version - version of the weps.run file being read

!     subr_poly - polygons defining each subregion extent
!     subr_np - number of points in polygon read from file
!     lui1 - unit number for input of weps.run file

      integer linnum, typidx
!      data linnum /0/, typidx /0/

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
          write(*,*) 'WEPS run file not subregion enabled'
          stop
      end if

!     read simulation run file
  100 linnum = linnum + 1

      if( typidx .eq. 42 ) go to 200

      read (lui1,'(a)',err=80) line
!
! skip comment lines
      if (line(1:1) .eq. '#') go to 100
!
!!use case statement to appropriately assign values
      typidx = typidx + 1
      select case (typidx)
      case (1)
        usrnam = line(1:80)
      case (2)
        usrid = line(1:80)
        read (usrid((index(usrid,"|",back=.true.)+1):),*,err=80,        &
     &        iostat=ios) n_rot_cycles
        print *, 'n_rot_cycles', n_rot_cycles
      case (3)
        usrloc = line(1:80)
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
        read (line,*,err=80) awclsn
      case (8)
        read (line,*,err=80) awwisn
      case (9)
        read (line,*,err=80) id,im,iy
      case (10)
        read (line,*,err=80) ld,lm,ly
        if (((id .lt. 1) .or. (id .gt. lstday(im,iy))) .or. ((ld .lt. 1)&
     &    .or. (ld .gt. lstday(lm,ly)))) then
          write (*,2240)
          goto 80
        end if
        if (((id .lt. 1) .or. (id .gt. 31)) .or. ((ld .lt. 1) .or. (ld  &
     &    .gt. 31))) then
          write (*,2250)
          goto 80
        end if
        if (((im .lt. 1) .or. (im .gt. 12)) .or. ((lm .lt. 1) .or. (lm  &
     &    .gt. 12))) then
          write (*,2250)
          goto 80
        end if
        if (((iy .lt. 0) .or. (iy .gt.9999)) .or. ((ly .lt. 0) .or. (ly &
     &    .gt. 9999))) then
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


!     read CLIGEN file name
      case (12)
        write(luolog, *) 'line0: ', line
        write(luolog, *) 'line1: ', line(1:len_trim(line))
        clifil = rootp(1:len_trim(rootp)) // line(1:len_trim(line))
!     open CLIGEN run file
        write(luolog, *) 'line2: ', line
        write(luolog, *) 'clifil: ', clifil
        write(luolog, *) 'len: ', len(clifil), len_trim(clifil)
        call fopenk (luicli, clifil, 'old')
        write(luolog,*) 'opened cligen file to determine db format...'
!     read 1st line of CLIGEN file

        read(luicli,fmt="(a)",err=90) line
        write(6,*) '1st cligen output line is: ', line
!
! I think this is pretty messy.  It was working with the Lahey compiler
! with a "73x,f" format but the Sun F95 compiler didn't like that, so
! it was changed to "73x,f6.3".  I am now assuming that the "old versions"
! of cligen had the version number there.  Anyway, I had to change from
! "f" to "f6.3" for the Sun compiler on the second read of the line string.
!
        ! Probably not a very robust way to do this
        read(line,fmt="(73x,f6.3)",err=90) cligen_version
        if (cligen_version <= 5.1) then   ! assume new version of cligen
           read(line,fmt="(f6.3)",err=90) cligen_version
        end if

        write(luolog,*) 'cligen version: ', cligen_version
        write(6,*) 'cligen version: ', cligen_version

! I assume this is where I read the old cligen's version info
!       read(luicli,fmt="(73x,f)",err=90) cligen_version
!       write(luolog,*) 'cligen version: ', cligen_version

        ! We will now check the header to determine which cligen data file
        ! format we are reading, either the old one or the new one.
!       if (index(line,'CLIGEN VERSION 5.101') > 0 ) then

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
		goto 30

        !check for errors opening cli_gen data file here
   90   write(*,9002) clifil, line
        goto 80

   30   continue
      case (13)
!     read WINDGEN file name
        winfil = rootp(1:len_trim(rootp)) // line
!     open WINDGEN file
        call fopenk (luiwin, winfil, 'old')
        ! We will now check the header to determine which wind_gen data file
        ! format we are reading, either the old one (daily max and min wind
        ! speed, etc.) or the new one (24 hourly values per day).
        ! We now have a global wind_gen format flag we will set once we know.
        read(luiwin,fmt="(a80)",err=91) line
!       write(6,*) 'line:', line
        if (index(line,'WIND_GEN4') > 0 ) then
           wind_gen_fmt_flag = 2
        else if (index(line,'WIND_GEN3') > 0 ) then
           wind_gen_fmt_flag = 2
        else if (index(line,'WIND_GEN2') > 0 ) then
           wind_gen_fmt_flag = 2
        else
           wind_gen_fmt_flag = 1
        endif
        rewind luiwin
		goto 40

        !check for errors opening wind_gen data file here
   91   write(*,9002) winfil, line
9002    format('Error in file: ',a,' reading: ',a)
        goto 80

   40   continue
      case (14)
!     read subdaily wind file name
        if (line(1:4) .ne. 'none') then
          subfil = rootp(1:len_trim(rootp)) // line

!     inquire(file = subfil, exist = fexist)
!      if(.not. fexist) then
!         write(*,*) '      '
!         write(*,*) ' warning, the subdaily wind file:'
!         write(*,*) subfil,'was not found - all winds will be generated'
!      end if

!     open sub-daily wind file (i.e.'real' data) if it exists
          inquire(file = subfil, exist = fexist)
          if(fexist) then
            write(*,2270) subfil
 2270 format (/,' using the sub-daily wind file: ',a80)
            call fopenk (luiwsd, subfil, 'old')
          endif
        endif
      case (15)
!     read the flags to select the various general report forms
        read (line,*,err=80) (gnrpt(i), i=1,6)
!     read code to select period for output
!     yearly and simulation summaries are always given
      case (16)
        read (line,*,err=80) erosrpt
!
!     read flags to print submodel output
      case (17)
        read (line,*,err=80) am0hfl,am0sfl,am0tfl,am0cfl,am0dfl,am0efl
        if (am0tfl .eq. 1) call fopenk(luomanage,                       &
     &     rootp(1:len_trim(rootp)) // 'manage.out', 'unknown')

      case (18)
        ! debug flag line. Add zero integer to end to make sure six values
        ! are available to read. Previously interface only set 5 flags.
        ! Now should set six.
        line = line(1:len_trim(line)) // ' 0'
        read (line,*,err=80) am0hdb,am0sdb,am0tdb,am0cdb,am0ddb,am0edb

      case (19)
        read (line,*,err=80) amasim
      case (20)
        read (line,*,err=80) amxsim(1,1), amxsim(2,1)
      case (21)
        read (line,*,err=80) amxsim(1,2),amxsim(2,2)
        ! compute the simulation area
        sim_area = (amxsim(1,2) - amxsim(1,1)) *                        &
     &             (amxsim(2,2) - amxsim(2,1))
        write(6,*) "Simulation area (m^2)", sim_area
        !write(6,*) amxsim(2,1),amxsim(1,1),amxsim(2,2),amxsim(1,2)
      case (22)
 !       These values are scaling factors for interface, not used in WEPS
        read (line,*,err=80) sclsim, sclbar
      case (23)
        read (line,*,err=80) nacctr
        ! set counter iar for reading in next lines
        iar = 1
      case (24)
        read (line,*,err=80) amxar(1,1,iar), amxar(2,1,iar)
     
      case (25)
        read (line,*,err=80) amxar(1,2,iar), amxar(2,2,iar)
        iar = iar + 1
        if( iar .le. nacctr ) then
           ! read another accounting region
           typidx = typidx - 2
        end if

      case (26)
        ! read Subregion count
        read (line,*,err=80) nsubr

        ! set up isr for reading in next lines for each subregion
        isr = 1

        ! create array of subregion polygons
        allocate(subr_poly(nsubr))

      case (27)
        ! read subregion polygon point count
        read (line,*,err=80) subr_np
        ! create polygon point storage
        subr_poly(isr) = create_polygon(subr_np)
        ! set counter for reading each point pair
        ipol = 1

      case (28)
        ! read point pair
        read (line,*,err=80) subr_poly(isr)%points(ipol)%x,             &
     &                       subr_poly(isr)%points(ipol)%y
        ! read next point pair
        ipol = ipol + 1
        if( ipol .le. subr_np ) then
            ! read another point pair
            typidx = typidx - 1
        end if

      case (29)
        !        The new "versioned" IFC files contain a slope value
        !        which will be used if this value is set negative, 
        !        ie. not entered. It is now the only way to set a 
        !        non default slope when using the older "non-versioned"
        !        IFC files.   
        read (line,*,err=80) amrslp(isr)        ! weps.run file has slope gradient (m/m)

      case (30)
        read (line,*,err=80) SoilRockFragments(isr)   
        write(6,*) 'SoilRockFragments = ', SoilRockFragments(isr)

      case (31)
!     read in initial field conditions file name
        sinfil(isr) = rootp(1:len_trim(rootp)) // line           

      case (32)
!     read in management file name
        tinfil(isr) = rootp(1:len_trim(rootp)) // line

      case (33)
        read (line,*,err=80) WaterErosion(isr)
        write(*,*) "WaterErosion",WaterErosion(isr)

        ! this is last item in subregion group
        ! index to next subregion or continue on
        isr = isr + 1
        if (isr .le. nsubr) then
          typidx = typidx - 7
        end if

      case (34)
!       read in barrier info
        read (line,*,err=80) nbr
 !     write(6,*) ' reading barriers ', nbr
        if( nbr .lt. 1 ) then
           ! skip reading barrier information
           typidx = typidx + 6
        else
           ! set index for first barrier
           ibr = 1
        end if
      case (35)
        read (line,*,err=80) amxbr(1,1,ibr), amxbr(2,1,ibr)
      case (36)
        read (line,*,err=80) amxbr(1,2,ibr), amxbr(2,2,ibr)
      case (37)
        read (line,*,err=80) amzbt(ibr)
      case (38)
        read (line,*,err=80) amzbr(ibr)
      case (39)
        read (line,*,err=80) amxbrw(ibr)
      case (40)
        read (line,*,err=80) ampbr(ibr)

!      write(6,*) 'Barrier Number: ',ibr,'before (x,y)'
!      write(6,*) amxbr(1,1,ibr), amxbr(2,1,ibr)
!      write(6,*) amxbr(1,2,ibr), amxbr(2,2,ibr)

!  Convert (x,y) barrier rectangular corner coordinates to
!  (x,y) midline coordinates and width as currently defined in WEPS.

!  I don't like the different coordinate systems within WEPS.
!  We should eventually move to a uniform spatial coordinate
!  system for all spatial objects (simulation region, subregions,
!  accounting regions, barriers, etc.).  LEW AUG 23, 2000  8:04 AM
!
!  NOTE:  We don't convert to true midline coordinates because
!         the erosion submodel assumes the midline is the barrier
!         edge at this time.  Since WEPS 1.0 only handles barriers
!         that exist on the simulation region (field) boundary, the
!         the barrier (x,y) coordinates are set to match the simulation
!         region boundary coordinates, not to the actual barrier
!         midline coordinates.  LEW AUG 23, 2000  8:07 AM

!     if (Xs1 == Xb1) && (Xs2 == Xb2) then N or S barrier
!        if (Ys1 == Yb2) then S barrier (Ys1 >= Yb1)
!        if (Ys2 == Yb1) then N barrier (Ys2 <= Yb2)
!     if (Ys1 == Yb1) && (Ys2 == Yb2) then E or W barrier
!        if (Xs1 == Xb2) then E barrier (Xs1 >= Xb1)
!        if (Xs2 == Xb1) then W barrier (Xs2 <= Xb2)

      if ((amxsim(1,1) .eq. amxbr(1,1,ibr)) .and.                       &
     &   (amxsim(1,2) .eq. amxbr(1,2,ibr))) then          ! N or S barrier

         if (amxsim(2,1) .eq. amxbr(2,2,ibr)) then       ! S barrier
            amxbr(2,1,ibr) = amxsim(2,1)
!            write(6,*) 'South barrier'
         else if (amxsim(2,2) .eq. amxbr(2,1,ibr)) then  ! N barrier
            amxbr(2,2,ibr) = amxsim(2,2)
!            write(6,*) 'North barrier'
         endif

      else if ((amxsim(2,1) .eq. amxbr(2,1,ibr)) .and.                  &
     &   (amxsim(2,2) .eq. amxbr(2,2,ibr))) then          ! E or W barrier

         if (amxsim(1,1) .eq. amxbr(1,2,ibr)) then       ! W barrier
            amxbr(1,1,ibr) = amxsim(1,1)
!            write(6,*) 'West barrier'
         else if (amxsim(1,2) .eq. amxbr(1,1,ibr)) then  ! E barrier
            amxbr(1,2,ibr) = amxsim(1,2)
!            write(6,*) 'East barrier'
         endif
      else
         write(6,*) 'No barrier match for barrier: ', ibr
      endif

!      write(6,*) 'Barrier Number: ',ibr,'after (x,y)'
!      write(6,*) amxbr(1,1,ibr), amxbr(2,1,ibr)
!      write(6,*) amxbr(1,2,ibr), amxbr(2,2,ibr)

        ibr = ibr + 1
        if (ibr.le.nbr) typidx=typidx-6

      case (41)
        ! this does nothing but skip the line for shape name

      case (42)
        ! this does nothing but skip the line for shape radius
      end select
      goto 100
!
   80 write(0,9001) runfil, linnum, typidx, line
9001  format('Error in file ',a,' on line #',i4,i3,' ',a)
      call exit(1)
  200 close (lui1)
     
!
! Format statements
!
 2220 format (/,' error, latitude is not between -90. and 90. degrees',/&
     &,' - please check run file')
 2230 format (/,' error, longitude is not between -180. and 180. degrees&
     &',/,'  - please check run file')
 2240 format (/,' error, initial or last day of simulation is out of bou&
     &nds',/,'  - please check run file')
 2250 format (/,' error, initial or last day or month of simulation is o&
     &ut of bounds',/,'  - please check run file')
 2260 format (/,' error, initial or last year of simulation is not betwe&
     &en 0 and 99',/,'  - please check run file')
 2265 format (/,' error, initial year of simulation is greater than the &
     &last year of simulation',/,'  - please check run file')
      end

