!$Author: wagner $
!$Date: 2008-11-04 15:57:25 -0600 (Tue, 04 Nov 2008) $
!$Revision: 9882 $
!$HeadURL: https://svn.weru.ksu.edu/weru/weps1/trunk/weps.src/main/inprun.for $
      subroutine open_cli_win(isr)
! ***************************************************************** wjr
! reads weps simulation run file
!
!     Edit History
!     06-Feb-99   wjr   created from existing code, select added
!
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
      include 'd1gen.inc'
      include 'd1glob.inc'
      include 'h1hydro.inc'
      include 'h1scs.inc'
      include 'h1db1.inc'
      include 'w1clig.inc'
      include 'w1wind.inc'
      include 'file.inc'

!     + + + LOCAL COMMON BLOCKS + + +
      include 'main/main.inc'
!     + + + LOCAL VARIABLES + + +
      integer       i, isr, iar, ios, ibr
      character     line*256
      real          sclsim, sclbar
      real          cligen_version
      logical       fexist
      real          wepsrun_version

!     + + + FUNCTION DECLARATIONS + + +
!      integer   julday
       integer   lstday
 
      integer linnum, typidx
!      data linnum /0/, typidx /0/
      wepsrun_version = -1.0
      linnum = 1
      typidx = 0
 
!     open cli_gen.cli file
   
        clifil = rootp(1:len_trim(rootp)) //'cli_gen.cli' 

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
    
   30   continue

!     read WINDGEN file name
        winfil = rootp(1:len_trim(rootp)) //'win_gen.win' 
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
	        
        !check for errors opening wind_gen data file here
   91   write(*,9002) winfil, line

9002    format('End in file: ',a,' reading: ',a)

      end

