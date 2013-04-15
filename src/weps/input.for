!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine   input( n_rot_cycles )

!     + + + PURPOSE + + +
!     This subroutine perforns some screen I/O, reads in the
!     run files and performs various error checkings.

!     author: John Tatarko
!     version: 95.08

!     EDIT History
!     06-Feb-99   wjr   changed crop_db, etc., location
!                       to be scenario dir
!     06-Feb-99   wjr   made inprun and inpsub into separate subrs
!     06-Feb-99   wjr   used select statement to read config files
!     06-Feb-99   wjr   moved opening of sinfil from inprun to inpsub
!     06-Feb-99   wjr   changed files.inc to file.inc
!     06-Feb-99   wjr   added luolog & luodbg
!     06-Nov-03   LEW   removed cmdline processing and put in "cmdline.for"

!     + + + KEY WORDS + + +
!     WEPS, cligen, windgen

!     + + + GLOBAL COMMON BLOCKS + + +
      use weps_interface_defs

!     + + + ARGUMENT DECLARATIONS + + +
      integer, intent(out) :: n_rot_cycles

      include 'p1werm.inc'
      include 'wpath.inc'
      include 'm1subr.inc'
      include 'm1sim.inc'
      include 'm1flag.inc'
      include 'm1dbug.inc'
      include 'command.inc'

!     + + + LOCAL COMMON BLOCKS + + +
      include 'main/main.inc'

!     + + + LOCAL VARIABLES + + +
      logical       fexist

!     + + + LOCAL DEFINITIONS + + +
!   fexist    - flag used to indicate result of file existence

!     + + + END SPECIFICATIONS + + +

!     open simulation run file
      runfil = rootp(1:len_trim(rootp)) // 'weps.run'
      inquire(file = runfil(1:len_trim(runfil)), exist = fexist)
      if (.not.fexist) then
        write(0,*) ' simulation run file not found '
        call exit(1)
      end if

!     load the simulation run file
      call inprun(n_rot_cycles)

!     If this is a simulation that does water erosion read any extra WEPP
!     input data.
      if (run_erosion.gt.1) call inpwepp

      return
      end

