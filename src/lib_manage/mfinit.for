!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!
!$Header: /weru/cvs/weps/weps.src/manage/mfinit.for,v 1.23 2007-08-22 12:59:32 wagner Exp $
!
!
      subroutine mfinit (sr, fname)
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
      include 'p1werm.inc'
      include 'wpath.inc'
      include 'm1flag.inc'
      include 'manage/man.inc'
      include 'manage/asd.inc'
      include 'manage/tcrop.inc'

!     + + + ARGUMENT DECLARATIONS + + +
      integer sr
      character fname*(*)

!     + + + ARGUMENT DEFINITIONS + + +
!     sr - current subregion
!     fname - management file name

!     + + + LOCAL VARIABLES + + +
      integer           linidx, eofidx, endidx
      character*256      line
      integer           idx,idy
      integer           luimandate   ! unit number for reading in management file

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

      do idy = 1,mnsub
         atmstandstem(idy) = 0.0
         atmstandleaf(idy) = 0.0
         atmstandstore(idy) = 0.0

         atmflatstem(idy) = 0.0
         atmflatleaf(idy) = 0.0
         atmflatstore(idy) = 0.0

         atmflatrootstore(idy) = 0.0
         atmflatrootfiber(idy) = 0.0

         atzht(idy) = 0.0
         atdstm(idy) = 0.0
         atxstmrep(idy) = 0.0
         atzrtd(idy) = 0.0
         atgrainf(idy) = 0.0

         do idx = 1,mnsz
            atmbgstemz(idx,idy) = 0.0
            atmbgleafz(idx,idy) = 0.0
            atmbgstorez(idx,idy) = 0.0

            atmbgrootstorez(idx,idy) = 0.0
            atmbgrootfiberz(idx,idy) = 0.0
         end do
      end do

!     + + + END SPECIFICATIONS + + +

!     read in management file

      call fopenk(luimandate, fname(1:len_trim(fname)), 'old')
   10 read(luimandate, '(a)', end=20) line
      if (line(1:1).eq.'#') goto 10
      if (line(1:1).eq.'T') goto 10    ! Skip new "multi-line" string variables like comment lines
      if (line(1:1).eq.'N') goto 10    ! Skip new "multi-line" management file notes like comment lines
      mtbl(linidx) = line    !Actually add the line to the management table      
      linidx = linidx + 1
! ***      write (*,*) ' man fil: ',linidx, line
      if (linidx.le.mxtbln) goto 10
      write (0,*) 'Management table too long - ', fname
      call exit (1)
   20 mbeg(sr+1) = linidx
      close(luimandate)
! ***
!     debugging code to dump table
!
! ***      write(*,*) 'start dump of management file ', fname
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
           write(0,*) 'You need to convert ', fname
           write(0,*) ' to the correct format.'
           call exit (1)
        endif
      else
        write(0,*) 'Version not found in management file ', fname
        call exit (1)
      endif
!
      line = mtbl(mbeg(sr) + 1)
!     "*START" position found?
      if (line (1:6).eq.'*START') then

!       Obtain the number of years for the subregion's management cycle
        read (line (8:10), '(i3)', err=901) mperod(sr)

      else 
        write(0,*) '*START not second non-comment line in ', fname
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
!   
! Used for debugging purposes
!       Output info about each subregion's management cycle
!        print *, 'Management filename is: ', fname
!        print *, 'Management cycle is ', mperod(sr),
!     &         ' years for Subregion ', sr
!        print *, 'The *START line is: ', start(sr)
!     &         ' first operation line is: ', curnt(sr)

! *** return before dump
      return
!     debugging code to dump table
!
!      write(*,*) 'start dump of management file ', fname
!      do 111 linidx = mbeg(sr), mbeg(sr+1)
!        write(*,*) linidx, mtbl(linidx)
!  111 continue
!      write(*,*) 'end of dump'
!      write(*,*) 'leaving mfinit'
      return
!      
! Error stops
!
  901 write(0,*) 'Error reading start param ', line(8:10)
      call exit (1)
  902 write(0,*) 'Duplicate *END statements in ', fname
      call exit (1)
  903 write(0,*) 'Duplicate *EOF statements in ', fname
      call exit (1)
  904 write(0,*) '*END not penultimate line in ', fname
      call exit (1)
  905 write(0,*) '*EOF not last line in ', fname
      call exit (1)
  906 write(0,*) 'No starting date specified in ', fname
      call exit (1)
!
      end
!
!
