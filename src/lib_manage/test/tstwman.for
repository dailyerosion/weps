!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!
      program tstwman

*$noereference
      include 'p1werm.inc'
      include 'm1subr.inc'
      include 's1layr.inc'
      include 's1agg.inc'
      include 'manage/man.inc'
      include 'manage/asd.inc'
*$reference

      integer lay, sr, mm, dd, yyyy, syear, eyear
      character *30 manfil
      integer thisyr
      logical fexist
      data mm, dd, yyyy, syear, eyear
     &    /01, 01, 1992, 1992,  2001/

c     Init the management data file
      nsubr = 3
      nslay(1) = 5
      nslay(2) = 5
      aszlyt(1,1) = 1.0
      aszlyt(2,1) = 2.0
      aszlyt(3,1) = 3.0
      aszlyt(4,1) = 5.0
      aszlyt(5,1) = 5.0
      aszlyt(1,2) = 1.0
      aszlyt(2,2) = 2.5
      aszlyt(3,2) = 5.0
      aszlyt(4,2) = 3.0
      aszlyt(5,2) = 3.0

      fexist = .FALSE.

 01   write(*,*) 'Enter MANAGEMENT input file (return for default)'
      read (*,'(a)') manfil
      inquire(file=manfil, exist = fexist)
 05   if (.not. fexist) then
        write(*,*) 'MANAGEMENT input file not found.  Using default...'
        manfil = 'mngmnt2.dat'
        inquire(file=manfil, exist = fexist)
        if (.not. fexist) then
          write(*,*) 'Default file not found!'
          goto 01
        endif
        goto 05
      endif

      do 200 sr=1, nsubr
        do 100 lay=1, nslay(sr)
          aslagm(lay, sr) = 10*sr+5*lay
          as0ags(lay, sr) = 4 + lay
          aslagn(lay, sr) = 0.01 * sr
          aslagx(lay, sr) = 100.0 + sr
100     continue
200   continue

      call mfinit (nsubr,manfil)

c     Now in the mfinit routine - LEW
c     Init the asd stuff
c     call asdini ()
c     -------------------------------------------------------------------------

      print *, 'Starting up daily time step'
c     Cycle through a calendar year.
c
c     <while> year is <= eyear
 10   do 20 sr=1, nsubr
         call manage (sr, dd, mm, yyyy, syear)
 20   continue
      thisyr = yyyy
      call mvdate (1, dd, mm, yyyy, dd, mm, yyyy)
      if (thisyr .ne. yyyy) print *, 'End of year: ', thisyr
      if (yyyy.le.eyear) go to 10
c     <end while>
c
      close (unit=10)
      stop
      end
