!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!
c$Header: /weru/cvs/weps/weps.src/util/date/test/tstdate.for,v 1.1.1.1 1999-03-12 17:05:31 wagner Exp $
c
      program tstdat
c     + + + PURPOSE + + +
c     To test JULDAY function and CALDAT subroutine
c
c     + + + KEYWORDS + + +
c     date, utility, test
c
c     + + + ARGUMENT DECLARATIONS + + + 
      integer dd, mm, yyyy, day, month, year
c
c     + + + ARGUMENT DEFINITIONS + + +
c     dd     - day   --\
c     mm     - month    >-- parsed from input
c     yyyy   - year  --/
c
c     day    - day   --\
c     month  - month    >-- returned from CALDAT
c     year   - year  --/
c
c     + + + LOCAL VARIABLES + + +
      integer nod, wk
c
c     + + + SUBROUTINES CALLED + + +
c     caldat, mvdate
c
c     + + + FUNCTION DECLARATIONS + + +
      integer julday, lstday, difdat
	  integer dayear, wkday, wkjday
      logical isleap
c
c     + + + INPUT FORMATS + + +
 1000  format (i2, 1x, i2, 1x, i4)
c
c     + + + OUTPUT FORMATS + + +
 2000 format ('Date Entered?    : ', i2.2, '/', i2.2, '/', i4.4) 
 2005 format ('Substituting date: ', i2.2, '/', i2.2, '/', i4.4) 
 2006 format ('Day of year is   : ', i3.3)
 2010 format ('A week ago it was: ', i2.2, '/', i2.2, '/', i4.4)
 2020 format ('Tommorrow will be: ', i2.2, '/', i2.2, '/', i4.4)
 2030 format ('There are ', i2, ' more days til payday')
 2040 format ('Payday is on Friday, the ', i2, 'th.')
c
c     + + + END SPECIFICATIONS + + +
c
      print*, 'Enter a date (dd/mm/yyyy)'
      read 1000, dd, mm, yyyy
      call caldat (julday (dd, mm, yyyy), day, month, year)
      print 2000, day, month, year
c
c     day, month, year contains corrected date (i.e. 31/09/XXXX => 01/10/XXXX)
      if (day.ne.dd) then
         print*, 'The date entered does not exist or algorithm ERROR.'
         print 2005, day, month, year 
c        dd, mm, yyyy are now corrected also
         dd=day
         mm=month
         yyyy=year
      endif
c
      if (isleap(yyyy)) then
          print*, 'This is a leap year'
      else
          print*, 'This is not a leap year'
      endif
      print 2006, dayear(dd,mm,yyyy)
      wk = wkday(dd,mm,yyyy)
      if (wk .eq. 0) then
          print*, 'This is a Monday'
      else if (wk .eq. 1) then
          print*, 'This is a Tuesday'
      else if (wk .eq. 2) then
          print*, 'This is a Wednesday'
      else if (wk .eq. 3) then
          print*, 'This is a Thursday'
      else if (wk .eq. 4) then
          print*, 'This is a Friday'
      else if (wk .eq. 5) then
          print*, 'This is a Saturday'
      else if (wk .eq. 6) then
          print*, 'This is a Sunday'
      endif
      if (wk .eq. wkjday(julday(dd,mm,yyyy))) then
          print*, 'I agree with the day of the week'
      else
          print*, 'I disagree with weekday - check wkday/wkjday'
      endif
      print*, 'Last day of the month is: ', lstday (mm, yyyy)
      print*, 'Julian date is: ', julday (dd, mm, yyyy)
      call mvdate (-7, dd, mm, yyyy, day, month, year)
      print 2010, day, month, year
      call mvdate (1, dd, mm, yyyy, day, month, year)
      print 2020, day, month, year
      nod=difdat (dd, mm, yyyy, lstday (mm, yyyy), mm, yyyy)+1
      if (nod.eq.1) then
         print*, 'There is 1 more day till the next month.'
      else
         wk=wkjday(julday(lstday (mm, yyyy),mm,yyyy) + 1)
         day = lstday(mm, yyyy)
         if (wk .eq. 5) then
           nod = nod - 1
           print 2030, nod
           day = day - 1
           print 2040, day
         else
           if (wk .eq. 6) then
             nod = nod - 2
             print 2030, nod
             day = day - 2
             print 2040, day
           else
             print 2030, nod 
             print*, 'PS: Pay day is first of next month.'
           endif
         endif
      endif
      end
c
c$Log: not supported by cvs2svn $
c Revision 1.1.1.1  1995/01/18  04:20:08  wagner
c Initial checkin
c
c Revision 2.2  1993/03/08  18:58:35  dudley
c pay day function is more acurrate! :)
c
c Revision 2.1  1992/03/27  17:22:53  wagner
c Generic test routine for the date functions.
c It does not exercise any routines thoroughly
c but does exercise all of them some.
c
c Version 2 code.
c
