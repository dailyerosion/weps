!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!
c$Header: /weru/cvs/weps/weps.src/util/date/test/tstdate3.for,v 1.1.1.1 1999-03-12 17:05:31 wagner Exp $
c
	integer id, im, iy, jj, jjulday
	read*, id, im, iy
	jj = jjulday(id, im, iy)
	stop
	end
c
c     JULDAY is taken from _Numerical_Recipes:_The_Art_of_Scientific_Computing_
c
      integer   function   jjulday
     i                           (d1, m1, yyy1)
c
c     + + + PURPOSE + + +
c     In this routine JULDAY returns the Julian Day Number which begins at
c     noon of the calendar date specified by day "d1", month "m1", & year "yyy1"
c     All are integer variables. Positive year signifies A.D.; negative, B.C.
c     Remember that the year after 1 B.C. was 1 A.D.
c
c     + + + KEYWORDS + + +
c     date, utility
c
c     + + + ARGUMENT DECLARATIONS + + +
      integer   d1, m1, yyy1
      integer   igreg
c
c
c     + + + ARGUMENT DEFINITIONS + + +
c     d1     - integer value of day in the range 1-31
c     m1     -                  month in the range 1-12
c     yyy1   -                  year (negative A.D., positive B.C.)
c
c     + + + PARAMETERS + + +
c     Gregorian Calendar was adopted on Oct. 15, 1582.
      parameter   (igreg=15+31*(10+12*1582))
c
c     + + + LOCAL VARIABLES + + +
      integer   jy, jm, ja
c
c     + + + END SPECIFICATIONS + + +
c
      if (yyy1.lt.0) yyy1=yyy1+1
      if (m1.gt.2) then
         jy=yyy1
         jm=m1+1
      else
         jy=yyy1-1
         jm=m1+13
      endif
c     jjulday=int(dble(365.25)*dble(jy))+int(dble(30.6001)*dble(jm))+d1+1720995
      jjulday=int(365.25*jy)+int(30.6001*jm)+d1+1720995
	  print*, jjulday
      if (d1+31*(m1+12*yyy1).ge.igreg) then
         ja=jy/100
c         ja=int(dble(0.01)*dble(jy))
         print*, ' division',jy/100
		 print*,' ja',ja, dble(0.01)*dble(jy),int(0.01*jy)
	     print*,' idint', idint(dble(0.01)*dble(jy)),idint(0.01*jy)
         jjulday=jjulday+2-ja+int(dble(0.25)*dble(ja))
		 print*, jy, (0.01*jy), int(0.01*jy),ja, jjulday
		 print*, 0.25*ja, int(0.25*ja)
		 print*, (dble(0.25)*dble(ja)),int(dble(0.25)*dble(ja))
      endif
      return
      end
c
c$Log: not supported by cvs2svn $
c Revision 1.1.1.1  1995/01/18  04:20:08  wagner
c Initial checkin
c
c Revision 2.2  1992/04/03  23:14:07  wagner
c located and incorporated fix for roundoff
c problem that existed with MS FORTRAN 5.1
c
c Revision 2.1  1992/03/27  17:22:53  wagner
c Version 2 code.
c
