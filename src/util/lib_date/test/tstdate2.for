!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!
c$Header: /weru/cvs/weps/weps.src/util/date/test/tstdate2.for,v 1.1.1.1 1999-03-12 17:05:31 wagner Exp $
c
      PROGRAM TST

          use datetime_mod

	  INTEGER j0,jmin, jmax, j1, j2, d1,m1,y1, d2,m2,y2,x
	  print*, 'enter jmin, jmax, x'
	  read*, jmin, jmax, x
	  DO 10 j0=jmin, jmax
		call caldat(j0,d1,m1,y1)
		j1 = julday(d1,m1,y1)
		call caldat(j1,d2,m2,y2)
		j2 = julday(d2,m2,y2)

		if (x .eq. 0) then
			if ( (j0 .ne. j1) .or. (j1 .ne. j2) ) then
				print *, j0, j1, j2, d1, m1, y1, d2, m2, y2
			else if ( (d1.ne.d2).or.(m1.ne.m2).or.(y1.ne.y2) ) then
				print *, j0, j1, j2, d1, m1, y1, d2, m2, y2
			endif
		else
			print *, j0, j1, j2, d1, m1, y1, d2, m2, y2
		endif

10    continue
	stop
	end
c
c$Log: not supported by cvs2svn $
c Revision 1.1.1.1  1995/01/18  04:20:08  wagner
c Initial checkin
c
c Revision 2.1  1992/03/27  17:22:53  wagner
c Version 2 code.
c
