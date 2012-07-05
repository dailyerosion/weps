!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!
c $Header: /weru/cvs/weps/weps.src/util/math/test/tstmath.for,v 1.1.1.1 1999-03-12 17:05:32 wagner Exp $
c
c     program tsttutil2.for

      integer i

      real erf,z

C     -------------------------------------------------------------------------
C
      do 20 i=0, 1500 
         z = z + .001
         print*, z, erf(z)
20    continue

      stop
      end
