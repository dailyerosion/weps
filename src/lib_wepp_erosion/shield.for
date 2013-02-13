!$Author$
!$Date$
!$Revision$
!$HeadURL$

      function shield(reyn)
      implicit none
!
!     + + + purpose + + +
!
!     function shield generates parameters (shield parameters)
!     by interpolating values from a table (shield diagram)
!     for given reynolds numbers.
!
!     called from: srs trncap, yalin
!     author(s): ascough ii, r. van der zweep, v. lopes
!     reference in user guide:
!
!     version:
!     date recoded:
!     recoded by: jim ascough ii
!
!     + + + keywords + + +
!
!     + + + parameters + + +
!
!     + + + argument declarations + + +
!
      real, intent(in) :: reyn
!
!     + + + argument definitions + + +
!
!     reyn -
!
!     + + + common blocks + + +
!
!     + + + local variables + + +
!
      real y(8), r(8), shield, slope, ycr
      integer i
!
!     + + + local definitions + + +
!
!     real variables
!
!     y(8)   -
!     r(8)   -
!     shield -
!     slope  -
!     ycr    -
!
!     integer variables
!
!     i -
!
!     + + + saves + + +
!
!     save  - removed 3/30/2007 jrf
!
!     + + + subroutines called + + +
!
!     + + + data initializations + + +
!
      data y /0.0772, 0.0579, 0.04, 0.035, 0.034, 0.045, 0.055, 0.057/
      data r /1.0, 2.0, 4.0, 8.0, 12.0, 100.0, 400.0, 1000.0/
!
!     + + + end specifications + + +
!
!
      if (reyn.lt.r(1)) then
         i = 2
         slope = (alog(y(i))-alog(y(i-1))) / (alog(r(i))-alog(r(i-1)))
         ycr = alog(y(1)) - slope * (alog(r(1))-alog(reyn))
      else if (reyn.gt.r(8)) then
         i = 8
         slope = (alog(y(i))-alog(y(i-1))) / (alog(r(i))-alog(r(i-1)))
         ycr = y(8) + slope * (alog(reyn)-alog(r(8)))
!     
      else
!        
         do 10 i = 2, 8
!           
            if (reyn.ge.r(i-1).and.reyn.le.r(i)) then
               slope = (alog(y(i))-alog(y(i-1))) / (alog(r(i))-         &
     &             alog(r(i-1)))
               ycr = alog(y(i-1)) + slope * (alog(reyn)-alog(r(i-1)))
               go to 20
            end if
!        
   10    continue
!     
      end if
!     
   20 shield = exp(ycr)
!     
      return
      end
