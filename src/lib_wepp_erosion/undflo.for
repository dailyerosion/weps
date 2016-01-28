!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine undflo(factor,expon)
      
      use wepp_interface_defs, ignore_me=>undflo
      implicit none
!
!     subroutine undflo protects against numeric underflows 
!     and overflows
!
!     module adapted from wepp version 2004.7 and called from 
!     subroutine enrich and functions depeqs/depend, 
!
!     author(s): d.c. flanagan and j.c. ascough ii
!     date last modified:  9-30-2004
!
!     + + + argument declarations + + +
!
      real, intent(inout) :: factor, expon
!
!     + + + argument definitions + + +
!
!     factor -
!     expon  -
!
!     + + + local variables + + +
!
      real exp10, power
!
!     + + + local variable definitions + + +
!
!     exp10 -
!     power -
!
!     + + + data initializations + + +
!
      data power /30.0/
!
!     begin subroutine undflo
!
      if (factor.gt.0.0) then
         exp10 = expon * alog10(factor)
!        
         if (abs(exp10).gt.power) then
            factor = 0.0
            expon = 1.0
         end if
!     
      end if
!     
      return
      end
