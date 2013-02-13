!$Author$
!$Date$
!$Revision$
!$HeadURL$



      subroutine  write_event(luowepperod,cd, cm, cy, precp,runoff,     &
     &     irdgdx,avedet,maxdet,ptdet,avedep,maxdep,ptdep,avsole,       &
     &     enrato)
     
!
!  write_event()
!
!  This subroutine writes a information for a single event to one line of the 
!  event output file.
     
      integer, intent(in) :: luowepperod,cd,cm,cy
      real, intent(in) :: precp,runoff,irdgdx,avedet,maxdet,ptdet
      real, intent(in) :: avedep,maxdep,ptdep,avsole,enrato
      
      write(luowepperod,1100) cd,cm,cy,precp,runoff,irdgdx,avedet,      &
     &  maxdet,ptdet,avedep,maxdep,ptdep,avsole,enrato
     
      return
     
1100  format (3(i5), 3x, f5.1, 3x, f5.1, f7.3,f7.2,f7.2,f7.1,           &
     &    f7.2,f7.2,f7.1,2x,f8.3,f7.2) 
     
      end