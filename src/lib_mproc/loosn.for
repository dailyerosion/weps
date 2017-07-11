!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine loosn (u,tillf,nlay,density,sbd,laythk)


!     + + + PURPOSE + + +
     
!     This subroutine reads in the array(s) containing the components 
!     that need to be loosen/compact(ed). 

!     + + + KEYWORDS + + +
!     loosen/compact, tillage 

!     + + + ARGUMENT DECLARATIONS + + +
      integer nlay
      real    u,tillf,density(*),laythk(*),sbd(*)

!     + + + ARGUMENT DEFINITIONS + + +

!     nlay     - number of soil layers used
!     u        - loosening coefficient
!     tillf    - fraction of soil area tilled by the machine
!     density  - present soil bulk density
!     sbd      - settled soil bulk density
!     laythk   - layer thickness

!     + + + LOCAL VARIABLES + + +
      integer  i 
      real dum(nlay)       

!     + + + LOCAL VARIABLE DEFINITIONS + + +
!     dum = dummy variable used in calculating the mass in a subregion
!     i = loop variable on layers 

!     + + + END SPECIFICATIONS + + + 

!     perform the loosen/compact process on the layers in a subregion 

      do 300 i=1,nlay
         dum(i)= density(i)-((density(i)-(2.0/3.0)*sbd(i))*u*tillf)
         laythk(i)=laythk(i)*(density(i)/dum(i))
         density(i)=dum(i)
300   continue
      end
