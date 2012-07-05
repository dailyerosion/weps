!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine mixproc(u, nlay, xcomp, cmass, mass) 

!     + + + PURPOSE + + +
!     
!     This subroutine perfoms the actual mixing process.
!
!     + + + KEYWORDS + + +
!     mixing 
!
      include 'p1werm.inc'
!
!     + + + ARGUMENT DECLARATIONS + + +
      integer nlay  
	  real xcomp(mnsz), mass, cmass, u

!     + + + ARGUMENT DEFINITIONS + + +
!
!     cmass	- total mass of a component contained in a subregion 
!     mass	- total mass in a subregion 
!     nlay	- number of layers to be mixed
!     u		- mixing coefficient
!	  xcomp	- component value that is mixed

!     + + + LOCAL VARIABLES + + +
!
      integer i  
	  real mixed(mnsz) 
!
!     + + + LOCAL VARIABLE DEFINITIONS + + +
!
!     i		- index for layers in a subregion
!	  mixed	- temperory variable containing the mixed component 

!     + + + END SPECIFICATIONS + + + 

!     Do the mixing process. 
	  
	  do 100 i=1,nlay 
	    mixed(i) = (1-u)*xcomp(i)+(u*cmass/mass) 
		xcomp(i) = mixed(i) 
100   continue
	  
	  end 
