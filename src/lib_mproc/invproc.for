!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!
!
!
      subroutine invproc(nlay,thick,xcomp) 

      include 'p1werm.inc'

!     + + + PURPOSE + + +
!     
!	  Invert the component passed to xcomp 
!	  
!     + + + KEYWORDS + + +
!     inversion, tillage 
!
!     + + + ARGUMENT DECLARATIONS + + +

      real xcomp(mnsz), thick(mnsz)
      integer nlay
!
!     + + + ARGUMENT DEFINITIONS + + +

!     nlay		- number of soil layers used
!     thick		- thickness of each layer in a subregion
!	  xcomp		- component that needs inverting
!

!     + + + LOCAL VARIABLES + + +

      integer   idx, odx
      real      ithick(mnsz), ixcomp(mnsz)
      real      othick(mnsz)
      real      dthick

!     + + + LOCAL VARIABLE DEFINITIONS + + +
!     idx      - input index for layers
!     odx      - output index for layers
!     ithick   - inverted thickness of layers
!     ixcomp   - inverted property of layers
!     othick   - temp thickness
!     dthick   - delta thickness
!
! create inverted layer thickness and property arrays
! and zero out output array
!
      do 10 idx=1, nlay
        ithick(idx) = thick(nlay-idx+1)
        ixcomp(idx) = xcomp(nlay-idx+1)
        xcomp(nlay-idx+1) = 0.0
        othick(idx) = thick(idx)
   10 continue

      idx = 1
      odx = 1

   20 dthick = min(ithick(idx), othick(odx))
      xcomp(odx) = xcomp(odx) + ixcomp(idx) * dthick
      ithick(idx) = ithick(idx) - dthick
      othick(odx) = othick(odx) - dthick
      if (ithick(idx).eq.0.0) idx = idx + 1
      if (othick(odx).eq.0.0) odx = odx + 1
      if (idx.le.nlay.and.odx.le.nlay) goto 20

      do 30 odx = 1, nlay
        xcomp(odx) = xcomp(odx) / thick(odx)
   30 continue

      return
      end
      

! ***c     + + + LOCAL VARIABLES + + +
! ***
! ***      integer i
! ***	  real dum(mnsz),dum1(mnsz),dum2(mnsz) 
! ***	  real inverse(mnsz), depth(mnsz), oldepth(0:mnsz)
! ***	  real x, new,  p(mnsz)  
! ***c
! ***c     + + + LOCAL VARIABLE DEFINITIONS + + +
! ***c
! ***c     dum		- dummy variable used in making a property array
! ***c     dum1		- dummy variable used in making a property array
! ***c     dum2		- dummy variable used in making a property array
! ***c     depth		- depth matrix containing inverted layer depths from the surface
! ***c     i			- loop variable for soil layers
! ***c     inverse	- inverse layer thickness matrix
! ***c	  new		- new inverted property value based on interpolation or
! ***c	  			  extrapolation
! ***c     oldepth	- depth matrix containing original layer depths from the surface
! ***c     p			- property matrix after inversion
! ***c     x			- variable containing the original depths from the surface
! ***c				  used in the call to polint
! ***	   
! ***c     Do the inversion process. 
! ***c     Initialize the dummy variables to zero.
! ***
! ***	  dum(1) = 0.0
! ***	  dum1(1) = 0.0
! ***	  dum2(1) = 0.0
! ***	  oldepth(0) = 0.0
! ***
! ***	  do 100 i=1,nlay
! ***c      invert the layers (layer thickness) 
! ***	    inverse(i) = thick((nlay+1)-i)
! ***c      invert the property passed to xcomp  
! ***		p(i) = xcomp((nlay+1)-i)
! ***c     form a property array for the depth (thichness) and component based
! ***c     on the inverted matrix 
! ***	  dum(i+1) = thick(i)
! ***	  dum1(i+1) = inverse(i) 
! ***	  depth(i)=inverse(i)/2.0+dum1(i)/2.0+dum2(i) 
! ***	  dum2(i+1)= depth(i)
! ***	  oldepth(i)=thick(i)/2.0+dum(i)/2.0+oldepth(i-1)
! ***100   continue
! ***c
! ***c     make a call to subroutine polint which takes the current
! ***c     property matrix and depth matrix and either interpolates
! ***c     or extrapolates the property matrix to correspond to the
! ***c     original layer thickness before the inversion process 
! ***c     was performed.  Make call for each layer (nlay). 
! ***c
! ***      do 200 i=1,nlay
! ***        x=oldepth(i) 
! ***	    call polint(depth, p, nlay, x, new)
! ***c
! ***c     set the component for a layer equal to the interpolated
! ***c     or extrapolated value calculated in polint.for 
! ***c
! ***	    xcomp(i) = new
! ***200   continue   
! ***	  end 
