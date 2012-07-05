      subroutine sheart(q,sslope,rspace,wdhtop,frcsol,frctrl,shearq)
      
      use wepp_interface_defs
      
      implicit none
c
c     compute rill width adjustments, chezy's coefficient for rill
c     flow, flow depth, wetted area, wetted perimeter, hydraulic
c     radius, and shear stress at the end of the slope
c
c     module adapted from wepp version 2008.907 and called from 
c     subroutine param
c
c     author(s): d.c. flanagan and j.c. ascough ii
c     date last modified: 11-6-2008
c
      real, intent(inout) :: q,sslope,wdhtop
      real, intent(in) :: rspace,frcsol,frctrl
      real, intent(out) :: shearq
c
c     q      - flow discharge (m**3/s)
c     sslope - channel slope (m/m)
c
      real dz, tol, u, chezch, sinang, wp, xsarea, hydrad
      real accgav,wtdens,dpthch
c
c      u      - portion of uniform flow equation
c      wdthck - test width to check against to see if wider than
c               current rill width
c      chezch - chezy c - roughness factor
c      dz     - trial valve for channel depth (m)
c      xsarea - cross sectional area of the flow (m**2)
c      wp     - wetted perimeter of the flow (m)
c      hydrad - hydraulic radius (m)
c      sinang - sine of slope angle
c      tol    - tolerance value
c
!      save
c
      accgav = 9.807
      wtdens = 9807.0
c     
      tol = 5.0e-06
      q = abs(q)
c     
      if (sslope.le.0.0) sslope = 0.000001
c     
c     limit top rill width (wdhtop) to rspace
c     
      if (wdhtop.gt.rspace) wdhtop = rspace
c     
c     compute chezy's coefficient
c     
      chezch = sqrt(8.0*accgav/frctrl)
c     
c     compute rill flow depth (dpthch) - this is an iterative
c     process to solve the uniform flow equation
c     
      if (q.le.0.) then
         dpthch = 0.0
      else
         u = (q/chezch/sqrt(sslope)) ** (2./3.) / wdhtop
         dpthch = 0.2 * q ** .36
c         
   10    dz = dpthch
c   
         dpthch = u * (wdhtop+dz+dz) ** (1./3.)
c         
         if (abs(dz/dpthch-1.).gt.tol) go to 10
c         
      end if
c     
c     compute wetted area (xsarea), wetted perimeter (wp), and
c     hydraulic radius (hydrad)
c     
      xsarea = dpthch * wdhtop
      wp = wdhtop + 2.0 * dpthch
      hydrad = xsarea / wp
c     
c     correction made to compute shear stress using the sine of the
c     slope angle   
c
      sinang = sin(atan(sslope))
c      
c     compute shear stress
c
      shearq = wtdens * sinang * hydrad * frcsol / frctrl
c      
      return
      end
