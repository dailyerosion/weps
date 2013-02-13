!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine sheart(q,sslope,rspace,wdhtop,frcsol,frctrl,shearq)
      
      use wepp_interface_defs
      
      implicit none
!
!     compute rill width adjustments, chezy's coefficient for rill
!     flow, flow depth, wetted area, wetted perimeter, hydraulic
!     radius, and shear stress at the end of the slope
!
!     module adapted from wepp version 2008.907 and called from 
!     subroutine param
!
!     author(s): d.c. flanagan and j.c. ascough ii
!     date last modified: 11-6-2008
!
      real, intent(inout) :: q,sslope,wdhtop
      real, intent(in) :: rspace,frcsol,frctrl
      real, intent(out) :: shearq
!
!     q      - flow discharge (m**3/s)
!     sslope - channel slope (m/m)
!
      real dz, tol, u, chezch, sinang, wp, xsarea, hydrad
      real accgav,wtdens,dpthch
!
!      u      - portion of uniform flow equation
!      wdthck - test width to check against to see if wider than
!               current rill width
!      chezch - chezy c - roughness factor
!      dz     - trial valve for channel depth (m)
!      xsarea - cross sectional area of the flow (m**2)
!      wp     - wetted perimeter of the flow (m)
!      hydrad - hydraulic radius (m)
!      sinang - sine of slope angle
!      tol    - tolerance value
!
!      save
!
      accgav = 9.807
      wtdens = 9807.0
!     
      tol = 5.0e-06
      q = abs(q)
!     
      if (sslope.le.0.0) sslope = 0.000001
!     
!     limit top rill width (wdhtop) to rspace
!     
      if (wdhtop.gt.rspace) wdhtop = rspace
!     
!     compute chezy's coefficient
!     
      chezch = sqrt(8.0*accgav/frctrl)
!     
!     compute rill flow depth (dpthch) - this is an iterative
!     process to solve the uniform flow equation
!     
      if (q.le.0.) then
         dpthch = 0.0
      else
         u = (q/chezch/sqrt(sslope)) ** (2./3.) / wdhtop
         dpthch = 0.2 * q ** .36
!         
   10    dz = dpthch
!   
         dpthch = u * (wdhtop+dz+dz) ** (1./3.)
!         
         if (abs(dz/dpthch-1.).gt.tol) go to 10
!         
      end if
!     
!     compute wetted area (xsarea), wetted perimeter (wp), and
!     hydraulic radius (hydrad)
!     
      xsarea = dpthch * wdhtop
      wp = wdhtop + 2.0 * dpthch
      hydrad = xsarea / wp
!     
!     correction made to compute shear stress using the sine of the
!     slope angle   
!
      sinang = sin(atan(sslope))
!      
!     compute shear stress
!
      shearq = wtdens * sinang * hydrad * frcsol / frctrl
!      
      return
      end
