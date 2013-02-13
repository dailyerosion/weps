!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine shears(q,sslope,rspace,width,frcsol,frctrl,shearq,     &
     &  rwflag)
      implicit none
!
!     + + + purpose + + +
!     compute rill width adjustments, chezy's coefficient for rill
!     flow, flow depth, wetted area, wetted perimeter, hydraulic
!     radius, and shear stress at the end of the slope
!
!     called from param
!     author(s):
!     reference in user guide:
!
!     + + + keywords + + +
!
!
!     + + + argument declarations + + +
      real, intent(inout) :: width
      real, intent(in) :: q, rspace, frcsol, frctrl
      real, intent(inout) :: sslope
      real, intent(out) :: shearq
      integer, intent(in) :: rwflag
!
!     + + + argument definitions + + +
!
!     q      : flow discharge (m**3/s)
!     sslope : channel slope (m/m)
!     rspace : rill spacing (m)
!     width  : rill width (m)
!     frcsol : soil grain friction factor
!     frctrl : total rill friction factor
!     rwflag : use temporary or permanent rill widths
!
!     + + + local variables + + +
      real dz, tol, u, chezch, sinang, wp, xsarea, hydrad
      real accgav, wtdens, dpthch, q2, wdthck
!
!     + + + local definitions + + +
!
!      u      : portion of uniform flow equation
!      wdthck : test width to check against to see if wider than
!               current rill width
!      chezch : chezy c - roughness factor
!      dz     : trial valve for channel depth (m)
!      xsarea : cross sectional area of the flow (m**2)
!      wp     : wetted perimeter of the flow (m)
!      hydrad : hydraulic radius (m)
!      sinang : sine of slope angle
!      tol    : tolerance value
!
!      save   - removed this 3-28-07 jrf
!
!********************************************************************
!
      accgav = 9.807
      wtdens = 9807.0
!     
!     tolerance value changed by baffaut, 1996.  dcf 3/97
!     tol = 5.0e-05
      tol = 5.0e-06
!     6-11-2007 - now done in xinflo.for
      q2 = abs(q)  
!     
!     sslope value changed by baffaut, 1996.  dcf 3/97
!     if (sslope.le.0.0) sslope = 0.00001
      if (sslope.le.0.0) sslope = 0.000001
!     
!     compute rill width (width). note that when tillage occurs
!     rill width is set to zero in sr contin (or soil???)
!     
!     using gilley's relationship
!     
      if (rwflag.eq.1) then
        wdthck = 1.13 * q2 ** 0.303
        if (width.lt.wdthck) width = wdthck
      end if
!     
      if (width.gt.rspace) width = rspace
!     
!     compute chezy's coefficient:
!     
      chezch = sqrt(8.0*accgav/frctrl)
!     
!     compute rill flow depth (dpthch(iplane)). this is an iterative
!     process to solve the uniform flow equation:
!     
!     dpthch(iplane)=((q/chezch/sqrt(sslope)**
!     (2/3)/width)*(width+2*dpthch(iplane))**(1/3)
!     
      if (q.le.0.) then
         dpthch = 0.0
      else
         u = (q/chezch/sqrt(sslope)) ** (2./3.) / width
         dpthch = 0.2 * q ** .36
   10    dz = dpthch
         dpthch = u * (width+dz+dz) ** (1./3.)
         if (abs(dz/dpthch-1.).gt.tol) go to 10
      end if
!     
!     compute wetted area (xsarea), wetted perimeter (wp), and
!     hydraulic radius (hydrad):
!     
      xsarea = dpthch * width
      wp = width + 2.0 * dpthch
      hydrad = xsarea / wp
!     
!     compute shear stress:
!     
!     correction made to compute shear stress using the sine of the
!     slope angle, not the tangent of the slope angle as has previously
!     been computed.  this should only impact results greatly on steeper
!     hillslopes.   dcf   1/21/93
!     shears=wtdens*sslope*hydrad*frcsol(iplane)/frctrl(iplane)
!     
      sinang = sin(atan(sslope))
      shearq = wtdens * sinang * hydrad * frcsol / frctrl
      return
      end