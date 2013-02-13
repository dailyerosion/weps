!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine profil(a,b,avgslp,nslpts,slplen,xinput,slpinp,xu,xl,   &  
     & y,x,totlen)
     
      use wepp_interface_defs
      
      implicit none
!
!     subroutine profil calculates slope input coefficients
!
!     module adapted from wepp version 2004.7 and called from the
!     main program
!
!     author(s): d.c flanagan and j.c. ascough ii
!     date last modified: 4-1-2005
!
!     + + + parameter declarations + + + 
!
      integer mxslp
      parameter (mxslp = 40)
!
!     + + + argument declarations + + +
!
      real, intent(out) :: a(mxslp), b(mxslp), avgslp, xu(mxslp)
      real, intent(out) :: xl(mxslp), y(*), x(*), totlen
      real, intent(in) :: slplen, xinput(mxslp), slpinp(mxslp)
      integer, intent(in) :: nslpts
!      
!     + + + argument definitions + + +
!
!     a(mxslp) - profile coefficient for curvature
!     b(mxslp) - 
!     avgslp - average slope of the section
!     nslpts - number of slope points
!     slplen - slope length(m)
!     xinput - distance in meters of a slope point
!     slpinp - slope gradient of a point (m/m)
!     xu - dimensionless upper end of section
!     xl - dimensionless lower end of section
!     y - vertical spacing of 100 points
!     x - horizonal spacing of 100 points
!     totlen - length of slope in meters
!
!     + + + local declarations + + + 
!                              
      real slen, sstar(mxslp), xstar(mxslp),                            &
     &     yl(mxslp), yu(mxslp), c(mxslp) 
      integer km,k, l
!
!     + + + local variable definitions + + + 
!
!     begin subroutine profil - 3-28-07 jrf now passed as args
!       
!      read (7,*) nslpts, slplen
!      read (7,*) (xinput(j),slpinp(j),j = 1,nslpts)
!     
      slen = xinput(nslpts)
      y(nslpts) = 0.0
!     
      do k = 1, nslpts - 1
         km = nslpts - k
         y(km) = y(km+1) + (xinput(km+1) - xinput(km)) *                &
     &           (slpinp(km) + slpinp(km+1)) / 2.0
      end do
!     
      avgslp = y(1) / slen
!     
      if (avgslp.le.0.0) avgslp = 0.000001
!    
      do k = 1, nslpts
         sstar(k) = slpinp(k) / avgslp
         xstar(k) = xinput(k) / slen
      end do
!    
      do k = 2, nslpts
         a(k) = (sstar(k)-sstar(k-1)) / (xstar(k)-xstar(k-1))
         b(k) = sstar(k-1) - a(k) * xstar(k-1)
      end do
!    
      yl(1) = 1.0
      xl(1) = 0.0
!    
      do k = 2, nslpts
         yu(k) = yl(k-1)
         xu(k) = xl(k-1)
         c(k) = yu(k) + a(k) * xstar(k-1) ** 2 / 2.0 + b(k) * xstar(k-1)
         yl(k) = -a(k) * xstar(k) ** 2 / 2.0 - b(k) * xstar(k) + c(k)
         xl(k) = xstar(k)
      end do
!    
      k = 2
      y(1) = 1.0
      x(1) = 0.0
!    
      do l = 2, 101
         x(l) = float(l-1) * 0.01
   10    if (x(l).gt.xstar(k)) then
            k = k + 1
            go to 10
         end if
         y(l) = -a(k) * x(l) ** 2 / 2.0 - b(k) * x(l) + c(k)
      end do
!    
      totlen = slplen
!
      return
      end
