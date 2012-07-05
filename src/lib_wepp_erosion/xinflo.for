      subroutine xinflo(xinput,efflen,slplen,a,b,qin,qout,peakro,       &
     &    qostar,ainf,binf,cinf,ainftc,binftc,cinftc,qshear,rspace,     &
     &    nslpts)
     
      use wepp_interface_defs
      
      implicit none
!
!     subroutine xinflo controls the variables affected by the
!     runoff both leaving and entering the flow plane
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
      real, intent(inout) :: qout
      real, intent(out) :: xinput(*), qin, qostar, qshear
      real, intent(in) :: peakro, efflen, slplen, a(mxslp)
      real, intent(in) :: b(mxslp), rspace
      real, intent(out) :: ainf(mxslp), binf(mxslp), cinf(mxslp)
      real, intent(out) :: ainftc(mxslp), binftc(mxslp), cinftc(mxslp)
      integer, intent(in) :: nslpts
    
!
!     + + + argument definitions + + +
!
!     xinput(101) - unitless distances (points) down the slope 
!     efflen - effective flow length
!     slplen - slope length (m)
!     a(mxslp) - profile coefficient for curvature
!     b(mxslp) -
!     qin - flow discharge per unit width (m^3/m*s) at the top
!     qout - flow discharge per unit width (m^3/m*s) at the bottom
!     peakro - peak runoff rate(m/s)
!     qostar - non-dimensional discharge out
!     ainf(mxslp) -  nondimensional shear stress coefficient
!     binf(mxslp) -  nondimensional shear stress coefficient
!     cinf(mxslp) - nondimensional shear stress coefficient
!     ainftc(mxslp) - nondimensional transport coefficient
!     binftc(mxslp) - nondimensional transport coefficient
!     cinftc(mxslp) - nondimensional transport coefficient
!     qshear - peak flow discharge (m^3/s)
!     rspace - rill spacing (m).
!     nslpts - number of slope points
!
!     + + + local declarations + + +
!
      integer i
      real del
!
!     begin subroutine xinflo
!
      do i = 2, 101
         xinput(i) = float(i-1) * .01
      end do
!
      qin = qout
      qout = peakro * efflen
      del = qout - qin
!     
      if (qout.le.0.0) then
         qostar = -efflen / slplen
      else if (abs(del).gt.1.0e-10) then
         if (qin.le.0.0) then
            qostar = 0.0
         else
            qostar = qin / del
         end if
      else
         if (del.ge.0.0) qostar = qin / 1.0e-10
         if (del.lt.0.0) qostar = -qin / 1.0e-10
      end if
!     
      if (qout.gt.0.0) then
         if (qostar.eq.-1.0) qostar = -1.001
!        
         do i = 2, nslpts
            ainf(i) = a(i) / (qostar+1.0)
            binf(i) = (a(i)*qostar+b(i)) / (qostar+1.0)
            cinf(i) = (b(i)*qostar/(qostar+1.0))
            ainftc(i) = ainf(i)
            binftc(i) = binf(i)
            cinftc(i) = cinf(i)
         end do
!        
         qshear = qout * rspace
!     
      else
!        
         if (abs(qostar).lt.0.00001) qostar = -0.00001
!        
         do i = 2, nslpts
            ainf(i) = a(i) / (qostar)
            binf(i) = (a(i)*qostar+b(i)) / (qostar)
            cinf(i) = b(i)
            ainftc(i) = ainf(i)
            binftc(i) = binf(i)
            cinftc(i) = cinf(i)
         end do
!        
         qshear = qin * rspace
!     
      end if
!     
!     added 6-11-2007 - jrf 
      qshear = abs(qshear)

      return
      end
