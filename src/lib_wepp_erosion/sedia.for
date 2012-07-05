      function sedia(spg,eqfall)
      implicit none
!*******************************************************************
!                                                                  *
!  This function calculates the equivalent sand diameter of a      *
!  particle class. It called by SR PRTCMP.                         *
!                                                                  *
!*******************************************************************
!                                                                  *
!  Argumants                                                       *
!     spg                                                          *
!     eqfall                                                       *
!                                                                  *
!*******************************************************************
!
      real, intent(in) :: spg,eqfall
      real sedia


      real rey, rtsid, cddre(9), cdre(9)
      integer i
      real accgav,kinvis
!
!    accgav : acceleration of gravity (m/s^2)
!    kinvis : kinematic viscosity of water (m^2/s)
!*******************************************************************
!                                                                  *
!  Local Variables                                                 *
!    rtsid :                                                       *
!    i     :                                                       *
!    rey   :                                                       *
!    sedia :                                                       *
!                                                                  *
!*******************************************************************
!
     
      cddre(1) = 16.83594
      cddre(2) = 12.23077
      cddre(3) = 7.62560
      cddre(4) = 3.17805
      cddre(5) = -0.89160
      cddre(6) = -4.55638
      cddre(7) = -7.75173
      cddre(8) = -10.12663
      cddre(9) = -12.33391
      
      cdre(1) = -6.90775
      cdre(2) = -4.60517
      cdre(3) = -2.30258
      cdre(4) = 0.0
      cdre(5) = 2.30258
      cdre(6) = 4.60517
      cdre(7) = 6.90775
      cdre(8) = 9.21034
      cdre(9) = 11.51292
      
      kinvis = 1.0E-06
      accgav = 9.807
      
      
      rtsid = 1.3333 * accgav * (spg-1.0) * kinvis / (eqfall**3)
      if (rtsid.le.2.0e+06) then
        rtsid = alog(rtsid)
        do 10 i = 1, 9
          if (cddre(i).lt.rtsid) then
            rey = exp((rtsid-cddre(i-1))/(cddre(i)-cddre(i-1))*(        &
     &          cdre(i)-cdre(i-1))+cdre(i-1))
            sedia = rey * kinvis / eqfall
            return
          end if
   10   continue
!
        sedia = exp(cdre(9)) * kinvis / eqfall
        return
!
!     ** Sediment diameter using Stokes equation for small spheres. **
!
      else
        sedia = sqrt(18.0*eqfall*kinvis/((spg-1.0)*accgav))
      end if
!
      return
      end
