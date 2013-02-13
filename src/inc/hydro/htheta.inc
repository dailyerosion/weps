!$Author$
!$Date$
!$Revision$
!$HeadURL$
!***********************************************************************
!
! must include 'p1werm.inc'
!
!***********************************************************************
!
! global hydro variables (local to hydro submodel)
!
!***********************************************************************

!                    surface soil water content (m^3/m^3) (theta(0))
!                    soil layer water content (m^3/m^3)
      real theta(0:mnsz)
!                    daily maximum soil layer water content (m^3/m^3)
      real thetadmx(mnsz)
!-----------------------------------------------------------------------
!                    soil water content at saturation (m^3/m^3)
      real thetas(mnsz)
!                    effective saturated water content (m^3/m^3)
      real thetes(mnsz)
!                    soil water content at field capacity (m^3/m^3)
      real thetaf(mnsz)
!                    residual soil water cotent (m^3/m^3)
      real thetar(mnsz)
!                    soil water content at wilting point (m^3/m^3)
      real thetaw(mnsz)
!-----------------------------------------------------------------------
!                    soil layer water content (mm)
!      real wc(mnsz)
!                    soil profile water content (mm)
      real swc
!                    initial soil profile water content (mm)
      real swci
!-----------------------------------------------------------------------
!                    soil water matric head (mm)
!      real swm(mnsz)
!                    soil water matric potential (j/kg)
!      real potm(mnsz)
!                    exponent of campbell's unsaturated hydraulic
!                    conductivity model
!      real cm(mnsz)
!-----------------------------------------------------------------------

      common /stheta/ theta, thetadmx, thetas, thetaf, thetaw,          &
!     &       thetes, thetar, wc, swc, swci, lswc, swm, potm, cm
     &       thetes, thetar, swc, swci

      save :: /stheta/
