!$Author$
!$Date$
!$Revision$
!$HeadURL$
!***********************************************************************

! must include 'pwerm.inc'

!***********************************************************************

! global hydro variables (local to hydro submodel)

!***********************************************************************
!                    particle size distribution geometric mean dia. (mm)
      real gmd(mnsz)
!                    particle size distribution geometric std. dev. (mm)
      real gsd(mnsz)
!-----------------------------------------------------------------------
      common /psdist/ gmd, gsd

      save :: /psdist/
