!$Author$
!$Date$
!$Revision$
!$HeadURL$

!     This common block contains the character variables containing
!     information about the crop

!     Parameter include files:  p1werm.inc

!     + + + VARIABLE DECLARATIONS + + +

      integer         cprevrotation(mnsub)
      integer         cprevcalibrotation(mnsub)

      common / c1report / cprevrotation, cprevcalibrotation

      save :: /c1report/

!     + + + VARIABLE DEFINITIONS + + +

!     cprevrotation - rotation count number previously printed
!                     in crop harvest report

!     cprevcalibrotation - rotation count number previously printed
!                          in calibration crop harvest report
