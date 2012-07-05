!$Author: fredfox $
!$Date: 2005-02-01 22:58:51 $
!$Revision: 1.5 $
!$HeadURL$

      character     rootp*512
! ***      character     crdbp*256
! ***      character     outp*256
! ***      character     soutp*256
! ***      character     manp*256
! ***      character     cligp*256
! ***      character     windp*256
! ***      character     swindp*256
! ***      character     soilp*256
! ***      character     plotp*256

! ***      common /wpath/
! ***     c  rootp, crdbp, outp, soutp, manp, cligp,
! ***     c  windp, swindp, soilp, plotp
      common /wpath/ rootp

      save :: /wpath/
