!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!
      real function rumach ()
!***BEGIN PROLOGUE  RUMACH
!***PURPOSE  Compute the unit roundoff of the machine.
!***CATEGORY  R1
!***TYPE      SINGLE PRECISION (RUMACH-S, DUMACH-D)
!***KEYWORDS  MACHINE CONSTANTS
!***AUTHOR  Hindmarsh, Alan C., (LLNL)
!***DESCRIPTION
! *Usage:
!        real  a, rumach
!        a = rumach()
!
! *Function Return Values:
!     a : the unit roundoff of the machine.
!
! *Description:
!     The unit roundoff is defined as the smallest positive machine
!     number u such that  1.0 + u .ne. 1.0.  This is computed by RUMACH
!     in a machine-independent manner.
!
!***REFERENCES  (NONE)
!***ROUTINES CALLED  (NONE)
!***REVISION HISTORY  (YYMMDD)
!   930216  DATE WRITTEN
!   930818  Added SLATEC-format prologue.  (FNF)
!***END PROLOGUE  RUMACH
!
!*Internal Notes:
!-----------------------------------------------------------------------
! Subroutines/functions called by RUMACH.. None
!-----------------------------------------------------------------------
!**End
!
      real u, comp
!***first executable statement  rumach
      u = 1.0e0
 10   u = u*0.5e0
      comp = 1.0e0 + u
      if (comp .ne. 1.0e0) go to 10
      rumach = u*2.0e0
      return
!----------------------- end of function rumach ------------------------
      end
