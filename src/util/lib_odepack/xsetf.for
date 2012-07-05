!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!
      subroutine xsetf (mflag)
!***BEGIN PROLOGUE  XSETF
!***PURPOSE  Reset the error print control flag.
!***CATEGORY  R3A
!***TYPE      ALL (XSETF-A)
!***KEYWORDS  ERROR CONTROL
!***AUTHOR  Hindmarsh, Alan C., (LLNL)
!***DESCRIPTION
!
!   XSETF sets the error print control flag to MFLAG:
!      MFLAG=1 means print all messages (the default).
!      MFLAG=0 means no printing.
!
!***SEE ALSO  XERRWD, XERRWV
!***REFERENCES  (NONE)
!***ROUTINES CALLED  IXSAV
!***REVISION HISTORY  (YYMMDD)
!   921118  DATE WRITTEN
!   930329  Added SLATEC format prologue. (FNF)
!   930407  Corrected SEE ALSO section. (FNF)
!   930922  Made user-callable, and other cosmetic changes. (FNF)
!***END PROLOGUE  XSETF
!
! Subroutines called by XSETF.. None
! Function routine called by XSETF.. IXSAV
!-----------------------------------------------------------------------
!**End
      integer mflag, junk, ixsav
!
!***first executable statement  xsetf
      if (mflag .eq. 0 .or. mflag .eq. 1) junk = ixsav (2,mflag,.true.)
      return
!----------------------- end of subroutine xsetf -----------------------
      end
