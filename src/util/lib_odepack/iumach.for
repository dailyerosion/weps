!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!
      integer function iumach()
!***BEGIN PROLOGUE  IUMACH
!***PURPOSE  Provide standard output unit number.
!***CATEGORY  R1
!***TYPE      INTEGER (IUMACH-I)
!***KEYWORDS  MACHINE CONSTANTS
!***AUTHOR  Hindmarsh, Alan C., (LLNL)
!***DESCRIPTION
! *Usage:
!        integer  lout, iumach
!        lout = iumach()
!
! *Function Return Values:
!     lout : the standard logical unit for fortran output.
!
!***REFERENCES  (NONE)
!***ROUTINES CALLED  (NONE)
!***REVISION HISTORY  (YYMMDD)
!   930915  DATE WRITTEN
!   930922  Made user-callable, and other cosmetic changes. (FNF)
!***END PROLOGUE  IUMACH
!
!*Internal Notes:
!  The built-in value of 6 is standard on a wide range of Fortran
!  systems.  This may be machine-dependent.
!**End
!***first executable statement  iumach
      iumach = 6
!
      return
!----------------------- end of function iumach ------------------------
      end
