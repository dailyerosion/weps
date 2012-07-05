!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!
      integer function ixsav (ipar, ivalue, iset)
!***BEGIN PROLOGUE  IXSAV
!***SUBSIDIARY
!***PURPOSE  Save and recall error message control parameters.
!***CATEGORY  R3C
!***TYPE      ALL (IXSAV-A)
!***AUTHOR  Hindmarsh, Alan C., (LLNL)
!***DESCRIPTION
!
!  IXSAV saves and recalls one of two error message parameters:
!    LUNIT, the logical unit number to which messages are printed, and
!    MESFLG, the message print flag.
!  This is a modification of the SLATEC library routine J4SAVE.
!
!  Saved local variables..
!   lunit  = logical unit number for messages.  the default is obtained
!            by a call to iumach (may be machine-dependent).
!   mesflg = print control flag..
!            1 means print all messages (the default).
!            0 means no printing.
!
!  On input..
!    ipar   = parameter indicator (1 for lunit, 2 for mesflg).
!    ivalue = the value to be set for the parameter, if iset = .true.
!    iset   = logical flag to indicate whether to read or write.
!             if iset = .true., the parameter will be given
!             the value ivalue.  if iset = .false., the parameter
!             will be unchanged, and ivalue is a dummy argument.
!
!  On return..
!    IXSAV = The (old) value of the parameter.
!
!***SEE ALSO  XERRWD, XERRWV
!***ROUTINES CALLED  IUMACH
!***REVISION HISTORY  (YYMMDD)
!   921118  DATE WRITTEN
!   930329  Modified prologue to SLATEC format. (FNF)
!   930915  Added IUMACH call to get default output unit.  (ACH)
!   930922  Minor cosmetic changes. (FNF)
!   010425  Type declaration for IUMACH added. (ACH)
!***END PROLOGUE  IXSAV
!
! Subroutines called by IXSAV.. None
! Function routine called by IXSAV.. IUMACH
!-----------------------------------------------------------------------
!**End
      logical iset
      integer ipar, ivalue
!-----------------------------------------------------------------------
      integer iumach, lunit, mesflg
!-----------------------------------------------------------------------
! the following fortran-77 declaration is to cause the values of the
! listed (local) variables to be saved between calls to this routine.
!-----------------------------------------------------------------------
      save lunit, mesflg
      data lunit/-1/, mesflg/1/
!
!***first executable statement  ixsav
      if (ipar .eq. 1) then
        if (lunit .eq. -1) lunit = iumach()
        ixsav = lunit
        if (iset) lunit = ivalue
        endif
!
      if (ipar .eq. 2) then
        ixsav = mesflg
        if (iset) mesflg = ivalue
        endif
!
      return
!----------------------- end of function ixsav -------------------------
      end
