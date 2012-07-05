!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!
!
!     G_ARGC is used to deal with compiler differences
!
      integer   function g_argc()
!

!     + + + PURPOSE + + +
!     The WATCOM compiler returns a value that includes the cmd name/path.
!     The Unix compiler returns a value that includes only the args.
!     This function takes care of the discrepancy by using the WATCOM
!     directives to trigger additional code to get both compilers to
!     return the same value (number of args EXCLUDING the cmd name/path).
!
!     + + + KEYWORDS + + +
!     iargc, utility
!
!     + + + ARGUMENT DECLARATIONS + + +
!
!     + + + ARGUMENT DEFINITIONS + + +
!
!     + + + PARAMETERS + + +
!

!!    Use the following DEFINE to trigger specific code for WATCOM compiler
!
!     + + + LOCAL VARIABLES + + +
!
!     Declaration of internal Unix function
      integer iargc
!
!     + + + END SPECIFICATIONS + + +
!
      g_argc = iargc()
!     Correct for differences between "iargc" among compilers
!*$ifndef WATCOM
!         Note that the Unix compiler will see this line but not WATCOM
      g_argc = g_argc + 1
!*$else
!         Note that both the Unix and the WATCOM compiler will see this line
      g_argc = g_argc - 1
!*$endif

      return
      end
