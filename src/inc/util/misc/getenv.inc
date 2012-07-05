!$Author$
!$Date$
!$Revision$
!$HeadURL$

! Watcom compiler specific version of "getenv"
! It is "INCLUDED" directly into the "g_env.for" src file
! if compiling with the WATCOM compiler.

      envlen =  fgetenv (ename, eval)
!      if (envlen .eq. 0) then
!        eval = ' '
!      endif
