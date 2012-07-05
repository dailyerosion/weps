!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!
      subroutine ssrcma (rsav, isav, job)
!-----------------------------------------------------------------------
! This routine saves or restores (depending on JOB) the contents of
! the Common blocks SLS001, SLSA01, which are used
! internally by one or more ODEPACK solvers.
!
! RSAV = real array of length 10 or more.
! ISAV = integer array of length 29 or more.
! JOB  = flag indicating to save or restore the Common blocks:
!        JOB  = 1 if Common is to be saved (written to RSAV/ISAV)
!        JOB  = 2 if Common is to be restored (read from RSAV/ISAV)
!        A call with JOB = 2 presumes a prior call with JOB = 1.
!-----------------------------------------------------------------------
      integer isav, job
      integer ils, ilsa
      integer i, lenrls, lenils, lenila
      real rsav
      real rls, rlsa
      dimension rsav(*), isav(*)
      common /sls001/ rls(9), ils(25)
      common /slsa01/ rlsa, ilsa(4)
      data lenrls/9/, lenils/25/, lenila/4/
!
      if (job .eq. 2) go to 100
      do 10 i = 1,lenrls
 10     rsav(i) = rls(i)
      rsav(lenrls+1) = rlsa
!
      do 20 i = 1,lenils
 20     isav(i) = ils(i)
      do 25 i = 1,lenila
 25     isav(lenils+i) = ilsa(i)
!
      return
!
 100  continue
      do 110 i = 1,lenrls
 110     rls(i) = rsav(i)
      rlsa = rsav(lenrls+1)
!
      do 120 i = 1,lenils
 120     ils(i) = isav(i)
      do 125 i = 1,lenila
 125     ilsa(i) = isav(lenils+i)
!
      return
!----------------------- end of subroutine ssrcma ----------------------
      end
