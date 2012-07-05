!$Author$
!$Date$
!$Revision$
!$HeadURL$

!     common declarations for lsoda code

      real  ccmax, el0, h, hmin, hmxi, hu, rc, tn, uround
      integer icf, ierpj, iersl, jcur, jstart, kflag, l,                &
     &   lyh, lewt, lacor, lsavf, lwm, liwm, meth, miter,               &
     &   maxord, maxcor, msbp, mxncf, n, nq, nst, nfe, nje, nqu
      real pdnorm
      integer jtyp, mused, mxordn, mxords

      common /sls001/ ccmax, el0, h, hmin, hmxi, hu, rc, tn, uround,    &
     &   icf, ierpj, iersl, jcur, jstart, kflag, l,                     &
     &   lyh, lewt, lacor, lsavf, lwm, liwm, meth, miter,               &
     &   maxord, maxcor, msbp, mxncf, n, nq, nst, nfe, nje, nqu

      common /slsa01/ pdnorm, jtyp, mused, mxordn, mxords

      save :: /sls001/, /slsa01/
