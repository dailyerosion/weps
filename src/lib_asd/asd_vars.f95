MODULE asd_vars

  IMPLICIT NONE
  include "dummy.inc"

  INTEGER :: alloc_asd_vars            ! Allocate status return
  INTEGER :: asd_flg = 0               ! Initialize asd debug flag level (higher "bit" numbers means more detailed info)

  INTEGER, PARAMETER :: msieves = 25   ! Maximum number of sieves that can be used
  INTEGER :: nsieves                   ! Number of sieves actually used

  REAL, DIMENSION(:), ALLOCATABLE :: sdia    ! Array holding the sieve size diameters used to compute the sieve cuts (mm)
  REAL, DIMENSION(:), ALLOCATABLE :: trsdia  ! Array holding the transformed sieve size diameters used to compute the sieve cuts (mm)
  REAL, DIMENSION(:), ALLOCATABLE :: gmdia   ! Array holding the geometric mean diameters of the sieve cuts (mm)
  REAL, DIMENSION(:), ALLOCATABLE :: trgmdia ! Array holding the transformed geometric mean diameters of the sieve cuts (mm)
  REAL, DIMENSION(:), ALLOCATABLE :: ltrgmdia! Array holding the log of the transformed geometric mean diameters of the sieve cuts (mm)
  REAL, DIMENSION(:), ALLOCATABLE :: mf      ! Array holding the actual number of sieve "cuts" (nsieve+1)
  REAL :: mnsize = 0.005                     ! Minimum aggregate size to use for computing the lower sieve cut geometric mean diameter (mm)
  REAL :: mxsize = 1000.0                    ! Maximum aggregate size to use for computing the upper sieve cut geometric mean diameter (mm)

  REAL :: m_not                        ! Minimum sized diameter aggregate (mm)
  REAL :: m_inf                        ! Maximum sized diamater aggregate (mm)



contains
  FUNCTION asd_init_vars(ns, minsize, maxsize, mnot, minf) result (ret_status)

    INTEGER, INTENT (IN) :: ns          ! Number of sieves to use 0<ns<=msieves
    REAL, INTENT (IN) :: minsize        ! Minimum sieve size used for computing the lower sieve cut gmd (mm)
    REAL, INTENT (IN) :: maxsize        ! Maximum sieve size used for computing the upper sieve cut gmd (mm)
    REAL, INTENT (IN) :: mnot           ! Minimum sized diameter aggregate (mm)
    REAL, INTENT (IN) :: minf           ! Maximum sized diamater aggregate (mm)

    INTEGER :: alloc_status = 0         ! Local allocate status return
    INTEGER :: ret_status               ! Local status return

    INTEGER :: i                        ! local loop variable

    INTRINSIC ASSOCIATED                ! use to verify status of pointers

    ret_status = 0                      ! initialize to zero (won't let me in definition)

    IF (ns > 0 .and. ns <= msieves) THEN
      nsieves = ns                      ! Set the actual number of sieves to use
    ELSE
      write(0,*) "Error: The number of sieves specified", ns, "is less than 1 or greater than ", msieves
      ret_status = ret_status + 1
      call EXIT (ret_status)
    END IF
    IF (btest(asd_flg, 0)) THEN
      write(0,*) "nsieves: ", nsieves
    END IF

    IF (ALLOCATED (sdia) .neqv. .TRUE.) THEN
      ALLOCATE (sdia(nsieves), STAT = alloc_status)
      IF (alloc_status /= 0) THEN
        write(0,*) "Error allocating sdia(nsieves) - alloc_status: ", alloc_status
        ret_status = ret_status + alloc_status
        call EXIT (ret_status)
      END IF
    END IF

    IF (ALLOCATED (trsdia) .neqv. .TRUE.) THEN
      ALLOCATE (trsdia(nsieves), STAT = alloc_status)
      IF (alloc_status /= 0) THEN
        write(0,*) "Error allocating trsdia(nsieves) - alloc_status: ", alloc_status
        ret_status = ret_status + alloc_status
        call EXIT (ret_status)
      END IF
    END IF

    IF (ALLOCATED (gmdia) .neqv. .TRUE.) THEN
      ALLOCATE (gmdia(nsieves+1), STAT = alloc_status)
      IF (alloc_status /= 0) THEN
        write(0,*) "Error allocating gmdia(nsieves+1) - alloc_status: ", alloc_status
        ret_status = ret_status + alloc_status
        call EXIT (ret_status)
      END IF
    END IF

    IF (ALLOCATED (trgmdia) .neqv. .TRUE.) THEN
      ALLOCATE (trgmdia(nsieves+1), STAT = alloc_status)
      IF (alloc_status /= 0) THEN
        write(0,*) "Error allocating trgmdia(nsieves+1) - alloc_status: ", alloc_status
        ret_status = ret_status + alloc_status
        call EXIT (ret_status)
      END IF
    END IF

    IF (ALLOCATED (ltrgmdia) .neqv. .TRUE.) THEN
      ALLOCATE (ltrgmdia(nsieves+1), STAT = alloc_status)
      IF (alloc_status /= 0) THEN
        write(0,*) "Error allocating ltrgmdia(nsieves+1) - alloc_status: ", alloc_status
        ret_status = ret_status + alloc_status
        call EXIT (ret_status)
      END IF
    END IF

    IF (ALLOCATED (mf) .neqv. .TRUE.) THEN
      ALLOCATE (mf(nsieves+1), STAT = alloc_status)
      IF (alloc_status /= 0) THEN
        write(0,*) "Error allocating mf(nsieves+1) - alloc_status: ", alloc_status
        ret_status = ret_status + alloc_status
        call EXIT (ret_status)
      END IF
    END IF

    ! Compute a logarithmic progression of "nsieves" sieve size diameters (mm)
    DO i = 1, nsieves
          sdia(i) = exp(log(minsize) +  i*(log(maxsize)-log(minsize))/(nsieves+1))
    END DO
    IF (btest(asd_flg, 0)) THEN
      write(UNIT=0,FMT="(A)",ADVANCE="YES") "asdvars: sieve sizes - dia. in (mm): sdia(i) values"
      write(UNIT=0,FMT="(30(i8))",ADVANCE="YES") (i, i=1,nsieves)
      write(UNIT=0,FMT="(30(f8.3))",ADVANCE="YES") (sdia(i), i=1, nsieves)
      write(0,*)
    END IF

    ! Set m_not and m_inf values
    IF (mnot >= minsize .and. mnot <= sdia(1)) THEN
      m_not = mnot
    ELSE
      write(0,*) "Error: mnot ", mnot, " must be greater than minsize ", &
                 minsize, " and less than sdia(1) ", sdia(1)
      ret_status = ret_status +1
      call EXIT (ret_status)
    END IF
    IF (minf <= maxsize .and. minf >= sdia(nsieves)) THEN
      m_inf = minf
    ELSE
      write(0,*) "Error: minf ", minf, " must be greater than sdia(nsieve) ", &
                 sdia(nsieves), " and less than or equal to maxsize ", maxsize
      ret_status = ret_status +1
      call EXIT (ret_status)
    END IF
    IF (btest(asd_flg, 0)) THEN
      write(UNIT=0,FMT="(A,A,f8.3,A,f8.3)",ADVANCE="YES") 'asdvars: ', 'm_not: ', m_not, ' m_inf: ', m_inf
      write(0,*)
    END IF

    ! Compute the geometric mean diameter for each sieve cut
    gmdia(1) = sqrt(minsize*sdia(1))
    DO i = 2, nsieves
      gmdia(i) = sqrt(sdia(i)*sdia(i-1))
    END DO
    gmdia(nsieves+1) = sqrt(maxsize*sdia(nsieves))
    IF (btest(asd_flg, 0)) THEN
      write(UNIT=0,FMT="(A)",ADVANCE="YES") "asdvars: compute geometric mean value (gmdia) for each sieve cut"
      write(UNIT=0,FMT="(30(i8))",ADVANCE="YES") (i, i=1,nsieves+1)
      write(UNIT=0,FMT="(30(f8.3))",ADVANCE="YES") (gmdia(i), i=1, nsieves+1)
      write(0,*)
    END IF

    !write(0,*) "asd_init_vars() ret_status: ", ret_status
   END FUNCTION asd_init_vars
!-----------------------------------------------------------------------------------------------------------------------------------------
  ! Compute the mass fractions in each sieve cut when given gmdx,gsdx,minf,mnot values
  SUBROUTINE asd2mf (gmdx, gsdx, mnot, minf, mfr)

    REAL, INTENT (IN) :: gmdx, gsdx
    REAL, INTENT (IN) :: mnot, minf
    REAL, INTENT (OUT):: mfr(*)

    INTEGER :: i                        ! local loop variable
    REAL :: this, prev                  ! local temporary probability variables

    REAL :: lngmdx = 0.0
    REAL :: lngsdx = 0.0

    ! Compute transformed sieve dia. sizes
    DO i = 1, nsieves
      IF (sdia(i) .lt. minf) THEN
        trsdia(i) = (sdia(i)-mnot) * (minf-mnot) / (minf-sdia(i))
       END IF
    END DO
    IF (btest(asd_flg, 0)) THEN
       write(UNIT=0,FMT="(A)",ADVANCE="YES") "asd2mf: transformed sieve sizes in (mm): trsdia(i) values"
       write(UNIT=0,FMT="(30(i8))",ADVANCE="YES") (i, i=1,nsieves)
       write(UNIT=0,FMT="(30(f8.3))",ADVANCE="YES") (trsdia(i), i=1, nsieves)
       write(0,*)
    END IF

    lngmdx= log(gmdx)
    lngsdx= sqrt(2.0) * log(gsdx)
    IF (btest(asd_flg, 0)) THEN
     write(UNIT=0,FMT="(A,A,f12.3,A,f12.3)",ADVANCE="YES") 'mf2asd: ', 'lngmdx: ', lngmdx, ' lngsdx: ', lngsdx
     write(0,*)
    END IF

    prev= 1.0

    ! Compute each dia. cumulative probability
    DO i = 1, nsieves
      IF (sdia(i) .le. mnot) THEN
        this = 1.0
      ELSE IF (sdia(i) .lt. minf) THEN
        this = 0.5 -0.5*erf((log(trsdia(i)) - lngmdx) / lngsdx)
      ELSE
        this = 0.0
      END IF

      ! Compute mass fraction between prev and this dia
      mfr(i) = prev - this
      prev = this
!      IF (btest(asd_flg, 0)) THEN
!        write(0,*) 'asd2mf:',i,sdia(i),this,mfr(i)
!      END IF

      ! If roundoff errors or otherwise results in negative mass fraction THEN set to zero mass
      IF (mfr(i) .lt. 0.0) THEN
        mfr(i) = 0.0
      ELSE
        prev = this
      END IF
!      IF (btest(asd_flg, 0)) THEN
!        write(0,*) 'asd2mf: mfr(',i,')',mfr(i)
!      END IF
    END DO
    ! Get mass fraction for upper-most sieve cut
    mfr(nsieves+1) = prev

    ! Zero out the rest of the array which is used every where else
    DO i=nsieves+2, msieves+1
      mfr(i) = 0.0
    END DO

    IF (btest(asd_flg, 0)) THEN
     write(UNIT=0,FMT="(A,4(A,f12.3))") 'asd2mf: mfr(nsieves+1)', ' gmdx: ', gmdx, ' gsdx: ', gsdx, ' mnot: ', mnot, ' minf: ', minf
     write(UNIT=0,FMT="(30(i8))",ADVANCE="YES") (i, i=1,nsieves+1) 
     write(UNIT=0,FMT="(30(f8.3))",ADVANCE="YES") (mfr(i), i=1, nsieves+1)
     write(0,*)
    END IF

  END SUBROUTINE asd2mf
!-----------------------------------------------------------------------------------------------------------------------------------------
  ! Compute the gmdx, gsdx when given the number of sieve cuts, the mass fractions in each sieve cut, and minf andmnot values
  SUBROUTINE mf2asd (gmdx, gsdx, mnot, minf, mfr)

    REAL, INTENT (OUT):: gmdx, gsdx
    REAL, INTENT (IN) :: mnot, minf
    REAL, INTENT (IN) ::mfr(*)

!   initialize accumulators
    REAL :: alpha = 0.0
    REAL :: beta = 0.0
    REAL :: tmd(nsieves+1)
    INTEGER :: istart, istop, i
    INTEGER :: sdia_start, sdia_istop, sdia_temp

    ! Initialize local variables
    istart = 1
    istop = nsieves + 1

    ! do transformations for "modified" log-normal cases
!    DO i= istart, istop
    DO i = 1, nsieves+1
       trgmdia(i) = (gmdia(i)-mnot)*(minf-mnot)/(minf-gmdia(i))
    END DO
    IF (btest(asd_flg, 0)) THEN
      write(UNIT=0,FMT="(A)",ADVANCE="YES") "mf2asd: compute transformed geometric mean value (trgmdia) for each sieve cut"
      write(UNIT=0,FMT="(30(i8))",ADVANCE="YES") (i, i=1,nsieves+1)
      write(UNIT=0,FMT="(30(f8.3))",ADVANCE="YES") (trgmdia(i), i=1, nsieves+1)
      write(0,*)
    END IF

    DO i = 1, nsieves +1
       ! now compute the log of the transformed gmd dia
       ltrgmdia(i) = log(trgmdia(i))
    END DO
    IF (btest(asd_flg, 0)) THEN
      write(UNIT=0,FMT="(A)",ADVANCE="YES") "mf2asd: compute log of transformed geometric mean value (ltrgmdia) for each sieve cut"
      write(UNIT=0,FMT="(30(i8))",ADVANCE="YES") (i, i=1,nsieves+1)
      write(UNIT=0,FMT="(30(f8.3))",ADVANCE="YES") (ltrgmdia(i), i=1, nsieves+1)
      write(0,*)
    END IF

    DO i = 1, nsieves+1
       ! sum diameters  & their squares, over all aggregate sizes
       alpha = alpha + (mfr(i)*ltrgmdia(i))
       beta = beta + (mfr(i)*ltrgmdia(i)*ltrgmdia(i))
    END DO
    IF (btest(asd_flg, 0)) THEN
       write(UNIT=0,FMT="(A,4(A,f8.5))",ADVANCE="YES") 'mf2asd: ', 'alpha: ', alpha, ' beta_tmp: ', beta, &
                       ' beta_1/2: ', beta-alpha*alpha, ' beta: ', sqrt(beta-alpha*alpha)
       write(0,*)
    END IF

    ! compute geometric mean and standard deviation
    gmdx = exp(alpha)
!    IF ( beta - alpha*alpha .le. 0.0 ) THEN
!       gsdx = mingsd
!    ELSE 
!       gsdx = max(mingsd,exp(sqrt(beta-alpha*alpha)))
!    END IF

    gsdx = exp(sqrt(beta-alpha*alpha))

    IF (btest(asd_flg, 0)) THEN
       write(UNIT=0,FMT="(A,A,f8.5,A,f8.5)",ADVANCE="YES") 'mf2asd: ', 'gmdx: ', gmdx, ' gsdx: ', gsdx
       write(0,*)
    END IF




    write(0,*) "Trying with (tmd) = (gmdia)"
    DO i = 1, nsieves+1
      tmd(i) = gmdia(i)
    END DO
    IF (btest(asd_flg, 0)) THEN
      write(UNIT=0,FMT="(A)",ADVANCE="YES") "mf2asd: compute xg (tmd) for each sieve cut"
      write(UNIT=0,FMT="(30(i8))",ADVANCE="YES") (i, i=1,nsieves+1)
      write(UNIT=0,FMT="(30(f8.3))",ADVANCE="YES") (tmd(i), i=1, nsieves+1)
      write(0,*)
    END IF

    alpha = 0.0
    beta = 0.0
    DO i = 1, nsieves+1
       ! sum diameters  & their squares, over all aggregate sizes
!       alpha = alpha + (mfr(i)*log(tmd(i)))
!       beta = beta + (mfr(i)*log(tmd(i))*log(tmd(i)))
       alpha = alpha + (mfr(i)*(tmd(i)))
       beta = beta + (mfr(i)*(tmd(i))*(tmd(i)))
    END DO
    gmdx = exp(alpha)
    gsdx = exp(sqrt(beta-alpha*alpha))
    IF (btest(asd_flg, 0)) THEN
       write(0,*) 'mf2asd: ', 'gmdx: ', gmdx, ' gsdx: ', gsdx, log(gmdx), log(gsdx)
       write(0,*)
    END IF




!    tmd(1) = sqrt(log(mnot) * log(sdia(1)))
!    DO i = 2, nsieves
!      tmd(i) = sqrt(log(sdia(i-1)) * log(sdia(i)))
!      write(0,*) i,sdia(i-1),sdia(i),log(sdia(i-1)),log(sdia(i)),sqrt(log(sdia(i-1))*log(sdia(i)))
!    END DO
!    tmd(nsieves+1) = sqrt(log(sdia(nsieves)) * log(minf))
!      write(0,*) i,sdia(nsieves),minf,log(sdia(nsieves)),log(minf),sqrt(log(sdia(nsieves))*log(minf))

!    trmnot = (mnot-mnot)*(minf-mnot)/(minf-mnot) ! ==> 1
!    trminf = (minf-mnot)*(minf-mnot)/(minf-minf) ! ==> infinity
    tmd(1) = sqrt(log(mnot) * log(trsdia(1)))
    write(0,*) 1,mnot, trsdia(1),log(mnot), log(trsdia(1)),sqrt(log(trsdia(1))*log(mnot))
    DO i = 2, nsieves
      tmd(i) = sqrt(log(trsdia(i-1)) * log(trsdia(i)))
      write(0,*) i,trsdia(i-1),trsdia(i),log(trsdia(i-1)),log(trsdia(i)),sqrt(log(trsdia(i-1))*log(trsdia(i)))
    END DO
     tmd(nsieves+1) = sqrt(log(trsdia(nsieves)) * log(minf))
     write(0,*) nsieves+1,trsdia(nsieves),minf,log(trsdia(nsieves)),log(minf),sqrt(log(trsdia(nsieves))*log(minf))

    IF (btest(asd_flg, 0)) THEN
      write(UNIT=0,FMT="(A)",ADVANCE="YES") "mf2asd: compute xg (tmd) for each sieve cut"
      write(UNIT=0,FMT="(30(i8))",ADVANCE="YES") (i, i=1,nsieves+1)
      write(UNIT=0,FMT="(30(f8.3))",ADVANCE="YES") (tmd(i), i=1, nsieves+1)
      write(0,*)
    END IF

    alpha = 0.0
    beta = 0.0
    DO i = 1, nsieves+1
       ! sum diameters  & their squares, over all aggregate sizes
!       alpha = alpha + (mfr(i)*log(tmd(i)))
!       beta = beta + (mfr(i)*log(tmd(i))*log(tmd(i)))
       alpha = alpha + (mfr(i)*(tmd(i)))
       beta = beta + (mfr(i)*(tmd(i))*(tmd(i)))
    END DO
    gmdx = exp(alpha)
    gsdx = exp(sqrt(beta-alpha*alpha))
    IF (btest(asd_flg, 0)) THEN
       write(UNIT=0,FMT="(A,A,f8.5,A,f8.5)",ADVANCE="YES") 'mf2asd: ', 'gmdx: ', gmdx, ' gsdx: ', gsdx
       write(0,*)
    END IF


    ! restore modified geometric mean bin diameters
!    sdia(istart) = sdia_istart
!    sdia(istop) = sdia_istop

  END SUBROUTINE mf2asd
END MODULE asd_vars