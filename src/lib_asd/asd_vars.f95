MODULE asd_vars

  IMPLICIT NONE

  INTEGER :: alloc_asd_vars            ! Allocate status return

  INTEGER, PARAMETER :: msieves = 25   ! Maximum number of sieves that can be used
  INTEGER :: nsieves                   ! Number of sieves actually used

  REAL, DIMENSION(:), ALLOCATABLE :: sdia    ! Array holding the sieve size diameters used to compute the sieve cuts (mm)
  REAL, DIMENSION(:), ALLOCATABLE :: trdia   ! Array holding the transformed sieve size diameters used to compute the sieve cuts (mm)
  REAL, DIMENSION(:), ALLOCATABLE :: gmdia   ! Array holding the geometric mean diameters of the sieve cuts (mm)
  REAL, DIMENSION(:), ALLOCATABLE :: mf      ! Array holding the actual number of sieve "cuts" (nsieve+1)
  REAL :: mnsize                       ! Minimum sieve size to use for computing the lower sieve cut geometric mean diameter (mm)
  REAL :: mxsize                       ! Maximum sieve size to use for computing the upper sieve cut geometric mean diameter (mm)

  REAL :: m_not                        ! Minimum sized diameter aggregate (mm)
  REAL :: m_inf                        ! Maximum sized diamater aggregate (mm)

  include "p1werm.inc"

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
    INTEGER :: debug = 1                ! Initialize local "debug" variable

    INTRINSIC ASSOCIATED                ! use to verify status of pointers

    ret_status = 0                      ! initialize to zero (won't let me in definition)

    IF (ns > 0 .and. ns <= msieves) THEN
      nsieves = ns                      ! Set the actual number of sieves to use to 25
    ELSE
      write(0,*) "Error: The number of sieves specified", ns, "is less than 1 or greater than ", msieves
      ret_status = ret_status + 1
      call EXIT (ret_status)
    END IF
    IF (debug > 1) THEN
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

    IF (ALLOCATED (trdia) .neqv. .TRUE.) THEN
      ALLOCATE (trdia(nsieves), STAT = alloc_status)
      IF (alloc_status /= 0) THEN
        write(0,*) "Error allocating trdia(nsieves) - alloc_status: ", alloc_status
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
    IF (debug > 0) THEN
      write(0,*) "sieve sizes - dia. in (mm): sdia(i) values"
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
    IF (debug > 0) THEN
      write(UNIT=0,FMT="(A,f8.3,A,f8.3)",ADVANCE="YES") "m_not: ", m_not, " m_inf: ", m_inf
    END IF

    ! Compute the geometric mean diameter for each sieve cut
    gmdia(1) = sqrt(minsize*sdia(1))
    DO i = 2, nsieves
      gmdia(i) = sqrt(sdia(i)*sdia(i-1))
    END DO
    gmdia(nsieves+1) = sqrt(maxsize*sdia(nsieves))
    IF (debug > 0) THEN
      write(0,*) "compute geometric mean value for each sieve cut"
      write(UNIT=0,FMT="(30(i8))",ADVANCE="YES") (i, i=1,nsieves+1)
      write(UNIT=0,FMT="(30(f8.3))",ADVANCE="YES") (gmdia(i), i=1, nsieves+1)
      write(0,*)
    END IF

    write(0,*) "asd_init_vars() ret_status: ", ret_status
   END FUNCTION asd_init_vars
!-----------------------------------------------------------------------------------------------------------------------------------------
  ! Compute the mass fractions in each sieve cut when given gmdx,gsdx,minf,mnot values
  SUBROUTINE asd2mf (gmdx, gsdx, mnot, minf, mfr)

    REAL :: gmdx, gsdx, mnot, minf, mfr(*)

    INTEGER :: debug = 1                ! Initialize local "debug" variable
    INTEGER :: i                        ! local loop variable
    REAL :: this, prev                  ! local temporary probability variables

    REAL :: lngmdx = 0.0
    REAL :: lngsdx = 0.0

    ! Compute transformed sieve dia. sizes
    DO i = 1, nsieves
      IF (sdia(i) .lt. minf) THEN
        trdia(i) = (sdia(i)-mnot) * (minf-mnot) / (minf-sdia(i))
      END IF
    END DO

    lngmdx= log(gmdx)
    lngsdx= sqrt(2.0 * log(gsdx))
    prev= 1.0

    ! Compute each dia. cumulative probability
    DO i = 1, nsieves
      IF (sdia(i) .le. mnot) THEN
        this = 1.0
      ELSE IF (sdia(i) .lt. minf) THEN
        this = 0.5 -0.5*erf((alog(trdia(i)) - lngmdx) / lngsdx)
      ELSE
        this = 0.0
      END IF

      ! Compute mass fraction between prev and this dia
      mfr(i) = prev - this
      prev = this
!      IF (debug > 0) THEN
!        write(0,*) 'asd2mf:',i,sdia(i),this,mfr(i)
!      END IF

      ! If roundoff errors or otherwise results in negative mass fraction THEN set to zero mass
      IF (mfr(i) .lt. 0.0) THEN
        mfr(i) = 0.0
      ELSE
        prev = this
      END IF
!      IF (debug > 0) THEN
!        write(0,*) 'asd2mf: mfr(',i,')',mfr(i)
!      END IF
    END DO
    ! Get mass fraction for upper-most sieve cut
    mfr(nsieves+1) = prev

    ! Zero out the rest of the array which is used every where else
    DO i=nsieves+2, msieves+1
      mfr(i) = 0.0
    END DO

    IF (debug > 0) THEN
     write(0,*)'asd2mf: mfr(nsieves+1)'
     write(UNIT=0,FMT="(30(i8))",ADVANCE="YES") (i, i=1,nsieves+1) 
     write(UNIT=0,FMT="(30(f8.3))",ADVANCE="YES") (mfr(i), i=1, nsieves+1)
    END IF

  END SUBROUTINE asd2mf

END MODULE asd_vars