!$Author$
!$Date$
!$Revision$
!$HeadURL$

! Functions for signed log-likelihood ratio statistic from:
! Tian, L. and J. Wu. 2006. Confidence Intervals for the Mean of 
! Lognormal Data with Excess Zeros. Biometrical Journal 48:149-156

subroutine r0_z0(n,x,fvec,ni,iv,nr,rv,iflag)
    integer n,ni,iv(ni),nr
    integer, intent(inout) :: iflag
    double precision x(n),fvec(n),rv(nr)
    double precision :: r0

    ! variables passed to r0
    ! x(1) = psi
    ! iv(1) = nzero
    ! iv(2) = ngtz
    ! rv(1) = psihat
    ! rv(2) = muhat
    ! rv(3) = etahat
    ! rv(4) = t_sum
    ! rv(5) = s_sum

    fvec(1) = r0( x(1), iv(1), iv(2), rv(1), rv(2), rv(3), rv(4), rv(5), iflag ) - rv(6)

end subroutine

double precision function r0(psi, nzero, ngtz, psihat, muhat, etahat, t_sum, s_sum, iflag)
    double precision, intent(in) :: psi, psihat, muhat, etahat, t_sum, s_sum
    integer, intent(in) :: nzero, ngtz
    integer, intent(inout) :: iflag
    double precision :: muhatpsi, etahatpsi

    double precision :: log_likelihood !, r0 !Absoft compiler doesn't like the duplicate declaration
    double precision tmp0, B, one

    double precision :: constraint, muint, etaint

    ! variables required to use hybrd.f
    integer :: info, nfev, iv(2)
    double precision :: x(2), fvec(2), rv(3), diag(2), fjac(2,2), r(3)
    double precision :: qtf(2),wa1(2),wa2(2),wa3(2),wa4(2)

    external fnu

    ! check solution constraint to get intial guess in proper region
    ! initial guess
    muhatpsi  = muhat
    etahatpsi = etahat
    constraint = psi - muhatpsi - etahatpsi/2
    !write(*,*) "psi, muhatpsi, etahatpsi, constraint", psi, muhatpsi, etahatpsi, constraint
    ! find the point of intersection of the perpendicular to constraint boundary
    muint = 0.4*(2*psi + 0.5*muhatpsi - etahatpsi)
    etaint = 0.4*(psi - muhatpsi + 2*etahatpsi)
    !write(*,*) "intersection point - muint, etaint", muint, etaint
    ! check value against constraint boundary
    if( constraint .ge. 0.0 ) then
       ! point is in wrong region, out of bounds, refect point into solution region
       muhatpsi = 2*muint - muhatpsi
       etahatpsi = 2*etaint - etahatpsi
       constraint = psi - muhatpsi - etahatpsi/2
       !write(*,*) "nfev, psi, muhatpsi, etahatpsi, constraint", 0, psi, muhatpsi, etahatpsi, constraint
    end if
    nfev = 0
    do while( nfev .lt. 10 )
       ! evaluate functions
       x(1) =  muhatpsi
       x(2) =  etahatpsi
       iv(1) = nzero
       iv(2) = ngtz
       rv(1) = psi 
       rv(2) = s_sum
       rv(3) = t_sum
       !write(*,*) "nfev, psi, muhatpsi, etahatpsi, constraint", nfev, psi, muhatpsi, etahatpsi, constraint
       call fnu(2,x,fvec,2,iv,3,rv,info)
       nfev = nfev + 1
       if( (fvec(1) .lt. 0.0d0) .or. (fvec(2) .lt. 0.0d0) ) then
          muhatpsi = 0.5*(muint + muhatpsi)
          etahatpsi = 0.5*(etaint + etahatpsi)
          constraint = psi - muhatpsi - etahatpsi/2
          !write(*,*) "nfev, psi, muhatpsi, etahatpsi, constraint", nfev, psi, muhatpsi, etahatpsi, constraint
       else
          exit
       end if
    end do 
    
       
    ! for value of psi, find muhatpsi, etahatpsi
    x(1) =  muhatpsi
    x(2) =  etahatpsi
    iv(1) = nzero
    iv(2) = ngtz
    rv(1) = psi 
    rv(2) = s_sum
    rv(3) = t_sum
    ! call for signed log-likilihood ratio
    call hybrd(fnu,2,x,fvec,2,iv,3,rv,1.0d-6,100,1,1, &
               1.0d-9,diag,1,1.0d2,0,info,nfev,fjac, &
               2,r,3,qtf,wa1,wa2,wa3,wa4)
    muhatpsi  = x(1) 
    etahatpsi = x(2) 

    if( info .ne. 1 ) then
       iflag = -info
       !write(*,*) "info, muhatpsi, etahatpsi ", info, muhatpsi, etahatpsi
       write(*,*) "# WARNING, confidence interval calculation did not converge, ", nzero+ngtz, " values."
       r0 = 0.0d0
       return
    end if

    tmp0 = 0.0d0
    constraint = psi - muhatpsi - etahatpsi/2
    if( (constraint .ge. 0.0d0) .or. (etahatpsi .le. 0.0d0) ) then
       iflag = -1
       !write(*,*) "muhat ", muhat
       !write(*,*) "etahat ", etahat
       !write(*,*) "nzero ", nzero
       !write(*,*) "ngtz ", ngtz
       !write(*,*) "psi ", psi
       !write(*,*) "s_sum ", s_sum
       !write(*,*) "t_sum ", t_sum
       !write(*,*) "muhatpsi ", muhatpsi
       !write(*,*) "etahatpsi ", etahatpsi
       !write(*,*) "constraint", constraint
       write(*,*) "# WARNING, confidence interval calculation, bad likelihood, ", nzero+ngtz, " values."
    else
       ! signed log-likelihood ratio
       B = psihat - psi
       one = 1.0
       IF (B .EQ. 0.0) B = +0.0
       tmp0 = sign(one, B) &
          * sqrt( 2.0 &
          * (log_likelihood(nzero, ngtz, psihat, muhat, etahat, t_sum, s_sum) &
          - log_likelihood(nzero, ngtz, psi, muhatpsi, etahatpsi, t_sum, s_sum)))
    end if
    r0 = tmp0
end function

subroutine fnu(n,x,fvec,ni,iv,nr,rv,iflag)
    integer, intent(in) :: n,ni,iv(ni),nr
    integer, intent(inout) :: iflag
    double precision, intent(in) :: rv(nr)
    double precision, intent(inout) :: x(n)
    double precision, intent(out) :: fvec(n)
    double precision B, one

    integer :: nzero, ngtz
    double precision :: constraint, muhatpsi, etahatpsi, psi, s_sum, t_sum 

    ! variables passed to fnu
    muhatpsi  = x(1) 
    etahatpsi = x(2) 
    nzero  = iv(1)
    ngtz   = iv(2)
    psi    = rv(1)
    s_sum  = rv(2)
    t_sum  = rv(3)
                   
    constraint = psi - muhatpsi - etahatpsi/2
    ! check for out of range value
    if( constraint > 700.0 ) then
        ! return flag indicating failure of routine, please terminate
        iflag = -1
    else
        !write(*,*) 'fnu: constraint: ', constraint
        fvec(1) = (nzero * exp(constraint)) &
                / (1 - exp(constraint)) &
                - ngtz &
                + s_sum / etahatpsi &
                - ngtz * muhatpsi / etahatpsi
        fvec(2) = (nzero * exp(constraint)) &
                / 2 / (1 - exp(constraint)) &
                - ngtz / 2.0 &
                - ngtz / 2.0 / etahatpsi &
                + t_sum / 2.0 / etahatpsi / etahatpsi &
                - muhatpsi * s_sum / etahatpsi / etahatpsi &
                + ngtz * muhatpsi * muhatpsi / 2.0 / etahatpsi / etahatpsi

        ! check constraint
        !constraint = psi - muhatpsi - etahatpsi/2
        if( constraint .gt. 0 ) then
          one = 1.0
          B = fvec(1)
          IF (B .EQ. 0.0) B = +0.0
           fvec(1) = fvec(1) + sign(one,B)*constraint
          B = fvec(2)
          IF (B .EQ. 0.0) B = +0.0
           fvec(2) = fvec(2) + sign(one,B)*constraint
        end if
    end if

end subroutine

double precision function log_likelihood(nzero, ngtz, psi, mu, eta, t_sum, s_sum)
    integer, intent(in) :: nzero, ngtz
    double precision, intent(in) :: psi, mu, eta, t_sum, s_sum

    real pi
    parameter (pi = 3.1415926535)

    log_likelihood = nzero * log(1 - exp(psi - mu - eta/2)) &
                   + ngtz * (psi - mu - eta/2) &
                   - ngtz * log(2*pi) / 2 &
                   - ngtz * log(eta) / 2 &
                   - t_sum / 2 / eta &
                   + mu * s_sum / eta &
                   - ngtz * mu * mu / 2 / eta
end function
