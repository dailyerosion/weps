!$Author$
!$Date$
!$Revision$
!$HeadURL$

module ci_select_mod

contains

subroutine ci_select(value, nval, prob, mean, ci_hi, ci_low)
    integer, intent (in) :: nval       ! number of values
    real, intent (in) :: value(nval)  ! values to be used for confidence interval construction
    real, intent (in) :: prob         ! confidence interval, 90% two sided specified as 0.9
    real, intent (out) :: mean, ci_hi, ci_low       ! mean and confidence interval values

    integer :: idx        ! loop counter
    integer :: nzero, ngtz  ! number of rotation values equal to zero, greater than zero

    ! check that there are enough numbers
    if( nval .lt. 3 ) then
       ! write(*,*) "Cannot do statistics with less than 3 values"
       mean = 0.0
       ci_hi = 0.0
       ci_low = 0.0
       return
    end if

    nzero = 0
    ngtz = 0
    do idx = 1, nval
      ! count of zero and greater than zero values
       if( value(idx) .gt. 0.0 ) then
          ngtz = ngtz + 1
       else
          nzero = nzero + 1
       end if
    end do

    if( nzero .gt. 0 ) then
       ! have zero values
       if( ngtz .gt. 0 ) then
          call ci_logwithzeros(value, nval, nzero, ngtz, prob, mean, ci_hi, ci_low)
       else
          ! all zero values, no ci, just probability
          mean = 0.0
          ci_hi = 0.0
          ci_low = 0.0
       end if
    else
       ! all values greater than zero
       call ci_lognozeros(value, nval, prob, mean, ci_hi, ci_low)
    end if

end subroutine

subroutine ci_logwithzeros(value, nval, nzero, ngtz, prob, mean, ci_hi, ci_low)

    use r0_z0_mod, only: r0_z0

    ! input values are random numbers drawn from a log-normal distribution
    ! taking the log of each values transforms them into normal deviates
    ! the number of values greater than zero must be at least 2
    integer, intent (in) :: nval       ! number of values
    real, intent (in) :: value(nval)  ! values to be used for confidence interval construction
    integer, intent (in) :: nzero, ngtz  ! number of values equal to zero, greater than zero
    real, intent (in) :: prob         ! confidence interval, 90% two sided specified as 0.9
    real, intent (out) :: mean, ci_hi, ci_low       ! mean and confidence interval values

    real :: normvalue(ngtz), normsq(ngtz)  ! log of values and square of log values
    real :: s_sum, t_sum    ! S and T in Tian and Wu equation 12
    real :: muhat, etahat, psihat ! Tian and Wu equation 13
    real :: norm_dev                   ! amount to add to normalized mean, plus and minus
    real :: temp          ! temporary value

    double precision :: norm_z         ! z value returned

    ! variables required to use hybrd.f
    integer :: info, nfev, iv(2)
    double precision :: x(1), fvec(1), rv(6), diag(1), fjac(1,1), r(1)
    double precision :: qtf(1),wa1(1),wa2(1),wa3(1),wa4(1)

    real :: psi_hi, psi_low

    ! calculate CI using method from:
    ! Tian, L. and J. Wu. 2006. Confidence Intervals for the Mean of
    ! Lognormal Data with Excess Zeros. Biometrical Journal 48(1):149-156
    ! transform non-zero values for statistics
    call make_norm(nval, value, ngtz, normvalue)
    ! find squares
    call make_sq(normvalue, ngtz, normsq)
    ! Sum of log(x) values
    s_sum = sum(normvalue)
    ! sum of (log(x))**2 values
    t_sum = sum(normsq)
    muhat = s_sum / ngtz
    etahat = t_sum / ngtz - s_sum * s_sum / ngtz /ngtz
    psihat = log(1.0*ngtz/nval) + muhat + etahat/2.0

    ! method cannot handle only one non-zero value
    ! negative etahat fails in sqrt below (triggered with 2 non-zero values less than 1)
    ! return with guess only
    if( (ngtz .eq. 1) .or. (etahat .le. 0) ) then
        mean = exp(psihat)
        ci_hi = exp(normvalue(ngtz))
        ci_low = 0.0
        return
    end if

    ! find test statistic from standard normal
    ! given two sided % confidence interval, 0.9 gives 90% confidence interval
    norm_z = norm_z_stat( dble(prob) )

    ! find deviation from mean (modified Cox method) as initial estimate for psi_lo and psi_hi
    norm_dev = norm_z * sqrt( etahat/ngtz + etahat*etahat/2/(ngtz-1))

    ! find psi for lower ci limit
    x(1) = dble(psihat - norm_dev)
    iv(1) = nzero
    iv(2) = ngtz
    rv(1) = dble(psihat)
    rv(2) = dble(muhat)
    rv(3) = dble(etahat)
    rv(4) = dble(t_sum)
    rv(5) = dble(s_sum)
    rv(6) = dble(norm_z)
    ! call for signed log-likilihood ratio
    call hybrd(r0_z0,1,x,fvec,2,iv,6,rv,1.0d-6,100,1,1, &
               1.0d-9,diag,1,1.0d2,0,info,nfev,fjac, &
               1,r,1,qtf,wa1,wa2,wa3,wa4)
    if( info .lt. 0 ) then
       psi_low = -10.0
    else
       psi_low = real(x(1))
    end if

    ! find psi for upper ci limit
    x(1) = dble(psihat + norm_dev)
    iv(1) = nzero
    iv(2) = ngtz
    rv(1) = dble(psihat)
    rv(2) = dble(muhat)
    rv(3) = dble(etahat)
    rv(4) = dble(t_sum)
    rv(5) = dble(s_sum)
    rv(6) = dble(-norm_z)
    ! call for signed log-likilihood ratio
    call hybrd(r0_z0,1,x,fvec,2,iv,6,rv,1.0d-6,100,1,1, &
               1.0d-9,diag,1,1.0d2,0,info,nfev,fjac, &
               1,r,1,qtf,wa1,wa2,wa3,wa4)
    if( info .lt. 0 ) then
       psi_hi = 10.0
    else
       psi_hi = real(x(1))
       if( psi_hi .gt. 10.0 ) then
           psi_hi = 10.0
           write(*,*) "# WARNING, confidence interval high value out of range, ", nval, "values."
       end if
    end if
    !write(*,*) "psi-cox-hi, psi-hi, fvec, nfev ", psihat+norm_dev, psi_hi, fvec(1), nfev

    ! transform back to log normal
    mean = exp(psihat)
    ci_hi = exp(psi_hi)
    ci_low = exp(psi_low)

end subroutine

subroutine ci_lognozeros(value, nval, prob, mean, ci_hi, ci_low)

    use rstar_z0_mod, only: rstar_z0
    use r_z0_mod, only: r_z0

    ! input values are random numbers drawn from a log-normal distribution
    ! taking the log of each values transforms them into normal deviates
    integer, intent (in) :: nval       ! number of values
    real, intent (in) :: value(nval)  ! values to be used for confidence interval construction
    real, intent (in) :: prob         ! confidence interval, 90% two sided specified as 0.9
    real, intent (out) :: mean, ci_hi, ci_low       ! mean and confidence interval values

    real :: normvalue(nval)  ! log of values to be used
    real :: normmean, normvar, normstdev ! mean, variance and standard deviation of log(x) values
    real :: normdiffsq(nval)
    double precision :: norm_z         ! z value returned
    real :: norm_dev                   ! amount to add to normalized mean, plus and minus
    real :: temp

    real :: wbar1, wbar2, sigsqhat, psihat ! see definitions in Wu, Wong and Hang
    real :: psi_hi, psi_low, normsq(nval)

    ! variables required to use hybrd.f
    integer :: info, nfev, iv(1)
    double precision :: x(1), fvec(1), rv(5), diag(1), fjac(1,1), r(1)
    double precision :: qtf(1),wa1(1),wa2(1),wa3(1),wa4(1)

    ! calculate CI using method from:
    ! Wu, J., A.C.M Wong, and G. Jiang. 2003. Likelihood-based Confidence
    ! Intervals for a Lognormal mean. Statistics in Medicine 22:1849-1860
    ! transform to log values (normal)
    call make_norm(nval, value, nval, normvalue)
    ! find mean of transformed values (normal)
    normmean = sum(normvalue) / nval
    ! take diff and square (normal)
    call diff_sq(normvalue, nval, normmean, normdiffsq)
    ! find standard deviation (normal)
    normvar = sum(normdiffsq) / (nval-1)
    normstdev = sqrt(normvar)

    ! find test statistic from standard normal
    ! given two sided % confidence interval, 0.90 gives 90% confidence interval
    norm_z = norm_z_stat( dble(prob) )

    ! find deviation from mean (Cox method) as initial estimate for psi_lo and psi_hi
    norm_dev = norm_z * sqrt( normvar/nval + normvar*normvar/2/(nval-1))

    ! calculate CI using method from Wu, Wong and Hang
    ! sums (normal)
    wbar1 = sum(normvalue) / nval
    ! get squared values (normal)
    call make_sq(normvalue, nval, normsq)
    wbar2 = sum(normsq) / nval
    ! estimate variance of normal
    sigsqhat = wbar2 - wbar1*wbar1
    ! estimate mean of normal
    psihat = wbar1 + sigsqhat/2

    ! Output COX estimate
    temp = normmean + normvar / 2
    !mean = exp(temp)
    !ci_hi = exp(temp + norm_dev)
    !ci_low = exp(temp - norm_dev)
    !return
    !write(*,*) "muhat, sigsqhat, S^2, psihat", normmean, sigsqhat, normvar, psihat
    !write(*,*) "mean, cox-low, cox-hi ", exp(temp), exp(temp - norm_dev), exp(temp + norm_dev)
    !write(*,*) "log of mean, cox-low, cox-hi ", temp, temp - norm_dev, temp + norm_dev

    ! find psi for lower ci limit
    x(1) = psihat - norm_dev
    iv(1) = nval
    rv(1) = psihat
    rv(2) = sigsqhat
    rv(3) = wbar1
    rv(4) = wbar2
    rv(5) = norm_z
    ! call for signed log-likilihood ratio (works)
    !call hybrd(r_z0,1,x,fvec,1,iv,5,rv,1.0d-6,100,1,1, &
    !           1.0d-9,diag,1,1.0d2,0,info,nfev,fjac, &
    !           1,r,1,qtf,wa1,wa2,wa3,wa4)
    ! call for modified signed log-likilihood ratio
    call hybrd(rstar_z0,1,x,fvec,1,iv,5,rv,1.0d-6,100,1,1, &
               1.0d-9,diag,1,1.0d2,0,info,nfev,fjac, &
               1,r,1,qtf,wa1,wa2,wa3,wa4)
    psi_low = x(1)
    !write(*,*) "psi-cox-low, psi-low, fvec, nfev ", psihat-norm_dev, psi_low, fvec(1), nfev

    ! output rstar for a given sample size
    !psi_hi = 0.0
    !if( nval .eq. 10 ) then
    !   do temp = psihat + norm_dev, psihat - norm_dev, -norm_dev/10
    !      temp1 = rstar(temp, rv(1), rv(2), rv(3), rv(4), nval)
    !      write(*,*) "psi, rstar ", temp, temp1
    !   end do
    !end if
    !return

    ! find psi for upper ci limit
    x(1) = psihat + norm_dev
    rv(5) = -norm_z
    ! call for signed log-likilihood ratio (works)
    !call hybrd(r_z0,1,x,fvec,1,iv,5,rv,1.0d-4,100,1,1, &
    !           1.0d-7,diag,1,1.0d2,0,info,nfev,fjac, &
    !           1,r,1,qtf,wa1,wa2,wa3,wa4)
    !call for modified signed log-likilihood ratio
    call hybrd(rstar_z0,1,x,fvec,1,iv,5,rv,1.0d-4,100,1,1, &
               1.0d-7,diag,1,1.0d2,0,info,nfev,fjac, &
               1,r,1,qtf,wa1,wa2,wa3,wa4)
    psi_hi = x(1)
    !write(*,*) "psi-cox-hi, psi-hi, fvec, nfev ", psihat+norm_dev, psi_hi, fvec(1), nfev

    ! transform back to log normal
    mean = exp(psihat)
    ci_hi = exp(psi_hi)
    ci_low = exp(psi_low)
    
end subroutine

double precision function norm_z_stat(prob)
    double precision, intent(in) :: prob      ! confidence level desired

    integer :: st_sel     ! select which value will be calculated by cdfnor
    double precision :: twoside_prob  ! 2 sided probabiliy for lookup
    double precision :: com_prob  ! 1 minus 2 sided probability
    double precision :: stdnormmean    ! standard normal mean value
    double precision :: stdnormstdev   ! standard normal standard deviation
    double precision :: norm_z         ! z value returned
    integer :: status                  ! status return from cdft
    double precision bound             ! bound exceeded with non-zero status
    ! get z value
    ! convert lookup to a two sized probability
    twoside_prob = 0.5*(1.0d0 + prob)
    st_sel = 2   ! returns the z value
    com_prob = 1.0 - twoside_prob
    stdnormmean = 0.0
    stdnormstdev = 1.0
    call cdfnor(st_sel, twoside_prob, com_prob, norm_z, stdnormmean, stdnormstdev, status, bound)
    !write(*,*) "norm_z = ", norm_z

    if(status .gt. 0) then
       write(*,*) "WARNING: invalid standard normal z value. Confidence Interval not yet valid. Status = ", status
       norm_z_stat = 0.0
    else
       norm_z_stat = norm_z
    end if

end function

subroutine make_norm(nval, value, ngtz, logvalue)
    integer, intent (in) :: nval, ngtz ! number of onput values, number of output value greater than zero
    real, intent (in) :: value(nval)  ! values to be used for confidence interval construction
    real, intent (out) :: logvalue(ngtz)  ! log of values to be used

    integer :: idx, jdx        ! loop counters

    jdx = 0
    do idx = 1, nval
       if( value(idx) .gt. 0.0 ) then
          jdx = jdx + 1
          logvalue(jdx) = log(value(idx))
       end if
    end do
end subroutine

subroutine make_sq(value, nval, sq_val)
    integer, intent (in) :: nval      ! number of values
    real, intent (in) :: value(nval)  ! values to be differenced
    real, intent (out) :: sq_val(nval)  ! differenced and squared values

    integer :: idx        ! loop counter

    do idx = 1, nval
       sq_val(idx) = (value(idx))**2
    end do
end subroutine

subroutine diff_sq(value, nval, meanval, diff_sq_val)
    integer, intent (in) :: nval      ! number of values
    real, intent (in) :: value(nval)  ! values to be differenced
    real, intent (in) :: meanval      ! central value to use for differencing
    real, intent (out) :: diff_sq_val(nval)  ! differenced and squared values

    integer :: idx        ! loop counter

    do idx = 1, nval
       diff_sq_val(idx) = (value(idx) - meanval)**2
    end do
end subroutine

end module ci_select_mod

