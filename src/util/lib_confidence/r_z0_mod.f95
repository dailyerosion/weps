!$Author$
!$Date$
!$Revision$
!$HeadURL$

module r_z0_mod

contains

! Functions for signed log-likelihood ratio statistic from:
! Wu, J., A.C.M Wong, and G. Jiang. 2003. Likelihood-based Confidence
! intervals for a log-normal mean. Statistics in Medicine 22:1849-1860

subroutine r_z0(n,x,fvec,ni,iv,nr,rv,iflag)
    integer n,ni,iv(ni),nr,iflag
    double precision x(n),fvec(n),rv(nr)

    ! variables passed to rstar
    ! x(1) = psi
    ! iv(1) = nval
    ! rv(1) = psihat
    ! rv(2) = sigsqhat
    ! rv(3) = wbar1
    ! rv(4) = wbar2
    ! rv(5) = norm_z

    fvec(1) = r( x(1), rv(1), rv(2), rv(3), rv(4), iv(1) ) - rv(5)

end subroutine

double precision function r (psi, psihat, sigsqhat, wbar1, wbar2, nval)
    double precision, intent(in) :: psi, psihat, sigsqhat, wbar1, wbar2
    integer, intent(in) :: nval
    double precision :: sigsqhatpsi, r_psi, u_psi, B, one

    sigsqhatpsi = 2.0 * sqrt((psi+1)**2 + wbar2 - 2.0*psi*wbar1 - 2*psi) - 2.0
    B = psihat - psi
    one = 1.0
    IF (B .EQ. 0.0) B = +0.0
    r_psi = sign(one,B)*sqrt(nval*log(sigsqhatpsi/sigsqhat)+ nval*(wbar1-psi+sigsqhatpsi/2) )

    !write(*,*) "psihat, psi, sign(psihat-psi) ", psihat, psi, sign(1,psihat-psi)
    !write(*,*) "sigsqhatpsi, r_psi ", sigsqhatpsi, r_psi

    r = r_psi
end function

end module r_z0_mod

