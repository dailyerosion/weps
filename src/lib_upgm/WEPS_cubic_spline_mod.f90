!$Author: fredfox $
!$Date: 2016-12-16 18:07:13 -0700 (Fri, 16 Dec 2016) $
!$Revision: 14791 $
!$HeadURL: https://infosys.ars.usda.gov/svn/code/weps1/branches/weps.src.subregion.plants/src/util/lib_math/cubic_spline_mod.f95 $

module WEPS_cubic_spline_mod

! Taken from Press, W.H., B.P. Flannery, S.A. Teukoosky, W.T. Vetterling. 1986.
! Numberical Recipes: the Art of Scientific Computing. Cambridge University Press, Cambridge

    use constants, only: dp, int32
    implicit none

contains

    subroutine u_spline(x,y,n,yp1,ypn,y2)

! + + + argument definitions + + +
      real(dp)  x, y, yp1, ypn, y2
      integer(int32) n

! + + + local variables
      integer(int32) i, nmax, k
      real(dp)  u, sig, p, qn, un

      parameter (nmax=100)
      dimension x(n),y(n),y2(n),u(nmax)
      if (yp1.GT..99E30) then
        y2(1)=0.
        u(1)=0.
      else
        y2(1)=-0.5
        u(1)=(3./(x(2)-x(1)))*((y(2)-y(1))/(x(2)-x(1))-yp1)
      endif
      do 11 i=2,n-1
        sig=(x(i)-x(i-1))/(x(i+1)-x(i-1))
        p=sig*y2(i-1)+2.
        y2(i)=(sig-1.)/p
        u(i)=(6.*((y(i+1)-y(i))/(x(i+1)-x(i))-(y(i)-y(i-1))             &
     &      /(x(i)-x(i-1)))/(x(i+1)-x(i-1))-sig*u(i-1))/p
11    continue
      if (ypn.GT..99E30) then
        qn=0.
        un=0.
      else
        qn=0.5
        un=(3./(x(n)-x(n-1)))*(ypn-(y(n)-y(n-1))/(x(n)-x(n-1)))
      endif
      y2(n)=(un-qn*u(n-1))/(qn*y2(n-1)+1.)
      do 12 k=n-1,1,-1
        y2(k)=y2(k)*y2(k+1)+u(k)
12    continue
!       Debug lines. Remove after debugging.
!        do i=1,n
!                write(56,*)x(i),y(i),y2(i)
!        end do        
      return
    end subroutine u_spline

    subroutine u_splint(xa,ya,y2a,n,x,y)

! + + + argument definitions + + +
      real(dp)  x, y, y2a, ya, xa
      integer(int32) n

! + + + local variables
      integer(int32) klo, khi, k
      real(dp)  h, a, b

      dimension xa(n),ya(n),y2a(n)
      klo=1
      khi=n
!.... begin debug lines
!        do i=1,13
!           write (56,*)'n=',n,' xa(i)=', xa(i),' ya(i)=',ya(i),
!     &     ' y2a(i)=',y2a(i)
!        end do
! ......end debug lines          
1     if (khi-klo.gt.1) then
        k=(khi+klo)/2
        if(xa(k).gt.x)then
          khi=k
        else
          klo=k
        endif
      goto 1
      endif
      h=xa(khi)-xa(klo)
      IF (h.eq.0.) write(*,*) 'crop/splint.for: Bad XA input.'
      a=(xa(khi)-x)/h
      b=(x-xa(klo))/h
      y=a*ya(klo)+b*ya(khi)+                                            &
     &      ((a**3-a)*y2a(klo)+(b**3-b)*y2a(khi))*(h**2)/6.
!.... debug line
!       write(56,*)x,y
      return
    end subroutine u_splint

end module WEPS_cubic_spline_mod

