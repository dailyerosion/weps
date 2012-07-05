!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!

      subroutine splint(xa,ya,y2a,n,x,y)

! + + + argument definitions + + +
      real x, y, y2a, ya, xa
      integer n

! + + + local variables
      integer klo, khi, k
      real h, a, b

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
      end
