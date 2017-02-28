Program tstasd6

!      include 'manage/asd.inc' !msieve = 26 (allocation for maximum number of sieves) and sdia(msieve) defined here

     include 'dummy.inc'  ! Dummy include file to allow cook to work correctly

     integer          msieve
     real             mingsd

     parameter (msieve = 26)
     parameter (mingsd = 2.0)

     integer          nsieve

     real    :: sdia(msieve)
     real    :: mdia(msieve+1)
     real    :: total = 0.0

     integer :: sr,l,i
     real    :: mnsize, mxsize
     real    :: massf(msieve+1,1) ! allocate space for the maximum number of sieve "cuts" (msieve+1)
     real    :: gmd_prime, gsd_prime, gmd2_prime, gsd2_prime

     real    :: initgmd = 3.75, initgsd = 39.55
     real    :: m_not = 0.005, m_inf = 1000.0

     nsieve = msieve - 1 ! number of sieves set to 25 here therefore (nsieve+1) is the number of sieve "cuts" used
     mnsize = 0.005
     mxsize = 1000.0


!    compute geometric mean (lognormal) distribution of sieve sizes (dia.) for each sieve cut
     write(0,*) "sieve sizes - dia. in (mm): sdia(i) values"
     do i = 1, nsieve
        sdia(i) = exp(log(mnsize) + i*(log(mxsize)-log(mnsize))/(nsieve+1))
     end do
     write(UNIT=0,FMT="(30(i8))",ADVANCE="YES") (i, i=1,nsieve)
     write(UNIT=0,FMT="(30(f8.3))",ADVANCE="YES") (sdia(i), i=1, nsieve)
     write(0,*)

!    compute geometric mean value - dia. size in (mm) for each sieve "cut"
     write(0,*) "compute geometric mean value for each sieve cut"
     mdia(1) = sqrt(mnsize*sdia(1))
     do i = 2, nsieve
        mdia(i) = sqrt(sdia(i)*sdia(i-1))
     end do
     mdia(nsieve+1) = sqrt(mxsize*sdia(nsieve))

     write(UNIT=0,FMT="(30(i8))",ADVANCE="YES") (i, i=1,nsieve+1)
     write(UNIT=0,FMT="(30(f8.3))",ADVANCE="YES") (mdia(i), i=1, nsieve+1)
     write(0,*)

     write(UNIT=6,FMT="(6(A))",ADVANCE="YES") '     m_not','     m_inf', &
                   '   initgmd', '   initgsd', ' gmd_prime', ' gsd_prime'

     write(UNIT=6,FMT="(4(f10.4))",ADVANCE="NO") m_not, m_inf, initgmd, initgsd

     gmd = ((initgmd * m_inf) + (m_inf * m_not) - (m_not * m_not)) / (initgmd + m_inf - m_not)
     gsd = ((initgsd * m_inf) + (m_inf * m_not) - (m_not * m_not)) / (initgsd + m_inf - m_not)

    write(UNIT=6,FMT="(2(f10.4))",ADVANCE="YES") gmd, gsd
    write(0,*)

! Convert to mass fractions - massf()

     call asd2m(m_not, m_inf, initgmd, initgsd, 1, massf)

     write(UNIT=0,FMT="(30(i8))",ADVANCE="YES") (i, i=1,nsieve+1)
     write(UNIT=0,FMT="(30(f8.3))",ADVANCE="YES") (massf(i,1), i=1, nsieve+1)

     total = 0.0
     do i=1, msieve
        total = total + massf(i,1)
     end do
     if (total < 1.0) then
         write(0,*) 'total: ', total
     end if
     write(0,*)

! Convert to GMD, GSD again

!     call m2asd(massf, 1, m_not, m_inf, gmd_prime, gsd_prime)
!subroutine m2asd1 (sdia, mf, mnot, minf, gmd_p, gsd_p)
     call m2asd1(sdia, massf, m_not, m_inf, gmd_prime, gsd_prime)
     write(UNIT=6,FMT="(6(A))",ADVANCE="YES") '     m_not','     m_inf', &
                  ' gmd_prime', ' gsd_prime', '       gmd', '       gsd' 
     write(UNIT=6,FMT="(4(f10.4))",ADVANCE="NO") m_not, m_inf, gmd_prime, gsd_prime

          gmd = ((gmd_prime *m_inf) + (m_inf * m_not) - (m_not * m_not)) / (gmd_prime + m_inf - m_not)
          gsd = ((gsd_prime *m_inf) + (m_inf * m_not) - (m_not * m_not)) / (gsd_prime + m_inf - m_not)

     write(UNIT=6,FMT="(2(f10.4))",ADVANCE="YES") gmd, gsd
     write(0,*)

  do l = 1, 250
     call asd2m(m_not, m_inf, gmd_prime, gsd_prime, 1, massf)

     write(UNIT=0,FMT="(30(i8))",ADVANCE="YES") (i, i=1,nsieve+1)
     write(UNIT=0,FMT="(30(f8.3))",ADVANCE="YES") (massf(i,1), i=1, nsieve+1)
     write(0,*)

! Convert to GMD, GSD again

!     call m2asd(massf, 1, m_not, m_inf, gmd_prime, gsd_prime)
!subroutine m2asd1 (sdia, mf, mnot, minf, gmd_p, gsd_p)
     call m2asd1(sdia, massf, m_not, m_inf, gmd_p, gsd_p)
     write(UNIT=6,FMT="(6(A))",ADVANCE="YES") '     m_not','     m_inf', &
                  ' gmd_prime', ' gsd_prime', '       gmd', '       gsd' 
     write(UNIT=6,FMT="(4(f10.4))",ADVANCE="NO") m_not, m_inf, gmd_prime, gsd_prime

          gmd = ((gmd_prime *m_inf) + (m_inf * m_not) - (m_not * m_not)) / (gmd_prime + m_inf - m_not)
          gsd = ((gsd_prime *m_inf) + (m_inf * m_not) - (m_not * m_not)) / (gsd_prime + m_inf - m_not)

     write(UNIT=6,FMT="(2(f10.4))",ADVANCE="YES") gmd, gsd
     write(0,*)
 end do

      stop
      end program


subroutine asd2m1 (nsieve, sdiam, mfr, mnot, minf, gmd_p, gsd_p)

  INTEGER, PARAMETER :: msieves = 25   ! Maximum number of sieves that can be used

integer :: nsieve
real    :: sdiam(msieves)
real    :: mfr(msieves+1)
real    :: mnot, minf, gmd_p, gsd_p

real    ::  d(msieves)
real    ::  lngmdp, lngsdp
real    ::  prev, this
integer ::  i, j

    do i = 1, nsieve
          d(i) = (sdiam(i)-mnot)*(minf-mnot)/(minf-sdiam(i))
    end do

   lngmdp= log(gmd_p)
   lngsdp= sqrt(2.0) * log(gsd_p)
   prev= 1.0

! compute each dia. cumulative probability
   do i = 1, nsieve
      if (sdiam(i) .le. mnot) then
         this = 1.0
         else if (sdiam(i) .lt. minf) then
           this = 0.5 -0.5*erf((alog(d(i)) - lngmdp) / lngsdp)
         else
           this = 0.0
      end if
!  compute mass fraction between prev and this dia
      mfr(i) = prev - this
      prev = this
!     write(*,*) 'asd2m:',i,sdiam(i),this,mfr(i,j)

!     if roundoff errors or otherwise results in negative
!     mass fraction then set to zero mass
      if (mfr(i) .lt. 0.0) then
         mfr(i) = 0.0
      else
         prev = this
      endif
!     if(j.eq.4) write(*,*) 'asd2m: mfr(',i,j,')',mfr(i,j)
  end do

! get mass fraction for upper-most sieve cut
  mfr(nsieve+1) = prev
! if( j.eq.1 )write(*,*)'asd2m: mfr(',nsieve+1,j,')',mfr(nsieve+1,j)

return
end subroutine asd2m1