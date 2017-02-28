     Program tstasd5

     use soil_data_struct_defs, only: soil_def, allocate_soil

     include 'p1werm.inc'
     include 'm1subr.inc'
     include 's1layr.inc'
     include 's1agg.inc'
     include 'manage/asd.inc' !msieve = 26 (allocation for maximum number of sieves) and sdia(msieve) defined here

     integer :: alloc_stat = 0, sum_stat = 0
     integer :: nsubr = 1
     integer :: nsl = 1

     real    :: total = 0.0, log_mnsize, log_mxsize

     integer :: sr,l,i
     real    :: massf(msieve+1,1) ! allocate space for the maximum number of sieve "cuts" (msieve+1)
     real    :: gmd_prime, gsd_prime, gmd2_prime, gsd2_prime, gmdbad, gsdbad

     real    :: initgmd = 3.75 , initgsd = 39.55
     real    :: m_not = 0.005, m_inf = 1000.0

     logcas = 3
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

     write(UNIT=6,FMT="(8(A))",ADVANCE="YES") '     m_not', '     m_inf', &
          '   initgmd', '   initgsd', '       gmd', '       gsd', ' gmd_prime', ' gsd_prime'

     write(UNIT=6,FMT="(4(f10.4))",ADVANCE="NO") m_not, m_inf, initgmd, initgsd

    gmd = ((initgmd * m_inf) + (m_inf * m_not) - (m_not * m_not)) / (initgmd + m_inf - m_not)
    gsd = ((initgsd * m_inf) + (m_inf * m_not) - (m_not * m_not)) / (initgsd + m_inf - m_not)

     gmd_prime = (gmd - m_not) * (m_inf - m_not) / (m_inf - gmd)
     gsd_prime = (gsd - m_not) * (m_inf - m_not) / (m_inf - gsd)

     write(UNIT=6,FMT="(6(f10.4))",ADVANCE="YES") gmd, gsd, gmd_prime, gsd_prime
     write(0,*)

     if (initgmd .ne. gmd_prime) then
        write(6,*) "Error!!! ", "initgmd", initgmd, "!=", gmd_prime, "gmd_prime"
     else
        write(6,*) "Correct!!! ", "initgmd", initgmd, "==", gmd_prime, "gmd_prime"
     end if
     write(6,*)

! Convert to mass fractions - massf()
     call asd2m(m_not, m_inf, initgmd, initgsd, 1, massf)

     write(UNIT=0,FMT="(30(i8))",ADVANCE="YES") (i, i=1,nsieve+1)
     write(UNIT=0,FMT="(30(f8.3))",ADVANCE="YES") (massf(i,1), i=1, nsieve+1)

     total = 0.0
     do i=1, msieve
        total = total + massf(i,1)
     end do
     if (total .ne. 1.0) then
         write(0,*) 'total: ', total
     end if
     write(0,*)

! Convert to GMD, GSD again
     call m2asd(massf, 1, m_not, m_inf, gmd_prime, gsd_prime)

      gmd = ((gmd_prime *m_inf) + (m_inf * m_not) - (m_not * m_not)) / (gmd_prime + m_inf - m_not)
      gsd = ((gsd_prime *m_inf) + (m_inf * m_not) - (m_not * m_not)) / (gsd_prime + m_inf - m_not)

     write(UNIT=6,FMT="(6(A))",ADVANCE="YES") &
       '     m_not', '     m_inf', ' gmd_prime', ' gsd_prime', '       gmd', '       gsd'
     write(UNIT=6,FMT="(6(f10.4))",ADVANCE="YES") m_not, m_inf, gmd_prime, gsd_prime, gmd, gsd
     write(0,*)

  do l = 1, 5 !Run multiple loops, asd2m() and m2asd(), here
     call asd2m(m_not, m_inf, gmd_prime, gsd_prime, 1, massf)

     write(UNIT=0,FMT="(30(i8))",ADVANCE="YES") (i, i=1,nsieve+1)
     write(UNIT=0,FMT="(30(f8.3))",ADVANCE="YES") (massf(i,1), i=1, nsieve+1)
     write(0,*)
! Convert to GMD, GSD
     call m2asd(massf, 1, m_not, m_inf, gmd_prime, gsd_prime)

      gmd = ((gmd_prime *m_inf) + (m_inf * m_not) - (m_not * m_not)) / (gmd_prime + m_inf - m_not)
      gsd = ((gsd_prime *m_inf) + (m_inf * m_not) - (m_not * m_not)) / (gsd_prime + m_inf - m_not)

     write(UNIT=6,FMT="(6(A))",ADVANCE="YES") &
      '     m_not','     m_inf', ' gmd_prime', ' gsd_prime', '       gmd', '       gsd' 
     write(UNIT=6,FMT="(6(f10.4))",ADVANCE="YES") m_not, m_inf, gmd_prime, gsd_prime, gmd, gsd
     write(0,*)
 end do
      stop
      end program
