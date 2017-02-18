      Program tstasd3

      use soil_data_struct_defs, only: soil_def, allocate_soil

      include 'p1werm.inc'
      include 'm1subr.inc'
      include 's1layr.inc'
      include 's1agg.inc'
      include 'manage/asd.inc' !msieve = 26 (number of sieve "cuts")

      type(soil_def), dimension(:), allocatable :: soil, soil2             ! structure with soil state and parameters as updated suring simulation

      integer :: alloc_stat = 0, sum_stat = 0
      integer :: nsubr = 1
      integer :: nsl = 1


      integer :: sr,lay,l,i
      real    :: massf(msieve+1,mnsz)
      real    :: gmd_prime, gsd_prime

      real    :: initgmd = 3.75 , initgsd = 39.55
      real    :: m_not = 0.005, m_inf = 1000.0

      logcas = 3
      nsieve = msieve - 1 ! number of sieves = 25
      mnsize = 0.005
      mxsize = 1000.0

! allocate space for "soil" structures
      allocate(soil(0:nsubr), stat=alloc_stat)
      sum_stat = sum_stat + alloc_stat
      allocate(soil2(0:nsubr), stat=alloc_stat)
      sum_stat = sum_stat + alloc_stat
      if( sum_stat .gt. 0 ) then
        write(0,*) "ERROR: unable to allocate enough memory for weps main data arrays."
      end if

! allocate space for soil layer data
      do sr=1, nsubr
        soil(sr)%nslay = nsl
        soil2(sr)%nslay = nsl
        call allocate_soil(soil(sr))  ! allocate layer arrays
        call allocate_soil(soil2(sr)) ! allocate layer arrays
        write (0,*) 'sr: ', sr, 'nsl: ',nsl, 'soil(sr)%nslay: ',soil(sr)%nslay, 'soil2(sr)%nslay: ', soil2(sr)%nslay
      end do

!     compute geometric mean (lognormal) distribution of sieve sizes (dia.) for each sieve cut
      write(0,*) "sieve cut sizes: sdia(i) values"
      do i = 1, nsieve
          sdia(i) = exp(log(mnsize) + i*(log(mxsize)-log(mnsize))/(nsieve+1))
          write(UNIT=0, FMT="((i3),(f8.3))", ADVANCE="NO") i, sdia(i)
      end do
      write(0,*)

!     compute geometric mean value (dia. size) for each sieve cut
      write(0,*) "compute geometric mean value for each sieve cut"
      mdia(1) = sqrt(mnsize*sdia(1))
      write(UNIT=0, FMT="((i3),(f8.3))", ADVANCE="NO") 1, mdia(1)
      do i = 2, nsieve
           mdia(i) = sqrt(sdia(i)*sdia(i-1))
           write(UNIT=0, FMT="((i3),(f8.3))", ADVANCE="NO") i, mdia(i)
      end do
      mdia(nsieve+1) = sqrt(mxsize*sdia(nsieve))
      write(UNIT=0, FMT="((i3),(f8.3))", ADVANCE="YES") nsieve+1, mdia(nsieve+1)

!     Initialize m_not, gmd, gsd and m_inf values
      do sr=1, nsubr
        do lay=1, soil(sr)%nslay
          soil(sr)%aslagn(lay) = m_not
          soil(sr)%aslagm(lay) = initgmd
          soil(sr)%as0ags(lay) = initgsd
          soil(sr)%aslagx(lay) = m_inf
        end do
      end do
      write(UNIT=6,FMT="(2(A), 6(A))",ADVANCE="YES") '  sr', ' lay', '     m_not', '       gmd', '       gsd', &
                                                                     '     m_inf', ' gmd_prime', '   m_prime'
      do sr=1, nsubr
        do lay=1, soil(sr)%nslay
          write(UNIT=6,FMT="(2(i4), 4(f10.4))",ADVANCE="NO") sr, lay, &
               soil(sr)%aslagn(lay), soil(sr)%aslagm(lay), soil(sr)%as0ags(lay), soil(sr)%aslagx(lay)

          gmd_prime = (soil(sr)%aslagm(lay)-soil(sr)%aslagn(lay)) * (soil(sr)%aslagx(lay)-soil(sr)%aslagn(lay)) / &
               (soil(sr)%aslagx(lay)-soil(sr)%aslagm(lay))
          gsd_prime = (soil(sr)%as0ags(lay)-soil(sr)%aslagn(lay)) * (soil(sr)%aslagx(lay)-soil(sr)%aslagn(lay)) / &
               (soil(sr)%aslagx(lay)-soil(sr)%as0ags(lay))
          write(UNIT=6,FMT="(2(f10.4))",ADVANCE="YES") gmd_prime, gsd_prime
        end do
      end do
      write(0,*)

! Convert to massf and back again, then print
      do sr=1, nsubr
        do l=1, soil(sr)%nslay
          call asd2m(soil(sr)%aslagn(l), soil(sr)%aslagx(l),                &
     &         soil(sr)%aslagm(l), soil(sr)%as0ags(l),                      &
     &         soil(sr)%nslay, massf)
          write(0,*) 'sr lay:',sr, l
          do i=1, msieve+1
                write(UNIT=0,FMT="((i3), (f8.3))",ADVANCE="NO") i, massf(i,sr)
          end do
          write(0,*)
        end do
      end do
      stop
      end program