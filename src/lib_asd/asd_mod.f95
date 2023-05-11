!$Author$
!$Date$
!$Revision$
!$HeadURL$

module asd_mod

  integer, parameter :: msieve = 26  ! NOTE: values greater than 67 will fail in mproc_soil_mod/crush
                                     ! The binomial distribution will return nonsense values.
  double precision, parameter :: mingsd = 2.0d0

  double precision :: sdia(msieve) ! array containing sieve size diameters
  double precision :: mnsize       ! minimum (imaginary) sieve size to use for computing
                                   ! lower sieve cut geometric mean diameter
  double precision :: mxsize       ! maximum (imaginary) sieve size to use for computing
                                   ! upper sieve cut geometric mean diameter
  double precision :: mdia(msieve+1) ! array containing gmd sieve cut diameters
  integer :: nsieve  ! number of sieves used
  integer :: logcas  ! flag to represent which lognormal case to apply


  contains

    subroutine asdini()

      ! + + + PURPOSE + + +
      ! This subroutine  performs the initialization of the asd/sieve
      ! variables which include the number of sieves and their sizes,
      ! the geometric mean diameter of each sieve cut and specifies which
      ! lognormal case will be used to represent aggregate size distributions
      ! in WERM/WEPS.

      ! The routine decides which lognormal case to apply based on the
      ! value of logcas:

      ! logcas = 0 --> "normal" lognormal case (mnot = 0, minf = infinity)
      ! logcas = 1 --> "abnormal" lognormal case (mnot != 0, minf = infinity)
      ! logcas = 2 --> "abnormal" lognormal case (mnot = 0, minf != infinity)
      ! logcas = 3 --> "abnormal" lognormal case (mnot != 0, minf != infinity)

      ! + + + KEYWORDS + + +
      ! aggregate size distribution, asd, sieves, mass fractions

      ! + + + LOCAL VARIABLES + + +
      integer :: i  ! loop variable for sieve diameters

      ! + + + END SPECIFICATIONS + + +

      ! NOTE: using this method generates slightly different sieve sizes
      ! between debug and optimized compile switches. (and possibly between
      ! different compilers) To minimize these differences, we should return
      ! to exactly defined sieve sizes

      ! specificiations brought here from BLKDAT (see revision 1.4 comment below)
      !  data logcas / 3 /
      !  data nsieve / 13 /
      !  data sdia / 0.018, 0.037, 0.075, 0.15, 0.42, 0.84, 2.0,
      ! &            6.35, 19.05, 44.45, 76.2, 150.4, 300.8 /
      !  data mnsize, mxsize / 0.009, 601.2 /

      logcas = 3
      nsieve = msieve - 1
      mnsize = 0.005
      mxsize = 1000.0

      do i = 1, nsieve
          sdia(i) = exp(log(mnsize) + i*(log(mxsize)-log(mnsize))/(nsieve+1))
          !write(*,"(a, i0, 1x, ES24.17)") 'ASDINI: ', i, sdia(i)
      end do

      ! compute geometric mean dia. for each sieve cut
      mdia(1) = sqrt(mnsize*sdia(1))
      do i = 2, nsieve
           mdia(i) = sqrt(sdia(i)*sdia(i-1))
      end do
      mdia(nsieve+1) = sqrt(mxsize*sdia(nsieve))

    end subroutine asdini

    ! This subroutine  performs the inverse of subroutine m2asd.
    ! asd2m computes the mass fractions for each sieve cut from the
    ! lognormal representation of the soil aggregate size distribution.
    pure subroutine asd2m (mnot, minf, gmd, gsd, nlay, mf)

      use erf_mod, only: erf1

      real, intent(in) :: mnot(*) ! minimum size aggregate (assumed value is known)
      real, intent(in) :: minf(*) ! maximum size aggregate (assumed value is known)
      real, intent(in) :: gmd(*)  ! geometric mean diameter of aggregate size distribution
                                  ! (or transformed asd for "modified" lognormal cases)
      real, intent(in) :: gsd(*)  ! geometric standard deviation of aggregate size distribution
                                  ! (or transformed asd for "modified" lognormal cases)
      integer, intent(in) :: nlay ! number of soil layers used
      real, dimension(msieve+1,*), intent(out) :: mf ! mass fractions of aggregates within sieve cuts
                                                     ! (sum of all mass fractions are expected to = 1.0)

      double precision :: d(msieve+1) ! transformed sieve dia. values
                                      ! (if "abnormal" lognormal cases)
      double precision :: lngmd ! natural log of gmd
      double precision :: lngsd ! natural log of gsd
      double precision :: prev  ! contain previous sieve dia. cumulative prob
      double precision :: this  ! contain this sieve dia. cumulative prob
      integer :: i  ! loop variable for sieve sizes
      integer :: j  ! loop variable for soil layers

      do j = 1, nlay
          ! compute transformed sieve dia. sizes
          if (logcas .eq. 3) then
              do i = 1, nsieve
                  if(sdia(i).lt.minf(j)) then
                      d(i) = (sdia(i)-mnot(j)) * (minf(j)-mnot(j)) / (minf(j)-sdia(i))
                  end if
              end do
          elseif (logcas .eq. 2) then
              do i = 1, nsieve
                  if(sdia(i).lt.minf(j)) then
                      d(i) = sdia(i)*minf(j)/(minf(j)-sdia(i))
                  end if
              end do
          elseif (logcas .eq. 1) then
              do i = 1, nsieve
                   d(i) = sdia(i)-mnot(j)
              end do
          elseif (logcas .eq. 0) then
              do i = 1, nsieve
                   d(i) = sdia(i)
              end do
          endif
          lngmd = log(dble(gmd(j)))
          lngsd = sqrt(2.0) * log(max(mingsd,dble(gsd(j))))
          prev= 1.0

          ! compute each dia. cumulative probability
          do i = 1, nsieve
              if (sdia(i) .le. mnot(j)) then
                 this = 1.0
              else if (sdia(i) .lt. minf(j)) then
                 this = 0.5 -0.5*erf1(sngl((log(d(i)) - lngmd) / lngsd))
              else
                 this = 0.0
              end if
              ! compute mass fraction between prev and this dia
              mf(i,j) = prev - this
              prev = this
              ! write(*,*) 'asd2m:',i,sdia(i),this,mf(i,j)

              ! if roundoff errors or otherwise results in negative
              ! mass fraction then set to zero mass
              if (mf(i,j) .lt. 0.0) then
                  mf(i,j) = 0.0
              else
                  prev = this
              endif
              ! if(j.eq.4) write(*,*) 'asd2m: mf(',i,j,')',mf(i,j)
          end do

          ! get mass fraction for upper-most sieve cut
          mf(nsieve+1,j) = prev
          ! if( j.eq.1 )write(*,*)'asd2m: mf(',nsieve+1,j,')',mf(nsieve+1,j)

          ! zero out the rest of the array which is used every where else
          do i=nsieve+2, msieve+1
              mf(i,j) = 0.0
          end do

      end do
      return
    end subroutine asd2m

    ! This subroutine  performs the inverse of subroutine asd2m.
    ! m2asd computes the geometric mean & standard deviation for the
    ! lognormal representation of the soil aggregate size distribution
    ! from mf(i,j).
    pure subroutine m2asd (mf, nlay, mnot, minf, gmd, gsd)
      real, dimension(msieve+1,*), intent(in) :: mf ! mass fractions of aggregates within sieve cuts
                                                    ! (sum of all mass fractions are expected to = 1.0)
      integer, intent(in) :: nlay ! number of soil layers used
      real, intent(in) :: mnot(*) ! minimum size aggregate (assumed value is known)
      real, intent(in) :: minf(*) ! maximum size aggregate (assumed value is known)
      real, intent(out) :: gmd(*) ! geometric mean diameter of aggregate size distribution
                                  ! (or transformed asd for "modified" lognormal cases)
      real, intent(out) :: gsd(*) ! geometric standard deviation of aggregate size distribution
                                  ! (or transformed asd for "modified" lognormal cases)

      double precision :: tmd(msieve+1) ! transformed md (later log(tmd))
      double precision :: alpha       ! internal summation variable
      double precision :: beta        ! internal summation variable
      double precision :: mdia_istart ! temp variable, avoid changing mdia array for logcas 1,3
      double precision :: mdia_istop  ! temp variable, avoid changing mdia array for logcas 2,3
      double precision :: sdia_temp   ! temp variable
      integer i      ! loop variable for sieve diameters
      integer j      ! loop variable for soil layers
      integer istart ! loop start variable for sieve diameters
      integer istop  ! loop stop variable for sieve diameters

      ! for each soil layer
      do j = 1, nlay
           ! initialize accumulators
           alpha = 0.0
           beta = 0.0
           istart = 1
           istop = nsieve + 1

           ! check if sieve cut fractions are between mnot and minf
           ! adjust lower and upper mean diameters if mnot or minf
           ! fall within the bin range
           if (logcas .eq. 1 .or. logcas .eq. 3) then
              do i=nsieve, 1, -1
                 if (sdia(i) .gt. mnot(j)) then
                    istart = i
                 end if
              end do
              ! save value to be restored before exit
              ! mdia_istart = mdia(istart)
              ! set size of lower sieve in bottom bin
              if( istart.eq.1 ) then
                  sdia_temp = mnsize
              else
                  sdia_temp = sdia(istart-1)
              end if
              ! check if mnot falls within lower sieve bin
              if( (mnot(j).gt.sdia_temp).or.(mnot(j).lt.mnsize) ) then
                  ! recalculate lower bin mean diameter
                  mdia_istart = sqrt(sdia(istart)*mnot(j))
                  if (logcas .eq. 3) then
                      ! check that mdia is greater than mnot, or method fails
                      if( mdia_istart .lt. mnot(j) * 1.00001 ) then
                          mdia_istart = mnot(j) * 1.00001
                      end if
                  end if
              end if
           endif
           if (logcas .ge. 2) then
              do i=1, nsieve
                 if (sdia(i) .le. minf(j)) then
                    istop = i+1
                 end if
              end do
              ! set size of upper sieve in top bin
              if( istop.eq.nsieve+1 ) then
                  sdia_temp = mxsize
              else
                  sdia_temp = sdia(istop)
              end if
              ! save value to be restored before exit
              ! mdia_istop = mdia(istop)
              ! check if minf falls within upper sieve bin or outside mxsize
              if( (minf(j).lt.sdia_temp).or.(minf(j).gt.mxsize) ) then
                  ! recalculate upper bin mean diameter
                  mdia_istop = sqrt(sdia(istop-1)*minf(j))
                  if (logcas .ge. 2) then
                      ! check that mdia is less than minf, or method fails
                      if( mdia_istop .gt. minf(j) * 0.99999 ) then
                          mdia_istop = minf(j) * 0.99999
                      end if
                  end if
              end if
           else
              istop = nsieve + 1
           end if

           ! do transformations for "modified" log-normal cases
           do i= istart, istop
              if (logcas .eq. 3) then
                 if( i .eq. istart) then
                    tmd(i) = (mdia_istart-mnot(j)) * (minf(j)-mnot(j)) / (minf(j)-mdia_istart)
                 else if( i .eq. istop ) then
                    tmd(i) = (mdia_istop-mnot(j)) * (minf(j)-mnot(j)) / (minf(j)-mdia_istop)
                 else
                    tmd(i) = (mdia(i)-mnot(j)) * (minf(j)-mnot(j)) / (minf(j)-mdia(i))
                 end if
              else if (logcas .eq. 2) then
                 if( i .eq. istop ) then
                    tmd(i) = mdia_istop * minf(j) / (minf(j)-mdia_istop)
                 else
                    tmd(i) = mdia(i) * minf(j) / (minf(j)-mdia(i))
                 end if
              else if (logcas .eq. 1) then
                 if( i .eq. istart) then
                    tmd(i) = mdia_istart - mnot(j)
                 else
                    tmd(i) = mdia(i) - mnot(j)
                 end if
              else
                 tmd(i) = mdia(i)
              end if

              ! now compute the log of the gmd dia
              tmd(i) = log(tmd(i))

              ! sum diameters  & their squares, over all aggregate sizes
              alpha = alpha + (mf(i,j)*tmd(i))
              beta = beta + (mf(i,j)*tmd(i)*tmd(i))
           end do

           ! compute geometric mean and standard deviation
           gmd(j) = exp(alpha)
           if( beta-alpha*alpha.le.0.0 ) then
               gsd(j) = mingsd
           else 
               gsd(j) = max(mingsd,exp(sqrt(beta-alpha*alpha)))
           end if

           ! restore modified geometric mean bin diameters
           ! if (logcas .eq. 1 .or. logcas .eq. 3) then
           !   mdia(istart) = mdia_istart
           ! end if
           ! if (logcas .ge. 2) then
           !   mdia(istop) = mdia_istop
           ! end if

      end do
      return
    end subroutine m2asd

end module asd_mod
