!$Author$
!$Date$
!$Revision$
!$HeadURL$
!**********************************************************************
      subroutine erodinit
!
!     +++ PURPOSE +++
!
!     Controls calls to subroutines that:
!       create the Erosion submodel grid (sbgrid)    
!       initialize Erosion submodel output array to zero (sbigrd).
!       calculate normalized effect of hills on friction velocity 
!        on grid for each wind direction (not activated)
!       calculate normalized effect of barriers on friction velocity
!        on grid for each wind direction (sbbr)
!       initialize reporting variables that need to have a value even
!        when erosion is not being called.
!     + + + GLOBAL COMMON BLOCKS + + +
!

      include  'p1werm.inc'
      include  'm1flag.inc'
      include  'm1geo.inc'
      include  'm1subr.inc'
      include  'erosion/m2geo.inc'
      include  'erosion/e2grid.inc'  !needed for initialization of csr(*,*)
      include  'erosion/threshold.inc'
      include  's1surf.inc'
      include  'subglobe.inc'
!     +++ SUBROUTINES CALLED +++
!     sbgrid
!     sbigrd
!     sbhill (not activated)
!     sbbr

!     +++ LOCAL VARIABLES +++

      integer i, j, sr

!     + + + LOCAL VARIABLE DEFINITIONS + + +
!     nbr  = number of barriers (from m1geo.inc)

!     +++ END SPECIFICATIONS +++
! Grid is created at least once.
      if (am0eif .eqv. .true.) then
         ! check to see if grid dimensions specified via cmdline args
         if ((xgdpt > 0) .and. (ygdpt > 0)) then
           imax = xgdpt + 1
           jmax = ygdpt + 1
           ix = (amxsim(1,2) - amxsim(1,1)) / xgdpt
           jy = (amxsim(2,2) - amxsim(2,1)) / ygdpt
! Calculating the dimension for each subregion in X and Y direction by JG
        do  sr = 1, nsubr
          imax_sub(sr) = (amxsr(1,2,sr) - amxsr(1,1,sr)) / xgdpt
          jmax_sub(sr) = (amxsr(2,1,sr) - amxsr(2,2,sr)) / ygdpt
       
          do j = 0, jmax
              do i = 0, imax
!
!     for multiple subregions
!     assigning the subregion ID to CSR for each subgrids by JG      

         if (i*ix .gt. amxsr(1,1,sr) .and. i*ix .lt. amxsr(1,2,sr)      &
     & .and. j*jy .gt. amxsr(2,1,sr).and.j*jy.lt.amxsr(2,2,sr)) then
            csr(i,j) = sr
          end if
              end do
           end do          
        end do
   !code lifted from sbgrid because it is initialized there - LEW
   !        do j = 0, jmax
   !          do i = 0, imax
   !            csr(i,j) = 1          ! icsr = 1
   !          end do 
   !        end do
         else          !use Hagen's grid dimensioning as the default
           call sbgrid
         endif

         ! set grid cell output arrays to zero
         call sbigrd

         ! check for hills - sbhill not implemented
!        if (nhill .gt. 0) then
!        call sbhill
!        endif

         ! check for barriers
         if (nbr .gt. 0) then
         call sbbr
         endif

         ! Turn off grid creation flag
         am0eif = .false.
      endif

      do sr = 1, nsubr
           ! initalize erosion threshold trigger variables
           ne_erosion(sr) = 0
           ne_snowdepth(sr) = 0

           ne_wus_anemom(sr) = 0
           ne_wus_random(sr) = 0
           ne_wus_ridge(sr) = 0
           ne_wus_biodrag(sr) = 0
           ne_wus(sr) = 0

           ne_bare(sr) = 0
           ne_flat_cov(sr) = 0
           ne_surf_wet(sr) = 0
           ne_ag_den(sr) = 0
           ne_wust(sr) = 0

           ne_sfd84(sr) = 0
           ne_asvroc(sr) = 0
           ne_wzzo(sr) = 0
           ne_sfcv(sr) = 0

           ! initialize surface condition reporting values
           acanag(sr) = 0
           acancr(sr) = 0
      end do

      return
      end
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

