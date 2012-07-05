!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine arraymerge( nr, dt, trf, rf, irrig, durirr,            &
     &                       nf, tr, r, rr)

!     + + + PURPOSE + + +
!     merges rainfall, infiltration step and irrigation arrays

!     + + + ARGUMENT DECLARATIONS + + +
      integer, intent(in) :: nr
      real, intent(in) :: dt, trf(*), rf(*), irrig, durirr
      integer, intent(inout) :: nf
      real, intent(inout) :: tr(*), r(*), rr(*)

!     + + + ARGUMENT DEFINITIONS + + +
!     nr - number of points in the rainfall breakpoint representation
!     dt - infiltration array time step
!     trf - time values in the rainfall breakpoint representation (T)
!     rf - rate values in the rainfall breakpoint representation (L/T)
!     irrig - daily irrigation depth (L)
!     durirr - daily irrigation duration (T)
!     nf - number of values in the infiltration array
!     tr - time value in infiltration input array (T)
!     r - depth value in infiltration input array (rainfall) (L)
!     rr - cumulative depth for infiltration array (L)

!     + + + PARAMETERS + + +
      integer mxtime
      parameter (mxtime = 1000)

!     + + + PARAMETER DEFINITIONS + + +

!     + + + LOCAL VARIABLES + + +
      integer idx, jdx, ip, nri
      real*8 xx, test
      real dtc, rateirr, trfi(mxtime), rfi(mxtime)

!     + + + LOCAL DEFINITIONS + + +
!     idx - loop variable
!     jdx - loop variable
!     xx - cumulative sum value
!     test - intermediate value
!     dtc - cumulative time for infiltration (T)
!     rateirr - irrigation rate (L/T) (constant for irrigation duration)
!     trfi - time values in rainfall array with irrigation rate and duration added
!     rfi - rate values in rainfall array with irrigation rate and duration added

!     + + + DATA INITIALIZATIONS + + +

!     + + + END SPECIFICATIONS + + +

      if( (irrig .gt. 0.0) .and. (durirr .gt. 0.0) ) then
         ! compute irrigation rate
         rateirr = irrig / durirr
         ! add point for time irrigation ends
         ! find insertion point starting at end
         ip = 0
         do jdx = nr, 1, -1
            if (durirr.ge.trf(jdx)) then
               ! insertion point found
               ip = jdx
               test = abs(durirr-trf(jdx))
               if (test.gt.0.015d0) then
                  ! irrigation termination is different from this rainfall time
                  ! new time will be added
                  nri = nr + 1
               else
                  ! irrigation termination time is same as rainfall breakpoint
                  ! no insertion done, breakpoint used
                  nri = nr
               end if
               exit
            end if
         end do
      else
         nri = nr
         ip = 0
      end if

      ! add irrigation rate to rainfall rate.
      idx = 0
      do jdx = 1, nr
         if( ip .gt. jdx ) then
            ! irrigation being applied
            trfi(jdx) = trf(jdx)
            rfi(jdx) = rf(jdx) + rateirr
         else if( ip .eq. jdx ) then
            if( nri .gt. nr ) then
               ! point inserted
               idx = jdx + 1
               trfi(jdx) = trf(jdx)
               rfi(jdx) = rf(jdx)+ rateirr
               ! set irrigation termination breakpoint
               trfi(idx) = durirr
               rfi(idx) = rf(jdx)
            else
               ! no point inserted
               idx = jdx
               trfi(idx) = trf(jdx)
               rfi(idx) = rf(jdx)
            end if
         else ! ip .lt. jdx
            ! no irrigation being applied
            idx = idx + 1
            trfi(idx) = trf(jdx)
            rfi(idx) = rf(jdx)
         end if
      end do
      if( ip .eq. nri ) then
         trfi(nri) = durirr
         rfi(nri) = 0.0
      end if

      ! remove any zero entries in the beginning of the array

      ! search up the array for multiple zero time entries
      idx = nri
      do jdx = 2, idx
         if( trfi(jdx) .le. 0.0 ) then
             nri = nri - 1
         end if
      end do
      ! remove zero entries by shifting the array down
      ! set idx to number of values to be removed
      idx = idx - nri
      do jdx = 1, nri
         trfi(jdx) = trfi(jdx+idx)
         rfi(jdx) = rfi(jdx+idx)
      end do

      ! using modified rainfall array, merge with infiltration timestep array
      xx = 0.d0
      idx = 2
      dtc = dt
      tr(1) = trfi(1)
      r(1) = rfi(1)
      rr(1) = 0.0
     
      do jdx = 2, nri
  110    test = abs(dtc-trfi(jdx))
         if (idx.gt.2) then
            xx = xx + r(idx-2) * (tr(idx-1)-tr(idx-2))
            rr(idx-1) = xx
         end if
         if (test.gt..015d0) then
           
            if (dtc.lt.trfi(jdx)) then
               r(idx) = rfi(jdx-1)
               tr(idx) = dtc
               dtc = dtc + dt
               idx = idx + 1
               go to 110
            else
               tr(idx) = trfi(jdx)
               r(idx) = rfi(jdx)
               idx = idx + 1
            end if
        
         else
            tr(idx) = trfi(jdx)
            r(idx) = rfi(jdx)
            idx = idx + 1
            dtc = dtc + dt
         end if
      end do

      nf = idx - 1
      if( nf .lt. 3 ) then
         do jdx = 1, nf
            rr(nf) = 0.0
         end do
      else
         rr(nf) = rr(nf-1) + r(nf-1) * (tr(nf)-tr(nf-1))
      end if

      return
      end