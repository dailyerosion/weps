!$Author$
!$Date$
!$Revision$
!$HeadURL$
       subroutine freezeharden(bcthardnx, day_max_temp, day_min_temp)

!     + + + PURPOSE + + +
!     calculates the freeze hardening index for the day. The input value
!     is modified to reflect the effect of temperature on either increasing
!     or decreasing the index. Stage 1 hardening occurs when the plant
!     experiences cool temperatures from -1 to 8 degrees C. Stage 2 hardening
!     occurs only after stage 1 is complete and temperatures fall below
!     freezing.

!     method taken from: Ritchie, J.T. 1991. Wheat Phasic development in: 
!     Hanks, J. and Ritchie, J.T. eds. Modeling plant and soil systems.
!     Agronomy Monograph 31, pages 40-42, 52

!     + + + KEYWORDS + + +
!     Freeze hardening index

!     + + + ARGUMENT DECLARATIONS + + +
      real bcthardnx, day_max_temp, day_min_temp

!     note: input crown temperature rather than air temperature for best results

!     + + + ARGUMENT DEFINITIONS + + +
!     bcthardnx - hardening index for winter annuals (range from 0 t0 2)
!     day_max_temp - daily maximum temperature (deg.C)
!     day_min_temp - daily minimum temperature (deg.C)

!     + + + LOCAL VARIABLES + + +
      real tavg, hinc
      real t1min, t1opt, t1max, t2max, tbase, tdeh
      real hs1, hs2, deht, hardinc1, hardinc2

      parameter(t1min = -1.0)
      parameter(t1opt = 3.5)
      parameter(t1max = 8.0)
      parameter(t2max = 0.0)
      parameter(tbase = 0.0)
      parameter(tdeh = 10.0)
      parameter(hs1 = 1.0)
      parameter(hs2 = 2.0)
      parameter(deht = 0.02)
      parameter(hardinc1 = 0.1)
      parameter(hardinc2 = 0.083)

!     + + + LOCAL VARIABLE DEFINITIONS + + +
!     tavg     - daily everage temperature  (deg.C)
!     t1min     - minimum temperature in stage 1 index calculation(deg.C)
!     t1opt     - optimum temperature in stage 1 index calculation(deg.C)
!     t1max     - maximum temperature in stage 1 index calculation(deg.C)
!     t2max     - maximum temperature in stage 2 index calculation(deg.C)
!     tbase    - base temperature for hardening effects(deg.C) (like base growth temperature)
!     tdeh     - temperature above which dehardening can occur (deg.C)
!     hs1      - index value at completion of stage 1 hardening
!     hs2      - index value at completion of stage 2 hardening
!     deht     - index reduction multiplier for dehardening temperature excess
!     hardinc2 - stage 2 hardening index increment

      ! find average temperature
      tavg = 0.5 * (day_max_temp + day_min_temp)

      if( bcthardnx .ge. hs1 ) then
          ! stage 1 complete, into stage 2
          if( tavg .le. tbase + t2max ) then
              ! add stage 2 amount to index
              bcthardnx = bcthardnx + hardinc2
          end if
          if( day_max_temp .ge. tbase + tdeh ) then
              ! stage 2 dehardening
              hinc = deht * (tbase + tdeh - day_max_temp)
              bcthardnx = bcthardnx + hinc
              if( bcthardnx .ge. hs1 ) then
                  ! still in stage 2, take off some more
                  bcthardnx = bcthardnx + hinc
              end if
          end if
          bcthardnx = max( bcthardnx, 0.0)
          bcthardnx = min( bcthardnx, hs2)

      else if( tavg .ge. tbase + t1min) then
          ! stage 1 hardening
          if( tavg .le. tbase + t1max ) then
              ! add stage 1 amount to index, minus deduction for being on either side of optimum
              bcthardnx = bcthardnx + hardinc1                          &
     &                  - ((tavg - (tbase + t1opt))**2)/506.
              if( bcthardnx .ge. hs1 ) then
                  ! stage 1 complete, into stage 2
                  if( tavg .le. tbase + t2max ) then
                      ! add stage 2 amount to index
                      bcthardnx = bcthardnx + hardinc2
                  end if
              end if
          end if
          if( day_max_temp .ge. tbase + tdeh ) then
              ! stage 1 dehardening
              hinc = deht * (tbase + tdeh - day_max_temp)
              bcthardnx = bcthardnx + hinc
              if( bcthardnx .ge. hs1 ) then
                  ! really in stage 2, take off some more
                  bcthardnx = bcthardnx + hinc
              end if
          end if
          bcthardnx = max( bcthardnx, 0.0)
          bcthardnx = min( bcthardnx, hs2)

      end if
      
      return
      end