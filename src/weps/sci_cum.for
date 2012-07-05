!$Author$
!$Date$
!$Revision$
!$HeadURL$
      subroutine sci_cum( isr )

!     + + + ARGUMENT DECLARATIONS + + +
      integer isr

!     + + + ARGUMENT DEFINITIONS + + +
!     isr - subregion index

!     + + + INCLUDE + + +
      include 'p1werm.inc'
      include 'd1glob.inc'
      include 'command.inc'
      include 'erosion/e2erod.inc'
      include 'erosion/e2grid.inc'
      include 'erosion/m2geo.inc'
      include 'main/sci_report_val.inc'

!     + + + PURPOSE + + +
!     each time it is called, it adds a value to the total biomass increments
!     the counter for number of values added together.

!     + + + LOCAL VARIABLES + + +
      real total
      integer idx, jdy, ngdpt

      ! only do if flag is set 
      if( soil_cond .eq. 0 ) return

      ! initialize erosion totals
      total = 0.0
      ngdpt = 0
      do idx = 1, imax-1
          do jdy = 1, jmax-1
              if( csr(idx,jdy) .eq. isr ) then
                  total = total + egt(idx,jdy)
                  ngdpt = ngdpt + 1
              end if
           end do
      end do
      if( ngdpt .gt. 0 ) then
          total = total/ngdpt
      end if

      ! allbiomass_sum(isr) = allbiomass_sum(isr) + admtotto4(isr)
      allbiomass_sum(isr) = allbiomass_sum(isr) + admftot(isr)          &
     &      + admsttot(isr) + admbgtot(isr) + admrttotto4(isr)

      allerosion_sum(isr) = allerosion_sum(isr) + total
      days_sum(isr) = days_sum(isr) + 1

      return
      end