!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine sci_cum( isr, restot, cellstate )

      use weps_main_mod, only: soil_cond
      use biomaterial, only: biototal
      use erosion_data_struct_defs, only: cellsurfacestate
      use grid_mod, only: imax, jmax
      use sci_report_mod, only: scisum

!     + + + ARGUMENT DECLARATIONS + + +
      integer, intent(in) :: isr
      type(biototal), intent(in) :: restot
      type(cellsurfacestate), dimension(0:,0:), intent(in) :: cellstate     ! initialized grid cell state values

!     + + + ARGUMENT DEFINITIONS + + +
!     isr - subregion index
!     restot - structure containing residue totals

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
              if( isr .eq. cellstate(idx,jdy)%csr ) then
                  total = total + cellstate(idx,jdy)%egt
                  ngdpt = ngdpt + 1
              end if
           end do
      end do
      if( ngdpt .gt. 0 ) then
          total = total/ngdpt
      end if

      ! scisum(isr)%allbiomass = scisum(isr)%allbiomass + admtotto4(isr)
      scisum(isr)%allbiomass = scisum(isr)%allbiomass + restot%mftot    &
     &      + restot%msttot + restot%mbgtot + restot%mrttotto4

      scisum(isr)%allerosion = scisum(isr)%allerosion + total
      scisum(isr)%days = scisum(isr)%days + 1

      return
      end
