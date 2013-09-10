!$Author$
!$Date$
!$Revision$
!$HeadURL$
      subroutine stir_oper(isr)

      use weps_interface_defs
      use stir_report_mod, only: stircum, stir_report
      use manage_data_struct_defs, only: lastoper

!     + + + ARGUMENT DECLARATIONS + + +
      integer isr

!     + + + ARGUMENT DEFINITIONS + + +
!     isr - subregion index

!     + + + INCLUDE + + +
      include 'p1werm.inc'
      include 'command.inc'
      include 'm1flag.inc'

!     + + + PURPOSE + + +
!     each time it is called, it calculates the Soil Tillage Intensity Rating
!     for the current operation and adds it to the total.

!     + + + LOCAL VARIABLES + + +

      ! only do if flag is set 
      if( (soil_cond .eq. 0) .or. stircum(isr)%done_flg ) return

      ! increment index for planting harvest accounting
      stircum(isr)%phopidx = stircum(isr)%phopidx + 1
      ! check for total number, make maximum
      stircum(isr)%phopcnt = max( stircum(isr)%phopcnt, stircum(isr)%phopidx )

      if( report_loop .neqv. .true. ) return

      stircum(isr)%oper_cnt = stircum(isr)%oper_cnt + 1
      if( stircum(isr)%oper_cnt .gt. 1 ) then
          ! indicates multiple operations on the same day
          ! calculate stir and reset accumulators for this next operation
          call stir_report(isr, .false., lastoper(isr)%stir, lastoper(isr)%energyarea)
      end if

      stircum(isr)%phop(stircum(isr)%phopidx)%phopday = lastoper(isr)%day
      stircum(isr)%phop(stircum(isr)%phopidx)%phopmon = lastoper(isr)%mon
      stircum(isr)%phop(stircum(isr)%phopidx)%phopyr = lastoper(isr)%yr
      stircum(isr)%phop(stircum(isr)%phopidx)%stir_opname = lastoper(isr)%name
      stircum(isr)%phop(stircum(isr)%phopidx)%stir_fuelname = lastoper(isr)%fuel

      return
      end
