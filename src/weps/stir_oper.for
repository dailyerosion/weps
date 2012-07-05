!$Author$
!$Date$
!$Revision$
!$HeadURL$
      subroutine stir_oper(isr)

!     + + + ARGUMENT DECLARATIONS + + +
      integer isr

!     + + + ARGUMENT DEFINITIONS + + +
!     isr - subregion index

!     + + + INCLUDE + + +
      include 'p1werm.inc'
      include 'command.inc'
      include 'm1flag.inc'
      include 'main/stir_report_val.inc'
      include 'main/main.inc' ! lopday, lopmon, lopyr
      include 'manage/oper.inc' ! opname, ostir, oenergyarea, ofuel

!     + + + PURPOSE + + +
!     each time it is called, it calculates the Soil Tillage Intensity Rating
!     for the current operation and adds it to the total.

!     + + + LOCAL VARIABLES + + +

      ! only do if flag is set 
      if( (soil_cond .eq. 0) .or. done_flg(isr) ) return

      ! increment index for planting harvest accounting
      phopidx(isr) = phopidx(isr) + 1
      ! check for total number, make maximum
      phopcnt(isr) = max( phopcnt(isr), phopidx(isr) )

      if( report_loop .neqv. .true. ) return

      oper_cnt(isr) = oper_cnt(isr) + 1
      if( oper_cnt(isr) .gt. 1 ) then
          ! indicates multiple operations on the same day
          ! calculate stir and reset accumulators for this next operation
          call stir_report(isr, .false., ostir, oenergyarea)
      end if

      phopday(phopidx(isr), isr) = lopday
      phopmon(phopidx(isr), isr) = lopmon
      phopyr(phopidx(isr), isr) = lopyr
      stir_opname(phopidx(isr), isr) = opname
      stir_fuelname(phopidx(isr), isr) = ofuel

      return
      end
