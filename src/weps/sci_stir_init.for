!$Author$
!$Date$
!$Revision$
!$HeadURL$
      subroutine sci_stir_init(isr)

      use file_io_mod, only: luostir
      use sci_report_mod, only: scisum
      use stir_report_mod, only: stircum
      use manage_data_struct_defs, only: manFile

!     + + + ARGUMENT VARIABLES + + +
      integer isr

!     + + + ARGUMENT DEFINITIONS + + +
!     isr - subregion index

!     + + + INCLUDE + + +
      include 'p1werm.inc'
      include 'command.inc'

!     + + + PURPOSE + + +
!     each time it is called, it adds a value to the total biomass increments
!     the counter for number of values added together.

!     + + + LOCAL VARIABLES + + +
      integer idx

      ! only do if flag is set 
      if( soil_cond .eq. 0 ) return

      ! initialize sci accumulator values
      scisum(isr)%allbiomass = 0.0
      scisum(isr)%allerosion = 0.0
      scisum(isr)%days = 0
      scisum(isr)%stir = 0.0
      scisum(isr)%energy = 0.0

      ! initialize stir accumulator values
      stircum(isr)%oper_cnt = 0
      stircum(isr)%proc_cnt = 0
      stircum(isr)%stir_op_sum = 0.0
      stircum(isr)%stir_op_energy = 0.0

      ! initialize counters and arrays for planting and harvest operation tracking
      stircum(isr)%phopcnt = 0
      stircum(isr)%phopidx = 0
      do idx = 1, size(stircum(isr)%phop)
          stircum(isr)%phop(idx)%stir_opname = ''
          stircum(isr)%phop(idx)%stir_cropname = ''
          stircum(isr)%phop(idx)%stir_fuelname = ''
          stircum(isr)%phop(idx)%phop_type = 0
          stircum(isr)%phop(idx)%phop_stir = 0.0
          stircum(isr)%phop(idx)%phop_energy = 0.0
          stircum(isr)%phop(idx)%crop_num = 0
          stircum(isr)%phop(idx)%last_harv = 0
      end do 

      if (stircum(isr)%header_not_printed) then
          ! write header to stir_energy.out file
          write(luostir(isr), '(5A)') '#dd/mm/yyyy | operation name',   &
     &       ' | crop name (optional) | fuel | stir',                   &
     &       ' | energy (L diesel/ha)',                                 &
     &       ' | crop sequence number',                                 &
     &       ' | 1 if last harvest/termination of crop'
          ! write number of years in management rotation
          write(luostir(isr),'(i4,(A))') manFile(isr)%mperod,           &
     &              '  Number of years in WEPS management rotation file'
         stircum(isr)%header_not_printed = .FALSE.
      end if

      return
      end
