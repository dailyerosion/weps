!$Author$
!$Date$
!$Revision$
!$HeadURL$
      subroutine sci_init(isr)

!     + + + ARGUMENT VARIABLES + + +
      integer isr

!     + + + ARGUMENT DEFINITIONS + + +
!     isr - subregion index

!     + + + INCLUDE + + +
      include 'p1werm.inc'
      include 'command.inc'
      include 'file.inc'
      include 'main/sci_report_val.inc'
      include 'main/stir_report_val.inc'
      include 'manage/man.inc'

!     + + + PURPOSE + + +
!     each time it is called, it adds a value to the total biomass increments
!     the counter for number of values added together.

!     + + + LOCAL VARIABLES + + +
      integer idx
      logical, save :: header_not_printed = .TRUE.

      ! only do if flag is set 
      if( soil_cond .eq. 0 ) return

      ! initialize sci accumulator values
      allbiomass_sum(isr) = 0.0
      allerosion_sum(isr) = 0.0
      days_sum(isr) = 0
      stir_sum(isr) = 0.0
      energy_sum(isr) = 0.0

      ! initialize stir accumulator values
      oper_cnt(isr) = 0
      proc_cnt(isr) = 0
      done_flg(isr) = .false.
      man_eof(isr) = .false.
      stir_op_sum(isr) = 0.0
      stir_op_energy(isr) = 0.0

      ! initialize counters and arrays for planting and harvest operation tracking
      phopcnt(isr) = 0
      phopidx(isr) = 0
      do idx = 1, mxphops
          stir_opname(idx, isr) = ''
          stir_cropname(idx, isr) = ''
          stir_fuelname(idx, isr) = ''
          phop_type(idx, isr) = 0
          phop_stir(idx, isr) = 0.0
          phop_energy(idx, isr) = 0.0
          crop_num(idx, isr) = 0
          last_harv(idx, isr) = 0
      end do 

      if (header_not_printed) then
          ! write header to stir_energy.out file
          write(luostir, '(4A)') '#dd/mm/yyyy | operation name',        &
     &       ' | crop name (optional) | fuel | stir',                   &
     &       ' | energy (L diesel/ha)',                                 &
     &       ' | crop sequence number | 1 if last harvest of crop'
          ! write number of years in management rotation
          write(luostir,'(i4,(A))') mperod(1),                          &
     &              '  Number of years in WEPS management rotation file'
         header_not_printed = .FALSE.
      end if

      return
      end
