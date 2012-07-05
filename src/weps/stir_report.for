!$Author$
!$Date$
!$Revision$
!$HeadURL$
      subroutine stir_report(isr, end_of_file, ostir, oenergyarea)

!     + + + MODULES + + +
      use stir_soil_texture, only : get_stir_soil_multiplier
      
!     + + + ARGUMENT DECLARATIONS + + +
      integer isr
      logical end_of_file
      real ostir, oenergyarea

!     + + + ARGUMENT DEFINITIONS + + +
!     isr - subregion index
!     end_of_file - flag indicating reaching end of managment file
!     ostir - STIR value assigned as operation level parameter
!     oenergyarea - 

!     + + + INCLUDE + + +
      include 'p1werm.inc'
      include 'command.inc'
      include 'file.inc'
      include 'm1flag.inc'
      include 'main/sci_report_val.inc'
      include 'main/stir_report_val.inc'

!     + + + PURPOSE + + +
!     each time it is called, it calculates the Soil Tillage Intensity Rating
!     for the current operation and adds it to the total.

!     + + + LOCAL VARIABLES + + +
      real stir_op_avg, local_op_energy
      integer idx, jdx, kdx

!     + + + LOCAL DEFINITIONS + + +
!     stir_op_avg - average over all burial processes of stir value
!     idx         - index used to serach for crop_num

      ! only do if flag is set, before done flag set
      if( (soil_cond .eq. 0) .or. done_flg(isr) ) return

      if( end_of_file .and. ( .not. report_loop ) ) then
          ! used in stir_crop, which requires at least two passes to work
          ! reset the plant harvest array index for second pass
          phopidx(isr) = 0
      end if
 
      ! check for first time at end of managment file
      if( (.not. man_eof(isr)) .and. end_of_file ) then
          man_eof(isr) = end_of_file
      end if

      ! only go through stir calculation once (see done flag above)
      ! make sure to do on second time through and only in report loop
      if( (.not. man_eof(isr)) .or. (.not. report_loop) ) return

      ! in report loop, start accumulating
      if( proc_cnt(isr) .gt. 0 ) then
          stir_op_avg = stir_op_sum(isr)/proc_cnt(isr)
      else
          stir_op_avg = 0.0
      end if

      if( ostir .ge. 0.0 ) then
          stir_sum(isr) = stir_sum(isr) + ostir
          ! set stir and energy value for each operation
          phop_stir(phopidx(isr),isr) = ostir
      else
          stir_sum(isr) = stir_sum(isr) + stir_op_avg
          phop_stir(phopidx(isr),isr) = stir_op_avg
      end if

      phop_energy(phopidx(isr),isr) = oenergyarea
      if( oenergyarea .ge. 0.0 ) then
          energy_sum(isr) = energy_sum(isr) + oenergyarea
      end if

      ! reset values for next operation
      stir_op_sum(isr) = 0
      oper_cnt(isr) = 0
      proc_cnt(isr) = 0

      ! check for second time at end of managment file
      if( man_eof(isr) .and. end_of_file ) then
         ! set so only one STIR report produced
         done_flg(isr) = .true.
         ! create and print STIR report (2nd time through complete, info complete)
         do idx = 1, phopcnt(isr)
          ! assign crop number to non planting or harvest operations
           if( phop_type(idx,isr) .eq. 0 ) then
             ! assign crop number from next planting or harvest operation
             do jdx = idx, phopcnt(isr)
                ! index ahead in file to end
                if( phop_type(jdx,isr) .gt. 0 ) then
                   ! planting or harvest operation, use assigned crop number
                   crop_num(idx,isr) = crop_num(jdx,isr)
                   ! we don't need any more
                   exit
                else if( jdx .eq. phopcnt(isr) ) then
                   ! didn't find number, continue from start of file
                   do kdx = 1, idx-1
                      if( phop_type(kdx,isr) .gt. 0 ) then
                         ! planting or harvest operation, use assigned crop number
                         crop_num(idx,isr) = crop_num(kdx,isr)
                         ! we don't need any more
                         exit
                      end if
                   end do
                end if
             end do
           end if
           
           !multiple the energy by the soil multiplier
           local_op_energy = phop_energy(idx,isr) *                      &
     &                                   get_stir_soil_multiplier (isr)
           
           ! print this line
           write(luostir,1000) phopday(idx,isr), phopmon(idx,isr),      &
     &                         phopyr(idx,isr),                         &
     &                         trim(stir_opname(idx,isr)),              &
     &                         trim(stir_cropname(idx,isr)),            &
     &                         trim(stir_fuelname(idx,isr)),            &
     &                         phop_stir(idx,isr), local_op_energy,      &
     &                         crop_num(idx,isr), last_harv(idx,isr)
         end do
         return
      end if

1000  format (i2,'/',i2,'/',i4,3(' | ',a),2(' | ',f8.2),2(' | ',i1) )

      return
      end
