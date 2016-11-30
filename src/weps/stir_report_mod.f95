!$Author$
!$Date$
!$Revision$
!$HeadURL$

module stir_report_mod

   type stir_operation_vars
      integer phopday            ! day of the operation
      integer phopmon            ! month of the operation
      integer phopyr             ! year of the operation
      character*80 stir_opname   ! operation name from operation input
      character*80 stir_cropname ! crop name associated with this operation if a harvest
      character*80 stir_fuelname ! fuel name associated with this operation, may be blank to indicate use of the default fuel as defined by the interface
      integer phop_skip          ! skip operation flag, 0 = do every rotation, 1 = skip all but first instance (also skippped in the stir report)
      integer phop_type          ! operation type flag, 0 = not yet initialized, 1 = planting operation, 2 = harvest operation
      real phop_stir             ! STIR value for that operation
      real phop_energy           ! energy value for that operation
      integer crop_num           ! number of crop in the crop rotation cycle, 0 = not yet initialized, 1-n = number of crop
      integer last_harv          ! is this the last harvest in a crop rotation cycle, 0 = not the last harvest, 1 = this is the last harvest (ie. end of the cycle)
   end type stir_operation_vars

   type stir_accumulators
      logical header_not_printed  ! flag to keep from printing multiple headers to stir report file
      integer oper_cnt       ! stir count of operations, used to trigger accumulation
      integer proc_cnt       ! stir count of processes, used in averaging
      logical done_flg       ! flag to indicate that stir had completed second pass through management file
      logical man_eof        ! flag to indicate that man file has reached end once
      real    stir_op_sum    ! stir_op_sum - accumulated stir values, divide by proc count to get average
      real    stir_op_energy ! operation energy value from operation input
      integer phopcnt        ! actual number of (p)lanting and (h)arvest (op)erations tabulated
      integer phopidx        ! operation index, location in the array
      integer phoplastidx    ! operation index of previous call to stir_crop
      type(stir_operation_vars), dimension(:), allocatable :: phop ! individual values for each planting or harvest operation
   end type stir_accumulators

   type(stir_accumulators), dimension(:), allocatable :: stircum

 contains

    subroutine create_stir_accumulator(nsubr, mxphops)
        integer, intent(in) :: nsubr
        integer, intent(in) :: mxphops  ! maximum number of planting and harvest operations that can be tracked with these arrays

        ! local variable
        integer :: alloc_stat  ! allocation status return
        integer :: sum_stat    ! accumulates allocation status results so only one write/exit statement needed
        integer :: isr         ! variable to loop through subregions

        sum_stat = 0
        ! allocate main array for subregion
        allocate(stircum(nsubr), stat=alloc_stat)
        sum_stat = sum_stat + alloc_stat
        ! for each subregion, allocate operation arrays
        do isr = 1, nsubr
           allocate(stircum(isr)%phop(mxphops), stat=alloc_stat)
           sum_stat = sum_stat + alloc_stat
        end do

        if( sum_stat .gt. 0 ) then
           write(*,*) 'ERROR: unable to allocate memory for stir accumulators'
        else
           do isr = 1, nsubr
              stircum(isr)%header_not_printed = .TRUE.
           end do
        end if
    end subroutine create_stir_accumulator

    subroutine destroy_stir_accumulator(nsubr)
        integer, intent(in) :: nsubr

        ! local variable
        integer :: dealloc_stat  ! allocation status return
        integer :: sum_stat    ! accumulates allocation status results so only one write/exit statement needed
        integer :: isr         ! variable to loop through subregions

        sum_stat = 0
        ! for each subregion, deallocate operation arrays
        do isr = 1, nsubr
           deallocate(stircum(isr)%phop, stat=dealloc_stat)
           sum_stat = sum_stat + dealloc_stat
        end do
        ! deallocate main array
        deallocate(stircum, stat=dealloc_stat)
        sum_stat = sum_stat + dealloc_stat

        if( sum_stat .gt. 0 ) then
           write(*,*) 'ERROR: unable to deallocate memory for stir accumulators'
        end if
    end subroutine destroy_stir_accumulator

    subroutine stir_report(isr, end_of_file, ostir, oenergyarea)

!     + + + MODULES + + +
      use stir_soil_texture_mod, only : get_stir_soil_multiplier
      use file_io_mod, only: luostir
      use sci_report_mod, only: scisum
      
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
      include 'm1flag.inc'

!     + + + PURPOSE + + +
!     each time it is called, it calculates the Soil Tillage Intensity Rating
!     for the current operation and adds it to the total.

!     + + + LOCAL VARIABLES + + +
      real stir_op_avg
      integer idx, jdx, kdx

!     + + + LOCAL DEFINITIONS + + +
!     stir_op_avg - average over all burial processes of stir value
!     idx         - index used to search for crop_num

      ! only do if flag is set, before done flag set
      if( (soil_cond .eq. 0) .or. stircum(isr)%done_flg ) return

      if( end_of_file .and. ( .not. report_loop ) ) then
          ! used in stir_crop, which requires at least two passes to work
          ! reset the plant harvest array index during initialization
          stircum(isr)%phopidx = 0
      end if

      if( (.not. stircum(isr)%man_eof) .and. end_of_file ) then
          ! first time at end of managment file
          stircum(isr)%man_eof = end_of_file
          ! reset the plant harvest array index before last pass
          stircum(isr)%phopidx = 0
      else
          ! only go through stir calculation once (see done flag above)
          ! make sure to do on second time through and only in report loop
          if( (.not. stircum(isr)%man_eof) .or. (.not. report_loop) ) return

          ! in report loop and at least once through file, start accumulating
          if( stircum(isr)%proc_cnt .gt. 0 ) then
              stir_op_avg = stircum(isr)%stir_op_sum/stircum(isr)%proc_cnt
          else
              stir_op_avg = 0.0
          end if

          if( ostir .ge. 0.0 ) then
              scisum(isr)%stir = scisum(isr)%stir + ostir
              ! set stir value for each operation
              stircum(isr)%phop(stircum(isr)%phopidx)%phop_stir = ostir
          else
              scisum(isr)%stir = scisum(isr)%stir + stir_op_avg
              stircum(isr)%phop(stircum(isr)%phopidx)%phop_stir = stir_op_avg
          end if

          stircum(isr)%phop(stircum(isr)%phopidx)%phop_energy = oenergyarea * get_stir_soil_multiplier(isr)
          if( oenergyarea .ge. 0.0 ) then
              ! set energy value for each operation
              scisum(isr)%energy = scisum(isr)%energy + stircum(isr)%phop(stircum(isr)%phopidx)%phop_energy
          end if

          ! reset values for next operation
          stircum(isr)%stir_op_sum = 0
          stircum(isr)%oper_cnt = 0
          stircum(isr)%proc_cnt = 0

         ! check for second time at end of managment file
         if( stircum(isr)%man_eof .and. end_of_file ) then
            ! set so only one STIR report produced
            stircum(isr)%done_flg = .true.
            ! create and print STIR report (2nd time through complete, info complete)
            do idx = 1, stircum(isr)%phopcnt
              ! assign crop number to non planting or harvest operations
              if( stircum(isr)%phop(idx)%phop_type .eq. 0 ) then
                ! assign crop number from next planting or harvest operation
                do jdx = idx, stircum(isr)%phopcnt
                   ! index ahead in file to end
                   if( stircum(isr)%phop(jdx)%phop_type .gt. 0 ) then
                      ! planting or harvest operation, use assigned crop number
                      stircum(isr)%phop(idx)%crop_num = stircum(isr)%phop(jdx)%crop_num
                      ! we don't need any more
                      exit
                   else if( jdx .eq. stircum(isr)%phopcnt ) then
                      ! didn't find number, continue from start of file
                      do kdx = 1, idx-1
                         if( stircum(isr)%phop(kdx)%phop_type .gt. 0 ) then
                            ! planting or harvest operation, use assigned crop number
                            stircum(isr)%phop(idx)%crop_num = stircum(isr)%phop(kdx)%crop_num
                            ! we don't need any more
                            exit
                         end if
                      end do
                   end if
                end do
              end if
             
              if( stircum(isr)%phop(idx)%phop_skip .eq. 0 ) then
                ! print this line
                write(luostir(isr),1000) stircum(isr)%phop(idx)%phopday, stircum(isr)%phop(idx)%phopmon, &
                              stircum(isr)%phop(idx)%phopyr, &
                              trim(stircum(isr)%phop(idx)%stir_opname), &
                              trim(stircum(isr)%phop(idx)%stir_cropname), &
                              trim(stircum(isr)%phop(idx)%stir_fuelname), &
                              stircum(isr)%phop(idx)%phop_stir, stircum(isr)%phop(idx)%phop_energy, &
                              stircum(isr)%phop(idx)%crop_num, stircum(isr)%phop(idx)%last_harv
              end if
            end do
         end if
      end if
      return

1000  format (i2,'/',i2,'/',i4,3(' | ',a),2(' | ',f8.2),2(' | ',i1) )

    end subroutine stir_report

end module stir_report_mod

