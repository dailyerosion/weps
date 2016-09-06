!$Author$
!$Date$
!$Revision$
!$HeadURL$
      subroutine stir_crop(isr, bc0nam, plant_harv)

      use stir_report_mod, only: stircum

!     + + + ARGUMENT DECLARATIONS + + +
      integer isr
      character*(80)  bc0nam
      integer plant_harv

!     + + + ARGUMENT DEFINITIONS + + +
!     isr - subregion index
!     bc0nam - the crop name for this operation
!     plant_harv - planting or harvest/termination flag
!                  0 - unrelated operation
!                  1 - planting operation
!                  2 - harvest or termination operation

!     + + + INCLUDE + + +
      include 'p1werm.inc'
      include 'command.inc'
      include 'm1flag.inc'

!     + + + PURPOSE + + +
!     each time it is called, it assigns the last growing crop name to 
!     the stir crop name for reporting

!     + + + LOCAL VARIABLES + + +
      integer idx, jdx, kdx

      ! only do if flag is set 
      if( (soil_cond .eq. 0) .or. stircum(isr)%done_flg ) return

   ! debug start
   !   write(*,'(2(a,io))') 'subregion: ', isr, ' phopidx: ', stircum(isr)%phopidx
   !   do jdx = 1, stircum(isr)%phopcnt
   !      write(*,'(6(i0,a),a)') stircum(isr)%phop(jdx)%phopday, '/', stircum(isr)%phop(jdx)%phopmon, '/', &
   !                           stircum(isr)%phop(jdx)%phopyr, &
   !                           ' type ', stircum(isr)%phop(jdx)%phop_type, &
   !                           ' crop_num ', stircum(isr)%phop(jdx)%crop_num, &
   !                           ' last_harv ', stircum(isr)%phop(jdx)%last_harv, &
   !                           ' ', trim(stircum(isr)%phop(jdx)%stir_opname)
   !   end do
   ! debug end

      ! start accounting for crops and harvests/terminations
      ! this relies on an initialization cycle and one regular cycle
      if( plant_harv .gt. 0 ) then
         ! this is either a planting or harvest/termination operation.
         if( stircum(isr)%phopidx+1 .gt. size(stircum(isr)%phop) ) then
            ! maximum array size exceeded
            write(*,*) 'ERROR: too many planting and harvest Ops'
            write(*,*) ' increase number in WEPS main create_stir_accumulators() call'
            stop 1
         end if
         ! use temporary index to make shorter lines
         idx = stircum(isr)%phopidx
         stircum(isr)%phop(idx)%phop_type = plant_harv
         if( idx .eq. stircum(isr)%phoplastidx ) then
            ! stir_crop called multiple times in same operation
            ! reset crop number so it will be redone
            stircum(isr)%phop(idx)%crop_num = 0
         end if

         if( plant_harv .eq. 1 ) then
            ! planting operation
            if( stircum(isr)%phop(idx)%crop_num .eq. 0 ) then
               ! crop number not yet assigned
               if( idx .gt. 1 ) then
                  do jdx = idx-1, 1, -1
                     ! index back in op list
                     if( stircum(isr)%phop(jdx)%phop_type .eq. 2 ) then
                        ! found harvest/termination, this begins a new crop
                        stircum(isr)%phop(idx)%crop_num = stircum(isr)%phop(jdx)%crop_num + 1
                        ! only want the last one
                        exit
                     else if( stircum(isr)%phop(jdx)%phop_type .eq. 1 ) then
                        ! found planting without harvest/termination, this begins a new crop
                        stircum(isr)%phop(idx)%crop_num = stircum(isr)%phop(jdx)%crop_num + 1
                        ! only want the last one
                        exit
                     else if( jdx .le. 1 ) then
                        ! no harvest/termination found so first planting of file
                        stircum(isr)%phop(idx)%crop_num = 1
                     end if
                  end do
               else
                  ! planting is first operation of file
                  stircum(isr)%phop(idx)%crop_num = 1
               end if
            end if
         else if( plant_harv .eq. 2 ) then
            ! harvest/termination operation
            if( stircum(isr)%phop(idx)%crop_num .eq. 0 ) then
               ! crop number not yet assigned
               if( idx .gt. 1 ) then
                  do jdx = idx-1, 1, -1
                     ! index back in op list
                     if( stircum(isr)%phop(jdx)%phop_type .eq. 1 ) then
                        ! found planting, use to set crop number
                        stircum(isr)%phop(idx)%crop_num = stircum(isr)%phop(jdx)%crop_num
                        ! that is all
                        exit
                     else if( jdx .le. 1 ) then
                        ! at start of file, no planting found, so continue at end
                        do kdx = stircum(isr)%phopcnt, idx+1, -1
                           if( stircum(isr)%phop(kdx)%phop_type .eq. 1 ) then
                              ! found planting, use to set crop number
                              stircum(isr)%phop(idx)%crop_num = stircum(isr)%phop(kdx)%crop_num
                              ! that is all
                              exit
                           end if
                        end do
                     end if
                  end do
               else
                  ! at start of file, so search from end
                  do kdx = stircum(isr)%phopcnt, idx+1, -1
                     if( stircum(isr)%phop(kdx)%phop_type .eq. 1 ) then
                        ! found planting, use to set crop number
                        stircum(isr)%phop(idx)%crop_num = stircum(isr)%phop(kdx)%crop_num
                        ! that is all
                        exit
                     end if
                  end do
               end if
            end if
            if( report_loop .and. stircum(isr)%man_eof ) then ! man_eof indicates at least one pass completed
               ! All harvest/termination ops should be present, check for last harvest/termination
               if( idx .lt. stircum(isr)%phopcnt ) then
                  ! check operations to end of management file and wrap as needed
                  do jdx = idx+1, stircum(isr)%phopcnt
                     ! index forward looking for harvest/termination or planting op
                     if( stircum(isr)%phop(jdx)%phop_type .eq. 1 ) then
                        ! found planting, this is last harvest/termination
                        stircum(isr)%phop(idx)%last_harv = 1
                        ! no more checking needed
                        exit
                     else if( stircum(isr)%phop(jdx)%phop_type .eq. 2 ) then
                        ! found harvest/termination, this is not last harvest/termination
                        ! no more checking needed
                        exit
                     end if
                     if( jdx .eq. stircum(isr)%phopcnt ) then
                        ! at end of file, restart at beginning
                        do kdx = 1, idx-1
                           if( stircum(isr)%phop(kdx)%phop_type .eq. 1 ) then
                              ! found planting, this is last harvest/termination
                              stircum(isr)%phop(idx)%last_harv = 1
                              ! no more checking needed
                              exit
                           else if( stircum(isr)%phop(kdx)%phop_type .eq. 2 ) then
                              ! found harvest/termination, this is not last harvest/termination
                              ! no more checking needed
                              exit
                           end if
                        end do
                     end if
                  end do
               else
                  ! start checking operations at beginning of managment file, no wrapping required
                  do jdx = 1, stircum(isr)%phopcnt
                     ! index forward looking for harvest/termination or planting op
                     if( stircum(isr)%phop(jdx)%phop_type .eq. 1 ) then
                        ! found planting, this is last harvest/termination
                        stircum(isr)%phop(idx)%last_harv = 1
                        ! no more checking needed
                        exit
                     else if( stircum(isr)%phop(jdx)%phop_type .eq. 2 ) then
                        ! found harvest/termination, this is not last harvest/termination
                        ! no more checking needed
                        exit
                     end if
                  end do
               end if
            end if
         end if
         ! always reset
         stircum(isr)%phoplastidx = idx
      end if

      if( report_loop .neqv. .true. ) return

      ! register crop name for this event
      stircum(isr)%phop(stircum(isr)%phopidx)%stir_cropname = bc0nam

      return
      end
