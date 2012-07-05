!$Author$
!$Date$
!$Revision$
!$HeadURL$
      subroutine stir_crop(isr, bc0nam, plant_harv)

!     + + + ARGUMENT DECLARATIONS + + +
      integer isr
      character*(80)  bc0nam
      integer plant_harv

!     + + + ARGUMENT DEFINITIONS + + +
!     isr - subregion index
!     bc0nam - the crop name for this operation
!     plant_harv - planting or harvest flag
!                  0 - unrelated operation
!                  1 - planting operation
!                  2 - harvest operation

!     + + + INCLUDE + + +
      include 'p1werm.inc'
      include 'command.inc'
      include 'm1flag.inc'
      include 'main/stir_report_val.inc'

!     + + + PURPOSE + + +
!     each time it is called, it assigns the last growing crop name to 
!     the stir crop name for reporting

!     + + + LOCAL VARIABLES + + +
      integer idx, jdx, kdx

      ! only do if flag is set 
      if( (soil_cond .eq. 0) .or. done_flg(isr) ) return

      ! start accounting for crops and harvests
      ! this relies on an initialization cycle and one regular cycle
      if( plant_harv .gt. 0 ) then
         ! this is either a planting or harvest operation.
         if( phopidx(isr)+1 .gt. mxphops ) then
            ! maximum array size exceeded
            write(*,*) 'ERROR: too many planting and harvest Ops'
            write(*,*) ' increase mxphops in stir_report_val.inc'
            stop 1
         end if
         ! use temporary index to make shorter lines
         idx = phopidx(isr)
         phop_type(idx,isr) = plant_harv
         if( plant_harv .eq. 1 ) then
            ! planting operation
            if( crop_num(idx,isr) .eq. 0 ) then
               ! crop number not yet assigned
               do jdx = idx, 1, -1
                  ! index back in op list
                  if( phop_type(jdx,isr) .eq. 2 ) then
                     ! found harvest, this begins a new crop
                     crop_num(idx,isr) = crop_num(jdx,isr) + 1
                     ! only want the last one
                     exit
                  else if( jdx .le. 1 ) then
                     ! no harvest found so first planting of file
                     crop_num(idx,isr) = 1
                  end if
               end do
            end if
         else if( plant_harv .eq. 2 ) then
            ! harvest operation
            if( crop_num(idx,isr) .eq. 0 ) then
               ! crop number not yet assigned
               do jdx = idx, 1, -1
                  ! index back in op list
                  if( phop_type(jdx,isr) .eq. 1 ) then
                     ! found planting, use to set crop number
                     crop_num(idx,isr) = crop_num(jdx,isr)
                     ! that is all
                     exit
                  else if( jdx .le. 1 ) then
                     ! at start of file, no planting found, so continue at end
                     do kdx = phopcnt(isr), idx+1, -1
                        if( phop_type(kdx,isr) .eq. 1 ) then
                           ! found planting, use to set crop number
                           crop_num(idx,isr) = crop_num(kdx,isr)
                           ! that is all
                           exit
                        end if
                     end do
                  end if
               end do
            end if
            if( report_loop .eqv. .true. ) then
               ! All harvest ops should be present, check for last harvest
               do jdx = idx, phopcnt(isr)
                  ! index forward looking for harvest or planting op
                  if( phop_type(jdx,isr) .eq. 1 ) then
                     ! found planting, this is last harvest
                     last_harv(idx, isr) = 1
                     ! no more checking needed
                     exit
                  else if( jdx .eq. phopcnt(isr) ) then
                     ! at end of file, restart at beginning
                     do kdx = 1, idx-1
                        if( phop_type(kdx,isr) .eq. 1 ) then
                           ! found planting, this is last harvest
                           last_harv(idx, isr) = 1
                           ! no more checking needed
                           exit
                        else if( phop_type(kdx,isr) .eq. 2 ) then
                           ! found harvest, this is not last harvest
                           ! no more checking needed
                           exit
                        end if
                     end do
                  else if( (phop_type(jdx,isr) .eq. 2)                  &
     &               .and. (jdx .ne. idx) ) then
                     ! found harvest, this is not last harvest
                     ! no more checking needed
                     exit
                  end if
               end do
            end if
         end if
      end if

      if( report_loop .neqv. .true. ) return

      ! register crop name for this event
      stir_cropname(phopidx(isr),isr) = bc0nam

      return
      end
