!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!
! This routine determines the date of all management operations
! and their names, along with any crop names associated with them.
!
! It fills a dynamically allocated array "mandate" of user-defined
! type "man_opcrop_dates_type".  It first reads the management file
! to determine the number of operations.  Then it allocates the
! necessary space for "mandate".  Then it re-reads the file to
! fill it with the date and names info.

! Harvest operations are updated in "report_harvest" to have the 
! name of the harvested crop associated with them.

      subroutine mandates(mandate, manFile)

      use weps_main_mod, only: report_debug
      use mandate_mod, only: opercrop_date, create_mandate    ! Load shared mandate() array
      use manage_data_struct_defs, only: man_file_struct
      use manage_data_struct_mod, only: get_value_index

      type (opercrop_date), dimension(:), allocatable :: mandate
      type(man_file_struct), intent(inout) :: manFile

      integer idx

      integer cnt_man_dates
      integer i

      if (allocated (mandate)) then
         return   ! already allocated so values are already populated (calibration mode)
      end if

      ! count number of operations for allocation of mandate
      cnt_man_dates = 0
      manFile%oper => manFile%operFirst
      do while ( associated(manFile%oper) )
        cnt_man_dates = cnt_man_dates + 1
        manFile%oper => manFile%oper%operNext
      end do

      call create_mandate( cnt_man_dates, mandate )

      ! assign dates
      i = 0
      manFile%oper => manFile%operFirst
      do while ( associated(manFile%oper) )
        i = i + 1
        ! assign subregion
        mandate(i)%sr = manFile%isub
        ! assign operation dates
        mandate(i)%d = manFile%oper%operDate%day
        mandate(i)%m = manFile%oper%operDate%month
        mandate(i)%y = manFile%oper%operDate%year
        ! assign operation name
        mandate(i)%opname = manFile%oper%operName
        ! look for crop group
        mandate(i)%cropname = ""
        manFile%grp => manFile%oper%grpFirst
        do while ( associated(manFile%grp) )
          if ( manFile%grp%grpID .eq. '03' ) then
            idx=get_value_index(manFile%grp%OGPidx,'gcropname')
            mandate(i)%cropname = manFile%grp%s_param(idx)%p_value
          end if
          ! next group
          manFile%grp => manFile%grp%grpNext
        end do
        ! next operation
        manFile%oper => manFile%oper%operNext
      end do

      if( report_debug >= 1 ) then
          do i = 1, cnt_man_dates
              print *, i, mandate(i)%d, mandate(i)%m, mandate(i)%y,     &
     &          trim(mandate(i)%opname)," | ",trim(mandate(i)%cropname)
              !print *, 'mandate', mandate(i)
          end do
          print *, 'size of mandate', size(mandate)
      end if

      ! reset manFile  operation to beginning
      manFile%oper => manFile%operFirst

      return

      end
