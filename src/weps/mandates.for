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

! NOTE:  This array "mandate" gets updated within the "management"
!        submodel where the harvest operations get associated with
!        their respective crops.  Currently that src file is:
!        manage/report_harvest.for

      subroutine mandates(sr)

      use mandate_vars    ! Load shared mandates() array

      include 'p1werm.inc'
      include 'command.inc'
      include 'manage/man.inc'

      integer sr

      integer linidx
      character*256 line
      integer idx1, idx2

      integer cnt_man_dates
      logical cnt_em
      integer i

!     type :: man_opcrop_dates_type
!          integer :: d, m, y 
!          character(65) :: opname, cropname
!     end type man_opcrop_dates_type

!     type (man_opcrop_dates_type), dimension(:),
!    &        allocatable, target :: mandate
!     type (man_opcrop_dates_type), dimension(:), pointer :: mp

      cnt_man_dates = 0

      cnt_em = .TRUE.    ! first time parsing, just count op dates

1     do 5 linidx = mbeg(sr), mbeg(sr+1) - 1
         line = mtbl(linidx)
         if (line(1:1) .eq. 'D') then
            ! have a date - do something here
            if (cnt_em .eqv. .TRUE.) then
               cnt_man_dates = cnt_man_dates + 1
               goto 5 ! look for next operation date
            endif
            i = i + 1
            read (line (3:12),'(i2,1x,i2,1x,i4)', err=902)              &
     &                 mandate(i)%d,mandate(i)%m,mandate(i)%y
            !print *, mandate(i)%d,mandate(i)%m,mandate(i)%y

            ! Move the tbl ptr to the first operation after the date
            do 10 idx1 = linidx+1, mbeg(sr+1) -1 
              line = mtbl(idx1)
              if (line(1:1) .eq. 'O') then         ! Got an operation name
                read (line(6:), '(a)') mandate(i)%opname
                !print *, trim(mandate(i)%opname)

                ! Move the tbl ptr to the first "G 03" line after the operation
                do 15 idx2 = idx1+1, mbeg(sr+1) -1 
                  line = mtbl(idx2)
                  !print *, line
                  if (line(1:4) .eq. 'G 03') then   ! Got a crop group line
                    line = mtbl(idx2+1)             ! Crop name on next line
                    read (line(3:), '(a)') mandate(i)%cropname
                    !print *, trim(mandate(i)%cropname)
                    goto 10    ! can have only one crop per operation right now                

                  else if (line(1:1) .eq. 'D') then ! done looking for crop name
                    mandate(i)%cropname = ""
                    goto 10                 
                  else if (line(1:1) .eq. '*') then
                    mandate(i)%cropname = ""
                    !print *, "No cropname - end of management file"
                    goto 20 
                  endif
15              continue
              else if (line(1:1) .eq. 'D') then  ! done looking for op name
                goto 5
              else if (line(1:1) .eq. '*') then
                !print *, "No opname - end of management file"
                goto 20
              endif
10          continue

         endif
5     continue

20    if (cnt_em .eqv. .TRUE.) then
         cnt_em = .FALSE.
         !print *, "cnt_em", cnt_em, cnt_man_dates

         if (allocated (mandate)) then
            goto 999   !already allocated - must be in calibrate mode
         else
            allocate (mandate(1:cnt_man_dates))
         endif

         mp => mandate
         i = 0

         goto 1               ! Parse again, but this time grab dates and names
      endif
 
      if( report_debug >= 1 ) then
          do 30 i = 1, cnt_man_dates

              print *, i, mp(i)%d, mp(i)%m, mp(i)%y,                    &
     &            trim(mp(i)%opname)," ",trim(mp(i)%cropname)
              print *, i, mandate(i)%d, mandate(i)%m, mandate(i)%y,     &
     &            trim(mandate(i)%opname)," ",trim(mandate(i)%cropname)
              print *, 'mp', mp(i)

30        end do
          print *, 'size of mandate', size(mandate)
      end if

999   return

!
! Error stops
!
902   write(0, 9902) line, sr
9902  format('mandates.for: Bad date format ',a,' in subregion ',i2)
      call exit(1)

      end
