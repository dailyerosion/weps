
!$Author: wjr $
!$Date: 2011-11-06 $
!$Revison: 0.1 $
!$Source: grid.f95,v $
!-----------------------------------------------------------------------------------
!! Grid routines
!!

MODULE Grid

    USE Cell

    IMPLICIT NONE

!! Methods defined in this module

    PUBLIC :: LoadGridFile
    PUBLIC :: MakeGrid
    PUBLIC :: PrintTouches
    PRIVATE :: CalcXferFraction
    PUBLIC :: UpdateGridWithCells
    PUBLIC :: PrintGrid

!! Methods to be defined in this module

!    PUBLIC :: TranverseGrid
!    PUBLIC :: GridCellPtr

!! Data structures defined in this module

    TYPE, PUBLIC :: GridCell
	integer :: id					!! serial number (starts at 1)
	integer :: x1					!! left boundary of cell
	integer :: y1					!! top boundary of cell
	integer :: x2					!! right boundary of cell
	integer :: y2					!! bottom boundary of cell
	integer :: cellType
	type (GridCell), pointer :: nxtPtr		!! pointer to next cell in order of creation
							!! is list of cells read in
	type (CellRec), pointer :: cellp			!! cell associated with this grid point
							
	integer, dimension(:), allocatable :: lefCells	!! array of cell id's for cells touching on the left
	integer, dimension(:), allocatable :: topCells	!! array of cell id's for cells touching on the top
	integer, dimension(:), allocatable :: rigCells	!! array of cell id's for cells touching on the right
	integer, dimension(:), allocatable :: botCells	!! array of cell id's for cells touching on the bottom

	real, dimension(:), allocatable :: lefFrac	!! array of fractions for dividing flux flowing left
	real, dimension(:), allocatable :: topFrac	!! array of fractions for dividing flux flowing up
	real, dimension(:), allocatable :: rigFrac	!! array of fractions for dividing flux flowing right
	real, dimension(:), allocatable :: botFrac	!! array of fractions for dividing flux flowing down

    END TYPE

    TYPE, PUBLIC :: GridCellPtr
	type (GridCell), pointer :: ptr
    END TYPE
    
CONTAINS

!! Reads file containing grid cells -- format of file is x1, y1, x2, y2, type
!! That is, left, top, right, bottom coordinants of cell, followed by the type of cell described in a separate file

    FUNCTION LoadGridFile(filNam) result (GridPtr)
	
        character (len=*), intent(in) :: filNam
	type (GridCell), pointer :: GridPtr

	integer :: eofFlg
	integer :: cnt
	integer :: x1, y1, x2, y2

	type (GridCell), pointer :: basp
	type (GridCell), pointer :: curp
	type (GridCell), pointer :: prvp

!	write(*,*) 
!	write(*,*) 'List of input cells'

        open(11, file=filNam, status="old", action="read")
	read(11, fmt=*) ! skip header line

	allocate(basp)
	prvp => basp
	cnt = 1
	do
	    allocate(curp)
	    curp%id = cnt
	    cnt = cnt + 1
	    prvp%nxtPtr => curp
!	    read(unit=11, fmt=*, iostat=eofFlg) x1, y1, x2, y2
	    read(unit=11, fmt=*, iostat=eofFlg) curp%x1, curp%y1, curp%x2, curp%y2, curp%cellType
	    if (eofFlg<0) then
	        exit
	    end if
!	    write(*,*) 'read ', curp%x1
	    prvp => curp
	end do
	
	curp%id = -1
	curp%x1 = -1
	close(11)
	GridPtr => basp%nxtPtr
	
    END FUNCTION LoadGridFILE

    FUNCTION MakeGrid(InpGridPtr) result (RtnGridPtr)

        type (GridCell), pointer :: InpGridPtr
        type (GridCell), pointer :: RtnGridPtr
	
	type (GridCell), pointer :: curp
	type (GridCell), pointer :: tstp

	integer :: minX, minY, maxX, maxY
	integer :: ltouches,rtouches,ttouches,btouches,touches

	integer :: cnt
! This is a really stupid thing to do but F95 doesn't seem to have arrays of pointers
	type (GridCellPtr), dimension(:), allocatable :: GridCellList

	minX = InpGridPtr%x1
	minY = InpGridPtr%y1
	maxX = InpGridPtr%x2
	maxY = InpGridPtr%y2

	curp => InpGridPtr

	cnt = 0

	do while (curp%x1 /= -1)
	    minX = min(minX, curp%x1)
	    minY = min(minY, curp%y1)
	    maxX = max(maxX, curp%x2)
	    maxY = max(maxY, curp%y2)
	    curp => curp%nxtPtr
	    cnt = cnt + 1
	end do
	
	write(*,*) 'minX ', minX, ' minY ', minY, ' maxX ', maxX, ' maxY ', maxY
	write(*,*)

! The grid cell list is simply an array of pointers to grid cells. Apparently, F95 does
! not allow the creation of allocalable arrays of pointers but does allow allocalable 
! arrays of structures. So, the work-around is to create a structure that contains only
! a pointer to the grid cells and then allocate that. Unfortunately, you can't put this
! array into the structure itself because of the circular reference and the apparent
! fact that F95 doesn't allow forward references. (sigh)

	allocate(GridCellList(cnt))
	cnt = 1
	do while (curp%x1 /= -1)
	    GridCellList(cnt)%ptr => curp
	    cnt = cnt + 1
	end do

!! find adjacent cells and set up links

	curp => InpGridPtr
	do while (curp%x1 /= -1)

! touches on right
	    tstp => InpGridPtr
	    touches = 0
	    do while (tstp%x1 /= -1) 
	        if (curp%x2 == tstp%x1) then
		    if (curp%y1 < tstp%y2 .and. curp%y2 > tstp%y1) then
!		        write(*,*) curp%id, ' touches ', tstp%id
			touches = touches + 1
		    endif
		endif
		tstp => tstp%nxtPtr
	    end do
	    rtouches = touches
	    allocate(curp%rigCells(touches+1))  ! allocate an extra so -1 flags end of list
	    allocate(curp%rigFrac(touches))
	    curp%rigFrac(1) = 1.0		! init to 1.0; will be changed if more than one touches
	    curp%rigCells(touches+1) = -1
	    if (touches > 0) then
!	    if (.false.) then
		tstp => InpGridPtr
		touches = 1
		do while (tstp%x1 /= -1) 
	            if (curp%x2 == tstp%x1) then
		        if (curp%y1 < tstp%y2 .and. curp%y2 > tstp%y1) then
!		            write(*,*) curp%id, ' touches ', tstp%id
			    curp%rigCells(touches) = tstp%id
			    curp%rigFrac(touches) = CalcXferFraction(curp%y1,curp%y2,tstp%y1,tstp%y2)
			    touches = touches + 1
			endif
		    endif
		    tstp => tstp%nxtPtr
	        end do
            endif

!touches on left
	    tstp => InpGridPtr
	    touches = 0
	    do while (tstp%x1 /= -1) 
	        if (curp%x1 == tstp%x2) then
		    if (curp%y1 < tstp%y2 .and. curp%y2 > tstp%y1) then
!		        write(*,*) curp%id, ' touches ', tstp%id
			touches = touches + 1
		    endif
		endif
		tstp => tstp%nxtPtr
	    end do
	    ltouches = touches
	    allocate(curp%lefCells(touches+1))
	    allocate(curp%lefFrac(touches))
	    curp%lefFrac(1) = 1.0		! init to 1.0; will be changed if more than one touches
	    curp%lefCells(touches+1) = -1
!	    if (.false.) then
	    if (touches > 0) then
	        tstp => InpGridPtr
		touches = 1
		do while (tstp%x1 /= -1) 
	            if (curp%x1 == tstp%x2) then
		        if (curp%y1 < tstp%y2 .and. curp%y2 > tstp%y1) then
!		            write(*,*) curp%id, ' touches ', tstp%id
			    curp%lefCells(touches) = tstp%id
			    curp%lefFrac(touches) = CalcXferFraction(curp%y1,curp%y2,tstp%y1,tstp%y2)
			    touches = touches + 1
		        endif
		    endif
		    tstp => tstp%nxtPtr
	        end do
            endif


!! find adjacent cells in y direction

! touches on top
	    tstp => InpGridPtr
	    touches = 0
	    do while (tstp%x1 /= -1) 
	        if (curp%y1 == tstp%y2) then
		    if (curp%x1 < tstp%x2 .and. curp%x2 > tstp%x1) then
!		        write(*,*) curp%id, ' touches ', tstp%id
			touches = touches + 1
		    endif
		endif
		tstp => tstp%nxtPtr
	    end do
	    ttouches = touches
	    allocate(curp%topCells(touches+1))
	    allocate(curp%topFrac(touches))
	    curp%topFrac(1) = 1.0		! init to 1.0; will be changed if more than one touches
	    curp%topCells(touches+1) = -1
	    if (touches > 0) then
!	    if (.false.) then
		tstp => InpGridPtr
		touches = 1
		do while (tstp%x1 /= -1) 
		    if (curp%y1 == tstp%y2) then
		        if (curp%x1 < tstp%x2 .and. curp%x2 > tstp%x1) then
!			    write(*,*) curp%id, ' touches ', tstp%id
	  		    curp%topCells(touches) = tstp%id
			    curp%topFrac(touches) = CalcXferFraction(curp%x1,curp%x2,tstp%x1,tstp%x2)
			    touches = touches + 1
			endif
		    endif
                    tstp => tstp%nxtPtr
	        end do
            endif

! touches on bottom
	    tstp => InpGridPtr
	    touches = 0
	    do while (tstp%x1 /= -1) 
	        if (curp%y2 == tstp%y1) then
		    if (curp%x1 < tstp%x2 .and. curp%x2 > tstp%x1) then
!		        write(*,*) curp%id, ' touches ', tstp%id
			touches = touches + 1
		    endif
		endif
		tstp => tstp%nxtPtr
	    end do
	    btouches = touches
	    allocate(curp%botCells(touches+1))
	    allocate(curp%botFrac(touches))
	    curp%botFrac(1) = 1.0		! init to 1.0; will be changed if more than one touches
	    curp%botCells(touches+1) = -1
!	    if (.false.) then
	    if (touches > 0) then
		tstp => InpGridPtr
		touches = 1
		do while (tstp%x1 /= -1) 
		    if (curp%y2 == tstp%y1) then
		        if (curp%x1 < tstp%x2 .and. curp%x2 > tstp%x1) then
!			    write(*,*) curp%id, ' touches ', tstp%id
	  		    curp%botCells(touches) = tstp%id
			    curp%botFrac(touches) = CalcXferFraction(curp%x1,curp%x2,tstp%x1,tstp%x2)
			    touches = touches + 1
			endif
		    endif
                    tstp => tstp%nxtPtr
	        end do
            endif

	    write(*,*) curp%id, ' touches l,r,t,b ', ltouches, rtouches, ttouches, btouches


	    curp => curp%nxtPtr
	end do

	RtnGridPtr => InpGridPtr

    END FUNCTION MakeGrid
    
    FUNCTION PrintTouches(InpGridPtr) result (RtnGridPtr)

        type (GridCell), pointer :: InpGridPtr
        type (GridCell), pointer :: RtnGridPtr

	type (GridCell), pointer :: curp
	integer :: idx

	curp => InpGridPtr

	do while (curp%x1 /= -1)
	    write(*,*) 'Cell ', curp%id
	    idx = 1
	    do while (curp%rigCells(idx) /= -1)
	        write(*,"(a,i2,f7.3)") '  touches on right ', curp%rigCells(idx), curp%rigFrac(idx)
		idx = idx + 1
	    end do
	    idx = 1
	    do while (curp%lefCells(idx) /= -1)
	        write(*,'(a,i2,f7.3)') '  touches on left ', curp%lefCells(idx), curp%lefFrac(idx)
		idx = idx + 1
	    end do
	    idx = 1
	    do while (curp%topCells(idx) /= -1)
	        write(*,'(a,i2,f7.3)') '  touches on top ', curp%topCells(idx), curp%topFrac(idx)
		idx = idx + 1
	    end do
	    idx = 1
	    do while (curp%botCells(idx) /= -1)
	        write(*,'(a,i2,f7.3)') '  touches on bottom ', curp%botCells(idx), curp%botFrac(idx)
		idx = idx + 1
	    end do
            curp => curp%nxtPtr
        end do

	RtnGridPtr => InpGridPtr

    END FUNCTION PrintTouches

!! The purpose of this function is to calculate the fractions flowing
!! from this cell into adjacent cells.
!! It takes either the x's or y's from the target cell and uses the 
!! overlap to calculate how much from this cell flows into the adjacent
!! cell. Obviously, if the is only one adjacent cell, this fraction is 
!! 1.0. If there is more than one adjacent cell the flow has to be 
!! apportioned. That is what this function does.

    FUNCTION CalcXferFraction(a1, a2, b1, b2) result (frac)

! a1 & a2 map to either x1 & x2 or y1 & y2 of the target cell
! this implies that x1 < x2 in all cases
        integer :: a1, a2    
! b1 & b2 map to either x1 & x2 or y1 & y2 of the adjacent cell
        integer :: b1, b2
! frac is the fraction of the target cell that the adjacent cell covers
! if the two cell have the same length than frac is 1.0; otherwise it is 0 < frac < 1.0
	real :: frac

! length of the target cell and the portion of the adjacent cell that abuts the target
	integer :: deltaA, deltaB

	deltaA = a2 - a1
	deltaB = min(a2,b2) - max(a1,b1)
	
	frac = (deltaB * 1.0) / (deltaA * 1.0)

!	write(*, '(a i3: i3: i3: i3: f7.3)') 'frac ', a2, a1, b2, b1, frac

    END FUNCTION CalcXferFraction

    SUBROUTINE UpdateGridWithCells(gridp, cellp)

	type (GridCell), pointer, INTENT (IN) :: gridp
	type (CellRec), pointer, INTENT (IN) :: cellp

	type (GridCell), pointer :: cgridp
	type (CellRec), pointer :: ccellp

	integer :: totCnt, fndCnt

	totCnt = 0
	fndCnt = 0
	cgridp => gridp

	do while (cgridp%id /= -1)
	    write(*,*) '@grid ',cgridp%id, cgridp%x1, cgridp%y1
	    totCnt = totCnt + 1
	    ccellp => cellp
	    do while (ccellp%id /= -1)
!		write(*,*) '>> ', ccellp%id, cgridp%cellType
	        if (ccellp%id == cgridp%cellType) then
		    fndCnt = fndCnt + 1
		    cgridp%cellp => CloneCell(ccellp)
		    exit
		endif
		ccellp => ccellp%nxtPtr
            end do
	    cgridp => cgridp%nxtPtr
	end do
	write(*,*) 'update grid with cell -- tot ', totCnt, ' fnd ', fndCnt
    END SUBROUTINE UpdateGridWithCells

    SUBROUTINE PrintGrid(luo, inpGrid)

	INTEGER, INTENT (IN) :: luo
	type (GridCell), pointer, INTENT (IN) :: inpGrid

	type (GridCell), pointer :: curp

	curp => inpGrid					

	do while (curp%x1 /= -1)
	    write(luo,'(5I4)') curp%id, curp%x1, curp%y1, curp%x2, curp%y2, curp%cellType
	    curp => curp%nxtPtr
	    if (.not. associated(curp)) then
	        exit
	    endif
        end do
	write(*,*)
    END SUBROUTINE PrintGrid

END MODULE Grid
  