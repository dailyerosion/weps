
!$Author: wjr $
!$Date: 2011-11-06 $
!$Revison: 0.1 $
!$Source: testerode.f95,v $
!-----------------------------------------------------------------------------------
!! Program to test erosion simulation
!!

program testerode

    use Grid
    use Cell
    use Wind
    use Soil
    use Erosion

    implicit none
    type (GridCell), pointer :: inpGrid
    type (GridCell), pointer :: curp
!    integer :: linCnt
    type (CellRec), pointer :: inpCell
    type (WindRec), pointer :: inpWind
    integer :: idx
    type (Summary), pointer, dimension(:) :: summaries
    real, parameter :: SNOWDEPTH = 20.0			! maximum snow depth where erosion can occur

! start program

    allocate(summaries(5))				! annual & quaterly summaries
	
    inpGrid => LoadGridFile("data/grid.run")		! load grid
    call PrintGrid(6, inpGrid)				! debugging statement

    inpGrid => MakeGrid(inpGrid)
    inpGrid => PrintTouches(inpGrid)

    inpCell => LoadCellFile("data/cell.run")

    call PrintCellFile(6, inpCell)

    call UpdateGridWithCells(inpGrid, inpCell)

    inpWind => LoadWindFile("data/wingen.win")

    write(*, fmt='(i2 i2 i2 )') inpWind%day, inpWind%month, inpWind%year

    write(*, fmt='(25f5.1)') inpWind%direction, (inpWind%speed(idx), idx=1,24)

    

end program testerode