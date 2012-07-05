
!$Author: wjr $
!$Date: 2011-11-06 $
!$Revison: 0.1 $
!$Source: testerode.f95,v $
!-----------------------------------------------------------------------------------
!! Program to test wind gen loading and manipulation routines
!!

program testwind

    use Wind

    implicit none

    TYPE (Wind), pointer :: windp
    TYPE (WindList), pointer :: listp
    integer sday, eday

    sday = 2 * 10000 + 3 * 100 + 20		!start day for wind list (3/20/02)
    eday = 2 * 10000 + 3 * 100 + 22		!end day for wind list (3/22/02)

    windp => LoadWindFile('win_gen.win') 

    call PrintWindFile(6, windp, 10)

    listp => MakeWindList(windp, sday, eday)

    call PrintWindList(6, listp, -1)

end program testwind