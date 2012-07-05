
!$Author: wjr $
!$Date: 2011-11-06 $
!$Revison: 0.1 $
!$Source: testerode.f95,v $
!-----------------------------------------------------------------------------------
!! Program to test erosion simulation
!!

program testsoil

    use Soil

    implicit none

    TYPE (SoilRec), pointer :: soilp   

    write(*,*) 'before loadsoil'
    soilp => LoadSoilFile('data/test.ifc') 

   call PrintSoil(6, soilp)

   ! soilp => LoadSoilFile('data/soil2.ifc') 

    !call PrintSoil(6, soilp)
    write(*,*) 'hello world 2'

end program testsoil