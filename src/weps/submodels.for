!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine submodels (isr, cd, cm, cy)

      include 'p1werm.inc'
      include 'm1flag.inc'      !am0cgf
      include 'main/main.inc'   !daysim, lopday, lopmon, lopyr, iy

! Arguments
      integer isr, cd, cm, cy

!        write(*,*) "Start manage"      !MANAGEment (tillage) submodel
        call manage (isr, cd, cm, cy,iy,lopday,lopmon,lopyr)

!        write(*,*) "Start updres"
        call updres(isr)                 !update decomp residue pools

!        write(*,*) "Start callhydr"
        call callhydr(daysim, isr)      !call HYDROLOGY submodel
        ! do not change order. Hydro may set irrigation amounts that
        ! will affect soil.

!        write(*,*) "Start callsoil"
        call callsoil(daysim, isr)       !SOIL submodel

!        write(*,*) "Start callcrop"     !CROP submodel
        ! Crop growth flag indicates growing crop
        ! Harvest flag indicates that harvest occured today. Crop is called
        ! to generate end of growth period report from values retained in
        ! the previous day crop data registers even though growth flag is
        ! turned off.
        if( am0cgf .or. (am0cropupfl.gt.0) ) then
            call callcrop(daysim, isr)
        end if

!        write(*,*) "Start decomp"
        call decomp(isr)                 !DECOMPosition submodel

!        write(*,*) "Start updres"
        call updres(isr)

!        write(*,*) "Start sumbio"
        call sumbio(isr) ! sum live and dead biomass

      return
      end

