!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine submodels (isr, crop, residue, restot, croptot,        &
     &                      biotot, decompfac, mandate)

      use weps_interface_defs
      use biomaterial, only: biomatter, biototal, decomp_factors
      use mandate_mod, only: opercrop_date

      include 'p1werm.inc'
      include 'm1flag.inc'      !am0cropupfl
      include 'main/main.inc'   !daysim, lopday, lopmon, lopyr, iy

!     + + + ARGUMENT DECLARATIONS + + +
      integer isr
      type(biomatter), intent(inout) :: crop    ! structure containing full crop description
      type(biomatter), dimension(:), intent(inout) :: residue
      type(biototal), intent(inout) :: restot, croptot, biotot
      type(decomp_factors), intent(inout) :: decompfac
      type(opercrop_date), dimension(:), intent(inout) :: mandate

!     + + + ARGUMENT DEFINITIONS + + +
!     restot          - structure array containing summary residue pool amounts for all subregions

!        write(*,*) "Start manage"      !MANAGEment (tillage) submodel
        call manage( isr, iy, lopday, lopmon, lopyr, crop, residue,     &
     &               biotot,mandate )

!        write(*,*) "Start updres"
        call updres(isr, residue, restot)                 !update decomp residue pools

!        write(*,*) "Start callhydr"
        call callhydr(daysim, isr, restot, biotot)      !call HYDROLOGY submodel
        ! do not change order. Hydro may set irrigation amounts that
        ! will affect soil.

!        write(*,*) "Start callsoil"
        call callsoil(daysim, isr, biotot)       !SOIL submodel

!        write(*,*) "Start callcrop"     !CROP submodel
        ! Crop growth flag indicates growing crop
        ! Harvest flag indicates that harvest occured today. Crop is called
        ! to generate end of growth period report from values retained in
        ! the previous day crop data registers even though growth flag is
        ! turned off.
        if( crop%growth%am0cgf .or. (am0cropupfl.gt.0) ) then
            call callcrop(daysim, isr, crop, residue, restot, croptot)
        end if

!        write(*,*) "Start decomp"
        call decomp(isr, residue, decompfac)         !DECOMPosition submodel

!        write(*,*) "Start updres"
        call updres(isr, residue, restot)

!        write(*,*) "Start sumbio"
        call sumbio(isr, residue, restot, croptot, biotot) ! sum live and dead biomass

      return
      end

