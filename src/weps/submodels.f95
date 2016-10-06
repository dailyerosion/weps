!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine submodels (isr, crop, residue, restot, croptot,        &
     &                      biotot, decompfac, mandate, h1et, wp, subrsurf)

      use weps_interface_defs, ignore_me=>submodels
      use biomaterial, only: biomatter, biototal, decomp_factors
      use mandate_mod, only: opercrop_date
      use hydro_data_struct_defs, only: hydro_derived_et
      use wepp_param_mod, only: wepp_param
      use erosion_data_struct_defs, only: subregionsurfacestate

      include 'p1werm.inc'
      include 'm1flag.inc'      !am0cropupfl
      include 'main/main.inc'   !daysim, iy
      include 's1layr.inc'      !aszlyd, nslay
      include 'c1gen.inc'       !ac0rg, acxrow
      include 'c1db1.inc'       !ac0ssa, ac0ssb, acdpop
      include 'h1hydro.inc'     !ahztranspdepth, ahzfurcut, ahztransprtmin, ahztransprtmax

!     + + + ARGUMENT DECLARATIONS + + +
      integer isr
      type(biomatter), intent(inout) :: crop    ! structure containing full crop description
      type(biomatter), dimension(:), intent(inout) :: residue
      type(biototal), intent(inout) :: restot, croptot, biotot
      type(decomp_factors), intent(inout) :: decompfac
      type(opercrop_date), dimension(:), intent(inout) :: mandate
      type(hydro_derived_et), intent(inout) :: h1et
      type(wepp_param), intent(inout) :: wp
      type(subregionsurfacestate), intent(inout) :: subrsurf  ! subregion surface conditions

!     + + + ARGUMENT DEFINITIONS + + +
!     restot          - structure array containing summary residue pool amounts for all subregions

!        write(*,*) "Start manage"      !MANAGEment (tillage) submodel
        call manage(isr, iy, crop, residue, biotot, mandate, h1et, subrsurf)

        if( am0cropupfl.gt.0 ) then
            ! update all derived globals for crop global variables
            call cropupdate( &
     &      subrsurf%aszrgh, aszlyd(1,isr), &
     &      ac0rg(isr), acxrow(isr), &
     &      nslay(isr), ac0ssa(isr), ac0ssb(isr), &
     &      acdpop(isr), &
     &      ahztranspdepth(isr), ahzfurcut(isr), &
     &      ahztransprtmin(isr), ahztransprtmax(isr), crop, croptot  )

            ! dependent variables have been updated
            am0cropupfl = 0
        end if

!        write(*,*) "Start updres"
        call updres(isr, residue, restot)                 !update decomp residue pools

!        write(*,*) "Start callhydr"
        call callhydr(daysim, isr, crop, restot, biotot, h1et, wp, subrsurf)      !call HYDROLOGY submodel
        ! do not change order. Hydro may set irrigation amounts that
        ! will affect soil.

!        write(*,*) "Start callsoil"
        call callsoil(daysim, isr, croptot, biotot, h1et, subrsurf)       !SOIL submodel

!        write(*,*) "Start callcrop"     !CROP submodel
        ! Crop growth flag indicates growing crop
        if( crop%growth%am0cgf ) then
            call callcrop(daysim, isr, crop, residue, restot, croptot, h1et, subrsurf)
        end if

!        write(*,*) "Start decomp"
        call decomp(isr, crop, residue, decompfac, h1et)         !DECOMPosition submodel

!        write(*,*) "Start updres"
        call updres(isr, residue, restot)

!        write(*,*) "Start sumbio"
        call sumbio(isr, crop, residue, restot, croptot, biotot, subrsurf) ! sum live and dead biomass

      return
      end

