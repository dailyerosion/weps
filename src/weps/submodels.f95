!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine submodels (isr, soil, crop, cropprev, residue, restot, croptot, &
     &                      biotot, decompfac, mandate, h1et, h1bal, wp, manFile)

      use weps_main_mod, only: daysim
      use weps_interface_defs, ignore_me=>submodels
      use soil_data_struct_defs, only: soil_def
      use biomaterial, only: biomatter, biototal, decomp_factors, bio_prevday
      use input_run_mod, only: iy
      use mandate_mod, only: opercrop_date
      use hydro_data_struct_defs, only: hydro_derived_et
      use report_hydrobal_mod, only: hydro_balance
      use wepp_param_mod, only: wepp_param
      use soil_mod, only: callsoil
      use crop_mod, only: callcrop
      use manage_mod, only: manage
      use manage_data_struct_defs, only: man_file_struct

      include 'p1werm.inc'
      include 'm1flag.inc'      !am0cropupfl
      include 'h1hydro.inc'     !ahztranspdepth, ahzfurcut, ahztransprtmin, ahztransprtmax

!     + + + ARGUMENT DECLARATIONS + + +
      integer isr
      type(soil_def), intent(inout) :: soil     ! soil for this subregion
      type(biomatter), intent(inout) :: crop    ! structure containing full crop description
      type(bio_prevday), intent(inout) :: cropprev
      type(biomatter), dimension(:), intent(inout) :: residue
      type(biototal), intent(inout) :: restot, croptot, biotot
      type(decomp_factors), intent(inout) :: decompfac
      type(opercrop_date), dimension(:), intent(inout) :: mandate
      type(hydro_derived_et), intent(inout) :: h1et
      type(hydro_balance), intent(inout) :: h1bal
      type(wepp_param), intent(inout) :: wp
      type(man_file_struct), intent(inout) :: manFile

!     + + + ARGUMENT DEFINITIONS + + +
!     restot          - structure array containing summary residue pool amounts for all subregions

!        write(*,*) "Start manage"      !MANAGEment (tillage) submodel
        call manage(isr, iy, soil, crop, cropprev, residue, biotot, mandate, h1et, manFile)

        if( am0cropupfl.gt.0 ) then
            ! update all derived globals for crop global variables
            call cropupdate( &
     &      soil%aszrgh, soil%aszlyd, &
     &      crop%geometry%rg, crop%geometry%xrow, &
     &      soil%nslay, crop%database%ssa, crop%database%ssb, &
     &      crop%geometry%dpop, &
     &      ahztranspdepth(isr), ahzfurcut(isr), &
     &      ahztransprtmin(isr), ahztransprtmax(isr), crop, croptot  )

            ! dependent variables have been updated
            am0cropupfl = 0
        end if

!        write(*,*) "Start updres"
        call updres(soil, residue, restot)                 !update decomp residue pools

!        write(*,*) "Start callhydr"
        call callhydr(daysim, isr, soil, crop, restot, biotot, h1et, h1bal, wp)      !call HYDROLOGY submodel
        ! do not change order. Hydro may set irrigation amounts that
        ! will affect soil.

!        write(*,*) "Start callsoil"
        call callsoil(daysim, isr, soil, croptot, biotot, h1et)       !SOIL submodel

!        write(*,*) "Start callcrop"     !CROP submodel
        ! Crop growth flag indicates growing crop
        if( crop%growth%am0cgf ) then
            call callcrop(daysim, isr, soil, crop, cropprev, residue, restot, croptot, h1et)
        end if

!        write(*,*) "Start decomp"
        call decomp(isr, soil, crop, residue, decompfac, h1et)         !DECOMPosition submodel

!        write(*,*) "Start updres"
        call updres(soil, residue, restot)

!        write(*,*) "Start sumbio"
        call sumbio(soil, crop, residue, restot, croptot, biotot) ! sum live and dead biomass

      return
      end

