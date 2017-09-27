!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine callhydr(daysim, isr, soil, crop, restot, biotot, h1et, h1bal, wp)

! ***************************************************************** wjr
! Wrapper to call hydro

      use weps_interface_defs, only: hdbug, hydro
      use soil_data_struct_defs, only: soil_def
      use biomaterial, only: biototal, biomatter
      use timer_mod, only: timer, TIMHYDR, TIMSTART, TIMSTOP
      use erosion_data_struct_defs, only: awudav
      use hydro_data_struct_defs, only: am0hdb, hydro_derived_et
      use report_hydrobal_mod, only: hydro_balance
      use wepp_param_mod, only: wepp_param
      use climate_input_mod, only: amzele

!     + + + ARGUMENT DECLARATIONS + + +
      integer daysim
      integer isr                   
      type(soil_def), intent(in) :: soil  ! soil for this subregion
      type(biomatter), intent(in) :: crop
      type(biototal), intent(in) :: restot
      type(biototal), intent(in) :: biotot
      type(hydro_derived_et), intent(inout) :: h1et
      type(hydro_balance), intent(inout) :: h1bal
      type(wepp_param), intent(inout) :: wp

!     + + + ARGUMENT DEFINITIONS + + +
!     restot          - structure array containing summary residue pool amounts for all subregions

! Includes
      include 'p1werm.inc'
      include 'h1hydro.inc'
      include 'h1temp.inc'
      include 'h1db1.inc'

      call timer(TIMHYDR,TIMSTART)      

      if (am0hdb(isr) .eq. 1) then
         call hdbug(isr, soil, crop, restot, h1et)
      end if

      call hydro( isr, soil%nslay, soil%amrslp, biotot%zht_ave,         &
     &            crop%deriv%rlai, crop%deriv%rsai, crop%geometry%zht, crop%growth%dayap,   &
     &       crop%geometry%xrow, crop%geometry%rg, biotot%ftcancov, crop%growth%fliveleaf,&
     &         biotot%mftot, biotot%evapredu, crop%geometry%zrtd, ahfwsf(isr), &
     &            soil%aszlyd, soil%asdblk, soil%asdblk0, &
     &            soil%asdpart, soil%asdwblk, soil%ahrwc, &
     &            soil%ahrwcdmx, soil%ahrwcs, soil%ahrwcf, &
     &            soil%ahrwcw, soil%ahrwcr, soil%ahrwca, &
     &            soil%ah0cb, soil%aheaep, soil%ahfredsat, &
     &            soil%asfsan, soil%asfsil, soil%asfcla, &
     &            soil%asvroc, soil%asfom, soil%asfcec, &
     &            ahtsav(1,isr), biotot%dstmtot, biotot%ffcvtot,        &
     &            soil%asxrgs, soil%aszrgh, soil%asfcr, &
                  soil%aslrro, soil%aslrr, amzele, &
     &            ahzdmaxirr(isr), ahratirr(isr), ahdurirr(isr),        &
     &            ahlocirr(isr), ahminirr(isr), am0monirr(isr),         &
     &            ahmadirr(isr), ahndayirr(isr), ahmintirr(isr),        &
     &            ahzoutflow(isr), ahzinf(isr),            &
     &            ahzsno(isr), ahtsno(isr), ahfsnfrz(isr), &
     &            ahzsmt(isr), ahfice(1, isr), soil%ahrsk, &
     &            ahtsmx(1, isr), ahtsmn(1, isr),                       &
     &            ahrwc0(1, isr), daysim,                               &
     &            soil%asfald, soil%asfalw, soil%aszlyt, &
     &            awudav, ahzwid(isr),                  &
     &            ahzeasurf(isr),                                       &
     &            crop%deriv%ztranspdepth, restot, h1et, h1bal, wp )

! removed from call: ah0cng(isr), ah0cnp(isr), 
!                 initswc(isr), initsnow(isr), initday(isr)

      if (am0hdb(isr) .eq. 1) then
         call hdbug(isr, soil, crop, restot, h1et)
      end if
      call timer(TIMHYDR,TIMSTOP)      

      end
