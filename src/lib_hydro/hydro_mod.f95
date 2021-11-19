!$Author$
!$Date$
!$Revision$
!$HeadURL$

module hydro_mod

  contains

    subroutine hydrinit(isr, soil, hstate, h1et, h1bal, wp)

      use datetime_mod, only: get_psim_daysim
      use hydro_wepp_util_mod, only: saxpar
      use soil_data_struct_defs, only: soil_def
      use hydro_data_struct_defs, only: hydro_derived_et, hydro_state, hhrs
      use report_hydrobal_mod, only: hydro_balance
      use wepp_param_mod, only: wepp_param

!     + + + ARGUMENT DECLARATIONS + + +
      integer isr
      type(soil_def), intent(inout) :: soil  ! soil for this subregion
      type(hydro_state), intent(inout) :: hstate
      type(hydro_derived_et), intent(inout) :: h1et
      type(hydro_balance), intent(inout) :: h1bal
      type(wepp_param), intent(inout) :: wp

      integer idx
      real ltheta(soil%nslay)
      real sand(soil%nslay),clay(soil%nslay),orgmat(soil%nslay)

      do idx = 1, soil%nslay
        ! Initialize the water holding capacity variable
        soil%ahrwca(idx) = soil%ahrwcf(idx) - soil%ahrwcw(idx)
        ! set volumetric water content to initialize reporting variable
        ltheta(idx) = soil%ahrwc(idx) * soil%asdblk(idx)
      end do

      ! Set infiltration water depth to 0.0
      hstate%zwid = 0.0
      hstate%zeasurf = 0.0

      ! soil layer temperature, ice fraction
      do idx = 1, soil%nslay
          soil%tsav(idx) = 0.0
          soil%fice(idx) = 0.0
      end do

      hstate%zsno = 0.0
      hstate%tsno = 0.0
      hstate%fsnfrz = 0.0

      ! set hydrologic balance variables
      h1bal%initswc = dot_product(ltheta(1:soil%nslay),                 &
     &                            soil%aszlyt(1:soil%nslay))
      h1bal%initsnow = hstate%zsno
      h1bal%initday = get_psim_daysim(isr)

      h1bal%presswc = h1bal%initswc
      h1bal%pressnow = h1bal%initsnow
      h1bal%presday = h1bal%initday

      h1bal%cumprecip = 0.0
      h1bal%cumirrig = 0.0
      h1bal%cumrunoff = 0.0
      h1bal%cumevap = 0.0
      h1bal%cumtrans = 0.0
      h1bal%cumdrain = 0.0
      h1bal%hprevrotation = 1

!     Initialize irrigation type and depth so values are set if no
!     irrigation processes are invoked
      h1et%zirr = 0.0
      hstate%ratirr = 0.0
      hstate%durirr = 0.0
      hstate%locirr = 0.0
      hstate%monirr = 0
      hstate%madirr = 0.0
      hstate%minirr = 0.0
      hstate%ndayirr = 0
      hstate%mintirr = 0

      h1et%zrun = 0.0
      hstate%zsmt = 0.0
      h1et%zper = 0.0

      do idx = 1, soil%nslay
          soil%tsmx(idx) = 0.0
          soil%tsmn(idx) = 0.0
      end do

      do idx = 1, soil%nslay
          ! Soil layer sand content (Mg/Mg)
          sand(idx) = soil%asfsan(idx)
          ! Soil layer clay content (Mg/Mg)		
          clay(idx) = soil%asfcla(idx)
          ! Soil layer organic matter content (Mg/Mg)		
          orgmat(idx) = soil%asfom(idx)
      end do
	
      call saxpar(sand,clay,orgmat,soil%nslay,wp%saxwp,wp%saxfc,        &
     &    wp%saxenp,wp%saxpor,wp%saxA, wp%saxB,wp%saxks)

      ! Added for WEPP bookeeping      
      wp%totalPrecip = 0.0
      wp%totalRunoff = 0.0
      wp%precipEvents = 0
      wp%runoffEvents = 0
      wp%snowmeltEvents = 0
      wp%totalSnowrunoff = 0.0
      wp%prev_crust_frac = -1.0
      wp%rkecum = 0.0
      ! End WEPP addition      

      ! initializing a previously un-init'd variable
      h1et%zea = 0.0
      h1et%zep = 0.0
      h1et%zeta = 0.0
      h1et%zetp = 0.0
      h1et%zpta = 0.0
      h1et%zptp = 0.0
      h1et%zsnd = 0.0
      h1et%snow_protect = 0.0

      do idx = 1, hhrs
        hstate%rwc0(idx) = 0.0
      end do

      return
    end subroutine hydrinit

    subroutine callhydr(daysim, isr, soil, plant, croptot, restot, biotot, hstate, h1et, h1bal, wp)

! ***************************************************************** wjr
! Wrapper to call hydro

      use hydro_main_mod, only: hydro
      use soil_data_struct_defs, only: soil_def
      use biomaterial, only: plant_pointer, residue_pointer, biototal
      use hydro_data_struct_defs, only: am0hdb, hydro_derived_et, hydro_state
      use report_hydrobal_mod, only: hydro_balance
      use wepp_param_mod, only: wepp_param
      use climate_input_mod, only: amzele

      !  + + + ARGUMENT DECLARATIONS + + +
      integer daysim
      integer isr                   
      type(soil_def), intent(inout) :: soil  ! soil for this subregion
      type(plant_pointer), pointer :: plant  ! pointer to youngest plant data, which chains to older plant data
      type(biototal), intent(in) :: croptot  ! structure containing summary crop pool amounts
      type(biototal), intent(in) :: restot   ! structure containing summary residue pool amounts
      type(biototal), intent(in) :: biotot
      type(hydro_state), intent(inout) :: hstate
      type(hydro_derived_et), intent(inout) :: h1et
      type(hydro_balance), intent(inout) :: h1bal
      type(wepp_param), intent(inout) :: wp

      !  + + + ARGUMENT DEFINITIONS + + +
      !  restot          - structure array containing summary residue pool amounts for all subregions

      if (am0hdb(isr) .eq. 1) then
         call hdbug(.false., isr, soil, croptot, restot, hstate, h1et)
      end if

      call hydro( isr, biotot%rcdtot, biotot%zht_ave, &
                 biotot%rlaitot, croptot%zmht, croptot%dayap, &
                 biotot%ftcancov, biotot%rlailive, &
                 biotot%mftot, biotot%evapredu, &
                 biotot%dstmtot, biotot%ffcvtot, &
                 amzele, &
                 hstate%zdmaxirr, hstate%ratirr, hstate%durirr, &
                 hstate%locirr, hstate%minirr, hstate%monirr, &
                 hstate%madirr, hstate%ndayirr, hstate%mintirr, &
                 hstate%zoutflow, hstate%zinf, &
                 hstate%zsno, hstate%tsno, hstate%fsnfrz, &
                 hstate%zsmt, &
                 hstate%rwc0, daysim, &
                 hstate%zwid, &
                 hstate%zeasurf, &
                 soil, plant, h1et, h1bal, wp )

      if (am0hdb(isr) .eq. 1) then
         call hdbug(.true., isr, soil, croptot, restot, hstate, h1et)
      end if

    end subroutine callhydr

    subroutine  hdbug(aflg, isr, soil, croptot, restot, hstate, h1et)

      ! This program prints out many of the global variables before
      ! and after the call to HYDROLOGY provide a comparison of values
      ! which may be changed by HYDDROLOGY

      use datetime_mod, only: get_psim_doy, get_psim_year, get_psim_juld
      use file_io_mod, only: luohdb
      use soil_data_struct_defs, only: soil_def
      use biomaterial, only: biototal
      use hydro_data_struct_defs, only: hydro_derived_et, hydro_state
      use climate_input_mod, only: cli_day, wind_day

      !  + + + ARGUMENT DECLARATIONS + + +
      logical, intent(in) :: aflg   ! False, before call to hydro, True, after call to hydro
      integer :: isr  ! subregion index.
      type(soil_def), intent(in) :: soil     ! soil for this subregion
      type(biototal), intent(in) :: croptot  ! structure containing summary crop pool amounts
      type(biototal), intent(in) :: restot   ! structure containing summary residue pool amounts
      type(hydro_state), intent(in) :: hstate
      type(hydro_derived_et), intent(in) :: h1et

      !  + + + LOCAL VARIABLES + + +
      integer :: l   ! index on soil layers
      integer :: pjuld  ! present julian day

      !  + + + DATA INITIALIZATIONS + + +

      pjuld = get_psim_juld(isr)
      
      !  + + + OUTPUT FORMATS + + +
 2030 format ('**',1x,'Day ',i2,'of Year ',i4,'    After  call to HYDRO          Subregion No. ',i3)
 2031 format ('**',1x,'Day ',i2,'of Year ',i4,'    Before call to HYDRO          Subregion No. ',i3)
 2032 format (' cli_day%zdpt  cli_day%tdmx  cli_day%tdmn  cli_day%eirr  awudmx  awudmn ', &
              ' cli_day%tdpt  awadir  awhrmx ')
 2038 format (f7.2,9f8.2)
! 2045 format ('Subregion Number',i3)
 2050 format ('amrslp(',i2,') crop%ftcvtot(',i2,') croptot%rlaitot(',i2,')', &
              ' croptot%zrtd(',i2,') restot%mftot(',i2,') ahfwsf(',i2,')', &
              ' ahzper(',i2,')')
 2051 format (2f10.2,2f10.5,2x,f10.2,f10.2,f12.2)
 2052 format ('ahzrun(',i2,') h1et%zirr(',i2,') ahzsno(',i2,')', &
              ' ahzsmt(',i2,')  h1et%zeta      h1et%zetp     ', &
              ' h1et%zpta ')
 2053 format (5f10.2,2f12.2)
 2054 format ('      h1et%zea     h1et%zep    h1et%zptp   aslrr(',i2,')')
 2055 format (2f10.2,2f10.3,3f12.2)
 2056 format('layer aszlyt  ahrsk ahrwc ahrwcs ahrwca', &
             ' ahrwcf ahrwcw ah0cb aheaep ahtsmx ahtsmn')
 2060 format (i4,1x,f7.2,1x,e7.1,f6.2,4f7.2,f6.2,3f7.2)
 2065 format(' layer  asfsan asfsil asfcla asfom asdblk aslagm  as0ags', &
             ' aslagn  aslagx  aseags')
 2070 format (i4,2x,3f7.2,f7.3,2f7.2,f8.2,f7.3,2f8.2)

      !  + + + END SPECIFICATIONS + + +

      !  write weather cligen and windgen variables
      if( aflg ) then
         write(luohdb(isr),2030) get_psim_doy(isr),get_psim_year(isr),isr
      else
         write(luohdb(isr),2031) get_psim_doy(isr),get_psim_year(isr),isr
      end if
      write(luohdb(isr),2032)
      write(luohdb(isr),2038) cli_day(pjuld)%zdpt, cli_day(pjuld)%tdmx, cli_day(pjuld)%tdmn, cli_day(pjuld)%eirr, &
                              wind_day(pjuld)%wwudmx, wind_day(pjuld)%wwudmn, cli_day(pjuld)%tdpt, &
                              wind_day(pjuld)%wwadir, wind_day(pjuld)%wwhrmx

      !   write(luohdb(isr),2045) isr

      write(luohdb(isr),2050) isr, isr, isr, isr, isr, isr, isr
      write(luohdb(isr),2051) soil%amrslp, croptot%ftcvtot, croptot%rlaitot, &
                    restot%mftot, h1et%zper
      write(luohdb(isr),2052) isr, isr, isr, isr
      write(luohdb(isr),2053) h1et%zrun, h1et%zirr, hstate%zsno, &
                    hstate%zsmt, h1et%zeta, h1et%zetp, h1et%zpta
      write(luohdb(isr),2054) isr, isr, isr, isr
      write(luohdb(isr),2055) h1et%zea, h1et%zep, h1et%zptp, soil%aslrr
      write(luohdb(isr),2056)

      do l = 1,soil%nslay
         write(luohdb(isr),2060) l, soil%aszlyt(l), soil%ahrsk(l), soil%ahrwc(l), &
                       soil%ahrwcs(l), soil%ahrwca(l), soil%ahrwcf(l), &
                       soil%ahrwcw(l), soil%ah0cb(l), soil%aheaep(l), &
                       soil%tsmx(l), soil%tsmn(l)
      end do
      write(luohdb(isr),2065)

      do l=1,soil%nslay
         write(luohdb(isr),2070) l, soil%asfsan(l), soil%asfsil(l), &
                       soil%asfcla(l), soil%asfom(l), soil%asdblk(l), &
                       soil%aslagm(l), soil%as0ags(l), soil%aslagn(l), &
                       soil%aslagx(l), soil%aseags(l)
      end do

      return
    end subroutine  hdbug

end module hydro_mod

