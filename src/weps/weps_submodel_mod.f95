!$Author$
!$Date$
!$Revision$
!$HeadURL$

module weps_submodel_mod

  contains

    subroutine submodels (isr, soil, plant, plantIndex, restot, croptot, &
                          biotot, decompfac, mandate, hstate, h1et, h1bal, wp, manFile)

      use weps_main_mod, only: daysim
      use soil_data_struct_defs, only: soil_def
      use biomaterial, only: decomp_factors
      use biomaterial, only: plant_pointer, residue_pointer, biototal
      use input_run_mod, only: iy
      use mandate_mod, only: opercrop_date
      use hydro_data_struct_defs, only: hydro_derived_et, hydro_state
      use hydro_mod, only: callhydr
      use report_hydrobal_mod, only: hydro_balance
      use wepp_param_mod, only: wepp_param
      use soil_mod, only: callsoil
      use crop_mod, only: callcrop
      use decomp_process_mod, only: decomp
      use manage_mod, only: manage
      use manage_data_struct_defs, only: man_file_struct
      use update_mod, only: plantupdate

      ! + + + ARGUMENT DECLARATIONS + + +
      integer isr
      type(soil_def), intent(inout) :: soil     ! soil for this subregion
      type(plant_pointer), pointer :: plant     ! pointer to youngest plant data, which chains to older plant data
      integer, intent(inout) :: plantIndex      ! index used for detailed plant/residue output
      type(biototal), intent(inout) :: restot  ! structure array containing summary amounts for residue biomass
      type(biototal), intent(inout) :: croptot ! structure array containing summary amounts for living plant biomass
      type(biototal), intent(inout) :: biotot  ! structure array containing summary amounts for all biomass
      type(decomp_factors), intent(inout) :: decompfac
      type(opercrop_date), dimension(:), intent(inout) :: mandate
      type(hydro_state), intent(inout) :: hstate
      type(hydro_derived_et), intent(inout) :: h1et
      type(hydro_balance), intent(inout) :: h1bal
      type(wepp_param), intent(inout) :: wp
      type(man_file_struct), intent(inout) :: manFile

      ! write(*,*) "Start manage", daysim

      ! MANAGEment (tillage) submodel
      call manage(isr, iy, soil, plant, plantIndex, biotot, mandate, hstate, h1et, manFile)

      call plantupdate( soil, plant, croptot, restot, biotot )

      ! write(*,*) "Start callhydr", daysim

      ! HYDROLOGY submodel. Do not change call order. Hydro may set irrigation
      ! amounts that will affect soil.
      call callhydr(daysim, isr, soil, plant, croptot, restot, biotot, hstate, h1et, h1bal, wp)

      ! write(*,*) "Start callsoil", daysim

      ! SOIL submodel
      call callsoil(daysim, isr, soil, croptot, biotot, hstate, h1et)

      ! write(*,*) "Start callcrop", daysim

      ! CROP submodel
      call callcrop(daysim, isr, soil, plant, croptot, restot, biotot, h1et)
      ! NOTE: plant update called within callcrop

      ! write(*,*) "Start decomp", daysim

      ! DECOMPosition submodel
      call decomp(isr, soil, plant, decompfac, hstate, h1et)
      call plantupdate( soil, plant, croptot, restot, biotot )

      return
    end subroutine submodels

    subroutine erodsubr_update( sr, soil, plant, biotot, hstate, h1et, subrsurf )

      ! assign all input data for stand alone erosion to subrsurf structure

      use subregions_mod
      use soil_data_struct_defs, only: soil_def
      use biomaterial, only: plant_pointer, residue_pointer, biototal
      use hydro_data_struct_defs, only: hydro_derived_et, hydro_state, hhrs
      use erosion_data_struct_defs, only: subregionsurfacestate, create_brcdinputpools, destroy_brcdinputpools
      use sberod_mod, only: sbsfdall

      !     +++ ARGUMENT DECLARATIONS +++
      integer sr                               ! subregion index (eventually obsolete)
      type(soil_def), intent(in) :: soil  ! soil for this subregion
      type(plant_pointer), pointer :: plant     ! pointer to youngest plant data, which chains to older plant data
      type(biototal), intent(in) :: biotot
      type(hydro_state), intent(in) :: hstate
      type(hydro_derived_et), intent(in) :: h1et
      type(subregionsurfacestate), intent(inout) :: subrsurf  ! subregion surface conditions (erosion specific set)

      !     +++ LOCAL VARIABLES +++
      integer :: idx  ! loop index
      integer :: npools ! number of brcdInput pools
      type(plant_pointer), pointer :: thisPlant       ! pointer used to interate plant pointer chain
      type(residue_pointer), pointer :: thisResidue   ! pointer used to interate residue pointer chain

      !     +++ END SPECIFICATIONS +++

      ! clear out brcdInput values
      if( allocated( subrsurf%brcdInput ) ) then
        call destroy_brcdinputpools(subrsurf)
      end if

      ! count number of pools
      npools = 0      
      ! point to youngest plant
      thisPlant => plant
      do while ( associated(thisPlant) )
        if( (thisPlant%geometry%zht .gt. 0.0 ) .and. ((thisPlant%deriv%rlai .gt. 0.0) .or. (thisPlant%deriv%rsai .gt. 0.0)) ) then
          ! this has biodrag, add to subrsurf
          npools = npools + 1
        end if
        ! point to residue in thisPlant
        thisResidue => thisPlant%residue
        do while (associated(thisResidue))
          if( (thisResidue%zht .gt. 0.0) .and. ((thisResidue%deriv%rlai .gt. 0.0) .or. (thisResidue%deriv%rsai .gt. 0.0)) ) then
            ! this has biodrag, add to subrsurf
            npools = npools + 1
          end if
          ! point to next older residue
          thisResidue => thisResidue%olderResidue
        end do
        ! point to next older plant
        thisPlant => thisPlant%olderPlant
      end do

      ! allocate array for pools
      subrsurf%npools = npools
      call create_brcdinputpools(npools, subrsurf)

      ! insert new values
      npools = 0
      ! point to youngest plant
      thisPlant => plant
      do while ( associated(thisPlant) )

        if( (thisPlant%geometry%zht .gt. 0.0 ) .and. ((thisPlant%deriv%rlai .gt. 0.0) .or. (thisPlant%deriv%rsai .gt. 0.0)) ) then
          ! this has biodrag, add to subrsurf
          npools = npools + 1
          subrsurf%brcdInput(npools)%rlai = thisPlant%deriv%rlai
          subrsurf%brcdInput(npools)%rsai = thisPlant%deriv%rsai
          subrsurf%brcdInput(npools)%rg = thisPlant%geometry%rg
          subrsurf%brcdInput(npools)%xrow = thisPlant%geometry%xrow
          subrsurf%brcdInput(npools)%zht = thisPlant%geometry%zht
        end if

        ! point to residue in thisPlant
        thisResidue => thisPlant%residue
        do while (associated(thisResidue))

          if( (thisResidue%zht .gt. 0.0) .and. ((thisResidue%deriv%rlai .gt. 0.0) .or. (thisResidue%deriv%rsai .gt. 0.0)) ) then
            ! this has biodrag, add to subrsurf
            npools = npools + 1
            subrsurf%brcdInput(npools)%rlai = thisResidue%deriv%rlai
            subrsurf%brcdInput(npools)%rsai = thisResidue%deriv%rsai
            subrsurf%brcdInput(npools)%rg = thisPlant%geometry%rg
            subrsurf%brcdInput(npools)%xrow = thisPlant%geometry%xrow
            subrsurf%brcdInput(npools)%zht = thisResidue%zht
          end if

          ! point to next older residue
          thisResidue => thisResidue%olderResidue
        end do

        ! point to next older plant
        thisPlant => thisPlant%olderPlant
      end do

      subrsurf%abffcv = biotot%ffcvtot
      subrsurf%asfcr = soil%asfcr
      subrsurf%aszcr = soil%aszcr
      subrsurf%asflos = soil%asflos
      subrsurf%asmlos = soil%asmlos
      subrsurf%asdcr = soil%asdcr
      subrsurf%asecr = soil%asecr
      subrsurf%aslrr = soil%aslrr
      subrsurf%aszrgh = soil%aszrgh
      subrsurf%asxrgs = soil%asxrgs
      subrsurf%asxrgw = soil%asxrgw
      subrsurf%asargo = soil%asargo

      do idx = 1, soil%nslay

        subrsurf%bsl(idx)%aszlyt = soil%aszlyt(idx)
        subrsurf%bsl(idx)%asdblk = soil%asdblk(idx)
        subrsurf%bsl(idx)%asfsan = soil%asfsan(idx)
        subrsurf%bsl(idx)%asfvfs = soil%asfvfs(idx)
        subrsurf%bsl(idx)%asfsil = soil%asfsil(idx)
        subrsurf%bsl(idx)%asfcla = soil%asfcla(idx)
        subrsurf%bsl(idx)%asvroc = soil%asvroc(idx)
        subrsurf%bsl(idx)%asdagd = soil%asdagd(idx)
        subrsurf%bsl(idx)%aseags = soil%aseags(idx)
        subrsurf%bsl(idx)%aslagm = soil%aslagm(idx)
        subrsurf%bsl(idx)%aslagn = soil%aslagn(idx)
        subrsurf%bsl(idx)%aslagx = soil%aslagx(idx)
        subrsurf%bsl(idx)%as0ags = soil%as0ags(idx)

        subrsurf%bsl(idx)%ahrwcw = soil%ahrwcw(idx)
        subrsurf%bsl(idx)%ahrwca = soil%ahrwca(idx)

      end do

      subrsurf%ahzsnd = h1et%zsnd

      do idx = 1, hhrs
        subrsurf%ahrwc0(idx) = hstate%rwc0(idx)
      end do

      ! derived
      subrsurf%abrsai = biotot%rsaitot
      subrsurf%abrlai = biotot%rlaitot
      subrsurf%abzht = biotot%zht_ave
      ! real :: sxprg      ! sxprg  - ridge spacing parallel the wind direction(mm)

      ! updates acanag, acancr, asf10an, asf10en, asf10bk, 
      ! sfd1, sfd10, sfd84, sfd200, sf1ic, sf10ic, sf84ic, sf200ic
      ! for reporting in plot.out
      call sbsfdall( subrsurf )

      return
    end subroutine erodsubr_update

end module weps_submodel_mod

