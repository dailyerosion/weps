!$Author$
!$Date$
!$Revision$
!$HeadURL$

module update_mod

  type evap_redu
    real :: flatmass
    real :: evapredu_a
    real :: evapredu_b
  end type evap_redu

  real, parameter :: minimum_res = 0.001 ! (kg/m^2) ie. 0.001 = 1 gram/m^2

  logical :: am0cropupfl  ! flag to determine that the crop state has been changed
                          ! external to crop and that the crop update process must
                          ! run to synchronize dependent variable values with state values
                          ! .true. - update crop dependent
                          ! .false. - update not necessary due to mangement operations

  contains

    subroutine plantupdate( soil, plant, croptot, restot, biotot )

      ! + + + PURPOSE + + +
      ! calculates values of derived variables based on the present values
      ! or the state variables. The derived variables are commonly used
      ! where residue totals are required.

      use biomaterial, only: plant_pointer, residue_pointer, biototal, ncanlay, plantDestroy, residueDestroy
      use p1unconv_mod, only: pi
      use wind_mod, only: biodrag
      use crop_growth_mod, only: ht_dia_sai
      use soil_data_struct_defs, only: soil_def

      use weps_main_mod, only: daysim

!     + + +   ARGUMENT DECLARATIONS + + +
      type(soil_def), intent(in) :: soil  ! soil for this subregion
      type(plant_pointer), pointer :: plant     ! pointer to youngest plant data, which chains to older plant data
      type(biototal), intent(inout) :: croptot  ! structure containing living crop derived variables
      type(biototal), intent(inout) :: restot   ! structure containing residue derived variables
      type(biototal), intent(inout) :: biotot   ! structure containing all biomass crop derived variables

      ! LOCAL VARIABLES
      type(plant_pointer), pointer :: thisPlant       ! pointer used to interate plant pointer chain
      type(residue_pointer), pointer :: thisResidue   ! pointer used to interate residue pointer chain
      type(plant_pointer), pointer :: parentPlant     ! retain parent pointer to update on plantDestroy
      type(residue_pointer), pointer :: parentResidue ! retain parent pointer to update on residueDestroy
      real temp1, temp2
      integer :: idx  ! indexing variable
      integer :: jdx  ! indexing variable
      real :: atotal  ! total used in weighting

      integer :: iplt ! plant index
      integer :: ires ! residue pool index
      integer :: iallres ! all residue pools index

      integer :: pool_cnt ! total number of biomass pools, crops and residue
      type(evap_redu), dimension(:), allocatable :: evapredu  ! derived total flat mass and evapredu params, pools, 'youngest to oldest'
      integer :: alloc_stat

      ! parameter to control depth of slice
      real scidepth
      parameter( scidepth = 101.6 ) ! mm 101.6 = 4 inches for SCI
      real weppdepth
      parameter( weppdepth = 150.0 ) ! mm 150.0 = 5.9 inches for WEPP

      ! function declarations
      real valbydepth
      real transpdepth

      temp1 = 0.0
      temp2 = 0.0
      iplt = 0
      iallres = 0

      ! point to youngest plant
      thisPlant => plant
      nullify(parentPlant)

      ! bring all derived variables up to date
      do while ( associated(thisPlant) )

        ! Living plant material section
        iplt = iplt + 1

        if( thisPlant%growth%am0cgf .or. am0cropupfl ) then
          ! plant growing so update derived variables (if not growing should be residue)
          am0cropupfl = .false.

          ! accumulate layer values into root mass totals
          ! NOTE: mbgleaf and mbgstore are set to zero and not updated for live plant
          thisPlant%deriv%mbgstem = 0.0
          thisPlant%deriv%mbgrootstore = 0.0
          thisPlant%deriv%mbgrootfiber = 0.0
          do idx = 1, soil%nslay
            thisPlant%deriv%mbgstem = thisPlant%deriv%mbgstem + thisPlant%mass%stemz(idx)
            thisPlant%deriv%mbgrootstore = thisPlant%deriv%mbgrootstore + thisPlant%mass%rootstorez(idx)
            thisPlant%deriv%mbgrootfiber = thisPlant%deriv%mbgrootfiber + thisPlant%mass%rootfiberz(idx)
          end do

          thisPlant%deriv%mst = thisPlant%mass%standstem + thisPlant%mass%standleaf + thisPlant%mass%standstore
          thisPlant%deriv%mf = thisPlant%mass%flatstem + thisPlant%mass%flatleaf + thisPlant%mass%flatstore
          thisPlant%deriv%mrt = thisPlant%deriv%mbgstem + thisPlant%deriv%mbgrootstore + thisPlant%deriv%mbgrootfiber
          thisPlant%deriv%m = thisPlant%deriv%mst + thisPlant%deriv%mf + thisPlant%deriv%mrt

          do idx = 1, soil%nslay
            thisPlant%deriv%mrtz(idx) = thisPlant%mass%rootstorez(idx) + thisPlant%mass%rootfiberz(idx)
            thisPlant%deriv%mbgz(idx) = thisPlant%mass%stemz(idx) + thisPlant%mass%leafz(idx) + thisPlant%mass%storez(idx)
          end do
          ! sum root and below ground from layer values to the SCI depth (4 inches)
          thisPlant%deriv%dmrtto4 = valbydepth(soil%nslay, soil%aszlyd, thisPlant%deriv%mrtz, 2, 0.0, scidepth)
          thisPlant%deriv%dmbgto4 = valbydepth(soil%nslay, soil%aszlyd, thisPlant%deriv%mbgz, 2, 0.0, scidepth)
          ! sum root and below ground from layer values to the WEPP adjustment depth (15 cm)
          thisPlant%deriv%dmrtto15 = valbydepth(soil%nslay, soil%aszlyd, thisPlant%deriv%mrtz, 2, 0.0, weppdepth)
          thisPlant%deriv%dmbgto15 = valbydepth(soil%nslay, soil%aszlyd, thisPlant%deriv%mbgz, 2, 0.0, weppdepth)

          ! calculate new stem area index and representative stem diameter
          call ht_dia_sai( thisPlant%geometry%dpop, thisPlant%mass%standstem, temp1, &
                         thisPlant%database%ssa, thisPlant%database%ssb, thisPlant%geometry%dstm, &
                         thisPlant%geometry%zht, temp2, thisPlant%geometry%xstmrep, thisPlant%deriv%rsai )

          ! leaf area index for standing material
          ! m^2 leaf/kg * kg/m^2 ground = m^2 leaf/m^2 ground
          thisPlant%deriv%rlai = thisPlant%database%sla * thisPlant%mass%standleaf

          ! set stem and leaf area by plant height increments
          ! these are divided equally for a first approximation
          do idx = 1, ncanlay
            thisPlant%deriv%rsaz(idx) = thisPlant%deriv%rsai / ncanlay
            thisPlant%deriv%rlaz(idx) = thisPlant%deriv%rlai / ncanlay
          end do

          ! effective silhouette
          thisPlant%deriv%rcd = biodrag(0.0, 0.0, thisPlant%deriv%rlai, thisPlant%deriv%rsai, &
                     thisPlant%geometry%rg, thisPlant%geometry%xrow, thisPlant%geometry%zht, soil%aszrgh)
          ! surface cover
          thisPlant%deriv%ffcv = 1.0 - exp( -thisPlant%database%covfact * thisPlant%deriv%mf )
          thisPlant%deriv%fscv = thisPlant%geometry%dstm * pi * (thisPlant%database%xstm/2.0)*(thisPlant%database%xstm/2.0)
          if (thisPlant%deriv%fscv > 1.0) thisPlant%deriv%fscv = 1.0
          thisPlant%deriv%ftcv = thisPlant%deriv%fscv + thisPlant%deriv%ffcv !no overlap
          if (thisPlant%deriv%ftcv > 1.0) thisPlant%deriv%ftcv = 1.0
          ! crop leaf interception area (canopy cover)
          thisPlant%deriv%fcancov = 1.0 - exp( - thisPlant%database%ck * thisPlant%deriv%rlai)
          ! transpiration depth as a function of furrow cut depth and root depth
          thisPlant%deriv%ztranspdepth = transpdepth(thisPlant%geometry%zrtd, thisPlant%geometry%zfurcut, &
                                         thisPlant%geometry%ztransprtmin, thisPlant%geometry%ztransprtmax)
        end if

        ! point to newest residue for thisPlant
        ires = 0
        thisResidue => thisPlant%residue
        nullify(parentResidue)

        ! Dead plant material section
        do while (associated(thisResidue))

          ires = ires + 1
          iallres = iallres + 1

          ! accumulate layer values into pool mass totals
          thisResidue%deriv%mbgstem = 0.0
          thisResidue%deriv%mbgleaf = 0.0
          thisResidue%deriv%mbgstore = 0.0
          thisResidue%deriv%mbgrootstore = 0.0
          thisResidue%deriv%mbgrootfiber = 0.0
          do idx = 1, soil%nslay
              thisResidue%deriv%mbgstem = thisResidue%deriv%mbgstem + thisResidue%stemz(idx)
              thisResidue%deriv%mbgleaf = thisResidue%deriv%mbgleaf + thisResidue%leafz(idx)
              thisResidue%deriv%mbgstore = thisResidue%deriv%mbgstore + thisResidue%storez(idx)
              thisResidue%deriv%mbgrootstore = thisResidue%deriv%mbgrootstore + thisResidue%rootstorez(idx)
              thisResidue%deriv%mbgrootfiber = thisResidue%deriv%mbgrootfiber + thisResidue%rootfiberz(idx)
          end do
          ! sum buried root and residue masses for each layer and each pool
          do idx = 1, soil%nslay
              thisResidue%deriv%mrtz(idx) = thisResidue%rootstorez(idx) + thisResidue%rootfiberz(idx)
              thisResidue%deriv%mbgz(idx) = thisResidue%stemz(idx) + thisResidue%leafz(idx) + thisResidue%storez(idx)
          end do

          ! sum root and below ground from layer values for each pool
          thisResidue%deriv%mrt = 0.0
          thisResidue%deriv%mbg = 0.0
          do idx = 1, soil%nslay
              thisResidue%deriv%mrt = thisResidue%deriv%mrt + thisResidue%deriv%mrtz(idx)
              thisResidue%deriv%mbg = thisResidue%deriv%mbg + thisResidue%deriv%mbgz(idx)
          end do

          ! sum above ground totals
          thisResidue%deriv%mst = thisResidue%standstem + thisResidue%standleaf + thisResidue%standstore
          thisResidue%deriv%mf = thisResidue%flatstem + thisResidue%flatleaf + thisResidue%flatstore &
                                + thisResidue%flatrootstore + thisResidue%flatrootfiber
          thisResidue%deriv%m = thisResidue%deriv%mst + thisResidue%deriv%mf + thisResidue%deriv%mrt + thisResidue%deriv%mbg
          ! sum root and below ground from layer values to the SCI depth (4 inches)
          thisResidue%deriv%dmrtto4 = valbydepth(soil%nslay, soil%aszlyd, thisResidue%deriv%mrtz, 2, 0.0, scidepth)
          thisResidue%deriv%dmbgto4 = valbydepth(soil%nslay, soil%aszlyd, thisResidue%deriv%mbgz, 2, 0.0, scidepth)
          ! sum root and below ground from layer values to the WEPP adjustment depth (15 cm)
          thisResidue%deriv%dmrtto15 = valbydepth(soil%nslay, soil%aszlyd, thisResidue%deriv%mrtz, 2, 0.0, weppdepth)
          thisResidue%deriv%dmbgto15 = valbydepth(soil%nslay, soil%aszlyd, thisResidue%deriv%mbgz, 2, 0.0, weppdepth)

          ! calculate residue stem area index (plants/m^2 ground) * m * m/plant = m^2 stem / m^2 ground
          thisResidue%deriv%rsai = thisResidue%dstm * thisResidue%zht * thisResidue%xstmrep
          ! leaf area index for standing material, m^2 leaf/kg * kg/m^2 ground = m^2 leaf/m^2 ground
          thisResidue%deriv%rlai = thisPlant%database%sla * thisResidue%standleaf
          ! effective silhouette
          thisResidue%deriv%rcd = biodrag(thisResidue%deriv%rlai, thisResidue%deriv%rsai, 0.0, 0.0, 0, 0.0, 0.0, 0.0)

          ! set stem and leaf area by plant height increments
          ! these are divided equally for now, until development of plant growth method to do this
          do idx = 1, ncanlay
              thisResidue%deriv%rsaz(idx) = thisResidue%deriv%rsai / ncanlay
              thisResidue%deriv%rlaz(idx) = thisResidue%deriv%rlai / ncanlay
          end do

          ! cover from flat mass estimated using Gregory, 1982. Trans. ASAE 25:1333-1337
          ! fraction (m2/m2) modified to take overlap into account.
          thisResidue%deriv%ffcv = 1.0 - exp( -thisPlant%database%covfact * thisResidue%deriv%mf )
          ! cover from standing stems  !!! should this really use geometry%xstmrep for consistency ???
          thisResidue%deriv%fscv = thisResidue%dstm * pi * ( thisResidue%xstmrep/2.0 )**2.0
          if (thisResidue%deriv%fscv > 1.0) thisResidue%deriv%fscv = 1.0
          ! total cover (flat + standing)
          thisResidue%deriv%ftcv = thisResidue%deriv%ffcv + thisResidue%deriv%fscv !no overlap
          if (thisResidue%deriv%ftcv > 1.0) thisResidue%deriv%ftcv = 1.0
          ! residue leaf interception area (canopy cover)
          thisResidue%deriv%fcancov = 1.0 - exp( - thisPlant%database%ck * thisResidue%deriv%rlai)

          ! header
          !write(*, &
          !  "('  #p  #r #ar #STstm mST_ST mST_LF mST_RP cumDDS lay massST massLF massRP massRS massRF cumDDF/G')")

          ! first line
          !idx = 0
          !write(*,"(3(1x,i3),4f7.3,f9.3,1x,i3,5f7.3,f9.3)") iplt, ires, iallres, thisResidue%dstm, &
          !  thisResidue%standstem, thisResidue%standleaf, thisResidue%standstore, thisResidue%cumdds, &
          !  idx, thisResidue%flatstem, thisResidue%flatleaf, thisResidue%flatstore, &
          !  thisResidue%flatrootstore, thisResidue%flatrootfiber, thisResidue%cumddf
          ! soil layers
          !do idx = 1, soil%nslay
          !   write(*,"(50x,i3,5f7.3,f9.3)") idx, &
          !     thisResidue%stemz(idx), thisResidue%leafz(idx), thisResidue%storez(idx), &
          !     thisResidue%rootstorez(idx), thisResidue%rootfiberz(idx), thisResidue%cumddg(idx)
          ! end do

          ! check if residue is gone
          if( thisResidue%deriv%m .lt. minimum_res ) then
            ! residue is below some minimum for pool tracking
            call residueDestroy( thisResidue )
            if( ires .eq. 1 ) then
              ! this is the first residue pool, reassociate with plant
              thisPlant%residue => thisResidue
            else
              ! reassociate with parentResidue
              parentResidue%olderResidue => thisResidue
            end if
            ires = ires - 1
            iallres = iallres - 1
          else
            ! has enough residue
            ! thisResidue becomes parentResidue
            parentResidue => thisResidue
            ! point to next older residue
            thisResidue => thisResidue%olderResidue
          end if

        end do

        ! check if plant is not growing and has no residue
        ! NOTE: this assumes no biomass contained in plant pools for a non growing plant
        if( thisPlant%growth%am0cgf .or. associated(thisPlant%residue) ) then
          ! plant is growing or has residue
          parentPlant => thisPlant
          ! point to next older plant
          thisPlant => thisPlant%olderPlant
        else
          ! no growth and no residue
          call plantDestroy( thisPlant )
          if( iplt .eq. 1 ) then
            ! this is the first plant pool, reassociate with plant
            plant => thisPlant
          else
            ! reassociate with parentPlant
            parentPlant%olderPlant => thisPlant
          end if
          iplt = iplt - 1
        end if

      end do

      pool_cnt = iplt + iallres

      ! accumulate all living "crop" values into croptot variables
      ! zero accumulators
      croptot%dstmtot = 0.0
      croptot%zmht = 0.0 
      croptot%dayap = 99999
      croptot%mstandstore = 0.0 
      croptot%mflatstore = 0.0 
      croptot%mtot = 0.0 
      croptot%mtotto4 = 0.0 
      croptot%mftot = 0.0 
      croptot%msttot = 0.0 
      croptot%mbgtot = 0.0
      croptot%mbgtotto4 = 0.0
      croptot%mbgtotto15 = 0.0
      croptot%mrttot = 0.0 
      croptot%mrttotto4 = 0.0 
      croptot%mrttotto15 = 0.0 
      do idx = 1, soil%nslay
          croptot%mrtz(idx) = 0.0
          croptot%mbgz(idx) = 0.0
      end do
      croptot%rsaitot = 0.0
      croptot%rlaitot = 0.0
      croptot%rlailive = 0.0
      do idx = 1, ncanlay
          croptot%rsaz(idx) = 0.0
          croptot%rlaz(idx) = 0.0
      end do
      croptot%rcdtot = 0.0
      croptot%ffcvtot = 0.0
      croptot%fscvtot = 0.0
      croptot%ftcvtot = 0.0
      croptot%ftcancov = 0.0
      croptot%evapredu = 0.0

      ! accumulate all residue values into restot variables
      ! zero accumulators
      restot%dstmtot = 0.0
      restot%zmht = 0.0
      restot%dayap = 99999
      restot%mstandstore = 0.0
      restot%mflatstore = 0.0
      restot%mtot = 0.0
      restot%mtotto4 = 0.0
      restot%mftot = 0.0
      restot%msttot = 0.0
      restot%mbgtot = 0.0
      restot%mbgtotto4 = 0.0
      restot%mbgtotto15 = 0.0
      restot%mrttot = 0.0
      restot%mrttotto4 = 0.0
      restot%mrttotto15 = 0.0
      do idx = 1, soil%nslay
          restot%mrtz(idx) = 0.0
          restot%mbgz(idx) = 0.0
      end do
      restot%rsaitot = 0.0
      restot%rlaitot = 0.0
      restot%rlailive = 0.0
      do idx = 1, ncanlay
         restot%rsaz(idx) = 0.0
         restot%rlaz(idx) = 0.0
      end do
      restot%rcdtot = 0.0
      restot%ffcvtot = 0.0
      restot%fscvtot = 0.0
      restot%ftcvtot = 0.0
      restot%ftcancov = 0.0
      restot%evapredu = 0.0

      iplt = 0
      iallres = 0
      ! point to youngest living plant
      thisPlant => plant

      ! interate over all plants
      do while ( associated(thisPlant) )

        ! Living plant material section
        iplt = iplt + 1

        !write(*,'(a,3(1x,i0),2f24.20,l2)') 'RSAI: ', daysim, iplt, 0, thisPlant%deriv%rlai, thisPlant%deriv%rsai, &
        !                                                              thisPlant%growth%am0cgf

        if( thisPlant%growth%am0cgf ) then

          ! this is a living plant, add to croptot
          croptot%dstmtot = croptot%dstmtot + thisPlant%geometry%dstm   ! total number of stems  per unit area (#/m^2)
          croptot%zmht = max( croptot%zmht, thisPlant%geometry%zht )    ! Tallest biomass height across pools (m)
          croptot%dayap = min( croptot%dayap, plant%growth%dayap )      ! most recent planting

          croptot%mstandstore = croptot%mstandstore + thisPlant%mass%standstore
          croptot%mflatstore = croptot%mflatstore + thisPlant%mass%flatstore
          ! Total mass across pools (standing + flat + roots + buried) (kg/m^2)
          croptot%mtot = croptot%mtot + thisPlant%deriv%m
          ! Total mass across pools (standing + flat + roots + buried to a 4 inch depth) (kg/m^2)
          croptot%mtotto4 = croptot%mtotto4 + thisPlant%deriv%mst + thisPlant%deriv%mf &
                                            + thisPlant%deriv%dmrtto4 + thisPlant%deriv%dmbgto4
          ! Flat mass across pools (flatstem + flatleaf + flatstore) (kg/m^2)
          croptot%mftot = croptot%mftot + thisPlant%deriv%mf
          ! Standing mass across pools (standstem + standleaf + standstore) (kg/m^2)
          croptot%msttot = croptot%msttot + thisPlant%deriv%mst
          ! Buried mass across pools (kg/m^2)
          croptot%mbgtot = croptot%mbgtot + 0.0
          ! Buried (to a 4 inch depth) mass across pools (kg/m^2)
          croptot%mbgtotto4 = croptot%mbgtotto4 + 0.0
          ! Buried (to a 15 cm depth) mass across pools (kg/m^2)
          croptot%mbgtotto15 = croptot%mbgtotto15 + 0.0
          ! Buried root mass across pools (kg/m^2)
          croptot%mrttot = croptot%mrttot + thisPlant%deriv%mrt
          ! Buried (to a 4 inch depth) root mass across pools (kg/m^2)
          croptot%mrttotto4 = croptot%mrttotto4 + valbydepth(soil%nslay, soil%aszlyd, thisPlant%deriv%mrtz, 2, 0.0, scidepth)
          ! Buried (to a 15 cm depth) root mass across pools (kg/m^2)
          croptot%mrttotto15 = croptot%mrttotto15 + valbydepth(soil%nslay, soil%aszlyd, thisPlant%deriv%mrtz, 2, 0.0, weppdepth)

          ! sum layer mass across pools
          do idx = 1, soil%nslay
              croptot%mrtz(idx) = thisPlant%deriv%mrtz(idx) ! Buried root mass by soil layer (kg/m^2)
              croptot%mbgz(idx) = thisPlant%deriv%mbgz(idx) ! Buried mass by soil layer (kg/m^2)
          end do

          croptot%rsaitot = croptot%rsaitot + thisPlant%deriv%rsai      ! total of stem area index across pools (m^2/m^2)
          croptot%rlaitot = croptot%rlaitot + thisPlant%deriv%rlai      ! total of leaf area index across pools (m^2/m^2)
          croptot%rlailive = croptot%rlailive + thisPlant%deriv%rlai * thisPlant%growth%fliveleaf  ! living leaf area index total (m^2/m^2)
          do idx = 1, ncanlay
              croptot%rsaz(idx) = croptot%rsaz(idx) + thisPlant%deriv%rsai / ncanlay           ! stem area index by height (1/m)
              croptot%rlaz(idx) = croptot%rlaz(idx) + thisPlant%deriv%rlai / ncanlay           ! leaf area index by height (1/m)
          end do

          ! biodrag is the sum of discounted lai and sai for each pool
          croptot%rcdtot = croptot%rcdtot + biodrag(0.0,0.0,thisPlant%deriv%rlai, thisPlant%deriv%rsai, &
                                                    thisPlant%geometry%rg, thisPlant%geometry%xrow, &
                                                    thisPlant%geometry%zht, soil%aszrgh) 

          ! biomass cover across pools - flat with overlap (m^2/m^2)
          croptot%ffcvtot = croptot%ffcvtot + (1.0 - croptot%ffcvtot) * thisPlant%deriv%ffcv
          croptot%fscvtot = croptot%fscvtot + thisPlant%deriv%fscv      ! biomass cover across pools - standing (m^2/m^2)
          if (croptot%fscvtot > 1.0) croptot%fscvtot = 1.0

          ! fraction of soil surface covered by canopy across pools (m^2/m^2)
          croptot%ftcancov = croptot%ftcancov + thisPlant%deriv%fcancov

          ! Note: this is not consistent with including flat living biomass in biotot evapredu totaling
          ! this should be a composite value for any living plants with flat biomass.
          croptot%evapredu = 0.0     ! composite evaporation reduction from across pools (ea/ep ratio)
        end if

        ! point to newest residue for thisPlant
        ires = 0
        thisResidue => thisPlant%residue

        ! interate over all residue
        do while (associated(thisResidue))

          ires = ires + 1
          iallres = iallres + 1

          !write(*,'(a,3(1x,i0),2f24.20)') 'RSAI: ', daysim, iplt, ires, thisResidue%deriv%rlai, thisResidue%deriv%rsai

          ! this is a residue, add to restot
          restot%dstmtot = restot%dstmtot + thisResidue%dstm   ! total number of stems  per unit area (#/m^2)
          restot%zmht = max( restot%zmht, thisResidue%zht )    ! Tallest biomass height across pools (m)

          ! sum mass across pools
          restot%mstandstore = restot%mstandstore + thisResidue%standstore
          restot%mflatstore = restot%mflatstore + thisResidue%flatstore
          restot%mtot = restot%mtot + thisResidue%deriv%m
          restot%mtotto4 = restot%mtotto4 + thisResidue%deriv%mst + thisResidue%deriv%mf &
                                          + thisResidue%deriv%dmrtto4 + thisResidue%deriv%dmbgto4
          restot%mftot = restot%mftot + thisResidue%deriv%mf
          restot%msttot = restot%msttot + thisResidue%deriv%mst
          restot%mbgtot = restot%mbgtot + thisResidue%deriv%mbg
          restot%mbgtotto4 = restot%mbgtotto4 + thisResidue%deriv%dmbgto4
          restot%mbgtotto15 = restot%mbgtotto15 + thisResidue%deriv%dmbgto15
          restot%mrttot = restot%mrttot + thisResidue%deriv%mrt
          restot%mrttotto4 = restot%mrttotto4 + thisResidue%deriv%dmrtto4
          restot%mrttotto15 = restot%mrttotto15 + thisResidue%deriv%dmrtto15
          ! sum layer mass across pools
          do idx = 1, soil%nslay
            restot%mrtz(idx) = restot%mrtz(idx) + thisResidue%deriv%mrtz(idx)
            restot%mbgz(idx) = restot%mbgz(idx) + thisResidue%deriv%mbgz(idx)
          end do

          restot%rsaitot = restot%rsaitot + thisResidue%deriv%rsai
          restot%rlaitot = restot%rlaitot + thisResidue%deriv%rlai
          ! sum area indexes by layer across pools
          do idx = 1, ncanlay
            restot%rsaz(idx) = restot%rsaz(idx) + thisResidue%deriv%rsaz(idx)
            restot%rlaz(idx) = restot%rlaz(idx) + thisResidue%deriv%rlaz(idx)
          end do

          restot%rcdtot = restot%rcdtot + biodrag(0.0,0.0,thisResidue%deriv%rlai, thisResidue%deriv%rsai, &
                                                  thisPlant%geometry%rg, thisPlant%geometry%xrow, &
                                                  thisResidue%zht, soil%aszrgh) 

          ! Residue cover calculations.
          ! Overlap only applies when adding flat and flat, not flat and standing, or standing and standing.
          restot%ffcvtot = restot%ffcvtot + (1.0 - restot%ffcvtot) * thisResidue%deriv%ffcv !flat, with overlap
          restot%fscvtot = restot%fscvtot + thisResidue%deriv%fscv !standing, no overlap
          if (restot%fscvtot > 1.0) restot%fscvtot = 1.0
          ! residue leaf interception area (canopy cover)
          restot%ftcancov = restot%ftcancov + thisResidue%deriv%fcancov * (1.0 - restot%ftcancov)

          ! set to next older residue
          thisResidue => thisResidue%olderResidue
        end do

        ! point to next older plant
        thisPlant => thisPlant%olderPlant

      end do

      ! allocate array for flat mass values "youngest to oldest"
      allocate( evapredu(pool_cnt), stat=alloc_stat )
      if( alloc_stat .gt. 0 ) then
        write(*,*) 'ERROR: unable to allocate memory for evapredu array in plant_update'
      end if
      jdx = 0

      ! for weighted values, repeat iteration (weighting relies on a full total for normalization)
      ! zero accumulators
      croptot%zht_ave = 0.0 
      croptot%xstmrep = 0.0 
      restot%zht_ave = 0.0
      restot%xstmrep = 0.0

      ! point to youngest plant
      thisPlant => plant

      ! interate over all plants
      do while ( associated(thisPlant) )

        jdx = jdx + 1
        evapredu(jdx)%flatmass = thisPlant%deriv%mf
        evapredu(jdx)%evapredu_a = thisPlant%database%resevapa
        evapredu(jdx)%evapredu_b = thisPlant%database%resevapb

        if( thisPlant%growth%am0cgf ) then
          ! this is a living plant, add to croptot

          ! use sai weighting for average height and representative stem diameter
          if( croptot%rsaitot .gt. 0.0 ) then
            ! Weighted ave height across pools (m)
            croptot%zht_ave = croptot%zht_ave + thisPlant%geometry%zht * thisPlant%deriv%rsai / croptot%rsaitot
            ! Weighted ave representative stem diameter across pools (m)
            croptot%xstmrep = croptot%xstmrep + thisPlant%geometry%xstmrep * thisPlant%deriv%rsai / croptot%rsaitot
          end if
        end if

        ! set to newest plant residue
        thisResidue => thisPlant%residue

        ! interate over all residue
        do while (associated(thisResidue))

          jdx = jdx + 1
          evapredu(jdx)%flatmass = thisResidue%deriv%mf
          evapredu(jdx)%evapredu_a = thisPlant%database%resevapa
          evapredu(jdx)%evapredu_b = thisPlant%database%resevapb

          ! use sai weighting for average height and representative stem diameter
          if( restot%rsaitot .gt. 0.0 ) then
            restot%zht_ave = restot%zht_ave + thisResidue%zht * thisResidue%deriv%rsai / restot%rsaitot
            restot%xstmrep = restot%xstmrep + thisResidue%xstmrep * thisResidue%deriv%rsai / restot%rsaitot
          end if

          ! set to next older residue
          thisResidue => thisResidue%olderResidue
        end do

        ! point to next older plant
        thisPlant => thisPlant%olderPlant

      end do

      ! totaling complete
      ! calculate values that rely only on other total values

      ! biomass cover across pools - total (m^2/m^2) (adffcvtot + adfscvtot)
      croptot%ftcvtot = croptot%ffcvtot + croptot%fscvtot
      if (croptot%ftcvtot > 1.0) croptot%ftcvtot = 1.0

      restot%ftcvtot = restot%ffcvtot + restot%fscvtot !total, no overlap
      if (restot%ftcvtot > 1.0) restot%ftcvtot = 1.0

      ! all biomaterial totals

      ! Compute total number of stems
      biotot%dstmtot = croptot%dstmtot + restot%dstmtot

      ! compute the weighted average residue height

      ! determine weighting factors (stem area index)
      atotal = croptot%rsaitot + restot%rsaitot

      ! linearly weight height and representative diameter from crops and residue pools  based on stem area index
      if( atotal .gt. 0.0 ) then
         biotot%zht_ave = (croptot%zht_ave * croptot%rsaitot + restot%zht_ave * restot%rsaitot) / atotal
         biotot%xstmrep = (croptot%xstmrep * croptot%rsaitot + restot%xstmrep * restot%rsaitot) / atotal
      else
         biotot%zht_ave = 0.0
         biotot%xstmrep = 0.0
      end if

      ! set the tallest biomass height
      biotot%zmht = max( croptot%zmht, restot%zmht )

      ! sum the flat, standing, buried, and root biomass from each pool
      biotot%mftot = croptot%mftot + restot%mftot    !flat
      biotot%msttot = croptot%msttot + restot%msttot !standing 

      biotot%mbgtot = croptot%mbgtot + restot%mbgtot !below ground
      biotot%mrttot = croptot%mrttot + restot%mrttot !roots

      ! determine the total mass of biomass (above, flat and below ground)
      biotot%mtot = croptot%mtot + restot%mtot

      ! sum the buried biomass by layer
      ! sum the root mass by layer
      do jdx=1, size(biotot%mbgz)
        biotot%mbgz(jdx) = croptot%mbgz(jdx) + restot%mbgz(jdx)
        biotot%mrtz(jdx) = croptot%mrtz(jdx) + restot%mrtz(jdx)
      end do

      ! sum the stem area index and leaf area index values
      biotot%rsaitot = croptot%rsaitot + restot%rsaitot
      biotot%rlaitot = croptot%rlaitot + restot%rlaitot
      biotot%rlailive = croptot%rlailive + restot%rlailive

      ! sum the stem area index and leaf area index values by height
      ! this is based upon the "tallest" biomass pool height value (abzmht) determined previously.

      ! This divides the biomass equally into the height increments
      ! it isn't used yet and !really!!! is not right!!! since each
      ! pool should have it's own height, and hence divisions. This
      ! should at least stay within the arrays.
      do jdx = 1, size(biotot%rsaz)
          biotot%rsaz(jdx) = croptot%rsaz(jdx) + restot%rsaz(jdx)
          biotot%rlaz(jdx) = croptot%rlaz(jdx) + restot%rlaz(jdx)
      end do

      ! total biodrag is sum of crop and residue drag
      biotot%rcdtot = croptot%rcdtot + restot%rcdtot

      ! Combine residue cover from crop and decomp. pools.
      ! Overlap only applies when adding flat and flat, not flat and standing, or standing and standing.
      ! Note that these values shouldn't ever exceed 1.0 or be less than zero

      ! flat and flat, with overlap
      biotot%ffcvtot = croptot%ffcvtot + restot%ffcvtot * (1.0-croptot%ffcvtot)

      ! standing and standing, no overlap
      biotot%fscvtot = croptot%fscvtot + restot%fscvtot
      if (biotot%fscvtot > 1.0) biotot%fscvtot = 1.0

      ! flat and standing, no overlap
      biotot%ftcvtot =  biotot%ffcvtot + biotot%fscvtot
      if (biotot%ftcvtot > 1.0) biotot%ftcvtot = 1.0

      ! canopy cover for all biomass (overlaps)
      biotot%ftcancov = croptot%ftcancov + restot%ftcancov*(1.0-croptot%ftcancov)

      ! find composite evaporation supression for total flat residue
      ! set initial value to no residue condition
      biotot%evapredu = 1.0
      ! start with older flat residue layers
      do idx = pool_cnt, 1, -1
          if( evapredu(idx)%flatmass .gt. 0.0 ) then
              biotot%evapredu = resevapredu( biotot%evapredu, evapredu(idx)%flatmass, &
                                             evapredu(idx)%evapredu_a, evapredu(idx)%evapredu_b )
          end if
      end do

      ! deallocate evapredu
      deallocate( evapredu, stat=alloc_stat )
      if( alloc_stat .gt. 0 ) then
        write(*,*) 'ERROR: unable to deallocate memory for evapredu array in plant_update'
      end if


      return
    end subroutine plantupdate

    function resevapredu(prev_redu_ratio, biomass, coeff_a, coeff_b) result(evapredu)

!     + + + VARIABLE DECLARATIONS + + +
      real prev_redu_ratio
      real biomass
      real coeff_a
      real coeff_b

      real :: evapredu

!     + + + PURPOSE + + +
      ! calculates the evaporation reduction ratio given an accumulated
      ! evaporation reduction ratio from lower layers of residue. This
      ! accumulated ratio moves the effect of the additional biomass
      ! to a lower point on the evaporation reduction - biomass function
      ! curve and returns a ratio effecting the additional effect of the
      ! additional biomass.

!     + + + VARIABLE DEFINITIONS + + +
!     prev_redu_ratio - Accumulated evaporation reduction ratio from lower layers
!     biomass - additional biomass being added to evaporation reduction effect
!     coeff_a - coefficient a of ratio = exp( a * biomass ** b ) for this biomass
!     coeff_b - coefficient b of ratio = exp( a * biomass ** b ) for this biomass

!     LOCAL VARIABLES
      real pseudo_biomass

!     LOCAL VARIABLE DEFINITIONS
!     pseudo_biomass - an amount of biomass that would result in the prev_redu_ratio

      if( prev_redu_ratio .gt. 0.0 ) then
        ! zero value results in error taking log  below. This only happens
        ! with very large amounts of residue in multiple residue pools.
        if( (coeff_a .ne. 0.0) .and. (coeff_b .ne. 0.0) ) then
          pseudo_biomass =(log(prev_redu_ratio)/coeff_a )**(1.0/coeff_b)
        else
          pseudo_biomass = 0.0
        end if

        evapredu = exp(coeff_a * (pseudo_biomass + biomass)**coeff_b)
      else
        evapredu = 0.0
      end if

      return
    end function resevapredu

end module update_mod

