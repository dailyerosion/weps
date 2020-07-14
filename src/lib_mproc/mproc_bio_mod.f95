!$Author$
!$Date$
!$Revision$
!$HeadURL$

module mproc_bio_mod

    integer, parameter :: mnrbc = 5  ! number of burial coefficients (residue burial classes)
    integer, parameter :: mndk = 5  ! number of residue decomposition parameters

  contains

    subroutine flatvt (fltcoef, tillf, plant, bflg)

      ! This subroutine performs the biomass manipulation process of transferring
      ! standing biomass to flat biomass based upon a flattening coefficient.
      ! The standing component (either crop or a biomass pool) flattened
      ! is determined by a flag which is set before the call to this
      ! subroutine.  The flag may contain any number of combinations
      ! found below.

      ! This version changes the implicit assumption that if you flatten it,
      ! it is removed from the living crop and put into the temporary pool
      ! to become residue. It now just moves living crop material from standing to flat.
      ! Previously, flat crop leaf mass was assumed to be dead. It is now considered living.

      !        Flags values (binary #'s actually)
      ! bit no.                                    decimal value
      ! x  - flatten standing material in all pools      (0)
      ! 0  - flatten first plant (crop and residue)      (1)
      ! 1  - flatten second plant (crop and residue)     (2)
      ! 2  - flatten third plant (crop and residue)      (4)
      ! 3  - flatten fourth plant (crop and residue)     (8)

      ! Note that biomass for any of these pools that are flattened
      ! is transfered to the corresponding flat pool.

      use biomaterial, only: plant_pointer, residue_pointer

      !     + + + ARGUMENT DECLARATIONS + + +
      real :: fltcoef(mnrbc)  ! flattening coefficients of implement for different residue burial classes (m^2/m^2)
      real :: tillf           ! fraction of soil area tilled by the machine
      type(plant_pointer), pointer :: plant ! pointer to youngest plant data, which chains to older plant data
      integer :: bflg  ! flag indicating what to flatten
                       ! 0 - All standing material is flatttened (both crop and residue)

                       ! single subregion legacy understanding
                       ! 1 - Growing Crop is flattened
                       ! 2 - 1st residue pool is flattened
                       ! 4 - 2nd residue pool is flattened
                       ! 8 - 3rd residue pool is flattened
                       ! ....
                       ! 2**n - (n-1)th residue pool is flattened

                       ! Note that any combination of pools or crop may be used
                       ! A bit test is done on the binary number to see what to modify

      ! local variables
      integer :: idy     ! index variable for plants
      integer :: tflg    ! temporary biomass flag
      real :: flatfrac   ! fraction of material to be flattened
      type(plant_pointer), pointer :: thisPlant
      type(residue_pointer), pointer :: thisResidue

      ! set tflg bits correctly for "all" pools if bflg=0
      if (bflg .eq. 0) then
        tflg = 0
        do idy = 0, (bit_size(tflg) - 2)
           tflg = ibset(tflg, idy)
        end do
      else
        ! to deal with legacy understanding that crop or residue are flattened (or both),
        ! pad values to end so all residue pools after third are flattened if 3rd is flattened
        tflg = bflg
        if (BTEST(tflg,3)) then
          do idy = 4, (bit_size(tflg) - 2)
            tflg = ibset(tflg, idy)
          end do
        end if
      endif

      ! begin with provided plant then loop to older plants
      idy = 0
      thisPlant => plant
      do while( associated(thisPlant) )
        if( (thisPlant%database%rbc.ge.1).and.(thisPlant%database%rbc.le.mnrbc) ) then
          ! residue burial class indexes are within range
          flatfrac = min( 1.0, fltcoef(thisPlant%database%rbc) * tillf )
          if (BTEST(tflg,0)) then
          ! flag indicates to flatten this plant
            if( flatfrac .gt. 0.0 ) then
              ! flatten standing living plant biomass for thisPlant
              ! increase flat pools
              thisPlant%mass%flatstem = thisPlant%mass%flatstem + thisPlant%mass%standstem * flatfrac
              thisPlant%mass%flatleaf = thisPlant%mass%flatleaf + thisPlant%mass%standleaf * flatfrac
              thisPlant%mass%flatstore = thisPlant%mass%flatstore + thisPlant%mass%standstore * flatfrac
              ! decrease standing pools
              thisPlant%mass%standstem = thisPlant%mass%standstem * (1.0 - flatfrac)
              thisPlant%mass%standleaf = thisPlant%mass%standleaf * (1.0 - flatfrac)
              thisPlant%mass%standstore = thisPlant%mass%standstore * (1.0 - flatfrac)
              ! reduce # of stems
              thisPlant%geometry%dstm = thisPlant%geometry%dstm * (1.0 - flatfrac)
            end if
          end if

          ! flatten standing residue biomass for thisPlant
          thisResidue => thisPlant%residue
          do while( associated(thisResidue) )
            idy = idy + 1
            if (BTEST(tflg,idy)) then
              if( flatfrac .gt. 0.0 ) then
                ! increase flat pools
                thisResidue%flatstem = thisResidue%flatstem + thisResidue%standstem * flatfrac
                thisResidue%flatleaf = thisResidue%flatleaf + thisResidue%standleaf * flatfrac
                thisResidue%flatstore = thisResidue%flatstore + thisResidue%standstore * flatfrac
                ! decrease standing pools
                thisResidue%standstem = thisResidue%standstem * (1.0 - flatfrac)
                thisResidue%standleaf = thisResidue%standleaf * (1.0 - flatfrac)
                thisResidue%standstore = thisResidue%standstore * (1.0 - flatfrac)
                ! reduce # of stems
                thisResidue%dstm = thisResidue%dstm * (1.0 - flatfrac)
              end if
            end if

            ! go to next older residue in thisPlant
            thisResidue => thisResidue%olderResidue
          end do
        end if

        ! go to next older plant
        thisPlant => thisPlant%olderPlant
      end do

      return

    end subroutine flatvt

    subroutine fall_mod_vt ( rate_mult_vt, thresh_mult_vt, sel_pool, fracarea, plant)

      ! This subroutine modifies the stem fall rate for standing crop and
      ! residue material using a multiplier. The rate multiplier is
      ! selected based on the toughness class and adjusted if the part of 
      ! the area is affected.

      use biomaterial, only: plant_pointer

      ! + + + ARGUMENT DECLARATIONS + + +
      real       rate_mult_vt(mnrbc)      ! standing stem fall rate multiplier
      real       thresh_mult_vt(mnrbc)    ! standing stem fall rate multiplier
      integer    sel_pool   ! pool to which percentages will be applied
                            ! 0 - don't apply to anything
                            ! 1 - apply to crop pool
                            ! 2 - apply to temporary pool
                            ! 3 - apply to crop and temporary pools
                            ! 4 - apply to residue pools
                            ! 5 - apply to crop and residue pools
                            ! 6 - apply to temporary and residue pools
                            ! 7 - apply to crop, temporary and residue pools
                            !     this corresponds to the bit pattern:
                            !     msb(residue, temporary, crop)lsb

       ! MODIFIED TO
                       ! 1 - 1'st Crop/residue is modified
                       ! 2 - 2'nd Crop/residue
                       ! 4 - 3'rd Crop/residue
                       ! ....
                       ! 2**n - (n-1)th Crop/residue


      real       fracarea   ! fraction of surface area affected by operation
      type(plant_pointer), pointer :: plant ! pointer to youngest plant data, which chains to older plant data

      ! + + + LOCAL VARIABLES + + +
      integer idx                      ! loop/index variable
      real area_adj_rate_mult(mnrbc)   ! adjust the rate multiplier based on area fraction
      real area_adj_thresh_mult(mnrbc) ! adjust the threshold multiplier based on area fraction
      type(plant_pointer), pointer :: thisPlant

      ! + + + END SPECIFICATIONS + + +

      do idx = 1, mnrbc
          area_adj_rate_mult(idx) = 1.0 + fracarea*(rate_mult_vt(idx)-1.0)
          area_adj_thresh_mult(idx) = 1.0 + fracarea*(thresh_mult_vt(idx)-1.0)
      end do

      idx = 0
      thisPlant => plant
      do while( associated(thisPlant) )
        if( BTEST(sel_pool,idx) ) then
          ! Adjust for proper residue burial class
          thisPlant%database%dkrate(5) = thisPlant%database%dkrate(5) * area_adj_rate_mult(thisPlant%database%rbc)
          thisPlant%database%ddsthrsh = thisPlant%database%ddsthrsh * area_adj_thresh_mult(thisPlant%database%rbc)
        end if
        idx = idx + 1
        thisPlant => thisPlant%olderPlant
      end do

      return

    end subroutine fall_mod_vt

    subroutine liftvt (liftf, tillf, nlay, nslay, plant, resurface_roots, bflg)

      ! This subroutine performs the biomass manipulation process of bringing
      ! buried biomass to the surface. It can act on living roots and buried residue.
      ! If living roots are lifted, they become residue.

      use biomaterial, only: plant_pointer, residue_pointer, residueAdd

      ! + + + ARGUMENT DECLARATIONS + + +
      real, intent(in) :: liftf(*)  ! fraction of buried material lifted to the surface for
                                    ! different residue burial classes (m^2/m^2)
      real, intent(in) :: tillf     ! fraction of soil area tilled by the machine
      integer, intent(in) :: nlay   ! number of soil layers affected by the operation(s)
      integer, intent(in) :: nslay  ! total number of layers in soil
      type(plant_pointer), pointer :: plant ! pointer to youngest plant data, which chains to older plant data
      integer, intent(in) :: resurface_roots  ! flag to specify whether roots are resurfaced or not
      integer, intent(in) :: bflg  ! flag indicating what to manipulate
                       ! 0 - All standing material is manipulate (both crop and residue)
                       ! 1 - Crop roots are resurfaced
                       ! 2 - 1'st residue pool biomass is surfaced
                       ! 4 - 2'nd residue pool
                       ! ....
                       ! 2**n - (n-1)th residue pool

      ! + + + LOCAL VARIABLES + + +
      integer :: lay   ! layer index
      integer :: idy   ! index of plants
      integer :: tflg  ! modified bflg
      real :: liftlay(nlay) ! buried material lifted to the surface in each layer
      real :: lifttot    ! total buried material lifted to the surface
      type(plant_pointer), pointer :: thisPlant
      type(residue_pointer), pointer :: thisResidue

      ! + + + END SPECIFICATIONS + + +

      ! set tflg bits correctly for "all" pools if bflg=0
      if (bflg .eq. 0) then
        tflg = 0
        do idy = 0, (bit_size(tflg) - 2)
           tflg = ibset(tflg, idy)
        end do
      else
          tflg = bflg
      endif

      ! begin with provided plant then loop to older plants
      idy = 0
      thisPlant => plant
      do while( associated(thisPlant) )
        if (BTEST(tflg,idy)) then
          ! flag indicates to lift biomass for this plant
          if( (thisPlant%database%rbc .ge. 1) .and. (thisPlant%database%rbc .le. mnrbc) ) then
            ! residue burial class indexes are within range

            ! add residue pool for living root biomass being lifted and killed
            thisPlant%residue => residueAdd( thisPlant%residue, thisPlant%residueIndex, nslay )

            ! living below ground stems
            lifttot = 0.0
            do lay=1,nlay
                liftlay(lay) = thisPlant%mass%stemz(lay) * liftf(thisPlant%database%rbc) * tillf
                lifttot = lifttot + liftlay(lay)
                thisPlant%mass%stemz(lay) = thisPlant%mass%stemz(lay) - liftlay(lay)
            end do
            thisPlant%residue%flatstem = thisPlant%residue%flatstem + lifttot

            ! living fibrous roots
            if (resurface_roots == 1) then
              lifttot = 0.0
              do lay=1,nlay
                liftlay(lay) = thisPlant%mass%rootfiberz(lay) * liftf(thisPlant%database%rbc) * tillf
                lifttot = lifttot + liftlay(lay)
                thisPlant%mass%rootfiberz(lay) = thisPlant%mass%rootfiberz(lay) - liftlay(lay)
              end do
              thisPlant%residue%flatrootfiber = thisPlant%residue%flatrootfiber + lifttot

              ! living storage roots
              lifttot = 0.0
              do lay=1,nlay
                liftlay(lay) = thisPlant%mass%rootstorez(lay) * liftf(thisPlant%database%rbc) * tillf
                lifttot = lifttot + liftlay(lay)
                thisPlant%mass%rootstorez(lay) = thisPlant%mass%rootstorez(lay) - liftlay(lay)
              end do
              thisPlant%residue%flatrootstore = thisPlant%residue%flatrootstore + lifttot
            end if

            ! lift all residues, skip newly created pool since no belowground in it
            thisResidue => thisPlant%residue%olderResidue
            do while( associated(thisResidue) )

              ! stem
              lifttot = 0.0
              do lay=1,nlay
                liftlay(lay) = thisResidue%stemz(lay) * liftf(thisPlant%database%rbc) * tillf
                lifttot = lifttot + liftlay(lay)
                thisResidue%stemz(lay) = thisResidue%stemz(lay) - liftlay(lay)
              end do
              thisResidue%flatstem = thisResidue%flatstem + lifttot

              ! leaf
              lifttot = 0.0
              do lay=1,nlay
                liftlay(lay) = thisResidue%leafz(lay) * liftf(thisPlant%database%rbc) * tillf
                lifttot = lifttot + liftlay(lay)
                thisResidue%leafz(lay) = thisResidue%leafz(lay) - liftlay(lay)
              end do
              thisResidue%flatleaf = thisResidue%flatleaf + lifttot

              ! store
              lifttot = 0.0
              do lay=1,nlay
                liftlay(lay) = thisResidue%storez(lay) * liftf(thisPlant%database%rbc) * tillf
                lifttot = lifttot + liftlay(lay)
                thisResidue%storez(lay) = thisResidue%storez(lay) - liftlay(lay)
              end do
              thisResidue%flatstore = thisResidue%flatstore + lifttot

              if (resurface_roots == 1) then
                ! rootstore
                lifttot = 0.0
                do lay=1,nlay
                  liftlay(lay) = thisResidue%rootstorez(lay) * liftf(thisPlant%database%rbc) * tillf
                  lifttot = lifttot + liftlay(lay)
                  thisResidue%rootstorez(lay) = thisResidue%rootstorez(lay) - liftlay(lay)
                end do
                thisResidue%flatrootstore = thisResidue%flatrootstore + lifttot

                ! rootfiber
                lifttot = 0.0
                do lay=1,nlay
                  liftlay(lay) = thisResidue%rootfiberz(lay) * liftf(thisPlant%database%rbc) * tillf
                  lifttot = lifttot + liftlay(lay)
                  thisResidue%rootfiberz(lay) = thisResidue%rootfiberz(lay) - liftlay(lay)
                end do
                thisResidue%flatrootfiber = thisResidue%flatrootfiber + lifttot
              endif

              ! go to next older residue in thisPlant
              thisResidue => thisResidue%olderResidue
            end do
          endif
        endif

        ! go to next older plant
        idy = idy+1
        thisPlant => thisPlant%olderPlant
      end do

      return
    end subroutine liftvt

    subroutine mburyvt (buryf, tillf, burydistflg, nlay, soil, plant, bflg )

      ! This subroutine performs the biomass manipulation process of transferring
      ! the above ground biomass into the soil or the inverse process of bringing
      ! buried biomass to the surface.  It deals only with flat biomass

      use soil_data_struct_defs, only: soil_def
      use biomaterial, only: plant_pointer, residue_pointer, residueAdd

      ! + + + ARGUMENT DECLARATIONS + + +
      real    buryf(*)    ! fraction of flat material buried for
                          ! different residue burial classes (m^2/m^2)
      real    tillf       ! fraction of soil area tilled by the machine
      integer burydistflg ! distribution function to be used
                          ! 0    o uniform distribution
                          ! 1    o Mixing+Inversion Burial Distribution
                          ! 2    o Mixing Burial Distribution
                          ! 3    o Inversion Burial Distribution
                          ! 4    o Lifting, Fracturing Burial Distribution
                          ! 5    o Compression
      integer nlay        ! number of soil layers used in the operation(s)
      type(soil_def), intent(in) :: soil  ! soil for this subregion
      type(plant_pointer), pointer :: plant ! pointer to youngest plant data, which chains to older plant data
      integer :: bflg  ! flag indicating what to manipulate
                       ! 0 - All standing material is manipulate (both crop and residue)
                       ! 1 - Crop flat is buried
                       ! 2 - 1'st residue pool biomass is buried
                       ! 4 - 2'nd residue pool
                       ! ....
                       ! 2**n - (n-1)th residue pool

      ! + + + FUNCTIONS + + +
      !  real burydist

      ! + + + LOCAL VARIABLES + + +
      integer :: lay  ! loop variable for layers
      integer :: idy  ! index of plants
      integer :: tflg ! modified bflg
      real     tbury  ! mass of biomass that is buried
      real     fracbury(nlay) ! fractions of total to be buried in each layer
      type(plant_pointer), pointer :: thisPlant
      type(residue_pointer), pointer :: thisResidue

      ! + + + END SPECIFICATIONS + + +

      ! set tflg bits correctly for "all" pools if bflg=0
      if (bflg .eq. 0) then
        tflg = 0
        do idy = 0, (bit_size(tflg) - 2)
           tflg = ibset(tflg, idy)
        end do
      else
          tflg = bflg
      endif

      ! calculate fractions of total to be buried in each layer
      do lay = 1, nlay
          fracbury(lay) = burydist(lay, burydistflg, soil%aszlyt, soil%aszlyd, nlay)
      end do

      ! begin with provided plant then loop to older plants
      idy = 0
      thisPlant => plant
      do while( associated(thisPlant) )
        if (BTEST(tflg,idy)) then
          ! flag indicates to bury biomass for this plant
          if( (thisPlant%database%rbc.ge.1).and.(thisPlant%database%rbc.le.mnrbc) ) then
            ! residue burial class indexes are within range

            ! add residue pool for living biomass being buried and killed
            thisPlant%residue => residueAdd( thisPlant%residue, thisPlant%residueIndex, soil%nslay )
            ! store affected, set residue grain fraction from plant value 
            thisPlant%residue%grainf = thisPlant%geometry%grainf

            ! stem component
            tbury = thisPlant%mass%flatstem*buryf(thisPlant%database%rbc)*tillf

            !write(*,*) 'MBURY STEM: ', tbury, thisPlant%mass%flatstem

            do lay=1,nlay
              thisPlant%residue%stemz(lay) = thisPlant%residue%stemz(lay)+tbury*fracbury(lay)
            end do
            thisPlant%mass%flatstem = thisPlant%mass%flatstem-tbury

            ! leaf component
            tbury = thisPlant%mass%flatleaf*buryf(thisPlant%database%rbc)*tillf

            !write(*,*) 'MBURY LEAF: ', tbury

            do lay=1,nlay
              thisPlant%residue%leafz(lay) = thisPlant%residue%leafz(lay)+tbury*fracbury(lay)
            end do
            thisPlant%mass%flatleaf = thisPlant%mass%flatleaf-tbury

            ! storage component
            tbury = thisPlant%mass%flatstore*buryf(thisPlant%database%rbc)*tillf

            !write(*,*) 'MBURY STORE: ', tbury

            do lay=1,nlay
              thisPlant%residue%storez(lay) = thisPlant%residue%storez(lay) + tbury*fracbury(lay)
            end do
            thisPlant%mass%flatstore = thisPlant%mass%flatstore-tbury

            ! bury all residues, skip newly created pool since no flat in it
            thisResidue => thisPlant%residue%olderResidue
            do while( associated(thisResidue) )

              ! stem
              tbury = thisResidue%flatstem * buryf(thisPlant%database%rbc) * tillf
              do lay=1,nlay
                thisResidue%stemz(lay) = thisResidue%stemz(lay) + tbury*fracbury(lay)
              end do
              thisResidue%flatstem = thisResidue%flatstem - tbury

              ! leaf
              tbury = thisResidue%flatleaf * buryf(thisPlant%database%rbc) * tillf
              do lay=1,nlay
                thisResidue%leafz(lay) = thisResidue%leafz(lay) + tbury*fracbury(lay)
              end do
              thisResidue%flatleaf = thisResidue%flatleaf - tbury

              ! store
              tbury = thisResidue%flatstore * buryf(thisPlant%database%rbc) * tillf
              do lay=1,nlay
                thisResidue%storez(lay) = thisResidue%storez(lay) + tbury*fracbury(lay)
              end do
              thisResidue%flatstore = thisResidue%flatstore - tbury

              ! rootstore
              tbury = thisResidue%flatrootstore * buryf(thisPlant%database%rbc) * tillf
              do lay=1,nlay
                thisResidue%rootstorez(lay) = thisResidue%rootstorez(lay) + tbury*fracbury(lay)
              end do
              thisResidue%flatrootstore = thisResidue%flatrootstore - tbury

              ! root fiber
              tbury = thisResidue%flatrootfiber * buryf(thisPlant%database%rbc) * tillf
              do lay=1,nlay
                thisResidue%rootfiberz(lay) = thisResidue%rootfiberz(lay) + tbury*fracbury(lay)
              end do
              thisResidue%flatrootfiber = thisResidue%flatrootfiber - tbury

              ! go to next older residue in thisPlant
              thisResidue => thisResidue%olderResidue
            end do
          endif
        endif

        ! go to next older plant
        idy = idy+1
        thisPlant => thisPlant%olderPlant
      end do

      return

    end subroutine mburyvt

    function kill_plant( bm0kilfl, nslay, plant ) result(plant_killed)

      ! This subroutine performs the kill crop process and transferring of
      ! biomass from living crop to residue.  Transfer of biomass is performed
      ! on above ground biomass and the root biomass.  The transfer is
      ! from the living crop mass into residue mass.

      use biomaterial, only: plant_pointer, residueAdd

      ! + + + ARGUMENT DECLARATIONS + + +
      integer, intent(in) :: bm0kilfl   ! flag indicating action
      integer, intent(in) :: nslay      ! total number of layers in soil
      type(plant_pointer), pointer :: plant     ! pointer to youngest plant data, which chains to older plant data

      logical :: plant_killed

      ! + + + LOCAL VARIABLES + + +
      integer :: lay  ! soil layer index
      type(plant_pointer), pointer :: thisPlant

      ! + + + END SPECIFICATIONS + + +

      plant_killed = .false.

      thisPlant => plant
      do while( associated(thisPlant) )
        ! plant exists
        if( thisPlant%growth%growing .and. .not. thisPlant%growth%am0cif ) then
          ! crop growth flag on and not on initialization cycle
          if ((bm0kilfl.eq.2).or.((bm0kilfl.eq.1).and.((thisPlant%database%idc.eq.1)&
             .or.(thisPlant%database%idc.eq.2).or.(thisPlant%database%idc.eq.4) &
             .or.(thisPlant%database%idc.eq.5)))) then

            ! stop the crop growth (ie. stop calling crop submodel)
            thisPlant%growth%growing = .false.

            ! add residue pool for living mass being killed
            thisPlant%residue => residueAdd( thisPlant%residue, thisPlant%residueIndex, nslay ) 

            ! move biomass from living to dead
            thisPlant%residue%standstem = thisPlant%mass%standstem
            thisPlant%mass%standstem = 0.0
            thisPlant%residue%standleaf = thisPlant%mass%standleaf
            thisPlant%mass%standleaf = 0.0
            thisPlant%residue%standstore = thisPlant%mass%standstore
            thisPlant%mass%standstore = 0.0

            thisPlant%residue%flatstem = thisPlant%mass%flatstem
            thisPlant%mass%flatstem = 0.0
            thisPlant%residue%flatleaf = thisPlant%mass%flatleaf
            thisPlant%mass%flatleaf = 0.0
            thisPlant%residue%flatstore = thisPlant%mass%flatstore
            thisPlant%mass%flatstore = 0.0

            do lay = 1,nslay
              thisPlant%residue%rootstorez(lay) = thisPlant%mass%rootstorez(lay)
              thisPlant%mass%rootstorez(lay) = 0.0
              thisPlant%residue%rootfiberz(lay)= thisPlant%mass%rootfiberz(lay)
              thisPlant%mass%rootfiberz(lay) = 0.0
              thisPlant%residue%stemz(lay) = thisPlant%mass%stemz(lay)
              thisPlant%mass%stemz(lay) = 0.0
            end do

            thisPlant%residue%zht = thisPlant%geometry%zht 
            thisPlant%geometry%zht = 0.0
            thisPlant%residue%dstm = thisPlant%geometry%dstm
            thisPlant%geometry%dstm = 0.0
            thisPlant%residue%xstmrep = thisPlant%geometry%xstmrep
            ! do not zero to preserver for end of season report
            ! It is set to zero by plant update
            ! thisPlant%geometry%xstmrep = 0.0
            thisPlant%residue%zrtd = thisPlant%geometry%zrtd
            thisPlant%geometry%zrtd = 0.0
            thisPlant%residue%grainf = thisPlant%geometry%grainf
            thisPlant%geometry%grainf = 0.0

            plant_killed = .true.

          end if
        end if

        ! go to next older plant
        thisPlant => thisPlant%olderPlant
      end do
      
    end function kill_plant

    function defoliate( bm0kilfl, nslay, plant ) result(plant_defoliated)

      ! This subroutine performs the defoliation process, transferring all plant
      ! leaf mass from living crop leaves to flat residue.

      use biomaterial, only: plant_pointer, residueAdd

      ! + + + ARGUMENT DECLARATIONS + + +
      integer, intent(in) :: bm0kilfl   ! flag indicating action
      integer, intent(in) :: nslay      ! total number of layers in soil
      type(plant_pointer), pointer :: plant     ! pointer to youngest plant data, which chains to older plant data

      logical :: plant_defoliated

      ! + + + LOCAL VARIABLES + + +
      integer :: lay  ! soil layer index
      type(plant_pointer), pointer :: thisPlant

      ! + + + END SPECIFICATIONS + + +

      plant_defoliated = .false.

      thisPlant => plant
      do while( associated(thisPlant) )
        ! plant exists
        if( thisPlant%growth%growing .and. .not. thisPlant%growth%am0cif ) then
          ! crop growth flag on and not on initialization cycle
          if( bm0kilfl .eq. 3 ) then
            ! defoliate flag set

            ! add residue pool for living mass being killed
            thisPlant%residue => residueAdd( thisPlant%residue, thisPlant%residueIndex, nslay ) 

            ! move biomass from living to dead
            thisPlant%residue%flatleaf = thisPlant%mass%standleaf
            thisPlant%mass%standleaf = 0.0
            thisPlant%residue%flatleaf = thisPlant%residue%flatleaf + thisPlant%mass%flatleaf
            thisPlant%mass%flatleaf = 0.0

            plant_defoliated = .true.

          end if
        end if

        ! go to next older plant
        thisPlant => thisPlant%olderPlant
      end do

    end function defoliate

! This routine adjusts the burial coefficients for operation speed
! and tillage depth

    subroutine buryadj( burycoef,mnrbc,                               &
     &                    speed,stdspeed,minspeed,maxspeed,             &
     &                    depth,stddepth,mindepth,maxdepth)

!     argument declarations
      real    burycoef(mnrbc)
      integer mnrbc
      real    speed,stdspeed,minspeed,maxspeed
      real    depth,stddepth,mindepth,maxdepth

!     argument definitions
!     burycoef - burial fraction coefficient to be adjusted
!     mnrbc    - number of burial coefficients (residue burial classes)
!     speed    - actual
!     stdspeed - standard, where coefficient remains unchanged
!     minspeed - minimum
!     maxspeed - maximum
!     depth    - actual
!     stddepth - standard, where coefficient remains unchanged
!     mindepth - minimum
!     maxdepth - maximum

!     local variable declarations
      integer index
      real    rspeed, rdepth
      real    expspeed, s1speed, s2speed, expdepth

      parameter (expspeed = 0.5)
      parameter (s1speed = 0.6)
      parameter (s2speed = 0.4)
      parameter (expdepth = 2.7)

!     find speed adjustment parameter
      speed = max( min(speed, maxspeed), minspeed )
      rspeed = (s1speed+s2speed*(speed/maxspeed)**expspeed)/            &
     &         (s1speed+s2speed*(stdspeed/maxspeed)**expspeed)

!     find depth adjustment parameter
      depth = max(min(depth, maxdepth), mindepth )
      rdepth = (1.0-(1.0-depth/maxdepth)**expdepth)/                    &
     &         (1.0-(1.0-stddepth/maxdepth)**expdepth)

!     adjust burial coefficients and keep within range 0 to 1
      do 100 index=1,mnrbc
          burycoef(index) = burycoef(index)*rspeed*rdepth
          burycoef(index) = min( 1.0, max( 0.0, burycoef(index)))
 100  continue
      return

    end subroutine buryadj

! This routine returns the fraction of material buried in layer number 
! LAY given the burial distribution function type BURYDISTFLG and the
! layer thicknesses LTHICK and the total number of layers in which
! material will be buried NLAY and the tillage depth, soil layer
! thicknesses, and the number of soil layers.  It returns the number
! of layers that will be considered to be within the tillage zone for
! this operation.

    real function burydist( lay, burydistflg, lthick, ldepth, nlay)

!     argument declarations
      integer lay
      integer burydistflg
      real    lthick(*)
      real    ldepth(*)
      integer nlay

!     argument definitions
!     lay         - soil layer for which fraction is returned
!     tlay        - number of soil layers affected by tillage
!     burydistflg - distribution function to be used
!              0    o uniform distribution
!              1    o Mixing+Inversion Burial Distribution
!              2    o Mixing Burial Distribution
!              3    o Inversion Burial Distribution
!              4    o Lifting, Fracturing Burial Distribution
!              5    o Compression
!     lthick      - thickness of soil layer
!     ldepth      - distance from surface to bottom of layer
!     nlay        - number of soil layers affected

!     local variable declarations
      real upper, lower
      real c1exp, c2exp
      real c3e1, c3e2, c3brk, c3split

      parameter (c1exp = 0.5)
      parameter (c2exp = 0.3)
      parameter (c3brk = 0.60)

!     assign depth from surface to upper and lower layer bounds
      if( lay.eq.1 ) then
          upper = 0.0
      else
          upper = ldepth(lay-1) / ldepth(nlay)
      end if
      lower = ldepth(lay) / ldepth(nlay)

!     find fraction of material buried in layer LAY
      select case (burydistflg)
      case(1)
          burydist = lower**c1exp - upper**c1exp
      case(2,5) ! same for compression and mixing from Nat. Agron. Manual, 508CrevisionwSTIR 071106DTL
          burydist = lower**c2exp - upper**c2exp
      case(3)
          if(lower.le.c3brk) then 
              burydist = 0.28*(exp(1.83*lower)-1.0)
          else
              burydist = 1.0-0.441*((1.0-lower)/0.4)**1.4
          endif
          if(upper.le.c3brk) then 
              burydist = burydist - (0.28*(exp(1.83*upper)-1.0))
          else
              burydist = burydist - (1.0-0.441*((1.0-upper)/0.4)**1.4)
          endif
      case(4)
          burydist = lower**c1exp - upper**c1exp
      case default   !uniform burial distribution
          burydist = lower - upper
      end select
 1000 return
    end function burydist

    subroutine resinit(resmass, resdepth, nlay, resarray, laythick)

!     + + + INPUT VARIABLE DECLARATIONS + + +
      real resmass
      real resdepth
      integer nlay
      real resarray(nlay)
      real laythick(nlay)

!     + + + INPUT VARIABLE DEFINITIONS + + +
!     resmass - residue mass (Kg/m^2)
!     resdepth - Depth residue is distributed in soil (mm)
!     nlay - number of soil layers
!     resarray(nlay) - soil residue array by layer (Kg/m^2)
!     laythick(nlay) - soil layer thickness (mm)

!     + + + LOCAL VARIABLE DECLARATIONS + + +
      integer ilay
      real    depth

!     + + + LOCAL VARIABLE DEFINITIONS + + +
!     ilay - array index
!     depth - accumulator for depth
!     thick - thickness of slice to which residue is to be added

      depth = resdepth
      do ilay = 1, nlay
          if (depth.gt.0.0) then
              resarray(ilay) = resmass                                  &
     &                       * min( depth,laythick(ilay)) / (resdepth)
              depth = depth - laythick(ilay)
          else
              resarray(ilay) = 0.0
          end if
      end do

      return
    end subroutine resinit

end module mproc_bio_mod
