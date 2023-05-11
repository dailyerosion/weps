!$Author$
!$Date$
!$Revision$
!$HeadURL$

module mproc_soil_mod

  contains

    pure subroutine mix (u, tillf, nlay, bulkden, laythk, &
                    sand, silt, clay, vfrock, &
                    vc_sand, c_sand, m_sand, f_sand, vf_sand, &
                    w_bd, &
                    organic, ph, calcarb, cation, &
                    lin_ext, &
                    aggden, drystab, &
                    soilwatr, &
                    satwatr, thrdbar, ftnbar, &
                    avawatr, &
                    soilcb, soilair, satcond, &
                    plant, massf)

      ! This subroutine reads in the array(s) containing the components 
      ! that need to be mixed.  It then calls the subroutine mixproc
      ! and the actual mixing process is performed.

      ! NOTE:  This subroutine needs other components to be passed to it so
      !        they can be mixed.  Currently this is not done.  I need to get
      !        together with L. Wagner on this.  A.N.Hawkins 8/1/95 

      use asd_mod, only: msieve
      use biomaterial, only: plant_pointer, residue_pointer
      use soilden_mod, only: den_rock, getLayMass, setVolFrac, setLayThick

      ! + + + ARGUMENT DECLARATIONS + + +
      integer, intent(in) :: nlay       ! number of soil layers used
      real, intent(in) :: u             ! mixing coefficient
      real, intent(in) :: tillf         ! fraction of the soil area tilled by the machine
      real, intent(inout) :: bulkden(*) ! fine soil bulk density
      real, intent(inout) :: laythk(*)  ! layer thickness
      real, intent(inout) :: sand(*)    ! fraction of sand
      real, intent(inout) :: silt(*)    ! fraction of silt
      real, intent(inout) :: clay(*)    ! fraction of clay
      real, intent(inout) :: vfrock(*)  ! volume fraction of rock
      real, intent(inout) :: vc_sand(*) ! fraction of very course sand
      real, intent(inout) :: c_sand(*)  ! fraction of course sand
      real, intent(inout) :: m_sand(*)  ! fraction of medium sand
      real, intent(inout) :: f_sand(*)  ! fraction of fine sand
      real, intent(inout) :: vf_sand(*) ! fraction of very fine sand
      real, intent(inout) :: w_bd(*)    ! wet (1/3 bar) fine soil density
      real, intent(inout) :: organic(*) ! fraction of organic matter
      real, intent(inout) :: ph(*)      ! soil Ph
      real, intent(inout) :: calcarb(*) ! fraction of calcium carbonate
      real, intent(inout) :: cation(*)  ! cation exchange capcity
      real, intent(inout) :: lin_ext(*) ! linear extensibility
      real, intent(inout) :: aggden(*)  ! aggregrate density
      real, intent(inout) :: drystab(*) ! dry aggregrate stability
      real, intent(inout) :: soilwatr(*) ! soil water content (mass bases)
      real, intent(inout) :: satwatr(*) ! saturation soil water content
      real, intent(inout) :: thrdbar(*) ! 1/3 bar soil water content
      real, intent(inout) :: ftnbar(*)  ! 15 bar soil water content
      real, intent(inout) :: avawatr(*) ! available soil water content
      real, intent(inout) :: soilcb(*)  ! soil CB value
      real, intent(inout) :: soilair(*) ! soil air entery potential
      real, intent(inout) :: satcond(*) ! saturated hydraulic conductivity
      type(plant_pointer), pointer :: plant ! pointer to youngest plant data, which chains to older plant data
      real, dimension(msieve+1,*), intent(inout) :: massf ! mass fractions for sieve cuts

      ! + + + LOCAL VARIABLES + + +
      real :: tillmix                   ! combination of mixing coefficient and tilled area fraction
      real, dimension(nlay) :: dum      ! dummy variable used in calculating the mass in a layer
      real, dimension(nlay) :: msand    ! by layer mass of fine soil sand
      real, dimension(nlay) :: msilt    ! by layer mass of fine soil silt
      real, dimension(nlay) :: mclay    ! by layer mass of fine soil clay
      real, dimension(nlay) :: morganic ! by layer mass of fine soil organic matter
      real, dimension(nlay) :: mvc_sand ! by layer mass of fine soil very course sand
      real, dimension(nlay) :: mc_sand  ! by layer mass of fine soil course sand
      real, dimension(nlay) :: mm_sand  ! by layer mass of fine soil medium sand
      real, dimension(nlay) :: mf_sand  ! by layer mass of fine soil fine sand
      real, dimension(nlay) :: mvf_sand ! by layer mass of fine soil very fine sand
      real, dimension(nlay) :: m_rock   ! by layer mass of rock fragments
      real :: cmass                     ! summation of component mass over layers
      real, dimension(nlay) :: slmass   ! by layer mass of fine soil
      real, dimension(nlay) :: srlmass  ! by layer mass of fine soil plus rock fragments
      real :: srmass                    ! summation of soil plus rock fragment mass over layers
      real :: tmass                     ! temporary mass
      integer :: i                      ! loop index variable
      integer :: j                      ! loop index variable
      type(plant_pointer), pointer :: thisPlant
      type(residue_pointer), pointer :: thisResidue

      ! + + + END SPECIFICATIONS + + + 

      ! Print the initial masses calculated above

      ! find combination coefficient based on fraction of area and mixing
      tillmix = u*tillf

      ! Calculate the total mass in all layers within a subregion
      srmass = 0.0
      do i = 1, nlay
         tmass = bulkden(i)*(1.0-vfrock(i))
         slmass(i) = laythk(i) * tmass
         srlmass(i) = laythk(i) * (tmass + den_rock*vfrock(i))
         srmass = srmass + srlmass(i)
      end do

      ! Make calls to the mixing process.  First need to calculate
      ! the total mass of the component to be mixed.  This is then passed
      ! in the call.

      ! Need to calculate the component mass before making the call
      ! to mixproc for each and every component.  This is then passed to
      ! mixproc and used in the mix calculation. 

      !******************SOIL VARIABLES********************

      ! sand
      cmass = 0.0
      do i = 1, nlay
         msand(i) = getLayMass( sand(i), laythk(i), vfrock(i), bulkden(i), organic(i) )
         cmass = cmass + msand(i)
      end do
      call mixproc(tillmix, nlay, msand, cmass, srlmass, srmass)

      ! silt
      cmass = 0.0
      do i = 1, nlay
         msilt(i) = getLayMass( silt(i), laythk(i), vfrock(i), bulkden(i), organic(i) )
         cmass = cmass + msilt(i)
      end do
      call mixproc(tillmix, nlay, msilt, cmass, srlmass, srmass)
  
      ! clay
      cmass = 0.0
      do i = 1, nlay
         mclay(i) = getLayMass( clay(i), laythk(i), vfrock(i), bulkden(i), organic(i) )
         cmass = cmass + mclay(i)
      end do
      call mixproc(tillmix, nlay, mclay, cmass, srlmass, srmass)
  
      ! organic matter      
      cmass = 0.0
      do i = 1, nlay
         morganic(i) = getLayMass( organic(i), laythk(i), vfrock(i), bulkden(i))
         cmass = cmass + morganic(i)
      end do
      call mixproc(tillmix, nlay, morganic, cmass, srlmass, srmass)

      ! very course sand
      cmass = 0.0
      do i = 1, nlay
         mvc_sand(i) = getLayMass( vc_sand(i), laythk(i), vfrock(i), bulkden(i), organic(i) )
         cmass = cmass + mvc_sand(i)
      end do
      call mixproc(tillmix, nlay, mvc_sand, cmass, srlmass, srmass)
      
      ! course sand
      cmass = 0.0
      do i = 1, nlay
         mc_sand(i) = getLayMass( c_sand(i), laythk(i), vfrock(i), bulkden(i), organic(i) )
         cmass = cmass + mc_sand(i)
      end do
      call mixproc(tillmix, nlay, mc_sand, cmass, srlmass, srmass)
      
      ! medium sand
      cmass = 0.0
      do i = 1, nlay
         mm_sand(i) = getLayMass( m_sand(i), laythk(i), vfrock(i), bulkden(i), organic(i) )
         cmass = cmass + mm_sand(i)
      end do
      call mixproc(tillmix, nlay, mm_sand, cmass, srlmass, srmass)
      
      ! fine sand
      cmass = 0.0
      do i = 1, nlay
         mf_sand(i) = getLayMass( f_sand(i), laythk(i), vfrock(i), bulkden(i), organic(i) )
         cmass = cmass + mf_sand(i)
      end do
      call mixproc(tillmix, nlay, mf_sand, cmass, srlmass, srmass)
      
      ! very fine sand
      cmass = 0.0
      do i = 1, nlay
         mvf_sand(i) = getLayMass( vf_sand(i), laythk(i), vfrock(i), bulkden(i), organic(i) )
         cmass = cmass + mvf_sand(i)
      end do
      call mixproc(tillmix, nlay, mvf_sand, cmass, srlmass, srmass)
      
      ! rock fragments
      cmass = 0.0
      do i = 1, nlay
         m_rock(i) = getLayMass( vfrock(i), laythk(i), den_rock )
         cmass = cmass + m_rock(i)
      end do
      call mixproc(tillmix, nlay, m_rock, cmass, srlmass, srmass)

      ! wet bulk density
      cmass = 0.0
      do i = 1, nlay
         ! find layer mass equivalent
         dum(i) = w_bd(i) * slmass(i)
         cmass = cmass + dum(i)
      end do
      call mixproc(tillmix, nlay, dum, cmass, srlmass, srmass)
      do i = 1, nlay
         ! adjust back to property
         dum(i) = dum(i) / slmass(i)
         ! set to new value
         w_bd(i) = dum(i)
      end do

      ! ph
      cmass = 0.0
      do i = 1, nlay
         ! find layer mass equivalent
         dum(i) = ph(i) * slmass(i)
         cmass = cmass + dum(i)
      end do
      call mixproc(tillmix, nlay, dum, cmass, srlmass, srmass)
      do i = 1, nlay
         ! adjust back to property
         dum(i) = dum(i) / slmass(i)
         ! set to new value
         ph(i) = dum(i)
      end do

      ! calcium carbonate
      cmass = 0.0
      do i = 1, nlay
         ! find layer mass equivalent
         dum(i) = calcarb(i) * slmass(i)
         cmass = cmass + dum(i)
      end do
      call mixproc(tillmix, nlay, dum, cmass, srlmass, srmass)
      do i = 1, nlay
         ! adjust back to property
         dum(i) = dum(i) / slmass(i)
         ! set to new value
         calcarb(i) = dum(i)
      end do

      ! cation exchange capacity
      cmass = 0.0
      do i = 1, nlay
         ! find layer mass equivalent
         dum(i) = cation(i) * slmass(i)
         cmass = cmass + dum(i)
      end do
      call mixproc(tillmix, nlay, dum, cmass, srlmass, srmass)
      do i = 1, nlay
         ! adjust back to property
         dum(i) = dum(i) / slmass(i)
         ! set to new value
         cation(i) = dum(i)
      end do

      ! linear extensibility
      cmass = 0.0
      do i = 1, nlay
         ! find layer mass equivalent
         dum(i) = lin_ext(i) * slmass(i)
         cmass = cmass + dum(i)
      end do
      call mixproc(tillmix, nlay, dum, cmass, srlmass, srmass)
      do i = 1, nlay
         ! adjust back to property
         dum(i) = dum(i) / slmass(i)
         ! set to new value
         lin_ext(i) = dum(i)
      end do

      ! aggregate density
      cmass = 0.0
      do i = 1, nlay
         ! find layer mass equivalent
         dum(i) = aggden(i) * slmass(i)
         cmass = cmass + dum(i)
      end do
      call mixproc(tillmix, nlay, dum, cmass, srlmass, srmass)
      do i = 1, nlay
         ! adjust back to property
         dum(i) = dum(i) / slmass(i)
         ! set to new value
         aggden(i) = dum(i)
      end do

      ! dry aggregate stability
      cmass = 0.0
      do i = 1, nlay
         ! find layer mass equivalent
         dum(i) = drystab(i) * slmass(i)
         cmass = cmass + dum(i)
      end do
      call mixproc(tillmix, nlay, dum, cmass, srlmass, srmass)
      do i = 1, nlay
         ! adjust back to property
         dum(i) = dum(i) / slmass(i)
         ! set to new value
         drystab(i) = dum(i)
      end do

      !******************SOIL VARIABLES********************	


      !****************HYDROLOGY VARIABLES********************	
 
      ! Soil water content
      cmass = 0.0
      do i = 1, nlay
         ! find layer mass equivalent
         dum(i) = soilwatr(i) * slmass(i)
         cmass = cmass + dum(i)
      end do
      call mixproc(tillmix, nlay, dum, cmass, srlmass, srmass)
      do i = 1, nlay
         ! adjust back to property
         dum(i) = dum(i) / slmass(i)
         ! set to new value
         soilwatr(i) = dum(i)
      end do

      ! soil saturated water content
      cmass = 0.0
      do i = 1, nlay
         ! find layer mass equivalent
         dum(i) = satwatr(i) * slmass(i)
         cmass = cmass + dum(i)
      end do
      call mixproc(tillmix, nlay, dum, cmass, srlmass, srmass)
      do i = 1, nlay
         ! adjust back to property
         dum(i) = dum(i) / slmass(i)
         ! set to new value
         satwatr(i) = dum(i)
      end do

      ! soil on third bar water content
      cmass = 0.0
      do i = 1, nlay
         ! find layer mass equivalent
         dum(i) = thrdbar(i) * slmass(i)
         cmass = cmass + dum(i)
      end do
      call mixproc(tillmix, nlay, dum, cmass, srlmass, srmass)
      do i = 1, nlay
         ! adjust back to property
         dum(i) = dum(i) / slmass(i)
         ! set to new value
         thrdbar(i) = dum(i)
      end do

      ! soil fifteen bar water content
      cmass = 0.0
      do i = 1, nlay
         ! find layer mass equivalent
         dum(i) = ftnbar(i) * slmass(i)
         cmass = cmass + dum(i)
      end do
      call mixproc(tillmix, nlay, dum, cmass, srlmass, srmass)
      do i = 1, nlay
         ! adjust back to property
         dum(i) = dum(i) / slmass(i)
         ! set to new value
         ftnbar(i) = dum(i)
      end do

      ! soil available water content
      cmass = 0.0
      do i = 1, nlay
         ! find layer mass equivalent
         dum(i) = avawatr(i) * slmass(i)
         cmass = cmass + dum(i)
      end do
      call mixproc(tillmix, nlay, dum, cmass, srlmass, srmass)
      do i = 1, nlay
         ! adjust back to property
         dum(i) = dum(i) / slmass(i)
         ! set to new value
         avawatr(i) = dum(i)
      end do

      ! soil brooks and corey exponent
      cmass = 0.0
      do i = 1, nlay
         ! find layer mass equivalent
         dum(i) = soilcb(i) * slmass(i)
         cmass = cmass + dum(i)
      end do
      call mixproc(tillmix, nlay, dum, cmass, srlmass, srmass)
      do i = 1, nlay
         ! adjust back to property
         dum(i) = dum(i) / slmass(i)
         ! set to new value
         soilcb(i) = dum(i)
      end do

      ! soil air entry potential
      cmass = 0.0
      do i = 1, nlay
         ! find layer mass equivalent
         dum(i) = soilair(i) * slmass(i)
         cmass = cmass + dum(i)
      end do
      call mixproc(tillmix, nlay, dum, cmass, srlmass, srmass)
      do i = 1, nlay
         ! adjust back to property
         dum(i) = dum(i) / slmass(i)
         ! set to new value
         soilair(i) = dum(i)
      end do

      ! soil saturated conductivity
      cmass = 0.0
      do i = 1, nlay
         ! find layer mass equivalent
         dum(i) = satcond(i) * slmass(i)
         cmass = cmass + dum(i)
      end do
      call mixproc(tillmix, nlay, dum, cmass, srlmass, srmass)
      do i = 1, nlay
         ! adjust back to property
         dum(i) = dum(i) / slmass(i)
         ! set to new value
         satcond(i) = dum(i)
      end do
      !******************HYDROLOGY VARIABLES********************	
 

      !**************DECOMPOSITION VARIABLES********************	
      ! need to mix both pools and layers for these next two variables 
      thisPlant => plant
      do while( associated(thisPlant) )
        ! plant exists

        ! mix all residues in the soil
        thisResidue => thisPlant%residue
        do while( associated(thisResidue) )

          cmass = 0.0
          do i = 1, nlay
            cmass = cmass + thisResidue%stemz(i)
          end do

          call mixproc(tillmix, nlay, thisResidue%stemz, cmass, srlmass, srmass)
          cmass = 0.0
          do i = 1, nlay
            cmass = cmass + thisResidue%stemz(i)
          end do

          cmass = 0.0
          do i = 1, nlay
            cmass = cmass + thisResidue%leafz(i)
          end do
          call mixproc(tillmix, nlay, thisResidue%leafz, cmass, srlmass, srmass)

          cmass = 0.0
          do i = 1, nlay
            cmass = cmass + thisResidue%storez(i)
          end do
          call mixproc(tillmix, nlay, thisResidue%storez, cmass, srlmass, srmass)

          cmass = 0.0
          do i = 1, nlay
            cmass = cmass + thisResidue%rootstorez(i)
          end do
          call mixproc(tillmix, nlay, thisResidue%rootstorez, cmass, srlmass, srmass)

          cmass = 0.0
          do i = 1, nlay
            cmass = cmass + thisResidue%rootfiberz(i)
          end do
          call mixproc(tillmix, nlay, thisResidue%rootfiberz, cmass, srlmass, srmass)

          ! go to next older residue in thisPlant
          thisResidue => thisResidue%olderResidue
        end do

        ! go to next older plant
        thisPlant => thisPlant%olderPlant
      end do
      !**************DECOMPOSITION VARIABLES********************

 
      !******************ASD MASS FRACTIONS********************	
      do j = 1, msieve
         cmass = 0.0
         do i = 1, nlay
           ! find layer mass equivalent
            dum(i) = massf(j,i) * slmass(i)
            cmass = cmass + dum(i)
         end do
         call mixproc(tillmix, nlay, dum, cmass, srlmass, srmass)
         do i = 1, nlay
            dum(i) = dum(i) / slmass(i)
            massf(j,i) = dum(i)
         end do
      end do
      !******************ASD MASS FRACTIONS********************	


      ! back calculate mass and volume fractions
      do i = 1, nlay
         ! fine soil total mass
         tmass = msand(i) + msilt(i) + mclay(i) + morganic(i)
         vfrock(i) = setVolFrac( m_rock(i), laythk(i), den_rock )
         organic(i) = morganic(i) / tmass
         ! inorganic fine soil total mass 
         tmass = msand(i) + msilt(i) + mclay(i)
         sand(i) = msand(i) / tmass
         silt(i) = msilt(i) / tmass
         clay(i) = mclay(i) / tmass
         vc_sand(i) = mvc_sand(i) / tmass
         c_sand(i) = mc_sand(i) / tmass
         m_sand(i) = mm_sand(i) / tmass
         f_sand(i) = mf_sand(i) / tmass
         vf_sand(i) = mvf_sand(i) / tmass
      end do

      !**********************BULK DENSITY**********************
      ! a weight based adjustment of bulk density results in a
      ! change in layer thickness. THIS MUST BE DONE LAST!!!
      cmass = 0.0
      do i = 1, nlay
         ! find layer mass equivalent
         dum(i) = bulkden(i) * slmass(i)
         cmass = cmass + dum(i)
      end do
      call mixproc(tillmix, nlay, dum, cmass, srlmass, srmass)
      do i = 1, nlay
         ! adjust back to property
         dum(i) = dum(i) / slmass(i)
         ! adjust layer thickness
         call setLayThick( laythk(i), vfrock(i), bulkden(i), dum(i))
         ! set to new value
         bulkden(i) = dum(i)
      end do

    end subroutine mix

    pure subroutine mixproc(u, nlay, xcomp, cmass, lmass, mass) 

      ! This subroutine perfoms the actual mixing process.

      ! + + + ARGUMENT DECLARATIONS + + +
      real, intent(in) :: u           ! mixing coefficient
      integer, intent(in) :: nlay     ! number of layers to be mixed
      real, intent(inout) :: xcomp(*) ! component value that is mixed
      real, intent(in) :: cmass       ! total mass of a component contained in a subregion
      real, intent(in) :: lmass(*)    ! total mass in soil layer
      real, intent(in) :: mass        ! total mass in a subregion

      ! + + + LOCAL VARIABLES + + +
      integer i        ! index for layers in a subregion
      real mixed(nlay) ! temporary variable containing the mixed component

      ! + + + END SPECIFICATIONS + + + 

      ! Do the mixing process. 
      do i = 1, nlay
         mixed(i) = (1-u)*xcomp(i) + (lmass(i)/mass)*u*cmass
         xcomp(i) = mixed(i) 
      end do
	  
    end subroutine mixproc

    subroutine invert (nlay, bulkden, laythk, &
                     sand, silt, clay, vfrock, &
                     vc_sand, c_sand, m_sand, f_sand, vf_sand, &
                     w_bd, organic, ph, calcarb, cation, &
                     lin_ext, aggden, drystab, &
                     soilwatr, satwatr, thrdbar, ftnbar, &
                     reswatr, avawatr, &
                     soilcb, soilair, satcond, rstwatr, &
                     plant, massf)


      ! This subroutine reads in the array(s) containing the components 
      ! that need to be inverted.  It then calls the subroutine invproc 
      ! and the actual inversion process is performed.

      use asd_mod, only: msieve
      use biomaterial, only: plant_pointer, residue_pointer
      use soilden_mod, only: setLayThick

      ! + + + ARGUMENT DECLARATIONS + + +
      integer, intent(in) :: nlay       ! number of soil layers used
      real, intent(inout) :: bulkden(*) ! fine soil bulk density 
      real, intent(inout) :: laythk(*)  ! layer thickness
      real, intent(inout) :: sand(*)    ! fraction of sand
      real, intent(inout) :: silt(*)    ! fraction of silt
      real, intent(inout) :: clay(*)    ! fraction of clay
      real, intent(inout) :: vfrock(*)  ! volume fraction of rock
      real, intent(inout) :: vc_sand(*) ! fraction of very course sand
      real, intent(inout) :: c_sand(*)  ! fraction of course sand
      real, intent(inout) :: m_sand(*)  ! fraction of medium sand
      real, intent(inout) :: f_sand(*)  ! fraction of fine sand
      real, intent(inout) :: vf_sand(*) ! fraction of very fine sand
      real, intent(inout) :: w_bd(*)    ! wet (1/3 bar) fine soil density
      real, intent(inout) :: organic(*) ! fraction of organic matter
      real, intent(inout) :: ph(*)      ! soil Ph
      real, intent(inout) :: calcarb(*) ! fraction of calcium carbonate
      real, intent(inout) :: cation(*)  ! cation exchange capcity
      real, intent(inout) :: lin_ext(*) ! linear extensibility
      real, intent(inout) :: aggden(*)  ! aggregrate density
      real, intent(inout) :: drystab(*) ! dry aggregrate stability
      real, intent(inout) :: soilwatr(*) ! soil water content (mass bases)
      real, intent(inout) :: satwatr(*) ! saturation soil water content
      real, intent(inout) :: thrdbar(*) ! 1/3 bar soil water content
      real, intent(inout) :: ftnbar(*)  ! 15 bar soil water content
      real, intent(inout) :: reswatr(*) ! residual water content
      real, intent(inout) :: avawatr(*) ! available soil water content
      real, intent(inout) :: soilcb(*)  ! soil CB value
      real, intent(inout) :: soilair(*) ! soil air entery potential
      real, intent(inout) :: satcond(*) ! saturated hydraulic conductivity
      real, intent(inout) :: rstwatr(*) ! reduced saturation water content
      type(plant_pointer), pointer :: plant ! pointer to youngest plant data, which chains to older plant data
      real, dimension(msieve+1,*), intent(inout) :: massf ! mass fractions for sieve cuts

      ! + + + LOCAL VARIABLES + + +
      integer :: j  ! loop variable on asd sieves 
      integer :: k  ! loop variable on the number of layers
      real :: dum(nlay) ! dummy variable containing a variable array to be passed to the inversion process routine
      type(plant_pointer), pointer :: thisPlant
      type(residue_pointer), pointer :: thisResidue

      ! + + + END SPECIFICATIONS + + + 

      ! Make calls to the inversion process for all variables that need 
      ! to be inverted. 

      !******************SOIL VARIABLES********************	
      call invproc(nlay,laythk,sand)
      call invproc(nlay,laythk,silt)
      call invproc(nlay,laythk,clay)
      call invproc(nlay,laythk,vfrock)

      call invproc(nlay,laythk,vc_sand)
      call invproc(nlay,laythk,c_sand)
      call invproc(nlay,laythk,m_sand)
      call invproc(nlay,laythk,f_sand)
      call invproc(nlay,laythk,vf_sand)

      call invproc(nlay,laythk,w_bd)
      call invproc(nlay,laythk,organic)
      call invproc(nlay,laythk,ph)
      call invproc(nlay,laythk,calcarb)
      call invproc(nlay,laythk,cation)

      call invproc(nlay,laythk,lin_ext)

      call invproc(nlay,laythk,aggden)
      call invproc(nlay,laythk,drystab)
      !******************SOIL VARIABLES********************	

      !******************HYDROLOGY VARIABLES********************	
      call invproc(nlay,laythk,soilwatr)
      call invproc(nlay,laythk,satwatr)
      call invproc(nlay,laythk,thrdbar)
      call invproc(nlay,laythk,ftnbar)
      call invproc(nlay,laythk,reswatr)
      call invproc(nlay,laythk,avawatr)

      call invproc(nlay,laythk,soilcb)
      call invproc(nlay,laythk,soilair)
      call invproc(nlay,laythk,satcond)
      call invproc(nlay,laythk,rstwatr)
      !******************HYDROLOGY VARIABLES********************	

      !******************ASD MASS FRACTIONS********************	
      ! need to invert mass fractions for all sieve cuts and layers 

      do j = 1, msieve
         do k = 1, nlay
            dum(k)=massf(j,k)
         end do
         call invproc(nlay,laythk,dum)
         do k = 1, nlay
            massf(j,k)=dum(k)
         end do
      end do
      !******************ASD MASS FRACTIONS********************	


      !******************RESIDUE MASS VARIABLES********************	
      ! need to invert each pool for these

      thisPlant => plant
      do while( associated(thisPlant) )
        ! plant exists

        ! mix all residues in the soil
        thisResidue => thisPlant%residue
        do while( associated(thisResidue) )

          do k = 1, nlay
            dum(k) = thisResidue%stemz(k) / laythk(k)
          end do
          call invproc(nlay,laythk,dum)
          do k = 1, nlay
            thisResidue%stemz(k) = dum(k) * laythk(k)
          end do

          do k = 1, nlay
            dum(k) = thisResidue%leafz(k) / laythk(k)
          end do
          call invproc(nlay,laythk,dum)
          do k = 1, nlay
            thisResidue%leafz(k) = dum(k) * laythk(k)
          end do

          do k = 1, nlay
            dum(k) = thisResidue%storez(k) / laythk(k)
          end do
          call invproc(nlay,laythk,dum)
          do k = 1, nlay
            thisResidue%storez(k) = dum(k) * laythk(k)
          end do

          do k = 1, nlay
            dum(k) = thisResidue%rootstorez(k) / laythk(k)
          end do
          call invproc(nlay,laythk,dum)
          do k = 1, nlay
            thisResidue%rootstorez(k) = dum(k) * laythk(k)
          end do

          do k = 1, nlay
            dum(k) = thisResidue%rootfiberz(k) / laythk(k)
          end do
          call invproc(nlay,laythk,dum)
          do k = 1, nlay
            thisResidue%rootfiberz(k) = dum(k) * laythk(k)
          end do

          ! go to next older residue in thisPlant
          thisResidue => thisResidue%olderResidue
        end do

        ! go to next older plant
        thisPlant => thisPlant%olderPlant
      end do
      !******************RESIDUE MASS VARIABLES********************
		  
      do k = 1, nlay
         dum(k) = bulkden(k)
      end do
      call invproc(nlay,laythk,dum)
      do k = 1, nlay
         ! adjust layer thickness
         call setLayThick( laythk(k), vfrock(k), bulkden(k), dum(k))
         ! set to new value
         bulkden(k) = dum(k)
      end do

    end subroutine invert

    subroutine invproc(nlay,thick,xcomp) 

      ! + + + PURPOSE + + +
      ! Invert the component passed to xcomp 
  
      ! + + + KEYWORDS + + +
      ! inversion, tillage 

      ! + + + ARGUMENT DECLARATIONS + + +
      real xcomp(*), thick(*)
      integer nlay

      ! + + + ARGUMENT DEFINITIONS + + +
      ! nlay		- number of soil layers used
      ! thick		- thickness of each layer in a subregion
      ! xcomp		- component that needs inverting

      ! + + + LOCAL VARIABLES + + +
      integer   idx, odx
      real      ithick(nlay), ixcomp(nlay)
      real      othick(nlay)
      real      dthick

      ! + + + LOCAL VARIABLE DEFINITIONS + + +
      ! idx      - input index for layers
      ! odx      - output index for layers
      ! ithick   - inverted thickness of layers
      ! ixcomp   - inverted property of layers
      ! othick   - temp thickness
      ! dthick   - delta thickness

      ! create inverted layer thickness and property arrays
      ! and zero out output array

      do idx = 1, nlay
        ithick(idx) = thick(nlay-idx+1)
        ixcomp(idx) = xcomp(nlay-idx+1)
        xcomp(nlay-idx+1) = 0.0
        othick(idx) = thick(idx)
      end do

      if( nlay .gt. 0 ) then
        idx = 1
        odx = 1

   20   dthick = min(ithick(idx), othick(odx))
        xcomp(odx) = xcomp(odx) + ixcomp(idx) * dthick
        ithick(idx) = ithick(idx) - dthick
        othick(odx) = othick(odx) - dthick
        if (ithick(idx).eq.0.0) idx = idx + 1
        if (othick(odx).eq.0.0) odx = odx + 1
        if (idx.le.nlay.and.odx.le.nlay) goto 20
      end if

      do odx = 1, nlay
        xcomp(odx) = xcomp(odx) / thick(odx)
      end do

      return
    end subroutine invproc

    subroutine loosn (u, tillf, nlay, bulkden, sbd, laythk, vfrock)
      ! This subroutine changes the fine soil bulk density and updates
      ! layer thickness and rock volume fraction based on conservation of mass

      use soilden_mod, only: setLayThick

      ! + + + ARGUMENT DECLARATIONS + + +
      integer, intent(in) :: nlay       ! number of soil layers used
      real, intent(in) :: u             ! loosening coefficient
      real, intent(in) :: tillf         ! fraction of soil area tilled by the machine
      real, intent(inout) :: bulkden(*) ! present soil bulk density
      real, intent(in) :: sbd(*)        ! settled soil bulk density
      real, intent(inout) :: laythk(*)  ! layer thickness
      real, intent(inout) :: vfrock(*)  ! rock volume fraction

      ! + + + LOCAL VARIABLES + + +
      integer :: i       ! loop variable on layers
      real :: dum(nlay)  ! dummy variable used in calculating the mass

      ! + + + END SPECIFICATIONS + + + 

      ! perform the loosen process on the layers in a subregion 
      do i=1,nlay
         dum(i)= bulkden(i)-((bulkden(i)-(2.0/3.0)*sbd(i))*u*tillf)
         call setLayThick( laythk(i), vfrock(i), bulkden(i), dum(i))
         bulkden(i)=dum(i)
      end do

    end subroutine loosn

    subroutine compact( u, load, tillf, tlay, nlay, bulkden, settled_bd, proc_bd_wc, proc_bd, laythk, vfrock )
      ! This subroutine compacts soil layers. 

      use p1unconv_mod, only: pi, MgtoN, mmtom
      use soilden_mod, only: setLayThick

      ! + + + ARGUMENT DECLARATIONS + + +
      integer, intent(in) :: tlay       ! starting soil layer for compaction
      integer, intent(in) :: nlay       ! total number of soil layers in horizon
      real, intent(in) :: u             ! Compaction coefficient
      real, intent(in) :: load          ! Compaction load (Mg, Megagrams) (also known as metric ton)
      real, intent(in) :: tillf         ! fraction of soil area tilled by the machine
      real, intent(inout) :: bulkden(*) ! present soil bulk density (Mg/m^3)
      real, intent(in) :: settled_bd(*) ! settled soil bulk density
      real, intent(in) :: proc_bd_wc(*) ! proctor soil bulk density adjusted for water content (Mg/m^3)
      real, intent(in) :: proc_bd(*)    ! proctor soil bulk density (maximum dry density) (Mg/m^3)
      real, intent(inout) :: laythk(*)  ! layer thickness (mm)
      real, intent(inout) :: vfrock(*)  ! rock volume fraction

      ! + + + LOCAL VARIABLES + + +
      integer :: blay       ! bottom soil layer to be compacted
      integer :: i          ! loop variable on layers 
      integer :: concfactor_harder  ! concentration factor for the harder soil condition
      integer :: concfactor_softer  ! concentration factor for the softer soil condition
      real :: concfactor_interp     ! interpolated concentration factor
      real :: eff_depth(nlay)! effective depth to bottom of soil layer accounting for stress propagation in upper layers (m)
      real :: force         ! load converted to force (N, newtons)
      real :: dum(nlay)     ! dummy variable used in calculating the adjusted density
      real :: interpfactor  ! interpolation factor in proportion to present bulk density
      real :: lay_stress_top  ! stress at top of soil layer (Pa, pascals)
      real :: lay_stress_bot  ! stress at bottom of soil layer (Pa, pascals)
      real :: bear_interp   ! interpolated soil load bearing capacity for layer (Pa, pascals)
      real :: eff_depth_avg ! average of effective depth at top and bottom of layer (m)
      real :: u_adj         ! compaction coefficient adjusted for depth in soil compation zone

      real, parameter :: bear_max = 95760.52 ! very hard soil load bearing (Pa, pascals)
      real, parameter :: bear_min = 47880.26 ! soft soil load bearing (Pa, pascals)

      ! + + + END SPECIFICATIONS + + + 

      ! beginning stress at the top of soil layer (point load assumption) is infinite.
      lay_stress_top = huge(lay_stress_top)
      ! convert load to force
      force = load * MgtoN

      ! find stress for each layer
      do i = tlay, nlay
        ! interpolate concentration factor between soft, firm, hard, and very hard conditions which affect
        ! the depth of the stress distribution. soft = 6, firm = 5, hard = 4, very hard = 3
        ! Smith, D.L.0. 1985. Compaction by wheels: a numerical model for agricultural soils.
        ! Journal of Soil Science, vol.36:,621-632
        if( bulkden(i) .le. settled_bd(i) ) then
          concfactor_harder = 5
          concfactor_softer = 6
          interpfactor = ( ((2.0/3.0)*settled_bd(i) - bulkden(i)) / ((2.0/3.0)*settled_bd(i) - settled_bd(i)) )
        elseif( (bulkden(i) .gt. settled_bd(i)) &
          .and. (bulkden(i) .le. proc_bd_wc(i)) ) then
          concfactor_harder = 4
          concfactor_softer = 5
          interpfactor = ( (settled_bd(i) - bulkden(i)) / (settled_bd(i) - proc_bd_wc(i)) )
        elseif( bulkden(i) .lt. proc_bd_wc(i) ) then
          concfactor_harder = 3
          concfactor_softer = 4
          interpfactor = ( (proc_bd_wc(i) - bulkden(i)) / (proc_bd_wc(i) - proc_bd(i)) )
        end if
        concfactor_interp = concfactor_softer - (concfactor_softer-concfactor_harder) * interpfactor
        bear_interp = bear_min + (bear_max - bear_min) * (6.0-concfactor_interp)/3.0
 
        ! propagate the load into the soil layer by layer using the stress penetration of Soehne (1958)
        ! Soehne, W., 1958. Fundamentals of pressure distribution and soil compaction under tractor tyres.
        ! Agric. Eng. 39, 276-281.
        ! stop propagating compaction at layer where compaction pressure is less than the load bearing pressure.

        ! to accomodate varying soil concentration factors, effective depth must be back calculated
        ! to reflect stress in layers above
        eff_depth(i) = laythk(i)*mmtom + (force * concfactor_interp / (2.0*pi*lay_stress_top))**0.5
    
        ! find the stress at the bottom of layer
        lay_stress_bot = force * concfactor_interp / (2.0*pi*eff_depth(i)*eff_depth(i))

        ! find thickness of compaction layer as point where stress is less than bearing capacity
        if( lay_stress_bot .le. bear_interp ) then
          blay = i
          exit
        end if

        ! set values for next layer
        lay_stress_top = lay_stress_bot
      end do

      do i = tlay, blay
        ! effective depth at middle of layer
        if( i .eq. 1 ) then
          eff_depth_avg = eff_depth(i) / 2.0
        else
          eff_depth_avg = (eff_depth(i) + eff_depth(i-1)) / 2.0
        end if

        ! find ajusted compaction coefficient
        u_adj = u * (1.0 - eff_depth_avg/eff_depth(blay))

        ! compaction - linear decrease to depth of influence
        dum(i) = bulkden(i) + ( (proc_bd_wc(i) - bulkden(i)) * u_adj * tillf )
        call setLayThick( laythk(i), vfrock(i), bulkden(i), dum(i))
        bulkden(i) = dum(i)
      end do

      return
    end subroutine compact

    subroutine crush (alpha, beta,nlay,mf)

      use asd_mod, only: msieve, nsieve, mdia
      use binomial_mod, only: bino

!     + + + PURPOSE + + +
!     This subroutine  performs the crushing or breaking down of
!     soil aggregates into smaller sizes based on the initial aggregate
!     size distribution and two crushing parameters (alpha and beta).
!     The crushing parameters are assumed to be a function of the
!     soil intrinsic properties, soil water content, and tillage implement.
!     
!     + + + KEYWORDS + + +
!     aggregate size distribution, asd, sieves, mass fractions
!
!     + + + ARGUMENT DECLARATIONS + + +
      real    alpha, beta
      integer nlay
      real, dimension(msieve+1,*) :: mf
!
!
!     + + + ARGUMENT DEFINITIONS + + +
!
!     alpha  - Aggregate Size Distribution Factor
!     beta   - Crushing Intensity Factor
!     nlay   - number of soil layers used
!     mf     - mass fractions of aggregates within sieve cuts
!              (sum of all mass fractions are expected to = 1.0)
!
!     + + + ACCESSED COMMON BLOCK VARIABLE DEFINITIONS + + +
!
!     mdia   - array containing geometric mean diameters of sieve cuts
!     nsieve - number of sieves used

      real     pmat(msieve+1,msieve+1)
      real     dratio
      real     prob
      real     chk
      integer  i, j, k, m
      real     predmf(msieve+1)

!     pmat   - probability matrix
!     dratio - ratio of sieve cut d to maximum sieve cut d
!     prob   - probability value
!     chk    - variable to chk prob matrix integrity
!     i      - loop variable for sieve cut sizes
!     j      - loop variable for soil layers
!     k      - loop variable for sieve cut probabilities
!     predmf - local array to hold predicted mass fractions
!              before updating mf

!     for each soil layer
      do 500 j=1,nlay
!         compute transition matrix
          do 100 i=1,nsieve+1
              dratio = mdia(i)/mdia(nsieve+1)
              prob = 1.0d0 - exp(-dble(alpha)+dratio*dble(beta))
              chk = 0.0
              do 50 k=1,i
                  pmat(i,k) = bino(i-1,k-1,prob)
                  chk = chk+pmat(i,k)
 50           continue
              if (abs(chk-1.0) .gt. 0.001) then
                  write(0,*) 'Problem transition matrix (crush) chk:',  &
     &                    (chk-1.0)
!                 debug code to print out transition matrix
                  do 2 k=nsieve+1,1,-1
                      print*,(pmat(k,m), m=k,1,-1)
2                 continue
                  call exit (1)
              endif
100       continue
          do 300 i=1,nsieve+1
              predmf(i) = 0.0
              do 200 k=i,nsieve+1
                  predmf(i) = predmf(i) + mf(k,j) * pmat(k,i)
200           continue
300       continue
!         put predicted mass fractions into mf
          do 400 i=1,nsieve+1
              mf(i,j) = predmf(i)
400       continue
500   continue
      return
    end subroutine crush

    SUBROUTINE set_asd (gmdx, gsdx, mnot, minf, nlay, soil)

      USE soil_data_struct_defs, only: soil_def
      TYPE(soil_def), INTENT(INOUT) :: soil

      !     + + + PURPOSE + + +
      ! This subroutine assigns the ASD modified lognormal parameters,
      ! e.g., the modified lognormal (transformed) GMD and GSD values
      ! as well as the GMDmin and GMDmax values
      ! to all soil layers within the specified depth.

      ! If the user is interested in setting different ASD values to different
      ! soil layers (depths) they should call this process repeatedly with
      ! smaller and smaller soil depths specified.

      ! Currently assumes we have "logcas = 3" condition (mnot != 0, minf != infinity)

      !     + + + ARGUMENT DECLARATIONS + + +
      REAL, INTENT (IN)    :: gmdx, gsdx
      REAL, INTENT (IN)    :: mnot, minf
      INTEGER, INTENT (IN) :: nlay


      ! + + + ARGUMENT DEFINITIONS + + +
      ! gmdx    - geometric mean diameter of aggregate size distribution
      !          (or transformed gmd for "modified" lognormal cases)
      ! gsdx    - geometric standard deviation of aggregate size distribution
      !          (or transformed gsd for "modified" lognormal cases)
      ! mnot    - minimum aggregate size in aggregate size distribution
      !          (for "modified" lognormal cases)
      ! minf    - maximum aggregate size in aggregate size distribution
      !          (for "modified" lognormal cases)
      ! nlay   - number of soil layers used


      ! + + + LOCAL VARIABLES + + +
      INTEGER :: j

      ! + + + LOCAL VARIABLE DEFINITIONS + + +
      ! j      - loop variable for soil layers

      IF (nlay .ge. 1) THEN    !for each soil layer
         DO j=1,nlay
            soil%aslagm(j) = gmdx
            soil%as0ags(j) = gsdx
            soil%aslagn(j) = mnot
            soil%aslagx(j) = minf
         END DO
      ELSE
         write (0,*) "Depth specified is negative, ASD values not assigned."
      END IF

    END SUBROUTINE set_asd

    SUBROUTINE set_wc (wc, nlay, soil)

      USE soil_data_struct_defs, only: soil_def
      TYPE(soil_def), INTENT(INOUT) :: soil

      !     + + + PURPOSE + + +
      ! This subroutine assigns the water content values,
      ! to all soil layers within the specified depth.

      ! If the user is interested in setting different water content values to different
      ! soil layers (depths) they should call this process repeatedly with
      ! smaller and smaller soil depths specified.

      !     + + + KEYWORDS + + +
      !     soil layer, wc

      !     + + + ARGUMENT DECLARATIONS + + +
      REAL, INTENT (IN)    :: wc
      INTEGER, INTENT (IN) :: nlay

      ! + + + ARGUMENT DEFINITIONS + + +
      ! wc      - water content (Mg/Mg)
      ! nlay   - number of soil layers used

      ! + + + LOCAL VARIABLES + + +
      INTEGER :: j

      ! + + + LOCAL VARIABLE DEFINITIONS + + +
      ! j      - loop variable for soil layers

      IF (nlay .ge. 1) THEN    !for each soil layer
         DO j=1,nlay
            soil%ahrwc(j) = wc
         END DO
      ELSE
         write (0,*) "Depth specified is negative, water content values not assigned."
      END IF

    END SUBROUTINE set_wc

end module mproc_soil_mod
