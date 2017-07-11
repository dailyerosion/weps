!$Author$
!$Date$
!$Revision$
!$HeadURL$

! NOTE:  This subroutine needs other components to be passed to it so
!        they can be mixed.  Currently this is not done.  I need to get
!        together with L. Wagner on this.  A.N.Hawkins 8/1/95 

      subroutine mix                                                    &
     &              (u,tillf,nlay,density,laythk,                       &
     &               sand,silt,clay, rock_vol,                          &
     &               c_sand, m_sand, f_sand, vf_sand,                   &
     &               w_bd,                                              &
     &               organic, ph, calcarb, cation,                      &
     &               lin_ext,                                           &
     &               aggden, drystab,                                   &
     &               soilwatr,                                          &
     &               satwatr, thrdbar, ftnbar,                          &
     &               avawatr,                                           &
     &               soilcb,soilair,satcond,                            &
     &               residue, massf)

!     + + + PURPOSE + + +
!     This subroutine reads in the array(s) containing the components 
!     that need to be mixed.  It then calls the subroutine mixproc
!     and the actual mixing process is performed.

!     + + + KEYWORDS + + +
!     mixing 

      use asd_mod, only: msieve
      use weps_interface_defs, ignore_me=>mix
      use biomaterial, only: biomatter

!     + + + ARGUMENT DECLARATIONS + + +
      integer nlay
      real u,tillf,density(*),laythk(*)
      real sand(*),silt(*),clay(*), rock_vol(*)
      real c_sand(*), m_sand(*), f_sand(*), vf_sand(*)
      real w_bd(*)
      real organic(*), ph(*), calcarb(*), cation(*)
      real lin_ext(*)
      real aggden(*), drystab(*)
      real soilwatr(*)
      real satwatr(*), thrdbar(*), ftnbar(*)
      real avawatr(*)
      real soilcb(*), soilair(*), satcond(*)
      type(biomatter), dimension(:), intent(inout) :: residue
      real, dimension(msieve+1,*) :: massf

!     + + + ARGUMENT DEFINITIONS + + +
!     u           - mixing coefficient
!     tillf       - fraction of the soil area tilled by the machine
!     density     - soil density 
!     laythk      - layer thickness

!     sand        - fraction of sand
!     silt        - fraction of silt
!     clay        - fraction of clay
!     rock_vol    - volume fraction of rock
!     c_sand      - fraction of course sand
!     m_sand      - fraction of medium sand
!     f_sand      - fraction of fine sand
!     vf_sand     - fraction of very fine sand

!     w_bd        - wet (1/3 bar) soil density 

!     organic     - fraction of organic matter
!     ph          - soil Ph
!     calcarb     - fraction of calcium carbonate
!     cation      - cation exchange capcity

!     lin_ext     - linear extensibility

!     aggden      - aggregrate density
!     drystab     - dry aggregrate stability

!     soilwatr    - soil water content (mass bases)
!     satwatr     - saturation soil water content
!     thrdbar     - 1/3 bar soil water content
!     ftnbar      - 15 bar soil water content
!     avawatr     - available soil water content

!     soilcbr     - soil CB value
!     soilair     - soil air entery potential
!     satcond     - saturated hydraulic conductivity

!     residue     - structure containing residue by soil layer
!     massf       - mass fractions for sieve cuts

!     nlay        - number of soil layers used

!     + + + LOCAL VARIABLES + + +
      real tillmix,dum(nlay), dum1(nlay), dum2(nlay), mass, cmass
      integer  i,j

!     + + + LOCAL VARIABLE DEFINITIONS + + +
!     tillmix - combination of mixing coefficient and tilled area fraction
!     cmass = total mass of a component contained in a subregion 
!     dum = dummy variable used in calculating the mass in a subregion
!     dum1 = dummy variable used in calculating mass of a component
!            in a subregion
!     lay = number of layers in a specified subregion
!     mass = total mass in a subregion 

!     + + + END SPECIFICATIONS + + + 

!     Print the initial masses calculated above

!      print*,'initial data - before mixing'
!         do 230 i=1,5 
!            print*, rootm(i,1)  
!230      continue

!     find combination coefficient based on fraction of area and mixing
      tillmix = u*tillf
	  
!     Calculate the total mass in all layers within a subregion
!
      mass = 0.0
      do 25 i=1,nlay
         dum(i) =density(i)*laythk(i)+mass
         mass = dum(i)
25    continue

!     Make calls to the mixing process.  First need to calculate
!     the total mass of the component to be mixed.  This is then passed
!     in the call.

!     Need to calculate the component mass before making the call
!     to mixproc for each and every component.  This is then passed to
!     mixproc and used in the mix calculation. 

!************************SOIL VARIABLES********************	
      cmass = 0.0
      do 50 i=1,nlay
         dum1(i) = density(i)*laythk(i)*sand(i)+cmass
         cmass = dum1(i)
50    continue
      call mixproc(tillmix, nlay, sand, cmass, mass)

      cmass = 0.0
      do 51 i=1,nlay
         dum1(i) = density(i)*laythk(i)*silt(i)+cmass
         cmass = dum1(i)
51    continue
      call mixproc(tillmix, nlay, silt, cmass, mass)
  
      cmass = 0.0
      do 52 i=1,nlay
         dum1(i) = density(i)*laythk(i)*clay(i)+cmass
         cmass = dum1(i)
52    continue
      call mixproc(tillmix, nlay, clay, cmass, mass)
  
      cmass = 0.0
      do 520 i=1,nlay
         dum1(i) = density(i)*laythk(i)*rock_vol(i)+cmass    ! NOTE: we are mixing rock vol on mass not volume ratio
         cmass = dum1(i)
520    continue
      call mixproc(tillmix, nlay, rock_vol, cmass, mass)

      cmass = 0.0
      do 521 i=1,nlay
         dum1(i) = density(i)*laythk(i)*c_sand(i)+cmass
         cmass = dum1(i)
521   continue
      call mixproc(tillmix, nlay, c_sand, cmass, mass)

      cmass = 0.0
      do 522 i=1,nlay
         dum1(i) = density(i)*laythk(i)*m_sand(i)+cmass
         cmass = dum1(i)
522   continue
      call mixproc(tillmix, nlay, m_sand, cmass, mass)

      cmass = 0.0
      do 523 i=1,nlay
         dum1(i) = density(i)*laythk(i)*f_sand(i)+cmass
         cmass = dum1(i)
523   continue
      call mixproc(tillmix, nlay, f_sand, cmass, mass)

      cmass = 0.0
      do 524 i=1,nlay
         dum1(i) = density(i)*laythk(i)*vf_sand(i)+cmass
         cmass = dum1(i)
524   continue
      call mixproc(tillmix, nlay, vf_sand, cmass, mass)

      cmass = 0.0
      do 525 i=1,nlay
         dum1(i) = density(i)*laythk(i)*w_bd(i)+cmass       !NOTE: mixed w_bd on mass basis (not entirely correct)
         cmass = dum1(i)
525   continue
      call mixproc(tillmix, nlay, w_bd, cmass, mass)

      cmass = 0.0
      do 53 i=1,nlay
         dum1(i) = density(i)*laythk(i)*organic(i)+cmass
         cmass = dum1(i)
53    continue
      call mixproc(tillmix, nlay, organic, cmass, mass)

      cmass = 0.0
      do 54 i=1,nlay
         dum1(i) = density(i)*laythk(i)*ph(i)+cmass
         cmass = dum1(i)
54    continue
      call mixproc(tillmix, nlay, ph, cmass, mass)

      cmass = 0.0
      do 55 i=1,nlay
         dum1(i) = density(i)*laythk(i)*calcarb(i)+cmass
         cmass = dum1(i)
55    continue
      call mixproc(tillmix, nlay, calcarb, cmass, mass)

      cmass = 0.0
      do 56 i=1,nlay
         dum1(i) = density(i)*laythk(i)*cation(i)+cmass
         cmass = dum1(i)
56    continue
      call mixproc(tillmix, nlay, cation, cmass, mass)

      cmass = 0.0
      do 58 i=1,nlay
         dum1(i) = density(i)*laythk(i)*lin_ext(i)+cmass
         cmass = dum1(i)
58    continue
      call mixproc(tillmix, nlay, lin_ext, cmass, mass)

      cmass = 0.0
      do 64 i=1,nlay
         dum1(i) = density(i)*laythk(i)*aggden(i)+cmass
         cmass = dum1(i)
64    continue
      call mixproc(tillmix, nlay, aggden, cmass, mass)

      cmass = 0.0
      do 65 i=1,nlay
         dum1(i) = density(i)*laythk(i)*drystab(i)+cmass
         cmass = dum1(i)
65    continue
      call mixproc(tillmix, nlay, drystab, cmass, mass)

!************************SOIL VARIABLES********************	
!
!**********************HYDROLOGY VARIABLES********************	
! 
      cmass = 0.0
      do 101 i=1,nlay
         dum1(i) = density(i)*laythk(i)*soilwatr(i)+cmass
         cmass = dum1(i)
101   continue
      call mixproc(tillmix, nlay, soilwatr, cmass, mass)

      cmass = 0.0
      do 102 i=1,nlay
         dum1(i) = density(i)*laythk(i)*satwatr(i)+cmass
         cmass = dum1(i)
102   continue
      call mixproc(tillmix, nlay, satwatr, cmass, mass)

      cmass = 0.0
      do 103 i=1,nlay
         dum1(i) = density(i)*laythk(i)*thrdbar(i)+cmass
         cmass = dum1(i)
103   continue
      call mixproc(tillmix, nlay, thrdbar, cmass, mass)

      cmass = 0.0
      do 104 i=1,nlay
         dum1(i) = density(i)*laythk(i)*ftnbar(i)+cmass
         cmass = dum1(i)
104   continue
      call mixproc(tillmix, nlay, ftnbar, cmass, mass)

      cmass = 0.0
      do 105 i=1,nlay
         dum1(i) = density(i)*laythk(i)*avawatr(i)+cmass
         cmass = dum1(i)
105   continue
      call mixproc(tillmix, nlay, avawatr, cmass, mass)

      cmass = 0.0
      do 106 i=1,nlay
         dum1(i) = density(i)*laythk(i)*soilcb(i)+cmass
         cmass = dum1(i)
106   continue
      call mixproc(tillmix, nlay, soilcb, cmass, mass)

      cmass = 0.0
      do 107 i=1,nlay
         dum1(i) = density(i)*laythk(i)*soilair(i)+cmass
         cmass = dum1(i)
107   continue
      call mixproc(tillmix, nlay, soilair, cmass, mass)

      cmass = 0.0
      do 108 i=1,nlay
         dum1(i) = density(i)*laythk(i)*satcond(i)+cmass
         cmass = dum1(i)
108   continue
      call mixproc(tillmix, nlay, satcond, cmass, mass)
!************************HYDROLOGY VARIABLES********************	
! 
!
!********************DECOMPOSITION VARIABLES********************	
!   need to mix both pools and layers for these next two variables 
      do j=1,size(residue)

         cmass = 0.0
         do i=1,nlay
            dum1(i) = density(i)*laythk(i)*residue(j)%mass%stemz(i) + cmass
            cmass = dum1(i)
            dum2(i) = residue(j)%mass%stemz(i)
         end do
         call mixproc(tillmix, nlay, dum2(1), cmass, mass)
         do i=1,nlay
            residue(j)%mass%stemz(i) = dum2(i)
         end do

         cmass = 0.0
         do i=1,nlay
            dum1(i) = density(i)*laythk(i)*residue(j)%mass%leafz(i) + cmass
            cmass = dum1(i)
            dum2(i) = residue(j)%mass%leafz(i)
         end do
         call mixproc(tillmix, nlay, dum2(1), cmass, mass)
         do i=1,nlay
            residue(j)%mass%leafz(i) = dum2(i)
         end do

         cmass = 0.0
         do i=1,nlay
            dum1(i) = density(i)*laythk(i)*residue(j)%mass%storez(i) + cmass
            cmass = dum1(i)
            dum2(i) = residue(j)%mass%storez(i)
         end do
         call mixproc(tillmix, nlay, dum2(1), cmass, mass)
         do i=1,nlay
            residue(j)%mass%storez(i) = dum2(i)
         end do

         cmass = 0.0
         do i=1,nlay
            dum1(i) = density(i)*laythk(i)*residue(j)%mass%rootstorez(i) + cmass
            cmass = dum1(i)
            dum2(i) = residue(j)%mass%rootstorez(i)
         end do
         call mixproc(tillmix, nlay, dum2(1), cmass, mass)
         do i=1,nlay
            residue(j)%mass%rootstorez(i) = dum2(i)
         end do

         cmass = 0.0
         do i=1,nlay
            dum1(i) = density(i)*laythk(i)*residue(j)%mass%rootfiberz(i) + cmass
            cmass = dum1(i)
            dum2(i) = residue(j)%mass%rootfiberz(i)
         end do
         call mixproc(tillmix, nlay, dum2(1), cmass, mass)
         do i=1,nlay
            residue(j)%mass%rootfiberz(i) = dum2(i)
         end do

      end do
!********************DECOMPOSITION VARIABLES********************
!
! 
!************************ASD MASS FRACTIONS********************	
      do 250 j=1,msieve
         cmass = 0.0
         do 200 i=1,nlay
            dum1(i) = density(i)*laythk(i)*massf(j,i)+cmass
            cmass = dum1(i)
            dum2(i)=massf(j,i)
200      continue
         call mixproc(tillmix, nlay, dum2(1), cmass, mass)
         do 503 i=1,nlay
            massf(j,i)=dum2(i)
503      continue
250   continue
!************************ASD MASS FRACTIONS********************	
!
!
!****************************BULK DENSITY**********************
!     a weight based adjustment of bulk density results in a
!     change in layer thickness. THIS MUST BE DONE LAST!!!
      cmass = 0.0
      do i=1,nlay
         dum1(i) = density(i)*laythk(i)*density(i)+cmass
         cmass = dum1(i)
         dum(i) = density(i)
      end do
      call mixproc(tillmix, nlay, dum, cmass, mass)
      do i=1,nlay
          laythk(i) = laythk(i)*(density(i)/dum(i))
          density(i) = dum(i)
      end do

      end
