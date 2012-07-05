!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!
!
!
      subroutine invert                                                 &
     &              (nlay,density,laythk,                               &
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
     &               root,blwgnd,massf)


!     + + + PURPOSE + + +
!     
!     This subroutine reads in the array(s) containing the components 
!     that need to be inverted.  It then calls the subroutine invproc 
!     and the actual inversion process is performed.
!
!
!
!     + + + KEYWORDS + + +
!     inversion, tillage 

      include 'p1werm.inc'
      include 'manage/asd.inc'

!
!     + + + ARGUMENT DECLARATIONS + + +
      integer nlay
      real density(mnsz),laythk(mnsz)
      real sand(mnsz),silt(mnsz),clay(mnsz), rock_vol(mnsz)
      real c_sand(mnsz), m_sand(mnsz), f_sand(mnsz), vf_sand(mnsz)
      real w_bd(mnsz)
      real organic(mnsz), ph(mnsz), calcarb(mnsz), cation(mnsz)
      real lin_ext(mnsz)
      real aggden(mnsz), drystab(mnsz)
      real soilwatr(mnsz)
      real satwatr(mnsz), thrdbar(mnsz), ftnbar(mnsz)
      real avawatr(mnsz)
      real soilcb(mnsz), soilair(mnsz), satcond(mnsz)
      real root(mnsz,mnbpls),blwgnd(mnsz,mnbpls)
      real massf(msieve+1,mnsz)
!
!
!     + + + ARGUMENT DEFINITIONS + + +
!
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

!     root        - root mass by layers
!     blwgnd      - below ground biomass
!     massf       - mass fractions for sieve cuts

!     nlay        - number of soil layers used
!
!     + + + ACCESSED COMMON BLOCK VARIABLE DEFINITIONS + + +
!     
!     mnsz	- max number of soil layers
!
!     + + + PARAMETERS + + +
!
!     + + + LOCAL VARIABLES + + +
!
      integer i,j,k
      real dum2(mnsz)
!
!     + + + LOCAL VARIABLE DEFINITIONS + + +
!
!     dum2    - dummy variable containing a variable array to
!               be passed to the inversion process routine
!     i       - loop variable on decomposition pools
!     j       - loop variable on asd sieves 
!     k       - loop variable on the number of layers 
!
!     + + + END SPECIFICATIONS + + + 

!  Make calls to the inversion process for all variables that need 
!  to be inverted. 
!
!************************SOIL VARIABLES********************	
      call invproc(nlay,laythk,sand)
      call invproc(nlay,laythk,silt)
      call invproc(nlay,laythk,clay)
      call invproc(nlay,laythk,rock_vol)

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
!************************SOIL VARIABLES********************	
!
!************************HYDROLOGY VARIABLES********************	
      call invproc(nlay,laythk,soilwatr)
      call invproc(nlay,laythk,satwatr)
      call invproc(nlay,laythk,thrdbar)
      call invproc(nlay,laythk,ftnbar)
      call invproc(nlay,laythk,avawatr)

      call invproc(nlay,laythk,soilcb)
      call invproc(nlay,laythk,soilair)
      call invproc(nlay,laythk,satcond)
!************************HYDROLOGY VARIABLES********************	
! 
!************************ASD MASS FRACTIONS********************	
!   need to invert mass fractions for all sieve cuts and layers 
!  
      do 170 j=1,msieve
         do 200 k=1,nlay
            dum2(k)=massf(j,k)
200      continue
         call invproc(nlay,laythk,dum2(1))
         do 201 k=1,nlay
            massf(j,k)=dum2(k)
201      continue
170   continue
!************************ASD MASS FRACTIONS********************	
! 
!************************DECOMPOSITION VARIABLES********************	
!   need to invert both pools and layers for these next two variables 

      do 175 i=1,mnbpls
         do 202 k=1,nlay
            dum2(k)=root(k,i)
202      continue
         call invproc(nlay,laythk,dum2(1))
         do 203 k=1,nlay
           root(k,i)=dum2(k)
203      continue
175   continue

      do 180 i=1,mnbpls
         do 204 k=1,nlay
         dum2(k)=blwgnd(k,i)
204   continue
      call invproc(nlay,laythk,dum2(1))
      do 205 k=1,nlay
         blwgnd(k,i)=dum2(k)
205     continue
180   continue 
!************************DECOMPOSITION VARIABLES********************
!
		  
      end
