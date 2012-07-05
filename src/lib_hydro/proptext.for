!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine proptext( nlay, clayf, sandf, organf,                  &
     &                 settled_bulkden, partden )

!     + + + PURPOSE + + +
!     
!     This subroutine updates the properties that depend on soil texture 
!     (texture can change in the model due to mixing and removal by wind)

!     + + + KEYWORDS + + +
!     texture properties 

!     + + + ARGUMENT DECLARATIONS + + +
      integer nlay
      real sandf(*),clayf(*),organf(*)
      real settled_bulkden(*), partden(*)

!     + + + ARGUMENT DEFINITIONS + + +
!     nlay     - number of soil layers to be updated
!     clayf    - fraction of soil mineral portion which is clay
!     sandf    - fraction of soil mineral portion which is sand
!     organf   - fraction of total soil mass which is organic matter
!     settled_bulkden - settled bulk density (Mg/m^3)
!     partden  - particle density (Mg/m^3)

!     + + + LOCAL COMMON BLOCKS + + +
      include 'hydro/partden.inc'

!     + + + FUNCTION DECLARATIONS + + +
      real   setbds

!     + + + LOCAL VARIABLES + + +
      integer lay

!     + + + LOCAL VARIABLE DEFINITIONS + + +

!     + + + END SPECIFICATIONS + + + 

      do lay=1,nlay
!         settled bulk density
          settled_bulkden(lay) = setbds( clayf(lay), sandf(lay),        &
     &                                   organf(lay))

!         calculate an average soil particle density
!         Particle density of organic matter
!         1.37 in Baver, Soil Physics, 1972 p.44 (humus)
!         1.1  in Marshall and Holmes, Soil Physics, 1979, p.277 (soil organic matter)
!         Old incorrect formula
!          partden(lay) = organf(lay)*den_organic
!     &                 + (1.0-organf(lay))*den_quartz
!         New correct formula
          partden(lay) = 1.0 / (organf(lay)/den_organic                 &
     &                 + (1.0-organf(lay))/den_quartz)

!         make sure particle density is significantly greater than settled bulk density
          if( partden(lay).lt.(1.2*settled_bulkden(lay)) ) then
              partden(lay) = 1.2*settled_bulkden(lay)
          endif
      end do

      end
