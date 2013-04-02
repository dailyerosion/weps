!$Author$
!$Date$
!$Revision$
!$HeadURL$
!**********************************************************************
! subroutine sbzdisp
!**********************************************************************
      subroutine sbzdisp (szrgh, bcxrow, bc0rg, wzoflg,                 &
     &                 bdrlai, bdrsai, bbzht, bcrlai, bcrsai, bczht,    &
     &                 awzdisp, wzdisp)

!     +++ PURPOSE +++
!     Calc. zero plane displacement (mm)

!     set field zero plane displacement equal to Anemometer zero plane
!     displacement when anem. at the field site, ie. wzoflg = 1

      ! using equation from: Raupach, M.R. 1994. Simplified Expressions for
      ! Vegetation Roughness Length and Zero-Plane Displacement as functions
      ! of Canopy Height and Area index. Boundary Layer meteorology 71:211-216.

!     +++ ARGUMENT DECLATION +++
      real szrgh, bcxrow
      integer bc0rg, wzoflg
      real bdrlai, bdrsai, bbzht
      real bcrlai, bcrsai, bczht
      real awzdisp, wzdisp

!     +++ ARGUMENT DEFINITIONS +++
!     szrgh  - ridge height (mm)
!     bcxrow - crop row spacing (m)
!     bc0rg  - flag=0 - crop planted in furrow bottom
!              flag=1 - crop planted on ridge top
!     wzoflg - flag=0 - weather measurements are from a distant station
!              flag=1 - weather measurements are in field
!     bdrlai - residue leaf area index (total)(m2/m2)
!     bdrsai - residue stem area index (total)(m2/m2)
!     bbzht  - composite average residue height (m)
!     bcrlai - crop leaf area index (m2/m2)
!     bcrsai - crop stem area index (m2/m2)
!     bczht  - crop height (m)
!     bc0rg  - flag=0 - crop planted in furrow bottom
!              flag=1 - crop planted on ridge top
!     awzdisp - zero plane displacement at weather location (mm)
!     wzdisp - zero plane displacement at location (mm)

!     +++ FUNCTIONS CALLED
      real biodrag

!     +++ LOCAL VARIABLES +++
      real  bht, bsai

!     +++ LOCAL VARIABLE DEFINITIONS +++
!     bht   - biomass height (mm)
!     bsai  - biomass silhouette area index

!     +++ INCLUDE FILES+++
      include 'p1unconv.inc'  ! mtomm

!     +++ PARAMETERS +++

!     +++ END SPECIFICATIONS +++

      ! calculate "effective" biomass silhouette area index
      bsai = biodrag( bdrlai, bdrsai, bcrlai, bcrsai, bc0rg,            &
     &                bcxrow, bczht, szrgh )

      ! find maximum biomass height and convert to mm
      bht = max(bbzht, bczht) * mtomm

      ! use silhouette area index and biomass height to find zero plane displacement
      if( bsai .gt. 1.0e-10 ) then
          wzdisp = bht * (1.0 - (1.0 - exp( -(15.0*bsai)**0.5) )        &
     &           / (15.0*bsai)**0.5)
      else
          wzdisp = 0.0
      end if

      if (wzoflg .eq. 1) then
         ! anemom. in field set weather displacement to field displacement
         awzdisp = wzdisp
      endif

      return
      end

