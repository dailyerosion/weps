!$Author$
!$Date$
!$Revision$
!$HeadURL$

!     This common block contains variables representing the crop
!     database properties for each subregion.

!     Parameter include files:  p1werm.inc

!     + + + VARIABLE DECLARATIONS + + +

      real             ac0bn1(mnsub)
      real             ac0bn2(mnsub)
      real             ac0bn3(mnsub)
      real             ac0bp1(mnsub)
      real             ac0bp2(mnsub)
      real             ac0bp3(mnsub)
      real             acfny(mnsub)
      real             acfpy(mnsub)
      real             acfwy(mnsub)

      common / c1db3 /                                                  &
     &                 ac0bn1, ac0bn2, ac0bn3, ac0bp1, ac0bp2, ac0bp3,  &
     &                 acfny, acfpy, acfwy

      save :: /c1db3/

!     + + + VARIABLE DEFINITIONS + + +

!     ac0bn1 - Crop nitrogen uptake parm (N fraction at emergence)
!     ac0bn2 - Crop nitrogen uptake parm (N fraction @ 1/2 maturity)
!     ac0bn3 - Crop nitrogen uptake parm (N fraction at maturity)
!     ac0bp1 - Crop phosphorous uptake parm (N fraction at emergence)
!     ac0bp2 - Crop phosphorous uptake parm (N fraction @ 1/2 maturity)
!     ac0bp3 - Crop phosphorous uptake parm (N fraction at maturity)
!     acfny  - Fraction of nitrogen in yield (?)
!     acfpy  - Fraction of phosphorous in yield (?)
!     acfwy  - Fraction of water in yield (?)
