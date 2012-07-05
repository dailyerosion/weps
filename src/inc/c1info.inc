!$Author$
!$Date$
!$Revision$
!$HeadURL$

!     This common block contains the character variables containing
!     information about the crop

!     Parameter include files:  p1strlen.inc

!     + + + VARIABLE DECLARATIONS + + +

      character*(80)  ac0nam(mnsub)
      integer         ac0idc(mnsub)

      common / c1info /                                                 &
     &                  ac0nam, ac0idc

      save :: /c1info/

!     + + + VARIABLE DEFINITIONS + + +

!     ac0nam - Crop name
!     ac0idc - The crop type number (1 = annual, perennial, . . .)
!              1,4 Summer Annual
!              2,5 Winter Annual
!              3,6 Perennial
!              7   Biannual with tuber dormancy
!              8   Perennial with staged crown bud release

!     ------------------------------------------------------------------
