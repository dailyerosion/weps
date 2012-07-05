!$Author$
!$Date$
!$Revision$
!$HeadURL$
!     ------------------------------------------------------------------
!     file 'h1db1.inc'
!
!     This common block contains the variables representing the soil
!     hydrologic properties in WERM for each soil layer.
!
!     Parameter include files:  p1werm.inc
!
!     + + + VARIABLE DECLARATIONS + + +
!
      real             ahrwcr(mnsz, mnsub)
      real             ahrwcw(mnsz, mnsub)
      real             ahrwcf(mnsz, mnsub)
      real             ahrwcs(mnsz, mnsub)
      real             ahrwca(mnsz, mnsub)
      real             ahrwc0(mnhhrs, mnsub)
      real             ahzsnd(mnsub)
      real             ahrwc1(mnsz,mnsub)
      real             ahfredsat(mnsz, mnsub)
!
      common / h1db1 /                                                  &
     &         ahrwcr, ahrwcw, ahrwcf, ahrwcs, ahrwca, ahrwc0, ahzsnd,  &
     &         ahrwc1, ahfredsat

      save :: /h1db1/
!
!     + + + VARIABLE DEFINITIONS + + +
!
!     ahrwcr - Soil layer residual water content (kg/kg)
!     ahrwcw - Soil layer wilting point water content (kg/kg)
!     ahrwcf - Soil layer field capacity water content (kg/kg)
!     ahrwcs - Soil layer saturated water content (kg/kg)
!     ahrwca - Available soil layer water content (kg/kg)
!     ahrwc0 - surface soil water content (kg/kg)
!     ahzsnd - snow depth (mm)
!     ahrwc1 - Soil layer water content at 0.1 bar (kg/kg)
!     ahfredsat - fraction of soil porosity that will be filled with water
!                 while wetting under normal field conditions due to entrapped air
