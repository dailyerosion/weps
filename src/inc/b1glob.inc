!$Author$
!$Date$
!$Revision$
!$HeadURL$
!     ------------------------------------------------------------------
!     file 'b1glob.inc'

!     This common block contains variables representing the daily
!     updated biomass (live and dead) properties for each subregion.
!     These variables may be used to estimate other vegetative
!      geometric and mass properties.
!
!     All of these variables are updated within "main" on a daily
!     basis for erosion and outputs.  Only a "biomass update" process
!     is intended to modify these variables since they are all dependent
!     upon the individual live (crop) and dead (residue) biomass values.

!     Parameter include files:  p1werm.inc

!     + + + VARIABLE DECLARATIONS + + +

      real             abzht(mnsub)
      real             abzmht(mnsub)

      real             abm(mnsub)

      real             abmst(mnsub)
      real             abmf(mnsub)
      real             abmbg(mnsub)
      real             abmrt(mnsub)
      real             abmbgz(mnsz,mnsub)
      real             abmrtz(mnsz,mnsub)

!      real             abmyld(mnsub) (removed)

      real             abdstm(mnsub)

      real             abrsai(mnsub)
      real             abrlai(mnsub)
      real             abrcd(mnsub)
      real             abrsaz(mncz,mnsub)
      real             abrlaz(mncz,mnsub)

      real             abffcv(mnsub)
      real             abfscv(mnsub)
      real             abftcv(mnsub)
      real             abfcancov(mnsub)

      real             abevapredu(mnsub)

      common / b1glob /                                                 &
     &           abzht, abzmht,                                         &
     &           abm, abmst, abmf,                                      &
     &           abmbg, abmrt, abmbgz, abmrtz,                          &
     &           abdstm, abrsai, abrlai, abrcd, abrsaz, abrlaz,         &
     &           abffcv, abfscv, abftcv, abfcancov, abevapredu

      save :: /b1glob/

!     + + + VARIABLE DEFINITIONS + + +

!     abzht  - Composite weighted average biomass height (m)
!     abzmht - Tallest biomass height (m) greatest of daily crop or residue height
!     abm - Total biomass (kg/m^2) standing + roots + flat + buried + yield
!     abmst - Standing biomass - above ground (kg/m^2)
!     abmf    - Flat biomass (kg/m^2)
!     abmbg - Buried biomass (kg/m^2)
!     abmrt - Buried root biomass (kg/m^2)
!     abmbgz - Buried biomass by soil layer (kg/m^2)
!     abmrtz - Buried root biomass by soil layer (kg/m^2)
!     abmyld - Biomass (crop) yield mass (kg/m^2)(reproductive components)
!     abdstm - Total number of stems (#/m^2) (live and dead) May be a weighted summation.
!     abrsai - Biomass stem area index (m^2/m^2)
!     abrlai - Biomass leaf area index (m^2/m^2)
!     abrcd  - effective Biomass silhouette area (SAI+LAI) (m^2/m^2)
!              (combination of leaf area and stem area indices)
!     abrsaz - Biomass stem area index by height (1/m)
!     abrlaz - Biomass leaf area index by height (1/m)
!     abffcv - Biomass cover - flat  (m^2/m^2)
!     abfscv - Biomass cover - standing  (m^2/m^2)
!     abftcv - Biomass cover - total  (m^2/m^2)
!              (sum of abffcv and abfscv)
!     abfcancov - fraction of soil surface covered by all canopy (m^2/m^2)
!     abevapredu - composite evaporation reduction from crop and residue materials (ea/ep ratio)
