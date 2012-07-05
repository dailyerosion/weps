!$Author$
!$Date$
!$Revision$
!$HeadURL$
!     ------------------------------------------------------------------
!     file 'd1glob.inc'

!     This common block contains variables representing the daily
!     updated residue pool properties for each subregion.  These variables
!     may be used to estimate other residue geometric and mass properties.

!     The decomp submodel will maintain all of these variables 
!     on a daily basis.  Any other processes that modify these
!     variables is responsible for making sure they are consistent,
!     ie., the total root mass equals the sum of the root mass in
!     each of the soil layers (admrt = sum [admrts(1-mnsz)] for each
!     residue pool.
!     The goal is to assume that these variables are always up-to-date
!     and that any routine can access them via read-only and not
!     need to initialize them.

!     Parameter include files required:  p1werm.inc

!     + + + VARIABLE DECLARATIONS + + +
      real             admstandstem(mnbpls,mnsub) !added state
      real             admstandleaf(mnbpls,mnsub) !added state
      real             admstandstore(mnbpls,mnsub) !added state

      real             admflatstem(mnbpls,mnsub) !added state
      real             admflatleaf(mnbpls,mnsub) !added state
      real             admflatstore(mnbpls,mnsub) !added state

      real             admflatrootstore(mnbpls,mnsub) !added state
      real             admflatrootfiber(mnbpls,mnsub) !added state

      real             admbgstemz(mnsz,mnbpls,mnsub) !added state
      real             admbgleafz(mnsz,mnbpls,mnsub) !added state
      real             admbgstorez(mnsz,mnbpls,mnsub) !added state

      real             admbgrootstorez(mnsz,mnbpls,mnsub) !added state
      real             admbgrootfiberz(mnsz,mnbpls,mnsub) !added state

      real             adzht(mnbpls,mnsub) !state
      real             addstm(mnbpls,mnsub) !state
      real             adxstmrep(mnbpls,mnsub) !state
      real             adgrainf(mnbpls,mnsub) !added state
      integer          adhyfg(mnbpls,mnsub) ! added state

      real             admbgstem(mnbpls,mnsub) !added
      real             admbgleaf(mnbpls,mnsub) !added
      real             admbgstore(mnbpls,mnsub) !added

      real             admbgrootstore(mnbpls,mnsub) !added
      real             admbgrootfiber(mnbpls,mnsub) !added

      real             adm(mnbpls,mnsub)
      real             admst(mnbpls,mnsub)
      real             admf(mnbpls,mnsub)
      real             admrt(mnbpls,mnsub)
      real             admrtz(mnsz,mnbpls,mnsub)
      real             admbg(mnbpls,mnsub)
      real             admbgz(mnsz,mnbpls,mnsub)

      real             adrsai(mnbpls,mnsub)
      real             adrlai(mnbpls,mnsub)
      real             adrsaz(mncz,mnbpls,mnsub)
      real             adrlaz(mncz,mnbpls,mnsub)

      real             adrcd(mnbpls,mnsub)
      real             adffcv(mnbpls,mnsub)
      real             adfscv(mnbpls,mnsub)
      real             adftcv(mnbpls,mnsub)
      real             adfcancov(mnbpls,mnsub)

      real             addstmtot(mnsub)
      real             adzht_ave(mnsub)
      real             admtot(mnsub)
      real             admtotto4(mnsub)
      real             admsttot(mnsub)
      real             admftot(mnsub)
      real             admbgtot(mnsub)
      real             admbgtotto4(mnsub)
      real             admrttot(mnsub)
      real             admrttotto4(mnsub)

      real             adrcdtot(mnsub)
      real             adrsaitot(mnsub)
      real             adrlaitot(mnsub)
      real             adffcvtot(mnsub)
      real             adfscvtot(mnsub)
      real             adftcvtot(mnsub)
      real             adftcancov(mnsub)

      common / d1glob /                                                 &
     &           admstandstem, admstandleaf, admstandstore,             &
     &           admflatstem, admflatleaf, admflatstore,                &
     &           admflatrootstore, admflatrootfiber,                    &
     &           admbgstemz, admbgleafz, admbgstorez,                   &
     &           admbgrootstorez, admbgrootfiberz,                      &
     &           adzht, addstm, adxstmrep, adgrainf, adhyfg,            &
     &           admbgstem, admbgleaf, admbgstore,                      &
     &           admbgrootstore, admbgrootfiber,                        &
     &           adm, admst, admf, admrt, admrtz, admbg, admbgz,        &
     &           adrsai, adrlai, adrsaz, adrlaz, adrcd,                 &
     &           adffcv, adfscv, adftcv, adfcancov, addstmtot,          &
     &           adzht_ave, admtot, admtotto4, admsttot, admftot,       &
     &           admbgtot, admbgtotto4, admrttot, admrttotto4,          &
     &           adrcdtot, adrsaitot, adrlaitot, adffcvtot, adfscvtot,  &
     &           adftcvtot, adftcancov

      save :: /d1glob/

!   + + + VARIABLE DEFINITIONS + + +

! for each of the residue age pools and each subregion

!     admstandstem  - standing stem mass (kg/m^2)
!     admstandleaf  - standing leaf mass (kg/m^2)
!     admstandstore - standing storage mass (kg/m^2)

!     admflatstem  - flat stem mass (kg/m^2)
!     admflatleaf  - flat leaf mass (kg/m^2)
!     admflatstore - flat storage mass (kg/m^2)

!     admflatrootstore - flat storage root mass (kg/m^2)
!     admflatrootfiber - flat fibrous root mass (kg/m^2)

!     admbgstemz  - buried stem mass by layer (kg/m^2)
!     admbgleafz  - buried leaf mass by layer (kg/m^2)
!     admbgstorez - buried (from above ground) storage mass by layer (kg/m^2)

!     admbgrootstorez - buried storage root mass by layer (kg/m^2)
!     admbgrootfiberz - buried fibrous root mass by layer (kg/m^2)

!     adzht  - Residue height (m)
!     addstm - Number of residue stems per unit area (#/m^2)
!     adxstmrep - a representative diameter so that addstm*adxstmrep*adzht=adrsai
!     adgrainf - internally computed grain fraction of reproductive mass
!     adhyfg - flag indicating the part of plant to apply the "grain fraction",
!              GRF, to when removing that plant part for yield
!         0     GRF applied to above ground storage (seeds, reproductive)
!         1     GRF times growth stage factor (see growth.for) applied to above ground storage (seeds, reproductive)
!         2     GRF applied to all aboveground biomass (forage)
!         3     GRF applied to leaf mass (tobacco)
!         4     GRF applied to stem mass (sugarcane)
!         5     GRF applied to below ground storage mass (potatoes, peanuts)

!     admbgstem - buried residue stem mass (kg/m^2)
!     admbgleaf - buried residue leaf mass (kg/m^2)
!     admbgstore - buried residue storage mass (kg/m^2)

!     admbgrootstore - buried storage root mass (kg/m^2)
!     admbgrootfiber - buried fibrous root mass (kg/m^2)

!     adm   - Total residue mass (standing + flat + roots + buried) (kg/m^2)
!     admst - Standing residue mass (standstem + standleaf + standstore) (kg/m^2)
!     admf  - Flat residue mass (flatstem + flatleaf + flatstore) (kg/m^2)
!     admrt - Buried residue root mass (rootfiber + rootstore)(kg/m^2)
!     admrtz - Buried residue root mass by soil layer (kg/m^2)
!     admbg - Buried residue mass (kg/m^2) Excludes root mass below the surface.
!     admbgz - Buried residue mass by soil layer (kg/m^2)

!     adrsai - Residue stem area index (m^2/m^2)
!     adrlai - Residue leaf area index (m^2/m^2)
!     adrsaz - Residue stem area index by height (1/m)
!     adrlaz - Residue leaf area index by height (1/m)

!     adffcv - Residue biomass cover - flat (m^2/m^2)
!     adfscv - Residue biomass cover - standing (m^2/m^2)
!     adftcv - Residue biomass cover - total (m^2/m^2)
!              (adffcv + adfscv)
!     adfcancov - fraction of soil surface covered by residue canopy (m^2/m^2)

!     adrcd  - effective Biomass silhouette area (SAI+LAI) (m^2/m^2)
!              (combination of leaf area and stem area indices)

!     addstmtot - total number of residue stems  per unit area (#/m^2)
!     adzht_ave - Weighted ave height of residue across all residue pools (m)

!     admtot    - Total residue mass across pools (standing + flat + roots + buried) (kg/m^2)
!     admtotto4 - Total residue mass across pools (standing + flat + roots + buried to a 4 inch depth) (kg/m^2)
!     admsttot  - Standing residue mass across pools (standstem + standleaf + standstore) (kg/m^2)
!     admftot   - Flat residue mass across pools (flatstem + flatleaf + flatstore) (kg/m^2)
!     admbgtot  - Buried residue mass across pools (kg/m^2)
!     admbgtotto4 - Buried (to a 4 inch depth) residue mass across pools (kg/m^2)
!     admrttot  - Buried residue root mass across pools (kg/m^2)
!     admrttotto4 - Buried (to a 4 inch depth) residue root mass across pools (kg/m^2)

!     adrsaitot - total of stem area index across pools (m^2/m^2)
!     adrlaitot - total of leaf area index across pools (m^2/m^2)

!     adrcdtot  - effective Biomass silhouette area across pools (SAI+LAI) (m^2/m^2)
!                 (combination of leaf area and stem area indices)

!     adffcvtot - Residue biomass cover across pools - flat (m^2/m^2)
!     adfscvtot - Residue biomass cover across pools - standing (m^2/m^2)
!     adftcvtot - Residue biomass cover across pools - total (m^2/m^2)
!                 (adffcvtot + adfscvtot)
!     adftcancov - fraction of soil surface covered by residue canopy across pools (m^2/m^2)
