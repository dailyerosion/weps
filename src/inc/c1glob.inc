!$Author$
!$Date$
!$Revision$
!$HeadURL$

!     This common block contains variables representing the daily
!     updated crop properties for each subregion.  These variables
!     may be used to estimate other crop geometric and mass properties.

!     The crop submodel will maintain all of these variables while
!     a crop is growing.  Any other processes that modify these
!     variables is responsible for making sure they are consistent,
!     ie., the total root mass equals the sum of the root mass in
!     each of the soil layers (acmrt = sum [acmrts(1-mnsz)].
!     The goal is to assume that these variables are always up-to-date
!     and that any routine can access them via read-only and not
!     need to initialize them.

!     Parameter include files required:  p1werm.inc

!     + + + VARIABLE DECLARATIONS + + +
      real             acmstandstem(mnsub) !added state
      real             acmstandleaf(mnsub) !added state
      real             acmstandstore(mnsub) !added state

      real             acmflatstem(mnsub) !added state
      real             acmflatleaf(mnsub) !added state
      real             acmflatstore(mnsub) !added state

      real             acmshoot(mnsub) !added state
      real             acmtotshoot(mnsub) !added state
      real             acmbgstemz(mnsz,mnsub) !added state

      real             acmrootstorez(mnsz,mnsub) !added state
      real             acmrootfiberz(mnsz,mnsub) !added state

      real             aczht(mnsub) !state
      real             aczshoot(mnsub) !added state
      real             acdstm(mnsub) !state
      real             aczrtd(mnsub) !state

      real             acthucum(mnsub) !added state
      real             actrthucum(mnsub) !added state
      real             acgrainf(mnsub) !added state
      real             aczgrowpt(mnsub) !added state

      real             acfliveleaf(mnsub) !added state
      real             acleafareatrend(mnsub) !added state
      real             acstemmasstrend(mnsub) !added state

      integer          actwarmdays(mnsub) !added state
      real             actchillucum(mnsub) !added state
      real             acthardnx(mnsub) !added state

      real             acthu_shoot_beg(mnsub) !added state
      real             acthu_shoot_end(mnsub) !added state

      integer          acdayap(mnsub) !added
      integer          acdayam(mnsub) !added
      real             acmbgstem(mnsub) !added
      real             acmrootstore(mnsub) !added
      real             acmrootfiber(mnsub) !added
      real             acxstmrep(mnsub)

      real             acm(mnsub)
      real             acmst(mnsub)
      real             acmf(mnsub)
      real             acmrt(mnsub)
      real             acmrtz(mnsz,mnsub)

      real             acrsai(mnsub)
      real             acrlai(mnsub)
      real             acrcd(mnsub)
      real             acrsaz(mncz,mnsub)
      real             acrlaz(mncz,mnsub)

      real             acffcv(mnsub)
      real             acfscv(mnsub)
      real             acftcv(mnsub)
      real             acfcancov(mnsub)

      integer          acdayspring(mnsub)

      common / c1glob /                                                 &
     &           acmstandstem, acmstandleaf, acmstandstore,             &
     &           acmflatstem, acmflatleaf, acmflatstore,                &
     &           acmshoot, acmtotshoot, acmbgstemz,                     &
     &           acmrootstorez, acmrootfiberz,                          &
     &           aczht, aczshoot, acdstm, aczrtd,                       &
     &           acthucum, actrthucum, acgrainf,                        &
     &           aczgrowpt, acfliveleaf, acleafareatrend,               &
     &           acstemmasstrend, actwarmdays, actchillucum, acthardnx, &
     &           acthu_shoot_beg, acthu_shoot_end, acdayap, acdayam,    &
     &           acmbgstem, acmrootstore, acmrootfiber, acxstmrep,      &
     &           acm, acmst, acmf, acmrt, acmrtz,                       &
     &           acrsai, acrlai, acrcd, acrsaz, acrlaz,                 &
     &           acffcv, acfscv, acftcv, acfcancov,                     &
     &           acdayspring

      save :: /c1glob/

!     + + + VARIABLE DEFINITIONS + + +

!     acmstandstem - crop standing stem mass (kg/m^2)
!     acmstandleaf - crop standing leaf mass (kg/m^2)
!     acmstandstore - crop standing storage mass (kg/m^2)
!                    (head with seed, or vegetative head (cabbage, pineapple))

!     acmflatstem  - crop flat stem mass (kg/m^2)
!     acmflatleaf  - crop flat leaf mass (kg/m^2)
!     acmflatstore - crop flat storage mass (kg/m^2)

!     acmshoot - mass of shoot growing from root storage biomass (kg/m^2)
!                this is a "breakout" mass and does not represent a unique pool
!                since this mass is distributed into below ground stem and
!                standing stem as each increment of the shoot is added
!     acmtotshoot - total mass of shoot growing from root storage biomass (kg/m^2)
!                   in the period from beginning to completion of emergence heat units
!     acmbgstemz - crop stem mass below soil surface by layer (kg/m^2)
!                 indicates a below ground growing point for shoots
!                 from seeds or roots

!     acmrootstorez - crop root storage mass by soil layer (kg/m^2)
!                   (tubers (potatoes, carrots), extended leaf (onion), seeds (peanuts))
!     acmrootfiberz - crop root fibrous mass by soil layer (kg/m^2)

!     aczht  - Crop height (m)
!     aczshoot - length of actively growing shoot from root biomass (m)
!     acdstm - Number of crop stems per unit area (#/m^2)
!     aczrtd  - Crop root depth (m)

!     acthucum - crop accumulated heat units
!     actrthucum - accumulated root growth heat units (degree-days)
!     acgrainf - internally computed grain fraction of reproductive mass

!     aczgrowpt - depth in the soil of the growing point (m)
!     acfliveleaf - fraction of standing plant leaf which is living (transpiring)
!     acleafareatrend - direction in which leaf area is trending.
!                        Saves trend even if leaf area is static for long periods.
!     acstemmasstrend - direction in which stem mass is trending.
!                        Saves trend even if stem mass is static for long periods.

!     actwarmdays - number of consecutive days that the temperature has been above the minimum growth temperature
!     actchillucum - accumulated chilling units (days)
!     acthardnx - hardening index for winter annuals (range from 0 t0 2)

!     acthu_shoot_beg - heat unit total for beginning of shoot grow from root storage period
!     acthu_shoot_end - heat unit total for end of shoot grow from root storage period

!     acdayap - number of days of growth completed since crop planted
!     acdayam - number of days since crop matured
!     acmbgstem - crop stem mass below soil surface (kg/m^2)
!     acmrootstore - crop root storage mass (kg/m^2)
!                   (tubers (potatoes, carrots), extended leaf (onion), seeds (peanuts))
!     acmrootfiber - crop root fibrous mass (kg/m^2)
!     acxstmrep - a representative diameter so that acdstm*acxstmrep*aczht=acrsai

!     acm - Total crop mass (stand + flat+ root) (kg/m^2)
!     acmst - Standing crop mass (standstem + standleaf + standstore) (kg/m^2)
!     acmf - Flat crop mass (flatstem + flatleaf + flatstore) (kg/m^2)
!              flag to crop distributes stem, leaf and storeabove
!              elements between standing and flat
!     acmrt - total crop root mass (rootfiber + rootstore) (kg/m^2)
!     acmrtz - Crop root mass by soil layer (kg/m^2)

!     acrsai - Crop stem area index (m^2/m^2)
!     acrlai - Crop leaf area index (m^2/m^2) 
!     acrcd  - effective Biomass silhouette area (SAI+LAI) (m^2/m^2)
!              (combination of leaf area and stem area indices)
!     acrsaz - Crop stem area index by height (1/m)
!     acrlaz - Crop leaf area index by height (1/m)

!     acffcv - Crop biomass cover - flat  (m^2/m^2)
!     acfscv - Crop biomass cover - standing  (m^2/m^2)
!     acftcv - Crop biomass cover - total  (m^2/m^2)
!              (sum of acffcv and acfscv)
!     acfcancov - fraction of soil surface covered by crop canopy (m^2/m^2)

!     acdayspring - day of year in which a winter annual released stored growth
