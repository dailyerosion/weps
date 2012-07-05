!$Author$
!$Date$
!$Revision$
!$HeadURL$
      subroutine cropupdate(                                            &
     &      bcmstandstem, bcmstandleaf, bcmstandstore,                  &
     &      bcmflatstem, bcmflatleaf, bcmflatstore,                     &
     &      bcmshoot, bcmbgstemz,                                       &
     &      bcmrootstorez, bcmrootfiberz,                               &
     &      bczht, bcdstm, bczrtd,                                      &
     &      bcthucum, bczgrowpt, bcmbgstem,                             &
     &      bcmrootstore, bcmrootfiber, bcxstmrep,                      &
     &      bcm, bcmst, bcmf, bcmrt, bcmrtz,                            &
     &      bcrcd, bszrgh, bsxrgs, bsargo,                              &
     &      bcrsai, bcrlai, bcrsaz, bcrlaz,                             &
     &      bcffcv, bcfscv, bcftcv, bcfcancov,                          &
     &      bc0rg, bcxrow,                                              &
     &      bnslay, bc0ssa, bc0ssb, bc0sla,                             &
     &      bcovfact, bc0ck, bcxstm, bcdpop,                            &
     &      bhztranspdepth, bhzfurcut,                                  &
     &      bhztransprtmin, bhztransprtmax )

!     INCLUDE
      include 'p1const.inc'
      include 'p1werm.inc'

!     + + + FUNCTION DECLARATIONS + + +

      real biodrag
      real transpdepth

!     + + + VARIABLE DECLARATIONS + + +

      ! state variables
      real bcmstandstem, bcmstandleaf, bcmstandstore
      real bcmflatstem, bcmflatleaf, bcmflatstore
      real bcmshoot, bcmbgstemz(mnsz)
      real bcmrootstorez(mnsz), bcmrootfiberz(mnsz)
      real bczht, bcdstm, bczrtd
      real bcthucum, bczgrowpt
      real bszrgh, bsxrgs, bsargo
      integer bc0rg
      real bcxrow

      ! derived variables
      real bcmbgstem, bcmrootstore, bcmrootfiber, bcxstmrep
      real bcm, bcmst, bcmf, bcmrt, bcmrtz(mnsz)
      real bcrcd
      real bcrsai, bcrlai, bcrsaz(mncz), bcrlaz(mncz)
      real bcffcv, bcfscv, bcftcv, bcfcancov
      real bhztranspdepth, bhzfurcut
      real bhztransprtmin, bhztransprtmax

      ! database variables
      integer bnslay
      real bc0ssa, bc0ssb, bc0sla
      real bcovfact, bc0ck, bcxstm, bcdpop

!     + + + PURPOSE + + +
      ! calculates values of derived variables based on the present values
      ! or the state variables. The derived variables are commonly used
      ! where residue totals are required.

!     + + + VARIABLE DEFINITIONS + + +

!     bcmstandstem - crop standing stem mass (kg/m^2)
!     bcmstandleaf - crop standing leaf mass (kg/m^2)
!     bcmstandstore - crop standing storage mass (kg/m^2)
!                    (head with seed, or vegetative head (cabbage, pineapple))

!     bcmflatstem  - crop flat stem mass (kg/m^2)
!     bcmflatleaf  - crop flat leaf mass (kg/m^2)
!     bcmflatstore - crop flat storage mass (kg/m^2)

!     bcmshoot - crop shoot mass grown from root storage (kg/m^2)
!                this is a "breakout" mass and does not represent a unique pool
!                since this mass is destributed into below ground stem and
!                standing stem as each increment of the shoot is added
!     bcmbgstemz - crop stem mass below soil surface by layer (kg/m^2)

!     bcmrootstorez - crop root storage mass by soil layer (kg/m^2)
!                   (tubers (potatoes, carrots), extended leaf (onion), seeds (peanuts))
!     bcmrootfiberz - crop root fibrous mass by soil layer (kg/m^2)

!     bczht  - Crop height (m)
!     bcdstm - Number of stems per unit area (#/m^2)(used in shoot calculations)
!     bczrtd  - Crop root depth (m)

!     bcthucum - crop accumulated heat units
!     bczgrowpt - depth in the soil of the gowing point (m)

!     bcmbgstem = crop stem mass below the soil surface (kg/m^2)
!     bcmrootstore - crop root storage mass (kg/m^2)
!                   (tubers (potatoes, carrots), extended leaf (onion), seeds (peanuts))
!     bcmrootfiber - crop root fibrous mass (kg/m^2)
!     bcxstmrep - a representative diameter so that acdstm*acxstmrep*aczht=acrsai

!     bcm - Total crop mass (stand + flat+ root) (kg/m^2)
!     bcmst - Standing crop mass (standstem + standleaf + standstore) (kg/m^2)
!     bcmf - Flat crop mass (flatstem + flatleaf + flatstore) (kg/m^2)
!              flag to crop distributes stem, leaf and storeabove
!              elements between standing and flat
!     bcmrt - total crop root mass (rootfiber + rootstore) (kg/m^2)
!     bcmrtz - Crop root mass by soil layer (kg/m^2)

!     bcrcd  - effective Biomass silhouette area (SAI+LAI) (m^2/m^2)
!              (combination of leaf area and stem area indices)
!     bszrgh - ridge height
!     bsxrgs - ridge spacing
!     bsargo - ridge direction
!     bc0rg - crop seeding location flag (0 - in furrow, 1 - on ridge)
!     bcxrow - crop row spacing

!     bcrsai - Crop stem area index (m^2/m^2)
!     bcrlai - Crop leaf area index (m^2/m^2)
!     bcrsaz - Crop stem area index by height (1/m)
!     bcrlaz - Crop leaf area index by height (1/m)

!     bcffcv - Crop biomass cover - flat  (m^2/m^2)
!     bcfscv - Crop biomass cover - standing  (m^2/m^2)
!     bcftcv - Crop biomass cover - total  (m^2/m^2)
!              (sum of bcffcv and bcfscv)
!     bcfcancov - fraction of soil surface covered by crop canopy (m^2/m^2)
!     bhztranspdepth - depth in soil from which transpiration is extracted (m)
!                     when crop is furrow planted, this is deeper than root depth
!                     and is used in place of it when calling transp subroutine
!     bhzfurcut - estimated furrow bottom depth below flat soil surface (mm)
!     bhztransprtmin - root depth where transpiration depth reduction begins (m)
!     bhztransprtmax - root depth where transpiration depth equals root depth (m)

!     bnslay - number of soil layers
!     bmncz - number of standing crop height divisions
!     bc0ssa - stem area to mass ratio coefficient a
!     bc0ssb - stem area to mass ratio coefficient b
!     bc0sla - specific leaf area (m^2/kg)
!     bcovfact - cover to mass ratio for crop residue (m^2/kg)
!     bc0ck  - light extinction coeffficient (fraction)
!     bcxstm - stem diameter for crop residue (m)
!     bcdpop - Number of plants (with single or multple stems) per unit area (#/m^2)

!     LOCAL VARIABLES
      integer index
      real temp

      temp = 0.0

      ! accumulate layer values into root mass totals
      bcmbgstem = 0.0
      bcmrootstore = 0.0
      bcmrootfiber = 0.0
      do index = 1, bnslay
          bcmbgstem = bcmbgstem + bcmbgstemz(index)
          bcmrootstore = bcmrootstore + bcmrootstorez(index)
          bcmrootfiber = bcmrootfiber + bcmrootfiberz(index)
      end do

      bcmst = bcmstandstem + bcmstandleaf + bcmstandstore
      bcmf = bcmflatstem + bcmflatleaf + bcmflatstore
      bcmrt = bcmbgstem + bcmrootstore + bcmrootfiber
      bcm = bcmst + bcmf + bcmrt

      do index = 1, bnslay
          bcmrtz(index) = bcmbgstemz(index) + bcmrootstorez(index)      &
     &                  + bcmrootfiberz(index)
      end do

      ! calculate new stem area index and representative stem diameter
      call ht_dia_sai( bcdpop, bcmstandstem, bc0ssa, bc0ssb,            &
     &                 bcdstm, bcxstm, 2.0, bczht, temp,                &
     &                 bcxstmrep, bcrsai )

      ! leaf area index for standing material
      ! m^2 leaf/kg * kg/m^2 ground = m^2 leaf/m^2 ground
      bcrlai = bc0sla * bcmstandleaf

      ! set stem and leaf area by plant height increments
      ! these are divided equally for a first approximation
      do index = 1, mncz
          bcrsaz(index) = bcrsai / mncz
          bcrlaz(index) = bcrlai / mncz
      end do

      ! effective silhouette
      bcrcd = biodrag(0.0, 0.0, bcrlai, bcrsai,                         &
     &                bc0rg, bcxrow, bczht, bszrgh)

      bcfcancov = 1.0 - exp( - bc0ck * bcrlai)  !crop leaf interception area (canopy cover)
      bcffcv = 1.0 - exp( -bcovfact * bcmf )
      bcfscv = bcdstm * pi * (bcxstm/2.0)*(bcxstm/2.0)
      if (bcfscv > 1.0) bcfscv = 1.0
      bcftcv = bcfscv + bcffcv !no overlap
      if (bcftcv > 1.0) bcftcv = 1.0

      ! transpiration depth as a function of furrow cut depth and root depth
      bhztranspdepth = transpdepth(bczrtd, bhzfurcut,                   &
     &                             bhztransprtmin, bhztransprtmax)

      return
      end