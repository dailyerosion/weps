!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine cropupdate(                                            &
     &      bcmstandstem, bcmstandleaf, bcmstandstore,                  &
     &      bcmflatstem, bcmflatleaf, bcmflatstore,                     &
     &      bcmbgstemz,                                                 &
     &      bcmrootstorez, bcmrootfiberz,                               &
     &      bczht, bcdstm, bczrtd,                                      &
     &      bcmbgstem,                                                  &
     &      bcmrootstore, bcmrootfiber, bcxstmrep,                      &
     &      bcm, bcmst, bcmf, bcmrt, bcmrtz,                            &
     &      bcrcd, bszrgh, bszlyd,                                      &
     &      bcrsai, bcrlai, bcrsaz, bcrlaz,                             &
     &      bcffcv, bcfscv, bcftcv, bcfcancov,                          &
     &      bc0rg, bcxrow,                                              &
     &      bnslay, bc0ssa, bc0ssb, bc0sla,                             &
     &      bcovfact, bc0ck, bcxstm, bcdpop,                            &
     &      bhztranspdepth, bhzfurcut,                                  &
     &      bhztransprtmin, bhztransprtmax, croptot )

      use weps_interface_defs
      use biomaterial, only: biomatter, biototal
      use p1unconv_mod, only: pi

!     INCLUDE
      include 'p1werm.inc'

!     + + + FUNCTION DECLARATIONS + + +

!      real biodrag
!      real transpdepth

!     + + + ARGUMENT DECLARATIONS + + +

      ! state variables
      real bcmstandstem, bcmstandleaf, bcmstandstore
      real bcmflatstem, bcmflatleaf, bcmflatstore
      real bcmbgstemz(*)
      real bcmrootstorez(*), bcmrootfiberz(*)
      real bczht, bcdstm, bczrtd
      real bszrgh, bszlyd(*)
      integer bc0rg
      real bcxrow

      ! derived variables
      real bcmbgstem, bcmrootstore, bcmrootfiber, bcxstmrep
      real bcm, bcmst, bcmf, bcmrt, bcmrtz(*)
      real bcrcd
      real bcrsai, bcrlai, bcrsaz(*), bcrlaz(*)
      real bcffcv, bcfscv, bcftcv, bcfcancov
      real bhztranspdepth, bhzfurcut
      real bhztransprtmin, bhztransprtmax
      type(biototal), intent(inout) :: croptot

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

!     bcmbgstemz - crop stem mass below soil surface by layer (kg/m^2)

!     bcmrootstorez - crop root storage mass by soil layer (kg/m^2)
!                   (tubers (potatoes, carrots), extended leaf (onion), seeds (peanuts))
!     bcmrootfiberz - crop root fibrous mass by soil layer (kg/m^2)

!     bczht  - Crop height (m)
!     bcdstm - Number of stems per unit area (#/m^2)(used in shoot calculations)
!     bczrtd  - Crop root depth (m)

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
!     bszlyd - Depth to bottom of each soil layer(mm)
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
      integer idx
      real temp
      ! parameter to control depth of slice
      real scidepth
      parameter( scidepth = 101.6 ) ! mm 101.6 = 4 inches for SCI
      real weppdepth
      parameter( weppdepth = 150.0 ) ! mm 150.0 = 5.9 inches for WEPP

      temp = 0.0

      ! accumulate layer values into root mass totals
      bcmbgstem = 0.0
      bcmrootstore = 0.0
      bcmrootfiber = 0.0
      do idx = 1, bnslay
          bcmbgstem = bcmbgstem + bcmbgstemz(idx)
          bcmrootstore = bcmrootstore + bcmrootstorez(idx)
          bcmrootfiber = bcmrootfiber + bcmrootfiberz(idx)
      end do

      bcmst = bcmstandstem + bcmstandleaf + bcmstandstore
      bcmf = bcmflatstem + bcmflatleaf + bcmflatstore
      bcmrt = bcmbgstem + bcmrootstore + bcmrootfiber
      bcm = bcmst + bcmf + bcmrt

      do idx = 1, bnslay
          bcmrtz(idx) = bcmbgstemz(idx) + bcmrootstorez(idx)            &
     &                  + bcmrootfiberz(idx)
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
      do idx = 1, mncz
          bcrsaz(idx) = bcrsai / mncz
          bcrlaz(idx) = bcrlai / mncz
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

      ! assign values to croptot variables
      croptot%mrttotto4 =                                               &
     &              valbydepth(bnslay, bszlyd, bcmrtz, 2, 0.0, scidepth)    ! Buried (to a 4 inch depth) root mass across pools (kg/m^2)
      croptot%dstmtot = bcdstm      ! total number of stems  per unit area (#/m^2)
      croptot%zht_ave = bczht      ! Weighted ave height across pools (m)
      croptot%zmht = bczht         ! Tallest biomass height across pools (m)
      croptot%mtot = bcm         ! Total mass across pools (standing + flat + roots + buried) (kg/m^2)
      croptot%mtotto4 = bcmst + bcmf + croptot%mrttotto4      ! Total mass across pools (standing + flat + roots + buried to a 4 inch depth) (kg/m^2)
      croptot%msttot = bcmst       ! Standing mass across pools (standstem + standleaf + standstore) (kg/m^2)
      croptot%mftot = bcmf        ! Flat mass across pools (flatstem + flatleaf + flatstore) (kg/m^2)
      croptot%mbgtot = 0.0       ! Buried mass across pools (kg/m^2)
      croptot%mbgtotto4 = 0.0    ! Buried (to a 4 inch depth) mass across pools (kg/m^2)
      croptot%mbgtotto15 = 0.0   ! Buried (to a 15 cm depth) mass across pools (kg/m^2)
      croptot%mrttot = bcmrt       ! Buried root mass across pools (kg/m^2)
      croptot%mrttotto15 =                                              &
     &             valbydepth(bnslay, bszlyd, bcmrtz, 2, 0.0, weppdepth)   ! Buried (to a 15 cm depth) root mass across pools (kg/m^2)

      do idx = 1, bnslay
          croptot%mrtz(idx) = bcmrtz(idx)           ! Buried root mass by soil layer (kg/m^2)
          croptot%mbgz(idx) = 0.0           ! Buried mass by soil layer (kg/m^2)
      end do

      croptot%rsaitot = bcrsai      ! total of stem area index across pools (m^2/m^2)
      croptot%rlaitot = bcrlai      ! total of leaf area index across pools (m^2/m^2)

      do idx = 1, mncz
          croptot%rsaz(idx) = bcrsai / mncz           ! stem area index by height (1/m)
          croptot%rlaz(idx) = bcrlai / mncz           ! leaf area index by height (1/m)
      end do

      croptot%rcdtot =                                                  &
     &  biodrag(croptot%rlaitot,croptot%rsaitot,0.0,0.0, 0, 0.0,0.0,0.0)       ! effective Biomass silhouette area across pools (SAI+LAI) (m^2/m^2)
                          ! (combination of leaf area and stem area indices)

      croptot%ffcvtot = bcffcv      ! biomass cover across pools - flat (m^2/m^2)
      croptot%fscvtot = bcfscv      ! biomass cover across pools - standing (m^2/m^2)
      croptot%ftcvtot = bcftcv      ! biomass cover across pools - total (m^2/m^2)
                          ! (adffcvtot + adfscvtot)
      croptot%ftcancov = bcfcancov     ! fraction of soil surface covered by canopy across pools (m^2/m^2)
      croptot%evapredu = 0.0     ! composite evaporation reduction from across pools (ea/ep ratio)

      croptot%xrow = bcxrow
      croptot%c0rg = bc0rg

      return
      end
