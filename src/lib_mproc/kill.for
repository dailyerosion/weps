!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine kill_crop( am0cgf, nlay,                               &
     &           bcmstandstem, bcmstandleaf, bcmstandstore,             &
     &           bcmflatstem, bcmflatleaf, bcmflatstore,                &
     &           bcmrootstorez, bcmrootfiberz,                          &
     &           bcmbgstemz,                                            &
     &           bczht, bcdstm, bcxstmrep, bczrtd,                      &
     &           bcgrainf,                                              &
     &           btmstandstem, btmstandleaf, btmstandstore,             &
     &           btmflatstem, btmflatleaf, btmflatstore,                &
     &           btmbgrootstorez, btmbgrootfiberz,                      &
     &           btmbgstemz,                                            &
     &           btzht, btdstm, btxstmrep, btzrtd,                      &
     &           btgrainf )

!     + + + PURPOSE + + +
!
!     This subroutine performs the kill crop process and transferring of
!     biomass from crop to temporary pool.  Transfer of biomass is performed
!     on above ground biomass and the root biomass.  The transfer is
!     from the crop pool to the "temporary" crop pool.
!
!     + + + KEYWORDS + + +
!     kill, transfer, biomass manipulation

      include 'p1werm.inc'
!
!     + + + ARGUMENT DECLARATIONS + + +
!
      logical am0cgf
      integer nlay

      real    bcmstandstem
      real    bcmstandleaf
      real    bcmstandstore

      real    bcmflatstem
      real    bcmflatleaf
      real    bcmflatstore

      real    bcmrootstorez(mnsz)
      real    bcmrootfiberz(mnsz)

      real    bcmbgstemz(mnsz)

      real    bczht
      real    bcdstm
      real    bcxstmrep
      real    bczrtd

      real    bcgrainf

      real    btmstandstem
      real    btmstandleaf
      real    btmstandstore

      real    btmbgstemz(mnsz)

      real    btmflatstem
      real    btmflatleaf
      real    btmflatstore

      real    btmbgrootstorez(mnsz)
      real    btmbgrootfiberz(mnsz)

      real    btzht
      real    btdstm
      real    btxstmrep
      real    btzrtd

      real    btgrainf

!     + + + ARGUMENT DEFINITIONS + + +

!     am0cgf      - flag to start and stop crop growth submodel
!     nlay        - number of soil layers

!     bcmstandstem - crop standing stem mass (kg/m^2)
!     bcmstandleaf - crop standing leaf mass (kg/m^2)
!     bcmstandstore - crop standing storage mass (kg/m^2)
!                    (head with seed, or vegetative head (cabbage, pineapple))

!     bcmflatstem  - crop flat stem mass (kg/m^2)
!     bcmflatleaf  - crop flat leaf mass (kg/m^2)
!     bcmflatstore - crop flat storage mass (kg/m^2)

!     bcmrootstorez - crop root storage mass by soil layer (kg/m^2)
!                   (tubers (potatoes, carrots), extended leaf (onion), seeds (peanuts))
!     bcmrootfiberz - crop root fibrous mass by soil layer (kg/m^2)

!     bcmbgstemz  - crop buried stem mass by layer (kg/m^2)

!     bczht  - Crop height (m)
!     bcdstm - Number of crop stems per unit area (#/m^2)
!            - It is computed by taking the tillering factor
!              times the plant population density.
!     bcxstmrep - a representative diameter so that acdstm*acxstmrep*aczht=acrsai
!     bczrtd  - Crop root depth (m)

!     bcgrainf - internally computed grain fraction of reproductive mass


!     btmstandstem - crop standing stem mass (kg/m^2)
!     btmstandleaf - crop standing leaf mass (kg/m^2)
!     btmstandstore - crop standing storage mass (kg/m^2)
!                    (head with seed, or vegetative head (cabbage, pineapple))

!     btmflatstem  - crop flat stem mass (kg/m^2)
!     btmflatleaf  - crop flat leaf mass (kg/m^2)
!     btmflatstore - crop flat storage mass (kg/m^2)

!     btmbgrootstorez - crop root storage mass by layer (kg/m^2)
!                   (tubers (potatoes, carrots), extended leaf (onion), seeds (peanuts))
!     btmbgrootfiberz - crop root fibrous mass by layer (kg/m^2)

!     btmbgstemz  - crop buried stem mass by layer (kg/m^2)

!     btzht  - Crop height (m)
!     btdstm - Number of crop stems per unit area (#/m^2)
!            - It is computed by taking the tillering factor
!              times the plant population density.
!     btxstmrep - a representative diameter so that acdstm*acxstmrep*aczht=acrsai
!     btzrtd  - Crop root depth (m)

!     btgrainf - internally computed grain fraction of reproductive mass

!     + + + ACCESSED COMMON BLOCK VARIABLE DEFINITIONS + + +

!     mnsz          - max number of soil layers

!     + + + PARAMETERS + + +

!     + + + LOCAL VARIABLES + + +

      integer lay

!     + + + LOCAL VARIABLE DEFINITIONS + + +

!     lay        - soil layer index

!     + + + END SPECIFICATIONS + + +

!     need to stop the crop growth (ie. stop calling crop submodel)
      am0cgf = .false.

      ! once we move crop biomass into the "temporary" crop pool
      ! it is assumed to be dead biomass.

      btmstandstem = btmstandstem + bcmstandstem
      bcmstandstem = 0.0
      btmstandleaf = btmstandleaf + bcmstandleaf
      bcmstandleaf = 0.0
      btmstandstore = btmstandstore + bcmstandstore
      bcmstandstore = 0.0

      btmflatstem = btmflatstem + bcmflatstem
      bcmflatstem = 0.0
      btmflatleaf = btmflatleaf + bcmflatleaf
      bcmflatleaf = 0.0
      btmflatstore = btmflatstore + bcmflatstore
      bcmflatstore = 0.0

      do lay = 1,nlay
         btmbgrootstorez(lay)= btmbgrootstorez(lay) + bcmrootstorez(lay)
         bcmrootstorez(lay) = 0.0
         btmbgrootfiberz(lay)= btmbgrootfiberz(lay) + bcmrootfiberz(lay)
         bcmrootfiberz(lay) = 0.0
         btmbgstemz(lay) = btmbgstemz(lay) + bcmbgstemz(lay)
         bcmbgstemz(lay) = 0.0
      end do

      btzht = max( btzht, bczht )
      bczht = 0.0
      btdstm = btdstm + bcdstm
      bcdstm = 0.0
      btxstmrep = bcxstmrep
      ! do not zero out this value.
      ! it is derived anyway and is displayed after the harvest
      ! bcxstmrep = 0.0
      btzrtd = max( btzrtd, bczrtd )
      bczrtd = 0.0

      btgrainf = bcgrainf
      ! do not zero out this value.
      ! it is used until the temporary pool is transferred
      ! bcgrainf = 0.0
      
      end
