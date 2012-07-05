!$Author$
!$Date$
!$Revision$
!$HeadURL$
!     ------------------------------------------------------------------
!     file 'crop/gcrop.inc'
!
!     This common block consists of variables that contain the crop
!     growth values that are "eventually" going to be moved into the
!     "decomp" pools.  Thus, these are used for temporary storage
!     before that transfer occurs, ie. like a "temporary" pool.
!     
!     This is used in crop to handle residue created when a plant that
!     regrows from the crown is cut or frozen, and any residual above
!     ground crop crop material needs to be moved to residue. 
!     
!     The "transfer" is then done when crop is exited that day.
!
!     Parameter include files required:  p1werm.inc
!
!     + + + VARIABLE DECLARATIONS + + +
      real             agmstandstem(mnsub) !added state
      real             agmstandleaf(mnsub) !added state
      real             agmstandstore(mnsub) !added state

      real             agmflatstem(mnsub) !added state
      real             agmflatleaf(mnsub) !added state
      real             agmflatstore(mnsub) !added state

      real             agmflatrootstore(mnsub) !added state
      real             agmflatrootfiber(mnsub) !added state

      real             agmbgstemz(mnsz,mnsub) !added state
      real             agmbgleafz(mnsz,mnsub) !added state
      real             agmbgstorez(mnsz,mnsub) !added state

      real             agmbgrootstorez(mnsz,mnsub) !added state
      real             agmbgrootfiberz(mnsz,mnsub) !added state

      real             agzht(mnsub)  !changed from tczht state
      real             agdstm(mnsub) !changed from tcdstm state
      real             agxstmrep(mnsub) !changed from tcxstmrep state
      real             agzrtd(mnsub) !changed from tczrtd state
      real             aggrainf(mnsub) !added state

      common / gcrop /                                                  &
     &           agmstandstem, agmstandleaf, agmstandstore,             &
     &           agmflatstem, agmflatleaf, agmflatstore,                &
     &           agmflatrootstore, agmflatrootfiber,                    &
     &           agmbgstemz, agmbgleafz, agmbgstorez,                   &
     &           agmbgrootstorez, agmbgrootfiberz,                      &
     &           agzht, agdstm, agxstmrep, agzrtd, aggrainf

      save :: /gcrop/

!     + + + VARIABLE DEFINITIONS + + +

!     agmstandstem - crop standing stem mass (kg/m^2)
!     agmstandleaf - crop standing leaf mass (kg/m^2)
!     agmstandstore - crop standing storage mass (kg/m^2)
!                    (head with seed, or vegetative head (cabbage, pineapple))

!     agmflatstem  - crop flat stem mass (kg/m^2)
!     agmflatleaf  - crop flat leaf mass (kg/m^2)
!     agmflatstore - crop flat storage mass (kg/m^2)

!     agmflatrootstore - crop flat root storage mass (kg/m^2)
!                   (tubers (potatoes, carrots), extended leaf (onion), seeds (peanuts))
!     agmflatrootfiber - crop flat root fibrous mass (kg/m^2)

!     agmbgstemz  - crop buried stem mass by layer (kg/m^2)
!     agmbgleafz  - crop buried leaf mass by layer (kg/m^2)
!     agmbgstorez - crop buried storage mass by layer (kg/m^2)

!     agmbgrootstorez - crop root storage mass by layer (kg/m^2)
!                   (tubers (potatoes, carrots), extended leaf (onion), seeds (peanuts))
!     agmbgrootfiberz - crop root fibrous mass by layer (kg/m^2)

!     agzht  - Crop height (m)
!     agdstm - Number of crop stems per unit area (#/m^2)
!            - It is computed by taking the tillering factor
!              times the plant population density.
!     agxstmrep - a representative diameter so that acdstm*acxstmrep*aczht=acrsai
!     agzrtd  - Crop root depth (m)
!     aggrainf - internally computed grain fraction of reproductive mass
