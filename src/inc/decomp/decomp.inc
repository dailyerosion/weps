!$Author$
!$Date$
!$Revision$
!$HeadURL$

! NOTE:  all global decomp pools have been modified to have an array size of
!        mnbpls (3) even though some pools are not currently used/defined
!        within the DECOMP model.  This change is being made to make coding
!        easier for all other submodels needing to manipulate the residue pools
!        (ie. will only need one index value for loops on all residue pool classes).
!        Eventually it is envisioned that the DECOMP model will be expanded to
!        truly contain the same number of age groups (3) for all residue pools
!        (standing, flat, buried, and roots). Currently only the flat residue pool
!        contains 3 age groups, the others contain only 2 age groups.
!
!        This include file contains only local variables for the DECOMP routines.
!        Thus, the local variables will not have the array sizes modified at
!        this time.  However, if and when the decomp routines are expanded to
!        work with 3 age groups for all pools, they will need to be modified then.
!
!        All array dimensions are updated by defining them via the parameter
!        mnbpls or a function of that parameter value.
!
!        Sat Jul 29 18:08:25 CDT 1995
!
! LEW
!
!     DECOMP.INC
!
!     These local variables are used by
!     the decomposition submodel DECOMP, DECINI, DECHRV, AND DECOUT
!     subroutines
!     Variables that need to be maintained by subregion are
!     arrayed be mnsub
!
!     requires p1werm.inc to be included prior to inclusion of this file
!
!     + + + VARIABLE DECLARATIONS + + +

      integer dweti(mnsub)
      integer hrvflag
      integer iage
      integer idtype
      integer iht
      integer ipool
      integer ipoolf
      integer isz

!cc
      integer iwets
      integer iwetf
      integer iwetg(mnsz)

!cc

      real aqua
      real covfact(mnbpls,mnsub)
      real cumddf(mnbpls,mnsub)
      real cumddg(mnsz,mnbpls,mnsub)
      real cumdds(mnbpls,mnsub)
      real ddsthrsh(mnbpls,mnsub)
      real diddf
      real diddg(mnsz)
      real didds
      real ditca
      real ditcg(mnsz)
      real diwcf
      real diwcg(mnsz)
      real diwcs
      real diwcsy(mnsub)
      real dkrate(mndk,mnbpls,mnsub)

      real adresevapa(mnbpls,mnsub)
      real adresevapb(mnbpls,mnsub)

! + + + VARIABLE DEFINITIONS + + +

!     dweti   - days since anticedent moisture (4 to 0) index

!     hrvflag - harvest flag used to increment residue pools
!
!         range values                   min                  max
!         ------------                   ------               ------
!         fixed                          0                    number of harvest + 1
!         typical                        0                    ?
!
!         - Needed for determining the number of residue pools for
!           the first three harvests.

!     iage   - residue pool age index
!
!         range values                    min                  max
!         ------------                    ------               ------
!         fixed                           1                    3
!         typical                         1                    3
!
!         - counter for the number of residue pools for standing, flat, buried  and
!           below ground residues

!     idtype  - index of residue type
!
!         1 = standing
!         2 = flat or surface
!         3 = buried (non root)
!         4 = root
!         5 = stem number
!
!          - used for assigning residue decomposition rates

!     iht  - index for standing residues vertical distribution
!
!         range values                    min                  max
!         ------------                    ------               ------
!         fixed                           1                    5
!         typical                         1                    5

!     ipool - residue pool age variable for standing, buried, root and
!             stem number age pools.
!
!         range values                    min                  max
!         ------------                    ------               ------
!         fixed                           1                    2
!         typical                         1                    2

!     ipoolf - residue pool age variable for surface residues.
!
!         range values                    min                  max
!         ------------                    ------               ------
!         fixed                           1                    3
!         typical                         1                    3

!     isz     - soil layer indexing variable
!
!         range values                    min                  max
!         ------------                    ------               ------
!         fixed                           1                    max no. soil layers
!         typical                         1                    ?

!     aqua    - sum of precip, irrigation and snow melt (mm)
!
!         range values                    min                  max
!         ------------                    ------               ------
!         fixed                           fmin                 fmax
!         typical                         tmin                 tmax

!     covfact - coeficient for converting mass to cover  (m^2/kg)
!
!         range values                    min                  max
!         ------------                    ------               ------
!         fixed                           0                    10
!         typical                         0                    10

!     cumddf - cummlative decomp days for surface res. by pool  (days)
!
!         range values                    min                  max
!         ------------                    ------               ------
!         fixed                           0                    fmax
!         typical                         0                    tmax

!     cumddg - cumm. decomp days below ground res by pool and layer (days)
!
!         range values                    min                  max
!         ------------                    ------               ------
!         fixed                           0                    fmax
!         typical                         0                    tmax

!     cumdds - cumulative decomp days for standing res. by pool (days)
!
!         range values                    min                  max
!         ------------                    ------               ------
!         fixed                           0                    fmax
!         typical                         0                    tmax

!     ddsthrsh - threshhold number of decomp. days before stems begin to fall
!
!         range values                    min                  max
!         ------------                    ------               ------
!         fixed                           0                    25
!         typical                         0                    15

!     diddf   - decomposition day for surface residue (0 to 1)
!
!         range values                    min                  max
!         ------------                    ------               ------
!         fixed                           0                    1
!         typical                         0                    1

!     diddg() - decomp. day for below ground residue by soil layer (0 to 1)
!
!         range values                    min                  max
!         ------------                    ------               ------
!         fixed                           0                    1
!         typical                         0                    1

!     didds   - decomposition day for standing residue (0 to 1)
!
!         range values                    min                  max
!         ------------                    ------               ------
!         fixed                           0                    1
!         typical                         0                    1

!     ditca   - temperature coef. for above ground res. (0 to 1)
!
!         range values                    min                  max
!         ------------                    ------               ------
!         fixed                           0                    1
!         typical                         0                    1

!     ditcg() - temperature coef. below ground res. by soil layer (0 to 1)
!
!         range values                    min                  max
!         ------------                    ------               ------
!         fixed                           0                    1
!         typical                         0                    1

!     diwcf   - daily water coefficient for surface residues (0 to 1)
!
!         range values                    min                  max
!         ------------                    ------               ------
!         fixed                           0                    1
!         typical                         0                    1

!     diwcg - water coef. for below ground res. by soil layer (0 to 1)
!
!         range values                    min                  max
!         ------------                    ------               ------
!         fixed                           0                    1
!         typical                         0                    1

!     diwcs - water coefficient for standing residues (0 to 1)
!
!         range values                    min                  max
!         ------------                    ------               ------
!         fixed                           0                    1
!         typical                         0                    1

!     diwcsy  - water coefficient from previous day standing res.  (0 to 1)
!
!         range values                    min                  max
!         ------------                    ------               ------
!         fixed                           0                    1
!         typical                         0                    1

!     dkrate - decomposition rate for each age pool and location (d < 1) (g/g/day)
!
!         range values                    min                  max
!         ------------                    ------               ------
!         fixed                           0                    .1
!         typical                         0                    .1

!     adresevapa - coefficient a in relation ea/ep = exp(resevap * (flat mass kg/m^2)**resevapb)
!     adresevapb - coefficient b in relation ea/ep = exp(resevap * (flat mass kg/m^2)**resevapb)

      common / decom /                                                  &
     &    iwets,iwetf,iwetg,                                            &
     &    dweti, hrvflag, iage, idtype, iht,                            &
     &    ipool, ipoolf, isz,                                           &
     &    aqua, covfact, cumddf, cumddg, cumdds,                        &
     &    ddsthrsh, diddf, diddg, didds,                                &
     &    ditca, ditcg, diwcf, diwcg, diwcs, diwcsy,                    &
     &    dkrate, adresevapa, adresevapb

      save :: /decom/
