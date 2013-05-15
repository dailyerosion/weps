!$Author$
!$Date$
!$Revision$
!$HeadURL$

!     This common block contains some of the variables
!     representing the crop/decomp
!     database properties for each subregion.

!     Parameter include files:  p1werm.inc

!     + + + VARIABLE DECLARATIONS + + +

      integer          acbaflg(mnsub)
      real             acbaf(mnsub)
      real             acyraf(mnsub)
      real             acrcn(mnsub)
      integer          actdtm(mnsub)
      real             acthum(mnsub)
      real             aczmrt(mnsub)
      real             aczmxc(mnsub)
      real             acrbe(mnsub)
      real             acrbed(mnsub)
      real             ac0lad(mncz, mnsub)
      real             ac0sad(mncz, mnsub)
      real             acgrf(mnsub)
      real             acehu0(mnsub)
      real             actverndel(mnsub)
      real             ac0bceff(mnsub)
      real             ac0alf(mnsub)
      real             ac0blf(mnsub)
      real             ac0clf(mnsub)
      real             ac0dlf(mnsub)
      real             ac0arp(mnsub)
      real             ac0brp(mnsub)
      real             ac0crp(mnsub)
      real             ac0drp(mnsub)
      real             ac0aht(mnsub)
      real             ac0bht(mnsub)
      real             ac0ssa(mnsub)
      real             ac0ssb(mnsub)
!      real             ac0sla(mnsub)
      real             ac0hue(mnsub)

!      integer          acrbc(mnsub)

!  These are the names used by decomp (with an "ac" prepended)
!      real             acdkrate(mndk,mnsub)
!      real             acxstm(mnsub)
!      real             acddsthrsh(mnsub)
!      real             accovfact(mnsub)
! ------

      real    acdmaxshoot(mnsub)
      integer ac0transf(mnsub)
      real    ac0storeinit(mnsub)
      real    acfshoot(mnsub)
      real    ac0growdepth(mnsub)
      real    acfleafstem(mnsub)
      real    ac0shoot(mnsub)
      real    ac0diammax(mnsub)

      real    acfleaf2stor(mnsub)
      real    acfstem2stor(mnsub)
      real    acfstor2stor(mnsub)

!      real    acresevapa(mnsub)
!      real    acresevapb(mnsub)

      real    acyld_coef(mnsub)
      real    acresid_int(mnsub)

      real    aczloc_regrow(mnsub)

      common / c1db1 /                                                  &
     &                 acbaflg,                                         &
     &                 acbaf, acyraf,                                   &
     &                 acrcn, actdtm, acthum, aczmrt, aczmxc,           &
     &                 acrbe, acrbed, ac0lad, ac0sad ,acgrf,            &
     &                 acehu0, actverndel, ac0bceff, ac0alf ,ac0blf,    &
     &                 ac0clf, ac0dlf, ac0arp, ac0brp, ac0crp, ac0drp,  &
!     &                 ac0aht, ac0bht, ac0ssa, ac0ssb, ac0sla, ac0hue,  &
     &                 ac0aht, ac0bht, ac0ssa, ac0ssb, ac0hue,          &
!     &                 acrbc,                                           &
!     &                 acdkrate, acxstm, acddsthrsh, accovfact,         &
     &                 acdmaxshoot,                                     &
     &                 ac0transf, ac0storeinit, acfshoot,               &
     &                 ac0growdepth, acfleafstem, ac0shoot,             &
     &                 ac0diammax,                                      &
     &                 acfleaf2stor, acfstem2stor, acfstor2stor,        &
!     &                 acresevapa, acresevapb,                          &
     &                 acyld_coef, acresid_int, aczloc_regrow

      save :: /c1db1/

!     + + + VARIABLE DEFINITIONS + + +

!     acbaflg - flag for biomass adjustment action
!         0     o normal crop growth
!         1     o find biomass adjustment factor for target yield
!         2     o Use given biomass adjustment factor

!     acbaf  - biomass adjustment factor
!     acyraf - yield to biomass ratio adjustment factor
!     acrcn  - Crop C:N ratio (Mg/Mg)
!     acgrf  - Fraction of reproductive biomass that is grain (Mg/Mg)
!     actdtm - days from planting to maturity for summer crops, or the days
!              from start of spring growth to maturity for winter and 
!              perennial crops. 
!     acthum - accumulated heat units from planting to maturity, or from 
!              start of growth to maturity for perennial crops
!     aczmrt - Maximum crop root depth (m)
!     aczmxc - Maximum crop height (m)
!     acrbe  - Crop biomass-energy ratio (kg/ha*MJ)
!     acrbed - Crop biomass-energy decline rate (?)
!     ac0lad - leaf area distribution (?)
!     ac0sad - stem area distribution (?)
!     acehu0 - heat unit index leaf senescence starts
!     actverndel - thermal delay coefficient pre-vernalization
!     acrbev - relates the vapor pressure deficit to biomass conversion efficiency
!     ac0bceff - biomass conversion efficiency
!     acxstm - Crop stem diameter (m)
!     adthrs - threshold number of decomp days before stems begin to fall
!     acm2c  - Crop mass to cover coefficient  (m^2/kg)
!     adm2c  - Residue mass to cover coefficient  (m^2/kg)

!     ac0aht
!     ac0bht
!     ac0ssa - stem area to mass coefficient a, result is m^2 per plant
!     ac0ssb - stem area to mass coefficient b, argument is kg per plant
!     ac0sla
!     ac0hue

!     acrbc  - crop residue burial class (it exists in crop so it can be carried into residue)
!         1   o Fragile-very small (soybeans) residue
!         2   o Moderately tough-short (wheat) residue
!         3   o Non fragile-med (corn) residue
!         4   o Woody-large residue
!         5   o Gravel-rock

!     acdkrate - array of decomposition rate parameters
!     acdkrate(1) - standing residue mass decomposition rate (d<1) (g/g/day)
!     acdkrate(2) - flat residue mass decomposition rate (d<1) (g/g/day)
!     acdkrate(3) - buried residue mass decomposition rate (d<1) (g/g/day)
!     acdkrate(4) - root residue mass decomposition rate (d<1) (g/g/day)
!     acdkrate(5) - stem residue number decline rate (d<1) (#/m^2/day)? (fall rate)
!     acxstm      - mature crop stem diameter (m)
!     acddsthrsh  - decomposition days required for first stem fall
!     accovfact   - residue cover factor (m^2/kg)

!     acdmaxshoot - maximum number of shoots possible from each plant
!     ac0transf - db input flag:
!                 0 = crop is planted using stored biomass of seed or vegatative propagants
!                 1 = crop is planted as a transplant with roots, stems and leaves present
!     ac0storeinit - db input, crop storage root mass initialzation (mg/plant)
!     acfshoot - crop ratio of shoot diameter to length
!     ac0growdepth - depth of growing point at time of planting (m)
!     acfleafstem - crop leaf to stem mass ratio for shoots
!     ac0shoot - mass from root storage required for each regrowth shoot (mg/shoot)
!                seed shoots are smaller and adjusted for available seed mass
!     ac0diammax - crop maximum plant diameter (m)
!     acfleaf2stor - fraction of assimilate partitioned to leaf that is diverted to root store
!     acfstem2stor - fraction of assimilate partitioned to stem that is diverted to root store
!     acfstor2stor - fraction of assimilate partitioned to standing storage (reproductive) that is diverted to root store
!     acresevapa - coefficient a in relation ea/ep = exp(resevap * (flat mass kg/m^2)**resevapb)
!     acresevapb - coefficient b in relation ea/ep = exp(resevap * (flat mass kg/m^2)**resevapb)

!     acyld_coef - yield coefficient (kg/kg)     harvest_residue = acyld_coef(kg/kg) * Yield + acresid_int (kg/m^2)
!     acresid_int - residue intercept (kg/m^2)   harvest_residue = acyld_coef(kg/kg) * Yield + acresid_int (kg/m^2)

!     aczloc_regrow - location of regrowth point (+ on stem, 0 or negative from crown)
