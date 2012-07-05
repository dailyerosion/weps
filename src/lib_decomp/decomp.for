!$Author$
!$Date$
!$Revision$
!$HeadURL$
      subroutine decomp(isr)
!
!     +++ PURPOSE + + +
!
!     decomp.for calculates change in standing, flat and belowground
!     biomass. It carries three age pools of residues most recent, previous and
!     combined old material. Decomp also estimates the number of standing
!     stems and soil surface cover provided by the surface residues.
!     Data for each subregion that is needed following a harvest is
!     maintained within local variables on a daily basis.
!
!     Authors: Harry Schomberg and Jean Steiner
!              USDA-ARS Bushland, TX
!              USDA-ARS Watkinsville, GA
!
!     + + +   KEYWORDS + + +
!     decompdays, standing residue, surface residue, buried residue,
!     soil cover, residue cover, decomposition day
!
!       + + +  PARAMTERS AND COMMON BLOCKS +++

      include 'p1werm.inc'
      include 'w1clig.inc'
      include 's1layr.inc'
      include 'p1const.inc'
      include 'm1flag.inc'
      include 'm1dbug.inc'

!   These hydrology common blocks provide soil temp, moisture and irrigation

      include 'h1temp.inc'
      include 'h1db1.inc'
      include 'h1hydro.inc'

! Werm $INCLUDE:file for decomp variables
      include 'c1db1.inc'

      ! decomposition pools
      include 'd1glob.inc'
      ! crop pools (flat leaf included for decomp)
      include 'c1glob.inc'

! Local $INCLUDE:file for decomp variables
      include 'decomp/decomp.inc'

!     + + +   ARGUMENT DECLARATIONS + + +
      integer   isr

!
!     + + +   ARGUMENT DEFINITIIONS + + +
!     isr     - current subregion
!
!     + + +   LOCAL VARIABLES in DECOMP.inc  + + +

!    aqua    - sum of precip, irrigation and snow melt (mm)
!    covfact - coeficient for converting mass to cover  (m^2/kg)
!    cumddf  - cummlative decomp days for surface res. by pool  (days)
!    cumddg  - cumm. decomp days below ground res by pool and layer (days)
!    cumdds  - cumulative decomp days for standing res. by pool (days)
!    ddsthrsh- threshhold number of decomp. days before stems begin to fall
!    diddf   - decomposition day for surface residue (0. to 1.)
!    diddg   - decomp. day for below ground residue by soil layer (0. to 1.)
!    didds   - decomposition day for standing residue (0. to 1.)
!    ditca   - temperature coef. for above ground res. (0. to 1.)
!    ditcg   - temperature coef. below ground res. by soil layer (0. to 1.)
!    diwcf   - daily water coefficient for surface residues (0. to 1.)
!    diwcg   - water coef. for below ground res. by soil layer (0. to 1.)
!    diwcs   - water coefficient for standing res.  (0. to 1.)
!    diwcsy  - water coefficient from previous day standing res.  (0. to 1.)
!    dkrate  - decomposition rate for each age pool and location (d < 1.) (g/g/day)
!    admf   - surface biomass by age pool  (kg / m^2)
!    admbgz   - below ground biomass by layer and pool (kg / m^2)
!    admrtz   - root biomass by layer and pool (kg / m^2)
!    admst   - standing biomass by age pool (kg / m^2)
!    dweti   - days since anticedent moisture (4. to 0.) index
!    hrvflag - harvest flag used to increment residue pools
!    iage    - residue pool age index
!    idtype  - index of residue type   1 = standing 2 = flat or surface
!                         3 = buried (non root) 4 = root  5 =stem number
!    iht     - index for standing residues vertical distribution
!    mnbpls   - residue pool age variable for standing, buried, root,
!               flat, and stem number age pools.
!    isz     - soil layer indexing variable
!    addstm   - standing stem number by pool  (no. / m^2)
!
!     + + +  VARIABLES MAINTIANED BY SUBREGION + + +
!
!         dweti(mnsub)
!         diwcsy(mnsub)
!         diwcg(mnsz)
!         ditcg(mnsz)
!         diddg(mnsz)
!         cumdds(mnbpls,mnsub)
!         cumddf(mnbpls,mnsub)
!         cumddg(mnsz,mnbpls,mnsub)

! + + +  ADDITIONAL LOCAL VARIABLES NOT IN DECOMP.KOM + + +
!     These are used in tc function.
!     tavgsq - average temperature squared  (C)
!     temp    - average air or soil temp    (C)
!     toptsq  - optimum temperature for residue decomposition (32C)

      real dec_fac, prev_mass
      real leaf_fac, store_fac
      parameter (leaf_fac = 3.0)
      parameter (store_fac = 1.5)

!     leaf_fac  - leaf decomposition rate = leaf_fac * stem decomposition rate
!     store_fac - store decomposition rate = store_fac * stem decomposition rate

!     + + +   FUNCTION CALLS +++
!
!     tc - Calculates temperature based scaling factor
      real  tc
      
      logical dbgflg
!
!     + + +   DATA INITIALIZATIONS + + +
!  These data initializations are being done every day.  Need to make
!  sure that when a harvest takes place that all the decomp pools are
!  updated correctly.
!
      data dbgflg /.false./

      if (am0ddb .eq. 1) call ddbug(isr, nslay(isr))

      if (dbgflg) write(*,*) 'decomp 1'

!     + + +  END SPECIFICATIONS + + +
      if (dbgflg) write(*,*) 'decomp 1a'

!   call initilization
!     if (am0dif .eqv. .true.) then
!         call decini (isr)
!     end if
!     am0dif = .false.
!  Calculation of water coefficent for decomp days
! Standing residues water factor  ( 0. to 1. )
! Steiner et al. 1994 Agronomy Journal  Jan-Feb issue

! sum rain, irr, snow melt

      if (dbgflg) write(*,*) 'decomp 2'
      aqua = awzdpt + ahzirr(isr) + ahzsmt(isr)

! Test for water input day.

      if (aqua .gt. 0.) then

         dweti(isr) = 4.0                  !set # of days for antecedent
         diwcs = aqua / 4.0                !4 mm or more is optimum for decomp.
         diwcs = diwcs + diwcsy(isr) * 0.4 !add previous antecdent moisture

         if (diwcs.gt.1.) diwcs = 1.0      !Limit no greater than 1.

      else if (dweti(isr).gt.0.) then      !No precip but recent water input
         dweti(isr) = dweti(isr) - 1.0     !decrement days since precip
         diwcs = diwcsy(isr)*0.4           !set diwcs to decremented value

      else                                 
         diwcs = 0.0                       !no decomp after 4 or more days without precip.
      endif

      diwcsy(isr) = diwcs !save diwcs for calc. of tomorrows water factor

!  Surface water factor same as standing  (0. to 1.)
!     Need to set up better test of water factor  (12-8-1993)


!     code changed to use hydrology global variables  HHS 1- 4- 1994
!     old code >    diwcf = theta(1)/thetaf(1)

      diwcf = ahrwc0(12,isr) / ahrwcf(1,isr) !use water content at surface at 12 noon
      !diwcf = ahrwc(1,isr) / ahrwcf(1,isr) !use water content of soil layer 1

!     water factor = water content of top soil layer / optimum water content of top soil layer
      if (diwcf.gt.1.0) diwcf = 1.0
      diwcf = max(diwcf,diwcs) !for flat residue, use the greatest of flat and standing water factor



      !diwcf = diwcs !flat: precip based (like standing) --> underestimation of decomposition
      !diwcf = 1.0 !flat: optimum moisture for decomp --> overestimation of decomposition




! Belowground water factor             (0. to 1.)
! Stanford and Epstien 1974, SSSAJ 34:103-107 theta/thetaopt


! code changed to use global hydrology variables HHS 1-4-1994
!
!      do 30 isz = 1 , nslay(isr)
!          diwcg(isz) = theta(isz)/ thetaf(isz)
!          if (diwcg(isz) .gt. 1.) diwcg(isz) = 1.
!   30 continue

      if (dbgflg) write(*,*) 'decomp 3'

      do 30 isz = 1 , nslay(isr)
         diwcg(isz) = ahrwc(isz,isr)/ ahrwcf(isz,isr)
!        water factor = water content of soil layer / optimum water content of soil layer
         if (diwcg(isz) .gt. 1.0) diwcg(isz) = 1.0
   30 continue

! Calculate temperature coefficient    (0. to 1.)
! Stroo et al., 1989, SSSAJ 53:91-99 used in the tc function.
! Above ground (standing and flat) biomass tc use air temp.
! Compute TC(Tmax) and TC(Tmin)and then average the two results.
! This is the way it was intended to be (Harry Schomberg, 
! phone call with Simon van Donk, July 2002)

      ditca =  (tc(awtdmn) + tc(awtdmx))/2

! Below ground biomass tc calculated for each soil layer
!!use average of max and min for calculation

      do 40  isz = 1, nslay(isr)

! Code changed to use global hydrology soil temp variable
!              tsavg= (tsmax(isz) + tsmin(isz))/2.
!              ditcg(isz) = tc(tsavg)

         ditcg(isz) =  tc(ahtsav(isz,isr))
   40 continue

! Select minimum of temperature or water functions for
! the quantity (fraction) of a decomposition day accumulated
! during the current 24 hr period.

!  for standing, flat and buried residues
      didds = min(diwcs,ditca) !standing
      diddf = min(diwcf,ditca) !flat
      do 50 isz = 1, nslay(isr)
         diddg(isz) = min(diwcg(isz),ditcg(isz)) !buried
   50 continue

! Summation of DECOMPOSITION days for graphing
! this is indexed based on the number of residue age pools

! standing, flat and below ground
      do 70  iage = 1,mnbpls
         cumdds(iage,isr) = cumdds(iage,isr) + didds
         cumddf(iage,isr) = cumddf(iage,isr) + diddf
         do 60 isz = 1, nslay(isr)
            cumddg(isz,iage,isr)=cumddg(isz,iage,isr)                   &
     &             + diddg(isz)
 60      continue
 70   continue

      if (dbgflg) write(*,*) 'decomp 4'

! Decompose each age pool of residue based on decomp days accumulated in
! the present 24 hr using the numerical formula for exponential decay
!      Mass(t) = mass(t-1) * (1 - k * dday)

      ! crop flat leaves are dead and assumed to start decomposing
      if( acmflatleaf(isr) .gt. 0.0 ) then
          dec_fac = max(0.0, 1.0 - leaf_fac*acdkrate(2,isr)*diddf)
          acmflatleaf(isr) = acmflatleaf(isr) * dec_fac
      end if

      do 100  iage = 1,mnbpls
        !standing residue mass
        dec_fac = max(0.0, 1.0 - dkrate(1,iage,isr)*didds)
        admstandstem(iage,isr) = admstandstem(iage,isr) * dec_fac
        dec_fac = max(0.0, 1.0 - leaf_fac*dkrate(1,iage,isr)*didds)
        admstandleaf(iage,isr) = admstandleaf(iage,isr) * dec_fac
        dec_fac = max(0.0, 1.0 - store_fac*dkrate(1,iage,isr)*didds)
        admstandstore(iage,isr) = admstandstore(iage,isr) * dec_fac

        !flat residue mass
        dec_fac = max(0.0, 1.0 - dkrate(2,iage,isr)*diddf)
        admflatstem(iage,isr) = admflatstem(iage,isr) * dec_fac
        dec_fac = max(0.0, 1.0 - leaf_fac*dkrate(2,iage,isr)*diddf)
        admflatleaf(iage,isr) = admflatleaf(iage,isr) * dec_fac
        dec_fac = max(0.0, 1.0 - store_fac*dkrate(2,iage,isr)*diddf)
        admflatstore(iage,isr) = admflatstore(iage,isr) * dec_fac

        ! unburied root mass
        dec_fac = max(0.0, 1.0 - dkrate(2,iage,isr)*diddf)
        admflatrootstore(iage,isr) =admflatrootstore(iage,isr) * dec_fac
        admflatrootfiber(iage,isr) =admflatrootfiber(iage,isr) * dec_fac

        do 90  isz = 1, nslay(isr)
          ! buried surface biomass
          dec_fac = max(0.0, 1.0-dkrate(3,iage,isr)*diddg(isz))
          admbgstemz(isz,iage,isr) = admbgstemz(isz,iage,isr) * dec_fac
          dec_fac = max(0.0, 1.0-leaf_fac*dkrate(3,iage,isr)*diddg(isz))
          admbgleafz(isz,iage,isr) = admbgleafz(isz,iage,isr) * dec_fac
          dec_fac = max(0.0,1.0-store_fac*dkrate(3,iage,isr)*diddg(isz))
          admbgstorez(isz,iage,isr)=admbgstorez(isz,iage,isr) * dec_fac

          ! buried root biomass
          dec_fac = max(0.0, 1.0 - dkrate(4,iage,isr)*diddg(isz))
          admbgrootstorez(isz,iage,isr) = admbgrootstorez(isz,iage,isr) &
     &                                  * dec_fac
          admbgrootfiberz(isz,iage,isr) = admbgrootfiberz(isz,iage,isr) &
     &                                  * dec_fac
  90      continue
 100  continue

! Change standing stem number and adjust the mass for standing
! and surface biomass Steiner et al., 1994 Agronomy Journal

      if (dbgflg) write(*,*) 'decomp 5'

      do 120  iage = 1,mnbpls
         ! check for threshold ddays value before allowing stems to decline
         if (cumdds(iage,isr).gt.ddsthrsh(iage,isr)) then
            if (addstm(iage,isr) .gt. 0.0) then
               ! Calculate stem fall and new stem number. This stem fall
               ! ratio is then applied to the standing pools since their 
               ! mass is transferred to flat in the same proportion
               dec_fac = max(0.0, 1.0 - dkrate(5,iage,isr)* didds)
               addstm(iage,isr) = addstm(iage,isr) * dec_fac

               ! Move corresponding standing stem mass to flat stem mass
               prev_mass = admstandstem(iage,isr)
               admstandstem(iage,isr) = admstandstem(iage,isr) * dec_fac
               admflatstem(iage,isr) = admflatstem(iage,isr)            &
     &                            + (prev_mass - admstandstem(iage,isr))

               ! Move corresponding standing leaf mass to flat leaf mass
               prev_mass = admstandleaf(iage,isr)
               admstandleaf(iage,isr) = admstandleaf(iage,isr) * dec_fac
               admflatleaf(iage,isr) = admflatleaf(iage,isr)            &
     &                            + (prev_mass - admstandleaf(iage,isr))

               ! Move corresponding standing store mass to flat store mass
               prev_mass = admstandstore(iage,isr)
               admstandstore(iage,isr)= admstandstore(iage,isr)* dec_fac
               admflatstore(iage,isr) = admflatstore(iage,isr)          &
     &                           + (prev_mass - admstandstore(iage,isr))
            end if
         end if
  120 continue

      if (dbgflg) write(*,*) 'decomp 10'
      if (am0ddb .eq. 1) call ddbug(isr, nslay(isr))
      if ((am0dfl .eq. 1).or.(am0dfl .eq. 2).or.(am0dfl .eq.3))         &
     &   call decout

      return
      end

! + + +  Function tc

      real function tc (temp)

! + + +  PURPOSE + + +
!
!     Calculate temperature coefficients for estimation of decompsition days
!     using the temperature of the environment the residues are in.
!
!     Equation form is from Stroo et. al, 1989. SSSAJ 53:91-99
!     we used a different optimum temperature and set the "a" value
!     to zero to make the minimum microbial activity corespond to 0 C
!     In their equation the entire value was multiplied by 1.32 to
!     broaden the temperature range where temperature was optimum.
!     We felt that this parameter should be dropped
!     to allow greater interacting effects of water and moisture.
!
!  + + +  DECLARATION OF ARGUMENT + + +

      real temp

! + + +  DECLARATION OF VARIABLES  + + +

      real toptsq, tavgsq

! + + +  DEFINITION OF VARIABLEES AND ARGUMENTS + + +
!     all in degrees C

!     temp    - temperature of air or soil layer
!     toptsq  - optimum temperature squared
!     tavgsq  - temp variable squared

! + + +  END OF SPECIFICATION + + +

      if (temp .lt. 0.0) then
         tc = 0.0
      else
         toptsq = 32.0 * 32.0
         tavgsq = temp * temp
         tc = (2.0*tavgsq*toptsq-tavgsq*tavgsq) / (toptsq*toptsq)
      endif

      if (tc .gt. 1.0) tc = 1.0
      if (tc .lt. 0.0) tc = 0.0 !this prevents tc from becoming less than 0 at high temperatures! SVD

      return
      end

