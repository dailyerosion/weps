!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine sumbio(isr)
! ***************************************************************** wjr
! Contains init code from main

!       Edit History
!       04-Mar-99       wjr     created

      include 'p1werm.inc'
      include 's1layr.inc'
      include 's1sgeo.inc'
      include 'c1gen.inc'
      include 'c1db1.inc'
      include 'b1glob.inc'
      include 'c1glob.inc'
      include 'd1glob.inc'
      include 'main/main.inc'
      include 'decomp/decomp.inc'

! arguments

      integer isr

! local variables

      integer idx,jdx
      real atotal, aheight, a(0:mnbpls)

!     + + + FUNCTIONS CALLED + + +
      real    biodrag
      real    resevapredu

! *****************************************************************
!     Compute total number of stems

      abdstm(isr) = acdstm(isr)
      do 10 idx=1,mnbpls
        abdstm(isr) = abdstm(isr) + addstm(idx,isr)
10    continue
! *****************************************************************
!     compute the weighted average residue height

!     determine weighting factors (stem area index)
      atotal = 0.0
      do 14 idx=1,mnbpls
        a(idx) = adzht(idx,isr) * addstm(idx,isr) * adxstmrep(idx,isr)
        atotal = atotal + a(idx)
14    continue

!     linearly weight height from each residue pool based on stem area index
      aheight = 0.0
      if( atotal .gt. 0.0 ) then
        do idx=1,mnbpls
          aheight = aheight + adzht(idx,isr) * a(idx) / atotal
        end do
      end if

      adzht_ave(isr) = aheight
! *****************************************************************
!     compute the weighted average biomass height

!     determine weighting factors (stem area index)
      a(0) = aczht(isr) * acdstm(isr) * acxstmrep(isr)
!      atotal = a(0)
!      do 15 idx=1,mnbpls
!        a(idx) = adzht(idx,isr) * addstm(idx,isr) * adxstmrep(idx,isr)
!        atotal = atotal + a(idx)
!15    continue
        atotal = atotal + a(0)

!     linearly weight height from each pool (crop and residue) based on stem area index
      if( atotal .gt. 0.0 ) then
        aheight = aczht(isr) * a(0) / atotal
        do idx=1,mnbpls
          aheight = aheight + adzht(idx,isr) * a(idx) / atotal
        end do
      else
        aheight = 0.0
      end if

      abzht(isr) = aheight
! *****************************************************************
!     determine the pool with the tallest biomass height
!     and use that value
      abzmht(isr) = aczht(isr)
      do 30 idx=1,mnbpls
        if (abzmht(isr) .lt. adzht(idx,isr)) then
          abzmht(isr) = adzht(idx,isr)
        end if
30    continue

! *****************************************************************
!     sum the flat biomass from each pool
!     sum the standing biomass from each pool
!     sum the buried biomass from each pool
!     sum the root biomass from each pool

      abmf(isr) = acmf(isr) + admftot(isr)    !flat
      abmst(isr) = acmst(isr) + admsttot(isr) !standing 

      abmbg(isr) = 0.0        !below ground
      abmrt(isr) = acmrt(isr) !roots
      do 40 idx=1,mnbpls
        abmbg(isr) = abmbg(isr) + admbg(idx,isr) !below ground
        abmrt(isr) = abmrt(isr) + admrt(idx,isr) !roots
40    continue
! *****************************************************************
!     determine the total mass of biomass (above, flat and below ground)
      abm(isr) = acm(isr) + admtot(isr)
! *****************************************************************
!     sum the buried biomass by layer
!     sum the root mass by layer
      do 60 jdx=1,nslay(isr)
        abmbgz(jdx,isr) = 0.0
        abmrtz(jdx,isr) = 0.0
        do 50 idx=1,mnbpls
          abmbgz(jdx,isr) = abmbgz(jdx,isr) + admbgz(jdx,idx,isr)
          abmrtz(jdx,isr) = abmrtz(jdx,isr) + admrtz(jdx,idx,isr)
50      continue
60    continue
! *****************************************************************
!     sum the stem area index and leaf area index values
      abrsai(isr) = acrsai(isr) + adrsaitot(isr)
      abrlai(isr) = acrlai(isr) + adrlaitot(isr)

!     compute "effective biomass (live and dead) drag coefficient
!     from SAI and LAI values
      abrcd(isr) = biodrag( adrlaitot(isr), adrsaitot(isr), acrlai(isr),&
     &             acrsai(isr), ac0rg(isr), acxrow(isr), aczht(isr),    &
     &             aszrgh(isr) )
! *****************************************************************
!     sum the stem area index and leaf area index values by height
!     this is based upon the "tallest" biomass pool height value
!     (abzmht) determined previously.

      ! This divides the biomass equally into the height increments
      ! it isn't used yet and !really!!! is not right!!! since each
      ! pool should have it's own height, and hence divisions. This
      ! should at least stay within the arrays.
      do jdx = 1, mncz
          abrsaz(jdx,isr) = acrsaz(jdx,isr)
          abrlaz(jdx,isr) = acrlaz(jdx,isr)
          do idx=1,mnbpls
              abrsaz(jdx,isr) = abrsaz(jdx,isr) + adrsaz(jdx,idx,isr)
              abrlaz(jdx,isr) = abrlaz(jdx,isr) + adrlaz(jdx,idx,isr)
        end do
      end do


! *****************************************************************
!     Combine residue cover from crop and decomp. pools.
!     Overlap only applies when adding flat and flat, not flat and standing,
!     or standing and standing.
!     Note that these values shouldn't ever exceed 1.0 or be less than zero

      ! flat and flat, with overlap
      abffcv(isr) = acffcv(isr) + adffcvtot(isr) * (1.0-acffcv(isr))

      ! standing and standing, no overlap
      abfscv(isr) = acfscv(isr) + adfscvtot(isr)
      if (abfscv(isr) > 1.0) abfscv(isr) = 1.0

      ! flat and standing, no overlap
      abftcv(isr) =  abffcv(isr) + abfscv(isr)
      if (abftcv(isr) > 1.0) abftcv(isr) = 1.0

! ***        write(*,*) ' sumbio before: abffcv acfscv acftcv ',
! ***     *  abffcv(isr), acfscv(isr),acftcv(isr)

!!    do 100 idx=1,mnbpls

! ***          write(*,*) ' sumbio before: adffcv adfscv adftcv ',
! ***     *    adffcv(idx,isr), adfscv(idx,isr),adftcv(idx,isr)
!!      abffcv(isr) = abffcv(isr) + adffcv(idx,isr) * (1.0-abffcv(isr)) !flat
!!      abfscv(isr) = abfscv(isr) + adfscv(idx,isr) * (1.0-abfscv(isr)) !standing
!       do standing stems of different crops overlap? SVD
!!      abftcv(isr) = abftcv(isr) + adftcv(idx,isr) * (1.0-abftcv(isr)) !total

!! 100   continue
! ***      write(*,*) ' sumbio after: abffcv abfscv abftcv ',
! ***    *  abffcv(isr), abfscv(isr),abftcv(isr)

      ! canopy cover for all biomass (overlaps)
      abfcancov(isr)=acfcancov(isr)+adftcancov(isr)*(1.0-acfcancov(isr))

!     find composite evaporation supression for total flat residue
      ! set initial value to no residue condition
      abevapredu(isr) = 1.0
      ! start with older flat residue layers
      do idx = mnbpls,1,-1
          if( admf(idx,isr) .gt. 0.0 ) then
              abevapredu(isr) = resevapredu( abevapredu(isr),           &
     &         admf(idx,isr), adresevapa(idx,isr), adresevapb(idx,isr) )
          end if
      end do
      ! add any flat crop residue to the reduction
      if( acmf(isr) .gt. 0.0 ) then
          abevapredu(isr) = resevapredu( abevapredu(isr), acmf(isr),    &
     &                  acresevapa(isr), acresevapb(isr) )
      end if

      return
      end
