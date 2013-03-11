!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine sumbio(isr, residue, restot, croptot, biotot)

      use biomaterial, only: biomatter, biototal

!     + + +   ARGUMENT DECLARATIONS + + +
      integer, intent(in) :: isr
      type(biomatter), dimension(:), intent(in) :: residue
      type(biototal), intent(in) :: croptot
      type(biototal), intent(inout) :: restot, biotot

!     Update geometric properties of all biomass pools

      include 'p1werm.inc'
      include 's1layr.inc'
      include 's1sgeo.inc'
      include 'c1gen.inc'
      include 'c1db1.inc'
      include 'c1glob.inc'
      include 'main/main.inc'

! local variables

      integer idx,jdx
      real atotal, aheight, a(0:size(residue))

!     + + + FUNCTIONS CALLED + + +
      real    biodrag
      real    resevapredu

! *****************************************************************
!     Compute total number of stems

      biotot%dstmtot = acdstm(isr)
      do idx=1,mnbpls
        biotot%dstmtot = biotot%dstmtot + residue(idx)%geometry%dstm
      end do
! *****************************************************************
!     compute the weighted average residue height

!     determine weighting factors (stem area index)
      atotal = 0.0
      do idx=1,mnbpls
        a(idx) = residue(idx)%geometry%zht * residue(idx)%geometry%dstm * residue(idx)%geometry%xstmrep
        atotal = atotal + a(idx)
      end do

!     linearly weight height from each residue pool based on stem area index
      aheight = 0.0
      if( atotal .gt. 0.0 ) then
        do idx=1,mnbpls
          aheight = aheight + residue(idx)%geometry%zht * a(idx) / atotal
        end do
      end if

      restot%zht_ave = aheight
! *****************************************************************
!     compute the weighted average biomass height

!     determine weighting factors (stem area index)
      a(0) = aczht(isr) * acdstm(isr) * acxstmrep(isr)
!      atotal = a(0)
!      do 15 idx=1,mnbpls
!        a(idx) = residue(idx)%geometry%zht * residue(idx)%geometry%dstm * adxstmrep(idx,isr)
!        atotal = atotal + a(idx)
!15    continue
        atotal = atotal + a(0)

!     linearly weight height from each pool (crop and residue) based on stem area index
      if( atotal .gt. 0.0 ) then
        aheight = aczht(isr) * a(0) / atotal
        do idx=1,mnbpls
          aheight = aheight + residue(idx)%geometry%zht * a(idx) / atotal
        end do
      else
        aheight = 0.0
      end if

      biotot%zht_ave = aheight
! *****************************************************************
!     determine the pool with the tallest biomass height
!     and use that value
      biotot%zmht = aczht(isr)
      do idx=1,mnbpls
        if (biotot%zmht .lt. residue(idx)%geometry%zht) then
          biotot%zmht = residue(idx)%geometry%zht
        end if
      end do

! *****************************************************************
!     sum the flat biomass from each pool
!     sum the standing biomass from each pool
!     sum the buried biomass from each pool
!     sum the root biomass from each pool

      biotot%mftot = acmf(isr) + restot%mftot    !flat
      biotot%msttot = acmst(isr) + restot%msttot !standing 

      biotot%mbgtot = 0.0        !below ground
      biotot%mrttot = acmrt(isr) !roots
      do idx=1,mnbpls
        biotot%mbgtot = biotot%mbgtot + residue(idx)%deriv%mbg !below ground
        biotot%mrttot = biotot%mrttot + residue(idx)%deriv%mbg !roots
      end do
! *****************************************************************
!     determine the total mass of biomass (above, flat and below ground)
      biotot%mtot = acm(isr) + restot%mtot
! *****************************************************************
!     sum the buried biomass by layer
!     sum the root mass by layer
      do jdx=1,nslay(isr)
        biotot%mbgz(jdx) = 0.0
        biotot%mrtz(jdx) = 0.0
        do idx=1,mnbpls
          biotot%mbgz(jdx) = biotot%mbgz(jdx) + residue(idx)%deriv%mbgz(jdx)
          biotot%mrtz(jdx) = biotot%mrtz(jdx) + residue(idx)%deriv%mrtz(jdx)
        end do
      end do
! *****************************************************************
!     sum the stem area index and leaf area index values
      biotot%rsaitot = acrsai(isr) + restot%rsaitot
      biotot%rlaitot = acrlai(isr) + restot%rlaitot

!     compute "effective biomass (live and dead) drag coefficient
!     from SAI and LAI values
      biotot%rcdtot = biodrag( restot%rlaitot, restot%rsaitot, acrlai(isr),&
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
          biotot%rsaz(jdx) = acrsaz(jdx,isr)
          biotot%rlaz(jdx) = acrlaz(jdx,isr)
          do idx=1,mnbpls
              biotot%rsaz(jdx) = biotot%rsaz(jdx) + residue(idx)%deriv%rsaz(jdx)
              biotot%rlaz(jdx) = biotot%rlaz(jdx) + residue(idx)%deriv%rlaz(jdx)
          end do
      end do


! *****************************************************************
!     Combine residue cover from crop and decomp. pools.
!     Overlap only applies when adding flat and flat, not flat and standing,
!     or standing and standing.
!     Note that these values shouldn't ever exceed 1.0 or be less than zero

      ! flat and flat, with overlap
      biotot%ffcvtot = acffcv(isr) + restot%ffcvtot * (1.0-acffcv(isr))

      ! standing and standing, no overlap
      biotot%fscvtot = acfscv(isr) + restot%fscvtot
      if (biotot%fscvtot > 1.0) biotot%fscvtot = 1.0

      ! flat and standing, no overlap
      biotot%ftcvtot =  biotot%ffcvtot + biotot%fscvtot
      if (biotot%ftcvtot > 1.0) biotot%ftcvtot = 1.0

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
      biotot%ftcancov = acfcancov(isr) + restot%ftcancov*(1.0-acfcancov(isr))

!     find composite evaporation supression for total flat residue
      ! set initial value to no residue condition
      biotot%evapredu = 1.0
      ! start with older flat residue layers
      do idx = mnbpls,1,-1
          if( residue(idx)%deriv%mf .gt. 0.0 ) then
              biotot%evapredu = resevapredu( biotot%evapredu, residue(idx)%deriv%mf, &
     &                                       residue(idx)%database%resevapa, residue(idx)%database%resevapb )
          end if
      end do
      ! add any flat crop residue to the reduction
      if( acmf(isr) .gt. 0.0 ) then
          biotot%evapredu = resevapredu( biotot%evapredu, acmf(isr),    &
     &                  acresevapa(isr), acresevapb(isr) )
      end if

      return
      end
