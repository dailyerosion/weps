!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine cropupdate(                                            &
     &      bszrgh, bszlyd,                                             &
     &      bnslay,                                                     &
     &      bhzfurcut,                                                  &
     &      bhztransprtmin, bhztransprtmax, crop, croptot )

!     + + + PURPOSE + + +
      ! calculates values of derived variables based on the present values
      ! or the state variables. The derived variables are commonly used
      ! where residue totals are required.

      use weps_interface_defs, ignore_me=>cropupdate
      use biomaterial, only: biomatter, biototal
      use p1unconv_mod, only: pi
      use wind_mod, only: biodrag
      use crop_growth_mod, only: ht_dia_sai

!     + + + ARGUMENT DECLARATIONS + + +
      ! state variables
      real :: bszrgh    ! ridge height
      real :: bszlyd(*) ! Depth to bottom of each soil layer(mm)
      ! database variables
      integer :: bnslay ! number of soil layers

      ! derived variables
      real bhzfurcut
      real bhztransprtmin, bhztransprtmax

      type(biomatter), intent(inout) :: crop    ! structure containing full crop description
      type(biototal), intent(inout) :: croptot  ! structure containing derived variables

!     + + + VARIABLE DEFINITIONS + + +
!     bhzfurcut - estimated furrow bottom depth below flat soil surface (mm)
!     bhztransprtmin - root depth where transpiration depth reduction begins (m)
!     bhztransprtmax - root depth where transpiration depth equals root depth (m)

!     LOCAL VARIABLES
      integer idx
      real temp1, temp2

      integer :: ncanlay  ! number of standing crop height divisions

      ! parameter to control depth of slice
      real scidepth
      parameter( scidepth = 101.6 ) ! mm 101.6 = 4 inches for SCI
      real weppdepth
      parameter( weppdepth = 150.0 ) ! mm 150.0 = 5.9 inches for WEPP

      temp1 = 0.0
      temp2 = 0.0

      ! accumulate layer values into root mass totals
      crop%deriv%mbgstem = 0.0
      crop%deriv%mbgrootstore = 0.0
      crop%deriv%mbgrootfiber = 0.0
      do idx = 1, bnslay
          crop%deriv%mbgstem = crop%deriv%mbgstem + crop%mass%stemz(idx)
          crop%deriv%mbgrootstore = crop%deriv%mbgrootstore + crop%mass%rootstorez(idx)
          crop%deriv%mbgrootfiber = crop%deriv%mbgrootfiber + crop%mass%rootfiberz(idx)
      end do

      crop%deriv%mst = crop%mass%standstem + crop%mass%standleaf + crop%mass%standstore
      crop%deriv%mf = crop%mass%flatstem + crop%mass%flatleaf + crop%mass%flatstore
      crop%deriv%mrt = crop%deriv%mbgstem + crop%deriv%mbgrootstore + crop%deriv%mbgrootfiber
      crop%deriv%m = crop%deriv%mst + crop%deriv%mf + crop%deriv%mrt

      do idx = 1, bnslay
          crop%deriv%mrtz(idx) = crop%mass%stemz(idx) + crop%mass%rootstorez(idx) &
     &                  + crop%mass%rootfiberz(idx)
      end do

      ! calculate new stem area index and representative stem diameter
      call ht_dia_sai( crop%geometry%dpop, crop%mass%standstem, temp1, &
     &                 crop%database%ssa, crop%database%ssb, crop%geometry%dstm, &
     &                 crop%geometry%zht, temp2, crop%geometry%xstmrep, crop%deriv%rsai )

      ! leaf area index for standing material
      ! m^2 leaf/kg * kg/m^2 ground = m^2 leaf/m^2 ground
      crop%deriv%rlai = crop%database%sla * crop%mass%standleaf

      ! set stem and leaf area by plant height increments
      ! these are divided equally for a first approximation
      ncanlay = size(crop%deriv%rsaz)
      do idx = 1, ncanlay
          crop%deriv%rsaz(idx) = crop%deriv%rsai / ncanlay
          crop%deriv%rlaz(idx) = crop%deriv%rlai / ncanlay
      end do

      ! effective silhouette
      crop%deriv%rcd = biodrag(0.0, 0.0, crop%deriv%rlai, crop%deriv%rsai, &
     &                crop%geometry%rg, crop%geometry%xrow, crop%geometry%zht, bszrgh)

      crop%deriv%fcancov = 1.0 - exp( - crop%database%ck * crop%deriv%rlai)  !crop leaf interception area (canopy cover)
      crop%deriv%ffcv = 1.0 - exp( -crop%database%covfact * crop%deriv%mf )
      crop%deriv%fscv = crop%geometry%dstm * pi * (crop%database%xstm/2.0)*(crop%database%xstm/2.0)
      if (crop%deriv%fscv > 1.0) crop%deriv%fscv = 1.0
      crop%deriv%ftcv = crop%deriv%fscv + crop%deriv%ffcv !no overlap
      if (crop%deriv%ftcv > 1.0) crop%deriv%ftcv = 1.0

      ! transpiration depth as a function of furrow cut depth and root depth
      crop%deriv%ztranspdepth = transpdepth(crop%geometry%zrtd, bhzfurcut, bhztransprtmin, bhztransprtmax)

      ! assign values to croptot variables
      ! Buried (to a 4 inch depth) root mass across pools (kg/m^2)
      croptot%mrttotto4 = valbydepth(bnslay, bszlyd, crop%deriv%mrtz, 2, 0.0, scidepth)
      croptot%dstmtot = crop%geometry%dstm     ! total number of stems  per unit area (#/m^2)
      croptot%zht_ave = crop%geometry%zht      ! Weighted ave height across pools (m)
      croptot%zmht = crop%geometry%zht         ! Tallest biomass height across pools (m)
      croptot%xstmrep = crop%geometry%xstmrep  ! Weighted ave representative stem diameter across pools (m)
      croptot%zrtd = crop%geometry%zrtd        ! root depth (m)
      croptot%mstandstore = crop%mass%standstore
      croptot%mflatstore = crop%mass%flatstore
      croptot%mtot = crop%deriv%m         ! Total mass across pools (standing + flat + roots + buried) (kg/m^2)
      ! Total mass across pools (standing + flat + roots + buried to a 4 inch depth) (kg/m^2)
      croptot%mtotto4 = crop%deriv%mst + crop%deriv%mf + croptot%mrttotto4
      croptot%msttot = crop%deriv%mst       ! Standing mass across pools (standstem + standleaf + standstore) (kg/m^2)
      croptot%mftot = crop%deriv%mf        ! Flat mass across pools (flatstem + flatleaf + flatstore) (kg/m^2)
      croptot%mbgtot = 0.0       ! Buried mass across pools (kg/m^2)
      croptot%mbgtotto4 = 0.0    ! Buried (to a 4 inch depth) mass across pools (kg/m^2)
      croptot%mbgtotto15 = 0.0   ! Buried (to a 15 cm depth) mass across pools (kg/m^2)
      croptot%mrttot = crop%deriv%mrt       ! Buried root mass across pools (kg/m^2)
      ! Buried (to a 15 cm depth) root mass across pools (kg/m^2)
      croptot%mrttotto15 = valbydepth(bnslay, bszlyd, crop%deriv%mrtz, 2, 0.0, weppdepth)

      do idx = 1, bnslay
          croptot%mrtz(idx) = crop%deriv%mrtz(idx) ! Buried root mass by soil layer (kg/m^2)
          croptot%mbgz(idx) = 0.0           ! Buried mass by soil layer (kg/m^2)
      end do

      croptot%rsaitot = crop%deriv%rsai      ! total of stem area index across pools (m^2/m^2)
      croptot%rlaitot = crop%deriv%rlai      ! total of leaf area index across pools (m^2/m^2)

      do idx = 1, ncanlay
          croptot%rsaz(idx) = crop%deriv%rsai / ncanlay           ! stem area index by height (1/m)
          croptot%rlaz(idx) = crop%deriv%rlai / ncanlay           ! leaf area index by height (1/m)
      end do

      ! effective Biomass silhouette area across pools (SAI+LAI) (m^2/m^2) (combination of leaf area and stem area indices)
      croptot%rcdtot = biodrag(0.0,0.0,croptot%rlaitot,croptot%rsaitot, crop%geometry%rg, crop%geometry%xrow, &
                               croptot%zht_ave, bszrgh) 

      croptot%ffcvtot = crop%deriv%ffcv      ! biomass cover across pools - flat (m^2/m^2)
      croptot%fscvtot = crop%deriv%fscv      ! biomass cover across pools - standing (m^2/m^2)
      croptot%ftcvtot = crop%deriv%ftcv      ! biomass cover across pools - total (m^2/m^2) (adffcvtot + adfscvtot)
      croptot%ftcancov = crop%deriv%fcancov     ! fraction of soil surface covered by canopy across pools (m^2/m^2)
      croptot%evapredu = 0.0     ! composite evaporation reduction from across pools (ea/ep ratio)

      return
      end
