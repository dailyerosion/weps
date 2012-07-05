!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine cookyield(bchyfg, bnslay, dlfwt, dstwt, drpwt, drswt,  &
     &                     bcmstandstem, bcmstandleaf, bcmstandstore,   &
     &                     bcmflatstem, bcmflatleaf, bcmflatstore,      &
     &                     bcmrootstorez, lost_mass,                    &
     &                     bcyld_coef, bcresid_int, bcgrf )

!     + + + PURPOSE + + +
!     adjust incremental biomass allocation to leaf stem and reproductive
!     pools to match the input residue yield ratio and intercept value,
!     if running the model in that mode

!     + + + ARGUMENT DECLARATIONS + + +
      integer bchyfg, bnslay
      real dlfwt, dstwt, drpwt, drswt
      real bcmstandstem, bcmstandleaf, bcmstandstore
      real bcmflatstem, bcmflatleaf, bcmflatstore
      real bcmrootstorez(*), lost_mass
      real bcyld_coef, bcresid_int, bcgrf

!     + + + ARGUMENT DEFINITIONS + + +
!     bchyfg - flag indicating the part of plant to apply the "grain fraction",
!              GRF, to when removing that plant part for yield
!         0     GRF applied to above ground storage (seeds, reproductive)
!         1     GRF times growth stage factor (see growth.for) applied to
!               above ground storage (seeds, reproductive)
!         2     GRF applied to all aboveground biomass (forage)
!         3     GRF applied to leaf mass (tobacco)
!         4     GRF applied to stem mass (sugarcane)
!         5     GRF applied to below ground storage mass (potatoes, peanuts)
!     bnslay - number of soil layers
!     dlfwt - increment in leaf dry weight (kg/m^2)
!     dstwt - increment in dry weight of stem (kg/m^2)
!     drpwt - increment in reproductive mass (kg/m^2)
!     drswt - biomass diverted from partitioning to root storage
!     bcmstandstem - crop standing stem mass (kg/m^2)
!     bcmstandleaf - crop standing leaf mass (kg/m^2)
!     bcmstandstore - crop standing storage mass (kg/m^2)
!                    (head with seed, or vegetative head (cabbage, pineapple))
!     bcmflatstem  - crop flat stem mass (kg/m^2)
!     bcmflatleaf  - crop flat leaf mass (kg/m^2)
!     bcmflatstore - crop flat storage mass (kg/m^2)
!     bcmrootstorez - crop root storage mass by soil layer (kg/m^2)
!                   (tubers (potatoes, carrots), extended leaf (onion), seeds (peanuts))
!     lost_mass - biomass that decayed (disappeared) 
!     bcyld_coef - yield coefficient (kg/kg)     harvest_residue = bcyld_coef(kg/kg) * Yield + bcresid_int (kg/m^2)
!     bcresid_int - residue intercept (kg/m^2)   harvest_residue = bcyld_coef(kg/kg) * Yield + bcresid_int (kg/m^2)
!     bcgrf  - fraction of reproductive biomass that is yield

!     + + + COMMON BLOCKS + + +

!     + + + LOCAL VARIABLES + + +
      integer idx
      real ddm_res_yld, temp_tot, store_mass, ddm_adj

!     + + + LOCAL VARIABLE DEFINITIONS + + +
!     idx - array index used in loops
!     ddm_res_yld - increment in aboveground dry matter (kg/m^2)
!     temp_tot - temporary total biomass
!     store_mass - intermediate storage mass value
!     ddm_adj - adjusted increment in aboveground dry matter (kg/m^2)

!     + + + FUNCTIONS CALLED + + +

!     + + + SUBROUTINES CALLED + + +

!     + + + END OF SPECIFICATIONS + + +

      ! bchyfg = 0 - GRF times  reproductive mass
      ! bchyfg = 1 - GRF calculated in growth.FOR times reproductive mass (grain)
      ! bchyfg = 5 - GRF times below ground storage mass

      ! method based on yield residue relationship
      ! sum yield mass increments
      select case (bchyfg)
      case (0,1)
          ! 0 - GRF times  reproductive mass
          ! 1 - GRF calculated in growth.FOR times reproductive mass (grain)

          ! change in residue + yield biomass
          ! (new mass (abovegound + yield) - lost scenesced mass)
          ddm_res_yld = dlfwt + dstwt + drpwt - lost_mass
      case (5)
          ! 5 - GRF times below ground storage mass

          ! change in residue + yield biomass
          ! (new mass (abovegound + yield) - lost scenesced mass)
          ddm_res_yld = dlfwt + dstwt + drpwt + drswt - lost_mass
      case default
          ! no adjustment
          ! variable must be initialized
          ddm_res_yld = 0.0
      end select

      ! find yield storage mass increment based on yield residue relationship
      ! sum present yield + residue biomass
      temp_tot = 0.0
      if ( bchyfg .eq. 5) then
          ! 5 - GRF times below ground storage mass
          do idx = 1, bnslay
              temp_tot = temp_tot + bcmrootstorez(idx)
          end do
      end if
      ! add lost mass here to allow removing if mass was above threshold
      temp_tot = temp_tot + lost_mass                                   &
     &         + bcmstandstem + bcmstandleaf + bcmstandstore            &
     &         + bcmflatstem + bcmflatleaf + bcmflatstore
      if( temp_tot + ddm_res_yld .le. bcresid_int ) then
          store_mass = 0.0
      else if( temp_tot .le. bcresid_int ) then
          store_mass = (ddm_res_yld - (bcresid_int-temp_tot))           &
     &               / bcyld_coef / bcgrf
      else
          store_mass = ddm_res_yld / bcyld_coef / bcgrf
      end if
      select case (bchyfg)
      case (0,1)
          ! 0 - GRF times  reproductive mass
          ! 1 - GRF calculated in growth.FOR times reproductive mass (grain)

          ! (new mass (abovegound + yield) - lost scenesced mass)
          ddm_adj = dlfwt + dstwt + drpwt
          ! set reproductive mass increment
          drpwt = store_mass
          ! find remainder of mass increment
          ddm_adj = ddm_adj - drpwt
          ! distribute remainder of mass increment between stem and leaf
          ! leaf increment gets priority
          if( ddm_adj .gt. dlfwt ) then
              ! set stem increment
              dstwt = ddm_adj - dlfwt
          else
              ! not enough for both, leaf increment reduced
              dstwt = 0.0
              dlfwt = ddm_adj
          end if
      case (5)
          ! 5 - GRF times below ground storage mass

          ddm_adj = dlfwt + dstwt + drpwt + drswt
          ! set reproductive mass increment
          drswt = store_mass
          ! find remainder of mass increment
          ddm_adj = ddm_adj - drswt
          ! distribute remainder of mass increment between stem and leaf
          ! leaf increment, then reproductive gets priority
          if( ddm_adj .gt. dlfwt + drpwt ) then
              ! set stem increment
              dstwt = ddm_adj - dlfwt - drpwt
          else if( ddm_adj .gt. dlfwt ) then
              ! set stem increment
              dstwt = 0.0
              ! set reproductive increment
              drpwt = ddm_adj - dlfwt
          else
              ! not enough for both, leaf increment reduced
              dstwt = 0.0
              drpwt = 0.0
              dlfwt = ddm_adj
          end if
      case default
          ! no adjustment
      end select

      return
      end