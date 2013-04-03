!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine shootnum( shoot_flg, bnslay, bc0idc, bcdpop, bc0shoot, &
     &           bcdmaxshoot, bcmtotshoot, bcmrootstorez, bcdstm )

!     + + + PURPOSE + + +
!     determine the number of shoots that root storage mass can support,
!     and set the total mass to be released from root storage.

!     + + + KEYWORDS + + +
!     stem number, shoot growth

      use p1unconv_mod, only: mgtokg

!     + + + ARGUMENT DECLARATIONS + + +
      integer shoot_flg, bnslay, bc0idc
      real bcdpop, bc0shoot, bcdmaxshoot
      real bcmtotshoot
      real bcmrootstorez(*)
      real bcdstm

!     + + + ARGUMENT DEFINITIONS + + +
!     shoot_flg - used to control the behavior of the shootnum subroutine
!             0 - returns the shoot number constrained by bcdmaxshoot
!             1 - returns the shoot number unconstrained by bcdmaxshoot
!     bnslay - number of soil layers
!     bc0idc - crop type:annual,perennial,etc
!     bcdpop - Number of plants per unit area (#/m^2)
!            - Note: bcdstm/bcdpop gives number of stems per plant
!     bc0shoot - mass from root storage required for each shoot (mg/shoot)
!     bcdmaxshoot - maximum number of shoots possible from each plant
!     bcmtotshoot - total mass of shoot growing from root storage biomass (kg/m^2)
!                   in the period from beginning to completion of emegence heat units
!     bcmrootstorez - crop root storage mass by soil layer (kg/m^2)
!                   (tubers (potatoes, carrots), extended leaf (onion), seeds (peanuts))
!     bcdstm - Number of crop stems per unit area (#/m^2)

!     + + + LOCAL VARIABLES + + +
      integer lay
      real root_store_sum

!     + + + LOCAL VARIABLE DEFINITIONS + + +
!     lay - layer index for summing root storage
!     root_store_sum - sum of root storage

!     + + + PARAMETERS + + +
      real per_release
      PARAMETER (per_release = 0.9)
      real stage_release
      PARAMETER (stage_release = 0.5)

!     + + + PARAMETER DEFINITIONS + + +
!     per_release - fraction of available root stoage mass released to
!                   grow new shoots. Default is set to 90% of available

      ! Find number of shoots (stems) that can be supported from
      ! root storage mass up to the maximum
      root_store_sum = 0.0
      do lay = 1,bnslay
          root_store_sum = root_store_sum + bcmrootstorez(lay)
      end do

      ! determine number of regrowth shoots
      ! units are kg/m^2 / kg/shoot = shoots/m^2
      if( (bc0idc.eq.3) .or. (bc0idc.eq.6) ) then
          ! Perennials hold some mass in reserve
          bcdstm = max( bcdpop,                                         &
     &             per_release * root_store_sum/(bc0shoot*mgtokg)  )
      else if( bc0idc.eq.8 ) then
          ! This Perennial stages it's bud release, putting out less after each cutting
          bcdstm = max( bcdpop,                                         &
     &             stage_release * root_store_sum/(bc0shoot*mgtokg) )
      else
          ! all others go for broke
          bcdstm = max( bcdpop,                                         &
     &             root_store_sum/(bc0shoot*mgtokg) )
      end if

      if( shoot_flg .eq. 0 ) then
          ! respect maximum limit
          bcdstm =  min( bcdmaxshoot*bcdpop, bcdstm )
      end if

!      write(*,*) 'shootnum:bcdstm: ', bcdstm
      ! set the mass of root storage that is released (for use in shoot grow)
      ! units are shoots/m^2 * kg/shoot = kg/m^2
      bcmtotshoot = min( root_store_sum, bcdstm * bc0shoot * mgtokg )

      return
      end
