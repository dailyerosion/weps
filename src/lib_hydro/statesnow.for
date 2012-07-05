!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine statesnow( dh2o, new_mass, new_energy, new_depth,      &
     &                      bhzsno, bhtsno, bhfsnfrz, bhzsnd )

!     + + + PURPOSE + + +
!     Using inputs of present snow state, new added mass, energy, and
!     snow depth, determines the new snow state and any water drainage.

!     + + + COMMON BLOCKS + + +

!     + + + LOCAL COMMON BLOCKS + + +
      include 'hydro/snowprop.inc'
      include 'hydro/heatcap.inc'

!     + + + ARGUMENT DECLARATIONS + + +
      real dh2o, new_mass, new_energy, new_depth
      real bhzsno, bhtsno, bhfsnfrz, bhzsnd

!     + + + ARGUMENT DEFINITIONS + + +
!     dh2o - depth of water reaching soil surface through snow (mm)
!     new_energy - energy content of new snow (J/m^2)
!     new_mass - mass of new snow (kg/m^2)
!     new_depth - depth associated with new snow (mm) (indirect density)
!     bhzsno  - depth of water contained in snow layer (mm)
!     bhtsno  - temperature of snow layer (C)
!     bhfsnfrz  - fraction of snow layer water content which is frozen
!     bhzsnd  - actual thickness of snow layer (mm)

!     + + + LOCAL VARIABLES + + +
      real old_energy, old_mass, tot_energy, tot_mass
      real fz_energy, new_fsnfrz

!     + + + LOCAL DEFINITIONS + + +
!     old_energy - energy content of existing snow layer (J/m^2)
!     old_mass - mass of existing snow layer (kg/m^2)
!     tot_energy - sum of old and new energy
!     tot_mass - sum of old and new mass
!     fz_energy - energy of snow at zero degrees all frozen (J/m^2)
!     new_fsnfrz - new fraction of snow layer water content which is frozen

!     + + + SUBROUTINES CALLED + + +
!     drainsnow

!     + + + DATA INITIALIZATIONS + + +

!     + + + END SPECIFICATIONS + + +

      ! set mass of existing snow
      ! units: mm * (1m/1000mm) * 1000 kg/m^3 = kg/m^2
      old_mass = bhzsno

      ! calculate energy content of old snow
      ! units: kg/m^2 * ( C * J/(kg C) - J/kg ) = J/m^2
      old_energy = old_mass * bhfsnfrz                                  &
     &           * (bhtsno * iceheatcap - heat_fusion)                  &
     &           + old_mass * (1.0 - bhfsnfrz) * bhtsno * waterheatcap

      ! sum mass and energy
      tot_mass = old_mass + new_mass
      tot_energy = old_energy + new_energy

      ! find energy of full frozen zero degree snow
      fz_energy = (- heat_fusion * tot_mass)

      ! select based on break point energy
      if( tot_energy .le. fz_energy ) then
          ! all snow is frozen, find temperature
          bhfsnfrz = 1.0
          bhtsno = (tot_energy/tot_mass + heat_fusion) / iceheatcap

          ! set snow depth
          bhzsnd = bhzsnd + new_depth
          bhzsno = tot_mass

          ! zero out liquid water content
          dh2o = 0.0
      else if( tot_energy .lt. 0.0 ) then
          ! mixture of snow and water
          ! temperature at freezing
          bhtsno = 0.0

          ! find new frozen water content fraction
          new_fsnfrz = - tot_energy/(tot_mass*heat_fusion)

          ! adjust snow depth based on change in frozen water content fraction
          bhzsnd = bhzsnd * new_fsnfrz * tot_mass / (bhfsnfrz*old_mass)
          bhfsnfrz = new_fsnfrz

          ! check for density and drain water if above the drainage density
          call drainsnow( dh2o, tot_mass, bhfsnfrz, bhzsnd )
          bhzsno = tot_mass
      else
          ! all liquid water
          ! add all snow water content to liquid water
          ! remember kg/m^2 = mm of water using standard density
          dh2o = tot_mass

          ! zero out snow
          bhtsno = 0.0
          bhfsnfrz = 0.0
          bhzsnd = 0.0
          bhzsno = 0.0
      end if

      return
      end
