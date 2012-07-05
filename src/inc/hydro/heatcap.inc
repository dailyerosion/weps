!$Author$
!$Date$
!$Revision$
!$HeadURL$

      real sandheatcap, siltheatcap, clayheatcap
      real waterheatcap, iceheatcap, organheatcap

!     sandheatcap - heat capacity of sand (J/kg C)
!     siltheatcap - heat capacity of silt (J/kg C)
!     clayheatcap - heat capacity of clay (J/kg C)
!     waterheatcap - heat capacity of water (J/kg C)
!     iceheatcap - heat capacity of ice (J/kg C)
!     organheatcap - heat capacity of organic matter (J/kg C)

      ! values from Kluitenberg
      parameter( sandheatcap = 730.0 )
      parameter( siltheatcap = 730.0 )
      parameter( clayheatcap = 730.0 )
      parameter( waterheatcap = 4180.0 )

      ! value from deVries and from Incropera and DeWit
      parameter( iceheatcap = 2040.0 )

      ! values from Kluitenberg
      parameter( organheatcap = 1900.0 )
