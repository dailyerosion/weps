!$Author$
!$Date$
!$Revision$
!$HeadURL$

      real quartzheatcond, mineralheatcond
      real iceheatcond, organicheatcond

!     sandheatcond - thermal conductivity of sand (J/s m C) or (W/m C)
!     siltheatcond - thermal conductivity of silt (J/s m C) or (W/m C)
!     clayheatcond - thermal conductivity of clay (J/s m C) or (W/m C)
!     iceheatcond - thermal conductivity of ice (J/s m C) or (W/m C)
!     organicheatcond - thermal conductivity of organic matter (J/s m C) or (W/m C)

      ! values from Bristow in Dane and Topp (2002)
      parameter( quartzheatcond = 8.8)
      parameter( mineralheatcond = 2.9)
      parameter( iceheatcond = 2.18)
      parameter( organicheatcond = 0.25)
