!$Author$
!$Date$
!$Revision$
!$HeadURL$

      real function vaporden( airtemp, relhum )

!     returns the water vapor density in air (kg/m^3)
!     calculated directly from PV = nRT

!*** Argument declarations ***
      real airtemp, relhum
!     airtemp - the temperature of the air (C)
!     relhum - relative humidity of the air (fraction)

!*** function declarations ***
      real satvappres
!     satvappres - function to find the saturated vapor pressure (Pascals)

!*** Local declarations ***
      real actvappres
!     actvappres - actual vapor pressure (kPa)

      actvappres =  relhum * satvappres(airtemp)

!      if(actvappres.lt.1.0e-34) write(*,*) 'vaporden:',relhum,airtemp
      vaporden = 2.166 * actvappres / (airtemp + 273.15)

      return
      end
      
