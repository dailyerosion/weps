!$Author$
!$Date$
!$Revision$
!$HeadURL$

      real function satvappres( airtemp )

      ! returns the saturated vapor pressure for water (kPa)
      ! Approximation from Jensen ASCE manual 70 evapotranspiration
      ! valid in normal climatic condition range

      real airtemp
      ! airtemp - the temperature of the air (C)

      real c1, c2, c3
      ! c1, c2, c3 -  coefficients for saturated equation
      parameter (c1 = 0.611, c2 = 17.27, c3 = 237.3)

      satvappres = c1 * exp( c2 * airtemp/( airtemp + c3 ) )

      return
      end
      
