!$Author$
!$Date$
!$Revision$
!$HeadURL$
      subroutine asd( cslagm, cslmin, cslmax, chtsmx, chtmx0, cs0ags,           &
     &  cslagx, se0, se1)

!asd = aggregate size distribution
!this subroutine calculates:
!aggregate geometric mean diameter (cslagm)
!aggregate geometric standard deviation (cs0ags)
!max. aggregate diameter (cslagx)

!     + + + ARGUMENT DECLARATIONS + + +
      real cslagm, cslmin
      real cslmax, chtsmx, chtmx0, cs0ags
      real cslagx, se0, se1

!     + + + LOCAL VARIABLES + + +
      real c4p, c4f
      real gmd1, gmd0, gmd_avg0, gmd_avg1
      real slp0, slp_avg, slp
!     + + + ARGUMENT DEFINITIONS + + +
!     cs0ags    - aggregate geometric standard deviation
!     cslagm    - aggregate geometric mean diameter
!     cslmin    - min value of aggregate gmd
!     cslmax    - max value of aggregate gmd
!     chtsmx    - max temperature (C) of layer for the day
!     cslagx    - max value of aggregate size (mm)
!     se0       - relative agg stability at WP prior to SOIL update
!     se1       - relative agg stability at WP after SOIL update

!     + + + LOCAL VARIABLE DEFINITIONS + + +
!     c4p    -  scale coefficent in weibull gmd distribution
!     c4f    -  intercept coeffient in weibull gmd distribution
!     gmd0 - dimensionless geometric mean agg. diameter,prior day
!     gmd1 - dimensionless geometric mean agg. diameter, today
!     gmd_avg0 - dimesionless average gmd at se0 on prior day
!     gmd_avg1 - dimensionless avrage gmd at se1 today 

!     + + + END SPECIFICATIONS + + +
!         determine gmd_avg increase by root fibers 
!         by changing cofficients c4p, and c4f
!          fr_tot = fiber_roots + fiber_roots_dead ??????
!         if(fr_tot .lt. mass1/z1) then

!          elseif ( 
    
!         elseif (

!         else

!          end
!### tmp
!	write (*,*) 'start asd'
 !       write (*,*) 'se0=',se0, 'se1=',se1
       if (se0 .gt. 1.0 .and. se1 .gt. 1.0 ) then         
!	    gmd1 = gmd0         !no change or  all frozen
            go to 100
       endif
!         temp coef. values
          c4p = 0.6
          c4f = 0.0

!     calculate geometric mean diameter using prior geometric mean diameter
      if (chtmx0 .gt. 0.0) then
      cslagm = max (cslmin, cslagm)   !error trap
      endif
      gmd0 = (cslagm - cslmin)/(cslmax - cslmin)    !dimensionless
!
     
      if ((se0 .lt. 1.0) .and. (se1 .gt. 1.0)) then   !freeze
          gmd1 = gmd0 + se1
      elseif ((se0 .gt. 1.0) .and. (se1 .le. 1.0)) then   !thaw
!         se1 may be puddled, all freeze dried or between these states
          gmd1 = 1 - exp(-(se1/c4p)**2)
!
!        no freeze; calculate gmd1
      elseif ((se0 .eq. 1.0).and.(se1 .eq. 1.0) ) then 
          gmd_avg1 = (1 - exp(-(se1/c4p)**2))*(1-c4f) + c4f
          gmd1 = (gmd_avg1 + gmd0)/2.0

      elseif (se0 .eq. se1) then
           gmd_avg1 = (1 - exp(-(se1/c4p)**2))*(1-c4f) + c4f
           gmd1 = gmd_avg1*0.2 + gmd0*0.8
      else                              
          gmd_avg0 = (1 - exp(-(se0/c4p)**2))*(1-c4f) + c4f
          gmd_avg1 = (1 - exp(-(se1/c4p)**2))*(1-c4f) + c4f

          slp0 = (gmd_avg1 - gmd0)/(se1 - se0)
          slp_avg = (gmd_avg1 - gmd_avg0)/(se1 - se0)
          slp = (slp0 + slp_avg)/2.0
          gmd1 = gmd0 + slp*(se1 - se0)
      endif
      
        cslagm = (cslmax - cslmin) * gmd1 + cslmin  !dimensioned gmd      
 
!     restrict upper size if not frozen
      if ((chtsmx .ge. 0.0) .and. (cslagm .gt. cslmax)) then
          cslagm = cslmax
      endif

      ! restrict lower size unconditionally
      cslagm = max(cslmin, cslagm)

!     calculate geometric standard deviation (eq. S-??)
!     this equation is asmytotic to zero at zero and +infinity
!     Based on the definition of Geometric Standard Deviation this
!     should be asmototic to 1
!      cs0ags = 1.0 / (0.0203 + 0.00193  *cslagm +
!     &             0.074 / sqrt(cslagm))
!     this replacement equation is asmytotic to 1 and is very close
!     to the original where the gsd was greater than 1
      cs0ags = 1.0 + 1.0                                                &
     &       / (0.012448 + 0.002463*cslagm + 0.093467/sqrt(cslagm))

!     calculate max. aggregate diameter (cslagx)
      c4p = 1.52 * cslagm**(-0.449)
      cslagx = (cs0ags**c4p) * cslagm

  100 return
      end