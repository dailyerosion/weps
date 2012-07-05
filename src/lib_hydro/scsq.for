!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!

!     file name: scsq.for

      real function   scsq (rain,cniip,cniig,canp,slp,theta1,thetf1)

!     + + + PURPOSE + + +
!     This function estimates daily surface runoff by the united
!     states department of agriculture-soil conservation service (scs)
!     soil-cover method, which is commonly known as the curve number
!     method.
!     The return value is the estimated daily surface runoff (mm)
!     DATE:  09/22/93

!     + + + ARGUMENT DECLARATIONS + + +
      real rain
      real cniip
      real cniig
      real canp
      real slp
      real theta1
      real thetf1

!     + + + ARGUMENT DEFINITIONS + + +
!     rain   - Daily rainfall, snow melt, and/or irrigation (mm)
!     cniip  - Scs curve # for class ii antecedent soil moisture
!              conditions, under poor hydrologic conditions
!     cniig  - Scs curve # for class ii antecedent soil moisture
!              conditions, under good hydrologic conditions
!     canp   - Daily crop canopy (decimal percent)
!     slp    - Average slope of the simulation region (m/m)
!     theta1 - Soil wc for uppermost simulation layer (m^3/m^3)
!     thetf1 - Soil wc at fc for uppermost sim layer (m^3/m^3)

!     + + + LOCAL VARIABLES + + +
      real cn
      real cnis
      real cnii
      real cniis
      real cniii
      real cniiis
      real cndif
      real s
      real rma

!     + + + LOCAL DEFINITIONS + + +
!     cn     - Calculated scs curve number
!     cnis   - Slope-adjusted scs curve # for class i antecedent
!              soil moisture conditions
!     cnii   - Scs curve # for class ii antecedent soil moisture
!              conditions (average soil moisture conditions)
!     cniis  - Slope-adjusted scs curve # for class ii antecedent
!              soil moisture conditions
!     cniii  - Scs curve # for class iii antecedent soil moisture
!              conditions (wet soil moisture conditions)
!     cniiis - Slope-adjusted scs curve # for class iii antecedent
!              soil moisture conditions
!     cndif  - Difference between condition ii scs curve #s for
!              poor and good hydrologic conditions
!     s      - Retention parameter (mm)
!     rma    - Rainfall, snow melt, &/or irrigation minus
!              initial abstraction (mm)

!     + + + END SPECIFICATIONS + + +

!     prorate the calculated condition ii scs curve number according
!     to the daily estimate of crop canopy
! ***      write(*,*) ' cniip, cniig ', cniip, cniig
      cndif = cniip - cniig
      cnii = cniip - ( cndif * canp )

!     adjust the calculated condition ii scs curve number according
!     to the average slope of the simulation region
      cniii = 6.9386 + 1.6425*cnii - 0.0071*cnii*cnii
! ***      write(*,*) ' cniii, cnii, slp ', cniii, cnii, slp 
      cniis = 0.3333*(cniii-cnii)*(1-2*exp(-13.86*slp))+cnii

!     readjust the calculated scs curve number to the correct
!     class of antecedent soil moisture conditions on the basis of
!     the current status of surface soil moisture
! ***      write(*,*) ' cniss ', cniis
      if ( theta1 .gt. thetf1 ) then
          cniiis = 6.9386 + 1.6425*cniis - 0.0071*cniis*cniis
          cn = cniiis
      else
          if ( theta1 .lt. (thetf1*0.6) ) then
              cnis = 0.4678*(1.0113**cniis)*(cniis**0.9191)
              cn = cnis
          else
              cn = cniis
          endif
      endif

!     determine the retention parameter (s), and surface runoff (q).
      s = 254 * ( 100/cn - 1 )
      rma =  rain - 0.2*s
      if ( rma .gt. 0.0 ) then
          scsq = rma*rma/(rain + 0.8*s)
      else
          scsq = 0.0
      endif

      return
      end
