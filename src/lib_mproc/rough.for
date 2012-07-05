!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!
!
!
      subroutine rough                                                  &
     &              (roughflg, rrimpl,till_i,tillf,                     &
     &               rr, tillay, clayf, siltf,                          &
     &               rootmass, resmass,                                 &
     &               ldepth ) 

!     + + + PURPOSE + + +
!     
!     This subroutine performs a random roughness calculation 
!     after a tillage operation.
!
!     + + + KEYWORDS + + +
!     random roughness (RR), tillage (primary/secondary)
!
!     + + + ARGUMENT DECLARATIONS + + +
      include 'p1werm.inc'
!
      integer roughflg
	  real    tillf,rrimpl,rr,till_i
      integer tillay
      real    clayf(mnsz), siltf(mnsz)
      real    rootmass(mnsz), resmass(mnsz)
      real    ldepth(mnsz)
!
!     + + + ARGUMENT DEFINITIONS + + +
!
!     tillf	    - Fraction of the surface tilled (0-1)
!	  till_i	- Tillage intensity factor (0-1)
!     rrimpl	- Assigned nominal RR value for the tillage operation (mm) 
!     rr		- current surface random roughness (mm) 
!     tillay    - number of layers affected by tillage
!     clayf     - clay fraction of soil
!     siltf     - silt fraction of soil
!     rootmass  - mass of roots by layer, pools (kg/m^2)
!     resmass   - mass of buried crop residue by layer, pools (kg/m^2)
!     ldepth    - depth from soil surface of lower layer boundaries
!
!     + + + ACCESSED COMMON BLOCK VARIABLE DEFINITIONS + + +
!     
!
!     + + + PARAMETERS + + +
      real rrmin
      parameter ( rrmin = 6.096 ) !(mm) = 0.24 inches
!
!     + + + LOCAL VARIABLES + + +
      integer laycnt
      real    rradj, soiladj
      real    biomass
!
!     + + + LOCAL VARIABLE DEFINITIONS + + +
!     laycnt   - counter for layers
!     rradj    - adjusted implement random roughness
!     soiladj  - soil texture adjustment multiplier
!     biomass  - total biomass in the tillage zone
!
!     + + + END SPECIFICATIONS + + + 
!
!	  Perform the calculation of the surface RR after a tillage
!     operation.  Check to see if the tillage intensity factor is
!     needed before performing the calculation. 
!
!     adjust the input random roughness value based on flag
!     roughflg.eq.0 does nothing
      rradj = rrimpl
      if( (roughflg.eq.1).or.(roughflg.eq.2)) then
!         adjust for soil type
          soiladj = 0.16*siltf(1)**0.25+1.47*clayf(1)**0.27
          soiladj = max(0.6,soiladj)
          rradj = rradj * soiladj
      endif

      if( (roughflg.eq.1).or.(roughflg.eq.3)) then
!         adjust for buried residue amounts, handbook 703, eq 5-17
!         this equation is originally in lbs/ac/in
!         rradj = rrmin+(rradj-rrmin)*(0.8*(1-exp(-0.0012*biomass))+0.2)
!         This was modified in Wagners correspondence with Foster to use
!         the factor exp(-0.0015*biomass)
!         lbs/ac/in = 226692 * kg/m^2/mm
!         sum up total biomass in the tillage depth
          if( rrimpl.gt.rrmin ) then
              biomass = 0.0
              do 100 laycnt=1,tillay
                  biomass = biomass + rootmass(laycnt)
                  biomass = biomass + resmass(laycnt)
 100          continue
!             make it kg/m^2/mm
              biomass = biomass / ldepth(tillay)          
!             if value is below min, don't adjust since it would
!             increase it with less residue. 
              if(rradj.gt.rrmin) then
!                 this equation uses biomass in kg/m^2/mm
                  rradj = rrmin + (rradj-rrmin)                         &
     &                  *(0.8*(1-exp(-339.92*biomass))+0.2)
              endif
          endif
      endif

      ! Is RR going to be increased?  If so, then just do it.
      if (rradj .ge. rr) then 
	      rr = tillf*rradj + (1.0-tillf)*rr
      else
          rr = tillf*(till_i*rradj + (1.0-till_i)*rr)                   &
     &       + (1.0-tillf)*rr
      end if

	  return
	  end
