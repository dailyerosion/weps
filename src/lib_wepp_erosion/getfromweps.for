!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine getfromweps(isr,canhgt,cancov,sand,silt,clay,orgmat,   &
     & rtm15,thetdr,rrc,dg,st,thdp,frdp,thetfc,por,rh,                  &
     & frctrl, frcsol,rtm, smrm, precip)
     
!-------------------------------------------------------------------------------------
!     getFromWeps()
!
!     This subroutine is called on a daily basis before entering the WEPP erosion code.
!     WEPP variables are updated based on the WEPS variables they are linked to.
!   
!     Try to use the 'a' variables from WEPS because these are updated daily.
!
!     Jim Frankenberger 
!     November 7, 2008
!--------------------------------------------------------------------------------------     

      implicit none

      include 'p1werm.inc'
	include 's1dbh.inc'
	include 'c1glob.inc'
	include 'b1glob.inc'
      include 's1dbc.inc'
      include 'd1glob.inc'
!	  include 'hydro/dvolwparam.inc'
      include 's1sgeo.inc'
      include 's1phys.inc'
      include 's1layr.inc'
      include 'p1unconv.inc'
      include 'hydro/htheta.inc'
!     include 's1sgeo.inc'
      include 'm1flag.inc'
      include 'h1temp.inc'
      include 'wepp_erosion.inc'
      include 'w1clig.inc'
	   

      integer, intent(in):: isr
	real, intent(out):: canhgt,cancov
      real, intent(out):: sand(mxnsl), silt(mxnsl), clay(mxnsl)
	real, intent(out):: orgmat(mxnsl)
	real, intent(out):: thetdr(mxnsl), rrc, rtm15
	real, intent(out):: dg(mxnsl), st(mxnsl), thdp, frdp
	real, intent(out):: thetfc(mxnsl), por(mxnsl), rh
	real, intent(out):: frctrl, frcsol
	real, intent(out):: rtm(3), smrm(3), precip
	
!     + + + argument declarations + + +     
!     isr - This variable holds the subregion index.
!     canhgt - canopy height (m)
!     cancov - canopy cover fraction (crop)
!     sand() - sand fraction by layer
!     silt() - silt fraction by layer
!     clay() - clay fraction by layer
!     orgmat() - organic fraction by layer
!     rtm15 - root mass at 15 cm
!     thetdr() - 15-bar soil water content (wilting point)
!     rrc - random roughness coefficient
!     dg() - depth of each soil layer, meters
!     st() - current available water content per soil layer (m)
!     thdp - Thaw depth of the frozen-layer system (m).
!     frdp - Depth of the frost layer (m).
!     thetfc() - 1/3-bar soil water content (field capacity)
!     por() - porosity for each soil layer
!     rh - ridge height (m)
!     frctrl - total rill friction factor
!     frcsol - soil grain friction factor
!     rtm() - non living root mass (kg/m^2)
!     smrm() - submerged residue mass today (kg/m^2)
!     precip - daily precipitation amount (m)
!

      real bd(mxnsl), cec(mxnsl), cecc, solcon(mxnsl), coca(mxnsl)
	real oca, bottom, depth(100), lay_val(100)
      integer i,isFroze
      
      real valbydepth

!     canopy height in meters
      canhgt = aczht(isr)
      
!     canopy cover fraction (crop)       
	cancov = acfcancov(isr)
 
!     root mass at 15cm, interpolate root mass at depths use 2 age groups from weps
      do i=1, nslay(isr)
         depth(i) = aszlyt(i,isr)
         lay_val(i) = admrtz(i,1,isr)
         lay_val(i) = lay_val(i) + admrtz(i,2,isr)
      end do
      
	rtm15 = valbydepth(nslay(isr), depth, lay_val, 0, 150.0, 150.0)
!
!     random roughness in WEPS is mm, WEPP is m	
      rrc = aslrr(isr) / 1000.0

!     ridge height in WEPS is mm, WEPP is m
      rh = aszrgh(isr) / 1000.0
	 
!     soil grain friction factor - TODO	 
      frcsol = 1.11
      
!     daily precipitation (mm)      
      precip =  awzdpt

!     total rill friction factor - TODO
      frctrl = 1

!     True if surface has tillage has occurred
      if (am0til .eqv. .true.) then
	   wp_daydis = 0
      else
        wp_daydis = wp_daydis + 1
      end if

	frdp = 0.0
	thdp = 0.0
	bottom = 0.0
	isFroze = 0

!     May not be right, admrt is the buried residue root mass
!     and rtm is dead root mass from wepp with 3 ages: present
!     previous and total but weps only has two age groups
!     admbg - Buried residue mass (kg/m^2) Excludes root mass below the surface
!     smrm - submerged residue mass today
      do i=1, 3
       rtm(i) = admrt(i,isr)
	 smrm(i) = admbg(i,isr)
      end do

!
!     Don't really need to get the sand,silt,clay,orgmat, cec, these would
!     be constant during the simulation. 
!
      do i=1, nslay(isr)
!       wilting point (15 bar) volumetric water content      
        thetdr(i) = thetaw(i)
!       Soil layer sand content (Mg/Mg)
        sand(i) = asfsan(i,isr)
!       Soil layer silt content (Mg/Mg)        
		silt(i) = asfsil(i,isr)
!       Soil layer clay content (Mg/Mg)		
		clay(i) = asfcla(i,isr)
!       Soil layer organic matter content (Mg/Mg)		
		orgmat(i) = asfom(i,isr)
!		asfcec - Soil layer cation exchange capacity (cmol/kg) (meq/100g)
		cec(i) = asfcec(i,isr)

!       Soil layer bulk density for each subregion (Mg/m^3)
        bd(i) = 1000.0 * asdblk(i,isr)

!       Soil layer thicknesses for each subregion (mm)        
	  dg(i) = aszlyt(i,isr) * mmtom

!       theta() - soil layer water content (m^3/m^3)	  
!       thetaw() - wilting point (15 bar) volumetric water content 
!       dg() - Soil layer thicknesses for each subregion (mm)  
		st(i) = (theta(i) - thetaw(i)) * dg(i)
		thetfc(i) = thetaf(i)

		bottom = bottom + dg(i)

        cecc = cec(i) - orgmat(i) * (142.+170.* dg(i))

		if (clay(i).le.0.0) then
          solcon(i) = 0.0
        else
          solcon(i) = cecc / (100.*clay(i))
        end if

        if (solcon(i).lt.0.15) solcon(i) = 0.15
        if (solcon(i).gt.0.65) solcon(i) = 0.65

        oca = 3.80 + 1.9 * (clay(i)**2) - (3.365*sand(i))               &
     &      + (12.6*solcon(i)*clay(i)) + (100.*                         &
     &      orgmat(i)*((sand(i)/2.)**2))

        coca(i) = (1-oca/100.0)
		por(i) = (2650.-bd(i)) / 2650.
		por(i) = por(i) * coca(i)

!
!       simplified: if more than 50% of the water in a layer is 
!       frozen consider the soil frozen to that depth. When the
!       soil thaws move the thaw depth down until a frozen layer
!       is encountered.

!       ahfice - fraction of soil water in layer which is frozen  
        if (ahfice(i,isr).ge.0.5) then
		    frdp = bottom
			isFroze = 1
		end if

        if ((isFroze.eq.0).and.(ahfice(i,isr).lt.0.5)) then
		    thdp = bottom
		end if

		
	  end do

	  if ((isFroze.eq.1).and.(thdp.gt.0.0001)) then
	      wp_cycle = wp_cycle + 1
	  end if

	  if (isFroze.eq.0) then
	     wp_froday = wp_froday + 1
         end if

	  if (wp_froday.gt.50) then
	     wp_cycle = 0
	  end if

      return
      end
