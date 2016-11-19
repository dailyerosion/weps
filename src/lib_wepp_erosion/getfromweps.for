!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine getfromweps(isr,sand,silt,clay,orgmat,                 &
     & thetdr,rrc,dg,st,thdp,frdp,thetfc,por,rh,                        &
     & frctrl, frcsol, precip, soil)
     
!-------------------------------------------------------------------------------------
!     getfromweps()
!
!     This subroutine is called on a daily basis before entering the WEPP erosion code.
!     WEPP variables are updated based on the WEPS variables they are linked to.
!   
!     Try to use the 'a' variables from WEPS because these are updated daily.
!
!     Jim Frankenberger 
!     November 7, 2008
!--------------------------------------------------------------------------------------     

      use p1unconv_mod, only: mmtom
      use climate_input_mod, only: cli_today
      use soil_data_struct_defs, only: soil_def

      implicit none

      include 'p1werm.inc'
      include 'hydro/htheta.inc'
      include 'm1flag.inc'
      include 'h1temp.inc'
      include 'wepp_erosion.inc'

      integer, intent(in):: isr
      real, intent(out):: sand(mxnsl), silt(mxnsl), clay(mxnsl)
      real, intent(out):: orgmat(mxnsl)
      real, intent(out):: thetdr(mxnsl), rrc
      real, intent(out):: dg(mxnsl), st(mxnsl), thdp, frdp
      real, intent(out):: thetfc(mxnsl), por(mxnsl), rh
      real, intent(out):: frctrl, frcsol
      real, intent(out):: precip
      type(soil_def), intent(in) :: soil  ! soil for this subregion
      
!     + + + argument declarations + + +     
!     isr - This variable holds the subregion index.
!     sand() - sand fraction by layer
!     silt() - silt fraction by layer
!     clay() - clay fraction by layer
!     orgmat() - organic fraction by layer
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
!     precip - daily precipitation amount (m)
!

      real bd(mxnsl), cec(mxnsl), cecc, solcon(mxnsl), coca(mxnsl)
      real oca, bottom
      integer i,isFroze
      
!     random roughness in WEPS is mm, WEPP is m      
      rrc = soil%aslrr / 1000.0

!     ridge height in WEPS is mm, WEPP is m
      rh = soil%aszrgh / 1000.0
       
!     soil grain friction factor - TODO       
      frcsol = 1.11
      
!     daily precipitation (mm)      
      precip =  cli_today%zdpt

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

!     Don't really need to get the sand,silt,clay,orgmat, cec, these would
!     be constant during the simulation. 
!
      do i=1, soil%nslay
!       wilting point (15 bar) volumetric water content      
        thetdr(i) = thetaw(i)
!       Soil layer sand content (Mg/Mg)
        sand(i) = soil%asfsan(i)
!       Soil layer silt content (Mg/Mg)        
        silt(i) = soil%asfsil(i)
!       Soil layer clay content (Mg/Mg)            
        clay(i) = soil%asfcla(i)
!       Soil layer organic matter content (Mg/Mg)
        orgmat(i) = soil%asfom(i)
!       asfcec - Soil layer cation exchange capacity (cmol/kg) (meq/100g)
        cec(i) = soil%asfcec(i)

!       Soil layer bulk density for each subregion (Mg/m^3)
        bd(i) = 1000.0 * soil%asdblk(i)

!       Soil layer thicknesses for each subregion (mm)        
        dg(i) = soil%aszlyt(i) * mmtom

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
