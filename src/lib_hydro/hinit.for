!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine hinit(layrsn, bsdblk, bsdblk0, bsdpart, bsdwblk,       &
     &                 bhrwc, bhrwcs, bhrwcf, bhrwcw, bhrwcr,           &
     &                 bhrwca, bh0cb, bheaep, bhrsk, bhfredsat,         &
     &                 bsfsan, bsfsil, bsfcla, bsfom, bsfcec,           &
     &                 bszlyd, bszlyt, vaptrans, evaplimit)

!     + + + PURPOSE + + +
!     This subroutine controls the initialization of the HYDROLOGY
!     sumbodel of WEPS.  The program initializes the depth variables
!     of the soil simulation layers and converts the soil water
!     content variables from mass basis to volume basis.
!     DATE:  09/16/93
!     MODIFIED:  12/13/93
!     MODIFIED:  07/28/95
!     MODIFIED:  07/29/95
!     This change was done to determine the average of soil
!     properties from the first simulation layer (10 mm) and the second
!     uppermost simulation layer (40 mm).  The average for the new
!     uppermost simulation layer for the HYDROLOGY submodel (50 mm thick)
!     Using 50 mm as the thickness of the uppermost simulation layer for the
!     HYDROLOGY submodel will increase the speed of simulation and reduce the
!     the potential for errors.
!
!
!     + + + KEYWORDS + + +
!     initialization, hydrology

!     + + + ARGUMENT DECLARATIONS + + +
      integer layrsn
      real bsdblk(*), bsdblk0(*), bsdpart(*), bsdwblk(*)
      real bhrwc(*), bhrwcs(*), bhrwcf(*), bhrwcw(*), bhrwcr(*)
      real bhrwca(*), bh0cb(*), bheaep(*), bhrsk(*), bhfredsat(*)
      real bsfsan(*), bsfsil(*), bsfcla(*), bsfom(*), bsfcec(*)
      real bszlyd(*), bszlyt(*), vaptrans, evaplimit

!     + + + ARGUMENT DEFINITIONS + + +
!     layrsn - Number of soil layers used in simulation
!     bsdblk  - Soil bulk density (Mg/m^3)
!     bsdblk0 - Previous day soil bulk density (Mg/m^3)
!     bsdpart - Soil particle density (Mg/m^3)
!     bsdwblk - Soil wet bulk density (Mg/m^3)
!     bhrwc   - Soil water content (mg/mg)
!     bhrwcs  - Soil water content at saturation (mg/mg)
!     bhrwcf  - Soil water content at field capacity (mg/mg)
!     bhrwcw  - Soil water content at wilting point (mg/mg)
!     bhrwcr  - Residual Soil water content (mg/mg)
!     bh0cb   - Power of campbell's water release curve model (unitless)
!     bheaep  - Soil air entry potential (j/kg)
!     bhrsk   - Saturated hydraulic conductivity (m/s)
!     bhfredsat - fraction of soil porosity that will be filled with water
!                 while wetting under normal field conditions due to entrapped air
!     bsfsan  - Sand fractions
!     bsfsil  - Silt fractions
!     bsfcla  - Clay fractions
!     bsfom   - fraction of total soil mass which is organic matter
!     bsfcec  - Soil layer cation exchange capacity (cmol/kg) (meq/100g)
!     bszlyd  - depth to the bottom of soil layer (mm)
!     bszlyt  - soil layer thickness (mm)
!     vaptrans - vapor transmissivity (mm/d^.5)
!     evaplimit - accumulated surface evaporation since last complete rewetting
!                 defining limit of stage 1 (energy limited) and start of 
!                 stage 2 (soil vapor transmissivity limited) evaporation (mm)

!     + + + COMMON BLOCKS + + +
      include 'p1werm.inc'
      include 'p1unconv.inc'
      include 'command.inc'          !declarations for commandline args

!     + + + LOCAL COMMON BLOCKS + + +
      include 'hydro/htheta.inc'
      include 'hydro/hpsd.inc'
      include 'hydro/clayomprop.inc'

!     + + + LOCAL VARIABLES + + +
      integer k
      real potes(mnsz), temp, temp1, temp2, temp3

!     + + + LOCAL DEFINITION + + +
!     potes  - Air entry potential at a std. bsdblk of 1.3 Mg/m^3

!     + + + SUBROUTINES CALLED + + +
!     psd
!     extra

!     + + + FUNCTION DECLARATIONS + + +
      real waterk
!      real calctht0
      real volwatadsorb
      real volwat_matpot_bc

!     + + + END SPECIFICATIONS + + +
!     initialize various soil layer references

      if( wc_type.eq.4 ) then
          ! use texture based calculations from Rawls to set all soil
          ! water properties.
          call param_prop_bc(                                           &
     &        layrsn, bszlyd, bsdblk, bsdpart,                          &
     &        bsfcla, bsfsan, bsfom, bsfcec,                            &
     &        bhrwcs, bhrwcf, bhrwcw, bhrwcr,                           &
     &        bhrwca, bh0cb, bheaep, bhrsk,                             &
     &        bhfredsat )
      else
          ! adjust hydro parameters for a change in bulk density
          call param_blkden_adj( layrsn, bsdblk, bsdblk0,               &
     &        bsdpart, bhrwcf, bhrwcw, bhrwca,                          &
     &        bsfcla, bsfom,                                            &
     &        bh0cb, bheaep, bhrsk )
      end if

!     convert soil water contents from mass basis to volume basis
      do k=1,layrsn

          theta(k) = bhrwc(k) * bsdblk(k)
          thetadmx(k) = theta(k)
          if( theta(k) .lt. 0.0 ) then
              write (*,*) 'hinit: theta(',k,') .lt. 0'
              write (*,*) 'hinit: bhrwc =',bhrwc(k),'bsdblk =',bsdblk(k)
          end if
          if( wc_type.ne.4 ) then
              bhfredsat(k) = 0.883
          end if
          thetas(k) = 1 - bsdblk(k) / bsdpart(k)   ! saturation
          thetes(k) = thetas(k) * bhfredsat(k)     ! reduced saturation content accounted for by entrapped air
          thetaf(k) = bhrwcf(k) * bsdblk(k)        ! field capacity
          thetaw(k) = bhrwcw(k) * bsdblk(k)        ! wilting point

        if( wc_type.eq.4 ) then
          thetar(k) = bhrwcr(k) * bsdblk(k)        ! residual water content
        else
!!        use theta corresponding to 80% relhum in soil for thetar
          temp = bsdblk(k)*1000.0  !convert Mg/m^3 to kg/m^3
          thetar(k) = volwatadsorb( temp, bsfcla(k), bsfom(k),          &
     &                              claygrav80rh, orggrav80rh )
        end if

!        call propsaxt(bsfsan(k), bsfcla(k),                             &
!     &                temp, temp1, temp2 )   !thetas, thetaf, thetaw

!        temp3 = (1.0-temp) * bsdpart(k) !bulk density
!        write(*,1000) k,bszlyt(k),                                      &
!     &        bsfsan(k),bsfcla(k),bsfom(k),                             &
!     &        bsdblk(k),thetas(k),                                      &
!     &        thetaf(k),thetaw(k),                                      &
!     &        temp3, temp,     !bulkden, sat vol                        &
!     &        temp1,           !field vol                               &
!     &        temp2            !wilt vol

!!      used with output for soil file screening
! 1000     format(i3,f7.0,20f7.4)

!          write(*,*) 'hinit:',k,bh0cb(k),bheaep(k),bhrsk(k),thetas(k),
!     &               thetaf(k),thetaw(k),thetar(k)
!
!         Campbell functions
!          call psd( bsfsan(k), bsfsil(k), bsfcla(k), gmd(k), gsd(k) )
!          potes(k) = -0.2 * gmd(k)**(-0.5)                      !H-77
!          bh0cb(k) = -2. * potes(k) + 0.2 * gsd(k)              !H-78
!          bheaep(k) = potes(k)*(bsdblk(k)/1.3)**(0.67*bh0cb(k)) !H-79
!
!         reverse calculation of field capacity and wilting point
!          thetar(k) = 0.0
!          temp = -33.33
!          temp1 = 1.0 / bh0cb(k)
!          thetaf(k) = volwat_matpot_bc(temp, thetar(k), thetas(k),
!     &                                 bheaep(k), temp1)
!          temp = -1500.0
!          thetaw(k) = volwat_matpot_bc(temp, thetar(k), thetas(k),
!     &                                 bheaep(k), temp1)
!
!          write(*,*) 'hinit:',k,bsfsan(k),bsfsil(k),bsfcla(k),bsfom(k),
!     &               bh0cb(k),bheaep(k),thetas(k),
!     &               thetaf(k),thetaw(k),thetar(k),bhrsk(k)
!
!         this is used with campbell functions as well
!          bhrsk(k) = waterk(bsdblk(k), bh0cb(k), bsfcla(k), bsfsil(k))
!
!          write(*,*) 'hinit:',k,bsfsan(k),bsfsil(k),bsfcla(k),bsfom(k),
!     &               bh0cb(k),bheaep(k),thetas(k),
!     &               thetaf(k),thetaw(k),thetar(k),bhrsk(k)

      end do

!      swci = sum(wc(1:layrsn))
      swci = dot_product(theta(1:layrsn),bszlyt(1:layrsn))

!      theta(0) = calctht0(bszlyd, thetes, thetar, theta,
!     *  thetaw, thetaf(1) - thetaw(1), 0.0_8, 0.0_8)                        !H-64,65,66
      theta(0) = theta(1)

      ! calculate the vapor transmissivity (mm/d^.5) using the surface layer
      ! taken from WEPP documentation eq 5.2.11 with conversion to use soil 
      ! minerals in fractions not percent
      vaptrans = 4.165 + 2.456 * bsfsan(1) - 1.703 * bsfcla(1)          &
     &         - 4.0 * bsfsan(1) * bsfsan(1)

      ! calculate the cumulative evaporation limit between stage 1 and stage 2 evap
      ! taken from WEPP documentation eq 5.2.10 with conversion to mm
      if( vaptrans .le. 3.0 ) then
          evaplimit = 0.0
      else
          evaplimit = 9.0 * (vaptrans - 3.0) ** 0.42
      end if

!      write(*,*) 'calctht0: hinit - theta(0) after',theta(0)

!     call subroutine psd and calculate soil hydraulic parameters
      do 110 k=1,layrsn
          if ( bh0cb(k) .eq. -99.9 ) then
              call psd( bsfsan(k), bsfsil(k), bsfcla(k), gmd(k),gsd(k))
              potes(k) = -0.2 * gmd(k)**(-0.5)         !H-77
              bh0cb(k) = -2. * potes(k) + 0.2 * gsd(k)  !H-78
              if ( bheaep(k) .eq. -99.90 ) then
                 bheaep(k) = potes(k)*(bsdblk(k)/1.3)**(0.67*bh0cb(k))    !H-79
              end if
          end if
          if ( bhrsk(k) .eq. -99.90 ) then
             bhrsk(k) = waterk(bsdblk(k), bh0cb(k), bsfcla(k),bsfsil(k))
          end if
! ***       write (*,*) ' theta(k), etc. ', k, theta(k),thetas(k),-bh0cb(k)
110    continue

      return
      end
!
!
