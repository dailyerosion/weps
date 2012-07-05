!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine param_pot_bc( nlay, bsdblk,                            &
     &                         bsdpart, bhrwcf, bhrwcw,                 &
     &                         bsfcla, bsfom,                           &
     &                         bh0cb, bheaep )

!     + + + PURPOSE + + +
!     
!     This subroutine calculates matric potential parameters from given
!     values of bulk density, gravimetric 1.3 bar water, 15 bar water and
!     clay and organic matter fraction

!     + + + KEYWORDS + + +
!     matric potential parameters

      include 'hydro/clayomprop.inc'

!     + + + ACCESSED COMMON BLOCK VARIABLE DEFINITIONS + + +
!     claygrav80rh - gravimetric water content of clay at 80 percent relative humidity
!     orggrav80rh  - gravimetric water content of soil organic matter at 80 percent relative humidity

!     + + + ARGUMENT DECLARATIONS + + +
      integer nlay
      real bsdblk(*)
      real bsdpart(*), bhrwcf(*), bhrwcw(*)
      real bsfcla(*), bsfom(*)
      real bh0cb(*), bheaep(*)

!     + + + ARGUMENT DEFINITIONS + + +
!     nlay     - number of soil layers to be updated
!     bsdblk   - bulk density (Mg/m^3)
!     bsdpart  - particle density (Mg/m^3)
!     bhrwcf   - gravimetric 1/3 bar water
!     bhrwcw   - gravimetric 15 bar water
!     bsfcla   - fraction of soil mineral portion which is clay
!     bsfomf   - fraction of total soil mass which is organic matter
!     bh0cb    - Brooks and Corey pore size interation exponent b
!     bheaep   - Brooks and Corey air entry potential

!     + + + FUNCTION DECLARATIONS + + +
      real volwatadsorb

!     + + + LOCAL VARIABLES + + +
      integer lay
      real thetas, thetaf, thetaw, thetar
      real temp

!     + + + LOCAL VARIABLE DEFINITIONS + + +

!     + + + END SPECIFICATIONS + + + 

      do lay=1,nlay
          thetaf = bhrwcf(lay) * bsdblk(lay)        ! field capacity
          thetaw = bhrwcw(lay) * bsdblk(lay)        ! wilting point
          thetas = 1 - bsdblk(lay) / bsdpart(lay)   ! saturation

!!        use theta corresponding to 80% relhum in soil for thetar
          temp = bsdblk(lay)*1000.0  !convert Mg/m^3 to kg/m^3
          thetar = volwatadsorb( temp, bsfcla(lay), bsfom(lay),         &
     &                           claygrav80rh, orggrav80rh )

!         error check and adjustments to prevent numerical problems
!         with curve fit
          if( thetas.le.thetaf ) then
              write(0,*)                                                &
     &         'Error: saturation less than field capacity, layer', lay
              call exit(1)
          else if( thetaf.le.thetaw ) then
              write(0,*) 'field capacity less than wilting point, layer'&
     &                  ,lay
              call exit(1)
          end if
          thetar = min( thetar, 0.8 * thetaw )

!         Calculate air entry and b to match saturated, field
!         capacity, permanent wilting point and residual water
!         as calcuated above 
          bh0cb(lay) = -(log(33.333)-log(1500.0))                       &
     &            / (log((thetaf-thetar)/(thetas-thetar))               &
     &            - log((thetaw-thetar)/(thetas-thetar)))
          bheaep(lay)=-exp(log(1500.0)+bh0cb(lay)                       &
     &             *log((thetaw-thetar)/(thetas-thetar)))

!         error check brooks and corey b value to keep in range and
!         prevent later numberical problems
          if( bh0cb(lay).gt.50.0 ) then
              write(0,*) 'Derived Brooks and Corey b too large, layer', &
     &                   lay
              call exit(1)
          endif

      end do

      end
