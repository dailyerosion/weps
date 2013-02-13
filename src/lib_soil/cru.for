!$Author$
!$Date$
!$Revision$
!$HeadURL$
      subroutine cru( bszcr,cumpa,csfcla,dcump,bsfcr,bhzsmt,            &
     &  bsmlos,csfom,csfcce,csfsan,bsmls0,bszrgh,bszrr,bsflos)

!calculates 4 crust variables:
!crust thickness, mm
!fraction of soil crust cover, m2/m2
!mass of loose erodible material on crust, kg/m2
!fraction cover of loose erodible material, m2/m2 

!     + + + ARGUMENT DECLARATIONS + + +
      real bszcr,cumpa,csfcla,dcump,bsfcr,bhzsmt,bsmlos,csfom
      real csfcce,csfsan,bsmls0,bszrgh,bszrr,bsflos 

!     + + + LOCAL VARIABLES + + +
      real sz, cflos
      real temp

!     + + + LOCAL DEFINITIONS + + +
!   sz        - maximum of ridge height and 4 times random roughness
!   cflos     - correction factor for decease of fraction loose cover
!               area on crust caused by roughness

!     calc. apparent precip. (eq. S-14)
      if (bszcr .ge. 7.6) bszcr = 7.599
      cumpa = -(alog(1.0-bszcr/7.6)) / (0.0705-0.0687*csfcla**0.146)
!     check for threshold precip.
!     ie. check to see if a H2O addition exceeding 10mm has been made
! *** threshold is not noted for S-15, this test should go later
!      write(*,*) '*******cumpa + dcump<10.? ',cumpa,dcump
      if((cumpa + dcump) .lt. 10. ) go to 12
! ***
!     calc. crust thickness (eq. S-16, *** sb S-15)
      temp = (0.0705 - 0.0687*csfcla**0.146)*(cumpa + dcump)
      if (temp.gt.20.0) then                !check to avoid underflow
          bszcr = 7.6
      else
          bszcr = 7.6*(1.0 - exp(-temp))
      endif

!     calc. apparent precip (eq. S-17 *** sb S-16)
      if( bsfcr .lt. 1.0 ) then
          cumpa = -(alog(1.0 - bsfcr))/0.045
          ! calc. crust cover fraction (eq. S-18, *** sb S-17)
          bsfcr = 1.0 - exp(- 0.045*(cumpa + dcump))
      end if

!  loose erodible material on crust
!     set max loose mass (eq S-20, *** sb S-19)
      if (bhzsmt .eq. 0.0) then !if no snow melt
         if (csfcla .eq. 0.0) then
            bsmlos = 0.1*exp(-0.57 + 0.22 * 999. + 7.0 * csfcce - csfom)
         else
            bsmlos = 0.1*exp(-0.57 + 0.22 * csfsan / csfcla             &
     &               + 7.0 * csfcce - csfom)
         end if
!        set upper limit on loose mass (eq. S-21, *** sb S-20)
         if (bsmlos .gt. 3.0) bsmlos = 3.0
      else
!        check if water is from snowmelt (eq. S-22, *** sb S-21,22)
         if (bhzsmt .gt. 0.0) then !if snow melt
             bsmls0 = bsmlos
             bsmlos = bsmlos * (1.0 - 0.1 * bhzsmt)
             if (bsmlos .lt. bsmls0*0.1) bsmlos = bsmls0 * 0.1
         else
!             bsmlos = bsmlos * (1.0 - 0.0053 * dcump)
             bsmlos = bsmlos
         endif
      endif

!     fraction cover of loose erodible material (eq. S-24, S-25, sb S-23,24)
      sz = amax1(4.0*bszrr, bszrgh)
! ***      cflos = sqrt(bsmlos)/(0.24*sz)
! *** debugging fix
      cflos = exp(-0.08*sz**0.5)
! *** eodf
      if (cflos .gt. 1.0) cflos = 1.0
      bsflos = (1.0 - exp(-3.5*bsmlos**1.5))*cflos

   12 continue

      end

