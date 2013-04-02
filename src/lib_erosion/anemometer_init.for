!$Author$
!$Date$
!$Revision$
!$HeadURL$
       subroutine anemometer_init

! + + +  PURPOSE + + +
!     To provide initial default values to wx station variables
!     in the common block 'p1const'
!
!     The anemom. ht. and awwzo may be changed by read inputs in the
!     stand-alone erosion code. If anem. at the field  i.e flag =1,
!     then awwzo is set equal to the field zo in sbwus.
!
! + + + VARIABLE DEFINITIONS + + +
!     anemht = anemometer height (m)
!     awzzo  = aerodynamic roughness at anemometer (mm)
!     awzdisp - Weather station zero plane displacement height (mm)
!     wzoflg = flag = 0 for anem. and fixed awwzo at wx station
!              flag = 1 for anem. and variable awzzo at field
!
!     + + + GLOBAL COMMON BLOCKS + + +
!
! + + + END SPECIFICATIONS + + +

          ! named common block where these are declared
          include 'p1const.inc' ! anemht, awzzo, awzdisp, wzoflg

          ! set the default data values
          anemht =  10.0
          awzzo = 25.0
          awzdisp = 0.0
          wzoflg = 0

      return
      end

