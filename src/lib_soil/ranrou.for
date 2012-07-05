!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!
      subroutine ranrou(                                                &
     &  csfsil, csfsan, bszrr, bszrro, cumpa, dcump, cf2cov, csvroc)

!     + + + ARGUMENT DECLARATIONS + + +
      real csfsil, csfsan
      real bszrr, bszrro
      real cumpa, dcump, cf2cov, csvroc

!     + + + LOCAL VARIABLES + + +

      real arr, crr

!     + + + LOCAL DEFINITIONS + + +
!
!   arr       - regression coef. to calc. random roughness
!   crr       - regression coefficient for random roughness decrease
!   csfsan    - top layer fraction of sand.
!   csfsil    - top layer fraction of silt.
!   csvroc    - soil volume fraction of rock in top layer

!  RANDOM ROUGHNESS SECTION:
!     calc. reg. coefficients (eq. S-12, S-13)
      arr = 91.08 + 765.8 * csfsil
      crr = 0.53 + 4.66 * csfsan - 3.8 * csfsan**1.5-1.22*(csfsan)**0.5
!     calc. apparent precip. (eq. S-11 is S-14 solved for a bare surface)
!     changed * to ** to conform to equ S-10
!     erosion could make bszrr > bszrro so insert fix - LH
      if(bszrr .ge. bszrro) then
         cumpa = 0.0
         bszrro = bszrr
      else
         cumpa = arr * (-log(bszrr / bszrro)) ** (1.0 / crr)
      end if

!     update random roughness (eq. S-14)
! *** debugging fix

      if ((cumpa + (1.0 - csvroc) * cf2cov * dcump)/arr                 &
     &  .lt. 0.) then
         bszrr = bszrro
!         write(*,*) 'soil: debugging fix executed 1'
!         write(*,*) '  cumpa, dcump, cf2cov, arr, csvroc ',
!     *            cumpa, dcump, cf2cov, arr, csvroc
      else
! *** end of debugging fix
! ***      write(*,*) ' crr ', crr
         bszrr = bszrro * exp(-((cumpa + (1.0 - csvroc)*                &
     &     cf2cov*dcump) /arr)**crr)
      endif
      if ( bszrr .lt. 2.0) bszrr = 2.0
      end
