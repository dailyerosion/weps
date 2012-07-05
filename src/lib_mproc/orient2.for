!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!
!
!
      subroutine orient2 (dh,ds,impl_dh,impl_ds)


!     + + + PURPOSE + + +
!
!     This subroutine performs an oriented roughness calculation
!     after a tillage operation.  It creates dikes, regardless
!     whether ridges already exist (it assumes they do).
!
!
!     + + + KEYWORDS + + +
!     oriented roughness (OR), tillage (primary/secondary)
!
!     + + + ARGUMENT DECLARATIONS + + +
!
!
!     + + + ARGUMENT DEFINITIONS + + +
!
!
!     + + + ACCESSED COMMON BLOCK VARIABLE DEFINITIONS + + +
!
!
!     + + + PARAMETERS + + +
!
!     + + + LOCAL VARIABLES + + +
!
          real     dh,ds
          real     impl_dh,impl_ds
!
!     + + + LOCAL VARIABLE DEFINITIONS + + +
!
!     dh      - current dike height (mm)
!     ds      - current dike spacing (mm)
!     impl_ds - implement dike spacing (mm)
!     impl_dh - implement dike height (mm)

!     + + + END SPECIFICATIONS + + +
!
!  Perform the calculation of the oriented OR after a tillage
!     operation.
!
      ds = impl_ds
      dh = impl_dh

      return
      end
