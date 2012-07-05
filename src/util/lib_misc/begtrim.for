!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!
!
!     LENTRM is used to deal with compiler differences

      integer   function begtrm (val)

!     + + + ARGUMENT DECLARATIONS + + +
!
      character*(*) val
!
!     + + + ARGUMENT DEFINITIONS + + +
!
!     + + + PARAMETERS + + +
!
!     + + + LOCAL VARIABLES + + +
!
      integer idx
!
!     + + + END SPECIFICATIONS + + +
!
      do idx = 1, len(val)
        if (val(idx:idx).ne.' ') then
          begtrm = idx
          return
        endif
      end do

      begtrm = 1
      return 
      end
