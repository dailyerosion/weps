!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!
      integer function skpnam(line)
! ****************************************************************** wjr
! function to skip the type, code and name on records in management files
!
!     Edit History
!     20-Feb-99     wjr     wrote
!
      character line*80
!
      integer idx
      integer cnt
!
!     called functions
!      
      cnt = 0
      do 10 idx=2,len(line)
        if (line(idx:idx).eq.' '.and.line(idx-1:idx-1).ne.' ') then
          cnt = cnt + 1
          if (cnt.eq.3) then
              skpnam=idx
              return
          endif
        endif
   10 continue
      skpnam = len(line)
      return
      end
