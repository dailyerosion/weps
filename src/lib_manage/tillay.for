!$Author$
!$Date$
!$Revision$
!$HeadURL$
!
! $Header: /weru/cvs/weps/weps.src/manage/tillay.for,v 1.3 2002-09-04 20:21:49 wagner Exp $
!
! This routine accepts the tillage depth, soil layer thicknesses,
! and the number of soil layers.  It returns the number of layers
! that will be considered to be within the tillage zone for this
! operation.

      integer function tillay (tdepth, lthick, nlay)

      real    tdepth
      integer nlay
      real    lthick(nlay)

      integer i
      real    d

      if (tdepth .eq. 0.0) then
        tillay = 0
        goto 1000
      else if (tdepth .le. lthick(1)) then
        tillay = 1
        goto 1000
      endif
      d = lthick(1)
      do 100 i=2, nlay
        d = d + lthick(i)
        if (tdepth .lt. d) then
          if ( (d - tdepth) .lt. (tdepth - (d-lthick(i))) ) then
            tillay = i
            goto 1000
          else
            tillay = i-1
            goto 1000
          endif
        endif
  100 continue
        tillay = nlay
 1000 return
      end
