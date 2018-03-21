!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!
! This routine accepts the crop root depth, soil layer thicknesses,
! and the number of soil layers.  It returns the number of layers
! that will be considered to contain crop roots. 

      integer function rootlay (rtdepth, lthick, nlay)

      integer nlay
      real    rtdepth
      real    lthick(nlay)

      integer i
      real    d

      if ((rtdepth*1000.0) .lt. lthick(1)) then
        rootlay = 1
        return
      endif
      d = lthick(1)
      do i=2, nlay
        d = d + lthick(i)
        if ((rtdepth*1000.0) .lt. d) then
          if ( (d-(rtdepth*1000.)) .lt.                                   &
     &         ( (rtdepth*1000.) - (d-lthick(i)) ) ) then 
            rootlay = i
            return
          else
            rootlay = i-1
            return
          endif
        endif
      end do
      rootlay = nlay
      return
      end
