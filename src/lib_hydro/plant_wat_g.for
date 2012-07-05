!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!
      real function plant_wat_g( begind, endd, bhrwcf, bhrwcw, bsdblk,  &
     &                           bszlyt, nlay )

      integer nlay
      real bhrwcf(nlay), bhrwcw(nlay), bsdblk(nlay), bszlyt(nlay)
      real begind, endd

      integer lay
      real sum, depth, prevdepth, thick

      sum = 0.0
      depth = 0.0
      prevdepth = 0.0
      do lay = 1,nlay
          depth = depth + bszlyt(lay)
          if(         (depth .gt. begind)                               &
     &       .and. (prevdepth .lt. endd) ) then
              if(  prevdepth .le. begind                                &
     &            .and. depth .ge. endd ) then
                  thick = endd - begind
              else if( prevdepth .le. begind ) then
                  thick = depth - begind
              else if( depth .ge. endd ) then
                  thick = endd - prevdepth
              else
                  thick = bszlyt(lay)
              end if
              sum = sum + (bhrwcf(lay) - bhrwcw(lay))                   &
     &            * bsdblk(lay) * thick
          end if
          prevdepth = depth
      end do
      plant_wat_g = sum

      return
      end
