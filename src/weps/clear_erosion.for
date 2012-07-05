!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!
        subroutine clear_erosion()

        include "p1werm.inc"
        include "erosion/m2geo.inc"
        include "erosion/e2erod.inc"

        integer i,j

        do 215 i = 0, imax
           do 210 j = 0, jmax
              egt(i,j) = 0.0
              egtcs(i,j) = 0.0
              egtss(i,j) = 0.0
              egt10(i,j) = 0.0
210        continue
215     continue

        return
        end
