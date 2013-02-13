!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine sedist(dslost,dstot,stdist,delxx,slplen,avgslp,        &
     &    y,ysdist)
     
      use wepp_interface_defs
      
      implicit none
!
!     + + + purpose + + +
!     creates a representative hillslope profile (sediment load,
!     x-distance, delta-x) from overland flow elements.
!
!     called from sedseg
!     author(s): d.c. flanagan, j.c. ascough
!
!     version: this module modified from wepp version 2004.7
!     date modified: 9-30-2004
!     modified by: d. flanagan
!
!     + + + argument declarations + + +
!
      real, intent(in) :: slplen, avgslp, y(101), dslost(100)
      real, intent(out) :: ysdist(1000),dstot(1000),stdist(1000),delxx
!
!
!     + + + argument definitions + + +
!
!     dslost - net soil loss/gain at each point down entire
!     dstot  - sediment loss for each poitn down hillslope (kg/m^2)
!     stdist - distance down hilllslope at each point (m)
!     delxx  - delta x increments between each point down hilslope (m)
!     slplen - slope length of each ofe (m)
!     avglsp - average slope of line passing through endpoints
!              of an ofe  (m/m)
!     y      - vertical distance of each point on profile (m)
!
!     + + + local variables + + +
!
      real xdist(101), ydist(101), dist, ytot
      integer k, kk, ll
!
!     + + + local definitions + + +
!     xdist  -
!     ydist  -
!     dist   -
!     ytot   -
!     i      -
!     j      -
!     k      -
!     kk     -
!     ll     -
!
!     + + + end specifications + + +
!
!
      kk = 0
      ytot = 0.
!      if (nplane.gt.1) then
!        do 10 i = 2, nplane
!           ytot = ytot + slplen(i) * avgslp(i)
!  10    continue
!     end if
!     
!     do 40 i = 1, nplane
!        
         delxx = slplen / 100.0
!        
         do 30 k = 2, 101
            ll = k - 1
            dist = 0.0
!           do 20 j = 1, i
!              if (i.ne.j) then
!                 dist = slplen(i-j) + dist
!              end if
!  20       continue
            ydist(k) = ytot + slplen * avgslp * y(k)
            xdist(k) = dist + (slplen/100.0) * float(k-1)
            kk = kk + 1
            dstot(kk) = dslost(ll)
            stdist(kk) = xdist(k)
            ysdist(kk) = ydist(k)
   30    continue
!        if (i.ne.nplane) ytot = ytot - slplen(i+1) * avgslp(i+1)
!  40 continue
!     
      return
      end
