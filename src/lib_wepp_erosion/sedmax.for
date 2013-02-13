!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine sedmax(jnum,amax,amin,ptmax,ptmin,dstot,stdist,ibegin, &
     &    iend,jflag,lseg)
     
      use wepp_interface_defs
      
      implicit none
!
!     + + + purpose + + +
!     finds the maximum and minimum detachment and deposition
!     from segments.
!
!     called from sedsta.
!     author(s): d.c. flanagan, j.c. ascough
!
!     version: this module modified from wepp version 2004.7
!     date modified: 9-30-2004
!     modified by: d. flanagan
!
!     + + + argument declarations + + +
      integer, intent(in) :: jnum, ibegin, iend, jflag(100), lseg
      real, intent(out) ::  amax(100), amin(100), ptmax(100), ptmin(100)
      real, intent(in) :: dstot(1000), stdist(1000)

!
!     + + + argument definitions + + +
!     jnum   - detachment/deposition region number
!     amax   - maximum detachment/deposition for region
!     amin   - minimum detachment/deposition for region
!     ptmax  - location of maximum detachment/deposition
!     ptmin  - location of minimum detachment/deposition
!     dstot  - sediment loss down profile at each point (kg/m^2)
!     stdist - distance down profile at each point (m)
!     ibegin - beginning of deposition/detachment segment
!     iend   - end of deposition/detachment segment
!     jflag  - flag for whether deposition or detachment occurring
!     lseg   - flag for number of deposition/detachment segments
!              on hillslope profile
!
!     + + + local variables + + +
      integer iptmin, jptmin, iptmax, jptmax, i
      real chkval
!
!     + + + local definitions + + +
!     iptmin - point where min. is first encountered
!     jptmin - point where min. is last encountered
!     iptmax - point where max. is first encountered
!     jptmax - point where max. is last encountered
!     chkval - amount 2 numbers must differ by to be "different"
!
!
!     + + + end specifications + + +
!
      iptmin = ibegin
      iptmax = ibegin
      jptmin = ibegin
      jptmax = ibegin
      amax(jnum) = dstot(ibegin)
      ptmax(jnum) = stdist(ibegin)
      amin(jnum) = dstot(ibegin)
      ptmin(jnum) = stdist(ibegin)
!     
      chkval = dstot(ibegin) * 0.0001
!     
!     for a segment where detachment is occurring....
      if (jflag(lseg).eq.1) then
!        
         do 10 i = ibegin + 1, iend
!           
            if (dstot(i).gt.amax(jnum)) then
               if ((dstot(i)-amax(jnum)).gt.chkval) then
                  amax(jnum) = dstot(i)
                  ptmax(jnum) = stdist(i)
                  iptmax = i
                  jptmax = i
               else
                  jptmax = i
               end if
            end if
!           
!           
            if (dstot(i).lt.amin(jnum)) then
               if ((amin(jnum)-dstot(i)).gt.chkval) then
                  amin(jnum) = dstot(i)
                  ptmin(jnum) = stdist(i)
                  iptmin = i
                  jptmin = i
               else
                  jptmin = i
               end if
            end if
!        
   10    continue
!        
         if (iptmin.ne.jptmin) then
            i = (iptmin+jptmin) / 2
            amin(jnum) = dstot(i)
            ptmin(jnum) = stdist(i)
         end if
!        
         if (iptmax.ne.jptmax) then
            i = (iptmax+jptmax) / 2
            amax(jnum) = dstot(i)
            ptmax(jnum) = stdist(i)
         end if
!     
!     
!     for a segment where deposition (negative detachment)
!     is occurring....
!     
      else if (jflag(lseg).eq.0) then
!        
!        
         do 20 i = ibegin, iend
!           
            if (dstot(i).lt.amax(jnum)) then
               if ((amax(jnum)-dstot(i)).gt.chkval) then
                  amax(jnum) = dstot(i)
                  ptmax(jnum) = stdist(i)
                  iptmax = i
                  jptmax = i
               else
                  jptmax = i
               end if
            end if
!           
!           
            if (dstot(i).gt.amin(jnum)) then
               if ((dstot(i)-amin(jnum)).gt.chkval) then
                  amin(jnum) = dstot(i)
                  ptmin(jnum) = stdist(i)
                  iptmin = i
                  jptmin = i
               else
                  jptmin = i
               end if
            end if
!        
   20    continue
!        
         if (iptmin.ne.jptmin) then
            i = (iptmin+jptmin) / 2
            amin(jnum) = dstot(i)
            ptmin(jnum) = stdist(i)
         end if
!        
         if (iptmax.ne.jptmax) then
            i = (iptmax+jptmax) / 2
            amax(jnum) = dstot(i)
            ptmax(jnum) = stdist(i)
         end if
!     
!     
      else if (jflag(lseg).eq.2) then
         iptmax = ibegin
         iptmin = ibegin
      end if
!     
      return
      end
