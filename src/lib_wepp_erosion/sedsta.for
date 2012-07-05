      subroutine sedsta(jnum,dloss,dsstd,vmax,pmax,vmin,pmin,ibegin,    &
     &    iend,jflag,lseg,dstot,stdist,delxx)
     
      use wepp_interface_defs
      
      implicit none
!
!     + + + purpose + + +
!     finds the mean and standard deviation of detachment or
!     deposition points within a segment.  it calls sr sedmax.
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
      integer, intent(in) ::  jnum, ibegin, iend, jflag(100), lseg
      real, intent(out) :: pmax(100), pmin(100)
      real, intent(out) :: vmax(100), vmin(100)
      real, intent(in) :: dstot(1000), stdist(1000), delxx
      real, intent(out) :: dloss(100), dsstd(100)
!
!
!     + + + argument definitions + + +
!
!     jnum   - number of the detachment or deposition region
!     dloss  - computed detachment (or deposition) at a point
!     dsstd  - computed sample standard deviation of segment
!     vmax   - maximum detachment (or deposition) in a segment
!     pmax   - point of maximum detach. or depos. in segment (m)
!     vmin   - minimum detachment (or deposition) in a segment
!     pmin   - point of minimum detach. or depos. in segment (m)
!     dstot  - sediment loss down profile at each point (kg/m^2)
!     stdist - distance down profile at each point (m)
!     ibegin - beginning of deposition/detachment segment
!     iend   - end of deposition/detachment segment
!     jflag  - flag for whether deposition or detachment occurring
!     lseg   - flag for number of deposition/detachment segments
!              on hillslope profile
!     delxx  - delta x increments between each point down hilslope (m)
!
!     + + + local variables + + +
!
      real deviat, dssum, dsloss, dsdist
      integer i, jjplan, ncomp, ncompr
!
!     + + + local definitions + + +
!    deviat :  deviation of detach. (or depos.) from the mean
!    dssum  :  sum of the squared deviations from the mean
!    dsloss :  sum of the detachment (or deposition) at all points
!    i      :  counter variable
!    jjplan :  variable to assign ofe number for array assignment
!    ncomp  :  used to set jjplan
!    ncompr :  used to find which ofe point belongs to
!
!     + + + end specifications + + +
!*******************************************************************
!
      dssum = 0.0
      dsloss = 0.0
      dsdist = 0.0
      ncompr = 101
      ncomp = 1
!     
      do 20 i = ibegin, iend
   10    if (i.lt.ncompr) then
            jjplan = ncomp
         else
            ncompr = ncompr + 100
            ncomp = ncomp + 1
            go to 10
         end if
!        
         dsloss = dsloss + delxx * dstot(i)
         dsdist = dsdist + delxx
   20 continue
!     
!     
      dloss(jnum) = dsloss / dsdist
!     
!     
      do 30 i = ibegin, iend
!        
         deviat = dstot(i) - dloss(jnum)
         dssum = dssum + (deviat*deviat)
   30 continue
!     
      if (iend.ne.ibegin) then
         dsstd(jnum) = sqrt((dssum/(iend-ibegin)))
      else
         dsstd(jnum) = 0.0
      end if
!     
      call sedmax(jnum,vmax,vmin,pmax,pmin,dstot,stdist,ibegin,iend,    &
     &    jflag,lseg)
!     
      return
      end
