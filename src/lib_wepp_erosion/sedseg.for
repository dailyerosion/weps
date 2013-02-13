!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine sedseg(dslost,jun,iyear,noout,dstot,stdist,irdgdx,     &
     &    ysdist,avgslp,slplen,y,avedet,maxdet,ptdet,avedep,maxdep,     &
     &    ptdep,                                                        &
     &    detpt1, detpt2, dtavls, detstd, detmax, pdtmax, detmin,       &
     &    pdtmin, deppt1, deppt2, dpavls, depstd, depmax, pdpmax,       &
     &    depmin, pdpmin,ndetach,ndepos)  
     
          
      use wepp_interface_defs
     
      implicit none
!
!    + + + purpose + + +
!    this subroutine breaks the hillslope profile into detachment
!    and deposition segments.  it calls sr sedist and sr sedsta
!
!    called from subroutine sedout
!    author(s): m. nearing, d. flanagan
!
!    version: this module modified from wepp v2004.7 code, moved all
!     printing to separate routines.
!
!   date modified: 12-2008
!    modified by: d. flanagan, j. frankenberger
!
!    + + + argument declarations + + +
!
      integer, intent(in) :: jun, iyear, noout
      real, intent(in) :: dslost(100)                                 
      real, intent(in) :: irdgdx, avgslp, slplen, y(101)
      real, intent(out) :: dstot(1000), stdist(1000), ysdist(1000)
      real, intent(out) :: avedet,maxdet,ptdet
      real, intent(out) :: avedep,maxdep,ptdep
      real, intent(out) :: detpt1(100), detpt2(100), dtavls(100)
      real, intent(out) :: detstd(100), detmax(100), pdtmax(100)
      real, intent(out) :: detmin(100), deppt1(100), deppt2(100)
      real, intent(out) :: dpavls(100), depstd(100), depmax(100)
      real, intent(out) :: pdpmax(100), depmin(100), pdpmin(100)
      real, intent(out) :: pdtmin(100)
      integer, intent(out) :: ndetach, ndepos
!
!    + + + argument definitions + + +
!
!    dslost  - array containing the net soil loss/gain at each
!              of the 100 points on each ofe
!    jun     - unit number of file to write output to
!    iyear   - flag for printing out annual soil loss output
!    noout   - flag indicating printing of event by event
!              summary files is desired (see sedout)
!    dstot   -
!    stdist  -
!    irdgdx  - average interrill detachment rate on an ofe
!
!
!    + + + local variables + + +
!
      real detdis(100), depdis(100)                         
      real sum1, sum2, filoss, fidep, totmax, pdtmx, pdpmx
      real delxx
!    real mtf,kgmtpa
      integer idtsin(100), idpsin(100), jadet, jadep, jdet, itsin, ipsin
      integer lend, i, icnt, j, kbeg, kk, lbeg
      integer jflag(100), ibegin, lseg, jdep, iend
      integer imodel, ioutpt, ioutss
!    character*7 unit(9)
!
!    + + + local definitions + + +
!
!    deppt1 : distance where deposition section begins (m)
!    deppt2 : distance where deposition section ends (m)
!    dpavls : average deposition in section (kg/m**2)
!    detpt1 : distance where detachment section begins (m)
!    detpt2 : distance where detachment section ends (m)
!    dtavls : average detachment in section (kg/m**2)
!    detstd : standard deviation of detachment in sect.(kg/m**2)
!    detmax : maximum detachment in section (kg/m**2)
!    pdtmax : point of maximum detachment (m)
!    detmin : minimum detachment in section (kg/m**2)
!    pdtmin : point of minimum detachment (m)
!    depstd : standard deviation of deposition in sect.(kg/m**2)
!    depmax : maximum deposition in section (kg/m**2)
!    pdpmax : point of maximum deposition in section (m)
!    depmin : minimum deposition in section (kg/m**2)
!    pdpmin : point of minimum deposition in section (m)
!    delxx  - delta x increments between each point down ofe (m)
!
!    data unit /'     mm', '  kg/m2','    in.','    t/a','    ft.'
!   1,' lbs/ft','    lbs','   kg/m','      m'/
!
!
!    + + + end specifications + + +
!
      avedep = 0.0
      avedet = 0.0
      maxdep = 0.0
      maxdet = 0.0
      ptdep = 0.0
      ptdet = 0.0
!    tdep(ihill) = 0.0
!    tdet(ihill) = 0.0
      imodel = 1
      ioutpt = 1
      ioutss = 0
 
      call sedist(dslost,dstot,stdist,delxx,slplen,avgslp,y, ysdist)

!    
      lseg = 1
      jadep = 0
      jadet = 0
      sum1 = 0.0
      sum2 = 0.0
      jdet = 0
      jdep = 0
      itsin = 0
      ipsin = 0
      lend = 100
      kbeg = 0
      lbeg = 1
      icnt = 0
      ndetach = 0
      ndepos = 0
!    
      do 10 j = 1, lend
         if (dstot(j).ne.0.0) then
            if (kbeg.eq.0) then
               lbeg = j
               kbeg = 1
            end if
            icnt = j
         end if
   10 continue
!    
      ibegin = lbeg
      if (icnt.lt.lend) then
         lend = icnt + 1
      end if
!    
      if (dstot(lbeg).gt.0.0) jflag(lseg) = 1
      if (dstot(lbeg).lt.0.0) jflag(lseg) = 0
!    
!    
!    added by dcf 4/16/90 to cover possibility if dstot = 0
!    
      if (dstot(lbeg).eq.0.0) jflag(lseg) = 2
!    
      do 20 i = lbeg + 1, lend
         if ((jflag(lseg).eq.1.and.dstot(i).le.0.0).or.(i.eq.lend.and.  &
     &       dstot(i).gt.0.0)) then
!          
            jadet = jadet + 1
            iend = i - 1
            if (i.eq.lend.and.dstot(i).gt.0.0) iend = i
!          
!          if the beginning of the detachment is the first point on
!          the slope set the point to zero otherwise average i 
!          with the point before it
!          
            if (ibegin.eq.1) then
               detpt1(jadet) = 0.0
            else
               detpt1(jadet) = stdist(ibegin-1)
            end if
!          
            detpt2(jadet) = stdist(iend)
!          
            detdis(jadet) = detpt2(jadet) - detpt1(jadet)
!          
            if (i.eq.lend.and.dstot(i).lt.0.0) then
               jadep = jadep + 1
               idpsin(jadep) = 1
               dpavls(jadep) = dstot(lend)
               depstd(jadep) = 0.0
			   deppt1(jadep) = stdist(lend-1)   
               deppt2(jadep) = stdist(lend)
               depdis(jadep) = deppt2(jadep) - deppt1(jadep)
               depmax(jadep) = dstot(lend)
               pdpmax(jadep) = stdist(lend)
               depmin(jadep) = dstot(lend)
               pdpmin(jadep) = stdist(lend)
               ipsin = 1
               jdep = 1
            end if
            if (ibegin.eq.iend) then
               idtsin(jadet) = 1
			   detpt1(jadet) = stdist(ibegin-1)
               detpt2(jadet) = stdist(ibegin)
               detdis(jadet) = detpt2(jadet) - detpt1(jadet)
               dtavls(jadet) = dstot(ibegin)
               detstd(jadet) = 0.0
               detmax(jadet) = dstot(ibegin)
               pdtmax(jadet) = stdist(ibegin)
               detmin(jadet) = dstot(ibegin)
               pdtmin(jadet) = stdist(ibegin)
               itsin = 1
               jdet = 1
            else
               idtsin(jadet) = 0
               call sedsta(jadet,dtavls,detstd,detmax,pdtmax,detmin,    &
     &             pdtmin,ibegin,iend,jflag,lseg,dstot,stdist,delxx)
               jdet = 1
            end if
!           
            ibegin = iend + 1
            lseg = lseg + 1
            if (dstot(i).eq.0.0) jflag(lseg) = 2
            if (dstot(i).lt.0.0) jflag(lseg) = 0
!        
         else if ((jflag(lseg).eq.0.and.dstot(i).ge.0.0).or.(i.eq.lend  &
     &       .and.dstot(i).lt.0.0)) then
!           
            jadep = jadep + 1
            iend = i - 1
            if (i.eq.lend.and.dstot(i).lt.0.0) iend = i
            if (ibegin.eq.1) then
               deppt1(jadep) = 0.0
            else
               deppt1(jadep) = stdist(ibegin-1)
            end if
!           
            deppt2(jadep) = stdist(iend)
            depdis(jadep) = deppt2(jadep) - deppt1(jadep)
!           
            if (i.eq.lend.and.dstot(i).gt.0.0) then
               jadet = jadet + 1
               idtsin(jadet) = 1
               dtavls(jadet) = dstot(lend)
               detstd(jadet) = 0.0
               detpt1(jadet) = stdist(lend-1)
               detpt2(jadet) = stdist(lend)
               detdis(jadet) = detpt2(jadet) - detpt1(jadet)
               detmax(jadet) = dstot(lend)
               detmin(jadet) = dstot(lend)
               pdtmax(jadet) = stdist(lend)
               pdtmin(jadet) = stdist(lend)
               itsin = 1
               jdet = 1
            end if
            if (ibegin.eq.iend) then
               idpsin(jadep) = 1
               deppt1(jadep) = stdist(ibegin-1)
               deppt2(jadep) = stdist(ibegin)
               depdis(jadep) = deppt2(jadep) - deppt1(jadep)
               dpavls(jadep) = dstot(ibegin)
               depmin(jadep) = dstot(ibegin)
               depmax(jadep) = dstot(ibegin)
               depstd(jadep) = 0.0
               pdpmax(jadep) = stdist(ibegin)
               pdpmin(jadep) = stdist(ibegin)
               ipsin = 1
               jdep = 1
            else
               idpsin(jadep) = 0
               call sedsta(jadep,dpavls,depstd,depmax,pdpmax,depmin,    &
     &             pdpmin,ibegin,iend,jflag,lseg,dstot,stdist,delxx)
               jdep = 1
            end if
!           
            ibegin = iend + 1
            lseg = lseg + 1
            if (dstot(i).eq.0.0) jflag(lseg) = 2
            if (dstot(i).gt.0.0) jflag(lseg) = 1
!        
         else if (jflag(lseg).eq.2.and.dstot(i).ne.0.0) then
            iend = i - 1
            if (i.eq.lend) then
               if (jflag(lseg).eq.1) then
                  jadet = jadet + 1
                  idtsin(jadet) = 1
                  detpt1(jadet) = stdist(lend-1)
                  detpt2(jadet) = stdist(lend)
                  detdis(jadet) = detpt2(jadet) - detpt1(jadet)
                  dtavls(jadet) = dstot(lend)
                  detstd(jadet) = 0.0
                  detmax(jadet) = dstot(lend)
                  detmin(jadet) = dstot(lend)
                  pdtmin(jadet) = stdist(lend)
                  pdtmax(jadet) = stdist(lend)
                  itsin = 1
                  jdet = 1
               else if (jflag(lseg).eq.0) then
                  jadep = jadep + 1
                  idpsin(jadep) = 1
                  deppt1(jadep) = lend
                  deppt2(jadep) = lend
                  depdis(jadep) = deppt2(jadep) - deppt1(jadep)
                  dpavls(jadep) = dstot(lend)
                  depstd(jadep) = 0.0
                  depmax(jadep) = dstot(lend)
                  depmin(jadep) = dstot(lend)
                  pdpmin(jadep) = stdist(lend)
                  pdpmax(jadep) = stdist(lend)
                  ipsin = 1
                  jdep = 1
               end if
            end if
            lseg = lseg + 1
            ibegin = iend + 1
            if (dstot(i).gt.0.0) jflag(lseg) = 1
            if (dstot(i).lt.0.0) jflag(lseg) = 0
         end if
!     
   20 continue
!     
!     
      if (jadet.gt.0) then
         if (jdet.gt.0) then
            totmax = detmax(1)
            pdtmx = pdtmax(1)
            do 30 kk = 1, jadet
               sum1 = sum1 + (dtavls(kk)*detdis(kk))
               sum2 = sum2 + detdis(kk)
!              
               if (detmax(kk).gt.totmax) then
                  totmax = detmax(kk)
                  pdtmx = pdtmax(kk)
               end if
!           
   30       continue
!           
            if (sum2.ne.0.0) then
               filoss = sum1 / sum2
            end if
        avedet = filoss
        maxdet = totmax
        ptdet = pdtmx
         end if
      end if
      
      ndetach = jadet
!     
!     
      if (jadep.gt.0) then
!         if (noout.le.1) write (jun,1500)
         if (jdep.gt.0) then
            sum1 = 0.0
            sum2 = 0.0
            totmax = depmax(1)
            pdpmx = pdpmax(1)
!           
            do 90 kk = 1, jadep
               sum1 = sum1 + (dpavls(kk)*depdis(kk))
               sum2 = sum2 + depdis(kk)
               if (depmax(kk).lt.totmax) then
                  totmax = depmax(kk)
                  pdpmx = pdpmax(kk)
               end if
!           
   90       continue
!           
            if (sum2.ne.0.0) then
               fidep = sum1 / sum2
!              tdep(ihill)=sum2*fwidth*fidep

           avedep = fidep
           maxdep = totmax
           ptdep = pdpmx
            end if
!           
         end if
         
         ndepos = jadep
         
       end if
      return

!     
      end
