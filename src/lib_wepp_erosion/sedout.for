      subroutine sedout(sumfile,irdgdx,dslost,avsole,                   &
     &    npart,frac,                                                   &
     &    dia,spg,slplen,fwidth,avgslp,                                 &
     &    y,totlen,years)
     
      use wepp_interface_defs
      
      implicit none
!
!     + + + purpose + + +
!     controls the printing of output.
!     places detachment and deposition output data in column form.
!
!     called from subroutine contin
!     author(s): d. flanagan and j. ascough
!
!     version: this module modified from wepp v2004.7 code
!     date last modified:  4-1-2005
!
!     + + + parameter declarations + + +
!
      integer mxpart
      parameter (mxpart = 10)
!
!     + + + argument declarations + + +
!
      integer, intent(in) :: npart, sumfile, years
      real, intent(in) :: irdgdx, dslost(100), avsole,                  &
     & frac(mxpart),                                                    &
     &     dia(mxpart), spg(mxpart),                                    &
     &     slplen,                                                      &
     &     fwidth, avgslp
      real, intent(in) :: y(101), totlen
!
!     + + + argument definitions + + +
!
!     npart  - number of particle size classes
!     irdgdx - interrill contribution to dg/dx) in (kg/m^2)
!     dslost - net soil loss/gain at each point on hillslope for
!              a storm event (time*dg/dx) - (kg/m**2)
!     avsole - storm sediment loss (kg/m)
!     enrato - enrichment ratio of specific surface area of sediment
!     frac   - fraction of each particle class at point of detachment
!     dia    - diameter of particle class (m)
!     spg    - specific gravity of particle class
!     frcly  - fraction of clay in a particle class
!     frslt  - fraction of silt in a particle class
!     frsnd  - fraction of sand in a particle class
!     frorg  - fraction of organic matter in a particle class
!     frcflw - fraction of each particle type in flow
!     slplen - slope length of an ofe (m)
!     fwidth - field width (m)
!
!
!     + + + local variables + + +
!
!     integer iyear, isum, ievt, ifofe, noout
      integer iyear, noout, k
      integer i, j, jend, nelem1, nelem2, nelem3, m
      integer ncol1(10), ncol2(10), ncol3(10), ncol4(10), ncol11 
      integer ncol22, ncol33, ncol44, nfelem(1000)
      real ss1, ss2, ss3, x1, x2, x3, marea
      real dstot(1000), ysdist(1000), stdist(1000)
      real avedet,maxdet,ptdet,avedep,maxdep,ptdep
!
!     + + + local definitions + + +
!
!     iyear  - flag that indicates getting average annual summaries
!              through sedout ( set in contin or wshdrv)
!     isum   - flag to indicate if the information sent to sedout is
!              supposed to write to summary files
!              (0 - means no,  1 - means yes )
!     ievt   - flag to indicate if the information sent to sedout is
!              supposed to write to event line output
!              summary files  (0 - means no,  1 - means yes )
!     ifofe   - flag to indicate if the information sent to sedout is
!              supposed element line output  (0 - no; 1 - yes)
!     noout  - flag indicating type of output desired
!                    0 - event by event - no summary files
!                    1 - event by event - summary files
!                    2 - all other output - with summary or graphics
!     i      - counter variable
!     j      - counter variable
!     jend   - value of cumulative last point on hillslope number
!     nelem1 -
!     nelem2 -
!     nelem3 -
!     m      - 
!     ncol1  -
!     ncol2  -
!     ncol3  -
!     ncol4  -
!     ncol11 - 
!     ncol22 -
!     ncol33 -
!     ncol44 -
!     nfelem - number of cumulative points through ofes "j" units long
!     m      -
!     x1     - soil loss at point in column one to print out
!     ss1    - distance at point in column one to print out
!     x2     - soil loss at point in column two to print out
!     ss2    - distance at point in column two to print out
!     x3     - soil loss at point in column three to print out
!     ss3    - distance at point in column three to print out
!     totlen - total length of the hillslope profile (m)
!
!     + + + end specifications + + +
!
      character*7 units(13)
!
      data ncol1 /10 * 1/
      data ncol2 /35, 68, 101, 135, 168, 201, 235, 268, 301, 335/
      data ncol3 /69, 135, 201, 269, 335, 401, 469, 535, 601, 669/
      data ncol4 /33, 67, 101, 133, 167, 201, 233, 267, 301, 333/
      data nfelem /100 * 1, 100 * 2, 100 * 3, 100 * 4, 100 * 5, 100 * 6,&
     &    100 * 7, 100 * 8, 100 * 9, 100 * 10/                         
      data units /'     mm', '  kg/m2', '    in.', '    t/a', '    ft.',&
     &    ' lbs/ft', '    lbs', '   kg/m', '      m', '     kg',        &
     &    '   t/ha', '     ha', '  acres'/
!
!     totlen = 0.0
!     do 10 i = 1, nplane
!        totlen = totlen + slplen(i)
!  10 continue
!     
!     
!     metric hillslope area
      marea = totlen * fwidth
!     
!     data units
!     1=mm
!     2=kg/m2
!     3=in.
!     4=t/a
!     5=ft.
!     6=lbs/ft
!     7=lbs
!     8=kg/m
!     9=m
!     
!********************************************************************
!     
!     for now - set imodel = 2 for single storm simulation.
!     set for no event line output (ievt=0).
!     set for no summary output (isum=0)
!     set for single storm output (ioutpt=1)
!     set for event by event output with no summary files (noout=0)
!     set for no average annual summary (iyear = 0)
!     set for single storm output (ioutss = 0)
!     d. flanagan 9-29-2004
!     
!     
!     imodel = 2
!     ievt = 0
!     isum = 0
!     ioutpt = 1
      noout = 0
      iyear = 0
!     ioutss = 0
!     
!     jun = 31
!     if(imodel.eq.2)jun = 32
!     jun = 32

!     
      ncol11 = ncol1(1)
      ncol22 = ncol2(1)
      ncol33 = ncol3(1)
      ncol44 = ncol4(1)
      jend = 100

      write(sumfile,*) 'WEPS/WEPP Common Model'
      write(sumfile,*) 'October 28, 2008'
      write(sumfile,*) 'Results for ', years , ' year simulation.'
      write(sumfile,*) 'ANNUAL AVERAGE SUMMARIES'
      write(sumfile,*) '------------------------'

!	write(sumfile,*) 'I.   RAINFALL AND RUNOFF SUMMARY'
!	write(sumfile,*) '     ---------------------------'
!	write(sumfile,*) '     total summary:  years 1 - ',years

    
!     
!     call sedseg(dslost,sumfile,iyear,noout,avedet,maxdet,ptdep,
!     1            ptdet,avedep,maxdep,ptmax,ptmin,dstot,stdist,irdgdx)
 
!      call sedseg(dslost,sumfile,iyear,noout,dstot,stdist,irdgdx,ysdist,&
!     &     avgslp,slplen,y,avedet,maxdet,ptdet,avedep,maxdep,ptdep) 
!     
!      write (sumfile,1100)
!      write (sumfile,1200)
!      write (sumfile,1300)
!      write (sumfile,1400)
!      write (sumfile,1500)
!     
      m = ncol22 - 1
!     
      do 30 i = 1, m
!        
         j = 0
!        
         do 20 k = 1, jend
!           
            j = j + 1
            if (k.eq.ncol11) then
               x1 = dstot(k)
               ss1 = stdist(k)
               nelem1 = nfelem(j)
            else if (k.eq.ncol22) then
               x2 = dstot(k)
               ss2 = stdist(k)
               nelem2 = nfelem(j)
            else if (k.eq.ncol33) then
               x3 = dstot(k)
               ss3 = stdist(k)
               nelem3 = nfelem(j)
            end if
!        
   20    continue
!        
         if (i.lt.ncol44) then
!            write (sumfile,1000) ss1, x1, nelem1, ss2, x2, nelem2, ss3, &
!     &			x3, nelem3
         else
!            write (sumfile,1005) ss1, x1, nelem1, ss2, x2, nelem2
         end if
      
         ncol11 = ncol11 + 1
         ncol22 = ncol22 + 1
         ncol33 = ncol33 + 1
!     
   30 continue
!     
!      write (sumfile,1600)
!     
!      write (sumfile,1700)
!     
!     write to the output file the sediment leaving the profile
!     in kg/m, in kg/m^2, and in tonnes/ha.
!     
!      write (sumfile,1800) avsole
!      write (sumfile,1900) avsole * fwidth, units(10), fwidth, units(9)
!      write (sumfile,2000) (avsole*fwidth*0.001) / (marea*0.0001),      &
!     &    units(11), marea / 10000.0, units(12)
!     
!      write (sumfile,2100)
      
!      call enrprt(sumfile,npart,frac,frcflw,dia,spg,                    &
!     &         frsnd,frslt, frcly,frorg,enrato)
    
 1000 format (3(f7.2,1x,f9.3,1x,i3,5x))
 1005 format (2(f7.2,1x,f9.3,1x,i3,5x))
 1100 format (///2x,'c.  soil loss/deposition along slope profile',//,10&
     &    x,'profile distances are from top to bottom of hillslope'//)
 1200 format (1x,'distance',1x,'soil',2x,'flow',4x,'distance',3x,'soil',&
     &    2x,'flow',4x,'distance',3x,'soil',2x,'flow')
 1300 format (4x,'(m)',3x,'loss',2x,'elem',7x,'(m)',5x,'loss',2x,'elem',&
     &    7x,'(m)',5x,'loss',2x,'elem')
 1400 format (9x,'(kg/m2)',18x,'(kg/m2)',18x,'(kg/m2)')
 1500 format (72('-'),/)
 1600 format (/'note:  (+) soil loss - detachment     (-) soil loss',   &
     &    ' - deposition')
 1700 format (///'iii. off site effects  off site effects',             &
     &    '  off site effects',/,5x,(3(16('-'),2x))/)
 1800 format (5x,'a.  sediment leaving profile ',//,9x,f9.3,' kg/m'/)
 1900 format (9x,f12.3,a,' (based on profile width of  ',f9.3,a,')')
 2000 format (9x,f12.3,a,' (assuming contributions from ',f9.3,a,')')
 2100 format (//5x,'b.  sediment characteristics and enrichment')
 
 
!     
      end
