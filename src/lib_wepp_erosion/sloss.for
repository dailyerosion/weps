!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine sloss(load,tcend,width,rspace,effdrn,theta,            &
     &    slplen,irdgdx,qsout,dslost,dsmon,dsyear,dsavg,avsole,qout,    &
     &    frcflw,npart,enrato)
     
      use wepp_interface_defs
      
      implicit none

      include 'wepp_erosion.inc'
!
!     + + + purpose + + +
!     calculates sediment loss (dg/dx) at points, sediment yield,
!     and sediment concentration on a storm by storm basis.
!     passes results back to main program, to be the output in
!     subsequent routines (sedout, sedseg).
!
!     called from subroutine main
!     author(s): d. flanagan, j. ascough
!
!     version: this module modified from wepp v2004.7 code
!     date last modified:  4-1-2005
!
!     + + + argument declarations + + +
!
      real, intent(in) :: load(101), tcend, width, rspace, effdrn
      real, intent(in) :: theta, slplen, frcflw(mxpart)
      real, intent(in) :: qout,enrato
	  real, intent(out) :: dslost(100)
	  real, intent(out) :: avsole, irdgdx, qsout
	  real, intent(inout) :: dsmon(100), dsyear(100), dsavg(100)
	  integer, intent(in) :: npart
!     
!
!     load   - nondimensional sediment load at points down slope
!     tcend  - sediment transport capacity at end of the average
!              uniform slope profile that passes through the
!     width  - rill width (m)
!     rspace - rill spacing (m)
!     effdrn - effective duration of runoff (s)
!     theta  - nondimensional interrill detachment parameter
!              endpoints of an ofe  (kg/s*m).
!     slplen - slope length of ofe (m)
!     peakro - peak runoff rate (m/s)
!     runoff - runoff depth (m)
!     efflen - effective length of runoff down profile (m)
!     frcflw - fraction of each particle type in flow
!     irdgdx - interrill contribution to dg/dx) in (kg/m^2)
!     qsout  - sediment discharge at end of current ofe (kg/s*m)
!     dslost - net soil loss/gain at each point on hillslope for
!              a storm event (time*dg/dx) - (kg/m**2)
!     avsole - storm sediment loss (kg/m)
!     enrato - enrichment ratio of specific surface area of sediment
!     qout   - flow discharge out ofe (m^3/s*m)
!
!
!     + + + local variables + + +
!
      integer i,j
      real dslod1, dslod2
!
!     real csedls(mxplan)
!
!     dslend - dimensional sediment load at ofe end - (kg/m) 
!     dslod1 - dimensional sediment load at point - (kg/m) 
!     dslod2 - dimensional sediment load at point - (kg/m)
!     csedls - sediment loss from contoured ofes in (kg)
!
!     + + + end specifications + + +
!
!      save
!
!     data csedls /75 * 0.0/
!
!
!     initially, do output only for a single storm - dcf 9-29-2004
!     imodel = 2
!
!     initialize load variables
!
!     dslend = 0.0
      dslod1 = 0.0
      dslod2 = 0.0
      do 10 j = 2, 101
         dslost(j-1) = 0.0
   10 continue
!     
!     calculate dimensional sediment load at beginning of ofe (x=0)
!     
      dslod1 = load(1) * effdrn * tcend * width / rspace
!     
  
      do 20 j = 2, 101
         dslod2 = load(j) * effdrn * tcend * width / rspace
         dslost(j-1) = (dslod2-dslod1) / (slplen*0.01)
         dslod1 = dslod2
   20 continue
!     
!     compute interrill contribution to sediment loss.
!     
      irdgdx = (theta*tcend*effdrn*width) / (rspace*slplen)

!   summing for monthly and annual dslost

      do 40 j = 2, 101
         dsmon(j-1) = dsmon(j-1) + dslost(j-1)
         dsyear(j-1) = dsyear(j-1) + dslost(j-1)
         dsavg(j-1) = dsavg(j-1) + dslost(j-1)
   40 continue
!        
         if (qout.gt.0.0) then
            avsole = dslod2
         else
            avsole = 0.0
         end if
!        
         if (avsole.lt.0.0) avsole = 0.0
!     

      wp_enrff1 = wp_enrff1 + (enrato*avsole)
      wp_enrff2 = wp_enrff2 + avsole
!     
      do 50 i = 1, npart
         wp_frcff1(i) = wp_frcff1(i) + (frcflw(i)*avsole)
         wp_frcff2(i) = wp_frcff2(i) + avsole
!    
  50  continue
!     
!     
      if (qout.gt.0.0) qsout = dslod2 / effdrn
!     
!     
      return
      end
