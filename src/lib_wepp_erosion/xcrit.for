      subroutine xcrit(a,b,c,tauc,xb,xe,xc1,xc2,mshear)
      
      use wepp_interface_defs
      
      implicit none
!
!     subroutine xcrit determines whether shear stress exceeds 
!     critical shear stress for a certain segment and returns a flag
!
!     module adapted from wepp version 2004.7 and called from 
!     subroutine route
!
!     author(s): d.c. flanagan and j.c. ascough ii
!     date last modified: 9-30-2004
!
!     + + + argument declarations + + +
!
      real, intent(in) :: a, b, c, tauc, xb, xe
      integer, intent(out) :: mshear
      real, intent(out) :: xc1,xc2

!     + + + argument definitions + + +
!
!     a     - shear stress equation coefficient
!     b     - shear stress equation coefficient
!     c     - shear stress equation coefficient
!     tauc  - n.d. critical shear for the overland flow element
!     xb    - beginning distance for segment of ofe
!     xe    - ending distance for segment of ofe
!     xc1   - 1st point where shear = critical shear (if one exists)
!     xc2   - 2nd point where shear = critical shear (if one exists)
!     mshear- flag indicating what shear conditions exist on segment
!
!     mshear |      meaning
!     ----------------------------------------------------------------
!       1    |  shear less than critical throughout segment
!       2    |  shear greater than critical throughout segment
!       3    |  shear equal critical shear within a segment
!            |      upslope  shear < critical shear
!            |      downslope shear > critical shear
!       4    |  shear equal critical shear within a segment
!            |      shear decreasing within segment
!            |      upslope  shear > critical shear
!            |      downslope shear < critical shear
!       5    |  shear equal critical shear two places in a segment
!            |      shear increases then decreases
!     ----------------------------------------------------------------
!
!     + + + local variables + + +
!
      double precision tauchk, x1, x2
      real taub,taue,part
!
!     + + + local variable definitions + + +
!
!     tauchk - partial solution to quadratic equation used here
!     taub   - n.d. shear stress calculated at the beg. of segment
!     taue   - n.d. shear stress calculated at the end of the segment
!     part   - partial solution to quadratic equation used here
!
!     + + + subroutines called + + +
!
!     root
!
!     + + + function declarations + + +
!
!      real shear
!
!     begin subroutine xcrit
!
      tauchk = tauc ** 1.5 - c
      taub = shear(a,b,c,xb)
      taue = shear(a,b,c,xe)
!     
      if (a.eq.0.0) then
!        
!        uniform slope segment - determine location where critical 
!        shear stress is exceeded
!        
         if (b.ne.0.0) then
            xc1 = tauchk / b
         else
            xc1 = 1000.
         end if
!        
         if (taue.gt.taub) then
            mshear = 3
            if (xc1.le.xb) mshear = 2
            if (xc1.ge.xe) mshear = 1
         else
            mshear = 4
            if (xc1.ge.xe) mshear = 2
            if (xc1.le.xb) mshear = 1
         end if
!     
      else if (a.gt.0.0.and.taue.gt.taub) then
!        
!        convex segment on an plane on which shear increases downslope
!        
!        if shear stress at the beginning of the convex segment 
!        exceeds critical, then the entire segment exceeds critical 
!        shear stress
!        
         if (taub.ge.tauc) then
            mshear = 2
         else
!           
!           if shear stress at the end of the convex segment is less
!           than critical, then the entire segment is below critical 
!           shear stress
!           
            if (taue.le.tauc) then
               mshear = 1
            else
               call root(a,b,tauchk,x1,x2)
               mshear = 3
!              
!              determine the point where shear stress exceeds
!              critical shear stress
!              
               if (x1.ge.xb.and.x1.le.xe) then
                  xc1 = x1
               else
                  if (x2.ge.xb.and.x2.le.xe) xc1 = x2
               end if
            end if
         end if
!     
      else
!        
!        any other type of segment:
!        1) convex with shear decreasing down segment
!        2) concave with shear increasing or decreasing down segment
!        3) uniform with shear increasing or decreasing down segment
!        
         if (taue.ge.tauc.and.taub.ge.tauc) then
!           
!           if shear stress exceeds critical at both ends of the 
!           segment, it exceeds critical all along the segment
!           
            mshear = 2
!        
         else
!           
!           determine if shear exceeds critical shear somewhere
!           along the segment
!           
            part = b ** 2 + 4.0 * a * tauchk
!           
!           if solution of quadratic equation has no real roots, then
!           critical shear stress is not exceeded along entire segment
!           
            if (part.le.0.0) then
               mshear = 1
            else
!              
!              determine where shear stress equals critical shear
!              stress, by solving for roots of quadratic equation
!              
               call root(a,b,tauchk,x1,x2)
!              
!              if shear stress increases on segment, set mshear 
!              flag = 3 and return location where shear = critical
!              
               if (taub.le.tauc.and.taue.ge.tauc) then
                  mshear = 3
                  if (x1.le.xb.or.x1.ge.xe) then
                     xc1 = x2
                  else
                     xc1 = x1
                  end if
               else
!                 
!                 if shear stress decreases on segment, set mshear 
!                 flag = 4 and return location where shear = critical
!                 
                  if (taub.ge.tauc.and.taue.le.tauc) then
                     mshear = 4
                     if (x1.le.xb.or.x1.ge.xe) then
                        xc1 = x2
                     else
                        xc1 = x1
                     end if
                  else
!                    
!                    if shear at both top and bottom of segment is 
!                    below critical, return two locations where it 
!                    equals critical
!                    
                     if (taub.le.tauc.and.taue.le.tauc) then
                        mshear = 5
                        xc1 = x1
                        xc2 = x2
!                       
!                       check to make sure that for a case 4, both 
!                       points fall between xb and xe, and that they 
!                       are not the same point
!                       
                        if (x1.lt.xb.or.x1.gt.xe.or.x2.lt.xb.or.x2.gt.xe&
     &                      .or.x1.eq.x2) mshear = 1
                     end if     ! if (taub.le.tauc.and.taue.le.tauc) the
!                 
                  end if        ! if (taub.ge.tauc.and.taue.le.tauc) the
!              
               end if   ! if (taub.le.tauc.and.taue.ge.tauc) then
!           
            end if      ! if (part.le.0.0) then
!        
         end if ! if (taue.ge.tauc.and.taub.ge.tauc) then
!     
      end if    ! if (a.eq.0.0) then 
!     
      return
      end
