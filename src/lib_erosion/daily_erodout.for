!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine daily_erodout (o_unit, o_E_unit)

!     +++  PURPOSE +++

!     To print output desired from standalone EROSION submodel

!     +++ ARGUMENT DECLARATIONS +++

      integer o_unit, o_E_unit

      integer i, j
      real aegt, aegtss, aegt10
      real tt, lx, ly
      real topt,topss, top10, bott, botss, bot10, ritt, ritss, rit10
      real lftt, lftss, lft10, tot, totbnd

      integer initflag
      save    initflag
      integer yr, mo, da

!     + + + GLOBAL COMMON BLOCKS + + +
      include 'p1werm.inc'
      include 'm1geo.inc'
      include 'm1flag.inc'

!     + + + LOCAL COMMON BLOCKS
      include 'erosion/e2erod.inc'
      include 'erosion/m2geo.inc'
!
      integer x, y


      integer :: dt(8)
      character(len=3) :: mstring
      common / datetime / dt, mstring

!     ++++ ARGUMENT DEFINITIONS +++

!     +++ SUBROUTINES CALLED+++
!     plotout.for

!     ++++ LOCAL VARIABLES +++

!     +++ END SPECIFICATIONS +++


!     Calculate Averages Crossing Borders
!      top border
       aegt   = 0.0
       aegtss = 0.0
       aegt10 = 0.0
       j = jmax
       do 1 i = 1, imax-1
         aegt    = aegt   + egt(i,j)
         aegtss  = aegtss + egtss(i,j)
         aegt10  = aegt10 + egt10(i,j)
    1  continue
!      calc. average at top border
       topt  = aegt/(imax-1)
       topss = aegtss/(imax-1)
       top10 = aegt10/(imax-1)

!      bottom border
       aegt   = 0.0
       aegtss = 0.0
       aegt10 = 0.0
       j = 0
       do 2 i = 1, imax-1
         aegt    = aegt   + egt(i,j)
         aegtss  = aegtss + egtss(i,j)
         aegt10  = aegt10 + egt10(i,j)
    2  continue
!      calc. average at bottom border
        bott  = aegt/(imax-1)
        botss = aegtss/(imax-1)
        bot10 = aegt10/(imax-1)

!     right border
       aegt   = 0.0
       aegtss = 0.0
       aegt10 = 0.0
       i = imax
       do 3 j = 1, jmax-1
         aegt    = aegt   + egt(i,j)
         aegtss  = aegtss + egtss(i,j)
         aegt10  = aegt10 + egt10(i,j)
    3  continue
!      calc. average at right border
        ritt  = aegt/(jmax-1)
        ritss = aegtss/(jmax-1)
        rit10 = aegt10/(jmax-1)
!
!     left border
       aegt   = 0.0
       aegtss = 0.0
       aegt10 = 0.0
       i = 0
       do 4 j = 1, jmax-1
         aegt    = aegt   + egt(i,j)
         aegtss  = aegtss + egtss(i,j)
         aegt10  = aegt10 + egt10(i,j)
    4  continue
!      calc. average at left border
        lftt   = aegt/(jmax-1)
        lftss  = aegtss/(jmax-1)
        lft10  = aegt10/(jmax-1)

!     calculate averages of inner grid points
      aegt   = 0.0
      aegtss = 0.0
      aegt10 = 0.0
      do 5 j=1,jmax-1
       do 5 i= 1, imax-1
        aegt= aegt + egt(i,j)
        aegtss = aegtss + egtss(i,j)
        aegt10 = aegt10 + egt10(i,j)
    5 continue
      tt     = (imax-1)*(jmax-1)
      aegt   = aegt/tt
      aegtss = aegtss/tt
      aegt10 = aegt10/tt

!    calculate comparision of boundary and interior losses
      lx = amxsim(1,2) - amxsim(1,1)
      ly = amxsim(2,2) - amxsim(2,1)
      tot = aegt*lx*ly
      totbnd = (topt + bott + topss + botss)*lx +                       &
     &         (ritt + lftt + ritss + lftss)*ly


      if (btest(am0efl,1)) then
        if (initflag .eq. 0) then  !write header to files

            write (o_unit,*) 'Grid cell output from daily_erodout.for'
            write (o_unit,*)

           ! Print date of Run
12          format(1x,'Date of run: ',a3,' ',i2.2,', ',i4,' ',          &
     &          i2.2,':',i2.2,':',i2.2)
            write (o_unit,12) mstring, dt(3), dt(1), dt(5), dt(6), dt(7) 
            write (o_unit,*)

            write (o_unit,fmt="(1x,a,5f10.2)") "<field dimensions>",    &
     &                   amasim,((amxsim(x,y),x=1,2),y=1,2)
            write (o_unit,*)
            write (o_unit,*)                                            &
     &                 'Total grid size: (', imax+1,',', jmax+1, ')   ',&
     &                 'Inner grid size: (', imax-1,',', jmax-1, ')'
            write (o_unit,*)
            initflag = initflag + 1
        endif

        call caldatw (da, mo, yr) !get day, month and year
        write (UNIT=o_unit,FMT="(' ',i5,i3,i3,' ')",ADVANCE="YES")      &
     &         yr, mo, da
        write (o_unit,*)

      write (o_unit,*)                                                  &
     & '  top(i=1,imax-1,j=jmax) ',                                     &
     & 'bottom(i=1,imax-1,j=0) ',                                       &
     & 'right(i=imax,j=1,jmax-1) ',                                     &
     & 'left(i=0, j=1,jmax-1) '
      write (o_unit,10)  (egt(i,jmax)+egtss(i,jmax), i = 1, imax-1)
      write (o_unit,10)  (egt(i,0)+egtss(i,0), i = 1, imax-1)
      write (o_unit,10)  (egt(imax,j)+egtss(imax,j), j = 1, jmax-1)
      write (o_unit,10)  (egt(0,j)+egtss(0,j), j = 1, jmax-1)

      write (o_unit,*)
      write (o_unit,7)
      write (o_unit,*)                                                  &
     & '  top(i=1,imax-1,j=jmax) ',                                     &
     & 'bottom(i=1,imax-1,j=0) ',                                       &
     & 'right(i=imax,j=1,jmax-1) ',                                     &
     & 'left(i=0, j=1,jmax-1) '
      write (o_unit,10)  (egt(i,jmax), i = 1, imax-1)
      write (o_unit,10)  (egt(i,0), i = 1, imax-1)
      write (o_unit,10)  (egt(imax,j), j = 1, jmax-1)
      write (o_unit,10)  (egt(0,j), j = 1, jmax-1)

      write (o_unit,*)
      write (o_unit,8)
      write (o_unit,*)                                                  &
     & '  top(i=1,imax-1,j=jmax) ',                                     &
     & 'bottom(i=1,imax-1,j=0) ',                                       &
     & 'right(i=imax,j=1,jmax-1) ',                                     &
     & 'left(i=0, j=1,jmax-1) '
      write (o_unit,10)  (egtss(i,jmax), i = 1, imax-1)
      write (o_unit,10)  (egtss(i,0), i = 1, imax-1)
      write (o_unit,10)  (egtss(imax,j), j = 1, jmax-1)
      write (o_unit,10)  (egtss(0,j), j = 1, jmax-1)

      write (o_unit,*)
      write (o_unit,9)
      write (o_unit,*)                                                  &
     & '  top(i=1,imax-1,j=jmax) ',                                     &
     & 'bottom(i=1,imax-1,j=0) ',                                       &
     & 'right(i=imax,j=1,jmax-1) ',                                     &
     & 'left(i=0,j=1,jmax-1) '
      write (o_unit,11)  (egt10(i,jmax), i = 1, imax-1)
      write (o_unit,11)  (egt10(i,0), i = 1, imax-1)
      write (o_unit,11)  (egt10(imax,j), j = 1, jmax-1)
      write (o_unit,11)  (egt10(0,j), j = 1, jmax-1)

      write (o_unit,*)
      write (o_unit,fmt="(' <grid data> | ',3('|',a))")                 &
     & 'Total Soil Loss', 'soil loss', '(kg/m^2)'
      do 19  j = jmax-1, 1, -1
      write (o_unit,10)  (egt(i,j), i = 1, imax-1)
   19 continue
      write (o_unit,fmt="(' </grid data>')")

      write (o_unit,*)
      write (o_unit,fmt="(' <grid data> | ',3('|',a))")                 &                              
     & 'Saltation/Creep Soil Loss', 'salt/creep soil loss', '(kg/m^2)'
      do 29  j = jmax-1, 1, -1
      write (o_unit,10)  (egt(i,j)-egtss(i,j), i = 1, imax-1)
   29 continue
      write (o_unit,fmt="(' </grid data>')")

      write (o_unit,*)
      write (o_unit,fmt="(' <grid data> | ',3('|',a))")                 &                              
     & 'Suspension Soil Loss', 'suspension soil loss', '(kg/m^2)'
      do 39  j = jmax-1, 1, -1
      write (o_unit,10)  (egtss(i,j), i = 1, imax-1)
   39 continue
      write (o_unit,fmt="(' </grid data>')")

      write (o_unit,*)
      write (o_unit,fmt="(' <grid data> | ',3('|',a))")                 &
     & 'PM10 Soil Loss', 'PM10 soil loss', '(kg/m^2)'
      do 49  j = jmax-1, 1, -1
      write (o_unit,11)  (egt10(i,j), i = 1, imax-1)
   49 continue
      write (o_unit,fmt="(' </grid data>')")

      write (o_unit,*)
      write (o_unit,*) '**Averages - Field'
      write (o_unit,*) '     Total    salt/creep      susp       PM10 '
      write (o_unit,*) '     egt                      egtss      egt10'
      write (o_unit,*) '   -----------------kg/m^2--------------------'
      write (o_unit,15)    aegt, aegt-aegtss, aegtss, aegt10
      write (o_unit,*)
      write (o_unit,*) '**Averages - Crossing Boundaries '
      write (o_unit,*) 'Location      Total  Salt/Creep   Susp    PM10'
      write (o_unit,*) '--------------------kg/m----------------------'
      write (o_unit,21) topt+topss, topt, topss, top10
      write (o_unit,22) bott+botss, bott, botss, bot10
      write (o_unit,23) ritt+ritss, ritt, ritss, rit10
      write (o_unit,24) lftt+lftss, lftt, lftss, lft10
      write (o_unit,*)
      write (o_unit,*) '   Comparision of interior & boundary loss'
      write (o_unit,*) '      interior       boundary    int/bnd ratio'
      if( totbnd.gt.1.0e-9 ) then
          write (o_unit,16) tot, totbnd, tot/totbnd
      else
          !Boundary loss near or equal to zero
          write (o_unit,16) tot, totbnd, 1.0e-9
      end if

!^^^tmp out
!      write (o_unit,*) 'lx=', lx, 'ly=', ly,'tot',tot
!^^^end tmpout

!     additional output statements for easy shell script parsing
      write (o_unit,*)
!     write deposition as positive numbers
      write (o_unit,17) aegt, aegt-aegtss, aegtss, aegt10
   17 format (' repeat of total, salt/creep, susp, PM10:', 3f12.4,f12.6)

!     output formats

    6 format (1x,'  Passing Border Grid Cells - Total  egt+egtss(kg/m)')
    7 format (1x,'  Passing Border Grid Cells - Salt/Creep   egt(kg/m)')
    8 format (1x,'  Passing Border Grid Cells - Suspension egtss(kg/m)')
    9 format (1x,'  Passing Border Grid Cells - PM10       egt10(kg/m)')

   50 format (1x,'  Leaving Field Grid Cells - Total       egt(kg/m^2)')
   60 format (1x,'  Leaving Field Grid Cells - Salt/Creep egt-egtss(kg/m&
     &^2)')
   70 format (1x,'  Leaving Field Grid Cells - Suspension egtss(kg/m^2) &
     &')
   80 format (1x,'  Leaving Field Grid Cells - PM10      egt10(kg/m^2)')

   10 format (1x, 500f12.4)
   11 format (1x, 500f12.6)
   15 format (1x, 3(f12.4,2x), f12.6)
   16 format (1x, 2(f13.4,2x),2x, f13.4)
   21 format (1x, 'top   ', 1x, 4(f9.2,1x))
   22 format (1x, 'bottom', 1x, 4(f9.2,1x))
   23 format (1x, 'right ', 1x, 4(f9.2,1x))
   24 format (1x, 'left  ', 1x, 4(f9.2,1x))

      endif !if (btest(am0efl,1)) then

      !Erosion summary - total, salt/creep, susp, pm10
      !(deposition values are positive - erosion values are negative)
      if (btest(am0efl,0)) then
         call caldatw (da, mo, yr) !get day, month and year
         write (UNIT=o_E_unit,FMT="(' ',i5,i3,i3,' ')",ADVANCE="NO")    &
     &         yr, mo, da
         write (UNIT=o_E_unit,FMT="(4(f12.6),' ')",ADVANCE="YES")       &
     &          aegt, (aegt-aegtss), aegtss, aegt10
      endif

!    end of plot section

      return
      end
