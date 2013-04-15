!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine erodout (o_unit, o_E_unit, sgrd_u, input_filename, hagen_plot_flag, cellstate)

!     +++  PURPOSE +++
!     To print output desired from standalone EROSION submodel

      use erosion_data_struct_defs, only: cellsurfacestate, am0efl
      use grid_geo_def, only: imax, jmax, amasim, amxsim

!     +++ ARGUMENT DECLARATIONS +++
      integer o_unit, o_E_unit, sgrd_u
      character*1024 input_filename
      logical hagen_plot_flag
      type(cellsurfacestate), dimension(0:,0:), intent(out) :: cellstate     ! initialized grid cell state values

!     ++++ LOCAL VARIABLES +++
      integer i, j
      real aegt, aegtss, aegt10
      real tt, lx, ly
      real topt,topss, top10, bott, botss, bot10, ritt, ritss, rit10
      real lftt, lftss, lft10, tot, totbnd

      character*12 ycharin(30)
      integer yplot
      real yin(30)

!     ++++ LOCAL VARIABLES +++
      integer xplot
      character*12 xcharin(30)
      real xin(30)
      common/plot/xplot, xcharin, xin

      integer :: dt(8)
      character(len=3) :: mstring
      common / datetime / dt, mstring

      save :: /plot/, /datetime/

!     +++ SUBROUTINES CALLED+++
!     plotout.for

!     +++ END SPECIFICATIONS +++


!     Calculate Averages Crossing Borders
!      top border
       aegt   = 0.0
       aegtss = 0.0
       aegt10 = 0.0
       j = jmax
       do 1 i = 1, imax-1
         aegt    = aegt   + cellstate(i,j)%egt
         aegtss  = aegtss + cellstate(i,j)%egtss
         aegt10  = aegt10 + cellstate(i,j)%egt10
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
         aegt    = aegt   + cellstate(i,j)%egt
         aegtss  = aegtss + cellstate(i,j)%egtss
         aegt10  = aegt10 + cellstate(i,j)%egt10
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
         aegt    = aegt   + cellstate(i,j)%egt
         aegtss  = aegtss + cellstate(i,j)%egtss
         aegt10  = aegt10 + cellstate(i,j)%egt10
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
         aegt    = aegt   + cellstate(i,j)%egt
         aegtss  = aegtss + cellstate(i,j)%egtss
         aegt10  = aegt10 + cellstate(i,j)%egt10
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
        aegt= aegt + cellstate(i,j)%egt
        aegtss = aegtss + cellstate(i,j)%egtss
        aegt10 = aegt10 + cellstate(i,j)%egt10
    5 continue
      tt     = (imax-1)*(jmax-1)
      aegt   = aegt/tt
      aegtss = aegtss/tt
      aegt10 = aegt10/tt

!    calculate comparision of boundary and interior losses
      lx = amxsim(2)%x - amxsim(1)%x
      ly = amxsim(2)%y - amxsim(1)%y
      tot = aegt*lx*ly
      totbnd = (topt + bott + topss + botss)*lx +                       &
     &         (ritt + lftt + ritss + lftss)*ly


      if (btest(am0efl,1)) then
!     write header to files
      write (o_unit,*)
      write (o_unit,*)
      write (o_unit,*) 'OUTPUT FROM ERODOUT.FOR '
      write (o_unit,*)

      ! Print date of Run
12    format(1x,'Date of run: ',a3,' ',i2.2,', ',i4,' ',                &
     &          i2.2,':',i2.2,':',i2.2)
      write(o_unit,12) mstring, dt(3), dt(1), dt(5), dt(6), dt(7) 
      write(o_unit,*)

      write(o_unit,fmt="(1x,a)") "<field dimensions>"
      write(o_unit,fmt="(1x,5f10.2)") amasim, amxsim(1)%x, amxsim(1)%y, amxsim(2)%x, amxsim(2)%y
      write(o_unit,fmt="(1x,a)") "</field dimensions>"
      write(o_unit,*)
      write (o_unit,*) 'Total grid size: (', imax+1,',', jmax+1, ')   ',&
     &                 'Inner grid size: (', imax-1,',', jmax-1, ')'


      write (o_unit,*)
      write (o_unit,6)
      write (o_unit,*)                                                  &
     & '  top(i=1,imax-1,j=jmax) ',                                     &
     & 'bottom(i=1,imax-1,j=0) ',                                       &
     & 'right(i=imax,j=1,jmax-1) ',                                     &
     & 'left(i=0, j=1,jmax-1) '
      write (o_unit,10)  (cellstate(i,jmax)%egt+cellstate(i,jmax)%egtss, i = 1, imax-1)
      write (o_unit,10)  (cellstate(i,0)%egt+cellstate(i,0)%egtss, i = 1, imax-1)
      write (o_unit,10)  (cellstate(imax,j)%egt+cellstate(imax,j)%egtss, j = 1, jmax-1)
      write (o_unit,10)  (cellstate(0,j)%egt+cellstate(0,j)%egtss, j = 1, jmax-1)

      write (o_unit,*)
      write (o_unit,7)
      write (o_unit,*)                                                  &
     & '  top(i=1,imax-1,j=jmax) ',                                     &
     & 'bottom(i=1,imax-1,j=0) ',                                       &
     & 'right(i=imax,j=1,jmax-1) ',                                     &
     & 'left(i=0, j=1,jmax-1) '
      write (o_unit,10)  (cellstate(i,jmax)%egt, i = 1, imax-1)
      write (o_unit,10)  (cellstate(i,0)%egt, i = 1, imax-1)
      write (o_unit,10)  (cellstate(imax,j)%egt, j = 1, jmax-1)
      write (o_unit,10)  (cellstate(0,j)%egt, j = 1, jmax-1)

      write (o_unit,*)
      write (o_unit,8)
      write (o_unit,*)                                                  &
     & '  top(i=1,imax-1,j=jmax) ',                                     &
     & 'bottom(i=1,imax-1,j=0) ',                                       &
     & 'right(i=imax,j=1,jmax-1) ',                                     &
     & 'left(i=0, j=1,jmax-1) '
      write (o_unit,10)  (cellstate(i,jmax)%egtss, i = 1, imax-1)
      write (o_unit,10)  (cellstate(i,0)%egtss, i = 1, imax-1)
      write (o_unit,10)  (cellstate(imax,j)%egtss, j = 1, jmax-1)
      write (o_unit,10)  (cellstate(0,j)%egtss, j = 1, jmax-1)

      write (o_unit,*)
      write (o_unit,9)
      write (o_unit,*)                                                  &
     & '  top(i=1,imax-1,j=jmax) ',                                     &
     & 'bottom(i=1,imax-1,j=0) ',                                       &
     & 'right(i=imax,j=1,jmax-1) ',                                     &
     & 'left(i=0,j=1,jmax-1) '
      write (o_unit,11)  (cellstate(i,jmax)%egt10, i = 1, imax-1)
      write (o_unit,11)  (cellstate(i,0)%egt10, i = 1, imax-1)
      write (o_unit,11)  (cellstate(imax,j)%egt10, j = 1, jmax-1)
      write (o_unit,11)  (cellstate(0,j)%egt10, j = 1, jmax-1)

      write (o_unit,*)
      write (o_unit,fmt="(' <grid data> | ',3('|',a))")                 &                              
     & 'Total Soil Loss', 'soil loss', '(kg/m^2)'
      do 19  j = jmax-1, 1, -1
      write (o_unit,10)  (cellstate(i,j)%egt, i = 1, imax-1)
   19 continue
      write (o_unit,fmt="(' </grid data>')")

      write (o_unit,*)
      write (o_unit,fmt="(' <grid data> | ',3('|',a))")                 &                              
     & 'Saltation/Creep Soil Loss', 'salt/creep soil loss', '(kg/m^2)'
      do 29  j = jmax-1, 1, -1
      write (o_unit,10)  (cellstate(i,j)%egt-cellstate(i,j)%egtss, i = 1, imax-1)
   29 continue
      write (o_unit,fmt="(' </grid data>')")

      write (o_unit,*)
      write (o_unit,fmt="(' <grid data> | ',3('|',a))")                 &                              
     & 'Suspension Soil Loss', 'suspension soil loss', '(kg/m^2)'
      do 39  j = jmax-1, 1, -1
      write (o_unit,10)  (cellstate(i,j)%egtss, i = 1, imax-1)
   39 continue
      write (o_unit,fmt="(' </grid data>')")

      write (o_unit,*)
      write (o_unit,fmt="(' <grid data> | ',3('|',a))")                 &
     & 'PM10 Soil Loss', 'PM10 soil loss', '(kg/m^2)'
      do 49  j = jmax-1, 1, -1
      write (o_unit,11)  (cellstate(i,j)%egt10, i = 1, imax-1)
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
!     write losses as positive numbers
      write (o_unit,17) -aegt, aegtss-aegt, -aegtss, -aegt10
   17 format (' repeat of total, salt/creep, susp, PM10:', 3f12.4,f12.6)

!     output formats

    6 format (1x,'  Passing Border Grid Cells - Total  egt+egtss(kg/m)')
    7 format (1x,'  Passing Border Grid Cells - Salt/Creep   egt(kg/m)')
    8 format (1x,'  Passing Border Grid Cells - Suspension egtss(kg/m)')
    9 format (1x,'  Passing Border Grid Cells - PM10       egt10(kg/m)')

!   50 format (1x,'  Leaving Field Grid Cells - Total       egt(kg/m^2)')
!   60 format (1x,'  Leaving Field Grid Cells - Salt/Creep egt-egtss(kg/m^2)')
!   70 format (1x,'  Leaving Field Grid Cells - Suspension egtss(kg/m^2)')
!   80 format (1x,'  Leaving Field Grid Cells - PM10      egt10(kg/m^2)')

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
      !(loss values are positive - deposition values are negative)
      if (btest(am0efl,0)) then
         write (UNIT=o_E_unit,FMT="(4(f12.6),' ')",ADVANCE="NO")        &
     &          -aegt, -(aegt-aegtss), -aegtss, -aegt10
         write (UNIT=o_E_unit,FMT="(A)",ADVANCE="YES")                  &
     &         trim(input_filename)
      endif
      !Duplicate Erosion summary info for the *.sgrd file so "tsterode" interface
      ! can display this info on graphical report window
!      write(0,*) "Before btest(am0efl,3) test", am0efl, btest(am0efl,3)
!      if (btest(am0efl,3)) then
!      write(0,*) "In print section"
!         write (UNIT=sgrd_u,FMT="(4(f12.6),' ')",ADVANCE="NO")     &
!     &          -aegt, -(aegt-aegtss), -aegtss, -aegt10
!         write (UNIT=sgrd_u,FMT="(A)",ADVANCE="YES")               &
!     &         trim(input_filename)
!      endif
      if (btest(am0efl,3)) then
       write (sgrd_u,*)
       write (sgrd_u,*) '**Averages - Field'
       write (sgrd_u,*) '     Total    salt/creep      susp       PM10 '
       write (sgrd_u,*) '     egt                      egtss      egt10'
       write (sgrd_u,*) '   -----------------kg/m^2--------------------'
       write (sgrd_u,15)    aegt, aegt-aegtss, aegtss, aegt10
       write (sgrd_u,*)
       write (sgrd_u,*) '**Averages - Crossing Boundaries '
       write (sgrd_u,*) 'Location      Total  Salt/Creep   Susp    PM10'
       write (sgrd_u,*) '--------------------kg/m----------------------'
       write (sgrd_u,21) topt+topss, topt, topss, top10
       write (sgrd_u,22) bott+botss, bott, botss, bot10
       write (sgrd_u,23) ritt+ritss, ritt, ritss, rit10
       write (sgrd_u,24) lftt+lftss, lftt, lftss, lft10
       write (sgrd_u,*)
       write (sgrd_u,*) '   Comparision of interior & boundary loss'
       write (sgrd_u,*) '      interior       boundary    int/bnd ratio'
       if( totbnd.gt.1.0e-9 ) then
         write (sgrd_u,16) tot, totbnd, tot/totbnd
       else
         !Boundary loss near or equal to zero
         write (o_unit,16) tot, totbnd, 1.0e-9
       end if
      end if

!    test if plot info wanted
      if (hagen_plot_flag .EQV. .true.) then
!
!    test if plot info available from input files
!    (should allow one to mix input files and get only
!     wanted plot output)
       if (xplot > -1) then
!    specify plotout dep variables for all values of yplot
          yplot = 4
          if (yplot .gt. 0) then
          ycharin(1) = 'total_eros'
          ycharin(2) = 'salt/creep'
          ycharin(3) = 'suspension'
          ycharin(4) = 'PM10(kg/m^2)'
          yin(1) = aegt
          yin(2) = aegt-aegtss
          yin(3) = aegtss
          yin(4) = aegt10
          endif

        call plotout (yplot, ycharin, yin)
        endif
      endif
!    end of plot section

      return
      end
