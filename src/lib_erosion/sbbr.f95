!$Author$
!$Date$
!$Revision$
!$HeadURL$
!**********************************************************************
!     subroutine sbbar       3/20/98
!**********************************************************************
      subroutine sbbr( cellstate )
! ^^^ note must add wlen to call for tst version
!     + + + PURPOSE + + +
!     to calculate the fraction of open field friction velocity
!     in shelter for 8 cardinal wind directions 0, 45, ...315
!     at all interior nodes

      use weps_interface_defs
      use erosion_data_struct_defs, only: cellsurfacestate
      use grid_geo_def, only: imax, jmax, ix, jy
      use p1unconv_mod, only: pi

!     to assign the 8 fractons calculated at each node to as 3-d
!     array (W0br(i,j,k)) for all nodes inside sim. region.

!     + + + KEYWORDS + + +
!     wlen

!     + + + PARAMETERS + + +

!     + + + ARGUMENT DECLARATIONS + + +
      type(cellsurfacestate), dimension(0:,0:), intent(inout) :: cellstate     ! initialized grid cell state values

!     + + + GLOBAL COMMON BLOCKS + + +
      include 'p1werm.inc'
      include 'm1geo.inc'   ! amasim, nbr, amxsim, amxbr, amzbr, ampbr

!     + + + ARGUMENT DECLARATIONS + + +
! ^^^ used only in test version
!      real wlen(0:imax, 0:jmax, 8)

!     + + + ARGUMENT DEFINITIONS  + + +
!     wlen = windward(-) and leeward(+) distances from barrier
!            calculated for each wind direction;
!  ^^^ used only in test version

!     + + + LOCAL VARIABLES + + +

      integer i, j, k, n, tmpv

      real lx, ly
      real awar(8), a, b, c, aa, ca, waa
      real alpha, ceta, dmin, w, tmpa

      real tmpw0

!     + + + LOCAL VARIABLE DEFINITIONS + + +

!     i, j, k, n = do-loop indices
!     a,c = lengths to barrier ends from cell x(i,j) (m)
!     b = barrier lenght (m)
!     aa, ca = angles opposite lenths a and c.
!     waa = angle between barrier and wind direction
!     alpha = clockwise angle from d (0 deg) to a
!     ceta  = clockwise angle from d (0 deg) to c
!     dmin = minimum distance from barrier to x(i,j)
!     w = length from barrier to x(i,j) along wind direction
!     tmpa = temporary (intermediate) angle calculation

!     + + + SUBROUTINES CALLED + + +
!     none

!     + + + FUNCTION DECLARATIONS + + +
      real fu, slen, ang

!     + + + DATA INITIALIZATIONS + + +

!     initialize variable (fraction of open field fric. vel)
      do 10 i = 0, imax
        do 10 j = 0, jmax
          do 10 k = 1, 8
            cellstate(i,j)%w0br(k) = 1.0
   10 continue

!     + + + END SPECIFICATIONS + + +

      ! calc. 8 cardinal wind directions relative to the simulation
      ! region using a relative y-axis orientation of 0 deg for region.

      do 20 i = 1,8
        awar(i) = (i-1)*45 - amasim
        if (awar(i) .lt. 0) then
          awar(i) = awar(i) + 360
        elseif (awar(i) .gt. 360) then
          awar(i) = awar(i) - 360
        endif

        ! convert relative wind angles to radians
        awar(i) = awar(i)*pi/180
   20 continue 

      ! update interior nodes
      do 90 i = 1, imax-1
        do 90 j = 1, jmax-1
          !*2   calculate distance to grid points(maybe offset from origin)
          lx = (i-0.5)*ix + amxsim(1,1)
          ly = (j-0.5)*jy + amxsim(2,1)
! ^^^tmp
!      if (i .eq. 1 .and. j .eq. jmax-1)then
!       write (*,*) 'sbbr output at 1, jmax-1 lx=', lx, 'ly=', ly
!       write (*,*) 'ix=',ix, 'jy=', jy, 'imax=', imax, 'jmax=',jmax
!      endif
! ^^^ end tmp
          ! barrier sweep
          do 70 n = 1, nbr
            ! find distances a, c from x(i,j) to barrier ends
            a = slen(lx, ly, amxbr(1,1,n), amxbr(2,1,n))
            c = slen(lx, ly, amxbr(1,2,n), amxbr(2,2,n))

            ! find barrier length
            b = slen(amxbr(1,1,n), amxbr(2,1,n), amxbr(1,2,n),          &
     &          amxbr(2,2,n))

            ! for a grid cell on a barrier calc. fric. vel. fraction
            if ((a+c) .le. b) then
              do 50 k =1,8
                w = 0
                tmpw0 = cellstate(i,j)%w0br(k)
                cellstate(i,j)%w0br(k) = fu( w, amzbr(n), ampbr(n), amxbrw(n) )
                cellstate(i,j)%w0br(k) = min( tmpw0, cellstate(i,j)%w0br(k) )
! ^^^ used only in test version
!        wlen(i,j,k) = 0

   50         continue
              go to 70
            endif

            ! find angles from x(i,j) to barrier ends
            tmpa = ((b*b+c*c-a*a)/(2*b*c))
            if (tmpa .lt. -1) then
              tmpa = -1
            elseif (tmpa .gt. 1) then
              tmpa = 1
            endif
            aa = acos(tmpa)
            tmpa = ((a*a+b*b-c*c)/(2*a*b))
            if (tmpa .lt. -1) then
              tmpa = -1
            elseif (tmpa .gt. 1) then
              tmpa = 1
            endif
            ca = acos(tmpa)

            ! find minimum distance to barrier
            if (aa .gt. 90*pi/180 .or. ca .gt. 90*pi/180) then
              dmin = min(a,c)
            else
              dmin = b*sin(aa)*sin(ca)/sin(aa+ca)
            endif

!     is x(i,j) within 35*barrier height of barrier?
! ^^^tmp
!      write (*,*) 'sbbr.for output'
!      write (*,*) 'dmin=', dmin, 'i=',i,'j=',j
! ^^^ end tmp

            if (dmin .le. 35*amzbr(n)) then
              ! Find angles alpa and ceta
              alpha = ang(amxbr(1,1,n),amxbr(2,1,n),lx,ly)
              ceta  = ang(amxbr(1,2,n),amxbr(2,2,n),lx,ly)
              ! sweep relative wind directions
              do 60 k =  1, 8
                ! if grid cell downwind from barrier make tmpv = 1
                tmpv = 0
                if (abs(alpha - ceta) .lt. pi) then
                  if((awar(k) .ge. min(alpha,ceta))                     &
     &              .and. (awar(k) .lt. max(alpha, ceta))) then
                    tmpv = 1
                  endif
                elseif (abs(alpha-ceta) .ge. pi) then
                  if(awar(k) .le. min(alpha,ceta)) then
                    tmpv = 1
                  endif
                  if((awar(k) .ge. max(alpha,ceta))                     &
     &              .and. (awar(k) .le. 2*pi)) then
                    tmpv = 1
                  endif
                endif

! ^^^tmp used for test only
!      if (i .eq. 34 .and. j .eq. 1) then
!      write(*,*) ' barrier tmp output'
!      write (*,*) 'a=',a, ' b=', b, ' c=',c
!       write(*,*) 'aa=', aa, ' ca=',ca
!       write(*,*) 'dmin=', dmin
!       write(*,*) 'alpha=', alpha, '  ceta=', ceta
!       write(*,*) 'tmpv=', tmpv
!       write(*,*) 'awar(K)=', awar(k), ' k=', k
!      endif
! ^^^ end tmp
!
                ! calc. opposite side 'w'
                if (tmpv .gt. 0) then
                  ! calc. barrier to x(i,j) distance 'w' along wind vector
                  ! calc. opposite side 'w'
                  tmpa = abs(awar(k) - alpha)
                  if (tmpa .gt. pi) then
                    tmpa = 2*pi - tmpa
                  endif
                  waa = pi- tmpa - ca
                  waa = max(waa, 0.0001)  !edit LH 3-27-07 prevent zero 
                  ! by sin rule calc w
                  w = sin(ca)*a/sin(waa) 

                  ! calc. min fraction unsheltered friction vel.
                  tmpw0 = cellstate(i,j)%w0br(k)
                  cellstate(i,j)%w0br(k) = fu(w, amzbr(n), ampbr(n), amxbrw(n))
                  cellstate(i,j)%w0br(k) = min(tmpw0, cellstate(i,j)%w0br(k))

! ^^^^  tmp used only in test version
!            wlen(i,j,k) = w

                elseif (dmin .lt. 5*amzbr(n)) then
                  ! is x(i,j) within 5H of barrier
                  ! calc. frac. friction vel. for other directions
                  tmpw0 = cellstate(i,j)%w0br(k)
                  cellstate(i,j)%w0br(k) = fu( -dmin, amzbr(n), ampbr(n), amxbrw(n) )
                  cellstate(i,j)%w0br(k) = min( tmpw0, cellstate(i,j)%w0br(k) )

! ^^^     tmp used only in test version
!              wlen(i,j,k) = -dmin

                endif
   60         continue
            endif
   70     continue
   90 continue
! ^^^tmp output
!      n = imax/15
!      n = max(1,n)
!      write (*,*)
!      write (*,*) 'BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB'
!      write (*,*) 'output from sbbr.for, w0br(i,j,k)'
!      write (*,*) '    i index values'
!      write (*,310) (i, i=0,imax,n)
!      do 220 k = 1,8
!      do 220 j = jmax,0,-1
!      write (*,300) (j, (cellstate(i,j)%w0br(k),i=0,imax,n))
!  220 continue
!      write (*,*) 'BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB'
!  300 format (1x, i3,1x, 30(f4.2,1x))
!  310 format (1x, i8, 30i5)
! ^^^ end tmp

      end

!     function to calc. length
      real function slen(x1,y1,x2,y2)
      real x1, y1, x2, y2
          slen = abs(sqrt((x1-x2)*(x1-x2)+(y1-y2)*(y1-y2)))
      return
      end

!     function to calc quadrant and then angles alpha and ceta
!     line direction point to br; ang clockwise from north
      real function ang(xbr,ybr,xp,yp)
      real ybr, xbr, yp, xp, tempa,pi
      pi = 3.1415927
      if (xbr .ne. xp) then
          tempa = atan (abs((ybr-yp)/(xbr-xp)))
          if ((ybr .eq. yp).and. (xbr > xp)) then !horizontal line east
              ang = pi/2.0
          elseif ((ybr .eq. yp).and. (xbr < xp))then!horiz. line west
              ang = 1.5*pi
          elseif ((ybr > yp).and.(xbr > xp)) then !quad 1
              ang = pi/2.0 - tempa
          elseif ((ybr < yp).and. (xbr > xp)) then !quad 2 
              ang = pi/2.0 + tempa
          elseif ((ybr < yp).and. (xbr < xp)) then !quad 3
              ang = 1.5*pi - tempa
          elseif (( ybr > yp).and. (xbr < xp)) then !quad 4
              ang = 1.5*pi + tempa
          endif
      elseif  (ybr < yp) then   ! vertical line to south
          ang = pi
      else
          ang = 2*pi       ! vertical line to north
      endif

      return
      end

!     function to calc. fu (fraction upwind fric. velocity
!     near the  barrier)
!     (ranges: porosity 0 to 0.9, distance: -5*zbr to 50*zbr)
      real function fu (xh, zbr, pbr, xbrw)
      real xh, zbr, pbr, xbrw
      real a, b, c, d, x, xw, pb
!     scale distance & width by barrier height
      x = xh/zbr
      xw = xbrw/zbr
!     increase effective porosity with barrier width
      pb = pbr + (1 - exp(-0.5*xw))*0.3*(1-pbr)

!     calculate coef. as fn of porosity
      a = 0.008-0.17*pb+0.17*pb**1.05
      b = 1.35*exp(-0.5*pb**0.2)
      c = 10*(1-0.5*pb)
      d = 3 - pb
!     calc. frac. of fric. vel.
      fu = 1 - exp(-a*x**2) + b*exp(-0.003*(x+c)**d)
!     Cap fu at 1.0
      if (fu > 1.0) then
          fu = 1.0
      endif
      return
      end
