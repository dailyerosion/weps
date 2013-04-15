!$Author$
!$Date$
!$Revision$
!$HeadURL$
!**********************************************************************
!     subroutine sbbar       3/20/98
!**********************************************************************
      subroutine sbbr( rel_wind_angle, cellstate )

!     + + + PURPOSE + + +
!     to calculate the fraction of open field friction velocity
!     in from up wind and down wind sources of shelter at all interior nodes

      use weps_interface_defs
      use erosion_data_struct_defs, only: cellsurfacestate
      use grid_geo_def, only: imax, jmax, ix, jy
      use p1unconv_mod, only: pi
      use barriers_mod
      use Points_Mod
      use pnt_polyline_mod
      use lin_interp_mod
      use grid_geo_def, only: amxsim

!     + + + ARGUMENT DECLARATIONS + + +
      real, intent(in) :: rel_wind_angle  ! angle of the wind relative the grid positive y-axis (see sbdirini)
      type(cellsurfacestate), dimension(0:,0:), intent(inout) :: cellstate     ! initialized grid cell state values

!     + + + LOCAL VARIABLES + + +
      integer i, j, n   ! do-loop indices
      type(point) ::  pnt_grid  ! point form of grid coordinate
      type(location_intersect) ::  loc_intersect  ! point where upwind direction meets barrier, index in polyline and fraction distance between indexes
      real :: dist   ! distance from grid cell centroid to barrier
      real :: w0br_min   ! minimum value of sheltering effect (fraction of open field fric. vel) for this point
      integer :: npt     ! number of points along the barrier
      real :: zbr_interp  ! value of barrier height interpolated along barrier
      real :: pbr_interp  ! value of barrier porosity interpolated along barrier
      real :: xbrw_interp  ! value of barrier width interpolated along barrier

!     + + + FUNCTION DECLARATIONS + + +
      real fu

!     + + + END SPECIFICATIONS + + +

      ! update interior nodes
      do i = 1, imax-1
        do j = 1, jmax-1
          ! calculate distance to middle of grid cell (maybe offset from origin)
          pnt_grid%x = (i-0.5)*ix + amxsim(1)%x
          pnt_grid%y = (j-0.5)*jy + amxsim(1)%y
! ^^^tmp
!      if (i .eq. 1 .and. j .eq. jmax-1)then
!       write (*,*) 'sbbr output at 1, jmax-1 lx=', lx, 'ly=', ly
!       write (*,*) 'ix=',ix, 'jy=', jy, 'imax=', imax, 'jmax=',jmax
!      endif
! ^^^ end tmp
          ! barrier sweep
          w0br_min = 1.0   ! maximum value for parameter
          do n = 1, size(barrier)
            ! look for barrier up wind
            if( pl_intersect( pnt_grid, rel_wind_angle, barrier(n)%points, loc_intersect ) ) then
              ! intersection point found (it is minimum distance for this barrier)
              dist = slen(pnt_grid, loc_intersect%pnt)

              ! find number of points in barrier for interpolations
              npt = size(barrier(n)%param)

              ! barrier influence calculated down wind of barrier
              ! interpolate height along barrier segment
              zbr_interp = lin_interp( loc_intersect%low_index, loc_intersect%dist_frac, barrier(n)%param(1:npt)%amzbr )
              if (dist .le. 35*zbr_interp) then
                ! distance is close enough for effect
                ! interpolate parameters along barrier segment
                pbr_interp = lin_interp( loc_intersect%low_index, loc_intersect%dist_frac, barrier(n)%param(1:npt)%ampbr )
                xbrw_interp = lin_interp( loc_intersect%low_index, loc_intersect%dist_frac, barrier(n)%param(1:npt)%amxbrw )
                ! find shelter effect
                w0br_min = min(w0br_min, fu( dist, zbr_interp, pbr_interp, xbrw_interp ) )
              end if
            end if

            ! look for barrier down wind
            if( pl_intersect( pnt_grid, rel_wind_angle-180.0, barrier(n)%points, loc_intersect ) ) then
              ! intersection point found (it is minimum distance for this barrier)
              dist = slen(pnt_grid, loc_intersect%pnt)

              ! barrier influence calculated down wind of barrier
              ! interpolate height along barrier segment
              zbr_interp = lin_interp( loc_intersect%low_index, loc_intersect%dist_frac, barrier(n)%param(1:npt)%amzbr )
              if (dist .lt. 5*zbr_interp) then
                ! distance is close enough for effect
                ! interpolate parameters along barrier segment
                pbr_interp = lin_interp( loc_intersect%low_index, loc_intersect%dist_frac, barrier(n)%param(1:npt)%ampbr )
                xbrw_interp = lin_interp( loc_intersect%low_index, loc_intersect%dist_frac, barrier(n)%param(1:npt)%amxbrw )

                ! find shelter effect (on upwind side of barrier use negative distance for correct function value)
                w0br_min = min(w0br_min, fu( -dist, zbr_interp, pbr_interp, xbrw_interp ) )
              end if
            end if
          end do

          ! assign minimum value to grid cell
          cellstate(i,j)%w0br(1) = w0br_min

        end do
      end do

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
!      real function slen(x1,y1,x2,y2)
!      real x1, y1, x2, y2
!          slen = abs(sqrt((x1-x2)*(x1-x2)+(y1-y2)*(y1-y2)))
!      return
!      end

!     function to calc quadrant and then angles alpha and ceta
!     line direction point to br; ang clockwise from north
!      real function ang(xbr,ybr,xp,yp)
!      real ybr, xbr, yp, xp, tempa,pi
!      pi = 3.1415927
!      if (xbr .ne. xp) then
!          tempa = atan (abs((ybr-yp)/(xbr-xp)))
!          if ((ybr .eq. yp).and. (xbr > xp)) then !horizontal line east
!              ang = pi/2.0
!          elseif ((ybr .eq. yp).and. (xbr < xp))then!horiz. line west
!              ang = 1.5*pi
!          elseif ((ybr > yp).and.(xbr > xp)) then !quad 1
!              ang = pi/2.0 - tempa
!          elseif ((ybr < yp).and. (xbr > xp)) then !quad 2 
!              ang = pi/2.0 + tempa
!          elseif ((ybr < yp).and. (xbr < xp)) then !quad 3
!              ang = 1.5*pi - tempa
!          elseif (( ybr > yp).and. (xbr < xp)) then !quad 4
!              ang = 1.5*pi + tempa
!          endif
!     elseif  (ybr < yp) then   ! vertical line to south
!          ang = pi
!      else
!          ang = 2*pi       ! vertical line to north
!      endif
!
!      return
!      end

!     function to calc. fu (fraction of upwind fric. velocity
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
