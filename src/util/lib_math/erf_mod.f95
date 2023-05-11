!$Author$
!$Date$
!$Revision$
!$HeadURL$

module erf_mod

  interface erf1
    module procedure erf1_single
    module procedure erf1_double
  end interface erf1

  contains

    ! For the equation Y = erf(X), the function returns Y, given X
    ! written by L. Hagen and coded by I. Elmanyawi.
    pure function erf1_single(xin) result(ret_erf)
      real, intent(in) :: xin
      real :: ret_erf

      real x, a, b, c, d, e, f, g, h, i, j, k
      real x2, x3, x4, x5, z

      a = -0.000018936
      b =  0.030284321
      c =  1.12891921
      d =  0.37693092
      e =  0.029375235
      f =  0.088848989
      g =  0.068200064
      h =  0.022155958
      i =  0.050754183
      j =  0.038090749
      k =  0.034275052

      z = xin
      x = abs(xin)
      x2= x*x
      x3= x2*x
      x4= x3*x
      x5= x4*x

      ret_erf = (a + c*x + e*x2 + g*x3 + i*x4 + k*x5) / &
           (1.000000 + b*x + d*x2 + f*x3 + h*x4 + j*x5)
      ret_erf = ret_erf * z / x

    end function erf1_single

    ! For the equation Y = erf(X), the function returns Y, given X
    ! written by L. Hagen and coded by I. Elmanyawi.
    pure function erf1_double(xin) result(ret_erf)
      double precision, intent(in) :: xin
      real :: ret_erf

      real x, a, b, c, d, e, f, g, h, i, j, k
      real x2, x3, x4, x5, z

      a = -0.000018936
      b =  0.030284321
      c =  1.12891921
      d =  0.37693092
      e =  0.029375235
      f =  0.088848989
      g =  0.068200064
      h =  0.022155958
      i =  0.050754183
      j =  0.038090749
      k =  0.034275052

      z = xin
      x = abs(xin)
      x2= x*x
      x3= x2*x
      x4= x3*x
      x5= x4*x

      ret_erf = (a + c*x + e*x2 + g*x3 + i*x4 + k*x5) / &
           (1.000000 + b*x + d*x2 + f*x3 + h*x4 + j*x5)
      ret_erf = ret_erf * z / x

    end function erf1_double

    ! For the equation Y = erf(X), the function returns the value of X
    ! given the value of Y, i.e., the reverse of the error function.
    ! written by L. Hagen and coded by I. Elmanyawi
    pure function rerf(yin) result(ret_rerf)
      real, intent(in) :: yin
      real :: ret_rerf

      real y, a, b, c, d, e, f, g, h, i, j, a1, b1, c1, d1, a2, b2, c2
      real er1, er2, y2, y3, y4, y5, base
      real z, sqy, alogy

      a =  0.000009477
      b = -2.76913995
      c =  0.88589485
      d =  2.30199509
      e = -2.44996884
      f = -0.14332263
      g =  2.246990417
      h = -0.54046067
      i = -0.68292239
      j =  0.15092933
      z =  yin
      y =  abs(yin)
!
      a1 = 2857.5463
      b1 =  - 0.00016423
      c1 = - 5717.0696
      d1 = - 2857.5783
      a2 =  69161.649
      b2 =-277330.28
      c2 = 208168.95

      sqy = sqrt (y)
      alogy = alog(y)
      y2 = y*y
      y3 = y2*y
      y4 = y3*y
      y5 = y4*y

      base = ( a + c*y + e*y2 + g*y3 + i*y4 ) / &
             ( 1.000000 + b*y + d*y2 + f*y3 + h*y4 + j*y5 )

      if( y .le. 0.966105 ) then
        ret_rerf = base * z / y
      else if( y .gt. 0.999311 ) then
        er2 = a2 + b2 * y * sqy + c2 * y2
        ret_rerf = (base - er2) * z / y
      else
        er1 = a1 + b1 / alogy + c1 * alogy/y + d1 / y2
        ret_rerf = (base - er1) * z / y
      end if

    end function rerf

end module erf_mod
