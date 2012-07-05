!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!
!**********************************************************************
      function rerf(y)
!**********************************************************************
!     + + +  PURPOSE + + +
!     For the equation Y = erf(X), the function returns the value of X
!     given the value of Y, i.e., the reverse of the error function.
!     written by L. Hagen and coded by I. Elmanyawi

!     + + +  ARGUMENT DECLARATIONS + + +
      real rerf, y

!     + + + LOCAL VARIABLES + + +
      real a, b, c, d, e, f, g, h, i, j, a1, b1, c1, d1, a2, b2, c2
      real er1, er2, y2, y3, y4, y5, base
      real z, sqy, alogy
!     + + + END SPECIFICATIONS + + +
!
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
      z =  y
      y =  abs(y)
!
      a1 = 2857.5463
      b1 =  - 0.00016423
      c1 = - 5717.0696
      d1 = - 2857.5783
      a2 =  69161.649
      b2 =-277330.28
      c2 = 208168.95
!
      sqy = sqrt (y)
      alogy = alog(y)
      y2 = y*y
      y3 = y2*y
      y4 = y3*y
      y5 = y4*y
!
      base =  ( a + c*y + e*y2 + g*y3 + i*y4 )/                         &
     &        ( 1.000000 + b*y + d*y2 + f*y3 + h*y4 + j*y5 )
!
      if (y .le. 0.966105) go to 9
      if (y .gt. 0.999311) go to 5
!
      er1 =a1 + b1 / alogy + c1 * alogy/y + d1 / y2
      rerf = base - er1
      go to 10
!
 5    er2 = a2 + b2 * y * sqy + c2 * y2
      rerf = base - er2
      go to 10
 9    rerf = base
10    rerf = rerf* z / y
      return
      end
