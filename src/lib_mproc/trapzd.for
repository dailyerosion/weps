!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!
!  This routine is an algorithm which performs integration on a
!  function.  The algorithm is based on the extended trapzoidal rule.
!  The routine integrates the function between a and b.  N represents 
!  the N'th stage of refinment of the trapzoid rule.  N=1 gives the 
!  crudest estimate of the integrated function, subsequent call with 
!  N=2,3,... improve the accuracy of the calculation.  S is the value
!  of the integral and should not be modified between sequential calls.
!  For more info refer to "Numerical Recipies-The Art of Scientific Computing"
!  Cambridge University press, 1986. 

	   subroutine trapzd(a,b,s,n) 
       integer it, tmn, n,  j
	   real a, b, func, del, s, x, sum 
	   if (n.eq.1) then
		 s=0.5*(b-a)*(func(a)+func(b))
		 it = 1
       else
		 tmn = it
		 del = (b-a)/tmn
		 x=a+0.5*del
		 sum=0.0
		 do 100 j=1,it
		   sum=sum+func(x)
		   x=x+del
 100     continue
         s=0.5*(s+(b-a)*sum/tmn)
         it=2*it
       endif
	   return
	   end
