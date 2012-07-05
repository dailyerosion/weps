!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!
       
	   real function func(y)
!
!  This function is just the equation for a straight line 
!  and is used with trapzd.for which performs the integral
!  of this line.  The function is used for the triangular root distribution. 
!  Other equations for a line can also be used if the distribution
!  is not triangular.
       real y 
       
	   func=0.5*y 

	   end
