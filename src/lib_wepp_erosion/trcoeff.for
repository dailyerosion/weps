      subroutine trcoeff(trcoef,shrsol,sand,dia,spg,tcf1,npart,frac)
      
      use wepp_interface_defs
      
      implicit none
!
!************************************************************
!                                                           *
!   this function is called from sr param to compute the    *
!   sediment transport coefficient for rill flow. it calls  *
!   sr yalin.                                               *
!                                                           *
!************************************************************
!                                                           *
!   argument                                                *
!      shrsol  : flow shear stress                          *
!                                                           *
!************************************************************
!
!     + + + parameter declarations + + +
!
      include 'wepp_erosion.inc'
!
      real, intent(in) :: sand(mxnsl), dia(mxpart), spg(mxpart),        &
     &   frac(mxpart),shrsol
      integer, intent(in) ::  npart
	  real, intent(out) :: trcoef
	  real, intent(inout):: tcf1(mxpart)

	  real tottc
!      
      call yalin(shrsol,tottc,sand,dia,spg,tcf1,npart,frac)
      trcoef = tottc / shrsol ** 1.5
      if (trcoef.eq.0.0) trcoef = 0.000000001
!     
      return
      end
