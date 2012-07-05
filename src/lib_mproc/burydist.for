!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!
! This routine returns the fraction of material buried in layer number 
! LAY given the burial distribution function type BURYDISTFLG and the
! layer thicknesses LTHICK and the total number of layers in which
! material will be buried NLAY and the tillage depth, soil layer
! thicknesses, and the number of soil layers.  It returns the number
! of layers that will be considered to be within the tillage zone for
! this operation.

      real function burydist( lay, burydistflg, lthick, ldepth, nlay)

      include 'p1werm.inc'

!     argument declarations
      integer lay
      integer burydistflg
      real    lthick(mnsz)
      real    ldepth(mnsz)
      integer nlay

!     argument definitions
!     lay         - soil layer for which fraction is returned
!     tlay        - number of soil layers affected by tillage
!     burydistflg - distribution function to be used
!              0    o uniform distribution
!              1    o Mixing+Inversion Burial Distribution
!              2    o Mixing Burial Distribution
!              3    o Inversion Burial Distribution
!              4    o Lifting, Fracturing Burial Distribution
!              5    o Compression
!     lthick      - thickness of soil layer
!     ldepth      - distance from surface to bottom of layer
!     nlay        - number of soil layers affected

!     local variable declarations
      real upper, lower
      real c1exp, c2exp
      real c3e1, c3e2, c3brk, c3split

      parameter (c1exp = 0.5)
      parameter (c2exp = 0.3)
      parameter (c3brk = 0.60)

!     assign depth from surface to upper and lower layer bounds
      if( lay.eq.1 ) then
          upper = 0.0
      else
          upper = ldepth(lay-1) / ldepth(nlay)
      end if
      lower = ldepth(lay) / ldepth(nlay)

!     find fraction of material buried in layer LAY
      select case (burydistflg)
      case(1)
          burydist = lower**c1exp - upper**c1exp
      case(2,5) ! same for compression and mixing from Nat. Agron. Manual, 508CrevisionwSTIR 071106DTL
          burydist = lower**c2exp - upper**c2exp
      case(3)
          if(lower.le.c3brk) then 
              burydist = 0.28*(exp(1.83*lower)-1.0)
          else
              burydist = 1.0-0.441*((1.0-lower)/0.4)**1.4
          endif
          if(upper.le.c3brk) then 
              burydist = burydist - (0.28*(exp(1.83*upper)-1.0))
          else
              burydist = burydist - (1.0-0.441*((1.0-upper)/0.4)**1.4)
          endif
      case(4)
          burydist = lower**c1exp - upper**c1exp
      case default   !uniform burial distribution
          burydist = lower - upper
      end select
 1000 return
      end
