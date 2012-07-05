!$Author$
!$Date$
!$Revision$
!$HeadURL$

!     file: p1crop.inc

!     This file contains partameter variables
!     used in s-curve calculations

!     + + + VARIABLE DECLARATIONS + + +
!      real  co2
!      real  hix1,hiy1,hix2,hiy2
      real  s11x1,s11y1,s11x2,s11y2,s8x1,s8y1,s8x2,s8y2

!     parameter (co2     = 330.0)
!     parameter (hix1    =  0.50)
!     parameter (hix2    =  0.95)
!     parameter (hiy1    =  0.10)
!     parameter (hiy2    =  0.95)
      parameter (s11x1   =  5.00)
      parameter (s11x2   = 20.00)
      parameter (s11y1   =  0.01)
      parameter (s11y2   =  0.90)
      parameter (s8x1    = 20.00)
      parameter (s8x2    = 80.00)
      parameter (s8y1    =  0.50)
      parameter (s8y2    =  0.95)


!     + + + VARIABLE DEFINITIONS + + +
! hix1 - ratio of actual to potential et (1st x-coordinate in the water stress - harvest index s-curve)
! hiy1 - fraction of reduction in harvest index (1st x-coordinate in the water stress - harvest index s-curve)
! hix2 - ratio of actual to potential et (2nd x-coordinate in the water stress - harvest index s-curve)
! hiy2 - fraction of reduction in harvest index (2nd x-coordinate in the water stress - harvest index s-curve)
! s11x1 - soil labile p concentration (ppm) (1st x-coordinate in the p uptake reduction s-curve)
! s11y1 - p uptake restriction factor (1st y-coordinate in the p uptake reduction s-curve)
! s11x2 - soil labile p concentration (ppm) (2nd x-coordinate in the p uptake reduction s-curve)
! s11y2 - p uptake restriction factor (2nd y-coordinate in the p uptake reduction s-curve)
! s8x1 - scaled ratio of actaul to potential n or p (1st x-coordinate in the n stress factor s-curve)
! s8y1 - n or p stress factor (1st ycoordinate in the n or p stress s-curve)
! s8x2 - scaled ratio of actaul to potential n or p (2nd x-coordinate in the n stress factor s-curve)
! s8y2 - n or p stress factor (2nd ycoordinate in the n or p stress s-curve)
