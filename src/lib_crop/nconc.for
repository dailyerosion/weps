!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!
!     file: nconc.for
!
!
      subroutine nconc (po, p5,p1, a)
!
!     + + + PURPOSE + + +
!     This subroutine computes parameters of an equation describing the
!     N and P relations to biomass accumulation.
!
!     + + + ARGUMENT DECLARATIONS + + +
      real a, po, p5, p1

!     + + + LOCAL VARIABLES + + +
      integer i
      real a5,ea,ea1,eg,pog,eg1,peg,po1,rt,pg5,fu,dfda
!
!     + + + LOCAL VARIABLE DEFINITIONS + + +
!     a5 - intermediate variables for solving the N or P uptake ratios
!     ea - intermediate variables for solving the N or P uptake ratios
!     ea1 - intermediate variables for solving the N or P uptake ratios
!     eg - intermediate variables for solving the N or P uptake ratios
!     peg - intermediate variables for solving the N or P uptake ratios
!     po1 - intermediate variables for solving the N or P uptake ratios
!     rt - intermediate variables for solving the N or P uptake ratios
!     pg5 - intermediate variables for solving the N or P uptake ratios
!     fu - intermediate variables for solving the N or P uptake ratios
!     dfda - intermediate variables for solving the N or P uptake ratios
!
      a = 5.
      do 2 i=1,10
         a5=a*.5
         ea=exp(a)
         ea1=ea-1.
         eg=exp(-a5)
         pog=po*eg
         eg1=exp(a5)
         peg=p1*(ea-eg1)
         po1=po*(1.-eg)
         rt=peg-po1
         pg5=.5*pog
         fu=rt/ea1+pog-p5
         if (abs(fu) .lt. 1.e-7) go to 3
         dfda=(ea1*(p1*(ea-.5*eg1)-pg5)-ea*rt)/(ea1*ea1)-pg5
         a=a-fu/dfda
    2 continue
!     WRITE (*,4) A,FU
    3 p5=(p1*ea-po)/ea1
      po=po-p5
      return

    4 FORMAT (//T10,'NCONC DID NOT CONVERGE',2E16.6)
!     STOP
      end
