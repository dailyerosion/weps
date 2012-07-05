!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!
!     file: scrv1.for

      subroutine scrv1 (x1, y1, x2, y2, a, b)

!     Author : Amare Retta
!     + + + PURPOSE + + +
!     Program to compute parameters for an s-curve.

!     + + + ARGUMENT DECLARATIONS + + +
      real a,b,x1,x2,y1,y2

!     + + + LOCAL VARIABLES + + +
      real xx1,xx2
      real xx
      xx1=abs(x1)
      xx2=abs(x2)
!        write(*,*) 'scrv1',(y1-xx1)
        xx = alog(xx1/y1-xx1)
!        write(*,*) 'scrv2',(y2-xx2),(xx2-xx1)
        b=(xx-alog(xx2/y2-xx2))/(xx2-xx1)
        a=xx+b*xx1
!       write(21,900)
! 900   format(/,'scrv1')
!       write(21,901)
! 901   format('a,b,xx1,xx2,y1,y2')
!       write(21,*)a,b,xx1,xx2,y1,y2
        return
        end
