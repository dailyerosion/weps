!$Author$
!$Date$
!$Revision$
!$HeadURL$
!**********************************************************************
!     subroutine plotout.for
!**********************************************************************

      subroutine plotout (yplot, ycharin, yin)

!     + + +  PURPOSE + + +
!     1. to create headings for tsterode.eplt file
!     2. to store dep var (yin) and indep var (xin)
!         and write to tsterode.eplt file for each eros run

!     plotout is called from erodout.for with yin data
!     the xin data come from common/plot/ with erodin.for

!     + + + ARGUMENT DECLARATAIONS + + +
      integer yplot
      character*12 ycharin(30)
      real yin(30)

!     + + + ARGUMENT DEFINITIONS + + +
!     yplot     = number of dep variables to put in tsterode.eplt file
!     ycharin(i)= name(s) of dep. variables
!     yin(i)    = value(s) of dep. variables
!
!     + + + PARAMETERS + + +
!
!     + + + GLOBAL COMMON BLOCKS + + +
      include 'file.inc'
!
!     + + + LOCAL COMMON BLOCKS + + +

      integer xplot
      character*12 xcharin(30)
      real xin(30)
      common /plot/ xplot, xcharin, xin

      save :: /plot/
!
!     + + + LOCAL VARIABLES + + +
!
      integer i,j, nline
      logical used
      character*500 line, plotdat(500)
!
!     + + + LOCAL VARIABLE DEFINITIONS + + +
!     i,j    = temp indexes
!     used   = logical for presence of tsterode.eplt file
!
!     + + + SUBROUTINES CALLED + + +
!
!     + + + FORMATS + + +
  200 format(40f12.4)
  201 format(' file:')
  202 format('tsterode.eplt')
!
!     + + + END SPECIFICATIONS + + +
! ^^^ tmp out
!      write(*,*)'^^^'
!       write(*,*)'out from plotout.for'
!       write(*,*) 'yplot=', yplot
!       write(*,*) 'ycharin=', ycharin(1:yplot)
!       write(*,*) 'xplot=', xplot
!       write(*,*) 'xcharin=', xcharin(1:xplot)
!       write(*,*) 'xin=',  xin(1:xplot)
!       write(*,*) 'yin=', yin(1:yplot)
! ^^^ end tmp out
!
!     create tsterode.eplt file
      inquire (FILE='tsterode.eplt',EXIST=used)
      if(.not.used) then

!       write heading for tsterode.eplt
        open(UNIT=luo1,FILE='tsterode.eplt',STATUS='new')
        write(luo1,201)
        write(luo1,202)
!      write(luo1,*)((ycharin(i),i=1,yplot),(xcharin(i),i=1,xplot))
        write(luo1,*)  ycharin(1:yplot), xcharin(1:xplot)
        write(luo1,*)
        close(UNIT=luo1)
      endif
!
!     read current tsterode.eplt file to plotdat char. array
      open(UNIT=luo1,FILE='tsterode.eplt',STATUS='old')
      j=0
   20 read (luo1, '(a)', end=50) line
      j=j + 1
      plotdat(j) = line
      go to 20
!
!     update the tsterode.eplt file
   50 nline=j
      rewind (UNIT=luo1)
      do 55 j=1,nline
         write(luo1,'(a)') plotdat(j)
   55 continue
!     change sign of erosion components (yin) and write variables
      write (luo1,200)  (-1)*yin(1:yplot), xin(1:xplot)
      close (UNIT=luo1)

      return
      end
!**********************************************************************

