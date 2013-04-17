!$Author$
!$Date$
!$Revision$
!$HeadURL$
!**********************************************************************
!     subroutine plotout.for
!**********************************************************************

      subroutine plotout (yplot, ycharin, yin)

!     + + +  PURPOSE + + +
!     1. to create headings for sweep.eplt file
!     2. to store dep var (yin) and indep var (xin)
!         and write to sweep.eplt file for each eros run

!     plotout is called from erodout.for with yin data
!     the xin data come from common/plot/ with erodin.for

      use file_io_mod, only: fopenk

!     + + + ARGUMENT DECLARATAIONS + + +
      integer yplot
      character*12 ycharin(30)
      real yin(30)

!     + + + ARGUMENT DEFINITIONS + + +
!     yplot     = number of dep variables to put in sweep.eplt file
!     ycharin(i)= name(s) of dep. variables
!     yin(i)    = value(s) of dep. variables
!
!     + + + PARAMETERS + + +
!
!     + + + GLOBAL COMMON BLOCKS + + +

!     + + + LOCAL COMMON BLOCKS + + +

      integer xplot
      character*12 xcharin(30)
      real xin(30)
      common /plot/ xplot, xcharin, xin

      save :: /plot/
!
!     + + + LOCAL VARIABLES + + +
!
!      integer i
      integer j, nline
      logical used
      character*500 line, plotdat(500)
      integer luo1    ! output file unit number
!
!     + + + LOCAL VARIABLE DEFINITIONS + + +
!     i,j    = temp indexes
!     used   = logical for presence of sweep.eplt file
!
!     + + + SUBROUTINES CALLED + + +
!
!     + + + FORMATS + + +
  200 format(40f12.4)
  201 format(' file:')
  202 format('sweep.eplt')
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
!     create sweep.eplt file
      inquire (FILE='sweep.eplt',EXIST=used)
      if(.not.used) then

!       write heading for sweep.eplt
        call fopenk(luo1, 'sweep.eplt', 'new')
        write(luo1,201)
        write(luo1,202)
!      write(luo1,*)((ycharin(i),i=1,yplot),(xcharin(i),i=1,xplot))
        write(luo1,*)  ycharin(1:yplot), xcharin(1:xplot)
        write(luo1,*)
        close(UNIT=luo1)
      endif
!
!     read current sweep.eplt file to plotdat char. array
      call fopenk(luo1, 'sweep.eplt', 'old')
      rewind (UNIT=luo1)
      j = 0
   20 read (luo1, '(a)', end=50) line
      j = j + 1
      plotdat(j) = line
      go to 20
!
!     update the sweep.eplt file
   50 nline = j
      rewind (UNIT=luo1)
      do 55 j = 1, nline
         write(luo1,'(a)') trim(plotdat(j))
   55 continue
!     change sign of erosion components (yin) and write variables
      write (luo1,200)  (-1)*yin(1:yplot), xin(1:xplot)
      close (UNIT=luo1)

      return
      end
!**********************************************************************

