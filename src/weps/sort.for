!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!
      subroutine sort (iarr,n,p1,p5,p9)

      integer  i,j,k,l,m,n,nn
      real iarr(*),itemp, p,p1, p5, p9,ri,rn

!     open (unit = 10, file= 'tmp.tmp')
!     open (unit = 11, file= 'tmp.out')
!     n = 19
!     do 5 i = 1,n
!        read (10,*) idate,iarr(i)
! 5   continue

      m = n
      do 30 nn = 1,n
         m = m / 2
         k = n - m
         do 20 j = 1, k
            i = j
  10        l = i + m
            if (iarr(l) .lt. iarr(i)) then
               itemp = iarr(i)
               iarr(i) = iarr(l)
               iarr(l) = itemp
               i = i - m
               if (i .ge. 1) go to 10
            end if
  20     continue
  30  continue
      do 40 i = 1, n
         ri = i
         rn = n
         p = ri/rn
         if (p .le. 0.1) p1 = iarr(i)
         if (p .le. 0.5) p5 = iarr(i)
         if (p .le. 0.9) p9 = iarr(i)

!         write(*,*) i, iarr(i), p
  40  continue
!         write(*,*) p1,p5,p9

      return

      end

