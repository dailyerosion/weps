!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!
      subroutine fopenk(filnum, filnam, filsta)
! ****************************************************************** wjr
!
! Provides error trapped opening of files
!
!       Edit History
!       05-Feb-99       wjr     Original coding
!
!      include 'file.inc'
!      
      integer           filnum
      character*(*)      filnam
      character*(*)       filsta
      integer           ios
!
! ***      write(*,1991) filnum, filnam,filsta
! *** 1991    format('in copenk', i3,a,a)
      open(filnum,FILE=filnam(1:len_trim(filnam)),STATUS=filsta,        &
     &  ERR=100, IOSTAT=ios)
      write(*,101) filnam(1:len_trim(filnam)), filnum, filsta
  101 format(' Opened file: ',a,' on unit ',i3,' with status ',a)

      return
!
100   write(0,1000) filnam(1:len_trim(filnam)),filnum,filsta,ios
! *** 1000  format('I3  A  A')
1000  format(' Cannot open file: ',a,' on unit ',i3,                    &
     & ' with status ',a, ' and I/O status ', i5)
      call exit (1)
      end
