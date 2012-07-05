!$Author$
!$Date$
!$Revision$
!$HeadURL$

      integer   TIMSTART
      parameter (TIMSTART = 1)
!
      integer   TIMSTOP
      parameter (TIMSTOP = 2)
!
      integer   TIMPRINT
      parameter (TIMPRINT = 3)
!
      integer   TIMRESET
      parameter (TIMRESET = 4)
!
      integer   TIMCROP
      parameter (TIMCROP = 1)
!
      integer   TIMHYDR
      parameter (TIMHYDR = 2)
!
      integer   TIMSOIL
      parameter (TIMSOIL = 3)
!
      integer   TIMDECO
      parameter (TIMDECO = 4)
!
      integer   TIMMANG
      parameter (TIMMANG = 5)
!
      integer   TIMDARC
      parameter (TIMDARC = 6)
!
      integer   TIMEROS
      parameter (TIMEROS = 7)
!
      integer   TIMSBWIND
      parameter (TIMSBWIND = 8)
!
      integer   TIMSBEROD
      parameter (TIMSBEROD = 9)
!
      integer   TIMSBQOUT
      parameter (TIMSBQOUT = 10)
!
      integer   TIMWEPS
      parameter (TIMWEPS = 11)
!
      character*8  timnam(0:11)
      data timnam /'system', 'crop', 'hydro', 'soil', 'decomp',         &
     &  'manage', 'darcy', 'erosion', 'sbwind','sberod','sbqout','weps'/
!
