!$Author$
!$Date$
!$Revision$
!$HeadURL$
!***********************************************************************
!*     subroutine sbwind
!***********************************************************************
      subroutine sbwind (wustfl,awu, wind_dir, ntstep, intstep, rusust)
!
!     +++ PURPOSE +++
!     to update wzzo at each grid point;
!     To update  soil friction velocity on each grid point
!     and modify it for barriers and hills;
!     To initialize en thresh. and cp thresh. fr. velocites on grid;
!     To calculate max ratios of friction velocity to threshold
!     friction velocity
!
!     +++ ARGUMENT DECLARATIONS +++
      integer wustfl,intstep, ntstep
      real awu, rusust, wind_dir
!
!     +++ ARGUMENT DEFINITIONS +++
!     intstep  - current index of ntstep thru time
!     ntstep   - max. no. of time steps in day
!     icsr     - index of current subregion.
!     wusp     - subregion soil threshold friction vel. trans. cap. (m/s)
!     rusust    - max ratio of friction velocity to thresh. friction vel.
!     imax     - no. grid intervals in x-direction.
!     jmax     - no. grid intervals in y-direction.
!     wus      - soil friction velocity at grid points corrected for
!                hills and barriers (m/s).
!     wust     - threshold fr. vel. for en. at grid points
!     wusp     - threshold fr. vel. for trans. cap. at grid points
!     wind_dir - direction of wind (degrees from north)
!
!     + + + GLOBAL COMMON BLOCKS + + +

      include  'p1werm.inc'
      include  'c1gen.inc'
      include  'm1geo.inc'
      include  'b1glob.inc'
      include  'c1glob.inc'
      include  'd1glob.inc'
      include  'h1db1.inc'
      include  'p1const.inc'
      include  's1agg.inc'
      include  's1dbh.inc'
      include  's1sgeo.inc'
!
!     + + + LOCAL COMMON BLOCKS + + +
      include  'erosion/m2geo.inc'
      include  'erosion/w2wind.inc'
      include  'erosion/e2grid.inc'
      include  'erosion/e3grid.inc'
      include  'erosion/s2agg.inc'
      include  'erosion/s2sgeo.inc'
      include  'erosion/s2surf.inc'
!
!     +++ LOCAL VARIABLES +++
      integer i,j, icsr,k
      real wzorg, wzorr, wzzo, wzzov
      real at, rintstep, brcd
      real wubsts, wucsts, wucwts, wucdts, sfcv
!
!     +++ SUBROUTINES CALLED
!     sbzo
!     sbwus
!     sbwust
!
!     + + + END SPECIFICATIONS + + +

!     assign subregion index, currently only one
      icsr = 1
      rusust = 0.1

!     loop through grid interior to update
      do 40 i = 1, imax-1
      do 30 j = 1, jmax-1

!     update aerodynamic roughness
! ^^^ tmp out
!     write (*,*) 'in sbwind, call to sbzo'

      call sbzo                                                         &
     & (sxprg(icsr), szrgh(i,j), slrr(i,j),                             &
     &    wzoflg, adrlaitot(icsr), adrsaitot(icsr), abzht(icsr),        &
     &    acrlai(icsr), acrsai(icsr), aczht(icsr),                      &
     &    acxrow(icsr), ac0rg(icsr), wzorg, wzorr,                      &
     &    wzzo, wzzov, awzzo, brcd)

! ^^^ tmp out
!      write (*,*) 'in sbwind, call to sbwus'
!     update surface (below canopy) friction velocity
 
      call sbwus (anemht, awzzo, awu, wzzov, brcd, wus(i,j) )

!     correct friction velocity for hills
!      if (nhill .ne. 0 ) then
!           wus(i,j) = wus(i,j) * w0hill(i,j,kbr)
!        endif
!
!     correct friction velocity for barriers
      if (nbr .ne. 0 ) then
         wus(i,j) = wus (i,j) * w0br (i,j,kbr)
      endif

      if (wustfl .eq. 1) then
        ! update threshold friction velocities
        ! calculate hour k for surface water content
        rintstep = intstep
        k = aint(rintstep*23.75/ntstep) + 1

        call sbwust                                                     &
     &  (sf84(i,j), asdagd(1,icsr), sfcr(i,j), svroc(i,j),              &
     &  sflos(i,j), abffcv(icsr),wzzo, ahrwc0(k,icsr), ahrwcw(1,icsr),  &
     &  wus(i,j), sf84ic, asvroc(1,1), dmlos(i,j),                      & 
     &  wust(i,j), wusp(i,j), wusto, sf84mn(i,j), smaglos(i,j),         &
     &  smaglosmx(i,j), wubsts, wucsts, wucwts, wucdts, sfcv)

! ^^^ tmp out
!      if( wust(i,j) .le. 0.0 ) then
!       write(*,*) "sbwind: i,j", i, j
!       write(*,*) "sbwind: wus(i,j), wust(i,j), rusust",                &
!     &            wus(i,j), wust(i,j), rusust
!       write(*,*) "sf84(i,j) = ", sf84(i,j)
!       write(*,*) "asdagd(1,icsr)", asdagd(1,icsr)
!       write(*,*) "sfcr(i,j)", sfcr(i,j)
!       write(*,*) "svroc(i,j)", svroc(i,j)
!       write(*,*) "sflos(i,j) ",sflos(i,j)
!       write(*,*) "abffcv(icsr)", abffcv(icsr)
!       write(*,*) "wzzo", wzzo
!       write(*,*) "ahrwc0(k,icsr)", ahrwc0(k,icsr)
!       write(*,*) "ahrwcw(1,icsr)", ahrwcw(1,icsr)
!       write(*,*) "wus(i,j)", wus(i,j)
!       write(*,*) "sf84ic", sf84ic
!       write(*,*) "rusust", rusust
!       write(*,*) "asvroc(1,1)", asvroc(1,1)
!       write(*,*) "dmlos(i,j)", dmlos(i,j)
!       write(*,*) "wust(i,j)", wust(i,j)
!       write(*,*) "wusp(i,j)", wusp(i,j)
!       write(*,*) "sf84mn(i,j)", sf84mn(i,j)
!       write(*,*) "smaglos(i,j)", smaglos(i,j)
!       stop
!      end if

      endif

      at = wus(i,j)/wust(i,j)
      rusust = amax1(rusust, at)

   30 continue
   40 continue

!     write (*,*) 'at exit sbwind rusust =', rusust
!     write (*,*) ' wus(3,3), wust(3,3)', wus(3,3), wust(3,3)

      return
      end
!++++ +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
