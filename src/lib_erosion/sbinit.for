!$Author$
!$Date$
!$Revision$
!$HeadURL$
!**********************************************************************
!     subroutine sbinit
!**********************************************************************

      subroutine sbinit

!     +++ purpose +++
!     Input subregion values of variables from other submodels
!     to the grid points of the erosion submodel which erosion changes
!     Initialize output grid array
!     Calc. soil fraction of 4 dia. from asd, & rr shelter angles
!
!     +++ ARGUMENT DECLARATIONS +++
!
!     +++ ARGUMENT DEFINITIONS +++
!
!     +++ PARAMETER +++
!
!     + + + GLOBAL COMMON BLOCKS + + +

      include 'p1werm.inc'
      include 'm1subr.inc'
      include 's1phys.inc'
      include 's1agg.inc'
      include 's1dbh.inc'
      include 's1surf.inc'
      include 's1sgeo.inc'
      include 'b1glob.inc'
      include 'w1clig.inc'
!
!     + + +  LOCAL COMMON BLOCKS + + +
      include 'erosion/p1erode.inc'
      include 'erosion/m2geo.inc'
      include 'erosion/e2grid.inc'
      include 'erosion/s2agg.inc'
      include 'erosion/s2surf.inc'
      include 'erosion/s2sgeo.inc'
      include 'erosion/e2erod.inc'
!
!
!     + + + LOCAL VARIABLES + + +
      integer  icsr, i, j
      real sfd1(mnsub), sfd10(mnsub), sfd84(mnsub), sfd200(mnsub)

!     + + + LOCAL VARIABLE DEFINITIONS + + +
!     icsr  = index of current subregion
!
!     + + + SUBROUTINES CALLED + + +
!     sbsfdi
!     sbpm10
!     + + + END SPECIFICATION + + +
!
!     calculate abrasion and pm10 parameters    edit LH 3-4-05
      do 5 icsr = 1, nsubr
      call sbpm10                                                       &
     & (aseags(1,icsr),asecr(icsr),asfcla(1,icsr),asfsan(1,icsr),       &
     & awzypt, acanag(icsr), acancr(icsr),                              &
     & asf10an(icsr), asf10en(icsr), asf10bk(icsr)) 
!
!     calculate fraction less than diameter from asd
!       determine current subregion
!      do 5  icsr = 1, nsubr
      call sbsfdi                                                       &
     & (aslagm(1,icsr),as0ags(1,icsr), aslagn(1,icsr),                  &
     & aslagx(1,icsr), 0.01, sfd1(icsr))
      call sbsfdi                                                       &
     & (aslagm(1,icsr), as0ags(1,icsr), aslagn(1,icsr),                 &
     & aslagx(1,icsr), 0.1, sfd10(icsr))
      call sbsfdi                                                       &
     & (aslagm(1,icsr), as0ags(1,icsr), aslagn(1,icsr),                 &
     & aslagx(1,icsr), 0.84, sfd84(icsr))
!     store initial sf84
       sf84ic = sfd84(icsr)
       sf84ic = min(0.9999, max(sf84ic,0.0001))            !set limits
!     store initial sf10
       sf10ic = sfd10(icsr)
!       
!^^^ tmp out
!      write (*,*)
!      write (*,*) 'sbinit out'
!      write (*,*) 'aslagm as0ags aslagn aslagx sfd84',
!     &     aslagm(1,icsr), as0ags(1,icsr),aslagn(1,icsr),
!     &     aslagx(1,icsr),sfd84(icsr)
!      write (*,*)
!^^^ tmp end

      call sbsfdi                                                       &
     & (aslagm(1,icsr), as0ags(1,icsr), aslagn(1,icsr),                 &
     & aslagx(1,icsr), 2.0, sfd200(icsr))
    5  continue
!
      do 20 j = 0, jmax
      do 10 i = 0, imax

!     determine subregion (at present only 1 subregion)
!     input variables to grid cells
      icsr = csr(i,j)
      sf1  (i,j) = sfd1(icsr)
      sf10 (i,j) = sfd10(icsr)
      sf84 (i,j) = sfd84(icsr)
      sf200(i,j) = sfd200(icsr)
!     edit ljh - 1-22-04
      svroc(i,j) = asvroc(1,icsr)    ! if ifc has surface rock, 1st index maybe 0.
!
      szcr(i,j)  = aszcr  (icsr)
      sfcr(i,j)  = asfcr  (icsr)
      smlos(i,j) = asmlos (icsr)
      sflos(i,j) = asflos (icsr)
!
      szrgh(i,j) = aszrgh (icsr)

      !initialize RR values for each grid cell
      slrr(i,j)  = aslrr (icsr)

      if (slrr(i,j) < SLRR_MIN) then
          slrr(i,j) = SLRR_MIN
      else if (slrr(i,j) > SLRR_MAX) then
          slrr(i,j) = SLRR_MAX
      endif

      dmlos(i,j) = 0.0
      smaglos(i,j) = 0.0
      smaglosmx(i,j) = 0.0
      sf84mn(i,j) = 0.0
!
!     initialize output array- now in sbigrd
!      egt(i,j)    = 0
!      egtss(i,j)  = 0
!      egt10(i,j)  = 0
!
   10 continue
   20 continue
!
      return
      end
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
