!$Author$
!$Date$
!$Revision$
!$HeadURL$
!**********************************************************************
!     subroutine sb2out
!**********************************************************************
      subroutine sb2out (jj, nn, hr, ws, wdir, o_unit)
!
!     + + + PURPOSE + + +
!     To print to file tst.out some key variables used in erosion
!     use wind direction of 270 to see output along downwind direction
!
!     + + + ARGUEMENT DECLARATIONS + + +
      real ws, wdir, hr
      integer  jj, nn, o_unit
!
!     + + + ARGUMENT DEFINITIONS + + +
!     anemht = anemometer height (m)
!     awzzo =  aerodynamic roughness at anemometer (mm)
!     wzz0  =  aerodynamic roughness length (mm) 
!     awu   =  wind speed (m/s)
!     wus   =  friction velocity (m/s)
!     wust  =  threshold friction velocity (m/s)
!     wzoflg = flag to showing anemometer at field (1) or wx sta (0)
!     o_unit= Unit number for output file
!
!     + + + GLOBAL COMMON BLOCKS + + +

      include 'p1werm.inc'
      include 'h1db1.inc'
      include 'p1const.inc'
      include 'm1sim.inc'
!
!     + + + LOCAL COMMON BLOCKS + + +
!

      include 'erosion/s2agg.inc'
      include 'erosion/s2surf.inc'
      include 'erosion/s2sgeo.inc'
      include 'erosion/w2wind.inc'
      include 'erosion/m2geo.inc'
      include 'erosion/e2erod.inc'
      include 'erosion/e3grid.inc'
!
!     + + + LOCAL VARIABLES + + +

      real egavg(mngdpt)
      integer m, n, k, icsr
      integer initflag, ipd, npd
      save    initflag, ipd, npd

      integer yr, mo, da
      real    hhrr, tims
      save    yr, mo, da, hhrr, tims
      integer i,j

!     outflag = 0 - print heading output, 1 - no more heading

!     + + + END SPECIFICATIONS + + +

!     define index of current subregions
      icsr = 1

!     output headings?
      if (initflag .eq. 0) then

        ipd = 0
        npd = nn * ntstep

        tims = 3600*24/ntstep !seconds in each emission period
        call caldatw (da, mo, yr) !Set day, month and year
        hhrr = 0                    !Pre-set hhrr so we get start of period times

!        write (o_unit,*)
!        write (o_unit,*) 'OUT PUT from sb2out'
!        write (o_unit,*)

        initflag = 1    !     turn off heading output
      endif
      ipd = ipd + 1
      if (hhrr .ge. 24) then
         hhrr = tims/3600
         call caldatw (da, mo, yr)
      else
         hhrr = hhrr + tims/3600
      endif

      call caldatw (da, mo, yr)
!      write (o_unit, fmt="(a, 3(i3), f5.2, i3)")                        &
!     &       ' day mon yr hhrr update_period ', da, mo, yr, hhrr, jj

      write (o_unit, fmt="(a, i5, 2(i3), f7.3, 4(i4))")                 &
     & ' yr mon day hr upd_pd jj nn(subpd) npd (sbqout 2)',             &
     &   yr,mo, da, hr,ipd,   jj,nn,       npd


      write(o_unit,fmt="(' <grid data> |',i5,2(i3),f7.3,3('|',a))")     &
     &  yr, mo, da, hr,                                                 &
     & 'Cumulative Total Soil Loss',                                    &
     & 'soil loss', '(kg/m^2)'
      do j = jmax-1, 1, -1
        write (o_unit, fmt="(500f12.4)") (egt(i,j), i = 1, imax-1)
      end do
      write(o_unit,fmt="(' </grid data>')")

      write(o_unit,fmt="(' <grid data> |',i5,2(i3),f7.3,3('|',a))")     &
     &  yr, mo, da, hr,                                                 &
     & 'Cumulative Saltation/Creep Soil Loss',                          &
     & 'salt/creep soil loss', '(kg/m^2)'
      do j = jmax-1, 1, -1
        write (o_unit, fmt="(500f12.4)")(egt(i,j)-egtss(i,j),i=1,imax-1)
      end do
      write(o_unit,fmt="(' </grid data>')")

      write(o_unit,fmt="(' <grid data> |',i5,2(i3),f7.3,3('|',a))")     &
     &  yr, mo, da, hr,                                                 &
     & 'Cumulative Suspension Soil Loss',                               &
     & 'suspension soil loss', '(kg/m^2)'
      do j = jmax-1, 1, -1
        write (o_unit, fmt="(500f12.4)") (egtss(i,j), i = 1, imax-1)
      end do
      write(o_unit,fmt="(' </grid data>')")
      write(o_unit,fmt="(' <grid data> |',i5,2(i3),f7.3,3('|',a))")     &
     &  yr, mo, da, hr,                                                 &
     & 'Cumulative PM10 Soil Loss',                                     &
     & 'PM10 soil loss', '(kg/m^2)'
      do j = jmax-1, 1, -1
        write (o_unit, fmt="(500f12.6)") (egt10(i,j), i = 1, imax-1)
      end do
      write(o_unit,fmt="(' </grid data>')")

!!      if (ipd .eq. npd) then
!Grid Cell Surface properties
      write(o_unit,fmt="(' <grid data> |',i5,2(i3),f7.3,3('|',a))")     &
     &  yr, mo, da, hr,                                                 &
     & 'Surface Random Roughness (after)', 'random roughness', '(mm)'
      do j = jmax-1, 1, -1
        write (o_unit, fmt="(500f12.4)") (slrr(i,j), i = 1, imax-1)
      end do
      write(o_unit,fmt="(' </grid data>')")
      write(o_unit,fmt="(' <grid data> |',i5,2(i3),f7.3,3('|',a))")     &
     &  yr, mo, da, hr,                                                 &
     & 'Surface Oriented Roughness (after)', 'ridge height', '(mm)'
      do j = jmax-1, 1, -1
        write (o_unit, fmt="(500f12.4)") (szrgh(i,j), i = 1, imax-1)
      end do
      write(o_unit,fmt="(' </grid data>')")
      write(o_unit,fmt="(' <grid data> |',i5,2(i3),f7.3,3('|',a))")     &
     &  yr, mo, da, hr,                                                 &
     & 'Surface Rock (after)',                                          &
     & 'surface volume rock fraction', '(m^3/m^3)'
      do j = jmax-1, 1, -1
        write (o_unit, fmt="(500f12.4)") (svroc(i,j), i = 1, imax-1)
      end do
      write(o_unit,fmt="(' </grid data>')")
      write (o_unit,*)

      write(o_unit,fmt="(' <grid data> |',i5,2(i3),f7.3,3('|',a))")     &
     &  yr, mo, da, hr,                                                 &
     & 'Soil Agg. Size < 0.01','mass fraction < 0.01 mm size','(fract.)'
      do j = jmax-1, 1, -1
        write (o_unit, fmt="(500f12.4)") (sf1(i,j), i = 1, imax-1)
      end do
      write(o_unit,fmt="(' </grid data>')")
      write(o_unit,fmt="(' <grid data> |',i5,2(i3),f7.3,3('|',a))")     &
     &  yr, mo, da, hr,                                                 &
     & 'Soil Agg. Size < 0.1', 'mass fraction < 0.1 mm size', '(fract.)'
      do j = jmax-1, 1, -1
        write (o_unit, fmt="(500f12.4)") (sf10(i,j), i = 1, imax-1)
      end do
      write(o_unit,fmt="(' </grid data>')")
      write(o_unit,fmt="(' <grid data> |',i5,2(i3),f7.3,3('|',a))")     &
     &  yr, mo, da, hr,                                                 &
     & 'Soil Agg. Size < 0.84','mass fraction < 0.84 mm size','(fract.)'
      do j = jmax-1, 1, -1
        write (o_unit, fmt="(500f12.4)") (sf84(i,j), i = 1, imax-1)
      end do
      write(o_unit,fmt="(' </grid data>')")
      write(o_unit,fmt="(' <grid data> |',i5,2(i3),f7.3,3('|',a))")     &
     &  yr, mo, da, hr,                                                 &
     & 'Soil Agg. Size < 2.0', 'mass fraction < 2.0 mm size', '(fract.)'
      do j = jmax-1, 1, -1
        write (o_unit, fmt="(500f12.4)") (sf200(i,j), i = 1, imax-1)
      end do
      write(o_unit,fmt="(' </grid data>')")
      write(o_unit,fmt="(' <grid data> |',i5,2(i3),f7.3,3('|',a))")     &
     &  yr, mo, da, hr,                                                 &
     & 'Soil Agg. Size for u* to be the thresh. friction velocity (af)',&
     &'"effective" mass fraction < 0.84 mm size', '(fract.)'
      do j = jmax-1, 1, -1
        write (o_unit, fmt="(500f12.4)") (sf84mn(i,j), i = 1, imax-1)
      end do
      write(o_unit,fmt="(' </grid data>')")
      write(o_unit,fmt="(' <grid data> |',i5,2(i3),f7.3,3('|',a))")     &
     &  yr, mo, da, hr,                                                 &
     & 'Mobile soil removable from aggregated surface (after)',         &
     & 'mass removable', '(kg/m^2)'
      do j = jmax-1, 1, -1
        write (o_unit, fmt="(500f12.4)") (smaglos(i,j), i = 1, imax-1)
      end do
      write(o_unit,fmt="(' </grid data>')")
      write(o_unit,fmt="(' <grid data> |',i5,2(i3),f7.3,3('|',a))")     &
     &  yr, mo, da, hr,                                                 &
     & 'Change in mobile soil on aggregated surface (after)',           &
     & 'net mass change', '(kg/m^2)'
      do j = jmax-1, 1, -1
        write (o_unit, fmt="(500f12.4)") (dmlos(i,j), i = 1, imax-1)
      end do
      write(o_unit,fmt="(' </grid data>')")

! Crust properties
      write(o_unit,fmt="(' <grid data> |',i5,2(i3),f7.3,3('|',a))")     &
     &  yr, mo, da, hr,                                                 &
     & 'Consolidated crust thickness (after)', 'crust thickness', '(mm)'
      do j = jmax-1, 1, -1
        write (o_unit, fmt="(500f12.4)") (szcr(i,j), i = 1, imax-1)
      end do
      write(o_unit,fmt="(' </grid data>')")
      write(o_unit,fmt="(' <grid data> |',i5,2(i3),f7.3,3('|',a))")     &
     &  yr, mo, da, hr,                                                 &
     & 'Fraction of Surface covered with Crust (after)',                &
     & 'crust cover','(fract.)'
      do j = jmax-1, 1, -1
        write (o_unit, fmt="(500f12.4)") (sfcr(i,j), i = 1, imax-1)
      end do
      write(o_unit,fmt="(' </grid data>')")
      write(o_unit,fmt="(' <grid data> |',i5,2(i3),f7.3,3('|',a))")     &
     &  yr, mo, da, hr,                                                 &
     &'Fraction of Crusted Surface covered with Loose Erodible Soil(a)', &
     & 'loose erodible material', '(fract.)'
      do j = jmax-1, 1, -1
        write (o_unit, fmt="(500f12.4)") (sflos(i,j), i = 1, imax-1)
      end do
      write(o_unit,fmt="(' </grid data>')")
      write(o_unit,fmt="(' <grid data> |',i5,2(i3),f7.3,3('|',a))")     &
     &  yr, mo, da, hr,                                                 &
     & 'Mass of Loose Erodible Soil on Crusted Surface (after)',        &
     & 'loose erodible material', '(kg/m^2)'
      do j = jmax-1, 1, -1
        write (o_unit, fmt="(500f12.4)") (smlos(i,j), i = 1, imax-1)
      end do
      write(o_unit,fmt="(' </grid data>')")
      write (o_unit,*)

!!      endif

!      write (o_unit,20)  anemht,wzoflg,ws,kbr
!
!     set output increment
      m = (imax - 1)/8
      m = max0(m,1)
      n = (jmax-1)/2
      n = max(n,1) 
!
!     initialize avg erosion variable
      do 3 j = 1, imax
        egavg(j) = 0.0
    3 continue
!
!     calc. avg erosion over a given field length
!      do 5 j = 1, (imax-1), m
!      do 4 k = 1, j
!         egavg(j) = egavg(j) + egt(k,n)
!    4 continue
!         egavg(j) = egavg(j)/j
!    5 continue
    ! changed 1-12-07 LH
       do 5  i = 1,(imax-1)
       !average over y-direction
       do 4  j = 1, (jmax-1)
          egavg(i) = egavg(i) + egt(i,j)/(jmax-1)
    4  continue
    5  continue
       !average over x-direction
       do 6 i = 2, (imax-1)
         egavg(i) = ((i-1)*(egavg(i-1))+egavg(i))/i
    6  continue

!      write (o_unit,*)  'sb2out output'

!      write (o_unit,18)  (k , k=1,(imax-1),m), n
!      write (o_unit,21) (egt(k,n),k=1,(imax-1),m)
!      write (o_unit,22) (egtss(k,n),k=1,(imax-1),m)
!      write (o_unit,23) ((egtss(k,n)/(egt(k,n)+0.00001)),               &
!     &                   k=1,(imax-1),m)
! changed 3-12-07 LH
      write (o_unit,35) (egavg(k), k=1,(imax-1))
!      write (o_unit,36)
!      write (o_unit,37) (smaglos(k,n),k=1,(imax-1),m)
      write (o_unit,*) '----------------------------------------------'

!     output formats
!   10 format (1x, 'anemht wzoflg   ws    kbr')
   18 format(1x, 'i..n,j', 20i6)
!   15 format (1x, ' (m)          (m/s)    ')
   20 format (1x, 'anemht wzoflg ws kbr',                               &
     &  f6.0, i6, f8.2, i6)

   21 format (1x, 'egt=   ', 20f6.2)
   22 format (1x, 'egtss= ', 20f6.2)
   23 format (1x, 'egtss/egt=', 20f6.2)
   35 format (1x, 'egavg = ', 20f6.2)
   36 format (1x, 'corrected for Sf12')
   37 format (1x, 'smaglos=', 20f6.2)
   24 format (1x, 'sf84=', 20f6.2)

   25 format (1x, 'szcr=', 20f6.2)
   26 format (1x, 'sfcr=', 20f6.2)
   27 format (1x, 'smlos=', 20f6.2)
   28 format (1x, 'sflos=', 20f6.2)

   29 format (1x, 'szrgh=', 20f6.2)
   30 format (1x, 'slrr=', 20f6.2)

   31 format (1x, 'ahrwc0(icsr,12)', f6.2)
   32 format (1x, 'wus=', 20f6.2)
   33 format (1x, 'wusp=', 20f6.2)
   34 format (1x, 'wust=', 20f6.2)

      return
      end
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

