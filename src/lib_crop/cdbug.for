!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine  cdbug(isr,slay)

!     + + + PURPOSE + + +
!    This program prints out many of the global variables before
!    and after the call to CROP provide a comparison of values
!    which may be changed by CROP

!    author: John Tatarko
!    version: 09/01/92

!     + + + KEY WORDS + + +
!     wind, erosion, hydrology, tillage, soil, crop, decomposition
!     management

!     + + + GLOBAL COMMON BLOCKS + + +

      include 'p1werm.inc'
      include 'm1subr.inc'
      include 'm1flag.inc'
      include 's1layr.inc'
      include 's1phys.inc'
      include 's1agg.inc'
      include 's1dbh.inc'
      include 's1dbc.inc'
      include 's1sgeo.inc'
      include 'c1db1.inc'
      include 'c1db2.inc'
      include 'c1glob.inc'
      include 'c1info.inc'
      include 'd1glob.inc'
      include 'w1clig.inc'
      include 'w1wind.inc'
      include 'h1et.inc'
      include 'h1hydro.inc'
      include 'h1db1.inc'
      include 'h1temp.inc'
      include 'file.inc'

!     + + + LOCAL COMMON BLOCKS + + +
      include 'crop/cenvr.inc'
      include 'crop/tcdbug.inc'
      include 'main/main.inc'

!     + + + LOCAL VARIABLES + + +

      integer cd, cm, cy
      integer        isr
      integer        l
      integer        slay

!     + + + LOCAL DEFINITIONS + + +

!   cd        - The current day of simulation month.
!   cm        - The current month of simulation year.
!   cy        - The current year of simulation run.
!   isr       - This variable holds the subregion index.
!   l         - This variable is an index on soil layers.

!     + + + SUBROUTINES CALLED + + +

!     + + + FUNCTIONS CALLED + + +

!     + + + UNIT NUMBERS FOR INPUT/OUTPUT DEVICES + + +
!     * = screen and keyboard
!    27 = debug CROP

!     + + + DATA INITIALIZATIONS + + +

      if (am0cif  .eqv. .true.) then
          tday = -1
          tmo = -1
          tyr = -1
          tisr = -1
      end if
      call caldatw (cd, cm, cy)

!     + + + INPUT FORMATS + + +

!     + + + OUTPUT FORMATS + + +
 2030 format ('**',1x,2(i2,'/'),i4,'    After  call to CROP         Subr&
     &egion No. ',i3)
 2031 format ('**',1x,2(i2,'/'),i4,'    Before call to CROP         Subr&
     &egion No. ',i3)
 2032 format (' awzdpt  awtdmx  awtdmn  aweirr  awudmx  awudmn ',       &
     &        ' awtdpt  awadir  awhrmx   awrrh ')
 2038 format (f7.2,9f8.2)
! 2045 format ('Subregion Number',i3)
 2050 format ('amrslp(',i2,') acftcv(',i2,') acrlai(',i2,')',           &
     &        ' aczrtd(',i2,') admf(',i2,') ahfwsf(',i2,')',            &
     &        ' ac0nam(',i2,')')
 2051 format (2f10.2,2f10.5,2x,f10.2,f10.2,3x,a12)
 2052 format ('actdtm(',i2,') sum-phu(',i2,') acmst(',i2,')',           &
     &        '  acmrt(',i2,')  ahzeta      ahzetp     ',               &
     &        ' ahzpta ')
 2053 format (i10, 4f10.2,2f12.2)
 2054 format ('      ahzea     ahzep    ahzptp ',                       &
     &        ' actmin(',i2,') actopt(',i2,') as0rrk(',i2,')',          &
     &        ' aslrr(',i2,')')
 2055 format (2f10.2,2f10.3,3f12.2)
 2056 format('layer aszlyt  ahrsk ahrwc ahrwcs ahrwca',                 &
     &       ' ahrwcf ahrwcw ah0cb aheaep ahtsmx ahtsmn')
 2060 format (i4,1x,f7.2,1x,e7.1,f6.2,4f7.2,f6.2,3f7.2)
 2065 format(' layer  asfsan asfsil asfcla asfom asdblk aslagm  as0ags',&
     &       ' aslagn  aslagx  aseags')
 2070 format (i4,2x,3f7.2,f7.3,2f7.2,f8.2,f7.3,2f8.2)

!     + + + END SPECIFICATIONS + + +

!          write weather cligen and windgen variables
      if ((cd .eq. tday) .and. (cm .eq. tmo) .and. (cy .eq. tyr) .and.  &
     &   (isr .eq. tisr)) then
         write(luocdb,2030) cd,cm,cy,isr
      else
         write(luocdb,2031) cd,cm,cy,isr
      end if
      write(luocdb,2032)
      write(luocdb,2038) awzdpt, awtdmx, awtdmn, aweirr, awudmx, awudmn,&
     &               awtdpt, awadir, awhrmx, awrrh

!      write(luocdb,2045) isr

      write(luocdb,2050) isr, isr, isr, isr, isr, isr, isr
! admf(isr) is not dimensioned correctly anymore - LEW 04/23/99
! just commenting it out for now since it is a debug routine
!      write(luocdb,2051) amrslp(isr), acftcv(isr), acrlai(isr), aczrtd(isr),
!     &               admf(isr), ahfwsf(isr), ac0nam(isr)
      write(luocdb,2052) isr, isr, isr, isr
      write(luocdb,2053)                                                &
     &               actdtm(isr), acthucum(isr), acmst(isr), acmrt(isr),&
     &               ahzeta, ahzetp, ahzpta
      write(luocdb,2054) isr, isr, isr, isr
      write(luocdb,2055) ahzea, ahzep, ahzptp, actmin(isr),             &
     &               actopt(isr), as0rrk(isr), aslrr(isr)
      write(luocdb,2056)

      do 200 l = 1,slay
         write(luocdb,2060) l,aszlyt(l,isr), ahrsk(l,isr), ahrwc(l,isr),&
     &                  ahrwcs(l,isr), ahrwca(l,isr), ahrwcf(l,isr),    &
     &                  ahrwcw(l,isr), ah0cb(l,isr), aheaep(l,isr),     &
     &                  ahtsmx(l,isr), ahtsmn(l,isr)
  200 continue
         write(luocdb,2065)

      do 300 l=1,slay
         write(luocdb,2070) l,asfsan(l,isr),asfsil(l,isr),asfcla(l,isr),&
     &                  asfom(l,isr), asdblk(l,isr),                    &
     &                  aslagm(l,isr), as0ags(l,isr), aslagn(l,isr),    &
     &                  aslagx(l,isr), aseags(l,isr)
  300 continue

      tisr = isr
      tday = cd
      tmo = cm
      tyr = cy

      return
      end

