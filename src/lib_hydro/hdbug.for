!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine  hdbug(isr,slay)

!     + + + PURPOSE + + +
!    This program prints out many of the global variables before
!    and after the call to HYDROLOGY provide a comparison of values
!    which may be changed by HYDDROLOGY

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
      include 's1surf.inc'
      include 's1phys.inc'
      include 's1agg.inc'
      include 's1dbh.inc'
      include 's1dbc.inc'
      include 's1sgeo.inc'
      include 'c1gen.inc'
      include 'c1glob.inc'
      include 'd1glob.inc'
      include 'w1clig.inc'
      include 'w1wind.inc'
      include 'h1et.inc'
      include 'h1hydro.inc'
      include 'h1scs.inc'
      include 'h1db1.inc'
      include 'h1temp.inc'

!     + + + LOCAL COMMON BLOCKS + + +
      include 'main/main.inc'
      include 'hydro/thdbug.inc'

!     + + + LOCAL VARIABLES + + +

      integer cd, cm, cy, isr, l, slay


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
!    25 = debug HYDRO

!     + + + DATA INITIALIZATIONS + + +

         call caldatw (cd, cm, cy)


      if (am0ifl  .eqv. .true.) then
          tday = -1
          tmo = -1
          tyr = -1
          tisr = -1
      end if


!     + + + INPUT FORMATS + + +

!     + + + OUTPUT FORMATS + + +
 2030 format ('**',1x,2(i2,'/'),i4,'    After  call to HYDRO          Su&
     &bregion No. ',i3)
 2031 format ('**',1x,2(i2,'/'),i4,'    Before call to HYDRO          Su&
     &bregion No. ',i3)
 2032 format (' awzdpt  awtdmx  awtdmn  aweirr  awudmx  awudmn ',       &
     &        ' awtdpt  awadir  awhrmx   awrrh ')
 2038 format (f7.2,9f8.2)
! 2045 format ('Subregion Number',i3)
 2050 format ('amrslp(',i2,') acftcv(',i2,') acrlai(',i2,')',           &
     &        ' aczrtd(',i2,') admf(',i2,') ahfwsf(',i2,')',            &
     &        ' ahzper(',i2,')')
 2051 format (2f10.2,2f10.5,2x,f10.2,f10.2,f12.2)
 2052 format ('ahzrun(',i2,') ahzirr(',i2,') ahzsno(',i2,')',           &
     &        ' ahzsmt(',i2,')  ahzeta      ahzetp     ',               &
     &        ' ahzpta ')
 2053 format (5f10.2,2f12.2)
 2054 format ('      ahzea     ahzep    ahzptp ',                       &
     &        ' ah0cng(',i2,') ah0cnp(',i2,') as0rrk(',i2,')',          &
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
         write(25,2030) cd,cm,cy,isr
      else
         write(25,2031) cd,cm,cy,isr
      end if
      write(25,2032)
      write(25,2038) awzdpt, awtdmx, awtdmn, aweirr, awudmx, awudmn,    &
     &               awtdpt, awadir, awhrmx, awrrh

!      write(25,2045) isr

      write(25,2050) isr, isr, isr, isr, isr, isr, isr
! admf(isr) is not dimensioned correctly anymore - LEW 04/23/99
! just commenting it out for now since it is a debug routine
!      write(25,2051) amrslp(isr), acftcv(isr), acrlai(isr), aczrtd(isr),
!     &               admf(isr), ahfwsf(isr), ahzper(isr)
      write(25,2052) isr, isr, isr, isr
      write(25,2053) ahzrun(isr), ahzirr(isr), ahzsno(isr), ahzsmt(isr),&
     &               ahzeta, ahzetp, ahzpta
      write(25,2054) isr, isr, isr, isr
      write(25,2055) ahzea, ahzep, ahzptp, ah0cng(isr),                 &
     &               ah0cnp(isr), as0rrk(isr), aslrr(isr)
      write(25,2056)

      do 200 l = 1,slay
         write(25,2060) l, aszlyt(l,isr), ahrsk(l,isr), ahrwc(l,isr),   &
     &                  ahrwcs(l,isr), ahrwca(l,isr), ahrwcf(l,isr),    &
     &                  ahrwcw(l,isr), ah0cb(l,isr), aheaep(l,isr),     &
     &                  ahtsmx(l,isr), ahtsmn(l,isr)
  200 continue
         write(25,2065)

      do 300 l=1,slay
         write(25,2070) l, asfsan(l,isr), asfsil(l,isr), asfcla(l,isr), &
     &                  asfom(l,isr), asdblk(l,isr),                    &
     &                  aslagm(l,isr), as0ags(l,isr), aslagn(l,isr),    &
     &                  aslagx(l,isr), aseags(l,isr)
  300 continue

!
      tisr = isr
      tday = cd
      tmo = cm
      tyr = cy

      return
      end

