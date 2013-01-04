!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine  ddbug(isr,slay)

!     + + + PURPOSE + + +
!    This program prints out many of the global variables before
!    and after the call to DECOMP provide a comparison of values
!    which may be changed by DECOMP

!    author:  John Tatarko
!    version: 09/01/92
!    modified on 9/18/95 ANH
!    removed unnecessary variables and added variables which relate
!    directly to the ddbug.forosition submodel ANH
!
!     + + + KEY WORDS + + +
!     wind, erosion, hydrology, tillage, soil, crop, ddbug.forosition
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
      include 'file.inc'

!     + + + LOCAL COMMON BLOCKS + + +
      include 'decomp/decomp.inc'
      include 'decomp/tddbug.inc'
      include 'main/main.inc'

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
!    28 = debug DECOMP

!     + + + DATA INITIALIZATIONS + + +

      if (am0ifl  .eqv. .true.) then
          tday = -1
          tmo = -1
          tyr = -1
          tisr = -1
      end if
      call caldatw (cd, cm, cy)

!     + + + INPUT FORMATS + + +

!     + + + OUTPUT FORMATS + + +
 2030 format ('**',1x,2(i2,'/'),i4,'    After  call to DECOMP       Subr&
     &egion No. ',i3)
 2031 format ('**',1x,2(i2,'/'),i4,'    Before call to DECOMP       Subr&
     &egion No. ',i3)
 2060 format (i4,1x,f7.2,1x,e7.1,f6.2,4f7.2,f6.2,3f7.2)
 2065 format(' layer  admbgz(p#1) admbgz(p#2) admbgz(p#3) ',            &
     &       ' admrtz(p#1) admrtz(p#2) admrtz(p#3)')
 2066 format(' layer  admbgz(p#1) admbgz(p#2) admbgz(p#3) admrtz(p#1) ',&
     &       ' admrtz(p#2) admrtz(p#3)')
 2067 format(' layer  cumddg(p#1) cumddg(p#2) cumddg(p#3)')
 2068 format(' cumdds(p#1) cumdds(p#2) cumdds(p#3) cumddf(p#1) ',       &
     &       ' cumddf(p#2) cumddf(p#3)')
 2069 format(' admst(p#1) admst(p#2) admst(p#3) ',                      &
     &       '  admf(p#1)  admf(p#2)  admf(p#3)  ')
 2070 format (i4,2x,6(3x,f7.3))
 2071 format (i4,1x,6(4x,f7.3))
 2072 format (i4,1x,3(4x,f7.3))
 2073 format (6(4x,f7.3))
 2074 format (6(4x,f7.3))

!     + + + END SPECIFICATIONS + + +

!          write weather cligen and windgen variables
!      write(*,*) 'd1',cd,cm,cy,isr,tday,tmo,tyr,tisr
      if ((cd .eq. tday) .and. (cm .eq. tmo) .and. (cy .eq. tyr) .and.  &
     &   (isr .eq. tisr)) then
         write(luoddb,2030) cd,cm,cy,isr
      else
         write(luoddb,2031) cd,cm,cy,isr
      end if

!      do 200 l = 1,slay
!         write(luoddb,2060) l, aszlyt(l,isr), ahrsk(l,isr), ahrwc(l,isr),
!     &                  ahrwcs(l,isr), ahrwca(l,isr), ahrwcf(l,isr),
!     &                  ahrwcw(l,isr), ah0cb(l,isr), aheaep(l,isr),
!     &                  ahtsmx(l,isr), ahtsmn(l,isr)
!  200 continue
         write(luoddb,*)
         write(luoddb,2065)
      do 300 l=1,slay
         write(luoddb,2070)                                             &
     &          l,admbgz(l,1,isr),admbgz(l,2,isr),admbgz(l,3,isr),      &
     &            admrtz(l,1,isr),admrtz(l,2,isr),admrtz(l,3,isr)
  300 continue
!         write(luoddb,2066)
!      do 310 l=1,slay
!         write(luoddb,2071) l,admbgz(l,1,isr),admbgz(l,2,isr),admbgz(l,3,isr),
!     &                  admrtz(l,1,isr),admrtz(l,2,isr),admrtz(l,3,isr)
!  310 continue

         write(luoddb,2067)
      do 320 l=1,slay
         write(luoddb,2072) l,cumddg(l,1,isr),cumddg(l,2,isr),          &
     &                  cumddg(l,3,isr)
  320 continue

         write(luoddb,2068)
         write(luoddb,2073) cumdds(1,isr),cumdds(2,isr),cumdds(3,isr),  &
     &                  cumddf(1,isr),cumddf(2,isr),cumddf(3,isr)
         write(luoddb,2069)
         write(luoddb,2074) admst(1,isr),admst(2,isr),admst(3,isr),     &
     &                  admf(1,isr),admf(2,isr),admf(3,isr)
      tisr = isr
      tday = cd
      tmo = cm
      tyr = cy
!       write(*,*) 'd2',tday,tmo,tyr,tisr

      return
      end

