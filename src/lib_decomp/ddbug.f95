!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine  ddbug(isr, slay, residue)

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

      use weps_interface_defs
      use datetime_mod, only: get_simdate
      use file_io_mod, only: luoddb
      use biomaterial, only: biomatter
      use debug_mod, only: tddbug

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
      include 'w1clig.inc'
      include 'h1hydro.inc'
      include 'h1scs.inc'
      include 'h1db1.inc'
      include 'h1temp.inc'

!     + + + ARGUMENT DECLARATIONS + + +
      integer isr, slay
      type(biomatter), dimension(:), intent(in) :: residue

!     + + + LOCAL COMMON BLOCKS + + +
      include 'main/main.inc'

!     + + + LOCAL VARIABLES + + +

      integer cd, cm, cy, l

!     + + + LOCAL DEFINITIONS + + +

!   cd        - The current day of simulation month.
!   cm        - The current month of simulation year.
!   cy        - The current year of simulation run.
!   isr       - This variable holds the subregion index.
!   l         - This variable is an index on soil layers.

!     + + + SUBROUTINES CALLED + + +

!     + + + FUNCTIONS CALLED + + +

!     + + + DATA INITIALIZATIONS + + +

      if (am0ifl  .eqv. .true.) then
          tddbug(isr)%tday = -1
          tddbug(isr)%tmo = -1
          tddbug(isr)%tyr = -1
      end if
      call get_simdate (cd, cm, cy)

!     + + + INPUT FORMATS + + +

!     + + + OUTPUT FORMATS + + +
 2030 format ('**',1x,2(i2,'/'),i4,'    After  call to DECOMP       Subr&
     &egion No. ',i3)
 2031 format ('**',1x,2(i2,'/'),i4,'    Before call to DECOMP       Subr&
     &egion No. ',i3)
 !2060 format (i4,1x,f7.2,1x,e7.1,f6.2,4f7.2,f6.2,3f7.2)
 2065 format(' layer  admbgz(p#1) admbgz(p#2) admbgz(p#3) ',            &
     &       ' admrtz(p#1) admrtz(p#2) admrtz(p#3)')
 !2066 format(' layer  admbgz(p#1) admbgz(p#2) admbgz(p#3) admrtz(p#1) ',&
 !    &       ' admrtz(p#2) admrtz(p#3)')
 2067 format(' layer  cumddg(p#1) cumddg(p#2) cumddg(p#3)')
 2068 format(' cumdds(p#1) cumdds(p#2) cumdds(p#3) cumddf(p#1) ',       &
     &       ' cumddf(p#2) cumddf(p#3)')
 2069 format(' admst(p#1) admst(p#2) admst(p#3) ',                      &
     &       '  admf(p#1)  admf(p#2)  admf(p#3)  ')
 2070 format (i4,2x,6(3x,f7.3))
! 2071 format (i4,1x,6(4x,f7.3))
 2072 format (i4,1x,3(4x,f7.3))
 2073 format (6(4x,f7.3))
 2074 format (6(4x,f7.3))

!     + + + END SPECIFICATIONS + + +

!          write weather cligen and windgen variables
!      write(*,*) 'd1',cd,cm,cy,isr,tddbug(isr)%tday,tddbug(isr)%tmo,tddbug(isr)%tyr
      if ((cd .eq. tddbug(isr)%tday) .and. (cm .eq. tddbug(isr)%tmo) .and. (cy .eq. tddbug(isr)%tyr)) then
         write(luoddb(isr),2030) cd,cm,cy,isr
      else
         write(luoddb(isr),2031) cd,cm,cy,isr
      end if

!      do 200 l = 1,slay
!         write(luoddb(isr),2060) l, aszlyt(l,isr), ahrsk(l,isr), ahrwc(l,isr),
!     &                  ahrwcs(l,isr), ahrwca(l,isr), ahrwcf(l,isr),
!     &                  ahrwcw(l,isr), ah0cb(l,isr), aheaep(l,isr),
!     &                  ahtsmx(l,isr), ahtsmn(l,isr)
!  200 continue
         write(luoddb(isr),*)
         write(luoddb(isr),2065)
      do 300 l=1,slay
         write(luoddb(isr),2070)                                        &
     &          l,residue(1)%deriv%mbgz(l),residue(2)%deriv%mbgz(l),residue(3)%deriv%mbgz(l), &
     &            residue(1)%deriv%mrtz(l),residue(2)%deriv%mrtz(l),residue(3)%deriv%mrtz(l)
  300 continue
!         write(luoddb(isr),2066)
!      do 310 l=1,slay
!         write(luoddb(isr),2071) l,residue(1)%deriv%mbgz(l),residue(2)%deriv%mbgz(l),     &
!     &                           residue(3)%deriv%mbgz(l),                       &
!     &                  residue(1)%deriv%mrtz(l),residue(2)%deriv%mrtz(l),residue(3)%deriv%mrtz(l)
!  310 continue

         write(luoddb(isr),2067)
      do 320 l=1,slay
         write(luoddb(isr),2072) l,residue(1)%decomp%cumddg(l),residue(2)%decomp%cumddg(l), residue(3)%decomp%cumddg(l)
  320 continue

         write(luoddb(isr),2068)
         write(luoddb(isr),2073) residue(1)%decomp%cumdds,residue(2)%decomp%cumdds, residue(3)%decomp%cumdds, &
     &                  residue(1)%decomp%cumddf,residue(2)%decomp%cumddf,residue(3)%decomp%cumddf
         write(luoddb(isr),2069)
         write(luoddb(isr),2074) residue(1)%deriv%mst,residue(2)%deriv%mst,residue(3)%deriv%mst, &
     &                  residue(1)%deriv%mf,residue(2)%deriv%mf,residue(3)%deriv%mf
      tddbug(isr)%tday = cd
      tddbug(isr)%tmo = cm
      tddbug(isr)%tyr = cy
!       write(*,*) 'd2',tddbug(isr)%tday,tddbug(isr)%tmo,tddbug(isr)%tyr

      return
      end

