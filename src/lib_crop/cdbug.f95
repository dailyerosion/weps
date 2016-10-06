!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine  cdbug(isr, slay, crop, restot, h1et, subrsurf)

!     + + + PURPOSE + + +
!    This program prints out many of the global variables before
!    and after the call to CROP provide a comparison of values
!    which may be changed by CROP

!    author: John Tatarko
!    version: 09/01/92

!     + + + KEY WORDS + + +
!     wind, erosion, hydrology, tillage, soil, crop, decomposition
!     management

      use datetime_mod, only: get_simdate
      use file_io_mod, only: luocdb
      use biomaterial, only: biomatter, biototal
      use erosion_data_struct_defs, only: awadir, awhrmx, awudmx, awudmn
      use hydro_data_struct_defs, only: hydro_derived_et
      use erosion_data_struct_defs, only: subregionsurfacestate
      use climate_input_mod, only: cli_today

!     + + +   ARGUMENT DECLARATIONS + + +
      integer, intent(in) :: isr    ! subregion index
      integer, intent(in) :: slay   ! number of soil layers
      type(biomatter), intent(in) :: crop    ! structure containing full crop description
      type(biototal), intent(in) :: restot   ! structure containing residue totals
      type(hydro_derived_et), intent(in) :: h1et
      type(subregionsurfacestate), intent(inout) :: subrsurf  ! subregion surface conditions

!     + + + GLOBAL COMMON BLOCKS + + +
      include 'p1werm.inc'
      include 'm1subr.inc'
      include 'm1flag.inc'
      include 's1layr.inc'
      include 's1phys.inc'
      include 's1agg.inc'
      include 's1dbh.inc'
      include 's1dbc.inc'
      include 'c1db1.inc'
      include 'c1db2.inc'
      include 'h1hydro.inc'
      include 'h1db1.inc'
      include 'h1temp.inc'

!     + + + LOCAL COMMON BLOCKS + + +
      include 'crop/cenvr.inc'
      include 'crop/tcdbug.inc'
      include 'main/main.inc'

!     + + + LOCAL VARIABLES + + +

      integer cd, cm, cy
      integer        l

!     + + + LOCAL DEFINITIONS + + +

!   cd        - The current day of simulation month.
!   cm        - The current month of simulation year.
!   cy        - The current year of simulation run.
!   l         - This variable is an index on soil layers.

!     + + + SUBROUTINES CALLED + + +

!     + + + FUNCTIONS CALLED + + +

!     + + + UNIT NUMBERS FOR INPUT/OUTPUT DEVICES + + +
!     * = screen and keyboard
!    27 = debug CROP

!     + + + DATA INITIALIZATIONS + + +

      if (crop%growth%am0cif .eqv. .true.) then
          tday = -1
          tmo = -1
          tyr = -1
          tisr = -1
      end if
      call get_simdate (cd, cm, cy)

!     + + + INPUT FORMATS + + +

!     + + + OUTPUT FORMATS + + +
 2030 format ('**',1x,2(i2,'/'),i4,'    After  call to CROP         Subr&
     &egion No. ',i3)
 2031 format ('**',1x,2(i2,'/'),i4,'    Before call to CROP         Subr&
     &egion No. ',i3)
 2032 format (' awzdpt  awtdmx  awtdmn  aweirr  awudmx  awudmn ',       &
     &        ' awtdpt  awadir  awhrmx')
 2038 format (f7.2,9f8.2)
! 2045 format ('Subregion Number',i3)
 2050 format ('amrslp(',i2,') acftcv(',i2,') acrlai(',i2,')',           &
     &        ' aczrtd(',i2,') admftot(',i2,') ahfwsf(',i2,')',         &
     &        ' ac0nam(',i2,')')
 2051 format (2f10.2,2f10.5,2x,f10.2,f10.2,3x,a12)
 2052 format ('actdtm(',i2,') sum-phu(',i2,') acmst(',i2,')',  &
              '  acmrt(',i2,')  h1et%zeta h1et%zetp', &
              ' h1et%zpta ')
 2053 format (i10, 4f10.2,2f12.2)
 2054 format (' h1et%zea  h1et%zep h1et%zptp ', &
              ' actmin(',i2,') actopt(',i2,') aslrr(',i2,')')
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
         write(luocdb(isr),2030) cd,cm,cy,isr
      else
         write(luocdb(isr),2031) cd,cm,cy,isr
      end if
      write(luocdb(isr),2032)
      write(luocdb(isr),2038) cli_today%zdpt, cli_today%tdmx, cli_today%tdmn, cli_today%eirr, awudmx, &
     &                        awudmn, cli_today%tdpt, awadir, awhrmx

!      write(luocdb(isr),2045) isr

      write(luocdb(isr),2050) isr, isr, isr, isr, isr, isr, isr
      write(luocdb(isr),2051) amrslp(isr), crop%deriv%ftcv, crop%deriv%rlai,    &
     &               crop%geometry%zrtd, restot%mftot, ahfwsf(isr), crop%bname
      write(luocdb(isr),2052) isr, isr, isr, isr
      write(luocdb(isr),2053)                                           &
     &               actdtm(isr), crop%growth%thucum, crop%deriv%mst, crop%deriv%mrt,&
     &               h1et%zeta, h1et%zetp, h1et%zpta
      write(luocdb(isr),2054) isr, isr, isr, isr
      write(luocdb(isr),2055) h1et%zea, h1et%zep, h1et%zptp, actmin(isr),        &
     &               actopt(isr), subrsurf%aslrr
      write(luocdb(isr),2056)

      do 200 l = 1,slay
         write(luocdb(isr),2060) l,aszlyt(l,isr), ahrsk(l,isr),         &
     &                  ahrwc(l,isr),                                   &
     &                  ahrwcs(l,isr), ahrwca(l,isr), ahrwcf(l,isr),    &
     &                  ahrwcw(l,isr), ah0cb(l,isr), aheaep(l,isr),     &
     &                  ahtsmx(l,isr), ahtsmn(l,isr)
  200 continue
         write(luocdb(isr),2065)

      do 300 l=1,slay
         write(luocdb(isr),2070) l,asfsan(l,isr),asfsil(l,isr),         &
     &                  asfcla(l,isr), asfom(l,isr), asdblk(l,isr),     &
     &                  aslagm(l,isr), as0ags(l,isr), aslagn(l,isr),    &
     &                  aslagx(l,isr), aseags(l,isr)
  300 continue

      tisr = isr
      tday = cd
      tmo = cm
      tyr = cy

      return
      end

