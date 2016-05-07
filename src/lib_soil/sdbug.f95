!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine  sdbug(isr,slay, croptot, biotot, h1et)

!     + + + PURPOSE + + +
!    This program prints out many of the global variables before
!    and after the call to SOIL provide a comparison of values
!    which may be changed by SOIL

!    author: John Tatarko
!    version: 08/30/92

!     + + + KEY WORDS + + +
!     wind, erosion, hydrology, tillage, soil, crop, decomposition

      use weps_interface_defs, ignore_me=>sdbug
      use datetime_mod, only: get_simdate
      use file_io_mod, only: luosdb
      use biomaterial, only: biototal
      use erosion_data_struct_defs, only: awadir, awhrmx, awudmx, awudmn
      use climate_input_mod, only: cli_today
      use hydro_data_struct_defs, only: hydro_derived_et

!     + + + GLOBAL COMMON BLOCKS + + +
      include 'p1werm.inc'
      include 'm1subr.inc'
      include 'm1sim.inc'
      include 'm1flag.inc'
      include 's1layr.inc'
      include 's1surf.inc'
      include 's1phys.inc'
      include 's1agg.inc'
      include 's1dbh.inc'
      include 's1dbc.inc'
      include 's1sgeo.inc'
      include 's1psd.inc'
      include 'h1hydro.inc'
      include 'h1scs.inc'
      include 'h1db1.inc'
      include 'h1temp.inc'

!     + + + LOCAL COMMON BLOCKS + + +
      include 'main/main.inc'
      include 'soil/tsdbug.inc'

!     + + + ARGUMENT DECLARATIONS + + +
      integer isr, slay
      type(biototal), intent(in) :: croptot, biotot
      type(hydro_derived_et), intent(inout) :: h1et

!     + + + LOCAL VARIABLES + + +
      integer cd, cm, cy, l

!     + + + LOCAL DEFINITIONS + + +

!   cd        - The current day of simulation month.
!   cm        - The current month of simulation year.
!   cy        - The current year of simulation run.
!   daysim    - The surrent day of the simulation run.
!   isr       - This variable holds the subregion index.
!   l         - This variable is an index on soil layers.

!     + + + SUBROUTINES CALLED + + +

!     + + + FUNCTIONS CALLED + + +

!     + + + UNIT NUMBERS FOR INPUT/OUTPUT DEVICES + + +
!     * = screen and keyboard
!    26 = debug SOIL

!     + + + DATA INITIALIZATIONS + + +

      if (am0ifl  .eqv. .true.) then
          tday = -1
          tmo = -1
          tyr = -1
          tisr = -1
      end if
      call get_simdate (cd, cm, cy)

!     + + + INPUT FORMATS + + +

!     + + + OUTPUT FORMATS + + +
 2030 format ('**',1x,2(i2,'/'),i4,' daysim=',i4,'   After  call to SOIL&
     &    Subregion No. ',i3)
 2031 format ('**',1x,2(i2,'/'),i4,' daysim=',i4,'   Before call to SOIL&
     &    Subregion No. ',i3)
 2032 format (' cli_today%zdpt  cli_today%tdmx  cli_today%tdmn  cli_today%eirr  awudmx  awudmn ', &
     &        ' cli_today%tdpt  awadir  awhrmx  amzele ')
 2038 format (f7.2,9f8.2)
 2050 format ('amrslp(',i2,') croptot%ftcvtot(',i2,') croptot%rlaitot(',&
     &  i2,')',                                                         &
     &      ' croptot%zrtd(',i2,') biotot%mftot(',i2,') ahfwsf(',i2,')',&
     &      ' ahzper(',i2,')')
 2051 format (2f10.2,2f10.5,2x,f10.2,f10.2,f12.2)
 2052 format ('ahzrun(',i2,') h1et%zirr(',i2,') ahzsno(',i2,')',           &
     &        ' ahzsmt(',i2,') asxrgs(',i2,') aszrgh(',i2,')',          &
     &        ' aslrr(',i2,')')
 2053 format (5f10.2,2f12.2)
 2054 format (' asfcr(',i2,')  asecr(',i2,') asmlos(',i2,')',           &
     &        ' asflos(',i2,')  croptot%c0rg(',i2,') as0rrk(',i2,')',   &
     &        ' aszcr(',i2,')')
 2055 format (2f10.2,2f10.3,i10,2f12.2)
 2056 format('layer aszlyt  ahrsk ahrwc ahrwcs ahrwca',                 &
     &       ' ahrwcf ahrwcw ah0cb aheaep ahtsmx ahtsmn')
 2060 format (i4,1x,f6.1,1x,e7.1,f6.2,4f7.2,f6.2,3f7.2)
 2065 format(' layer asfsan asfcla asfom asdsblk asdblk aslagm as0ags', &
     &       ' aslagn  aslagx  aseags')
 2070 format (i4,2x,2f7.2,f7.3,3f7.2,f8.2,f7.3,2f8.2)

!     + + + END SPECIFICATIONS + + +

!          write weather cligen and windgen variables
      if ((cd .eq. tday) .and. (cm .eq. tmo) .and. (cy .eq. tyr) .and.  &
     &   (isr .eq. tisr)) then
         write(luosdb(isr),2030) cd,cm,cy,daysim,isr
      else
         write(luosdb(isr),2031) cd,cm,cy,daysim,isr
      end if
      write(luosdb(isr),2032)
      write(luosdb(isr),2038) cli_today%zdpt,cli_today%tdmx,cli_today%tdmn,cli_today%eirr,awudmx,awudmn,&
     &               cli_today%tdpt,awadir,awhrmx,amzele

      write(luosdb(isr),2050) isr,isr,isr,isr,isr,isr,isr

      write(luosdb(isr),2051) amrslp(isr), croptot%ftcvtot,             &
     &              croptot%rlaitot,                                    &
     &              croptot%zrtd, biotot%mftot, ahfwsf(isr), h1et%zper
      write(luosdb(isr),2052) isr,isr,isr,isr,isr,isr,isr
      write(luosdb(isr),2053) h1et%zrun,h1et%zirr,ahzsno(isr),      &
     &               ahzsmt(isr), asxrgs(isr),aszrgh(isr),aslrr(isr)
      write(luosdb(isr),2054) isr,isr,isr,isr,isr,isr,isr
      write(luosdb(isr),2055) asfcr(isr), asecr(isr), asmlos(isr),      &
     &               asflos(isr), croptot%c0rg,as0rrk(isr),aszcr(isr)
      write(luosdb(isr),2056)

      do 200 l = 1,slay
         write(luosdb(isr),2060) l, aszlyt(l,isr), ahrsk(l,isr),        &
     &                  ahrwc(l,isr),                                   &
     &                  ahrwcs(l,isr),ahrwca(l,isr), ahrwcf(l,isr),     &
     &                  ahrwcw(l,isr),ah0cb(l,isr),aheaep(l,isr),       &
     &                  ahtsmx(l,isr), ahtsmn(l,isr)
  200 continue
      write(luosdb(isr),2065)
!     om, bulk density, min ag dia., and ag. stability do not have
!     values at the surface and thus are set to high values so the
!     output will read '*******' for these values
      asdblk(0,isr) = 0.0
!      if (asfom(0,isr) .eq. 0.0)  asfom(0,isr)    = 1111111111111.
!      if (asdblk(0,isr) .eq. 0.0)  asdblk(0,isr) = 1111111111111.
!      if (aseags(0,isr) .eq. 0.0)  aseags(0,isr) = 1111111111111.
!      if (aslagn(0,isr) .eq. 0.0) aslagn(0,isr)  = 1111111111111.

      do 300 l=1,slay
         write(luosdb(isr),2070) l, asfsan(l,isr), asfcla(l,isr),       &
     &                  asfom(l,isr), asdsblk(l,isr), asdblk(l,isr),    &
     &                  aslagm(l,isr), as0ags(l,isr), aslagn(l,isr),    &
     &                  aslagx(l,isr), aseags(l,isr)
  300 continue

      tisr = isr
      tday = cd
      tmo = cm
      tyr = cy

      return
      end
