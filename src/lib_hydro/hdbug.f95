!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine  hdbug(isr, soil, crop, restot, h1et)

!     + + + PURPOSE + + +
!    This program prints out many of the global variables before
!    and after the call to HYDROLOGY provide a comparison of values
!    which may be changed by HYDDROLOGY

!    author: John Tatarko
!    version: 09/01/92

!     + + + KEY WORDS + + +
!     wind, erosion, hydrology, tillage, soil, crop, decomposition
!     management

      use weps_main_mod, only: am0ifl
      use datetime_mod, only: get_simdate
      use file_io_mod, only: luohdb
      use soil_data_struct_defs, only: soil_def
      use biomaterial, only: biototal, biomatter
      use erosion_data_struct_defs, only: awadir, awhrmx, awudmx, awudmn
      use hydro_data_struct_defs, only: hydro_derived_et
      use climate_input_mod, only: cli_today

!     + + + ARGUMENT DECLARATIONS + + +
      integer isr                   
      type(soil_def), intent(in) :: soil  ! soil for this subregion
      type(biomatter), intent(in) :: crop
      type(biototal), intent(in) :: restot
      type(hydro_derived_et), intent(in) :: h1et

!     + + + ARGUMENT DEFINITIONS + + +
!     isr       - subregion index.
!     restot    - structure containing summary residue pool amounts

!     + + + GLOBAL COMMON BLOCKS + + +
      include 'p1werm.inc'
      include 'h1hydro.inc'
      include 'h1db1.inc'
      include 'h1temp.inc'

!     + + + LOCAL COMMON BLOCKS + + +
      include 'hydro/thdbug.inc'

!     + + + LOCAL VARIABLES + + +

      integer cd, cm, cy, l


!     + + + LOCAL DEFINITIONS + + +

!   cd        - The current day of simulation month.
!   cm        - The current month of simulation year.
!   cy        - The current year of simulation run.
!   l         - This variable is an index on soil layers.

!     + + + SUBROUTINES CALLED + + +

!     + + + FUNCTIONS CALLED + + +

!     + + + UNIT NUMBERS FOR INPUT/OUTPUT DEVICES + + +
!     * = screen and keyboard
!    25 = debug HYDRO

!     + + + DATA INITIALIZATIONS + + +

         call get_simdate (cd, cm, cy)


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
 2032 format (' cli_today%zdpt  cli_today%tdmx  cli_today%tdmn  cli_today%eirr  awudmx  awudmn ',       &
     &        ' cli_today%tdpt  awadir  awhrmx ')
 2038 format (f7.2,9f8.2)
! 2045 format ('Subregion Number',i3)
 2050 format ('amrslp(',i2,') crop%deriv%ftcv(',i2,') crop%deriv%rlai(',i2,')',           &
     &        ' crop%geometry%zrtd(',i2,') restot%mftot(',i2,') ahfwsf(',i2,')',    &
     &        ' ahzper(',i2,')')
 2051 format (2f10.2,2f10.5,2x,f10.2,f10.2,f12.2)
 2052 format ('ahzrun(',i2,') h1et%zirr(',i2,') ahzsno(',i2,')',           &
     &        ' ahzsmt(',i2,')  h1et%zeta      h1et%zetp     ',               &
     &        ' h1et%zpta ')
 2053 format (5f10.2,2f12.2)
 2054 format ('      h1et%zea     h1et%zep    h1et%zptp   aslrr(',i2,')')
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
         (isr .eq. tisr)) then
         write(luohdb(isr),2030) cd,cm,cy,isr
      else
         write(luohdb(isr),2031) cd,cm,cy,isr
      end if
      write(luohdb(isr),2032)
      write(luohdb(isr),2038) cli_today%zdpt, cli_today%tdmx, cli_today%tdmn, cli_today%eirr, awudmx,   &
                    awudmn, cli_today%tdpt, awadir, awhrmx

!      write(luohdb(isr),2045) isr

      write(luohdb(isr),2050) isr, isr, isr, isr, isr, isr, isr
      write(luohdb(isr),2051) soil%amrslp, crop%deriv%ftcv, crop%deriv%rlai, &
                    crop%geometry%zrtd, restot%mftot, ahfwsf(isr), h1et%zper
      write(luohdb(isr),2052) isr, isr, isr, isr
      write(luohdb(isr),2053) h1et%zrun, h1et%zirr, ahzsno(isr), &
                    ahzsmt(isr), h1et%zeta, h1et%zetp, h1et%zpta
      write(luohdb(isr),2054) isr, isr, isr, isr
      write(luohdb(isr),2055) h1et%zea, h1et%zep, h1et%zptp, soil%aslrr
      write(luohdb(isr),2056)

      do 200 l = 1,soil%nslay
         write(luohdb(isr),2060) l, soil%aszlyt(l), soil%ahrsk(l), soil%ahrwc(l), &
     &                  soil%ahrwcs(l), soil%ahrwca(l), soil%ahrwcf(l), &
     &                  soil%ahrwcw(l), soil%ah0cb(l), soil%aheaep(l), &
     &                  ahtsmx(l,isr), ahtsmn(l,isr)
  200 continue
         write(luohdb(isr),2065)

      do 300 l=1,soil%nslay
         write(luohdb(isr),2070) l, soil%asfsan(l), soil%asfsil(l), &
     &                  soil%asfcla(l), soil%asfom(l), soil%asdblk(l), &
     &                  soil%aslagm(l), soil%as0ags(l), soil%aslagn(l), &
     &                  soil%aslagx(l), soil%aseags(l)
  300 continue


      tisr = isr
      tday = cd
      tmo = cm
      tyr = cy

      return
      end

