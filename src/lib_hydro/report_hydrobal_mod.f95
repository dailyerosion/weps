!$Author$
!$Date$
!$Revision$
!$HeadURL$

module report_hydrobal_mod

  type hydro_balance
    real :: cumprecip ! accumulation of rainfall (mm)
    real :: cumirrig  ! accumulation of irrigation (mm)
    real :: cumrunoff ! accumulation of runoff (mm)
    real :: cumevap   ! accumulation of evaporation (mm)
    real :: cumtrans  ! accumulation of transpiration (mm)
    real :: cumdrain  ! accumulation of drainage (mm)
    real :: initswc   ! initial soil water content (mm)
    real :: initsnow  ! initial snow water content (mm)
    real :: initday   ! initial day
    real :: presswc   ! present soil water content (mm)
    real :: pressnow  ! present snow water content (mm)
    real :: presday   ! present day
    integer :: hprevrotation ! rotation count number of previously printed hydro balance
  end type hydro_balance

  type(hydro_balance), dimension(:), allocatable :: h1bal

  contains

    subroutine report_hydrobal( isr, bmrotation, bmperod )

      use weps_main_mod, only: report_loop
      use datetime_mod, only: get_simdate, julday, caldat
      use file_io_mod, only: luohydrobal
      use manage_data_struct_defs, only: lastoper

!     + + + ARGUMENT DECLARATIONS + + +
      integer isr, bmrotation, bmperod

!     + + + ARGUMENT DEFINITIONS + + +
!     isr     - subregion number
!     bmrotation - rotation count updated in manage.for

!     + + + LOCAL VARIABLES + + +
      integer presdy, presmon, presyr
      integer initdy, initmon, inityr
      integer presjday
      integer initjday
      real fallow_eff
      real water_use_eff
      real water_use

!     + + + LOCAL DEFINITIONS + + +
!     fallow_eff - computed fallow efficiency from period rain and soil water content values
!     water_use_eff - computed water use efficiency from period rain and soil water content values
!     water use - The total water that was used during period

      if( .not. report_loop ) then  ! initilizing cycle

        ! set to the beginning of simulation
        ! to eliminate newline at beginning of file
        h1bal(isr)%hprevrotation = 1

      else  ! done when initializing cycle(s) completed

        if( bmrotation .gt. h1bal(isr)%hprevrotation ) then
          ! write newline
          write(unit=luohydrobal(isr),fmt=1001) ''
        end if

        ! check initial day and present day for order
        ! daysim counting is restarted when initialization complete
        if( h1bal(isr)%initday .gt. h1bal(isr)%presday ) then
            ! initial day greater than present day, correct using date math
            call get_simdate(presdy, presmon, presyr)
            presjday = julday(presdy, presmon, presyr)
            initjday = presjday + (h1bal(isr)%initday - h1bal(isr)%presday)
            call caldat(initjday, initdy, initmon, inityr)
            do while( initjday .gt. presjday )
                inityr = inityr - bmperod
                initjday = julday(initdy,initmon,inityr)
            end do
            h1bal(isr)%initday = h1bal(isr)%presday - (presjday - initjday)
        end if

        if ( h1bal(isr)%cumprecip .gt. 0.0 ) then
            fallow_eff = ( h1bal(isr)%presswc - h1bal(isr)%initswc                  &
     &                 + h1bal(isr)%pressnow - h1bal(isr)%initsnow - h1bal(isr)%cumirrig )&
     &                 / h1bal(isr)%cumprecip
        else
            fallow_eff = 0.0
        end if

        water_use = ( h1bal(isr)%initswc - h1bal(isr)%presswc                       &
     &            + h1bal(isr)%pressnow - h1bal(isr)%initsnow                       &
     &            + h1bal(isr)%cumprecip + h1bal(isr)%cumirrig )
        if( water_use .ne. 0.0 ) then
            water_use_eff = h1bal(isr)%cumtrans / water_use
        else
            water_use_eff = 0.0
        end if

        if( h1bal(isr)%cumtrans .gt. 0.0 ) then

        write(unit=luohydrobal(isr),fmt=1000, advance='NO')             &
     &  lastoper(isr)%day, lastoper(isr)%mon, lastoper(isr)%yr,         &
     &  trim(lastoper(isr)%name),                                       &
     &  'Start day,swc,snow', h1bal(isr)%initday, h1bal(isr)%initswc, h1bal(isr)%initsnow,&
     &  'End day,swc,snow', h1bal(isr)%presday, h1bal(isr)%presswc, h1bal(isr)%pressnow,  &
     &  'rain,irrigation,runoff,evap,trans,drain,check,wateruse_eff',   &
     &  h1bal(isr)%cumprecip, h1bal(isr)%cumirrig,                                  &
     &  h1bal(isr)%cumrunoff, h1bal(isr)%cumevap, h1bal(isr)%cumtrans, h1bal(isr)%cumdrain,     &
     &  h1bal(isr)%initswc - h1bal(isr)%presswc + h1bal(isr)%initsnow - h1bal(isr)%pressnow     &
     &  + h1bal(isr)%cumprecip + h1bal(isr)%cumirrig - h1bal(isr)%cumrunoff - h1bal(isr)%cumevap&
     &  - h1bal(isr)%cumtrans - h1bal(isr)%cumdrain, water_use_eff

        else

        write(unit=luohydrobal(isr),fmt=1000, advance='NO')             &
     &  lastoper(isr)%day, lastoper(isr)%mon, lastoper(isr)%yr,         &
     &  trim(lastoper(isr)%name),                                       &
     &  'Start day,swc,snow', h1bal(isr)%initday, h1bal(isr)%initswc, h1bal(isr)%initsnow,&
     &  'End day,swc,snow', h1bal(isr)%presday, h1bal(isr)%presswc, h1bal(isr)%pressnow,  &
     &  'rain,irrigation,runoff,evap,trans,drain,check,falloweff',      &
     &  h1bal(isr)%cumprecip, h1bal(isr)%cumirrig,                                  &
     &  h1bal(isr)%cumrunoff, h1bal(isr)%cumevap, h1bal(isr)%cumtrans, h1bal(isr)%cumdrain,     &
     &  h1bal(isr)%initswc - h1bal(isr)%presswc + h1bal(isr)%initsnow - h1bal(isr)%pressnow     &
     &  + h1bal(isr)%cumprecip + h1bal(isr)%cumirrig - h1bal(isr)%cumrunoff - h1bal(isr)%cumevap&
     &  - h1bal(isr)%cumtrans - h1bal(isr)%cumdrain, fallow_eff

        end if

 1000 format(1x,i2,'/',i2,'/',i2,'|',a,'|',a,'|',f7.0,'|',2(f9.3,'|'),  &
     &       a,'|',f7.0,'|',2(f9.3,'|'),a,'|',8(f9.3,'|'))
 1001 format(a)

        h1bal(isr)%hprevrotation = bmrotation

      end if

!     reset counters and accumulators
      h1bal(isr)%initday = h1bal(isr)%presday
      h1bal(isr)%initswc = h1bal(isr)%presswc
      h1bal(isr)%initsnow = h1bal(isr)%pressnow
      h1bal(isr)%cumprecip = 0.0
      h1bal(isr)%cumirrig = 0.0
      h1bal(isr)%cumrunoff = 0.0
      h1bal(isr)%cumevap = 0.0
      h1bal(isr)%cumtrans = 0.0
      h1bal(isr)%cumdrain = 0.0


      return
    end subroutine report_hydrobal

end module report_hydrobal_mod

