!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine report_hydrobal( isr, bmrotation )

      use file_io_mod, only: luohydrobal
      use manage_data_struct_defs, only: lastoper

!     + + + ARGUMENT DECLARATIONS + + +
      integer isr, bmrotation

!     + + + ARGUMENT DEFINITIONS + + +
!     isr     - subregion number
!     bmrotation - rotation count updated in manage.for

!     + + + PARAMETERS AND COMMON BLOCKS + + +
      include 'p1werm.inc'
      include 'm1flag.inc'
!      include 'main/main.inc'
!      include 'manage/oper.inc'
      include 'h1balance.inc'

!     + + + LOCAL VARIABLES + + +
      real fallow_eff

!     + + + LOCAL DEFINITIONS + + +
!     fallow_eff - computed fallow efficiency from period rain and soil
!                  water content values

      if( am0sif ) then  ! initilizing cycle

        ! set to the beginning of simulation
        ! to eliminate newline at beginning of file
        hprevrotation(isr) = 1

      else  ! done when initializing cycle(s) completed

        if( bmrotation .gt. hprevrotation(isr) ) then
          ! write newline
          write(unit=luohydrobal(isr),fmt=1001) ''
        end if

        ! check initial day and present day for order
        ! counting is restarted when initialization complete
        do while ( initday(isr) .gt. presday(isr) )
            ! initial day greater than present day, correct
            initday(isr) = initday(isr) - 365
        end do

        if ( cumprecip(isr) .gt. 0.0 ) then
            fallow_eff = ( presswc(isr)-initswc(isr) ) / cumprecip(isr)
        else
            fallow_eff = 0.0
        end if

        write(unit=luohydrobal(isr),fmt=1000, advance='NO')             &
     &  lastoper(isr)%day, lastoper(isr)%mon, lastoper(isr)%yr,         &
     &  trim(lastoper(isr)%name),                                       &
     &  'Start day,swc,snow', initday(isr), initswc(isr), initsnow(isr),&
     &  'End day,swc,snow', presday(isr), presswc(isr), pressnow(isr),  &
     &  'rain,runoff,evap,trans,drain,check,falloweff', cumprecip(isr), &
     &  cumrunoff(isr),cumevap(isr), cumtrans(isr), cumdrain(isr),      &
     &  initswc(isr) - presswc(isr) + initsnow(isr) - pressnow(isr)     &
     &  + cumprecip(isr) - cumrunoff(isr) - cumevap(isr)                &
     &  - cumtrans(isr) - cumdrain(isr), fallow_eff

 1000 format(1x,i2,'/',i2,'/',i2,'|',a,'|',a,'|',f7.0,'|',2(f9.3,'|'),  &
     &       a,'|',f7.0,'|',2(f9.3,'|'),a,'|',7(f9.3,'|'))
 1001 format(a)

        hprevrotation(isr) = bmrotation

      end if

!     reset counters and accumulators
      initday(isr) = presday(isr)
      initswc(isr) = presswc(isr)
      initsnow(isr) = pressnow(isr)
      cumprecip(isr) = 0.0
      cumrunoff(isr) = 0.0
      cumevap(isr) = 0.0
      cumtrans(isr) = 0.0
      cumdrain(isr) = 0.0


      return
      end
