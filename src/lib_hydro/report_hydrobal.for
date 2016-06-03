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
      include 'h1balance.inc'

!     + + + LOCAL VARIABLES + + +
      real fallow_eff
      real water_use_eff
      real water_use

!     + + + LOCAL DEFINITIONS + + +
!     fallow_eff - computed fallow efficiency from period rain and soil water content values
!     water_use_eff - computed water use efficiency from period rain and soil water content values
!     water use - The total water that was used during period

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
            fallow_eff = ( presswc(isr) - initswc(isr)                  &
     &                 + pressnow(isr) - initsnow(isr) - cumirrig(isr) )&
     &                 / cumprecip(isr)
        else
            fallow_eff = 0.0
        end if

        water_use = ( initswc(isr) - presswc(isr)                       &
     &            + pressnow(isr) - initsnow(isr)                       &
     &            + cumprecip(isr) + cumirrig(isr) )
        if( water_use .ne. 0.0 ) then
            water_use_eff = cumtrans(isr) / water_use
        else
            water_use_eff = 0.0
        end if

        if( cumtrans(isr) .gt. 0.0 ) then

        write(unit=luohydrobal(isr),fmt=1000, advance='NO')             &
     &  lastoper(isr)%day, lastoper(isr)%mon, lastoper(isr)%yr,         &
     &  trim(lastoper(isr)%name),                                       &
     &  'Start day,swc,snow', initday(isr), initswc(isr), initsnow(isr),&
     &  'End day,swc,snow', presday(isr), presswc(isr), pressnow(isr),  &
     &  'rain,irrigation,runoff,evap,trans,drain,check,wateruse_eff',   &
     &  cumprecip(isr), cumirrig(isr),                                  &
     &  cumrunoff(isr), cumevap(isr), cumtrans(isr), cumdrain(isr),     &
     &  initswc(isr) - presswc(isr) + initsnow(isr) - pressnow(isr)     &
     &  + cumprecip(isr) + cumirrig(isr) - cumrunoff(isr) - cumevap(isr)&
     &  - cumtrans(isr) - cumdrain(isr), water_use_eff

        else

        write(unit=luohydrobal(isr),fmt=1000, advance='NO')             &
     &  lastoper(isr)%day, lastoper(isr)%mon, lastoper(isr)%yr,         &
     &  trim(lastoper(isr)%name),                                       &
     &  'Start day,swc,snow', initday(isr), initswc(isr), initsnow(isr),&
     &  'End day,swc,snow', presday(isr), presswc(isr), pressnow(isr),  &
     &  'rain,irrigation,runoff,evap,trans,drain,check,falloweff',      &
     &  cumprecip(isr), cumirrig(isr),                                  &
     &  cumrunoff(isr), cumevap(isr), cumtrans(isr), cumdrain(isr),     &
     &  initswc(isr) - presswc(isr) + initsnow(isr) - pressnow(isr)     &
     &  + cumprecip(isr) + cumirrig(isr) - cumrunoff(isr) - cumevap(isr)&
     &  - cumtrans(isr) - cumdrain(isr), fallow_eff

        end if

 1000 format(1x,i2,'/',i2,'/',i2,'|',a,'|',a,'|',f7.0,'|',2(f9.3,'|'),  &
     &       a,'|',f7.0,'|',2(f9.3,'|'),a,'|',8(f9.3,'|'))
 1001 format(a)

        hprevrotation(isr) = bmrotation

      end if

!     reset counters and accumulators
      initday(isr) = presday(isr)
      initswc(isr) = presswc(isr)
      initsnow(isr) = pressnow(isr)
      cumprecip(isr) = 0.0
      cumirrig(isr) = 0.0
      cumrunoff(isr) = 0.0
      cumevap(isr) = 0.0
      cumtrans(isr) = 0.0
      cumdrain(isr) = 0.0


      return
      end
