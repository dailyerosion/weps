!$Author$
!$Date$
!$Revision$
!$HeadURL$
      subroutine stir_cum(isr, speed, depth, tilltype, fracarea)

      use stir_report_mod, only: stircum

!     + + + ARGUMENT DECLARATIONS + + +
      integer isr
      real speed, depth
      integer tilltype
      real fracarea

!     + + + ARGUMENT DEFINITIONS + + +
!     isr - subregion index
!     speed - operation speed (m/s)
!     depth - tillage depth (mm)
!     tilltype - tillage burial distribution type (0-5)
!              0    o uniform distribution
!              1    o Mixing+Inversion Burial Distribution
!              2    o Mixing Burial Distribution
!              3    o Inversion Burial Distribution
!              4    o Lifting, Fracturing Burial Distribution
!              5    o Compression
!     fracarea - fraction of area affected (fraction)

!     + + + INCLUDE + + +
      include 'p1werm.inc'
      include 'command.inc'

!     + + + PURPOSE + + +
!     each time it is called, it calculates the Soil Tillage Intensity Rating
!     for the current operation and adds it to the total.

!     + + + LOCAL VARIABLES + + +
      real stir_val
      real mstomph, mmtoin
      real tilltype_coef

!     + + + LOCAL DEFINITIONS + + +
!     stir_val - soil tilage intensity rating value for this residue burial
!     mstomph - conversion constant from meters per second to miles per hour
!     mmtoin  - conversion constant from millimeters to inches
!     tilltype_coef - multiplier value assigned to each tillage type

      parameter (mstomph = 2.237)
      parameter (mmtoin = 0.03937)

      ! only do if flag is set 
      if( soil_cond .eq. 0 ) return

      select case (tilltype)
      case (1)  ! Mixing, some inversion
          tilltype_coef = 0.8
      case (2)  ! Mixing
          tilltype_coef = 0.7
      case (3)  ! Inversion + some mixing
          tilltype_coef = 1.0
      case (4)  ! Lifting, Fracturing
          tilltype_coef = 0.4
      case (5)  ! Compression
          tilltype_coef = 0.15
      case default
          tilltype_coef = 0.4
      end select

      stir_val = speed * mstomph * 0.5                                  &
     &         * tilltype_coef * 3.25                                   &
     &         * depth * mmtoin                                         &
     &         * fracarea

      stircum(isr)%stir_op_sum = stircum(isr)%stir_op_sum + stir_val
      stircum(isr)%proc_cnt = stircum(isr)%proc_cnt + 1

      return
      end
