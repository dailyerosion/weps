!$Author$
!$Date$
!$Revision$
!$HeadURL$
      subroutine ht_dia_sai( bcdpop, bcmstandstem, bc0ssa, bc0ssb,      &
     &                       bcdstm, bcxstm, bczmxc, bczht, dht,        &
     &                       bcxstmrep, bcrsai )

!     + + + PURPOSE + + +
! this routine checks for consistency between plant height and biomass
! accumulation, using half and double the stem diameter (previously unused)
! as check points. The representative stem diameter is set to show where
! within the range the actual stem diameter is.

!     + + + ARGUMENT DECLARATIONS + + +
      real, intent(in) :: bcdpop, bcmstandstem, bc0ssa, bc0ssb
      real, intent(in) :: bcdstm, bcxstm, bczmxc, bczht 
      real, intent(inout) :: dht
      real, intent(out) :: bcxstmrep, bcrsai

!     + + + ARGUMENT DEFINITIONS + + +
!     bcdpop - Crop seeding density (#/m^2)
!     bcmstandstem - crop standing stem mass (kg/m^2)
!     bc0ssa - stem area to mass coefficient a, result is m^2 per plant
!     bc0ssb - stem area to mass coefficient b, argument is kg per plant
!     bcdstm - Number of crop stems per unit area (#/m^2)
!     bcxstm - Crop stem diameter (m)
!     bczmxc - maximum potential plant height (m)
!     bczht  - Crop height (m)
!     dht - daily height increment (m)
!     bcxstmrep - a representative diameter so that acdstm*acxstmrep*aczht=acrsai
!     bcrsai - Crop stem area index (m^2/m^2)

!     + + + LOCAL VARIABLES + + +
      real min_dia, max_dia, min_height, max_height, new_height

!     + + + LOCAL VARIABLE DEFINITIONS + + +
!     min_dia - minimum stem diameter
!     max_dia - maximum stem diameter
!     min_height - minimum plant height
!     max_height - maximum plant height
!     new_height - plant height plus increment

!     + + + LOCAL PARAMETERS + + +
      real multmin
      real multmax
      parameter(multmin = 0.5)
      parameter(multmax = 1.5)

!     + + + LOCAL PARAMETER DEFINITIONS + + +
!     multmin - multiplier to find minimum stem diameter from set stem diameter
!     multmax - multiplier to find maximum stem diameter from set stem diameter

!     + + + END OF SPECIFICATIONS + + +

      ! calculate crop stem area index
      ! when exponent is not 1, must use mass for single plant stem to get stem area
      ! bcmstandstem, convert (kg/m^2) / (plants/m^2) = kg/plant
      ! result of ((m^2 of stem)/plant) * (# plants/m^2 ground area) = (m^2 of stem)/(m^2 ground area)
      if( bcdpop .gt. 0.0 ) then
          bcrsai = bcdpop * bc0ssa * (bcmstandstem/bcdpop)**bc0ssb
      else
          bcrsai = 0.0
      end if

!      if( dht .lt. 0.0 ) then
!          write(*,*) 'ERROR - THIS SHOULD NEVER APPEAR'
!          stop
!      if( dht .gt. 0.0 ) then
!          ! only adjust height during period of height increase
!          ! Back calculate height limits
!          ! min diameter, max height
!          min_dia = multmin * bcxstm
!          max_height = bcrsai / (bcdstm * min_dia)
!      
!          ! max diameter, min height
!          max_dia = multmax * bcxstm
!          min_height = bcrsai / (bcdstm * max_dia)
!
!          ! check proposed height increase
!          if( dht .gt. 0.0 ) then
!              new_height = bczht + dht
!              if( new_height .gt. max_height ) then
!                  ! stem is too thin, slow height increase, no less than zero
!                  dht = max(0.0, max_height - bczht)
!              else if( new_height .lt. min_height ) then
!                  ! stem is too thick, speed height increase
!                  dht = min(bczmxc - bczht, min_height - bczht)
!              end if
!          end if
!      end if

      ! (m^2 stem / m^2 ground) / ((stems/m^2 ground) * m) = m/stem
      ! this value not reset unless it is meaningful
      if( (bcdstm * (bczht + dht)) .gt. 0.0 ) then
          bcxstmrep = bcrsai / (bcdstm * (bczht + dht))
      else
          bcxstmrep = 0.0
      end if

      return
      end


