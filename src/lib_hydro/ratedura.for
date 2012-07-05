!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine ratedura(bhzirr, bhratirr, bhdurirr)

!     + + + PURPOSE + + +
!     makes sure that irrigation depth, application rate and duration
!     are consistent. This routine always requires an irrigation depth
!     to be set on entry, although it can be safely be zero.

!     + + + ARGUMENT DECLARATIONS + + +

      real bhzirr, bhratirr, bhdurirr

!     + + + ARGUMENT DEFINITIONS + + +

!     bhzirr    - daily irrigation application depth (mm)
!     bhratirr  - characteristic system irrigation rate (mm/hour)
!     bhdurirr  - irrigation application duration (hours)

!     + + + END SPECIFICATIONS + + +

      if( bhratirr .gt. 0.0 ) then
          ! irrigation characteristic rate known, so find duration
          ! to apply the given depth
          bhdurirr = min( 24.0, bhzirr / bhratirr )
      else if( bhdurirr .gt. 0.0 ) then
          ! irrigation duration known, rate not known, so find the characteristic rate
          bhratirr = bhzirr / bhdurirr
      else
          ! neither rate nor duration known, so apply over 24 hours
          bhdurirr = 24.0
          bhratirr = bhzirr / bhdurirr
      end if

      return
      end

