!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!

      real function biodrag (bdrlai, bdrsai, bcrlai, bcrsai, bc0rg,     &
     &                       bcxrow, bczht, bszrgh)

!     + + + PURPOSE + + +
!     BIODRAG: combine effects of leaves and stems on drag coef.
!     Calling subroutine needs b1glob.inc, c1gen.inc s1sgeo.inc

!     Leaves are less effective at reducing the wind speed than
!     stems.  Three effects are simulated: 1. streamlining of leaves,
!     2. leaf sheltered in furrow, and
!     3.leaf area confined in wide rows that act as wind barriers.
!     This function combines these effects into a single
!     value for use by other routines. May still be too large.

!     + + + KEYWORDS + + +
!     biodrag

!     + + + ARGUMENT DECLARATIONS + + +
      real    bdrlai, bdrsai, bcrlai, bcrsai
      integer bc0rg
      real    bcxrow, bczht, bszrgh

!     + + + ARGUMENT DEFINITIONS + + +
!     biodrag  - drag coefficient (no units)
!     bdrlai   - residue leaf area index (sum of all pools)(m^2/m^2)
!     bdrsai   - residue stem silhouette area index (sum of all pools)(m^2/m^2)
!     bcrlai   - crop leaf area index (m^2/m^2)
!     bcrsai   - crop stem silhouette area index (m^2/m^2)
!     bc0rg    - crop seed location flag (0= in furrow, 1=on ridge)
!     bcxrow   - crop row spacing (m)(0 = broadcast)
!     bczht    - crop biomass height (m)
!     bszrgh   - ridge height (mm)

!     + + + PARAMETERS + + +
      real fur_dis
      parameter( fur_dis = 0.5 )
!     fur_dis  - coefficient for discounting drag of plant in furrow bottom

!     + + + GLOBAL COMMON BLOCKS + + +
      include 'p1unconv.inc'

!     + + + LOCAL VARIABLES + + +
      real red_lai, red_sai, red_fac

!     + + + LOCAL VARIABLE DEFINITIONS + + +
!     red_lai     - reduced leaf area index (m^2/m^2)
!     red_sai     - reduced stem area index (m^2/m^2)
!     red_fac     - reduction factor

!     + + + END SPECIFICATIONS + + +

      ! place crop values in temporary variables
      red_lai = bcrlai
      red_sai = bcrsai

      ! check for crop biomass position with respect to the ridge
      if(bc0rg .eq. 0) then
          ! biomass in furrow
          ! test plant height and ridge height for minimums
          if( bczht .gt. (fur_dis * bszrgh * mmtom) ) then
              ! sufficient height for some effect
              red_fac = (1.0 - fur_dis * bszrgh * mmtom / bczht)
              red_lai = red_lai * red_fac
              red_sai = red_sai * red_fac

              ! check for row width effect
              if( bcxrow .gt. bczht*5.0 ) then
                  red_fac = 1.0/(0.92 + 0.021 * bcxrow                  &
     &                    / (bczht - fur_dis * bszrgh * mmtom) )
                  red_lai = red_lai * red_fac
              end if

          else
              ! not tall enough to do anything
              red_lai = 0.0
              red_sai = 0.0
          endif
      else
          ! biomass not in furrow
          ! test plant height and ridge height for minimums
          if( bczht .gt. 0.0 ) then
              ! check for row width effect
              if( bcxrow .gt. bczht*5.0 ) then
                  red_fac = 1.0 / (0.92 + 0.021 * bcxrow / bczht)
                  red_lai = red_lai * red_fac
              end if
          else
              ! not tall enough to do anything
              red_lai = 0.0
              red_sai = 0.0
          endif
      end if

      ! add discounted crop values to biomass values
      red_lai = red_lai + bdrlai
      red_sai = red_sai + bdrsai

      ! streamline effect for total leaf area
      red_lai = red_lai * 0.2 * (1.0 - exp(-red_lai))

      ! final result
      biodrag = red_lai + red_sai

      return
      end
