!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine propsaxt( sandf, clayf, sat, fc, pwp )

!     Reference: K.E. Saxton et al., 1986, Estimating generalized soil-water
!     characteristics from texture. Soil Sci. Soc. Amer. J. 50(4):1031-1036

!     + + + ARGUMENT DECLARATIONS + + +
      real sandf, clayf, sat, fc, pwp

!     + + + ARGUMENT DEFINITIONS + + +
!     sandf - fraction of soil mineral portion which is sand
!     clayf - fraction of soil mineral portion which is clay
!     sat - saturated volumetric water content
!     fc - 1/3 bar volumetric water content
!     pwp - 15 bar volumetric water content

!     + + + LOCAL VARIABLES + + +
      real sand_2, clay_2, percent_sand, percent_clay, acoef, bcoef

!     + + + LOCAL DEFINITION + + +
!     percent_sand - sand fraction expressed as percent
!     percent_clay - clay fraction expressed as percent
!     acoef - intermediate expression
!     bcoef - intermediate expression

      percent_sand = sandf * 100.0
      percent_clay = clayf * 100.0

!     equations are only valid in this range. This makes any outside
!     stay at the boundary value.
      percent_sand = max( min( percent_sand, 95.0), 5.0)
      percent_clay = max( min( percent_clay, 60.0), 5.0)

      sand_2 = percent_sand * percent_sand
      clay_2 = percent_clay * percent_clay

      acoef = exp(-4.396 - 0.0715 * percent_clay -                      &
     &        4.88e-4 * sand_2 - 4.285e-5 * sand_2 * percent_clay)

      bcoef = - 3.140 - 0.00222 * clay_2                                &  
     &        - 3.484e-5 * sand_2 * percent_clay

      sat = 0.332 - 7.251e-4 * percent_sand                             &
     &    + 0.1276 * log10(percent_clay)

      if ((acoef .ne. 0.0) .and. (bcoef .ne. 0.0)) then
          fc   = (0.3333/ acoef)**(1.0 / bcoef)
          pwp  = (15.0  / acoef)**(1.0 / bcoef)
      end if

!      if (sat .ne. 0.0) then
!          ksat = exp((12.012 - 0.0755 * percent_sand)  +
!     &        (- 3.895 + 0.03671 * percent_sand 
!     &         - 0.1103 * percent_clay + 8.7546e-4 * clay_2) / sat)
!      end if

      return
      end
