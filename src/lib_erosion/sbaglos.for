!$Author$
!$Date$
!$Revision$
!$HeadURL$
!     extensive revision 6-27-06 LH

      subroutine sbaglos (wus, wust, wusto, sf84ic, asvroc,             &
     &                    smaglosmx, smaglos, sf84mn, sf84)

!     + + + PURPOSE + + +
!     calc. minimum erodible fraction (sf84mn) needed to stop emission
!     on aggregated portion of surface at current threshold friction velocity
!     that occurs when smaglos is removed.

!     calc. potential mobile soil reservoir (smaglosmx) for a smooth
!     surface 0.8 m/s friction velocity and sf84ic that armors clod surface.
!
!     calc. available mobile reservoir (smaglos) of sf84 for current surface 
!     based on wus-wust ratio for 

!     + + + ARGUMENT DECLARATIONS + + +
      real wus, wust, wusto, sf84ic, asvroc
      real smaglosmx, smaglos, sf84mn, sf84

!     + + +  ARGUMENT DEFINITIONS + + +
!     wus   - friction velocity (m/s)
!     wust  - friction velocity theshold for en (m/s)
!     wusto - threhold friction velocity = wus minus flat biomass and wetness
!             effects (m/s)
!     sf84ic- surface soil fraction <0.84 mm initial condition
!     asvroc - surface soil volume rock (m^3/m^3)
!     smaglosmx - max mobile soil reservoir of aggregateed sfc.(kg/m^2)
!     smaglos- potential mobile soil reservoir of aggregated sfc.(kg/m^2)
!     sf84mn- surface soil fraction <0.84 mm where wust= wus of ag.sfc.
!     sf84   - soil mass fraction in surface layer < 0.84 mm

!     + + + END SPECIFICATIONS + + +

      ! edit LH 6-26-06
      ! calc. max mobile soil at wus = 0.75 m/s for bare, smooth surface
      ! sf84 is assumed = 0 after this mass removal.
      smaglosmx = exp(2.708 - 7.603*((1-sf84ic)*(1-asvroc) + asvroc))

      ! reduce max mobile soil for roughness, cover, etc.
      smaglos = smaglosmx * (wus - wust) / (0.75 - amin1(wusto,wust))
      smaglos = max(0.0, smaglos)
      !smaglos = min(smaglosmx, smaglos)

      ! find sf84 when smaglos has been removed from control volume mass(denominator).
      ! in sbqout emission from cloddy surface goes to zero at sf84mn
      if ((smaglos .eq. 0.0) .or. (sf84 .eq. 0.0)) then
          sf84mn = sf84
      !elseif (smaglosmx .le. smaglos) then
      !    sf84mn = 0.05
      else
       sf84mn=(smaglosmx - smaglos)/(smaglosmx/(sf84ic*(1.001-asvroc)))
       sf84mn = amax1(0.0, sf84mn)
      endif

      return
      end
