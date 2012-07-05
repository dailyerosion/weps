!$Author$
!$Date$
!$Revision$
!$HeadURL$
      subroutine poolupdate(                                            &
     &           bdmstandstem, bdmstandleaf, bdmstandstore,             &
     &           bdmflatstem, bdmflatleaf, bdmflatstore,                &
     &           bdmflatrootstore, bdmflatrootfiber,                    &
     &           bdmbgstemz, bdmbgleafz, bdmbgstorez,                   &
     &           bdmbgrootstorez, bdmbgrootfiberz,                      &
     &           bdzht, bddstm, bdxstmrep, bdgrainf,                    &
     &           bdmbgstem, bdmbgleaf, bdmbgstore,                      &
     &           bdmbgrootstore, bdmbgrootfiber,                        &
     &           bdm, bdmst, bdmf, bdmrt, bdmrtz, bdmbg, bdmbgz,        &
     &           bdrsai, bdrlai, bdrsaz, bdrlaz,                        &
     &           bdffcv, bdfscv, bdftcv, bdfcancov,                     &
     &           bdrcd, bddstmtot, bdrsaitot, bdrlaitot,                &
     &           bdrcdtot, bdmtot, bdmtotto4, bdmsttot, bdmftot,        &
     &           bdmbgtot, bdmbgtotto4, bdmrttot, bdmrttotto4,          &
     &           bdffcvtot, bdfscvtot, bdftcvtot, bdftcancov,           &
     &           bnslay, bszlyd, bcovfact, bdxstm, bd0sla, bd0ck)

!     INCLUDE
      include 'p1const.inc'
      include 'p1werm.inc'

!     + + + FUNCTION DECLARATIONS + + +

      real biodrag

!     + + + VARIABLE DECLARATIONS + + +

      ! state variables
      real bdmstandstem(mnbpls)
      real bdmstandleaf(mnbpls)
      real bdmstandstore(mnbpls)

      real bdmflatstem(mnbpls)
      real bdmflatleaf(mnbpls)
      real bdmflatstore(mnbpls)

      real bdmflatrootstore(mnbpls)
      real bdmflatrootfiber(mnbpls)

      real bdmbgstemz(mnsz,mnbpls)
      real bdmbgleafz(mnsz,mnbpls)
      real bdmbgstorez(mnsz,mnbpls)

      real bdmbgrootstorez(mnsz,mnbpls)
      real bdmbgrootfiberz(mnsz,mnbpls)

      real bdzht(mnbpls)
      real bddstm(mnbpls)
      real bdxstmrep(mnbpls)
      real bdgrainf(mnbpls)

      ! derived variables
      real bdmbgstem(mnbpls)
      real bdmbgleaf(mnbpls)
      real bdmbgstore(mnbpls)
      real bdmbgrootstore(mnbpls)
      real bdmbgrootfiber(mnbpls)
      real bdm(mnbpls)
      real bdmst(mnbpls)
      real bdmf(mnbpls)
      real bdmrt(mnbpls)
      real bdmrtz(mnsz,mnbpls)
      real bdmbg(mnbpls)
      real bdmbgz(mnsz,mnbpls)
      real bdrsai(mnbpls)
      real bdrlai(mnbpls)
      real bdrsaz(mncz,mnbpls)
      real bdrlaz(mncz,mnbpls)
      real bdffcv(mnbpls)
      real bdfscv(mnbpls)
      real bdftcv(mnbpls)
      real bdfcancov(mnbpls)
      real bdrcd(mnbpls)

      ! derived variables (all pools)
      real bddstmtot
      real bdrsaitot
      real bdrlaitot
      real bdrcdtot
      real bdmtot
      real bdmtotto4
      real bdmsttot
      real bdmftot
      real bdmbgtot
      real bdmbgtotto4
      real bdmrttot
      real bdmrttotto4
      real bdffcvtot
      real bdfscvtot
      real bdftcvtot
      real bdftcancov

      ! database variables
      integer bnslay
      real bszlyd(mnsz)
      real bcovfact(mnbpls)
      real bdxstm(mnbpls)
      real bd0sla(mnbpls)
      real bd0ck(mnbpls)

!     + + + PURPOSE + + +
      ! calculates values of derived variables based on the present values
      ! or the state variables. The derived variables are commonly used
      ! where residue totals are required.

!     + + + VARIABLE DEFINITIONS + + +

! for each of the residue age pools and each subregion

!     bdmstandstem  - standing stem mass (kg/m^2)
!     bdmstandleaf  - standing leaf mass (kg/m^2)
!     bdmstandstore - standing storage mass (kg/m^2)b

!     bdmflatstem  - flat stem mass (kg/m^2)
!     bdmflatleaf  - flat leaf mass (kg/m^2)
!     bdmflatstore - flat storage mass (kg/m^2)

!     bdmflatstore - flat storage root mass (kg/m^2)
!     bdmflatfiber - flat fibrous root mass (kg/m^2)

!     bdmbgstem  - buried stem mass (kg/m^2)
!     bdmbgleaf  - buried leaf mass (kg/m^2)
!     bdmbgstore - buried (from above ground) storage mass (kg/m^2)

!     bdmbgstemz  - buried stem mass by layer (kg/m^2)
!     bdmbgleafz  - buried leaf mass by layer (kg/m^2)
!     bdmbgstorez - buried (from above ground) storage mass by layer (kg/m^2)

!     bdmbgrootstorez - buried storage root mass by layer (kg/m^2)
!     bdmbgrootfiberz - buried fibrous root mass by layer (kg/m^2)

!     bdzht  - Residue height (m)
!     bddstm - Number of residue stems per unit area (#/m^2)
!     bdxstmrep - a representative diameter so that addstm*adxstmrep*adzht=adrsai
!     bdgrainf - internally computed grain fraction of reproductive mass

!     bdmbgstem - buried residue stem mass (kg/m^2)
!     bdmbgleaf - buried residue leaf mass (kg/m^2)
!     bdmbgstore - buried residue storage mass (kg/m^2)

!     bdmbgrootstore - buried storage root mass (kg/m^2)
!     bdmbgrootfiber - buried fibrous root mass (kg/m^2)

!     these totals below are summed across pools
!     (since they are no longer state variables, we can do that)

!     bdm   - Total residue mass (standing + flat + roots + buried) (kg/m^2)
!     bdmst - Standing residue mass (standstem + standleaf + standstore) (kg/m^2)
!     bdmf  - Flat residue mass (flatstem + flatleaf + flatstore) (kg/m^2)
!     bdmrt - Buried residue root mass (rootfiber + rootstore)(kg/m^2)
!     bdmrtz - Buried residue root mass by soil layer (kg/m^2)
!     bdmbg - Buried residue mass (kg/m^2) Excludes root mass below the surface.
!     bdmbgz - Buried residue mass by soil layer (kg/m^2)

!     bdrsai - Residue stem area index (m^2/m^2)
!     bdrlai - Residue leaf area index (m^2/m^2)
!     bdrsaz - Residue stem area index by height (1/m)
!     bdrlaz - Residue leaf area index by height (1/m)

!     bdrcd  - effective Biomass silhouette area (SAI+LAI) (m^2/m^2)
!              (combination of leaf area and stem area indices)

!     bdffcv - Residue biomass cover - flat (m^2/m^2)
!     bdfscv - Residue biomass cover - standing (m^2/m^2)
!     bdftcv - Residue biomass cover - total (m^2/m^2)
!              (adffcv + adfscv)

!     bdmtot   - Total residue mass across pools (standing + flat + roots + buried) (kg/m^2)
!     bdmtotto4 - Total residue mass across pools (standing + flat + roots + buried to a 4 inch depth) (kg/m^2)
!     bdmsttot - Standing residue mass across pools (standstem + standleaf + standstore) (kg/m^2)
!     bdmftot  - Flat residue mass across pools (flatstem + flatleaf + flatstore) (kg/m^2)
!     bdmbgtot  - Buried residue mass across pools (kg/m^2)
!     bdmbgtotto4 - Buried (to a 4 inch depth) residue mass across pools (kg/m^2)
!     bdmrttot  - Buried residue root mass across pools (kg/m^2)
!     bdmrttotto4 - Buried (to a 4 inch depth) residue root mass across pools (kg/m^2)

!     bddstmtot - total number of residue stems  per unit area (#/m^2)
!     bdrsaitot - total of stem area index across pools (m^2/m^2)
!     bdrlaitot - total of leaf area index across pools (m^2/m^2)

!     bdrcdtot  - effective Biomass silhouette area across pools (SAI+LAI) (m^2/m^2)
!                 (combination of leaf area and stem area indices)

!     bdffcvtot - Residue biomass cover across pools - flat (m^2/m^2)
!     bdfscvtot - Residue biomass cover across pools - standing (m^2/m^2)
!     bdftcvtot - Residue biomass cover across pools - total (m^2/m^2)
!                 (adffcvtot + adfscvtot)

!     database variables
!     bnslay - number of soil layers used
!     bszlyd - Depth to bottom of each soil layer(mm)
!     bcovfact - residue cover factor (m^2/kg)
!     bcxstm - residue stem diameter (m)
!     bd0sla - standing residue specific leaf area (m^2/kg) (taken from crop)
!     bd0ck  - light extinction coeffficient (fraction)

!     LOCAL VARIABLES
      integer idx, idy, scilays
      real partlayrat
      real bdmrtto4(mnbpls), bdmbgto4(mnbpls), bdmto4(mnbpls)

!     LOCAL VARIABLE DEFINITIONS
!     idx - indexing variable
!     idy - indexing variable
!     scilays - number of layers in scidepth
!     partlayrat - proportion of last layer in scidepth range
!     bdmrtto4 - Buried residue root mass (rootfiber + rootstore)(kg/m^2) (in scidepth)
!     bdmbgto4 - Buried residue mass (kg/m^2) Excludes root mass below the surface. (in scidepth)
!     bdmto4   - Total residue mass (standing + flat + roots + buried) (kg/m^2) (in scidepth)
   
      ! parameter to control depth of slice for SCI
      real scidepth
      parameter( scidepth = 101.6 ) ! mm 101.6 = 4 inches

      ! accumulate layer values into pool mass totals
      do idy = 1, mnbpls
          bdmbgstem(idy) = 0.0
          bdmbgleaf(idy) = 0.0
          bdmbgstore(idy) = 0.0
          bdmbgrootstore(idy) = 0.0
          bdmbgrootfiber(idy) = 0.0
          do idx = 1, bnslay
              bdmbgstem(idy) = bdmbgstem(idy) + bdmbgstemz(idx,idy)
              bdmbgleaf(idy) = bdmbgleaf(idy) + bdmbgleafz(idx,idy)
              bdmbgstore(idy) = bdmbgstore(idy) + bdmbgstorez(idx,idy)
              bdmbgrootstore(idy) = bdmbgrootstore(idy)                 &
     &                            + bdmbgrootstorez(idx,idy)
              bdmbgrootfiber(idy) = bdmbgrootfiber(idy)                 &
     &                            + bdmbgrootfiberz(idx,idy)
          end do
      end do

      ! sum buried root and residue masses for each layer across pools
      do idx = 1, bnslay
          do idy = 1, mnbpls
              bdmrtz(idx,idy) = bdmbgrootstorez(idx,idy)                &
     &                        + bdmbgrootfiberz(idx,idy)
              bdmbgz(idx,idy) = bdmbgstemz(idx,idy)                     &
     &                        + bdmbgleafz(idx,idy)                     &
     &                        + bdmbgstorez(idx,idy)
          end do
      end do

      ! sum root and below ground from layer values
      do idy = 1, mnbpls
          bdmrt(idy) = 0.0
          bdmbg(idy) = 0.0
          do idx = 1, bnslay
              bdmrt(idy) = bdmrt(idy) + bdmrtz(idx,idy)
              bdmbg(idy) = bdmbg(idy) + bdmbgz(idx,idy)
          end do
      end do

      ! find the layers for the SCI depth (4 inches)
      scilays = 0
      do idx = 1, bnslay
          if( scidepth .le. bszlyd(idx) ) then
              scilays = idx
              exit
          end if
      end do
      if( scilays .eq. 0 ) then
          ! the soil is thinner than the scidepth
          scilays = bnslay
      end if
      ! calculate partial layer ratio
      if( scilays .gt. 1 ) then
          partlayrat = (scidepth - bszlyd(scilays-1))                   &
     &               / (bszlyd(scilays) - bszlyd(scilays-1))
      else
          partlayrat = scidepth / bszlyd(scilays)
      end if

      ! sum root and below ground from layer values to the SCI depth (4 inches)
      do idy = 1, mnbpls
          bdmrtto4(idy) = 0.0
          bdmbgto4(idy) = 0.0
          do idx = 1, scilays-1
              bdmrtto4(idy) = bdmrtto4(idy) + bdmrtz(idx,idy)
              bdmbgto4(idy) = bdmbgto4(idy) + bdmbgz(idx,idy)
          end do
          bdmrtto4(idy) = bdmrtto4(idy) + bdmrtz(scilays,idy)*partlayrat
          bdmbgto4(idy) = bdmbgto4(idy) + bdmbgz(scilays,idy)*partlayrat
      end do

      ! sum above ground (stem + leaf + store) by and across pools
      bdmtot = 0.0
      bdmtotto4 = 0.0
      bdmftot = 0.0
      bdmsttot = 0.0
      bdmbgtot = 0.0
      bdmbgtotto4 = 0.0
      bdmrttot = 0.0
      bdmrttotto4 = 0.0
      do idy = 1, mnbpls
          bdmst(idy) = bdmstandstem(idy) + bdmstandleaf(idy)            &
     &               + bdmstandstore(idy)
          bdmf(idy) = bdmflatstem(idy) + bdmflatleaf(idy)               &
     &              + bdmflatstore(idy) + bdmflatrootstore(idy)         &
     &              + bdmflatrootfiber(idy)
          bdm(idy) = bdmst(idy) + bdmf(idy) + bdmrt(idy) + bdmbg(idy)
          bdmto4(idy) = bdmst(idy) + bdmf(idy) + bdmrtto4(idy)          &
     &                + bdmbgto4(idy)
          bdmtot = bdmtot + bdm(idy)
          bdmtotto4 = bdmtotto4 + bdmto4(idy)
          bdmftot = bdmftot + bdmf(idy)
          bdmsttot = bdmsttot + bdmst(idy)
          bdmbgtot = bdmbgtot + bdmbg(idy)
          bdmbgtotto4 = bdmbgtotto4 + bdmbgto4(idy)
          bdmrttot = bdmrttot + bdmrt(idy)
          bdmrttotto4 = bdmrttotto4 + bdmrtto4(idy)
      end do

!      write(*,*) "scilays, partlayrat, bdmtot, bdmtotto4",              &
!     &            scilays, partlayrat, bdmtot, bdmtotto4

      bddstmtot = 0.0
      bdrlaitot = 0.0
      bdrsaitot = 0.0
      bdffcvtot = 0.0
      bdfscvtot = 0.0
      bdftcvtot = 0.0
      bdftcancov = 0.0
      do idy = 1, mnbpls
          ! total residue stems
          bddstmtot = bddstmtot + bddstm(idy)

          ! calculate residue stem area index
          ! (plants/m^2 ground) * m * m/plant = m^2 stem / m^2 ground
          bdrsai(idy) = bddstm(idy)*bdzht(idy)*bdxstmrep(idy)

          bdrsaitot = bdrsaitot + bdrsai(idy)

          ! leaf area index for standing material
          ! m^2 leaf/kg * kg/m^2 ground = m^2 leaf/m^2 ground
          bdrlai(idy) = bd0sla(idy) * bdmstandleaf(idy)

          bdrlaitot = bdrlaitot + bdrlai(idy)

          ! effective silhouette
          bdrcd(idy) = biodrag(bdrlai(idy), bdrsai(idy),                &
     &                 0.0, 0.0, 0, 0.0, bdzht(idy), 0.0)

          ! set stem and leaf area by plant height increments
          ! these are divided equally for a first approximation
          do idx = 1, mncz
              bdrsaz(idx,idy) = bdrsai(idy) / mncz
              bdrlaz(idx,idy) = bdrlai(idy) / mncz
          end do

          ! Residue cover calculations.
          ! Overlap only applies when adding flat and flat, not flat and standing,
          ! or standing and standing.

          ! cover from flat mass
          ! estimated using Gregory, 1982. Trans. ASAE 25:1333-1337
          ! fraction (m2/m2) modified to take overlap into account.
          bdffcv(idy) = 1.0 - exp( -bcovfact(idy) * bdmf(idy) )

          ! cover from standing stems
          bdfscv(idy) = bddstm(idy) * pi * ( bdxstm(idy)/2.0 )**2.0
          if (bdfscv(idy) > 1.0) bdfscv(idy) = 1.0

          ! total cover (flat + standing)
          bdftcv(idy) = bdffcv(idy) + bdfscv(idy) !no overlap
          if (bdftcv(idy) > 1.0) bdftcv(idy) = 1.0

          bdffcvtot = bdffcvtot + (1.0 - bdffcvtot) * bdffcv(idy) !flat, with overlap
          bdfscvtot = bdfscvtot + bdfscv(idy) !standing, no overlap

          ! residue leaf interception area (canopy cover)
          bdfcancov(idy) = 1.0 - exp( - bd0ck(idy) * bdrlai(idy))
          bdftcancov = bdftcancov + bdfcancov(idy) * (1.0 - bdftcancov)

      end do
      if (bdfscvtot > 1.0) bdfscvtot = 1.0
      bdftcvtot = bdffcvtot + bdfscvtot !total, no overlap
      if (bdftcvtot > 1.0) bdftcvtot = 1.0

      ! effective silhouette
      bdrcdtot = biodrag(bdrlaitot, bdrsaitot,                          &
     &                 0.0, 0.0, 0, 0.0, 0.0, 0.0)

      return
      end