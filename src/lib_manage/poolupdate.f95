!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine poolupdate(bnslay, bszlyd, residue, restot)

!     + + + PURPOSE + + +
      ! calculates values of derived variables based on the present values
      ! or the state variables. The derived variables are commonly used
      ! where residue totals are required.

      use weps_interface_defs, ignore_me=>poolupdate
      use biomaterial, only: biomatter, biototal
      use p1unconv_mod, only: pi
      use wind_mod, only: biodrag

!     + + +   ARGUMENT DECLARATIONS + + +
      integer :: bnslay
      real, dimension(:), intent(in) :: bszlyd
      type(biomatter), dimension(:), intent(inout) :: residue
      type(biototal), intent(inout) :: restot

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
!     bdrlai - Residue leaf area indexmtot (m^2/m^2)
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
      integer idx, idy
      real bdmrtto4(size(residue)), bdmbgto4(size(residue)), bdmto4(size(residue))
      real bdmrtto15(size(residue)), bdmbgto15(size(residue))

      integer :: ncanlay  ! number of standing crop height divisions

!     LOCAL VARIABLE DEFINITIONS
!     idx - indexing variable
!     idy - indexing variable
!     bdmrtto4 - Buried residue root mass (rootfiber + rootstore)(kg/m^2) (in scidepth)
!     bdmbgto4 - Buried residue mass (kg/m^2) Excludes root mass below the surface. (in scidepth)
!     bdmto4   - Total residue mass (standing + flat + roots + buried) (kg/m^2) (in scidepth)
   
      ! parameter to control depth of slice
      real scidepth
      parameter( scidepth = 101.6 ) ! mm 101.6 = 4 inches for SCI
      real weppdepth
      parameter( weppdepth = 150.0 ) ! mm 150.0 = 5.9 inches for WEPP

      ! accumulate layer values into pool mass totals
      do idy = 1, size(residue)
          residue(idy)%deriv%mbgstem = 0.0
          residue(idy)%deriv%mbgleaf = 0.0
          residue(idy)%deriv%mbgstore = 0.0
          residue(idy)%deriv%mbgrootstore = 0.0
          residue(idy)%deriv%mbgrootfiber = 0.0
          do idx = 1, size(residue(idy)%mass%stemz)
              residue(idy)%deriv%mbgstem = residue(idy)%deriv%mbgstem + residue(idy)%mass%stemz(idx)
              residue(idy)%deriv%mbgleaf = residue(idy)%deriv%mbgleaf + residue(idy)%mass%leafz(idx)
              residue(idy)%deriv%mbgstore = residue(idy)%deriv%mbgstore + residue(idy)%mass%storez(idx)
              residue(idy)%deriv%mbgrootstore = residue(idy)%deriv%mbgrootstore + residue(idy)%mass%rootstorez(idx)
              residue(idy)%deriv%mbgrootfiber = residue(idy)%deriv%mbgrootfiber + residue(idy)%mass%rootfiberz(idx)
          end do
      end do

      ! sum buried root and residue masses for each layer and each pool
      do idy = 1, size(residue)
           do idx = 1, size(residue(idy)%mass%rootfiberz)
              residue(idy)%deriv%mrtz(idx) = residue(idy)%mass%rootstorez(idx) + residue(idy)%mass%rootfiberz(idx)
              residue(idy)%deriv%mbgz(idx) = residue(idy)%mass%stemz(idx) + residue(idy)%mass%leafz(idx) &
     &                        + residue(idy)%mass%storez(idx)
          end do
      end do

      ! sum root and below ground from layer values for each pool
      do idy = 1, size(residue)
          residue(idy)%deriv%mrt = 0.0
          residue(idy)%deriv%mbg = 0.0
          do idx = 1, size(residue(idy)%deriv%mrtz)
              residue(idy)%deriv%mrt = residue(idy)%deriv%mrt + residue(idy)%deriv%mrtz(idx)
              residue(idy)%deriv%mbg = residue(idy)%deriv%mbg + residue(idy)%deriv%mbgz(idx)
          end do
      end do

      do idy = 1, size(residue)
          ! sum root and below ground from layer values to the SCI depth (4 inches)
          bdmrtto4(idy) = valbydepth(bnslay, bszlyd, residue(idy)%deriv%mrtz, 2, 0.0, scidepth)
          bdmbgto4(idy) = valbydepth(bnslay, bszlyd, residue(idy)%deriv%mbgz, 2, 0.0, scidepth)
          ! sum root and below ground from layer values to the WEPP adjustment depth (15 cm)
          bdmrtto15(idy) = valbydepth(bnslay, bszlyd, residue(idy)%deriv%mrtz, 2, 0.0, weppdepth)
          bdmbgto15(idy) = valbydepth(bnslay, bszlyd, residue(idy)%deriv%mbgz, 2, 0.0, weppdepth)
      end do

      ! sum mass across pools
      restot%mstandstore = 0.0
      restot%mflatstore = 0.0
      restot%mtot = 0.0
      restot%mtotto4 = 0.0
      restot%mftot = 0.0
      restot%msttot = 0.0
      restot%mbgtot = 0.0
      restot%mbgtotto4 = 0.0
      restot%mbgtotto15 = 0.0
      restot%mrttot = 0.0
      restot%mrttotto4 = 0.0
      restot%mrttotto15 = 0.0
      do idy = 1, size(residue)
          residue(idy)%deriv%mst = residue(idy)%mass%standstem + residue(idy)%mass%standleaf + residue(idy)%mass%standstore
          residue(idy)%deriv%mf = residue(idy)%mass%flatstem + residue(idy)%mass%flatleaf + residue(idy)%mass%flatstore &
                                + residue(idy)%mass%flatrootstore + residue(idy)%mass%flatrootfiber
          residue(idy)%deriv%m = residue(idy)%deriv%mst + residue(idy)%deriv%mf + residue(idy)%deriv%mrt + residue(idy)%deriv%mbg
          bdmto4(idy) = residue(idy)%deriv%mst + residue(idy)%deriv%mf + bdmrtto4(idy) + bdmbgto4(idy)
          restot%mstandstore = restot%mstandstore + residue(idy)%mass%standstore
          restot%mflatstore = restot%mflatstore + residue(idy)%mass%flatstore
          restot%mtot = restot%mtot + residue(idy)%deriv%m
          restot%mtotto4 = restot%mtotto4 + bdmto4(idy)
          restot%mftot = restot%mftot + residue(idy)%deriv%mf
          restot%msttot = restot%msttot + residue(idy)%deriv%mst
          restot%mbgtot = restot%mbgtot + residue(idy)%deriv%mbg
          restot%mbgtotto4 = restot%mbgtotto4 + bdmbgto4(idy)
          restot%mbgtotto15 = restot%mbgtotto15 + bdmbgto15(idy)
          restot%mrttot = restot%mrttot + residue(idy)%deriv%mrt
          restot%mrttotto4 = restot%mrttotto4 + bdmrtto4(idy)
          restot%mrttotto15 = restot%mrttotto15 + bdmrtto15(idy)
      end do

      ! sum layer mass across pools
      do idx = 1, size(restot%mrtz)
          restot%mrtz(idx) = 0.0
          restot%mbgz(idx) = 0.0
          do idy = 1, size(residue)
              restot%mrtz(idx) = restot%mrtz(idx) + residue(idy)%deriv%mrtz(idx)
              restot%mbgz(idx) = restot%mbgz(idx) + residue(idy)%deriv%mbgz(idx)
          end do
      end do

      restot%dstmtot = 0.0
      restot%rlaitot = 0.0
      restot%rsaitot = 0.0
      restot%ffcvtot = 0.0
      restot%fscvtot = 0.0
      restot%ftcvtot = 0.0
      restot%ftcancov = 0.0
      do idy = 1, size(residue)
          ! total residue stems
          restot%dstmtot = restot%dstmtot + residue(idy)%geometry%dstm

          ! calculate residue stem area index
          ! (plants/m^2 ground) * m * m/plant = m^2 stem / m^2 ground
          residue(idy)%deriv%rsai = residue(idy)%geometry%dstm * residue(idy)%geometry%zht * residue(idy)%geometry%xstmrep

          restot%rsaitot = restot%rsaitot + residue(idy)%deriv%rsai

          ! leaf area index for standing material
          ! m^2 leaf/kg * kg/m^2 ground = m^2 leaf/m^2 ground
          residue(idy)%deriv%rlai = residue(idy)%database%sla * residue(idy)%mass%standleaf

          restot%rlaitot = restot%rlaitot + residue(idy)%deriv%rlai

          ! effective silhouette
          residue(idy)%deriv%rcd = biodrag(residue(idy)%deriv%rlai, residue(idy)%deriv%rsai, 0.0, 0.0, 0, 0.0, 0.0, 0.0)

          ! set stem and leaf area by plant height increments
          ! these are divided equally for a first approximation
          ncanlay = size(residue(idy)%deriv%rsaz)
          do idx = 1, ncanlay
              residue(idy)%deriv%rsaz(idx) = residue(idy)%deriv%rsai / ncanlay
              residue(idy)%deriv%rlaz(idx) = residue(idy)%deriv%rlai / ncanlay
          end do

          ! Residue cover calculations.
          ! Overlap only applies when adding flat and flat, not flat and standing,
          ! or standing and standing.

          ! cover from flat mass
          ! estimated using Gregory, 1982. Trans. ASAE 25:1333-1337
          ! fraction (m2/m2) modified to take overlap into account.
          residue(idy)%deriv%ffcv = 1.0 - exp( -residue(idy)%database%covfact * residue(idy)%deriv%mf )

          ! cover from standing stems
          residue(idy)%deriv%fscv = residue(idy)%geometry%dstm * pi * ( residue(idy)%database%xstm/2.0 )**2.0  ! should this really use geometry%xstmrep for consistency
          if (residue(idy)%deriv%fscv > 1.0) residue(idy)%deriv%fscv = 1.0

          ! total cover (flat + standing)
          residue(idy)%deriv%ftcv = residue(idy)%deriv%ffcv + residue(idy)%deriv%fscv !no overlap
          if (residue(idy)%deriv%ftcv > 1.0) residue(idy)%deriv%ftcv = 1.0

          restot%ffcvtot = restot%ffcvtot + (1.0 - restot%ffcvtot) * residue(idy)%deriv%ffcv !flat, with overlap
          restot%fscvtot = restot%fscvtot + residue(idy)%deriv%fscv !standing, no overlap

          ! residue leaf interception area (canopy cover)
          residue(idy)%deriv%fcancov = 1.0 - exp( - residue(idy)%database%ck * residue(idy)%deriv%rlai)
          restot%ftcancov = restot%ftcancov + residue(idy)%deriv%fcancov * (1.0 - restot%ftcancov)

      end do
      if (restot%fscvtot > 1.0) restot%fscvtot = 1.0
      restot%ftcvtot = restot%ffcvtot + restot%fscvtot !total, no overlap
      if (restot%ftcvtot > 1.0) restot%ftcvtot = 1.0

      ! effective silhouette
      restot%rcdtot = biodrag(restot%rlaitot, restot%rsaitot, 0.0, 0.0, 0, 0.0, 0.0, 0.0)

      ! sum area indexes by layer across pools
      do idx = 1, ncanlay
         restot%rsaz(idx) = 0.0
         restot%rlaz(idx) = 0.0
         do idy = 1, size(residue)
              restot%rsaz(idx) = restot%rsaz(idx) + residue(idy)%deriv%rsaz(idx)
              restot%rlaz(idx) = restot%rlaz(idx) + residue(idy)%deriv%rlaz(idx)
         end do
      end do

      ! use sai weighting for average height and representative stem diameter
      restot%zht_ave = 0.0
      restot%xstmrep = 0.0
      restot%zmht = 0.0
      do idy = 1, size(residue)
         if( restot%rsaitot .gt. 0.0 ) then
            restot%zht_ave = restot%zht_ave + residue(idy)%geometry%zht * residue(idy)%deriv%rsai / restot%rsaitot
            restot%xstmrep = restot%xstmrep + residue(idy)%geometry%xstmrep * residue(idy)%deriv%rsai / restot%rsaitot
         end if
         restot%zmht = max( restot%zmht, residue(idy)%geometry%zht )
      end do

      ! use stem number weighting for average root depth (could use buried root mass also)
      ! need to add transfer of root depth from crop to residue before enabling
      !restot%zrtd = 0.0
      !do idy = 1, size(residue)
      !   restot%zrtd = restot%zrtd + residue(idy)%geometry%zrtd * residue(idy)%geometry%dstm / restot%dstmtot
      !end do

      return
      end
