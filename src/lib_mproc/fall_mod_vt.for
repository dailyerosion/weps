!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine fall_mod_vt ( rate_mult_vt, thresh_mult_vt,            &
     &                         sel_pool, fracarea,                      &
     &                         bcrbc, bcdkrate, bcddsthrsh,             &
     &                         bdrbc, bdkrate, bddsthrsh )

!     + + + PURPOSE + + +
!     This subroutine modifies the stem fall rate for standing crop and
!     residue material using a multiplier. The rate multiplier is
!     selected based on the toughness class and adjusted if the part of 
!     the area is affected.

!     + + + KEYWORDS + + +
!     standing stem fall fate

!     + + + COMMON BLOCKS + + +
      include 'p1werm.inc'

!     + + + ARGUMENT DECLARATIONS + + +
      real       rate_mult_vt(mnrbc)
      real       thresh_mult_vt(mnrbc)
      integer    sel_pool
      real       fracarea
      integer    bcrbc
      real       bcdkrate(mndk)
      real       bcddsthrsh
      integer    bdrbc(mnbpls)
      real       bdkrate(mndk,mnbpls)
      real       bddsthrsh(mnbpls)

!     + + + ARGUMENT DEFINITIONS + + +
!     rate_mult_vt - standing stem fall rate multiplier
!     thresh_mult_vt - standing stem fall threshhold multiplier
!     sel_pool - pool to which percentages will be applied
!            0 - don't apply to anything
!            1 - apply to crop pool
!            2 - apply to temporary pool
!            3 - apply to crop and temporary pools
!            4 - apply to residue pools
!            5 - apply to crop and residue pools
!            6 - apply to temporary and residue pools
!            7 - apply to crop, temporary and residue pools
!                this corresponds to the bit pattern:
!                msb(residue, temporary, crop)lsb
!     fracarea - fraction of surface area affected by operation
!     bcrbc  - crop residue burial class (it exists in crop so
!              it can be carried into residue)
!         1   o Fragile-very small (soybeans) residue
!         2   o Moderately tough-short (wheat) residue
!         3   o Non fragile-med (corn) residue
!         4   o Woody-large residue
!         5   o Gravel-rock
!     bcdkrate - crop array of decomposition rate parameters
!     bcddsthrsh  - crop decomposition days required for first stem fall
!     bdrbc  - residue burial class for each residue age pool
!         1   o Fragile-very small (soybeans) residue
!         2   o Moderately tough-short (wheat) residue
!         3   o Non fragile-med (corn) residue
!         4   o Woody-large residue
!         5   o Gravel-rock
!     bdkrate - array of decomposition rate parameters for
!               each residue age class
!     bddsthrsh  - decomposition days required for first stem fall for
!                  each residue age class

!     + + + ACCESSED COMMON BLOCK VARIABLE DEFINITIONS + + +
!     mnrbc         - number of residue burial classes
!     mndk          - number of residue decomposition parameters
!     mnbpls        - max number of decomposition pools (currently=3)

!     + + + PARAMETERS + + +

!     + + + LOCAL VARIABLES + + +
      integer idx, idy
      real area_adj_rate_mult(mnrbc)
      real area_adj_thresh_mult(mnrbc)

!     + + + LOCAL VARIABLE DEFINITIONS + + +
!     idx - loop variable
!     area_adj_mult - adjust the multiplier based on area fraction

!     + + + END SPECIFICATIONS + + +

      do idx = 1, mnrbc
          area_adj_rate_mult(idx) = 1.0+fracarea*(rate_mult_vt(idx)-1.0)
          area_adj_thresh_mult(idx) = 1.0                               &
     &                              + fracarea*(thresh_mult_vt(idx)-1.0)
      end do

      ! crop pool or temporary pool
      if( BTEST(sel_pool,0) .or. BTEST(sel_pool,1) ) then
          ! Adjust for proper residue burial class
          bcdkrate(5) = bcdkrate(5) * area_adj_rate_mult(bcrbc)
          bcddsthrsh = bcddsthrsh * area_adj_thresh_mult(bcrbc)
      end if
      ! residue pools
      if( BTEST(sel_pool,2) ) then
        ! for each residue pool
        do idy = 1, mnbpls
          ! Adjust for proper residue burial class
          bdkrate(5,idy) = bdkrate(5,idy)*area_adj_rate_mult(bdrbc(idy))
          bddsthrsh(idy) = bddsthrsh(idy)                               &
     &                   * area_adj_thresh_mult(bdrbc(idy))
        end do
      end if

      return
      end
