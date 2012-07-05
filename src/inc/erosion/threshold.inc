!$Author$
!$Date$
!$Revision$
!$HeadURL$

!     This common block contains variables used for tracking the conditions
!     that prevented any erosion.

!     + + + VARIABLE DECLARATIONS + + +

      integer             ne_erosion(mnsub)
      integer             ne_snowdepth(mnsub)

      real                ne_wus_anemom(mnsub)
      real                ne_wus_random(mnsub)
      real                ne_wus_ridge(mnsub)
      real                ne_wus_biodrag(mnsub)
      real                ne_wus(mnsub)

      real                ne_bare(mnsub)
      real                ne_flat_cov(mnsub)
      real                ne_surf_wet(mnsub)
      real                ne_ag_den(mnsub)
      real                ne_wust(mnsub)

      real                ne_sfd84(mnsub)
      real                ne_asvroc(mnsub)
      real                ne_wzzo(mnsub)
      real                ne_sfcv(mnsub)

      common /threshold/                                                &
     &    ne_erosion, ne_snowdepth,                                     &
     &    ne_wus_anemom, ne_wus_random, ne_wus_ridge, ne_wus_biodrag,   &
     &    ne_wus, ne_bare, ne_flat_cov, ne_surf_wet, ne_ag_den, ne_wust,&
     &    ne_sfd84, ne_asvroc, ne_wzzo, ne_sfcv

      save :: /threshold/

!     + + + VARIABLE DEFINITIONS + + +
!     ne_erosion - flag indicating that erosion was entered
!                  0 - erosion was not entered
!                  1 - erosion was entered
!     ne_snowdepth - snow depth is great enough that no erosion occurs
!                    0 - snow depth too low to prevent erosion
!                    1 - snow depth deeper than threshold, prevents erosion

!     ne_wus_anemom - anemometer located friction velocity for critical no erosion condition
!     ne_wus_random - site surface random roughness adjusted friction velocity for critical no erosion condition
!     ne_wus_ridge - site surface oriented roughness adjusted friction velocity for critical no erosion condition
!     ne_wus_biodrag - site biodrag adjusted friction velocity for critical no erosion condition
!     ne_wus - friction velocity for critical no erosion condition (biodrag added in)

!     ne_bare - bare friction veolocity greater
!     ne_flat_cov - flat cover increases threshold
!     ne_surf_wet - surface wetness increases threshold
!     ne_ag_den - ag density increases threshold
!     ne_wust - resultant threshold friction velocity

!     ne_sfd84 - fraction of the surface material less than 0.84 mm in diameter
!     ne_asvroc - fraction of the surface matherial greater than 2 mm in diameter
!     ne_wzzo - aerodynamic roughness length of the soil surface below canopy (mm)
!     ne_sfcv - fraction of soil surface which is non emitting
