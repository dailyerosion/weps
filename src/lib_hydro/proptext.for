!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine proptext( nlay, clayf, sandf, organf,                  &
     &                     bulkden, settled_bulkden, proctor_bulkden,   &
     &                     wet_bulkden, wet_set_rat, partden )

!     + + + PURPOSE + + +
!     
!     This subroutine updates the properties that depend on soil texture 
!     (texture can change in the model due to mixing and removal by wind)

!     + + + KEYWORDS + + +
!     texture properties 

      use soilden_mod

!     + + + ARGUMENT DECLARATIONS + + +
      integer nlay
      real sandf(*),clayf(*),organf(*)
      real bulkden(*)
      real settled_bulkden(*)
      real proctor_bulkden(*)
      real wet_bulkden(*)
      real wet_set_rat(*)
      real partden(*)

!     + + + ARGUMENT DEFINITIONS + + +
!     nlay     - number of soil layers to be updated
!     clayf    - fraction of soil mineral portion which is clay
!     sandf    - fraction of soil mineral portion which is sand
!     organf   - fraction of total soil mass which is organic matter
!     bulkden  - bulk density state of the soil.
!     settled_bulkden - settled bulk density (Mg/m^3)
!     proctor_bulkden - proctor bulk density (Mg/m^3)
!     wet_bulkden - 1/3 bar bulk density (Mg/m^3)
!     wet_sat_rat - Nondimensional ratio of wet to settled bulk density
!     partden  - particle density (Mg/m^3)

!     + + + LOCAL VARIABLES + + +
      integer lay

!     + + + LOCAL VARIABLE DEFINITIONS + + +

!     + + + END SPECIFICATIONS + + + 

      do lay=1,nlay
          ! settled bulk density
          settled_bulkden(lay) = setbds( clayf(lay), sandf(lay),        &
     &                                   organf(lay))

          ! calculate an average soil particle density
          partden(lay) = setpartden( organf(lay) )

          ! reference bulk density
          proctor_bulkden(lay) = setbdproc( clayf(lay), sandf(lay),     &
     &                                      organf(lay), partden(lay))

          ! make sure particle density is significantly greater than settled bulk density
          if( partden(lay).lt.(1.2*settled_bulkden(lay)) ) then
              partden(lay) = 1.2*settled_bulkden(lay)
          endif

          if( wet_set_rat(lay) .lt. 0.0 ) then
              ! ratio wet_set_rat is negative, so initialize it
              wet_set_rat(lay) = (partden(lay) - settled_bulkden(lay))  &
     &                         / (partden(lay) - wet_bulkden(lay))
              if( wet_set_rat(lay) .gt. 1.0 ) then
                  ! wet bulk density is greater than settled bulk density, adjust
                  write(*,*)'WARNING: settled bd(',settled_bulkden(lay),&  ! NOTE:  Changed to "WARNING" so message
     &                    ') < wet bd (',bulkden(lay),'), wbd = sbd'   !wouldn't display in GUI popup Warning dialog box
                  wet_set_rat(lay) = 1.0
                  wet_bulkden(lay) = settled_bulkden(lay)
              end if
              if( bulkden(lay) .gt. settled_bulkden(lay) ) then
                  ! do not start simulation in compacted state
                  bulkden(lay) = settled_bulkden(lay)
                  write(*,*)'WARNING: settled bd(',settled_bulkden(lay),&  ! NOTE:  Changed to "WARNING" so message
     &                    ') < initial bd(',bulkden(lay),'), bd = sbd'   !wouldn't display in GUI popup Warning dialog box
              end if

          else
              ! ratio wet_set_rat is positive, so use it to adjust wet_bulkden
              wet_bulkden(lay) = partden(lay)                           &
     &        - (partden(lay) - settled_bulkden(lay)) / wet_set_rat(lay)

          end if

      end do

      end
