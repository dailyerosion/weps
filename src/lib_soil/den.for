!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!
      subroutine den(                                                   &
     &  csdblk, csdsblk, csdwblk, cszlyt, csdagd,                       &
     &  chrwc0, chrwc, chrwca, chrwcw,                                  &
     &  bhzinf, chzwid, trigger)

!     + + + ARGUMENT DECLARATIONS + + +

      real csdblk, csdsblk, csdwblk, cszlyt, csdagd
      real chrwc0, chrwc, chrwca, chrwcw
      real bhzinf, chzwid
      integer trigger

!     + + + ARGUMENT DEFINITIONS + + +

!     csdblk - present soil bulk density (Mg/m^3)
!     csdsblk - Settled soil bulk density (Mg/m^3)
!     csdwblk - 1/3 bar soil bulk density (Mg/m^3)
!     cszlyt - Soil layer thickness (mm)
!     csdagd - Soil aggregate density (Mg/m^3)
!     chrwc0 - Soil water content from previous day (g/g)
!     chrwc  - Soil water content on present day (g/g)
!     chrwca - Available soil water content (g/g)
!     chrwcw - Wilting point soil water content (g/g)
!     bhzinf - daily water infiltration depth (mm)
!     chzwid - water infiltration depth (mm)

!     + + + LOCAL VARIABLES + + + 

      real bsdbk0
      integer j, nj
      real wsdblk, dsdblk
      real wszlyt

!     + + + LOCAL DEFINITIONS + + +

!   bsdbk0    - bulk density prior to update by SOIL, Mg/m^3
!   wsdblk    - bulk density of wet soil
!   dsdblk    - bulk density of dry soil
!   wszlyt    - depth of wetness in this layer

!  DENSITY SECTION:
!     update bulk density

!     store initial value of layer density
      bsdbk0 = csdblk

!     daily update density for other forces -- long term
!     only if current bulk density is less than settled bd
      ! removed restriction to allow compaction condition with slow amelioration
      !if (csdblk.lt.csdsblk) then
        dsdblk = csdblk + 0.01*(csdsblk - csdblk)
      !else
      ! dsdblk = csdblk
      !endif

! if water has infiltrated into the layer
      if (chzwid .gt. 0) then
        trigger = ibset(trigger, 7) !wet_bulk_den

!     update for infiltrated water additions in 5 mm increments
        nj =nint(bhzinf/5.)
        wsdblk = csdblk
        do j = 1,nj
          if (wsdblk .lt. 0.97 * csdsblk) then
            wsdblk = wsdblk+0.75 * (1-(wsdblk/(0.97 * csdsblk)))**1.5
          else
            exit
          endif
        enddo
      else 
! set wsdblk to something so that it won't bomb out as uninit'd later on
        wsdblk = dsdblk
      endif

! get weighted average of wet and dry densities
      wszlyt = min(cszlyt, chzwid)
      csdblk = (wsdblk*wszlyt + dsdblk*(cszlyt-wszlyt)) / cszlyt

! reduce wet depth by this layer's thickness or wet depth
      chzwid = chzwid - wszlyt
   
!     update layer thickness
      cszlyt = cszlyt * bsdbk0 / csdblk
      
!     update aggregate density
      ! After consultation with LH, it was decided that Agg. Density
      ! should be set to a constant in WEPS for now.  The erosion submodel
      ! code has been changed to allow friction velocity values to be adjusted
      ! based upon surface agg. density value.  Since the settled bulk
      ! density value is not a good alternative value to set the agg. density
      ! value too, it has been changed to 1.8.  This needs to be moved
      ! (probably removed entirely) once the WEPS interface code has been
      ! changed to default the incoming agg. density values to 1.8
      ! Mar. 15, 2006 - LEW

      ! csdagd = csdsblk
      csdagd = 1.8  ! Set to constant value in WEPS for now - LEW
!
      end
