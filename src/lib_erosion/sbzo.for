!$Author$
!$Date$
!$Revision$
!$HeadURL$
!**********************************************************************
! subroutine sbzo
!**********************************************************************
      subroutine sbzo (sxprg, szrgh, slrr,                              &
     &                 wzoflg, bdrlai, bdrsai, bbzht,                   &
     &                 bcrlai, bcrsai, bczht,                           &
     &                 bcxrow, bc0rg, wzorg, wzorr,                     &
     &                 wzzo, wzzov, awzzo, brcd)

!     +++ PURPOSE +++
!     Calc. aerodynamic roughness parm., wzzo, with no standing biomass
!           wzzo is used by sbwust

!     Calc. aerodynamic roughness parm. as wzzov, if standing biomass
!              else let wzzov = wzzo
!         wzzov is used by sbwus
!     Calc. biomass drag coef.(brcd) in function biodrag
!            brcd is also used by sbwus

!     set anem aero. roughness and field roughness equal when anem. at
!         the field site, ie. wzoflg = 1
!     to calculate aerodynamic roughness
!     of vegetation canopy. Ref. Trans ASAE 31(3):769-775
!     Armbrust and Bilbro, 1995

      use p1erode_def, only: WZZO_MIN, WZZO_MAX

!     +++ ARGUMENT DECLATION +++
      real sxprg, szrgh, slrr
      integer wzoflg
      real bdrlai, bdrsai, bbzht
      real bcrlai, bcrsai, bczht, bcxrow
      integer bc0rg
      real wzorg, wzorr, wzzo, wzzov, awzzo, brcd
     
!     +++ ARGUMENT DEFINITIONS +++
!     sxprg  - row/dike spacing parallel the wind (mm)
!     szrgh  - ridge height (mm)
!     wzoflg - flag=0 - anemometer at station
!              flag=1 - anemometer at field
!     slrr   - random roughness (mm)
!     bdrlai - residue leaf area index (total)(m2/m2)
!     bdrsai - residue stem area index (total)(m2/m2)
!     bbzht  - composite average residue height (m)
!     bcrlai - crop leaf area index (m2/m2)
!     bcrsai - crop stem area index (m2/m2)
!     bczht  - crop height (m)
!     bcxrow - crop row spacing (m)
!     bc0rg  - flag=0 - crop planted in furrow bottom
!              flag=1 - crop planted on ridge top
!     wzzo   - aerodynamic roughness of surface below canopy (mm)
!     wzorg  - aerodynamic roughness of ridge
!     wzorr  - aerodynamic roughness of random roughness
!     wzzov  - aerodynamic roughness length of canopy (mm)
!     awzzo  - aerodynamic roughness at anemom. site (mm)
!     brcd   - biomass drag coefficient

!     +++ FUNCTIONS CALLED
      real biodrag

!     +++ LOCAL VARIABLES +++
      real  hl, bht

!     +++ LOCAL VARIABLE DEFINITIONS +++
!     h1    - ratio of ridge height to parallel ridge spacing
!     bht   - biomass height (mm)

!     +++ INCLUDE FILES+++
      include 'p1unconv.inc'

!     +++ PARAMETERS +++
!     parameter(pid180 = 3.14159/180)
!     pid180- radians per degree
!
!     +++ END SPECIFICATIONS +++
      !Note: in BLOCK.FOR
      !wzoflg should be set to 1 and anemomht changed
      !      if the anemomenter is at the field site to
      !      obtain correct values from SBZO

!calc. for ridge aerodynamic roughness

      if (szrgh .gt. 5.0) then
        hl   = szrgh / sxprg
!     winds are never continually normal to ridges, so restrict hl.
        hl = min(0.20,hl)
         wzorg = szrgh * 1/(-64.1+135.5*hl+(20.84/sqrt(hl)))
       else
          wzorg = 0.
       endif

!calculation for random aerodynamic roughness

      wzorr = slrr*0.3
      !set upper and lower limits on aerodynamic roughness
      wzorr = min(WZZO_MAX, wzorr)   ! RR <= ~100.0mm
      wzorr = max(wzorr, WZZO_MIN)   ! RR >= ~1.67mm

!estimate combined ridge and random aerodynamic roughness
      !(later- no data sets at present)
      !chose the largest of the two.
      wzzo = max (wzorg, wzorr)

!calculate aerodynamic roughness of vegetation, if present

      ! calculate "effective" biomass drag coefficient
      ! new function for effective biomass drag coef.
       brcd = biodrag( bdrlai, bdrsai, bcrlai, bcrsai, bc0rg,           &
     &                 bcxrow, bczht, szrgh )

      ! convert biomass height to mm
      bht = bbzht * mtomm

      ! calculate roughness length of canopy ( in mm)
      if (brcd .gt. 0.1) then
          wzzov = bht * 1/(17.27-(1.254*alog(brcd)/brcd)-(3.714/brcd))
      else if( (bht .gt. 5.0) .and. (brcd .gt. 0.001) ) then
 !         wzzov = bht*exp(alog(wzzo/bht) + (alog(0.11*bht/wzzo)         &
 !    &          * alog(brcd/0.01))/2.3)   caused Simon's instability
       wzzov = bht*(wzzo/bht+((0.11-wzzo/bht)/4.60517)*alog(brcd/0.001))
      else
          wzzov = 0.0
      endif

      ! choose the maximum of canopy or surface roughness
      wzzov = max(wzzov, wzzo)
!    
      if (wzoflg .eq. 1) then
         ! anemom. in field set awzzo to wzzov
         awzzo = wzzov
      endif
!^tmp out
!      write(*,*) 'sbzo out'
!      write(*,*) 'wzorg      wzorr       wzzov        brcd       bht' 
!      write(*,*) wzorg, wzorr, wzzov, brcd, bht
!^ end tmp
      return
      end

