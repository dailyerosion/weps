!$Author$
!$Date$
!$Revision$
!$HeadURL$

!     variables defined to track cumulative water balance
!     so it can be periodically reported (works only with one subregion)

!     Parameter include files:  p1werm.inc

      real cumprecip(mnsub), cumirrig(mnsub), cumrunoff(mnsub)
      real cumevap(mnsub), cumtrans(mnsub), cumdrain(mnsub)
      real initswc(mnsub), initsnow(mnsub), initday(mnsub)
      real presswc(mnsub), pressnow(mnsub), presday(mnsub)

      integer hprevrotation(mnsub)

      common /balance/                                                  &
     &     cumprecip, cumirrig, cumrunoff,                              &
     &     cumevap, cumtrans, cumdrain,                                 &
     &     initswc, initsnow, initday,                                  &
     &     presswc, pressnow, presday,                                  &
     &     hprevrotation

      save :: /balance/

!     + + + LOCAL DEFINITIONS + + +

!     cumprecip - accumulation of rainfall (mm)
!     cumirrig  - accumulation of irrigation (mm)
!     cumrunoff - accumulation of runoff (mm)
!     cumevap   - accumulation of evaporation (mm)
!     cumtrans  - accumulation of transpiration (mm)
!     cumdrain  - accumulation of drainage (mm)
!     initswc   - initial soil water content (mm)
!     initsnow  - initial snow water content (mm)
!     initday   - initial day
!     presswc   - present soil water content (mm)
!     pressnow  - present snow water content (mm)
!     presday   - present day
!     hprevrotation - rotation count number of previously printed hydro balance
