!$Author$
!$Date$
!$Revision$
!$HeadURL$

!     variables defined to track cumulative water balance
!     so it can be periodically reported (works only with one subregion)

!     Parameter include files:  p1werm.inc

      real cumprecip(mnsub), cumrunoff(mnsub), cumevap(mnsub)
      real cumtrans(mnsub), cumdrain(mnsub)
      real initswc(mnsub), initsnow(mnsub), initday(mnsub)
      real presswc(mnsub), pressnow(mnsub), presday(mnsub)

      integer hprevrotation(mnsub)

      common /balance/                                                  &
     &     cumprecip, cumrunoff, cumevap,                               &
     &     cumtrans, cumdrain,                                          &
     &     initswc, initsnow, initday,                                  &
     &     presswc, pressnow, presday,                                  &
     &     hprevrotation

      save :: /balance/

!     + + + LOCAL DEFINITIONS + + +

!     cumprecip - accumulation of rainfall (mm)
!     cumrunoff - accumulation of runoff (mm) (mm)
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
