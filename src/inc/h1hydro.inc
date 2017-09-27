!$Author$
!$Date$
!$Revision$
!$HeadURL$

!     This common block contains the state variables relating to
!     the daily hydrologic parameters.

!     Parameter include files:

!     + + + VARIABLE DECLARATIONS + + +

!      real             ahrwc(mnsz,mnsub)
!      real             ahrwcdmx(mnsz,mnsub)
!      real             aheaep(mnsz,mnsub)
!      real             ah0cb(mnsz,mnsub)
!      real             ahrsk(mnsz,mnsub)
      real             ahfwsf(mnsub)
      real             ahzsno(mnsub)
      real             ahtsno(mnsub)
      real             ahfsnfrz(mnsub)
!      real             ahzirr(mnsub)
      real             ahzdmaxirr(mnsub)
      real             ahratirr(mnsub)
      real             ahdurirr(mnsub)
      real             ahlocirr(mnsub)
      real             ahminirr(mnsub)
      integer          am0monirr(mnsub)
      real             ahmadirr(mnsub)
      integer          ahndayirr(mnsub)
      integer          ahmintirr(mnsub)
      real             ahzoutflow(mnsub)
!      real             ahzper(mnsub)
!      real             ahzrun(mnsub)
      real             ahzinf(mnsub)
      real             ahzsmt(mnsub)
      real             ahzwid(mnsub)
      real             ahzeasurf(mnsub)
!      real             ahztranspdepth(mnsub)
      real             ahzfurcut(mnsub)
      real             ahztransprtmin(mnsub)
      real             ahztransprtmax(mnsub)

      common / h1hydro /                                                &
     &                  ahfwsf,                                         &
!     &                  ahrwc, ahrwcdmx, aheaep, ah0cb, ahrsk, ahfwsf,  &
     &                  ahzsno, ahtsno, ahfsnfrz,                       &
     &                  ahzdmaxirr, ahratirr, ahdurirr,                 &
!     &                  ahzirr, ahzdmaxirr, ahratirr, ahdurirr,         &
     &                  ahlocirr, ahminirr, am0monirr, ahmadirr,        &
     &                  ahndayirr, ahmintirr, ahzoutflow,               &
     &                  ahzinf,                                         &
!     &                  ahzper, ahzrun, ahzinf,                         &
!     &                  ahzsmt, ahzwid, ahzeasurf, ahztranspdepth,      &
     &                  ahzsmt, ahzwid, ahzeasurf,                       &
     &                  ahzfurcut, ahztransprtmin, ahztransprtmax

      save :: /h1hydro/

!     + + + VARIABLE DEFINITIONS + + +

!     ahrwc  - Soil water content (Kg/Kg)
!     ahrwcdmx - daily maximum soil water content (Kg/Kg)
!     aheaep - Soil air entry potential (J/kg)
!     ah0cb  - Power of Brooks and Corey water release curve model (unitless)
!     ahrsk  - Saturated soil hydraulic conductivity (m/s)
!     ahfwsf - Crop growth water stress factor (unitless)
!     ahzsno - Water content of snow (mm)
!     ahtsno - temperature of snow layer (C)
!     ahfsnfrz - fraction of snow layer water content which is frozen
!     ahzirr - Single day irrigation water applied (mm)
!     ahzdmaxirr - characteristic maximum irrigation system application depth (mm)
!     ahratirr - characteristic irrigation system application rate (mm/hour)
!     ahdurirr - duration of irrigation water application (hours) 
!                corresponding to the characteristic maximum irrigation
!                system application depth. This is used to set the rate (depth / duration)
!     ahlocirr - emitter location point (mm)
!                positive is above the soil surface
!                negative is below the soil surface
!     ahminirr - minimum irrigation application amount (mm)
!     am0monirr - flag setting monitoring for irrigation need
!                 0 - do not monitor irrigation need
!                 1 - monitor irrigation need
!     ahmadirr - management allowed depletion used in monitoring irrigation
!                0.0 sets up replacing yesterdays water loss today
!                1.0 schedules next application at wilting point
!     ahndayirr - the next simulation day on which an irrigation can occur (day)
!     ahmintirr - minimum interval for irrigation application (days)
!     ahzoutflow - height of runoff outlet above field surface (m)
!     ahzper - daily deep percolation (mm/day)
!     ahzrun - daily surface runoff (mm/day)
!     ahzinf - daily surface infiltration
!     ahzsmt - Snow melt (mm)
!     ahzwid - Water infiltration depth (mm)
!     ahzeasurf - accumulated surface evaporation since last complete rewetting (mm)
!     ahztranspdepth - depth in soil from which transpiration is extracted (m)
!                     when crop is furrow planted, this is deeper than root depth
!                     and is used in place of it when calling transp subroutine
!     ahzfurcut - estimated furrow bottom depth below flat soil surface (mm)
!     ahztransprtmin - root depth where transpiration depth reduction begins (m)
!     ahztransprtmax - root depth where transpiration depth equals root depth (m)
