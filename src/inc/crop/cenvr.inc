!$Author$
!$Date$
!$Revision$
!$HeadURL$

!     file: cenvr.inc

!     This file contains the declaration of the common block of variables
!     related to the weather data and temperature factors.

!     + + + VARIABLE DECLARATIONS + + +
       real vpd,tava,hrlt,hrlty,                                        &
     & dhu, hui, huiy, huirt, huirty, dhuf, ehu, tplant, fhr,           &
     & ts,hu,xlat,cta,ceta,ctab,cetab,ctas,cetas,ctay,cetay,prcp,       &
     & prcpb,prcpy,prcps,ctp,ctpb,ctpy,ctps, frs1, frs2,phu
      integer jda,jd

       common / envr / vpd,tava,hrlt,hrlty,                             &
     & dhu, hui, huiy, huirt, huirty, dhuf, jda, jd, ehu, frs1, frs2,   &
     & tplant,fhr,ts,hu,xlat,cta,ceta,ctab,cetab,ctas,cetas,            &
     & ctay,cetay,prcp,prcpb,prcpy,prcps,ctp,ctpb,ctpy,ctps,phu

       save :: /envr/

!     + + + VARIABLE DEFINITIONS + + +
!    cta - actual cummulated transpiration (mm)
!    ctab - periodic actual cummulated transpiration (mm) on day i
!    ctay - periodic actual cummulated transpiration (mm) on day i-1
!    ctas - seasonal actual cummulated transpiration  (mm)
!    ctp - potential cummulated transpiration (mm)
!    ctpb - periodic potential cummulated transpiration (mm)on day i
!    ctay - periodic potential cummulated transpiration (mm)on day i-1
!    ctas - seasonal potential cummulated transpiration  (mm)
!    ceta - actual cummulated evapotranspiration (mm)
!    cetab - periodic actual cummulated evapotranspiration (mm)on day i
!    cetay - periodic actual cummulated evapotranspiration (mm)on day i-1
!    cetas - seasonal actual cummulated evapotranspiration (mm)
!    dhu - daily heat units (deg. c)
!    dhuf - daily heat unit factor
!    ehu - heat units required from planting seedling emergence
!    frs1 - lower temperature threshold for frost damage (deg. C)
!    frs2 - upper temperature threshold for frost damage (deg. C)
!    fhr - reduction due to day length factor
!    hrlt - day length on day i (h)
!    hrlty - day length on day (i-1)
!    hui - heat unit index (ratio of acthucum to acthum)
!    huiy - heat unit index (ratio of acthucum to acthum) on day (i-1)
!    huirt - heat unit index used to drive root growth (no delays)
!    huirty - heat unit index used to drive root growth (no delays) on day (i-1)
!    hui0 - heat unit index when leaf senescence starts.
!    jda - date of sowing (same as day simulation begins)
!    jd - today's julian date calander
!    phu - potential heat units for crop maturity (deg. c)
!    prcp - cummulated precipitation (mm)
!    prcpb - periodic cummulated precipitation (mm)
!    prcpy - previous value of prcpb (mm)
!    prcps - season cummulated precipitation (mm)
!    r - daily precipitation (mm/day)
!    ra - solar radiation (mj/day)
!    rh - relative humidity (ratio)
!    tmn - manimum air temperature (deg. c)
!    tbas - base temperature (deg. c)
!    tmx - maximum air temperature (deg. c)
!    topt - optimum temperature (deg. c)
!    tplant - minimum soil temperature for planting (c)
!    ts - temperature stress factor
!    vpd - vapor pressure deficit (kpa)
!    xlat - latitude of a location (deg.)
