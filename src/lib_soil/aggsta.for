
!$Author$
!$Date$
!$Revision$
!$HeadURL$
      subroutine aggsta(daysim,                                         &
     &  cseags, cseagmn, cseagmx,                                       &
     &  cbhrwc0, cbhrwc, cbhrwcdmx,                                     &
     &  chrwcw, chrwca,chrwcs,                                          &
     &  chtmx0, chtsmn, chtsmx, ck4d,                                   &
     &  se0, se1, trigger,                                              &
     &  k4f, k4fs, k4fd, k4td, k4w, k4d)

!     + + + ARGUMENT DECLARATIONS + + +
      integer daysim
      real cseags, cseagmn, cseagmx
      real  cbhrwc0, cbhrwc, cbhrwcdmx
      real  chrwcw, chrwca,chrwcs
      real  chtmx0, chtsmn, chtsmx, ck4d
      real  se0, se1
      integer trigger
      real k4f, k4fs, k4fd, k4td, k4w, k4d

!     + + + LOCAL VARIABLES + + +
      real se, minse, maxse, hrwc0, hrwc1, hrwcdmx
      parameter (minse = 0.01)
      parameter (maxse = 1.0)
      real se00

!     + + + GLOBAL INCLUDES + + +
      include 'precision.inc' !declaration for portable math range checking
      include 'command.inc'   ! command line argument for new puddling option

!     + + + LOCAL DEFINITIONS + + +
!     se        - relative aggregate stability with partial update
!     minse     - minimum value allowed for se
!     maxse     - maximum value allowed for se
!     hrwc0     - relative water content on prior day of each layer
!     hrwc1     - relative water content on current day of each layer
!     hrwcdmx   - maximum relative water content on current day of each layer

!     AGGREGATE STABILITY SECTION:

!     relative agg stability for prior day
      se0 = (cseags - cseagmn)/(cseagmx - cseagmn)
      se00 = se0   !preserve relative agg stability from prior day
!tmp ####
!      write (*,*) 'k4w=',k4w, 'kfd=', k4d
!      write (*,*)'se0=',se0,'cseags=',cseags,'csagmx=',cseagmx
!      write (*,*)'se1=',se1
!     relative water content for prior day
      hrwc0 = (cbhrwc0 - chrwcw)/(chrwcs-chrwcw)
      if (hrwc0.lt.0.0) hrwc0 = 0.0
      if (hrwc0.gt.1.0) hrwc0 = 1.0
!     relative water content for current day
      hrwc1 = (cbhrwc - chrwcw) / (chrwcs-chrwcw)
      if (hrwc1 .lt. 0.0) hrwc1 = 0.0
      if (hrwc1 .gt. 1.0) hrwc1 = 1.0
!     daily maximum relative water content for current day
      hrwcdmx = (cbhrwcdmx - chrwcw) / (chrwcs - chrwcw)
      if (hrwcdmx .lt. 0.0) hrwcdmx = 0.0

!     check for two days unfrozen
      if((chtmx0.gt.0.0).and.(chtsmn.gt.0.0))then                            
         go to 70
      else
!        check for two dayscontinuous frozen         
         if ((chtmx0.lt.0.0) .and. (chtsmx .lt.0.0)) then
!          Trap for wrong initial unfrozen stability when frozen
           if (daysim .eq. 2) then
           if ( se0 .lt. (k4fd*k4f*hrwc0+0.5)) then   !freeze
!          Freeze process with prior day water content
            se = se0*(1.0001-k4w*k4f*hrwc0)/(1.0001-k4w*hrwc0)
            se = max(0.0,se)      !set lower limit
            se0 = se + k4fs*k4f*hrwc0 + 0.5 
           endif 
           endif

!          check for frozen drying or wetting
!           assumes water migration to frozen area(k4f term)
           if (hrwc1 .lt. hrwc0) then
             trigger = ibset (trigger, 5)  !frozen drying
!            frozen drying
             se1 = se0 + k4fd*k4f*(hrwc1-hrwc0)
             se1 = max(se1,0.0)
           elseif (hrwc1 .gt. hrwc0) then
             trigger = ibset(trigger, 4)   !frozen wetting
!            frozen wetting
             se1 = se0 + k4fs*k4f*(hrwc1-hrwc0) 
           else
             se1 = se0                     !no change 
           endif
!###tmp
!      write (*,*) 'frozen solid'
!       write (*,*)'hrwc0=',hrwc0,'hrwc1=',hrwc1,'se0=',se0,'se1=',se1
!      write (*,*)'k4fd=',k4fd,'k4f=',k4f,'k4fs=',k4fs
         go to 80
         endif

!     check for freeze/thaw
       if((chtmx0 .gt. 0.0).and.(chtsmn.lt.0.0)                         &
     &                         .and.(chtsmx.gt.0.0)) then 
          trigger = ibset(trigger, 1)  !freeze_thaw
!         Freeze process with prior day water content
          se = se0*(1.0001-k4w*k4f*hrwc0)/(1.0001-k4w*hrwc0)
          se = max(0.0,se)      !set lower limit
          se0 = se + k4fs*k4f*hrwc0 + 0.5
      endif 

!     Check for thaw process
       if(((chtsmn .lt. 0.0) .and. (chtsmx .gt. 0.0)) .or.              &
     &    ((chtmx0 .lt. 0.0) .and. (chtsmx .gt. 0.0 ))) then
           trigger = ibset (trigger, 2)     !thaw
!       thaw process with prior day water content
           if (hrwc0*k4f .gt. 1.0) then       !soil puddling 
             se0 = max(minse,0.999 - k4td*hrwc0)
           else
             se = se0 - k4fs*k4f*hrwc0 - 0.5  !thaw
             se = max(se, 0.0)
             se0 = se + k4td*hrwc0*(k4f-1)    !shrink
           endif
        endif
       endif
     
!    check for unfrozen drying or wetting 
   70  If (hrwc1 .lt. hrwc0) then
        trigger = ibset(trigger, 5)  !drying
!       drying process
        se1 = se0 + k4d*(hrwc0-hrwc1)
      else
         trigger = ibset(trigger, 4) !wetting
!        wetting process
         se1 = se0*(1.0001 - k4w*hrwc1)/(1.0001-k4w*hrwc0)
      endif
     

!    check for freeze process after wet/dry
       if( chtmx0 .gt. 0.0 .and. chtsmx .lt. 0) then
         trigger = ibset(trigger, 0)   !freeze
!        freeze process today
         se = se1*(1.0001-k4w*k4f*hrwc1)/(1.0001-k4w*hrwc1)
         se = max(0.0,se)        !set lower limit
         se1 = se + k4fs*k4f*hrwc1 + 0.5
!tmp ####
!         write (*,*) 'fz after w/d se=',se,'se1=',se1
!         write (*,*) 'chtmx0=',chtmx0,'chtsmx=',chtsmx
!         write (*,*) 'hrwc1=',hrwc1,'k4f=',k4f,'k4fs=',k4fs
       endif

                                   
   80 if (se1.lt.minse) then
         se1 = minse
      endif
      
!     size limits based on frozen status
      if( chtsmx .gt. 0.0 ) then
         ! if not frozen, don't allow over max
         se1 = min( se1, maxse )
      elseif (chtsmx .le. 0.0) then
         ! if frozen, allow greater stability but limit to prevent
         ! out of range asd calculation 
         se1 = min(se1, 10.0)
      endif

!     calc. new agg. stability
!     set resulting aggregate stability based on range limited se1
      cseags = se1*(cseagmx - cseagmn) + cseagmn

!    
!        (may want to use today values for asd ex.after freeze)
!     set se0 and se1 at wilting point for pass to asd subroutine
      se0 = se00 !use relative agg stability for prior day
        if (chtmx0 .gt. 0.0) then !not frozen soil prior day
          se0 = se0 + k4d*hrwc0  !dried
          se0 = min(maxse, se0)  !set upper limit
        endif
!
        if (chtsmx .gt. 0.0 ) then !not frozen today
          se1 = se1 + k4d*hrwc1  !dried
          se1 = min(maxse,se1)   !set upper limit
        endif
!
!      if (chtsmx .gt. 0.0) then !not frozen soil
!        if (ck4d .lt. k4w) then
!          slpd = (ck4d - 0.4*k4w)/0.6
!          intd = ck4d - slpd
!          se1 = (se1 + intd*hrwc1)/(1 - slpd*hrwc1) !dried
!        else
!          se1 = se1 + k4d*hrwc1 !dried
!        endif
!        se1 = min(maxse, se1) !set upper limit
!      endif

!     Can't have a negative value - yet in some cases we get them
!     So, we've put the following checks in here to trap them
!     The question is when the invalid (negative) values occur,
!     should they be set to the minimum or the maximum boundary
!     condition?  For now they are set to the minimum value.
      se0 = max(minse, se0) !set lower limit
      se1 = max(minse, se1) !set lower limit
      return      
      end
