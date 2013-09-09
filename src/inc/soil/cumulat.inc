!$Author$
!$Date$
!$Revision$
!$HeadURL$
!     defines for cumulative values retained from a previous event
      real cump

      common / cumulat /                                                &
     & cump(mnsub)

      save :: /cumulat/

!   cumulative precipitation since last event
