!$Author$
!$Date$
!$Revision$
!$HeadURL$

!**********************************************************************
!     MAIN for TSTINTERNODE
!**********************************************************************

!     +++  PURPOSE +++

!     Test code for internode_wt_bc code to compare with document

      program tstinternode

!     +++ FUNCTIONS CALLED+++
      real internode_wt_bc
     
!     ++++ LOCAL VARIABLES +++
      integer idx
      real cond_up, cond_low, ksat
      real lambda, tlay1, tlay2, airentry, kup, klow, kref, tref
      real weight
      
!     +++ END SPECIFICATIONS +++

      ksat = 1.0
      lambda = 1.0
      tref = 0.02
      airentry = -1.0e-1

      ! loop through values of K logarithmic
      ksat = 2.06490786E-005
      kup = ksat
      klow = ksat * 1.0e-10
      lambda = 3.57637256E-001
      airentry = -7.96725526E-002

!      kref = 1.0e-4
      idx = 1
      do while( idx .le. 40 )
!          kup = kref / (10.0**idx)
!          klow = kref
          tlay1 = tref + tref * idx
          tlay2 = tref + tref * idx
          weight = internode_wt_bc(kup, klow,                           &
     &             ksat, ksat, lambda, lambda,                          &
     &             tlay1, tlay2, airentry, airentry )

!          write(*,*) 'Kup, Klow, weight', kup, klow, weight
          idx = idx + 1
      end do

      end
