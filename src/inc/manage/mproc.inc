!$Author$
!$Date$
!$Revision$
!$HeadURL$

!     This common block contains the variables used in the MANAGEMENT
!     process subprograms.

!     Parameter include files required:  p1werm.inc

!     + + + VARIABLE DECLARATIONS + + +
      integer          prcode
      character*30     prname

      real             ospeed, odir, ostdspeed, ominspeed, omaxspeed
      real             tdepth, ti, tstddepth, tmindepth, tmaxdepth
      integer          tlayer, rdgflag
      real             imprs

      real             fracarea
      integer          bioflg

      integer          cropno
      character*80     cropname

      character*80     amdname

      common / till / ospeed, odir, ostdspeed, ominspeed, omaxspeed,    &
     &                tdepth, tlayer, ti,                               &
     &                tstddepth, tmindepth, tmaxdepth,                  &
     &                rdgflag, imprs
      common / bio / bioflg, fracarea

      common / mproc / cropno, cropname

      common / mproc1 / prcode, prname

      common / mproc2 / amdname

      save :: /till/, /bio/, /mproc/, /mproc1/, /mproc2/

!     + + + VARIABLE DEFINITIONS + + +
!     cropno - number that identifies the crop to be grown
!     fracarea - fraction of area tilled (not implement but field?) (m*m)
!     tdepth - tillage depth (mm)
!     rdgflag - tillage ridge flag
!     imprs  - implement ridge spacing (can be used to set row spacing)
!     ospeed - operation speed (m/s)
!     odir - operation direction (degrees from NORTH)
