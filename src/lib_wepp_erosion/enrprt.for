      subroutine enrprt(jun,npart,frac,frcflw,dia,spg,frsnd,            &
     &    frslt,frcly,frorg,enrato)
     
      use wepp_interface_defs
      
      implicit none
!
!     + + + purpose + + +
!     prints out the sediment size distribution in runoff
!     and the enrichment ratio for all overland flow elements
!     in a profile.
!
!     called from subroutine sedout
!     author(s): d. flanagan, j. ascough
!
!     version: this module taken largely from wepp v2004.7 code
!     date coded: 10-2004
!     coded by: d. flanagan
!
!     + + + parameter declarations + + +
!
      integer mxpart
      parameter (mxpart = 10)
!       
!     + + + argument declarations + + +
!
      integer, intent(in) :: jun, npart
      real, intent(in) ::  frac(mxpart), frcflw(mxpart), dia(mxpart),   & 
     &      spg(mxpart),                                                &
     &     frsnd(mxpart), frslt(mxpart), frcly(mxpart), frorg(mxpart),  &
     &     enrato
!
!
!     + + + argument definitions + + +
!
!     jun  - unit number
!     npart  - number of particle size classes
!     frac   - fraction of sediment in size class at point of detachment
!     frcflw - fraction of sediment in size class in flow
!     dia    - diameter of sediment particle size class (m)
!     spg    - specific gravity of sediment particle size class
!     frsnd  - fraction of sand in size class
!     frslt  - fraction of silt in size class
!     frcly  - fraction of clay in size class
!     enrato - enrichment ratio of the specific surface area
!
!     + + + local variables + + +
!
      real fsand(mxpart), fsilt(mxpart), fclay(mxpart), forg(mxpart),   &
     &     diam(mxpart)
      integer i
!
!     + + + local definitions + + +
!     diam  - diameter of particle class i in mm
!     fsand - percent of sand in particle class i
!     fsilt - percent of silt in particle class i
!     fclay - percent of clay in particle class i
!     forg  - percent of organic matter in class i
!     i     - counter variable used to indicate particle class
!
!     + + + end specifications + + +
!
!
      do 10 i = 1, npart
         diam(i) = dia(i) * 1000.
         fsand(i) = frsnd(i) * 100.
         fsilt(i) = frslt(i) * 100.
         fclay(i) = frcly(i) * 100.
         forg(i) = frorg(i) * 100.
   10 continue
!     
      write (jun,1000)
      do 20 i = 1, npart
         write (jun,1200) i, diam(i), spg(i), fsand(i), fsilt(i),       &
     &       fclay(i), forg(i), frac(i), frcflw(i)
   20 continue
      write (jun,1100)
      write (jun,1300) enrato
      close (jun)
!     
!     
!********************************************************************
!     *
!     format statements                                             *
!     *
!********************************************************************
!     
!     
      return
 1000 format (/5x,'sediment particle information leaving profile',/,    &
     &    '-------------------------------------------------------',    &
     &    '------------------------',/,                                 &
     &    '                                 particle composition',      &
     &    '         detached fraction',/,'class  diameter  specific  ---&
     &------------------------------','  sediment  in flow',/,9x,       &
     &    '(mm)    gravity   % sand   % silt   % clay   % o.m.',        &
     &    '  fraction  exiting',/,                                      &
     &    '-------------------------------------------------------',    &
     &    '------------------------')                
 1100 format ('---------------------------------------------------',    &
     &    '----------------------------'/)
 1200 format (1x,i2,4x,f6.3,6x,f4.2,4x,f5.1,4x,f5.1,4x,f5.1,4x,f5.1,5x, &
     &    f5.3,4x,f5.3)
 1300 format (/5x,'ssa enrichment ratio leaving profile = ',f6.2)
      end
