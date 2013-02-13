!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine print(slplen,avgslp,runoff,peakro,effdrn,efflen,       &
     &    effint,effdrr)
     
      use wepp_interface_defs
!
!     + + + purpose + + +
!     open the standalone output file.
!     print out the file header and the
!     input hydrology and slope parameters
!
!     called from subroutine main
!     author(s): d. flanagan
!
!     version: this module based on wepp v2004.7 print.for
!     date coded:  9-30-2004
!     coded by: d. flanagan
!
!     + + + argument declarations + + +
!
      real, intent(in) :: slplen, avgslp, runoff, peakro, effdrn,       &
     &     efflen, effint, effdrr
!
!     + + + argument definitions + + +
!
!     slplen - slope length of current ofe (m)
!     avgslp - average slope of a line passing through endpoints
!              of the current ofe (m/m)
!     runoff - runoff depth (m)
!     peakro - peak runoff rate (m/s)
!     effdrn - effective duration of runoff (s)
!     efflen - effective length of runoff (m)
!     effint - effective rainfall intensity (m/s)
!     effdrr - effective duration of rainfall (s)
!
!     + + + local variables + + +
!
!     integer ihill
!
!     + + + local definitions + + +
!
!     ihill  - current hillslope profile in a watershed simulation
!
!     + + + end specifications + + +
!
!
!*********************************************************************
!
!     if the first ofe, open the output file and print the 
!     header information.
!
      write (8,1000)
      write (8,1100)
      write (8,*)
!     
!     ihill = 1
!     write (8,1200) ihill
!     write (8,1300)
!     write (8,1400) iplane
      write (8,1500)
      write (8,1600) slplen, avgslp
      write (8,1700) runoff * 1000., peakro * 3.6e6,                    &
     &    effdrn / 60., efflen, effint * 3.6e6, effdrr / 60.
      write (8,1800)
!     
      return
 1000 format (/,' (wepp) erosion stand-alone output ',//)
 1100 format (//'i.   single storm hydrology',/,2x,9('-'),1x,5('-'),1x, &
     &    36('-'))
!1200 format (//2x,18('*'),/4x,'hillslope ',i2,/,2x,18('*'))
!1300 format (//6x,27('*'))
!1400 format (7x,'overland flow element ',i2)
 1500 format (6x,27('*'))
 1600 format (/'  input runoff parameters'/2x,52('-')/                  &
     &    '      plane length               ',f8.2,' (m)'/              &
     &    '      average slope of profile   ',f8.3/)
 1700 format (/'  runoff output'/2x,52('-')/                            &
     &    '      runoff depth               ',f8.2,' (mm)'/             &
     &    '      peak runoff rate           ',f8.2,' (mm/hr)'/          &
     &    '      effective runoff duration  ',f8.2,' (min)'/            &
     &    '      effective length           ',f8.2,' (meters)'/         &
     &    '      effective intensity        ',f8.2,' (mm/hr)'/          &
     &    '      effective rainfall duration',f8.2,' (min)'/)
 1800 format (2x,52('-'))
      end
