!$Author$
!$Date$
!$Revision$
!$HeadURL$

!     This block common is contains local variables used for a
!     simulation run for MAIN.

!     Requires parameter include file:  p1werm.inc

!     + + + VARIABLE DECLARATIONS + + +

      character        clifil*512
      character        dinfil*512
      character        hinfil*512
      character        runfil*512
      character        simout*512
      character        sinfil*512
      character        subfil*512
      character        tinfil*512
      character        usrid*80
      character        usrloc*80
      character        usrnam*80
      character        winfil*512
      character        series(mnsub)*80
!      character        wsum*80

      integer          daysim
      integer          erosrpt
      integer          gnrpt(8)
      integer          id
      integer          im
      integer          iy
      integer          ijday
      integer          ld
      integer          lm
      integer          ly
      integer          lopday
      integer          lopmon
      integer          lopyr
      integer          ljday
      integer          maxper
      integer          outcnt
      integer          tyears

      logical          subflg

      common / simday /                                                 &
     &                  clifil, dinfil, hinfil, runfil, simout, sinfil, &
     &                  subfil, tinfil, usrid, usrloc, usrnam, winfil,  &
     &                  series,                                         &

     &                  daysim, erosrpt, gnrpt,                         &
     &                  id, im, iy, ijday,                              &
     &                  ld, lm, ly, ljday,                              &
     &                  lopday, lopmon, lopyr,                          &
     &                  maxper,                                         &
     &                  outcnt, tyears,                                 &

     &                  subflg

      save :: /simday/

!     + + + VARIABLE DEFINITIONS + + +

!    clifil  - This variable holds the CLIGEN input file name.

!    daysim  - This variable contains the curret day number of
!              the simulation run.

!              range values      min       max
!              ------------   ---------  ---------
!              fixed              1         -
!              typical            1         -
!              ------------   ---------  ---------
!     dinfil - This variable holds the decomp input file name.

!    erosrpt - This varaible array holds flags for printing period.

!     gnrpt  - This varaible array holds flags for printing
!              general output forms where 1 = print output and
!              0 = print no output.

!     hinfil - This variable holds the hydrology input file name.

!   id,im,iy - These variables contain the values for the initial
!              day, month, and year of the simulation run.

!     ijday  - This variable contains the initial julian day of
!              the simulation run.

!   ld,lm,ly - The last day, month, and year of simulation.

!     ljday  - This variable contains the last julian day of
!              the simulation run.

!     lopday - The day, month, and year of the last operation.
!     lopmon
!     lopyr

!    maxper  - The maximum number of years in a rotation cycle
!              of all subregions.
!              All subregion rotation cycle period lengths (in years)
!              must be a factor in this value.  For example, 3 subregions
!              with individual rotation periods of 2, 3, and 4 years
!              each would have a "maxper" value of 12 years.  Note that
!              each of the individual subregion rotation periods can
!              divide evenly into the "maxper" value.

!     outcnt - Counter that holds number of times output is
!              written to the temporary file holding accounting region
!              erosion values.  With this, weps can calculate the number
!              of output periods.

!     runfil - This variable holds the simulation run input file name.

!     series - This character variable contains the soil series name
!              for each subregion.

!     simout - This variable holds the simulation output file name.

!     sinfil - This variable holds the soil input file name.

!     subfil - This variable holds the subdaily wind information
!              ('real data') file name.

!     subflg - This logical variable is used to read header information
!              in the subdaily wind file (if .true., read header).

!     tyears - The total number of years in the simulation run.

!     usrid  - This character variable is an identification string
!              to aid the user in identifying the simulation run.

!     usrloc - This character variable holds a location
!               description of the simulation site.

!     usrnam - This character variable holds the user name.

!     winfil - This variable holds the WINDGEN input file name.
!-----------------------------------------------------------------------
