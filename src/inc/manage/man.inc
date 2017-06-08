!$Author$
!$Date$
!$Revision$
!$HeadURL$
!     ------------------------------------------------------------------
!     file 'man.inc'
!
!     This common block contains the variables used in the MANAGEMENT
!     subprograms.
!
!     Parameter include files required:  p1werm.inc
!
!     + + + VARIABLE DECLARATIONS + + +
!
      integer        mxtbln
      parameter      (mxtbln = 10000)
!      
      character*256  mtbl  (mxtbln)
      integer        mbeg  (mnsub+1)
      integer        mcur  (mnsub)
      integer        mcount(mnsub)
!      real           mversion(mnsub)
!
!      common / man4 / mbeg, mcur, mtbl, mcount, mversion
      common / man4 / mbeg, mcur, mtbl, mcount
!            
! *** the following variables are deleted asap
      integer          mstart(mnsub)
      integer          mcurnt(mnsub)
!      integer          mperod(mnsub)
      character*80     manlin(mnsub)
      character*80     mline
      character        aline (80)*1
      integer          mlpos
      integer          mrecnum
      integer          mrcl(mnsub)
      integer          meor(mnsub)
!
      integer          istack(20)
      real             nstack(20)
      character*20     cstack(40)

      logical          harv_calib_not_selected(mnsub)

!      common / man1 / mstart, mperod, istack, nstack
      common / man1 / mstart, istack, nstack
      common / man2 / manlin, mline, cstack
      common / man3 / mcurnt,  mlpos, mrecnum, mrcl, meor
      common / man5 / harv_calib_not_selected

      save :: /man1/, /man2/, /man3/, /man4/, /man5/

      equivalence (mline, aline)
! *** end delete here      
!
!     + + + VARIABLE DEFINITIONS + + +
!
!     mtbl    - Array by index of character lines which holds the managment
!              table as read from the management file 
!     mbeg    - Array by subregion of indexes into MTBL that points to where
!              management entries for each subregion start
!     mcur    - Array by subregion of indexes into MTBL that points to the
!              management file line that is presently being read for each
!              subregion
!     mcount  - Array by subregion that indicates which pass through the
!              management file is being performed
!
!     mstart  - Holds the record number ("pointer") to the "beginning" 
!              of operations (in the management file) for each subregion.
!
!              range values      min        max
!              ------------   ---------  ---------
!              fixed          1          mnsub
!              typical        1          n_sr
!              ------------   ---------  ---------
!
!     aline  - Dummy variable to avoid compile warning under WATCOM
!
!
!
!     mcurnt  - Holds the record number ("pointer") to the next management
!              operation to be done for each subregion.
!
!              range values      min        max
!              ------------   ---------  ---------
!              fixed          1          mnsub
!              typical        1          n_sr
!              ------------   ---------  ---------
!
!
!     mperod - Holds the number of years for a management cycle
!              (length of crop rotation) for each subregion.
!
!              range values      min        max
!              ------------   ---------  ---------
!              fixed          1          no of sim years
!              typical        1          1-5
!              ------------   ---------  ---------
!
!
!     manlin - Holds the current management date/operation
!              to be done for each subregion on that date.
!
!              range values      min        max
!              ------------   ---------  ---------
!              fixed          -          -
!              typical        -          -
!              ------------   ---------  ---------
!
!
!     istack - Holds a list of integer data values that are
!              retrieved from the management data file.
!
!              range values      min        max
!              ------------   ---------  ---------
!              fixed          -          -
!              typical        -          -
!              ------------   ---------  ---------
!
!
!     nstack - Holds a list of real data values that are
!              retrieved from the management data file.
!
!              range values      min        max
!              ------------   ---------  ---------
!              fixed          -          -
!              typical        -          -
!              ------------   ---------  ---------
!
!
!     cstack - Holds a list of character data values that are
!              retrieved from the management data file.
!
!              range values      min        max
!              ------------   ---------  ---------
!              fixed          -          -
!              typical        -          -
!              ------------   ---------  ---------
!
!
!     mline  - Saves a line from the management file for use by
!              the getr function during and after it's call.
!
!              range values      min        max
!              ------------   ---------  ---------
!              fixed          -          -
!              typical        -          -
!              ------------   ---------  ---------
!
!
!     mlpos  - Saves the internal position within 'mline' for use
!              during and for latter getr function calls.
!
!              range values      min        max
!              ------------   ---------  ---------
!              fixed          -          -
!              typical        -          -
!              ------------   ---------  ---------
!
!
!     mrecnum - Holds the current record number.
!
!              range values      min        max
!              ------------   ---------  ---------
!              fixed          -          -
!              typical        -          -
!              ------------   ---------  ---------

!     mrcl    - Holds the record length of each management file
!
!              range values      min        max
!              ------------   ---------  ---------
!              fixed          -          -
!              typical        -          -
!              ------------   ---------  ---------

!     meor    - Holds the position of last printable ASCII char
!               in each record within the management files
!               (should be 70)
!
!              range values      min        max
!              ------------   ---------  ---------
!              fixed          -          -
!              typical        -          -
!              ------------   ---------  ---------

!     harv_calib_not_selected - flag indicating that a harvest operation
!                               has not been selected for calibration for that crop.

