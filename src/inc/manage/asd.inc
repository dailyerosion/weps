!$Author$
!$Date$
!$Revision$
!$HeadURL$
!     ------------------------------------------------------------------
!     file 'asd.inc'
!
!     This common block contains the variables used in the Tillage
!     subprograms that deal with sieve cuts, sieve sizes etc.
!
!     Parameter include files required: none
!     (parameters currently included in this file)
!
!     + + + VARIABLE DECLARATIONS + + +
!
      integer          msieve
      real             mingsd

      parameter (msieve = 26)
      parameter (mingsd = 2.0)

      real             sdia(msieve)
      real             mnsize
      real             mxsize
      real             mdia(msieve+1)
      integer          nsieve
      integer          logcas

!
      common / sieves / sdia, mnsize, mxsize, mdia, nsieve, logcas

      save :: /sieves/
!
!     + + + VARIABLE DEFINITIONS + + +
!
!     sdia   - Array holding the sieve size diameters
!              used to compute the sieve cuts.
!              Units are in mm.
!
!              range values      min        max
!              ------------   ---------  ---------
!              fixed          > 0.0      -
!              typical        0.01       100.0
!              ------------   ---------  ---------
!
!     mnsize - Minimum (imaginary) sieve size to use
!              for computing lower sieve cut
!              geometric mean diameter.
!              Units are in mm.
!
!              range values      min        max
!              ------------   ---------  ---------
!              fixed          0.0        < sdia(1)
!              typical        0.1        < sdia(1)
!              ------------   ---------  ---------
!
!     mxsize - Maximum (imaginary) sieve size to use
!              for computing upper sieve cut
!              geometric mean diameter.
!              Units are in mm.
!
!              range values      min        max
!              ------------   ---------  ---------
!              fixed            > sdia(nsieve)
!              typical          > sdia(nsieve)
!              ------------   ---------  ---------
!
!     mdia   - Array holding the geometric mean
!              diameters of the sieve cuts.
!              Units are in mm.
!
!              range values      min        max
!              ------------   ---------  ---------
!              fixed          < sdia(1)  sdia(nsieve)
!              typical        < sdia(1)  sdia(nsieve)
!              ------------   ---------  ---------
!
!     msieve - Maximum number of sieves that can be used.
!              (Compile time array size limit)
!
!              range values      min        max
!              ------------   ---------  ---------
!              fixed          1          -
!              typical        4          20
!              ------------   ---------  ---------
!
!     nsieve - Number of sieves actually used.
!
!              range values      min        max
!              ------------   ---------  ---------
!              fixed          1          msieve
!              typical        4          msieve
!              ------------   ---------  ---------
!
!     logcas - Specifies which lognormal case to apply.
!
!     logcas = 0 --> "normal" lognormal case (mnot = 0, minf = infinity)
!     logcas = 1 --> "abnormal" lognormal case (mnot != 0, minf = infinity)
!     logcas = 2 --> "abnormal" lognormal case (mnot = 0, minf != infinity)
!     logcas = 3 --> "abnormal" lognormal case (mnot != 0, minf != infinity)
!
!              range values      min        max
!              ------------   ---------  ---------
!              fixed          0          3
!              typical        3          3
!              ------------   ---------  ---------
!
!     mingsd - Specifies the minimum GSD value that will be used when creating
!              a bin and the minimum GSD that will be returned when conversting
!              from bins to modified log normal distribution
!
!              range values      min        max
!              ------------   ---------  ---------
!              fixed          1          e(2.7....)
!              typical        1.5        2.5
!              ------------   ---------  ---------
