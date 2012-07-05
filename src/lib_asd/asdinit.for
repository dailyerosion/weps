!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine asdini()

      include 'manage/asd.inc'

!     + + + PURPOSE + + +
!     This subroutine  performs the initialization of the asd/sieve
!     variables which include the number of sieves and their sizes,
!     the geometric mean diameter of each sieve cut and specifies which
!     lognormal case will be used to represent aggregate size distributions
!     in WERM/WEPS.

!     The routine decides which lognormal case to apply based on the
!     value of logcas:

!     logcas = 0 --> "normal" lognormal case (mnot = 0, minf = infinity)
!     logcas = 1 --> "abnormal" lognormal case (mnot != 0, minf = infinity)
!     logcas = 2 --> "abnormal" lognormal case (mnot = 0, minf != infinity)
!     logcas = 3 --> "abnormal" lognormal case (mnot != 0, minf != infinity)

!     + + + KEYWORDS + + +
!     aggregate size distribution, asd, sieves, mass fractions

!     + + + ARGUMENT DECLARATIONS + + +

!     currently none

!     + + + INCLUDED COMMON BLOCK DEFINITIONS + + +
!     nsieve - number of sieves used
!     sdia   - array containing sieve size diameters
!     mdia   - array containing gmd sieve cut diameters
!     mnsize - minimum (imaginary) sieve size to use for computing
!              lower sieve cut geometric mean diameter
!     mxsize - maximum (imaginary) sieve size to use for computing
!              upper sieve cut geometric mean diameter
!     logcas - flag to represent which lognormal case to apply

!     + + + PARAMETERS + + +

!     + + + LOCAL VARIABLES + + +

      integer  i

!     + + + LOCAL VARIABLE DEFINITIONS + + +
     
!     i      - loop variable for sieve diameters

!     + + + END SPECIFICATIONS + + +

      ! NOTE: using this method generates slightly different sieve sizes
      ! between debug and optimized compile switches. (and possibly between
      ! different compilers) To minimize these differences, we should return
      ! to exactly defined sieve sizes

! specificiations brought here from BLKDAT (see revision 1.4 comment below)
!      data logcas / 3 /
!      data nsieve / 13 /
!      data sdia / 0.018, 0.037, 0.075, 0.15, 0.42, 0.84, 2.0,
!     &            6.35, 19.05, 44.45, 76.2, 150.4, 300.8 /
!      data mnsize, mxsize / 0.009, 601.2 /


      logcas = 3
      nsieve = msieve - 1
      mnsize = 0.005
      mxsize = 1000.0

      do i = 1, nsieve
          sdia(i) = exp(log(mnsize)                                     &
     &            + i*(log(mxsize)-log(mnsize))/(nsieve+1))
      end do

!     compute geometric mean dia. for each sieve cut
      mdia(1) = sqrt(mnsize*sdia(1))
      do 5 i = 2, nsieve
           mdia(i) = sqrt(sdia(i)*sdia(i-1))
5     continue
      mdia(nsieve+1) = sqrt(mxsize*sdia(nsieve))

      return
      end

!$Log: not supported by cvs2svn $
!Revision 1.5  2002/09/04 20:18:40  wagner
!allow free format src compilation
!
!Revision 1.4  2002/04/29 16:24:38  fredfox
!Removed sieve size initialization from BLKDAT and placed here so that the sizes can be initialized dymanically based on a maximum and minimum size and number of sieves defined in ASD.INC
!
!Revision 1.3  2002/04/17 20:16:08  fredfox
!modified m2asd to properly handle the upper and lower bounds on the
!4 parameter lognormal distribution by allowing the specification of
!geometric mean bin diameter to use either the sieve above or the
!limit depending on which applies. (same on the lower bin).
!Modified asd2m to handle bin sizes outside the range of of the distribution.
!asdini.for changes were cosmetic only
!
!Revision 1.2  1999/03/16 23:55:41  wjr
!*** empty log message ***
!
! Revision 1.2  1995/09/13  15:49:34  wagner
! Necessary changes made to allow FORTRAN src files (*.for) to use the
! extended FORTRAN include statement rather than the MICROSOFT $INCLUDE
! directive as previously used.  This is required to allow use of other
! FORTRAN compilers.
!
! Changes have been made to the prologue.mk, epilogue.mk, and the Unix
! master startup.mk files as well as the src files.
!
! Revision 1.1.1.1  1995/01/18  04:19:56  wagner
! Initial checkin
!
! Revision 1.2  1992/10/10  21:44:14  wagner
! Changed names appropriate for submodel name change
! from TILLAGE to MANAGEMENT.
!
! Revision 1.1  1992/04/16  21:41:37  wagner
! Initial revision
!
