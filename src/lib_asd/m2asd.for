!$Author$
!$Date$
!$Revision$
!$HeadURL$

! NOTES:
!
!     We will probably want to rewrite what this subroutine does into
!     several smaller routines for both speed reasons and potentially
!     modularity.
!
!     The log(md) that we need could be moved to the
!     the initialization routine and access the log(md) rather than
!     accessing (md) and computing log(md) multiple times (SPEED SAVINGS).
!
!     We also may want to may want to separate out the looping among
!     soil layers so that it can be done at a higher level (may make
!     code more modular - do only one thing extremely well concept).
!     This should be discussed as to whether this would be beneficial
!     in the long run.
!
!     Tue Apr  6 14:15:48 CDT 1999 - LEW
! -----------------------------------------------------------------
!     This routine was simplified and recoded.
!     It now allows for the sieve cut sizes to lie outside the
!     range specified by "mnot" and "minf" by checking for this
!     situation and only using the sieve cuts between "mnot" and "minf"
!     (this only applies to the pertinent modified log-normal cases).
!
!     Note that:
!     a) the sieve cut size array, "mdia" must consist of 2 or more sizes
!        and contain values which increase in size,
!     b) "mnot" must be greater than or equal to zero,
!     c) "mnot" must be less than "minf" (with at least two sieve cut
!        sizes between them),
!     d) and the mass fractions, "mf" cannot be less than zero.

!     These conditions are NOT checked within this code.
!
!     Note also that the return values "gmd" and "gsd" are the
!     geometric mean and geometric standard deviation of the
!     "transformed" parameters, based upon the specific "logcas"
!     used and NOT always the geometric mean and standard deviation
!     of the aggregate sizes.
! -----------------------------------------------------------------

      subroutine m2asd (mf, nlay, mnot, minf, gmd, gsd)

      include 'p1werm.inc'
      include 'manage/asd.inc'

!     + + + PURPOSE + + +
!     This subroutine  performs the inverse of subroutine asd2m.
!     m2asd computes the geometric mean & standard deviation for the
!     lognormal representation of the soil aggregate size distribution
!     from mf(i,j).

!     The routine decides which lognormal case to apply based on the
!     value of logcas:
!
!     logcas = 0 --> "normal" lognormal case (mnot = 0, minf = infinity)
!     logcas = 1 --> "abnormal" lognormal case (mnot != 0, minf = infinity)
!     logcas = 2 --> "abnormal" lognormal case (mnot = 0, minf != infinity)
!     logcas = 3 --> "abnormal" lognormal case (mnot != 0, minf != infinity)
!
!     + + + KEYWORDS + + +
!     aggregate size distribution, asd, sieves, mass fractions
!
!     + + + ARGUMENT DECLARATIONS + + +
      real    mf(msieve+1, mnsz)
      integer nlay
      real    mnot(mnsz), minf(mnsz)
      real    gmd(mnsz), gsd(mnsz)
!
!
!     + + + ARGUMENT DEFINITIONS + + +
!     mf     - mass fractions of aggregates within sieve cuts
!              (sum of all mass fractions are expected to = 1.0)
!     nlay   - number of soil layers used
!     mnot   - minimum size aggregate (assumed value is known)
!     minf   - maximum size aggregate (assumed value is known)
!     gmd    - geometric mean diameter of aggregate size distribution
!              (or transformed asd for "modified" lognormal cases)
!     gsd    - geometric standard deviation of aggregate size distribution
!              (or transformed asd for "modified" lognormal cases)
!
!     + + + ACCESSED COMMON BLOCK VARIABLE DEFINITIONS + + +
!
!     nsieve - number of sieves used
!     mdia   - geometric mean dia. for each sieve cut
!     mnsize - minimum (imaginary) sieve size to use for computing
!              lower sieve cut geometric mean diameter
!     mxsize - maximum (imaginary) sieve size to use for computing
!              upper sieve cut geometric mean diameter
!     logcas - flag to represent which lognormal case to apply
!
!     + + + PARAMETERS + + +
!
!     + + + LOCAL VARIABLES + + +
!
      real     tmd(msieve+1)
      real     alpha, beta
      real     mdia_istart, mdia_istop, sdia_temp
      integer  i, j

      integer  istart, istop
!
!     + + + LOCAL VARIABLE DEFINITIONS + + +
!
!     tmd    - transformed md (later log(tmd))
!     alpha  - internal summation variable
!     beta   - internal summation variable
!     i      - loop variable for sieve diameters
!     istart - loop start variable for sieve diameters
!     istop  - loop stop variable for sieve diameters
!     j      - loop variable for soil layers
!
!     + + + END SPECIFICATIONS + + +
!
!     for each soil layer
      do 200 j=1,nlay
!          initialize accumulators
           alpha = 0.0
           beta = 0.0
           istart = 1
           istop = nsieve + 1

!          check if sieve cut fractions are between mnot and minf
!          adjust lower and upper mean diameters if mnot or minf
!          fall within the bin range
           if (logcas .eq. 1 .or. logcas .eq. 3) then
              do i=nsieve, 1, -1
                 if (sdia(i) .gt. mnot(j)) then
                    istart = i
                 end if
              end do
!             save value to be restored before exit
              mdia_istart = mdia(istart)
!             set size of lower sieve in bottom bin
              if( istart.eq.1 ) then
                  sdia_temp = mnsize
              else
                  sdia_temp = sdia(istart-1)
              end if
!             check if mnot falls within lower sieve bin
              if( (mnot(j).gt.sdia_temp).or.(mnot(j).lt.mnsize) ) then
!                 recalculate lower bin mean diameter
                  mdia(istart) = sqrt(sdia(istart)*mnot(j))
                  if (logcas .eq. 3) then
                      ! check that mdia is greater than mnot, or method fails
                      if( mdia(istart) .lt. mnot(j) * 1.00001 ) then
                          mdia(istart) = mnot(j) * 1.00001
                      end if
                  end if
              end if
           endif
           if (logcas .ge. 2) then
              do i=1, nsieve
                 if (sdia(i) .le. minf(j)) then
                    istop = i+1
                 end if
              end do
!             set size of upper sieve in top bin
              if( istop.eq.nsieve+1 ) then
                  sdia_temp = mxsize
              else
                  sdia_temp = sdia(istop)
              end if
!             save value to be restored before exit
              mdia_istop = mdia(istop)
!             check if minf falls within upper sieve bin or outside mxsize
              if( (minf(j).lt.sdia_temp).or.(minf(j).gt.mxsize) ) then
!                 recalculate upper bin mean diameter
                  mdia(istop) = sqrt(sdia(istop-1)*minf(j))
                  if (logcas .ge. 2) then
                      ! check that mdia is less than minf, or method fails
                      if( mdia(istop) .gt. minf(j) * 0.99999 ) then
                          mdia(istop) = minf(j) * 0.99999
                      end if
                  end if
              end if
           else
              istop = nsieve + 1
           end if

!     do transformations for "modified" log-normal cases
           do i= istart, istop
              if (logcas .eq. 3) then
                 tmd(i) = (mdia(i)-mnot(j))*(minf(j)-mnot(j))/          &
     &                    (minf(j)-mdia(i))
              else if (logcas .eq. 2) then
                 tmd(i) = mdia(i)*minf(j)/(minf(j)-mdia(i))
              else if (logcas .eq. 1) then
                 tmd(i) = mdia(i)-mnot(j)
              else
                 tmd(i) = mdia(i)
              end if

!             now compute the log of the gmd dia
              tmd(i) = log(tmd(i))

!             sum diameters  & their squares, over all aggregate sizes
              alpha = alpha + (mf(i,j)*tmd(i))
              beta = beta + (mf(i,j)*tmd(i)*tmd(i))
           end do

!          compute geometric mean and standard deviation
           gmd(j) = exp(alpha)
           if( beta-alpha*alpha.le.0.0 ) then
               gsd(j) = mingsd
           else 
               gsd(j) = max(mingsd,exp(sqrt(beta-alpha*alpha)))
           end if

!          restore modified geometric mean bin diameters
           if (logcas .eq. 1 .or. logcas .eq. 3) then
               mdia(istart) = mdia_istart
           end if
           if (logcas .ge. 2) then
               mdia(istop) = mdia_istop
           end if

200   continue
      return
      end
!
!$Log: not supported by cvs2svn $
!Revision 1.13  2002/09/06 18:55:44  fredfox
!removed declaration statements for unused variables
!
!Revision 1.12  2002/09/04 20:18:40  wagner
!allow free format src compilation
!
!Revision 1.11  2002/04/29 16:21:48  fredfox
!Added use of parameter to restrain the minimum value of GSD. Worked over array indexing when maximum size is greater or minimum size is lesser than sieves
!
!Revision 1.9  2001/09/27 20:36:52  fredfox
!merged in update hydro branch
!
!Revision 1.8.2.1  2001/08/15 22:12:33  fredfox
!Corrected index for zeroing out unused array elements, added headers, edited out debug write statements.
!
!Revision 1.8  2000/09/08 23:54:11  fredfox
!fixed test for valid MNOT for correct case and loop direction
!
!Revision 1.7  2000/09/08 22:52:04  wagner
!Added check for single 365 day year in cligen file and added code to keep out of an infinite loop when WEPS thinks a simulation year should have a leap day in it
!
!Revision 1.6  2000/09/08 15:07:20  fredfox
!corrected alpha,beta, i, j declaration statements
!
!Revision 1.5  2000/09/07 20:25:36  jt
!added file name to print alpha and beta.
!
!Revision 1.4  1999/04/22 19:02:30  wjr
!debugging write line added
!
!Revision 1.3  1999/04/06 19:40:29  wagner
!Simplified and recoded this routine.
!It now checks for and allows sieve fractions to be outside
!the range from "mnot" to "minf" for the "modified" log-normal
!cases.
!
!Revision 1.1.1.1  1999/03/12 17:05:17  wagner
!Baseline version of WEPS with Bill Rust's modifications
!
! Revision 1.2  1995/09/13  15:49:44  wagner
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
! Revision 1.4  1994/09/01  22:18:54  jt
! checking for floating point errors? - LEW
!
! Revision 1.3  1992/10/10  21:44:14  wagner
! Changed names appropriate for submodel name change
! from TILLAGE to MANAGEMENT.
!
! Revision 1.2  1992/04/16  21:41:37  wagner
! Uses common memory now.
!
! Revision 1.1  1992/04/16  13:29:01  wagner
! Initial revision
!
