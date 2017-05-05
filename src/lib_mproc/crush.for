!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine crush (alpha, beta,nlay,mf)

      use weps_interface_defs, ignore_me=>crush

      include 'p1werm.inc'
      include 'manage/asd.inc'

!     + + + PURPOSE + + +
!     This subroutine  performs the crushing or breaking down of
!     soil aggregates into smaller sizes based on the initial aggregate
!     size distribution and two crushing parameters (alpha and beta).
!     The crushing parameters are assumed to be a function of the
!     soil intrinsic properties, soil water content, and tillage implement.
!     
!     + + + KEYWORDS + + +
!     aggregate size distribution, asd, sieves, mass fractions
!
!     + + + ARGUMENT DECLARATIONS + + +
      real    alpha, beta
      integer nlay
      real    mf(msieve+1,mnsz)
!
!
!     + + + ARGUMENT DEFINITIONS + + +
!
!     alpha  - Aggregate Size Distribution Factor
!     beta   - Crushing Intensity Factor
!     nlay   - number of soil layers used
!     mf     - mass fractions of aggregates within sieve cuts
!              (sum of all mass fractions are expected to = 1.0)
!
!     + + + ACCESSED COMMON BLOCK VARIABLE DEFINITIONS + + +
!
!     mdia   - array containing geometric mean diameters of sieve cuts
!     nsieve - number of sieves used
!
!     + + + PARAMETERS + + +
!
!     + + + LOCAL VARIABLES + + +
!
      real     pmat(msieve+1,msieve+1)
      real     dratio
      real     prob
      real     chk
      integer  i, j, k, m
      real     predmf(msieve+1)
!
!     + + + LOCAL VARIABLE DEFINITIONS + + +
!     
!     pmat   - probability matrix
!     dratio - ratio of sieve cut d to maximum sieve cut d
!     prob   - probability value
!     chk    - variable to chk prob matrix integrity
!     i      - loop variable for sieve cut sizes
!     j      - loop variable for soil layers
!     k      - loop variable for sieve cut probabilities
!     predmf - local array to hold predicted mass fractions
!              before updating mf
!
!     + + + FUNCTIONS CALLED + + +

      real     bino

!     + + + END SPECIFICATIONS + + +

      write(*,*) 'CRUSH: ', alpha, beta, nlay

!
!     for each soil layer
      do 500 j=1,nlay
!         compute transition matrix
          do 100 i=1,nsieve+1
              dratio = mdia(i)/mdia(nsieve+1)
              prob = 1.0 - exp(-alpha+dratio*beta)
              chk = 0.0
              do 50 k=1,i
                  pmat(i,k) = bino(i-1,k-1,prob)

                  write(*,*) 'I K: ', i, k, prob
                  write(*,*) 'PMAT: ', pmat(i,k)
                  write(*,*) 'CHK: ', chk

                  chk = chk+pmat(i,k)
 50           continue
              if (abs(chk-1.0) .gt. 0.001) then
                  write(0,*) 'Problem transition matrix (crush) chk:',  &
     &                    (chk-1.0)
!                 debug code to print out transition matrix
                  do 2 k=nsieve+1,1,-1
                      print*,(pmat(k,m), m=k,1,-1)
2                 continue
                  call exit (1)
              endif
100       continue
          do 300 i=1,nsieve+1
              predmf(i) = 0.0
              do 200 k=i,nsieve+1
                  predmf(i) = predmf(i) + mf(k,j) * pmat(k,i)
200           continue
300       continue
!         put predicted mass fractions into mf
          do 400 i=1,nsieve+1
              mf(i,j) = predmf(i)
400       continue
500   continue
      return
      end
!
!$Log: not supported by cvs2svn $
!Revision 1.6  2003/05/29 22:19:04  wagner
!Changed all "stop" statements to subroutine "call(1)" statements
!to set the WEPS program return to non-zero values so that the
!WEPS 1.0 interface knows that WEPS did not run to completion
!normally.
!
!NOTE:  The subroutine "call()" statement is evidently not a
!Fortran 95 standard.  However, it is available on the Lahey
!and the Solaris Fortran 95 compilers, so we are going to use it.
!
!Revision 1.5  2002/09/04 20:22:00  wagner
!allow free format src compilation
!
!Revision 1.4  2002/04/17 21:01:56  fredfox
!removed obsolete statements and header for reporting version
!
!Revision 1.3  2000/09/29 19:36:44  fredfox
!moved function declaration to correct section of code comments
!
!Revision 1.2  2000/06/14 16:55:59  fredfox
!Added new functionality remove.for
!Added dubug statementments to crush.for
!Added limit to soil typ adjustment in rough.for
!
!Revision 1.1.1.1  1999/03/12 17:05:29  wagner
!Baseline version of WEPS with Bill Rust's modifications
!
! Revision 1.2  1995/09/13  15:49:36  wagner
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
! Revision 1.7  1992/10/13  07:56:55  wagner
! removed debug code (commented it out)
!
! Revision 1.6  1992/10/10  21:44:14  wagner
! Changed names appropriate for submodel name change
! from TILLAGE to MANAGEMENT.
!
! Revision 1.5  1992/06/01  14:59:41  dudley
! *** empty log message ***
!
! Revision 1.4  1992/04/29  15:07:01  wagner
! *** empty log message ***
!
! Revision 1.3  1992/04/17  14:56:55  wagner
! Removed hardcoded md values (dj's).
!
! Revision 1.2  1992/04/16  21:41:37  wagner
! Uses common memory now.
!
! Revision 1.1  1992/04/16  13:29:01  wagner
! Initial revision
!
