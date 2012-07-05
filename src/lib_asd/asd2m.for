!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine asd2m (mnot, minf, gmd, gsd, nlay, mf)

      include 'p1werm.inc'
      include 'manage/asd.inc'

!     + + + PURPOSE + + +
!     This subroutine  performs the inverse of subroutine m2asd.
!     asd2m computes the mass fractions for each sieve cut from the
!     lognormal representation of the soil aggregate size distribution.
!
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
      real    mnot(mnsz), minf(mnsz)
      real    gmd(mnsz), gsd(mnsz)
      integer nlay
      real    mf(msieve+1,mnsz)
!
!
!     + + + ARGUMENT DEFINITIONS + + +
!     mnot   - minimum size aggregate (assumed value is known)
!     minf   - maximum size aggregate (assumed value is known)
!     gmd    - geometric mean diameter of aggregate size distribution
!              (or transformed asd for "modified" lognormal cases)
!     gsd    - geometric standard deviation of aggregate size distribution
!              (or transformed asd for "modified" lognormal cases)
!     nlay   - number of soil layers used
!     mf     - mass fractions of aggregates within sieve cuts
!              (sum of all mass fractions are expected to = 1.0)
!
!     + + + ACCESSED COMMON BLOCK VARIABLE DEFINITIONS + + +
!
!     nsieve - number of sieves used
!     sdia   - array containing sieve size diameters
!     mdia   - geometric mean dia. for each sieve cut
!     logcas - flag to represent which lognormal case to apply
!
!
!     + + + PARAMETERS + + +
!
!     + + + LOCAL VARIABLES + + +

      real     d(msieve+1)
      real     lngmd, lngsd
      real     prev, this
      integer  i, j

!     + + + FUNCTION DEFINITIONS + + +
      real     erf

!     + + + LOCAL VARIABLE DEFINITIONS + + +
!
!     d      - transformed sieve dia. values
!              (if "abnormal" lognormal cases)
!     lngmd - natural log of gmd
!     lngsd - natural log of gsd
!     prev   - contain previous sieve dia. cumulative prob
!     this   - contain this sieve dia. cumulative prob
!     i      - loop variable for sieve sizes
!     j      - loop variable for soil layers
!
!     + + + END SPECIFICATIONS + + +

      do 20 j = 1, nlay
!         compute transformed sieve dia. sizes
          if (logcas .eq. 3) then
              do 1 i = 1, nsieve
                  if(sdia(i).lt.minf(j)) then
                      d(i) = (sdia(i)-mnot(j))*(minf(j)-mnot(j))/       &
     &                       (minf(j)-sdia(i))
                  end if
   1          continue
          elseif (logcas .eq. 2) then
              do 2 i = 1, nsieve
                  if(sdia(i).lt.minf(j)) then
                      d(i) = sdia(i)*minf(j)/(minf(j)-sdia(i))
                  end if
   2          continue
          elseif (logcas .eq. 1) then
              do 3 i = 1, nsieve
                   d(i) = sdia(i)-mnot(j)
   3          continue
          elseif (logcas .eq. 0) then
              do 4 i = 1, nsieve
                   d(i) = sdia(i)
   4          continue
          endif
          lngmd= log(gmd(j))
          lngsd= sqrt(2.0) * log(max(mingsd,gsd(j)))
          prev= 1.0

!         compute each dia. cumulative probability
          do 10 i = 1, nsieve
              if (sdia(i) .le. mnot(j)) then
                 this = 1.0
              else if (sdia(i) .lt. minf(j)) then
                 this = 0.5 -0.5*erf((alog(d(i)) - lngmd) / lngsd)
              else
                 this = 0.0
              end if
!             compute mass fraction between prev and this dia
              mf(i,j) = prev - this
              prev = this
!              write(*,*) 'asd2m:',i,sdia(i),this,mf(i,j)

!             if roundoff errors or otherwise results in negative
!             mass fraction then set to zero mass
              if (mf(i,j) .lt. 0.0) then
                  mf(i,j) = 0.0
              else
                  prev = this
              endif
!              if(j.eq.4) write(*,*) 'asd2m: mf(',i,j,')',mf(i,j)
  10      continue

!         get mass fraction for upper-most sieve cut
          mf(nsieve+1,j) = prev
!        if( j.eq.1 )write(*,*)'asd2m: mf(',nsieve+1,j,')',mf(nsieve+1,j)

!         zero out the rest of the array which is used every where else
          do i=nsieve+2, msieve+1
              mf(i,j) = 0.0
          end do

  20  continue
      return
      end

!      TRIAL CODE FOR ADJUSTABLE BIN SIZES (not implemented)
!      implementation would require creating unique bins for
!      every soil layer for every conversion
!      real     mxbin, mnbin, sdiahigh, sdialow
!      real     mod_log_normal
!      parameter( mxbin=0.02 )
!      parameter( mnbin=0.005 )
!      do 20 j = 1, nlay
!
!          lngmd= log(gmd(j))
!          lngsd= sqrt(2.0) * log(gsd(j))
!          prev= 1.0
!          mnsize = mnot(j)
!          mxsize = minf(j)
!          nsieve = msieve - 1
!
!         compute each dia. cumulative probability
!          do i=1, nsieve
!              if( i.gt.1 ) then
!                  sdia(i) = sdia(i-1)
!                  sdialow = sdia(i)
!              else
!                  sdia(i) = mnsize
!                  sdialow = sdia(i)
!              end if
!             double size until value within bound found or mxbin exceeded
!  12          sdia(i) = sdia(i)*2.0
!              this = mod_log_normal( sdia(i), lngmd, lngsd )
!              mf(i,j) = prev - this
!              if( (mf(i,j).lt.mnbin).and.(sdia(i).lt.mxsize) ) then
!                  !keep doubling
!                  goto 12
!              else if( mf(i,j).gt.mxbin ) then       
!                  !too far but bracketed so bisect (geometrically)
!                  sdiahigh = sdia(i)
!  15              sdia(i) = sqrt(sdiahigh*sdialow)
!                  this = mod_log_normal( sdia(i), lngmd, lngsd )
!                  mf(i,j) = prev - this
!                  if( mf(i,j).lt.mnbin ) then
!                      sdialow = sdia(i)
!                      go to 15
!                  else if( mf(i,j).gt.mxbin ) then       
!                      sdiahigh = sdia(i)
!                      go to 15
!                  end if
!              else if( sdia(i).ge.mxsize ) then
!                  nsieve = i-1
!                  go to 18
!              end if
!              prev = this
!c              write(*,*) 'asd2m:',i,sdia(i),this,mf(i,j)
!          end do
!
!  18      continue
!          write(*,*) 'asd2m:',i,mxsize,this,mf(i,j)
!
!c         compute geometric mean dia. for each sieve cut
!          mdia(1) = sqrt(mnsize*sdia(1))
!          do i = 2, nsieve
!               mdia(i) = sqrt(sdia(i)*sdia(i-1))
!          end do
!          mdia(nsieve+1) = sqrt(mxsize*sdia(nsieve))
!
!         zero out the rest of the array which is used every where else
!          do i=nsieve+2, msieve+1
!              mf(i,j) = 0.0
!          end do
!
!  20  continue
!
!      real function mod_log_normal( sieve_dia, lngmd, lngsd )
!
!      include 'manage/asd.inc'
!
!     + + + PURPOSE + + +
!     this function is used to calculate the fractions on the
!     modified log normal distribution
!
!     + + + ARGUMENT DECLARATIONS + + +
!      real sieve_dia, lngmd, lngsd
!
!     + + + LOCAL VARIABLES + + +
!      real mod_dia
!
!      if (sieve_dia .lt. mnsize) then
!          mod_log_normal = 1.0
!      else if (sieve_dia .lt. mxsize) then
!         compute transformed sieve dia. sizes
!          if (logcas .eq. 3) then
!              mod_dia = (sieve_dia-mnsize)*(mxsize-mnsize)/
!     &               (mxsize-sieve_dia)
!          elseif (logcas .eq. 2) then
!              mod_dia = sieve_dia*mxsize/(mxsize-sieve_dia)
!          elseif (logcas .eq. 1) then
!              mod_dia = sieve_dia-mnsize
!          elseif (logcas .eq. 0) then
!              mod_dia = sieve_dia
!          end if
!          mod_log_normal = 0.5 - 0.5*erf((log(mod_dia)-lngmd)/lngsd)
!      else
!          mod_log_normal = 0.0
!      end if
!
!      return
!      end

!$Log: not supported by cvs2svn $
!Revision 1.6  2002/04/29 16:16:50  fredfox
!Added use of parameter to restrain the minimum value of GSD. Cleaned up variable and function reference from alog to log (alog in real*4 only, log is general)
!
!Revision 1.5  2002/04/17 20:16:07  fredfox
!modified m2asd to properly handle the upper and lower bounds on the
!4 parameter lognormal distribution by allowing the specification of
!geometric mean bin diameter to use either the sieve above or the
!limit depending on which applies. (same on the lower bin).
!Modified asd2m to handle bin sizes outside the range of of the distribution.
!asdini.for changes were cosmetic only
!
!Revision 1.4  2001/09/27 20:36:51  fredfox
!merged in update hydro branch
!
!Revision 1.3.8.2  2001/08/15 22:12:33  fredfox
!Corrected index for zeroing out unused array elements, added headers, edited out debug write statements.
!
!Revision 1.3.8.1  2001/07/05 19:04:09  fredfox
!Previous change in method of data initialization did not account for using fewer than maximum number of sieves. Creation of sieve cuts extended to zero out all possible sieve elements
!
!Revision 1.3  1999/04/06 18:03:17  wjr
!removed debugging lines
!
!Revision 1.1.1.1  1999/03/12 17:05:17  wagner
!Baseline version of WEPS with Bill Rust's modifications
!
! Revision 1.2  1995/09/13  15:49:32  wagner
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
