!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine sbsfdi (slagm, s0ags, slagn, slagx, sldi, sfdi)

!     +++ PURPOSE +++
!     calc soil mass fraction (sfdi) < diameter (sldi)
!     given modified lognormal distribution parameters

!     +++ ARGUMENT DECLARATIONS +++
      real slagm, s0ags, slagn, slagx, sldi, sfdi

!     +++  ARGUMENT DEFINITIONS +++
!     slagm - aggregate distribution geometric mean diameter (mm).
!     s0ags - aggregate distribution geometric standard deviation.
!     slagn - aggregate distribution lower limit (mm).
!     slagx - aggregate distribution upper limit (mm).
!     sldi  - soil diameter in distribution (mm)
!     sfdi  - soil mass fraction < sldi

!     +++ LOCAL VARIABLES +++
      real slt

!     +++ FUNCTIONS CALLED+++
      real erf

!     +++ END SPECIFICATIONS +++

!     calc soil mass < sldi

      if (sldi .lt. slagx .and. sldi .gt. slagn) then
        slt = ((sldi - slagn)*(slagx - slagn))/((slagx - sldi)*slagm)
        sfdi = 0.5*(1 + erf(alog(slt)/(sqrt(2.0)*alog(s0ags))))
      elseif (sldi .ge. slagx) then
        sfdi = 1.0
      else
        sfdi = 0.0
      endif

      return
      end
