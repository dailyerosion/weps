!$Author$
!$Date$
!$Revision$
!$HeadURL$

      real function rainenergy( ninten, timem, intensity)

!     + + + PURPOSE + + +
!     Implements water drop kinetic energy (rain) from WEPP idat.for
!     returns kinetic energy of rainfall (J/m^2)

!     + + + ARGUMENT DECLARATIONS + + +
      integer, intent(in) :: ninten
      real, intent(in) :: timem(*), intensity(*)

!     + + + ARGUMENT DEFINITIONS + + +
!     nri     - number of rainfall breakpoint intervals
!     timem   - breakpoint time markers (sec)
!     intensity - breakpoint intensity (m/sec)

!     + + + PARAMETERS + + +
      real hrtosec
      parameter (hrtosec = 3600.0)

!     + + + PARAMETER DEFINITIONS + + +
!     convert hout time value to seconds

!     + + + LOCAL VARIABLES + + +
      integer idx
      real vtime, vint, vike
      real rkine

!     + + + LOCAL DEFINITIONS + + +
!     idx   - index for looping through array
!     vtime - interval time step (hr)
!     vint  - interval intensity (m/hr)
!     vike  - interval kinetic energy
!     rkine - kinetic energy of rainfall (J/m^2)

!     + + + DETAILED DESCRIPTION + + +

!     Calculate rainfall kinetic energy using equation from Van Doren
!     and Allmaras where KE is approximated by:
!
!     KE=(3.812+0.874 log10 RI)*time*RI
!
!     where KE is in J/cm2, Time is the duration(hr), and RI is the rainfall
!     intensity (m/hr).  Note: This equation is also given by Wischmeier for
!     english units.  To gain accuracy we apply to each time step.  I have
!     also developed an analytical solution to calculate KE for the WEPP
!     double exponential storm, however, I thought it may be more reasonable
!     to calculate KE based on the disaggregated storm.  Risse 11/4/93.

!     + + + END SPECIFICATIONS + + +

      rkine = 0.0
      do idx = 1, ninten - 1
        ! find time step in hours
        vtime = (timem(idx+1) - timem(idx)) / hrtosec
        ! convert intensity to meters per hour
        vint = intensity(idx) * hrtosec

        ! If intensity is greater than 3 in/hr energy does not increase as
        ! maximum drop size has been attained.
        if( vint .gt. 0.0765 ) vint = 0.0765
        if( (vtime .gt. 0.0) .and. (vint .gt. 0.0) ) then
          vike = (3.812 + 0.3796*log(vint)) * vtime * vint
        else
          vike = 0
        end if
        ! convert KE to J/m2
        vike = vike * 10000.0
        if( vike .gt. 0.0 ) rkine = vike + rkine
      end do

      rainenergy = rkine

      return
      end