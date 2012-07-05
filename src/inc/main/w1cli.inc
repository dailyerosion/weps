!$Author$
!$Date$
!$Revision$
!$HeadURL$
!     This common block contains the buffers that cligen data is read into

!     Edit History
!     08-Mar-99   wjr   created
!
!     Parameter include files:

!     + + + VARIABLE DECLARATIONS + + +

      integer  wcd(mndayr)
      integer  wcm(mndayr)
      integer  wcy(mndayr)
      integer  n_header
      integer  daycdx
      integer  newyrcdx
      logical  cwrnflg
      
      real     wwzdpt(mndayr) 
      real     wwdurpt(mndayr)
      real     wwpeaktpt(mndayr)
      real     wwpeakipt(mndayr)
      real     wwtdmx(mndayr)
      real     wwtdmn(mndayr)
      real     wgrad(mndayr)
      real     wwtdpt(mndayr)

      integer  maxday
      parameter (maxday = 366)
      
      common   /w1cli/                                                  &
     &  wwzdpt, wwtdmx, wwtdmn, wgrad,                                  &
     &  wwtdpt, wwdurpt, wwpeaktpt, wwpeakipt,                          &
     &  wcd, wcm, wcy, n_header, daycdx, newyrcdx, cwrnflg

      save :: /w1cli/
      
! the following definitions don't match the variables declared.  
! this should be fixed.      

!     + + + VARIABLE DEFINITIONS + + +

!     wcd    - day 
!     wcm    - month
!     wcy    - year
!     maxday - maximum number of days in the year
!     n_header - number of lines in the cligen file header
!     daycdx - index pointing to current day in cligen file buffer
!     newyrcdx - index pointing to first day of next year in cligen file buffer
!     cwrnflg - indicator turing off cligen warnings after the first

!     wwzdpt - Daily precipitation (mm)
!     wwdurpt - Duration of Daily precipitation (hours)
!     wwpeaktpt - Normalized time to peak of Daily
!                 precipitation (time to peak/duration)
!     wwpeakipt - Normalized intensity of peak Daily
!                 precipitation (peak intensity/average intensity)
!     wwtdmx - Maximum daily air temperature (deg C)
!     wwtdmn - Minimum daily air temperature (deg C)
!     wgrad - Daily global radiation (langleys/day) (as in cligen file)
!     wwtdpt - Daily dew point air temperature (deg C)



