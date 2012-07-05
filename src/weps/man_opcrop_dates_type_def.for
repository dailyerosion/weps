!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!
! This module defines the user-defined type for holding a table
! of management operation dates, their names, and any associated
! crops.

      MODULE man_opcrop_dates_type_def

      IMPLICIT NONE

          type :: man_opcrop_dates_type
               integer :: d, m, y 
               character(80) :: opname, cropname
          end type man_opcrop_dates_type

      END MODULE man_opcrop_dates_type_def
