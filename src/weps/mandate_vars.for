!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!
! This module needs to be "used" by mandates.for and others

      MODULE mandate_vars

      USE man_opcrop_dates_type_def

      IMPLICIT NONE

          type (man_opcrop_dates_type), dimension(:),                   &
     &            allocatable, target :: mandate

          type (man_opcrop_dates_type), dimension(:), pointer :: mp

      END MODULE mandate_vars
