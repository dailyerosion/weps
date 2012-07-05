!$Author$
!$Date$
!$Revision$
!$HeadURL$
!     ------------------------------------------------------------------
!      file 'm1dbug.inc'

!      This common block contains variables to control
!      printing of 'dbugging' files before and after the call to the
!      given subroutine.

!     + + + VARIABLE DECLARATIONS + + +

      integer          am0hdb
      integer          am0sdb
      integer          am0tdb
      integer          am0cdb
      integer          am0ddb
      integer          am0edb

      common / m1dbug /                                                 &
     &                  am0hdb, am0sdb, am0tdb, am0cdb, am0ddb, am0edb

      save :: /m1dbug/

!     + + + VARIABLE DEFINITIONS + + +

!     am0hdb - flag to print HYDROlogy variables before and after
!              the call to HYDRO
!
!                values  -    0, 1
!
!            - If variable is set to the following then print
!              various variables from HYDROlogy submodel
!
!                  0 = no output
!                  1 = output

!     am0sdb - flag to print SOIL variables before and after
!              call to SOIL

!                values  -    0, 1

!            - If am0sfl = 1 then print output,
!              if am0sfl = 0 then print no output

!     am0tdb - flag to print MANAGEMENT (TILLAGE) variables before and after
!              call to MANAGE

!                values  -   0, 1

!            - If am0tfl = 1 then print output,
!              if am0tfl = 0 then print no output

!     am0cdb - flag to print CROP variables before and after
!              call to CROP

!                values  -    0, 1

!            - If am0cfl = 1 then print output,
!              if am0cfl = 0 then print no output

!     am0ddb - flag to print DECOMP variables before and after
!              call to DECOMP

!                values  -    0 ,1

!            - If am0dfl = 1 then print output,
!              if am0dfl = 0 then print no output

!     am0edb - flag to print EROSION variables before and after
!              call to EROSION

!                values  -    0, 1

!            - If am0edb = 1 then print output,
!              if am0edb = 0 then print no output

!-----------------------------------------------------------------------
