!$Author$
!$Date$
!$Revision$
!$HeadURL$

! defined variables for portable checking of precision related numbers
! variables are initialized in main to make available to all subprograms

      real max_arg_exp, max_real

      common / precision / max_arg_exp, max_real

      save :: /precision/

!     max_arg_exp - maximum value allowed for argument to exponential
!                   function without overflowing

!     max_real    - maximum real number allowed
