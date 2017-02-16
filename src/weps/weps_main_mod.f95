!$Author:$
!$Date:$
!$Revision:$
!$HeadURL:$

module weps_main_mod

   logical :: old_run_file
   character*512 :: clifil  ! climate file name
   character*512 :: runfil  ! run file name
   character*512 :: subfil  ! subdaily wind file name
   character*512 :: winfil  ! wind file name
   character*256 :: usrnam  ! user name
   character*256 :: farmid  ! Farm identifier
   character*256 :: tractid ! Tract identifier
   character*256 :: fieldid ! Field identifier

   integer :: run_rot_cycles ! number of rotation cycles

   integer :: id     ! initial simulation day of month
   integer :: im     ! initial simulation month of year
   integer :: iy     ! initial simulation year
   integer :: ld     ! final (last) simulation day of month
   integer :: lm     ! final (last) simulation month of year
   integer :: ly     ! final (last) simulation year

   character*512 :: rootp*512  ! the root path from which the weps command was started.

! contains

end module weps_main_mod

