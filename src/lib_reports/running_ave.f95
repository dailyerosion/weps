!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!
SUBROUTINE run_ave(pd_ave, new_val, cnt) 

    USE pd_var_type_def

    IMPLICIT NONE

    TYPE (pd_var_type), INTENT (IN OUT) :: pd_ave
    REAL,    INTENT (IN) :: new_val
    INTEGER, INTENT (IN) :: cnt

if (cnt == 0) then
write(0,*) "cnt is: ", cnt
write(0,*) 'cnt error in running_ave'
call exit (1)
endif

    pd_ave%val = pd_ave%val * pd_ave%cnt / (pd_ave%cnt+cnt) +  &
              new_val / (pd_ave%cnt+cnt)

    pd_ave%cnt = pd_ave%cnt + cnt

END SUBROUTINE run_ave
