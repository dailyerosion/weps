PROGRAM tstasd7

include "dummy.inc"
USE asd_vars

REAL :: initgmd = 3.75, initgsd = 39.55
REAL :: initmnot = 0.005, initminf = 1000.0
REAL :: gmdp, gsdp
INTEGER :: result

!    mnsize = 0.005
!    mxsize = 1000.0
 
    asd_flg = 1

!    write(0,*) "Starting program"
!    write(0,*) "asd_flg", asd_flg,  "btest(asd_flg, 0)", btest(asd_flg, 0), "btest(asd_flg, 1)", btest(asd_flg, 1)
!    asd_flg = 0
!    write(0,*) "asd_flg", asd_flg,  "btest(asd_flg, 0)", btest(asd_flg, 0), "btest(asd_flg, 1)", btest(asd_flg, 1)
!    write(0,*) 

    result = asd_init_vars(25, mnsize, mxsize, initmnot, initminf)
    IF (result /= 0) then
       write(0,*) "tstasd7: asd_init_vars() failed", result
    END IF

!    write(0,*) "Calling asd2mf"
    call asd2mf(initgmd, initgsd, initmnot, initminf, mf)
!    write(0,*) "Called asd2mf"

    call mf2asd (gmdp, gsdp, initmnot, initminf, mf)

    write(0,*) 'gmdp gsdp', gmdp, gsdp

STOP
END PROGRAM tstasd7