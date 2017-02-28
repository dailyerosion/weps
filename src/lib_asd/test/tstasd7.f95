PROGRAM tstasd7

USE asd_vars

REAL :: initgmd = 3.75, initgsd = 39.55
REAL :: initmnot = 0.005, initminf = 1000.0
INTEGER :: result

    mnsize = 0.005
    mxsize = 1000.0

    result = asd_init_vars(25, mnsize, mxsize, initmnot, initminf)
    IF (result /= 0) then
       write(0,*) "tstasd7: asd_init_vars() failed", result
    ELSE
       write(0,*) "Completed asd_init_vars()"
    END IF

    write(0,*) "Calling asd2mf"
    call asd2mf(initgmd, initgsd, mnsize, mxsize, mf)
    write(0,*) "Called asd2mf"

STOP
END PROGRAM tstasd7