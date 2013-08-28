!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine   dooper (sr)

!     + + + PURPOSE + + +
!     Dooper reads in any coefficients associated with the
!     operation.

!     + + + KEYWORDS + + +
!     tillage, operation, management

      use weps_interface_defs

!     + + + PARAMETERS AND COMMON BLOCKS + + +
      include 'p1werm.inc'
      include 'manage/oper.inc'
      include 'manage/man.inc'
      include 'manage/mproc.inc'

!     + + + ARGUMENT DECLARATIONS + + +
      integer sr

!     + + + ARGUMENT DEFINITIONS + + +
!     sr - the subregion number

!     + + + ACCESSED COMMON BLOCK VARIABLE DEFINITIONS + + +
!     odir - operation direction (degrees from NORTH)
!     ospeed - operation speed 

!     + + + LOCAL VARIABLES + + +
      character*256   line
      character*1 opdumy
!
!     + + + SUBROUTINES CALLED + + +
!     + + + FUNCTIONS CALLED + + +

!     + + + DATA INITIALIZATIONS + + +

!     + + + END SPECIFICATIONS + + +

!      write(*,*) '*>dooper line |', mtbl(mcur(sr)), '|'
      read(mtbl(mcur(sr)), 1001) opdumy, opcode, opname
 1001 format(a1,1x,i2,1x,a)
      if( (opcode.eq.0).and.(mcount(sr).gt.0) ) then
          opskip = 1
          print*, 'SR',sr,' Skip operation', opcode,' ',opname
      else
          print*, 'SR',sr,' Do operation', opcode,' ',opname
      end if



      ! assign default fuel as blank.  Treated as default in reports
      ofuel = ''

      select case (opcode)
      case (1)  ! original ground engaging operation
          ! set energy and stir values to default
          oenergyarea = -1
          ostir = -1
!         get additional line of data
          mcur(sr) = mcur(sr) + 1
          line = mtbl(mcur(sr))
!         read tillage speed and direction
          read(line(2:len_trim(line)), *, err=901) ospeed, odir,        &
     &                             ostdspeed, ominspeed, omaxspeed
      case (3) ! added energy and stir to O1
!         get additional line of data
          mcur(sr) = mcur(sr) + 1
          line = mtbl(mcur(sr))
!         read tillage speed and direction
          read(line(2:len_trim(line)), *, err=901) oenergyarea, ostir,  &
     &                  ospeed, odir, ostdspeed, ominspeed, omaxspeed

!         Version 1.5 added ofuel
          if (mversion(sr) .ge. 1.50) then
              ! get fuel line
              mcur(sr) = mcur(sr) + 1
              line = mtbl(mcur(sr))
              if(len_trim(line) .gt. 1) then !only read a line if it has characters after the +
                  read(line(2:len_trim(line)), *) ofuel
              end if
          end if
          !write(6,*) 'opname: ', opname
          !write(6,*) 'ofuel: ', ofuel

      case (4) ! added energy and stir to O2
!         get additional line of data
          mcur(sr) = mcur(sr) + 1
          line = mtbl(mcur(sr))
!         read tillage speed and direction
          read(line(2:len_trim(line)), *, err=901) oenergyarea, ostir

!         Version 1.5 added ofuel
          if (mversion(sr) .ge. 1.50) then
              ! get fuel line
              mcur(sr) = mcur(sr) + 1
              line = mtbl(mcur(sr))
              if(len_trim(line) .gt. 1) then !only read a line if it has characters after the +
                  read(line(2:len_trim(line)), *) ofuel
              end if
          end if
          !write(6,*) 'opname: ', opname
          !write(6,*) 'ofuel: ', ofuel

      case default
          ! set energy and stir values to default
          oenergyarea = -1
          ostir = -1
          ! set fuel to blank (default)
          ofuel = ''
      end select

      ! set up stir accounting.  Must be after op case so that fuel is correct.
      call stir_oper(sr)

      ! initialize row spacing and ridge flag to zero. They are needed
      ! by P51, (set in P3 or P5) but may be set and not cleared by a previous operation.
      imprs = 0.0
      rdgflag = 0

      return
! Error stops
  901 write(0,9901) mtbl(mcur(sr))
 9901 format ('DOOPER: Error reading line ->', a)
      call exit (1)
      end

