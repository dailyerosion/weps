!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine   dooper (sr, manFile)

!     + + + PURPOSE + + +
!     Dooper reads in any coefficients associated with the
!     operation.

!     + + + KEYWORDS + + +
!     tillage, operation, management

      use weps_interface_defs, ignore_me=>dooper
      use manage_data_struct_defs, only: lastoper, man_file_struct 

!     + + + PARAMETERS AND COMMON BLOCKS + + +
      include 'p1werm.inc'
      include 'manage/man.inc'
      include 'manage/mproc.inc'

!     + + + ARGUMENT DECLARATIONS + + +
      integer sr
      type(man_file_struct), intent(in) :: manFile

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
      read(mtbl(mcur(sr)), 1001) opdumy, lastoper(sr)%code, lastoper(sr)%name
 1001 format(a1,1x,i2,1x,a)
      if( (lastoper(sr)%code.eq.0).and.(mcount(sr).gt.0) ) then
          lastoper(sr)%skip = 1
          print*, 'SR',sr,' Skip operation', lastoper(sr)%code,' ', trim(lastoper(sr)%name)
      else
          print*, 'SR',sr,' Do operation', lastoper(sr)%code,' ', trim(lastoper(sr)%name)
      end if

      ! set value of tlayer to zero before operation begins. Compaction occurs from tlayer
      ! downward, so operations without tillage need this set to zero to model surface compaction.
      tlayer = 0

      ! assign default fuel as blank.  Treated as default in reports
      lastoper(sr)%fuel = ''

      select case (lastoper(sr)%code)
      case (1)  ! original ground engaging operation
          ! set energy and stir values to default
          lastoper(sr)%energyarea = -1
          lastoper(sr)%stir = -1
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
          read(line(2:len_trim(line)), *, err=901) lastoper(sr)%energyarea, lastoper(sr)%stir,  &
     &                  ospeed, odir, ostdspeed, ominspeed, omaxspeed

!         Version 1.5 added ofuel
          if (manFile%mversion .ge. 1.50) then
              ! get fuel line
              mcur(sr) = mcur(sr) + 1
              line = mtbl(mcur(sr))
              if(len_trim(line) .gt. 1) then !only read a line if it has characters after the +
                  read(line(2:len_trim(line)), *) lastoper(sr)%fuel
              end if
          end if
          !write(6,*) 'opname: ', lastoper(sr)%name
          !write(6,*) 'ofuel: ', lastoper(sr)%fuel

      case (4) ! added energy and stir to O2
!         get additional line of data
          mcur(sr) = mcur(sr) + 1
          line = mtbl(mcur(sr))
!         read tillage speed and direction
          read(line(2:len_trim(line)), *, err=901) lastoper(sr)%energyarea, lastoper(sr)%stir

!         Version 1.5 added ofuel
          if (manFile%mversion .ge. 1.50) then
              ! get fuel line
              mcur(sr) = mcur(sr) + 1
              line = mtbl(mcur(sr))
              if(len_trim(line) .gt. 1) then !only read a line if it has characters after the +
                  read(line(2:len_trim(line)), *) lastoper(sr)%fuel
              end if
          end if
          !write(6,*) 'opname: ', lastoper(sr)%name
          !write(6,*) 'ofuel: ', lastoper(sr)%fuel

      case default
          ! set energy and stir values to default
          lastoper(sr)%energyarea = -1
          lastoper(sr)%stir = -1
          ! set fuel to blank (default)
          lastoper(sr)%fuel = ''
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

